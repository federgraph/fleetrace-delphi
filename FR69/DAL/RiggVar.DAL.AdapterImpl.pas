unit RiggVar.DAL.AdapterImpl;

(*
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
*)

interface

{$define DeveloperVersion}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.DAL.AdapterBase,
  RiggVar.DAL.WorkspaceInfo,
  RiggVar.DAL.Intf,
  RiggVar.DAL.Manager,
  RiggVar.DAL.WorkspaceIntf;

type
  TStoreAdapter = class(TStoreAdapterBase2)
  private
    FailedBefore: Boolean;
    procedure SafeCreateDir(dn: string);
  public
    function GetWorkspacePath(StrWorkspace: string): string; override;
    function GetWorkspaceSubDir(wn, dn: string): string; override;
    procedure CheckWorkspaceInfo(wi: TWorkspaceInfo); override;
    function GetWorkspaceTypeName(wi: TWorkspaceInfo): string; override;
    procedure InitDBInterface(dm: TDocManager; DBTypeName: string); override;

    procedure CheckResultsDir; override;
    procedure CheckConfigDir; override;
    procedure SaveBackup(SL: TStrings); override;
    procedure LoadBackup(SL: TStrings); override;
    procedure StringListSaveToFile(SL: TStrings; fn: string); override;
    procedure StringListSaveToFileUTF8(SL: TStrings; fn: string); override;
    procedure StringListSaveIfDirExists(SL: TStrings; dn, fn: string); override;
    procedure StringListSaveIfDirExistsUTF8(SL: TStrings; dn, fn: string); override;

    procedure SaveTimeStampedBackup(SL: TStrings);
  end;

implementation

uses
  Windows,
  SHFolder,
  RiggVar.App.Main,
  RiggVar.DAL.Redirector,
  RiggVar.DAL.MDB,
  RiggVar.DAL.WEB,
  RiggVar.DAL.REST,
  RiggVar.DAL.LNK,
  RiggVar.DAL.XML,
  RiggVar.Util.AppUtils;

{ TStoreAdapter }

procedure TStoreAdapter.InitDBInterface(dm: TDocManager;
  DBTypeName: string);
begin
  if DBTypeName = 'TXT' then
  begin
    dm.DB := TDBEvent.Create;
    dm.SaveEnabled := true;
    dm.SaveAsEnabled := true;
    dm.DeleteEnabled := true;
  end
  else if DBTypeName = 'LNK' then
  begin
    dm.DB := TDBEventLNK.Create;
    dm.SaveEnabled := true;
    dm.SaveAsEnabled := false;
    dm.DeleteEnabled := true;
  end
  else if DBTypeName = 'XML' then
  begin
    dm.DB := TDBEventXML.Create;
    dm.SaveEnabled := true;
    dm.SaveAsEnabled := false;
    dm.DeleteEnabled := true;
  end
  else if DBTypeName = 'MDB' then
  begin
    dm.DB := TdmCompDataMDB.Create(nil);
    dm.SaveEnabled := true;
    dm.SaveAsEnabled := true;
    dm.DeleteEnabled := true;
  end
  else if DBTypeName = 'WEB' then
  begin
    dm.DB := TCompDataWEB.Create;
    dm.SaveEnabled := true; //only for testing
  end
  else if DBTypeName = 'REST' then
  begin
    dm.DB := TCompDataREST.Create;
    dm.SaveEnabled := true; //only for testing
  end
end;

procedure TStoreAdapter.CheckResultsDir;
begin
  ForceDirectories(Main.FolderInfo.WorkspacePath + 'FRResult\');
end;

procedure TStoreAdapter.CheckConfigDir;
begin
  ForceDirectories(Main.FolderInfo.SettingsPath);
end;

procedure TStoreAdapter.CheckWorkspaceInfo(wi: TWorkspaceInfo);
begin
  //check for potential problems
  if wi.WorkspaceType = WorkspaceType_FixedFS then
  begin
    if not DirectoryExists(wi.WorkspaceRoot) then
    begin
      wi.HasError_WorkspaceRoot := true;
      wi.Clear;
      wi.WorkspaceType := 1;
      wi.AutoSaveIni := false;
      wi.WorkspaceRoot := 'invalid'
    end;
    //if not DirectoryIsWritable...
  end;
end;

function TStoreAdapter.GetWorkspacePath(StrWorkspace: string): string;
var
  namebuffer: array [0..MAX_PATH-1] of Char;
  namebuffer2: array [0..256] of Char;
  wt: Integer;
  FWorkspacePath: string;
begin
    wt := Main.WorkspaceInfo.WorkspaceType;
    if (wt < 0) or (wt > 6) then
      wt := 0;

    { manage the 0 }
    if (wt < 1) then
    begin
{$ifdef DeveloperVersion}
      wt := 1; //Developer GUI App default
{$endif}

{$ifdef WebBroker}
      wt := 6; //WebBroker default
{$endif}

{$ifdef Service}
      wt := 2; //Service default
{$endif}
    end;

    case wt of
      //Shared Workspace - use folder in AppData
      0: begin
        if SUCCEEDED(SHGetFolderPath(0, //Application.Handle,
          CSIDL_LOCAL_APPDATA, 0, 0, namebuffer)) then
        begin
          FWorkspacePath := string(namebuffer) +'\RiggVar\FR\';
          if not DirectoryExists(FWorkspacePath) then
            SafeCreateDir(FWorkspacePath);
        end;
      end;

      //Shared Workspace - use 'RiggVar Workspace' in user home dir
      1: begin
        if SUCCEEDED(SHGetFolderPath(0, //Application.Handle,
          CSIDL_PERSONAL, 0, 0, namebuffer)) then
        begin
          //FWorkspacePath := string(namebuffer) + '\' + StrWorkspace + '\';
          FWorkspacePath := string(namebuffer);
          FWorkspacePath := IncludeTrailingPathDelimiter(FWorkspacePath);
          FWorkspacePath := IncludeTrailingPathDelimiter(FWorkspacePath + StrWorkspace);
          if not DirectoryExists(FWorkspacePath) then
            SafeCreateDir(FWorkspacePath);
        end;
      end;

    //Local Workspace - create 'RiggVar Workspace' as subfolder
    2: begin
      FWorkspacePath := ExtractFilePath(TAppUtils.GetFullExeName) + '\' + StrWorkspace + '\';

      if not DirectoryExists(FWorkspacePath) then
        SafeCreateDir(FWorkspacePath);
    end;

    //Local DB
    3: begin
      FWorkspacePath := '\';
      if not DirectoryExists(FWorkspacePath) then
        SafeCreateDir(FWorkspacePath);
    end;

    //Remote DB / WebService
    4, 5: begin
      FWorkspacePath := '\';
    end;

      //Fixed WorkspaceRoot in local FileSystem
    6: begin
      if (Main.WorkspaceInfo.WorkspaceRoot <> '') then
        FWorkspacePath := IncludeTrailingPathDelimiter(Main.WorkspaceInfo.WorkspaceRoot)
      else
      begin
        GetTempPath(SizeOf(namebuffer2)-1, @namebuffer2);
        FWorkspacePath := namebuffer2;
        FWorkspacePath := IncludeTrailingPathDelimiter(FWorkspacePath);
        FWorkspacePath := FWorkspacePath + '\RiggVar Workspace\';
     end;
     if not DirectoryExists(FWorkspacePath) then
       ForceDirectories(FWorkspacePath);
    end;

    end;
  result := FWorkspacePath;
end;

function TStoreAdapter.GetWorkspaceSubDir(wn, dn: string): string;
begin
  result := IncludeTrailingPathDelimiter(wn + dn);
  if not DirectoryExists(result) then
    SafeCreateDir(result);
end;

function TStoreAdapter.GetWorkspaceTypeName(wi: TWorkspaceInfo): string;
begin
  case wi.WorkspaceType of
    WorkspaceType_SharedFS: result := 'Shared FS';
    WorkspaceType_PrivateFS: result := 'Private FS';
    WorkspaceType_LocalDB: result := 'Local DB';
    WorkspaceType_RemoteDB: result := 'Remote DB';
    WorkspaceType_WebService: result := 'Web Service';
    WorkspaceType_FixedFS: result := 'Fixed FS';
    else
      result := '';
  end;
end;

procedure TStoreAdapter.LoadBackup(SL: TStrings);
var
  fn: string;
begin
  fn := Main.FolderInfo.BackupPath + '_Backup.txt';
  if DBFileExists(fn) then
    SL.LoadFromFile(fn);
end;

procedure TStoreAdapter.SaveTimeStampedBackup(SL: TStrings);
var
  fn: string;
begin
  fn := Main.FolderInfo.BackupPath + '_Backup_' + FormatDateTime('yymmdd_hhnnss', Now) + '.txt';
  SL.SaveToFile(fn);
end;

procedure TStoreAdapter.SaveBackup(SL: TStrings);
var
  fn: string;
begin
  fn := Main.FolderInfo.BackupPath + '_Backup.txt';
  SL.SaveToFile(fn);
end;

procedure TStoreAdapter.StringListSaveIfDirExists(SL: TStrings; dn, fn: string);
begin
  if (Length(dn) > 4) and DirectoryExists(dn) then
    StringListSaveToFile(SL, fn);
end;

procedure TStoreAdapter.StringListSaveIfDirExistsUTF8(SL: TStrings; dn,
  fn: string);
begin
  if (Length(dn) > 4) and DirectoryExists(dn) then
    StringListSaveToFileUTF8(SL, fn);
end;

procedure TStoreAdapter.StringListSaveToFile(SL: TStrings; fn: string);
begin
  SL.SaveToFile(fn);
end;

procedure TStoreAdapter.StringListSaveToFileUTF8(SL: TStrings; fn: string);
begin
  SL.SaveToFile(fn, TEncoding.UTF8);
end;

procedure TStoreAdapter.SafeCreateDir(dn: string);
var
  em: Word;
  s: string;
begin
  if not FailedBefore then
  begin
    em := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      try
        CreateDir(dn);
      except
        on E: EExternalException do
          FailedBefore := True;
        on E: Exception do
        begin
          s := 'cannot create dir' + dn;
          OutputDebugString(PChar(s));
        end;
      end;
    finally
      SetErrorMode(em);
    end;
  end;
end;

end.
