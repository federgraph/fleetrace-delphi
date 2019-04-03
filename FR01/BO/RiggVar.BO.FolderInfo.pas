unit RiggVar.BO.FolderInfo;

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

uses
  System.SysUtils;

type
  TFolderInfo = class
  private
    FAppName: string;
    FWorkspacePath: string;
    FSettingsPath: string;
    FDataPath: string;
    FBackupPath: string;
    FHelpPath: string;
    FTracePath: string;
    FPublishPath: string;
    function GetWorkspacePath: string;
    procedure SetDataPath(const Value: string);
    procedure SetSettingsPath(const Value: string);
    function GetConfigName: string;
    function GetAppName: string;
    function GetBackupPath: string;
    function GetDataPath: string;
    function GetSettingsPath: string;
    function GetHelpPath: string;
    function GetTracePath: string;
    function WorkspaceSubDir(dn: string): string;
    procedure Clear;
    function GetPublishPath: string;
  public
    constructor Create;

    procedure WorkspaceInfoChanged;

    property AppName: string read GetAppName;
    property WorkspacePath: string read GetWorkspacePath;
    property SettingsPath: string read GetSettingsPath write SetSettingsPath;
    property DataPath: string read GetDataPath write SetDataPath;
    property BackupPath: string read GetBackupPath;
    property HelpPath: string read GetHelpPath;
    property TracePath: string read GetTracePath;
    property PublishPath: string read GetPublishPath;
    property ConfigFileName: string read GetConfigName;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Util.AppUtils;

resourcestring
  StrDBWorkspace = 'RVDBWS';
  StrWorkspace = 'RiggVar Workspace';
  StrData = 'DBEvent';
  StrBackup = 'Backup';
  StrHelp = 'Help';
  StrTrace = 'Trace';
  StrPublish = 'Published';
{$ifdef VER150}
  StrSettings = 'Settings D7';
{$else}
  StrSettings = 'Settings DS2018';
{$endif}
  StrConfigExtension = '.ini';

{ TFolderInfo}

procedure TFolderInfo.Clear;
begin
  FWorkspacePath := '';
  FSettingsPath := '';
  FDataPath := '';
  FBackupPath := '';
  FHelpPath := '';
  FTracePath := '';
  FPublishPath := '';
end;

procedure TFolderInfo.SetDataPath(const Value: string);
begin
  FDataPath := Value;
end;

procedure TFolderInfo.SetSettingsPath(const Value: string);
begin
  FSettingsPath := IncludeTrailingPathDelimiter(Value);
end;

procedure TFolderInfo.WorkspaceInfoChanged;
begin
  Clear;
end;

constructor TFolderInfo.Create;
begin
end;

function TFolderInfo.GetAppName: string;
begin
  if FAppName = '' then
  begin
    FAppName := ChangeFileExt(ExtractFileName(TAppUtils.GetFullExeName), '');
  end;
  result := FAppName;
end;

function TFolderInfo.GetWorkspacePath: string;
begin
  if FWorkspacePath = '' then
    FWorkspacePath := Main.StoreAdapter.GetWorkspacePath(StrWorkspace);
  result := FWorkspacePath;
end;

function TFolderInfo.WorkspaceSubDir(dn: string): string;
begin
  result := Main.StoreAdapter.GetWorkspaceSubDir(WorkspacePath, dn);
end;

function TFolderInfo.GetConfigName: string;
begin
  result := SettingsPath + AppName + StrConfigExtension;
end;

function TFolderInfo.GetSettingsPath: string;
begin
  if FSettingsPath = '' then
  begin
    FSettingsPath := WorkspaceSubDir(StrSettings);
  end;
  result := FSettingsPath;
end;

function TFolderInfo.GetDataPath: string;
begin
  if FDataPath = '' then
  begin
    FDataPath := WorkspaceSubDir(StrData);
  end;
  result := FDataPath;
end;

function TFolderInfo.GetBackupPath: string;
begin
  if FBackupPath = '' then
  begin
    FBackupPath := WorkspaceSubDir(StrBackup);
  end;
  result := FBackupPath + AppName;
end;

function TFolderInfo.GetHelpPath: string;
begin
  if FHelpPath = '' then
  begin
    FHelpPath := WorkspaceSubDir(StrHelp);
  end;
  result := FHelpPath + AppName;
end;

function TFolderInfo.GetPublishPath: string;
begin
  if FPublishPath = '' then
  begin
    FPublishPath := WorkspaceSubDir(StrPublish);
  end;
  result := FPublishPath;
end;

function TFolderInfo.GetTracePath: string;
begin
  if FTracePath = '' then
  begin
    FTracePath := WorkspaceSubDir(StrTrace);
  end;
  result := FTracePath + AppName;
end;

end.
