unit RiggVar.BO.StoreAdapter;

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
  System.SysUtils,
  System.Classes,
  RiggVar.DAL.Manager;

type
  TStoreAdapterBase = class
  public
    BackupString: string;

    procedure CheckResultsDir; virtual;
    procedure CheckConfigDir; virtual;

    procedure SaveBackup(SL: TStrings); virtual;
    procedure LoadBackup(SL: TStrings); virtual;

    function GetWorkspacePath(StrWorkspace: string): string; virtual;
    function GetWorkspaceSubDir(wn, dn: string): string; virtual;

    procedure StringListSaveToFile(SL: TStrings; fn: string); virtual;
    procedure StringListSaveToFileUTF8(SL: TStrings; fn: string); virtual;

    procedure StringListSaveIfDirExists(SL: TStrings; dn, fn: string); virtual;
    procedure StringListSaveIfDirExistsUTF8(SL: TStrings; dn, fn: string); virtual;

    procedure InitDBInterface(dm: TDocManager; DBTypeName: string); virtual;

    function GetWorkspaceType: Integer; virtual;
    function GetWorkspaceID: Integer; virtual;
    procedure InitWorkspaceManager; virtual;
    procedure UpdateWorkspace(WorkspaceType, WorkspaceID: Integer); virtual;
    procedure InitWorkspace(WorkspaceType: Integer; WorkspaceID: Integer); virtual;
  end;

implementation

{ TStoreAdapterBase }

procedure TStoreAdapterBase.CheckResultsDir;
begin
end;

procedure TStoreAdapterBase.CheckConfigDir;
begin
end;

function TStoreAdapterBase.GetWorkspacePath(StrWorkspace: string): string;
begin
  result := '';
end;

function TStoreAdapterBase.GetWorkspaceSubDir(wn, dn: string): string;
begin
  result := IncludeTrailingPathDelimiter(wn + dn);
end;

procedure TStoreAdapterBase.LoadBackup(SL: TStrings);
begin
  SL.Text := BackupString;
end;

procedure TStoreAdapterBase.SaveBackup(SL: TStrings);
begin
  BackupString := SL.Text;
end;

procedure TStoreAdapterBase.StringListSaveIfDirExists(SL: TStrings; dn, fn: string);
begin
end;

procedure TStoreAdapterBase.StringListSaveIfDirExistsUTF8(SL: TStrings; dn,
  fn: string);
begin
end;

procedure TStoreAdapterBase.StringListSaveToFile(SL: TStrings; fn: string);
begin
end;

procedure TStoreAdapterBase.StringListSaveToFileUTF8(SL: TStrings; fn: string);
begin
end;

procedure TStoreAdapterBase.InitDBInterface(dm: TDocManager; DBTypeName: string);
begin
end;

function TStoreAdapterBase.GetWorkspaceID: Integer;
begin
  result := 0;
end;

function TStoreAdapterBase.GetWorkspaceType: Integer;
begin
 result := 0;
end;

procedure TStoreAdapterBase.InitWorkspace(WorkspaceType, WorkspaceID: Integer);
begin
end;

procedure TStoreAdapterBase.InitWorkspaceManager;
begin
end;

procedure TStoreAdapterBase.UpdateWorkspace(WorkspaceType,
  WorkspaceID: Integer);
begin
end;

end.
