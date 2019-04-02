unit RiggVar.DAL.WorkspaceTXT;

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
  Data.DB,
  RiggVar.DAL.WorkspaceIntf;

type
  TdmFSWorkspaceFiles = class(TInterfacedObject, IDBWorkspace)
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetWorkspaceID(WorkspaceID: Integer);
    function GetWorkspaceID: Integer;
    function DBFileExists(const fn: string): Boolean;
    function DBDirectoryExists(const dn: string): Boolean;
    function DBDeleteFile(const fn: string): Boolean;
    function DBDeleteWorkspace: Boolean;
    procedure DBLoadFromFile(const fn: string; SL: TStrings);
    procedure DBSaveToFile(const fn: string; SL: TStrings);
    function DBGetEventNames(ExtensionFilter: string): string;
    function GetDataSet: TDataSet;
    procedure ExportFile(const WorkspaceDir: string); virtual;
    procedure ExportFiles(const WorkspaceDir: string); virtual;
  end;

implementation

uses
  RiggVar.DAL.Redirector;

{ TTdmFSWorkspaceFiles }

constructor TdmFSWorkspaceFiles.Create;
begin
  inherited Create;
end;

destructor TdmFSWorkspaceFiles.Destroy;
begin
  inherited Destroy;
end;

procedure TdmFSWorkspaceFiles.ExportFile(const WorkspaceDir: string);
begin
  //not implemented
end;

procedure TdmFSWorkspaceFiles.ExportFiles(const WorkspaceDir: string);
begin
  //not implemented
end;

function TdmFSWorkspaceFiles.DBDeleteFile(const fn: string): Boolean;
begin
  result := false;
end;

function TdmFSWorkspaceFiles.DBDeleteWorkspace: Boolean;
begin
  result := false;
end;

function TdmFSWorkspaceFiles.DBDirectoryExists(const dn: string): Boolean;
begin
  result := false;
end;

function TdmFSWorkspaceFiles.DBFileExists(const fn: string): Boolean;
begin
  result := false;
end;

function TdmFSWorkspaceFiles.DBGetEventNames(ExtensionFilter: string): string;
begin

end;

procedure TdmFSWorkspaceFiles.DBLoadFromFile(const fn: string; SL: TStrings);
begin

end;

procedure TdmFSWorkspaceFiles.DBSaveToFile(const fn: string; SL: TStrings);
begin

end;

function TdmFSWorkspaceFiles.GetDataSet: TDataSet;
begin
  result := nil;
end;

function TdmFSWorkspaceFiles.GetWorkspaceID: Integer;
begin
  result := 1;
end;

procedure TdmFSWorkspaceFiles.SetWorkspaceID(WorkspaceID: Integer);
begin

end;

end.
