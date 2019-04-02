unit RiggVar.DAL.AdapterExt;

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
  RiggVar.DAL.AdapterImpl,
  RiggVar.DAL.WorkspaceInfo,
  RiggVar.DAL.WorkspaceIntf,
  RiggVar.DAL.WorkspaceManager;

type
  TStoreAdapterEx = class(TStoreAdapter)
  private
    WorkspaceManager: TWorkspaceManager;
  public
    destructor Destroy; override;

    function GetWorkspaceType: Integer; override;
    function GetWorkspaceID: Integer; override;
    function GetDBWorkspace: IDBWorkspace; override;
    procedure InitWorkspaceManager; override;
    procedure SwapWorkspace(wi: TWorkspaceInfo); override;
    procedure UpdateWorkspace(WorkspaceType, WorkspaceID: Integer); override;
    procedure InitWorkspace(WorkspaceType: Integer; WorkspaceID: Integer); override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TStoreAdapter }

destructor TStoreAdapterEx.Destroy;
begin
  WorkspaceManager.Free;
  inherited;
end;

function TStoreAdapterEx.GetDBWorkspace: IDBWorkspace;
begin
  result := WorkspaceManager.DBWorkspace;
end;

function TStoreAdapterEx.GetWorkspaceID: Integer;
begin
  result := WorkspaceManager.WorkspaceID;
end;

function TStoreAdapterEx.GetWorkspaceType: Integer;
begin
  result := 0;
end;

procedure TStoreAdapterEx.InitWorkspace(WorkspaceType: Integer; WorkspaceID: Integer);
begin
  if Assigned(WorkspaceManager) then
    WorkspaceManager.Init(WorkspaceType, WorkspaceID);
end;

procedure TStoreAdapterEx.InitWorkspaceManager;
begin
  WorkspaceManager := TWorkspaceManager.Create;
end;

procedure TStoreAdapterEx.SwapWorkspace(wi: TWorkspaceInfo);
begin
  WorkspaceManager.InitNew(wi);
end;

procedure TStoreAdapterEx.UpdateWorkspace(WorkspaceType, WorkspaceID: Integer);
begin
  if (WorkspaceType <> Main.WorkspaceInfo.WorkspaceType)
    or (WorkspaceID <> Main.WorkspaceInfo.WorkspaceID) then
  begin
    InitWorkspace(WorkspaceType, WorkspaceID);
    Main.DisposeFormWorkspace;
  end;
end;

end.
