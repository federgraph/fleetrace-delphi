unit RiggVar.DAL.AdapterBase;

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
  RiggVar.BO.StoreAdapter,
  RiggVar.DAL.WorkspaceIntf,
  RiggVar.DAL.Manager,
  RiggVar.DAL.WorkspaceInfo;

type
  TStoreAdapterBase2 = class(TStoreAdapterBase)
  public
    procedure CheckWorkspaceInfo(wi: TWorkspaceInfo); virtual;
    function GetWorkspaceTypeName(wi: TWorkspaceInfo): string; virtual;
    function GetDBWorkspace: IDBWorkspace; virtual;
    procedure SwapWorkspace(wi: TWorkspaceInfo); virtual;
  end;

implementation

{ TStoreAdapterBase2 }

function TStoreAdapterBase2.GetWorkspaceTypeName(wi: TWorkspaceInfo): string;
begin
  result := '';
end;

procedure TStoreAdapterBase2.SwapWorkspace(wi: TWorkspaceInfo);
begin

end;

procedure TStoreAdapterBase2.CheckWorkspaceInfo(wi: TWorkspaceInfo);
begin

end;

function TStoreAdapterBase2.GetDBWorkspace: IDBWorkspace;
begin
  result := nil;
end;

end.
