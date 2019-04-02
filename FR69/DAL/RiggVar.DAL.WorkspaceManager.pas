unit RiggVar.DAL.WorkspaceManager;

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
  RiggVar.DAL.WorkspaceIntf,
  RiggVar.DAL.WorkspaceInfo;

type
  TWorkspaceManager = class
  private
    FDBWorkspace: IDBWorkspace;
    function GetWorkspaceID: Integer;
    procedure SetWorkspaceID(const Value: Integer);
    function GetWorkspaceType: Integer;
    procedure SetWorkspaceType(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(aWorkspaceType: Integer; aWorkspaceID: Integer);
    procedure InitNew(wsi: TWorkspaceInfo);

    property WorkspaceID: Integer read GetWorkspaceID write SetWorkspaceID;
    property WorkspaceType: Integer read GetWorkspaceType;
    property DBWorkspace: IDBWorkspace read FDBWorkspace;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.DAL.Redirector,
  RiggVar.DAL.WorkspaceTXT,
  RiggVar.DAL.WorkspaceSQL,
  RiggVar.DAL.WorkspaceWEB;

{ TWorkspaceManager }

constructor TWorkspaceManager.Create;
var
  info: TWorkspaceInfo;
begin
  inherited Create;
  info := Main.WorkspaceInfo;
  Init(info.WorkspaceType, info.WorkspaceID);
end;

destructor TWorkspaceManager.Destroy;
begin
  FDBWorkspace := nil;
  inherited Destroy;
end;

function TWorkspaceManager.GetWorkspaceID: Integer;
begin
  if Assigned(DBWorkspace) then
    result := DBWorkspace.GetWorkspaceID
  else
    result := WorkspaceID_Default;
end;

function TWorkspaceManager.GetWorkspaceType: Integer;
begin
  if Assigned(Main) and Assigned(Main.WorkspaceInfo) then
  begin
    result := Main.WorkspaceInfo.WorkspaceType;
  end
  else
    result := WorkspaceType_Default;
end;

procedure TWorkspaceManager.Init(aWorkspaceType: Integer; aWorkspaceID: Integer);
begin
  WorkspaceID := aWorkspaceID;

  if not Assigned(FDBWorkspace) or (aWorkspaceType <> WorkspaceType) then
  case aWorkspaceType of
    WorkspaceType_SharedFS:
    begin
      FDBWorkspace := TdmFSWorkspaceFiles.Create;
      Main.UseDB := False;
    end;
    WorkspaceType_PrivateFS:
    begin
      FDBWorkspace := TdmFSWorkspaceFiles.Create;
      Main.UseDB := False;
    end;
    WorkspaceType_RemoteDB:
    begin
      FDBWorkspace := TSQLWorkspaceFiles.Create;
      Main.UseDB := Assigned(DBWorkspace);
    end;
    WorkspaceType_WebService:
    begin
      FDBWorkspace := TWorkspaceWeb.Create;
      Main.UseDB := Assigned(DBWorkspace);
    end;
    WorkspaceType_FixedFS:
    begin
      FDBWorkspace := TdmFSWorkspaceFiles.Create;
      Main.UseDB := False;
    end;
    else
    begin
      FDBWorkspace := TdmFSWorkspaceFiles.Create;
      Main.UseDB := False;
    end;
  end;

  if Assigned(DBWorkspace) then
  begin
    SetWorkspaceType(aWorkspaceType); //update WorkspaceInfo and FolderInfo
    SetWorkspaceID(aWorkspaceID);
  end;
end;

procedure TWorkspaceManager.InitNew(wsi: TWorkspaceInfo);
begin
  FDBWorkspace := nil;
  Init(wsi.WorkspaceType, wsi.WorkspaceID);
end;

procedure TWorkspaceManager.SetWorkspaceType(Value: Integer);
begin
  if Assigned(Main.FolderInfo) and Assigned(Main.WorkspaceInfo) then
  begin
    Main.WorkspaceInfo.WorkspaceType := Value;
    Main.FolderInfo.WorkspaceInfoChanged;
  end;
end;

procedure TWorkspaceManager.SetWorkspaceID(const Value: Integer);
begin
  if Assigned(DBWorkspace) and Assigned(Main.WorkspaceInfo) then
  begin
    DBWorkspace.SetWorkspaceID(Value);
    Main.WorkspaceInfo.WorkspaceID := Value;
  end;
end;

end.
