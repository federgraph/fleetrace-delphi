unit RiggVar.DAL.WorkspaceRepo;

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
  System.Classes,
  RiggVar.DAL.WorkspaceIntf,
  RiggVar.DAL.WorkspaceInfo;

type
  TRepoItems = class
  private
    FCount: Integer;
    SLUrl: TStringList;
    SLRoot: TStringList;
    SLType: TStringList;
    procedure AddRepoItem(ScenarioID: Integer);
    function GetItems(Index: Integer): TWorkspaceInfo;
    function GetTypeName(WST: Integer): string;
  protected
    Repo: array of TWorkspaceInfo;
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateUrlCombo(SL: TStrings);
    procedure UpdateRootCombo(SL: TStrings);
    procedure UpdateRepoCombo(SL: TStrings);
    procedure UpdateTypeCombo(SL: TStrings);
    //function FindByName(name: string): TWorkspaceInfo;
    property Count: Integer read FCount;
    property Items[Index: Integer]: TWorkspaceInfo read GetItems;
  end;

implementation

{ TRepoItems }

constructor TRepoItems.Create;
begin
  SLType := TStringList.Create;
  SLUrl := TStringList.Create;
  SLRoot := TStringList.Create;
  Init;
end;

destructor TRepoItems.Destroy;
var
  i: Integer;
begin
  SLType.Free;
  SLUrl.Free;
  SLRoot.Free;
  for i := 0 to Length(Repo) - 1 do
    Repo[i].Free;
  inherited;
end;

procedure TRepoItems.Init;
var
  i: Integer;
  SL: TStringList;
begin
  SL := SLUrl;
  SL.Add('http://localhost/WebApplication/');
  SL.Add('http://gsup3/FR99/');
  SL.Add('http://gshsm/FR64/');
  SL.Add('http://riggvar.net/cgi-bin/FR99/');
  SL.Add('http://localhost:8199/FR99/');

  SL := SLRoot;
  SL.Add('<UserHome>\RiggVar Workspace');
  SL.Add('D:\Test\Workspace\FR64');
  SL.Add('C:\Users\Public\Documents\Workspace\FR64');

  SL := SLType;
  FCount := 7;
  for i := 0 to FCount do
    SL.Add(GetTypeName(i));

  SetLength(Repo, FCount);
  for i := 0 to Count-1 do
    AddRepoItem(i);
end;

function TRepoItems.GetTypeName(WST: Integer): string;
begin
  case WST of
    WorkspaceType_Unknown: result := 'unknown';
    WorkspaceType_WebService: result := 'Web Service';
    WorkspaceType_SharedFS: result := 'Shared FS';
    WorkspaceType_PrivateFS: result := 'Private FS';
    WorkspaceType_FixedFS: result := 'Fixed FS';
    WorkspaceType_LocalDB: result := 'Local DB';
    WorkspaceType_RemoteDB: result := 'Remote DB';
    else result := 'invalid';
  end;
end;

procedure TRepoItems.AddRepoItem(ScenarioID: Integer);
var
  ri: TWorkspaceInfo;
begin
  if ScenarioID < Length(Repo) then
  begin
    ri := TWorkspaceInfo.Create;
    Repo[ScenarioID] := ri;
    case ScenarioID of
      0:
      begin
        ri.WorkspaceName := 'current'; //placeholder
//        ri.WorkspaceType := wi.WorkspaceType;
//        ri.WorkspaceID := wi.WorkspaceID;
//        ri.WorkspaceUrl := wi.WorkspaceUrl;
//        ri.WorkspaceRoot := wi.WorkspaceRoot;
//        ri.AutoSaveIni := wi.AutoSaveIni;
      end;

      1: begin
        ri.WorkspaceName := 'gsup3-shared';
        ri.WorkspaceType := WorkspaceType_SharedFS;
        ri.WorkspaceID := 1;
        ri.WorkspaceUrl := 'http://gsup3/FR99/';
        ri.WorkspaceRoot := 'D:\Test\Workspace\FR64';
        ri.AutoSaveIni := false;
      end;

      2: begin
        ri.WorkspaceName := 'gsup3-web';
        ri.WorkspaceType := WorkspaceType_WebService;
        ri.WorkspaceID := 5;
        ri.WorkspaceUrl := 'http://gsup3/FR99/';
        ri.WorkspaceRoot := 'n/a';
        ri.AutoSaveIni := false;
      end;

      3: begin
        ri.WorkspaceName := 'gshsm-shared';
        ri.WorkspaceType := WorkspaceType_SharedFS;
        ri.WorkspaceID := 1;
        ri.WorkspaceUrl := 'http://gshsm/FR64/';
        ri.WorkspaceRoot := 'C:\Perforce\gshsm\RiggVar Workspace';
        ri.AutoSaveIni := false;
      end;

      4: begin
        ri.WorkspaceName := 'gshsm-web';
        ri.WorkspaceType := WorkspaceType_WebService;
        ri.WorkspaceID := 5;
        ri.WorkspaceUrl := 'http://gshsm/FR64/';
        ri.WorkspaceRoot := 'n/a';
        ri.AutoSaveIni := false;
      end;

      5: begin
        ri.WorkspaceName := 'gsup3-fixed';
        ri.WorkspaceType := WorkspaceType_FixedFS;
        ri.WorkspaceID := 1;
        ri.WorkspaceUrl := 'http://gsup3/FR99/';
        ri.WorkspaceRoot := 'D:\Test\Workspace\FR64';
        ri.AutoSaveIni := true;
      end;

      6: begin
        ri.WorkspaceName := 'gsmac-fixed';
        ri.WorkspaceType := WorkspaceType_FixedFS;
        ri.WorkspaceID := 1;
        ri.WorkspaceUrl := 'http://gsmac/FR64/';
        ri.WorkspaceRoot := 'C:\Users\Public\Documents\Workspace\FR64';
        ri.AutoSaveIni := true;
      end;

      else
      begin
        ri.WorkspaceName := 'default-shared';
        ri.WorkspaceType := WorkspaceType_SharedFS;
        ri.WorkspaceID := 1;
        ri.WorkspaceUrl := 'n/a';
        ri.WorkspaceRoot := 'n/a';
        ri.AutoSaveIni := false;
      end;
    end;
  end;
end;

function TRepoItems.GetItems(Index: Integer): TWorkspaceInfo;
begin
  if (Index >= 0) and (Index < Count) then
    result := Repo[Index]
  else
    result := nil;
end;

//function TRepoItems.FindByName(name: string): TWorkspaceInfo;
//var
//  i: Integer;
//  ri: TWorkspaceInfo;
//begin
//  result := nil;
//  for i := 0 to Length(Repo) - 1 do
//  begin
//    ri := Repo[i];
//    if ri.WorkspaceName = name then
//    begin
//      result := ri;
//      break;
//    end;
//  end;
//end;

procedure TRepoItems.UpdateRepoCombo(SL: TStrings);
var
  i: Integer;
begin
  for i := 0 to Length(Repo) - 1 do
    SL.Add(Repo[i].WorkspaceName);
end;

procedure TRepoItems.UpdateRootCombo(SL: TStrings);
begin
  SL.Assign(SLRoot);
end;

procedure TRepoItems.UpdateTypeCombo(SL: TStrings);
begin
  SL.Assign(SLType);
end;

procedure TRepoItems.UpdateUrlCombo(SL: TStrings);
begin
  SL.Assign(SLUrl);
end;

end.
