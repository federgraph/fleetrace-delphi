unit RiggVar.DAL.WorkspaceWEB;

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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Data.DB,
  RiggVar.DAL.WorkspaceIntf,
  WorkspaceService;

type
  TWorkspaceWeb = class(TInterfacedObject, IDBWorkspace)
  private
    SL: TStrings;
    FWorkspaceID: Integer;
    procedure ShowError(e: Exception; mn: string);
    function GetWorkspaceUrl: string;
  protected
    procedure SetWorkspaceID(WorkspaceID: Integer); virtual;
    function GetWorkspaceID: Integer;
    function DBGetEventNames(ExtensionFilter: string): string;
    function DBFileExists(const fn: string): Boolean;
    function DBDirectoryExists(const dn: string): Boolean;
    procedure DBLoadFromFile(const fn: string; SL: TStrings);
    procedure DBSaveToFile(const fn: string; SL: TStrings);
    function DBDeleteWorkspace: Boolean;
    function DBDeleteFile(const fn: string): Boolean;
    function GetDataSet: TDataSet; virtual;
    procedure ExportFile(const WorkspaceDir: string); virtual;
    procedure ExportFiles(const WorkspaceDir: string); virtual;
    property WorkspaceUrl: string read GetWorkspaceUrl;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TWorkspaceWeb }

constructor TWorkspaceWeb.Create;
begin
  inherited Create;
  FWorkspaceID := 1;
  SL := TStringList.Create;
end;

destructor TWorkspaceWeb.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

procedure TWorkspaceWeb.SetWorkspaceID(WorkspaceID: Integer);
begin
  FWorkspaceID := WorkspaceID;
end;

procedure TWorkspaceWeb.ShowError(e: Exception; mn: string);
begin
  if IsWinGui then
    if e.ClassName = 'EDOMParseError' then
      if mn = 'DBGetEventNames' then
        Main.ShowErrorMsg('please login to the web-application first...');
  Main.Logger.Error(e.ClassName);
  Main.Logger.Error(e.Message + ' when calling TWorkspaceWeb.' + mn);
end;

function TWorkspaceWeb.GetWorkspaceID: Integer;
begin
  result := FWorkspaceID;
end;

function TWorkspaceWeb.GetWorkspaceUrl: string;
begin
  result := Main.WorkspaceInfo.WorkspaceUrl;
end;

function TWorkspaceWeb.DBDeleteWorkspace: Boolean;
begin
  try
    WorkspaceService.WorkspaceServerName := WorkspaceUrl;
    result := WorkspaceService.GetWorkspaceFilesSoap.DBDeleteWorkspace(FWorkspaceID);
  except
    on e: Exception do
    begin
      ShowError(e, 'DBDeleteWorkspace');
      result := false;
    end;
  end;
end;

function TWorkspaceWeb.DBDeleteFile(const fn: string): Boolean;
begin
  try
    WorkspaceService.WorkspaceServerName := WorkspaceUrl;
    result := WorkspaceService.GetWorkspaceFilesSoap.DBDeleteFile(FWorkspaceID, fn);
  except
    on e: Exception do
    begin
      ShowError(e, 'DBDeleteFile');
      result := false;
    end;
  end;
end;

function TWorkspaceWeb.DBDirectoryExists(const dn: string): Boolean;
begin
  result := true;
end;

function TWorkspaceWeb.DBFileExists(const fn: string): Boolean;
begin
  try
    WorkspaceService.WorkspaceServerName := WorkspaceUrl;
    result := WorkspaceService.GetWorkspaceFilesSoap.DBFileExists(FWorkspaceID, fn);
  except
    on e: Exception do
    begin
      ShowError(e, 'DBFileExists');
      result := false;
    end;
  end;
end;

procedure TWorkspaceWeb.DBLoadFromFile(const fn: string; SL: TStrings);
var
  s: string;
begin
  try
    WorkspaceService.WorkspaceServerName := WorkspaceUrl;
    s := WorkspaceService.GetWorkspaceFilesSoap.DBLoadFromFile(FWorkspaceID, fn);
    SL.Text := s;
  except
    on e: Exception do
    begin
      ShowError(e, 'DBLoadFromFile');
    end;
  end;
end;

procedure TWorkspaceWeb.DBSaveToFile(const fn: string; SL: TStrings);
begin
  try
    WorkspaceService.WorkspaceServerName := WorkspaceUrl;
    WorkspaceService.GetWorkspaceFilesSoap.DBSaveToFile(FWorkspaceID, fn, SL.Text);
  except
    on e: Exception do
    begin
      ShowError(e, 'DBSaveToFile');
    end;
  end;
end;

function TWorkspaceWeb.DBGetEventNames(ExtensionFilter: string): string;
begin
  result := '';
  try
    WorkspaceService.WorkspaceServerName := WorkspaceUrl;
    result := WorkspaceService.GetWorkspaceFilesSoap.DBGetEventNames(FWorkspaceID, ExtensionFilter);
  except
    on e: Exception do
    begin
      ShowError(e, 'DBGetEventNames');
    end;
  end;
end;

function TWorkspaceWeb.GetDataSet: TDataSet;
begin
  result := nil; //not implemented here
end;

procedure TWorkspaceWeb.ExportFile(const WorkspaceDir: string);
begin
  //not implemented here
end;

procedure TWorkspaceWeb.ExportFiles(const WorkspaceDir: string);
begin
  //not implemented here
end;

end.
