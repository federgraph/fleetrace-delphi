unit RiggVar.App.Main;

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
  Vcl.ComCtrls,
  RiggVar.App.Config,
  RiggVar.App.GuiManager,
  RiggVar.App.Translation,
  RiggVar.BO.AngularPost,
  RiggVar.BO.Container,
  RiggVar.BO.Def,
  RiggVar.BO.FolderInfo,
  RiggVar.BO.FormAdapter,
  RiggVar.BO.Localizer,
  RiggVar.BO.Manager,
  RiggVar.BO.MsgToken,
  RiggVar.BO.Params,
  RiggVar.BO.ResourceManager,
  RiggVar.DAL.AdapterImpl,
  RiggVar.DAL.Manager,
  RiggVar.DAL.WorkspaceInfo,
  RiggVar.EM.CategoryCache,
  RiggVar.EM.WorkspaceListBase,
  RiggVar.Util.Logger;

type
  TMain = class
  private
    FBOContainer: TBOContainer;
    FBlankout: Boolean;
    function GetHaveAppPermission: Boolean;
    function GetHaveLocalAccess: Boolean;
    function GetHaveSocketPermission: Boolean;
    function GetHaveWebPermission: Boolean;
    function GetBOManager: TBOManager;
    function GetTestData: string;
    function GetWebStatusString: string;
    procedure SetUseDB(const Value: Boolean);
    procedure ShowError(e: Exception);
  public
    Params: TMainParams;

    IniImage: TIniImage;
    FolderInfo: TFolderInfo;
    WorkspaceInfo: TWorkspaceInfo;

    Logger: TLogger;
    FormAdapter: TFormAdapterBase;
    StoreAdapter: TStoreAdapter;

    GuiManager: TGuiManager;
    DocManager: TDocManager;
    LanguageManager: TBaseLanguageManager;
    ResourceManager: TResourceManager;

    WorkspaceList: TWorkspaceListBase;
    CategoryCache: TCategoryCache;

    StartupLogger: TStringList;
    AppTranslator: TAppTranslator;
    AngularPost: TAngularPost;

    constructor Create;
    destructor Destroy; override;

    procedure ForceToolbarProps(tb: TToolbar);
    function GetStatusString: string;
    procedure RecreateBOFromBackup;
    procedure RecreateBO(BOParams: TBOParams);
    procedure LoadNew(const Data: string);
    procedure BeforeDeleteBO(Sender: TObject);
    procedure AfterDeleteBO(Sender: TObject);
    function ChooseDB: Boolean;
    function ChooseDocAvail(SL: TStringList): string;
    function ChooseNewEventName: string;
    function GetUseDB: Boolean;
    procedure ShowErrorMsg(msg: string);
    function Save(XML: Boolean = false): string;

    property BOManager: TBOManager read GetBOManager;
    property TestData: string read GetTestData;

    property HaveAppPermission: Boolean read GetHaveAppPermission;
    property HaveFilePermission: Boolean read GetHaveLocalAccess;
    property HaveSocketPermission: Boolean read GetHaveSocketPermission;
    property HaveWebPermission: Boolean read GetHaveWebPermission;
    property WebStatusString: string read GetWebStatusString;
    property UseDB: Boolean read GetUseDB write SetUseDB;
    property BlankOut: Boolean read FBlankOut;
  end;

var
  Main: TMain;

  IsWebApp: Boolean = false;
  IsService: Boolean = false;
  IsWinGUI: Boolean = false;
  IsTest: Boolean = false;
  WantErrorWindow: Boolean = true;

implementation

uses
  RiggVar.App.Injector,
  RiggVar.DAL.Redirector;

{ TMain }

constructor TMain.Create;
begin
  Main := self;
  inherited Create;
  FBlankout := False;

  cAppTitle := 'FR01';
  cTokenA := 'FR';
  SetDivisionName('*');

  Params := TMainParams.Create;
  Params.IsSpecialVersion := False;
  Params.WantFeatures := False;
  Params.WantWeb := False;

  StartupLogger := TStringList.Create;
  StartupLogger.Add('TMain.Create');

  try
    StartupLogger.Add('Settig DecimalSeparator to .');
    FormatSettings.DecimalSeparator := '.';

    if not Assigned(InjectorImpl) then
    begin
      StartupLogger.Add('Creating InjectorMock');
      InjectorImpl := TInjectorMock.Create;
    end;

    if InjectorImpl <> nil then
    begin
    StartupLogger.Add('Calling InjectDependencies');
    if not IsTest then
        InjectorImpl.InjectDependencies
    else
        InjectorImpl.InjectTestDependencies;
    end;

    if not Assigned(AppTranslator) then
    begin
      StartupLogger.Add('Creating AppTranslator');
      AppTranslator := TAppTranslator.Create;
    end;

    //create StoreAdapter first, then WorkspaceInfo
    // because WorkspaceInfo.Check will access Main.StoreAdapter
    if not Assigned(StoreAdapter) then
    begin
      StartupLogger.Add('Creating StoreAdapter');
      StoreAdapter := TStoreAdapter.Create;
    end;

    if not Assigned(WorkspaceInfo) then
    begin
      StartupLogger.Add('Creating WorkspaceInfo');
      WorkspaceInfo := TWorkspaceInfo.Create;
      WorkspaceInfo.Load;
      if WorkspaceInfo.HasError_WorkspaceRoot then
      begin
        StartupLogger.Add('Invalid configuration of WorkspaceInfo.WorkspaceRoot');
        ShowError(nil);
    end;
    end;

    // will do nothing if StoreAdapter is not instanceof StoreAdapterEx
    // but need WorkspaceInfo for construction of WorkspaceManager field in Ex
    StartupLogger.Add('Calling StoreAdapter.InitWorkspaceManager');
    StoreAdapter.InitWorkspaceManager;

    if not Assigned(FolderInfo) then
    begin
      StartupLogger.Add('Creating FolderInfo');
      FolderInfo := TFolderInfo.Create;
    end;

    //need WorkspaceInfo to resolve ConfigFileName in FolderInfo
    if not Assigned(IniImage) then
    begin
      StartupLogger.Add('Logging FolderInfo.ConfigFileName:');
      StartupLogger.Add(FolderInfo.ConfigFileName);
      if DBFileExists(FolderInfo.ConfigFileName) then
        StartupLogger.Add('Config file exists!')
      else
        StartupLogger.Add('Config file does not exists.');

      StartupLogger.Add('Creating IniImage');
      IniImage := TIniImage.Create;
      IniImage.DefaultEventType := C_DefaultEventType;
      IniImage.UseIni := false;
      IniImage.UsePortSet := false;
      //need FolderInfo and StoreAdapter (via ReadUserConfig and CreateDefaultIni)
      IniImage.Init;
    end;

    if not Assigned(Logger) then
    begin
      StartupLogger.Add('Creating Logger');
      Logger := TLogger.Create;
    end;

    StartupLogger.Add('Creating BOContainer');
    FBOContainer := TBOContainer.Create;
    StartupLogger.Add('Calling BOContainer.InitBO');
    FBOContainer.InitBO;

    StartupLogger.Add('Creating ResourceManager');
    ResourceManager := TResourceManager.Create;
    ResourceManager.UseResources := IniImage.UseResource;

    StartupLogger.Add('Creating DocManager');
    DocManager := TDocManager.Create(IniImage.DBInterface);

    if not Assigned(LanguageManager) then
    begin
      StartupLogger.Add('Creating LanguageManager');
      LanguageManager := TBaseLanguageManager.Create;
    end;

    StartupLogger.Add('Calling BO.Connect');
    BO.Connect;

    if not Assigned(FormAdapter) then
    begin
      StartupLogger.Add('Creating FormAdapter');
      FormAdapter := TFormAdapterBase.Create;
    end;

    StartupLogger.Add('Creating GuiManager');
    GuiManager := TGuiManager.Create;

    if not Assigned(WorkspaceList) then
    begin
      StartupLogger.Add('Creating WorkspaceList');
      WorkspaceList := TWorkspaceListBase.Create;
    end;
    StartupLogger.Add('Initializing WorkspaceList');
    WorkspaceList.Init(False);

    if not Assigned(CategoryCache) then
    begin
      StartupLogger.Add('Creating CategoryCache');
      CategoryCache := TCategoryCache.Create;
    end;

  StartupLogger.Add('Creating AngularPost');
  AngularPost := TAngularPost.Create;

  except on e: Exception do
    ShowError(e);
  end;
end;

destructor TMain.Destroy;
begin
  AngularPost.Free;
  AppTranslator.Free;
  StartupLogger.Free;

  CategoryCache.Free;
  WorkspaceList.Free;
  GuiManager.Free;

  FBOContainer.Free;
  FBOContainer := nil;

  DocManager.Free;
  LanguageManager.Free;

  FormAdapter.Free;
  IniImage.Free;

  WorkspaceInfo.Free;
  FolderInfo.Free;
  ResourceManager.Free;

  Logger.Free;
  BO.Free;
  Params.Free;
  StoreAdapter.Free;
  inherited;
end;

function TMain.GetBOManager: TBOManager;
begin
  result := FBOContainer;
end;

function TMain.GetHaveAppPermission: Boolean;
begin
  result := True;
end;

function TMain.GetHaveLocalAccess: Boolean;
begin
  result := IniImage.WantLocalAccess;
end;

function TMain.GetHaveSocketPermission: Boolean;
begin
  result := true;
end;

function TMain.GetHaveWebPermission: Boolean;
begin
  result := true;
end;

function TMain.GetStatusString: string;
var
  SL: TStringList;
begin
  result := '';
  SL := TStringList.Create;
  try
    try
      SL.Add('DateTimeToStr(Now): ' + DateTimeToStr(Now));

      SL.Add('');
      SL.Add('---- InputNCP');
      BO.InputServer.Server.ConnectionReport(SL);

      SL.Add('');
      SL.Add('---- OutputNCP');
      BO.OutputServer.Server.ConnectionReport(SL);

      SL.Add('');
      SL.Add('---- WorkspaceInfo');
      WorkspaceInfo.WorkspaceReport(SL);

      result := SL.Text;
    finally
      SL.Free;
    end;
  except
  end;
end;

function TMain.Save(XML: Boolean): string;
begin
  result := BO.Save(Xml);
end;

function TMain.GetUseDB: Boolean;
begin
  result := Params.UseDB;
end;

procedure TMain.SetUseDB(const Value: Boolean);
begin
  Params.UseDB := Value;
end;

function TMain.GetWebStatusString: string;
var
  SL: TStringList;
  i: Integer;
  s: string;
begin
  result := '';
  SL := TStringList.Create;
  try
    try
      if BO.Connected then
        SL.Add('Connected')
      else
        SL.Add('Disconnected');

      s := DocManager.DBInterface;
      SL.Add(s);

      s := WorkspaceInfo.WorkspaceTypeName;
      SL.Add(s);

      i := WorkspaceInfo.WorkspaceID;
      SL.Add(IntToStr(i));

      if WorkspaceInfo.WorkspaceType = 5 then
      begin
        s := WorkspaceInfo.WorkspaceUrl;
        SL.Add(s);
      end;

      s := SL[0];
      for i := 1 to SL.Count - 1 do
        s := s + ' | ' + SL[i];

      result := s;
    finally
      SL.Free;
    end;
  except
  end;
end;

function TMain.GetTestData: string;
begin
  result := FBOContainer.GetTestData;
end;

procedure TMain.LoadNew(const Data: string);
begin
  FBOContainer.LoadNew(Data)
end;

procedure TMain.RecreateBO(BOParams: TBOParams);
begin
  FBOContainer.RecreateBO(BOParams);
end;

procedure TMain.RecreateBOFromBackup;
begin
  FBOContainer.RecreateBOFromBackup;
end;

procedure TMain.BeforeDeleteBO(Sender: TObject);
begin
  FormAdapter.BeforeDeleteBO(Sender);
end;

procedure TMain.AfterDeleteBO(Sender: TObject);
begin
  FormAdapter.AfterDeleteBO(Sender);
end;

function TMain.ChooseDocAvail(SL: TStringList): string;
begin
  result := FormAdapter.ChooseDocAvail(SL);
end;

function TMain.ChooseDB: Boolean;
begin
  //update of IniImage expected, returns true if changed
  result := FormAdapter.ChooseDB;
end;

function TMain.ChooseNewEventName: string;
begin
  result := FormAdapter.ChooseNewEventName;
end;

procedure TMain.ShowError(e: Exception);
begin
  if Assigned(e) then
  begin
  StartupLogger.Add('');
  StartupLogger.Add('Exception.Message:');
  StartupLogger.Add(e.Message);
  if (e.InnerException <> nil) then
  begin
    StartupLogger.Add('');
    StartupLogger.Add('Exception.InnerException.Message:');
    StartupLogger.Add(e.InnerException.Message);
  end;
  StartupLogger.Add('');
  StartupLogger.Add('Exception.StackTrace:');
  StartupLogger.Add(e.StackTrace);
  end;
end;

procedure TMain.ShowErrorMsg(msg: string);
begin
  FormAdapter.ShowError(msg);
end;

procedure TMain.ForceToolbarProps(tb: TToolbar);
begin
  tb.Caption := '';
  tb.Transparent := False;
  tb.BorderWidth := 1;
  tb.ButtonHeight := 22;
  tb.ButtonWidth := 23;
  tb.AutoSize := True;
end;

end.
