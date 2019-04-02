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
  System.StrUtils,
  Vcl.Forms,
  Vcl.ComCtrls,
  RiggVar.App.Config,
  RiggVar.App.GuiManager,
  RiggVar.App.TestCounter,
  RiggVar.App.Translation,
  RiggVar.BO.AngularPost,
  RiggVar.BO.Base,
  RiggVar.BO.BridgeAdapter,
  RiggVar.BO.Container,
  RiggVar.BO.Def,
  RiggVar.BO.FolderInfo,
  RiggVar.BO.FormAdapter,
  RiggVar.BO.Localizer,
  RiggVar.BO.Manager,
  RiggVar.BO.MsgBase,
  RiggVar.BO.NetworkAdapter,
  RiggVar.BO.Params,
  RiggVar.BO.ResourceManager,
  RiggVar.BO.ScenarioManager,
  RiggVar.BO.StoreAdapter,
  RiggVar.BR.BridgeServer,
  RiggVar.BR.BridgeClient,
  RiggVar.BR.PeerController,
  RiggVar.BR.PeerManager,
  RiggVar.Calc.EventProxyModule,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.DAL.AdapterExt,
  RiggVar.DAL.Manager,
  RiggVar.DAL.WorkspaceInfo,
  RiggVar.DAL.WorkspaceIntf,
  RiggVar.DAL.WorkspaceManager,
  RiggVar.EM.CategoryCache,
  RiggVar.EM.WorkspaceListBase,
  RiggVar.Out.Publisher,
  RiggVar.Util.Logger,
  RiggVar.Util.PolicyServer,
  RiggVar.Util.PortTest,
  RiggVar.Util.Sound,
  RiggVar.Util.WebUtils,
  RiggVar.Web1.Images,
  RiggVar.Web4.JsonCache;

type
  TMain = class
  private
    FBOContainer: TBOContainer;
    FBlankout: Boolean;
    function GetHaveAppPermission: Boolean;
    function GetHaveLocalAccess: Boolean;
    function GetHaveSocketPermission: Boolean;
    function GetHaveWebPermission: Boolean;
    function GetAdapterBO: TAdapterBO;
    function GetBOManager: TBOManager;
    function GetPeerController: TPeerController;
    function GetIsClientBridge: Boolean;
    function GetIsServerBridge: Boolean;
    function GetIsScenarioEC2: Boolean;
    function GetClientBridge: TClientBridge;
    function GetServerBridge: TServerBridge;
    function GetScenario: TConfigScenario;
    function GetTestData: string;
    function GetWebStatusString: string;
    procedure SetUseDB(const Value: Boolean);
    procedure ShowError(e: Exception);
    procedure NetReport(SL: TStringList);
    procedure EvaluateStartupScenario;
    procedure InitBridgeClientScenario;
    procedure InitDisconnectedScenario;
    procedure InitBridgeServerScenario(BridgeHost: string);
    procedure InitReadonlyScenario;
    procedure InitWebonlyScenario;
    procedure InitOutputClientScenario;
  public
    Params: TMainParams;

    IniImage: TIniImage;
    FolderInfo: TFolderInfo;
    WorkspaceInfo: TWorkspaceInfo;
    ScenarioInfo: TScenarioInfo;

    Logger: TLogger;
    FormAdapter: TFormAdapterBase;
    StoreAdapter: TStoreAdapterEx;
    BridgeAdapter: TBridgeAdapterBase;

    GuiManager: TGuiManager;
    DocManager: TDocManager;
    LanguageManager: TBaseLanguageManager;
    ResourceManager: TResourceManager;

    WorkspaceList: TWorkspaceListBase;
    CategoryCache: TCategoryCache;
    JsonCache: TJsonCache;
    ImageManager: TImageManager;
    SoundManager: TSoundManager;
    PeerManager: TPeerManager;

    ScoringModule: TScoringModule;
    Publisher: TPublisher;
    PolicyProvider: TPolicyProvider;
    PortTester: TPortTester;
    StartupLogger: TStringList;
    AppTranslator: TAppTranslator;
    AngularPost: TAngularPost;
    TestCounter: TTestCounter;

    constructor Create;
    destructor Destroy; override;

    procedure ForceToolbarProps(tb: TToolbar);
    function GetCubeFaceText(n: Integer): string;
    function GetStatusString: string;
    procedure RecreateBOFromBackup;
    procedure RecreateBO(BOParams: TBOParams);
    procedure LoadNew(const Data: string);
    procedure BeforeDeleteBO(Sender: TObject);
    procedure AfterDeleteBO(Sender: TObject);
    function ChooseDB: Boolean;
    function ChooseDocAvail(SL: TStringList): string;
    function ChooseNewEventName: string;
    function GetStoreAdapter: TStoreAdapterBase;
    function GetUseDB: Boolean;
    function GetFolderInfoDataPath: string;
    function GetDefaultEventName: string;
    function GetDefaultEventType: Integer;
    function GetBOData(WantXml: Boolean): string;
    function GetPrefix(KatID: Integer): string;
    function GetSuffix(KatID: Integer): string;
    function GetEventNamesDB(KatID: Integer): string;
    function EditDBEvent: Boolean;
    procedure SetDBInterface(Value: string);
    function WantOutput: Boolean;
    function GetUseUnicode: Boolean;
    function GetDBWorkspace: IDBWorkspace;
    procedure ShowErrorMsg(msg: string);
    procedure DisposeFormWorkspace;
    function Save(XML: Boolean = false): string;
    procedure InitBatchRequestItems(ML: TStrings);

    procedure InjectClientBridgeMsg(s: string);
    procedure InjectServerBridgeMsg(cm: TContextMsg);
    procedure InitPeer;
    procedure InitPolicyProvider;

    property PeerController: TPeerController read GetPeerController;
    property AdapterBO: TAdapterBO read GetAdapterBO;
    property BOManager: TBOManager read GetBOManager;
    property TestData: string read GetTestData;

    property HaveAppPermission: Boolean read GetHaveAppPermission;
    property HaveFilePermission: Boolean read GetHaveLocalAccess;
    property HaveSocketPermission: Boolean read GetHaveSocketPermission;
    property HaveWebPermission: Boolean read GetHaveWebPermission;
    property WebStatusString: string read GetWebStatusString;

    property IsServerBridge: Boolean read GetIsServerBridge;
    property ServerBridge: TServerBridge read GetServerBridge;
    property IsClientBridge: Boolean read GetIsClientBridge;
    property ClientBridge: TClientBridge read GetClientBridge;
    property Scenario: TConfigScenario read GetScenario;
    property IsScenarioEC2: Boolean read GetIsScenarioEC2;
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
  RiggVar.BO.MsgToken,
  RiggVar.BO.TemplateIDs,
  RiggVar.BR.BridgeAbstract,
  RiggVar.BR.BridgeController,
  RiggVar.DAL.Redirector;

{ TMain }

constructor TMain.Create;
begin
  Main := self;
  inherited Create;
  FBlankout := False;

  cAppTitle := 'FR62';
  cTokenA := 'FR';
  SetDivisionName('*');

  Params := TMainParams.Create;
  Params.IsSpecialVersion := False;
  Params.WantFeatures := True;
  Params.WantWeb := True;

  StartupLogger := TStringList.Create;
  StartupLogger.Add('TMain.Create');

  TestCounter := TTestCounter.Create;
  StartupLogger.Add('Creating TestCounter');

  StartupLogger.Add('Creating ScenarioInfo');
  ScenarioInfo := TScenarioInfo.Create;

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

    StartupLogger.Add('Creating SoundManager');
    SoundManager := TSoundManager.Create;
    SoundManager.Enabled := False;
    SoundManager.UseResource := True;

    StartupLogger.Add('Creating PortTester');
    PortTester := TPortTester.Create;

    if not Assigned(BridgeAdapter) then
    begin
      StartupLogger.Add('Creating BridgeAdapter');
      BridgeAdapter := TBridgeAdapterBase.Create;
    end;

    //create StoreAdapter first, then WorkspaceInfo
    // because WorkspaceInfo.Check will access Main.StoreAdapter
    if not Assigned(StoreAdapter) then
    begin
      StartupLogger.Add('Creating StoreAdapter');
      StoreAdapter := TStoreAdapterEx.Create;
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

    //need WorkspaceInfo to resolve resolve ConfigFileName in FolderInfo
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
      IniImage.UseIni := true;
      IniImage.UsePortSet := false;
      //need FolderInfo and StoreAdapter (via ReadUserConfig and CreateDefaultIni)
      IniImage.Init;
    end;

    {  provide configuration in Scenario.Config.txt file or
       hold shift key and double click exe to get scenario selector or }
    if (ScenarioInfo.Scenario = Scenario_ShowSelector) or
       (ssShift in KeyboardStateToShiftState) or
       (IniImage.WantScenarioSelector and (ScenarioInfo.Scenario = Scenario_Unknown))
    then
    begin
      StartupLogger.Add('Calling ShowScenarioSelector');
      InjectorImpl.ShowScenarioSelector;
    end;

    EvaluateStartupScenario;

    if not Assigned(Logger) then
    begin
      StartupLogger.Add('Creating Logger');
      Logger := TLogger.Create;
    end;

    StartupLogger.Add('Creating BOContainer');
    FBOContainer := TBOContainer.Create;
    if C_WantAdapter and IniImage.WantSockets then
    begin
      StartupLogger.Add('Calling BOManager.CreateAdapterBO');
      BOManager.CreateAdapterBO;
    end;
    StartupLogger.Add('Calling BOContainer.InitBO');
    FBOContainer.InitBO;

    StartupLogger.Add('Creating ResourceManager');
    ResourceManager := TResourceManager.Create;
    ResourceManager.UseResources := IniImage.UseResource;

    StartupLogger.Add('Creating DocManager');
    DocManager := TDocManager.Create(IniImage.DBInterface);

    StartupLogger.Add('Creating PeerManager');
    PeerManager := TPeerManager.Create(IniImage.BridgeProvider);

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

    StartupLogger.Add('Creating ScoringModule');
    ScoringModule := TScoringModule.Create(IniImage.ScoringModuleName);

    StartupLogger.Add('Creating Publisher');
    Publisher := TPublisher.Create;

    StartupLogger.Add('Creating ImageManager');
    ImageManager := TImageManager.Create;
    ImageManager.UseResources := IniImage.UseResource;

    StartupLogger.Add('Setting IniImage.AutoSaveIni');
    IniImage.AutoSaveIni := WorkspaceInfo.AutoSaveIni;

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

    if not Assigned(JsonCache) then
    begin
      StartupLogger.Add('Creating JsonCache');
      JsonCache := TJsonCache.Create;
      JsonCache.Init(3);
    end;

    StartupLogger.Add('Calling InitPeer');
    InitPeer;

    StartupLogger.Add('Creating PolicyProvider');
    PolicyProvider := TPolicyProvider.Create;

{
    if FormSplash <> nil then
    begin
      StartupLogger.Add('Closing Splash');
      FormSplash.Close;
      FormSplash.Free;
    end;
}

    if Params.AutoPolicyServer then
    begin
      StartupLogger.Add('Starting PolicyServer');
      InitPolicyProvider;
    end;

    if Params.AutoSilverlightWeb then
    begin
      if Assigned(GuiManager.SilverlightWeb) then
      begin
        StartupLogger.Add('Initializing Silverlight Web');
        GuiManager.SilverlightWeb.Init;
      end;
    end;

    if Params.AutoHomeWeb then
    begin
      if Assigned(GuiManager.WebMotorHome) then
      begin
        StartupLogger.Add('Initializing Home Web');
        GuiManager.WebMotorHome.Init;
      end;
    end;

    if Params.AutoRemoteWeb then
    begin
      if Assigned(GuiManager.WebMotorRemote) then
      begin
        StartupLogger.Add('Initializing Remote Web');
        GuiManager.WebMotorRemote.Init;
      end;
    end;

    //see GuiManager.SwapEvent;
    if PeerController.IsEnabled(SwitchOp_Upload) then
    begin
      if Params.AutoUpload then
      begin
        StartupLogger.Add('Calling PeerController.Upload');
        PeerController.Upload(BO.Save(false));
      end;
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
  JsonCache.Free;
  WorkspaceList.Free;
  GuiManager.Free;
  Publisher.Free;

  PeerManager.Free;
  PeerManager := nil;

  FBOContainer.Free;
  FBOContainer := nil;

  DocManager.Free;
  LanguageManager.Free;
  BridgeAdapter.Free;

  FormAdapter.Free;
  IniImage.Free;

  ScoringModule.Free;
  ScoringModule := nil;
  WorkspaceInfo.Free;
  FolderInfo.Free;
  ResourceManager.Free;
  ImageManager.Free;
  PortTester.Free;
  PolicyProvider.Free;
  SoundManager.Free;

  Logger.Free;
  BO.Free;
  Params.Free;
  ScenarioInfo.Free;
  StoreAdapter.Free;
  TestCounter.Free;
  inherited;
end;

procedure TMain.DisposeFormWorkspace;
begin
  if Assigned(FormAdapter) then
    FormAdapter.DisposeFormWorkspace;
end;

function TMain.GetAdapterBO: TAdapterBO;
begin
  result := BOManager.AdapterBO;
end;

function TMain.GetBOManager: TBOManager;
begin
  result := FBOContainer;
end;

function TMain.GetPeerController: TPeerController;
begin
  if PeerManager = nil then
    result := nil //should never happen!
  else
    result := PeerManager.PeerController;
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

function TMain.GetIsScenarioEC2: Boolean;
begin
  result := Scenario = TConfigScenario.Scenario_EC2;
end;

function TMain.GetIsServerBridge: Boolean;
begin
  result := ServerBridge <> nil;
end;

function TMain.GetScenario: TConfigScenario;
begin
  result := ScenarioInfo.Scenario;
end;

function TMain.GetServerBridge: TServerBridge;
var
  BridgeController: TBridgeController;
  Bridge: TBridge;
begin
  result := nil;
  if (PeerManager.ProviderID = 2) then //Bridge
  begin
    if PeerManager.PeerController is TBridgeController then
    begin
      BridgeController := PeerController as TBridgeController;
      if BridgeController.ProxyType = 4 then //ServerBridge
      begin
        Bridge := BridgeController.Bridge;
        if Bridge is TServerBridge then
        begin
          result := Bridge as TServerBridge;
        end;
      end;
    end;
  end;
end;

function TMain.GetIsClientBridge: Boolean;
begin
  result := ClientBridge <> nil;
end;

function TMain.GetClientBridge: TClientBridge;
var
  BridgeController: TBridgeController;
  Bridge: TBridge;
begin
  result := nil;
  if (PeerManager.ProviderID = 2) then //Bridge
  begin
    if PeerManager.PeerController is TBridgeController then
    begin
      BridgeController := PeerController as TBridgeController;
      //if (BridgeController.ProxyType = 3 then //ClientBridge
      //AsynchronBridge, ClientBridge, ProxyBridge, CombiBridge
      begin
        Bridge := BridgeController.Bridge;
        if Bridge is TClientBridge then
        begin
          result := Bridge as TClientBridge;
        end;
      end;
    end;
  end;
end;

function TMain.GetStatusString: string;
var
  SL: TStringList;
  i: Integer;
begin
  result := '';
  SL := TStringList.Create;
  try
    try
      SL.Add('DateTimeToStr(Now): ' + DateTimeToStr(Now));
      SL.Add('Scenario: ' + ScenarioInfo.ToString);

      SL.Add('');
      SL.Add('---- Host Info');
      SL.Add('BridgHost = ' + IniImage.BridgeHost);
      SL.Add('WebServerHost = ' + IniImage.WebServerHost);

      SL.Add('');
      SL.Add('---- Open Ports');
      NetReport(SL);

      SL.Add('');
      SL.Add('---- InputNCP');
      BO.InputServer.Server.ConnectionReport(SL);

      SL.Add('');
      SL.Add('---- OutputNCP');
      BO.OutputServer.Server.ConnectionReport(SL);

      SL.Add('');
      SL.Add('---- AdapterBO');
      if AdapterBO <> nil then
      begin
        i := AdapterBO.InputServer.Server.ConnectionCount;
        SL.Add('AdapterInputConnections: ' + IntToStr(i));

        i := AdapterBO.OutputServer.Server.ConnectionCount;
        SL.Add('AdapterOutputConnections: ' + IntToStr(i));
      end;

      SL.Add('');
      SL.Add('---- WorkspaceInfo');
      WorkspaceInfo.WorkspaceReport(SL);

      SL.Add('');
      SL.Add('---- PeerController');
      PeerController.GetStatusReport(SL);

      result := SL.Text;
    finally
      SL.Free;
    end;
  except
  end;
end;

function TMain.GetBOData(WantXml: Boolean): string;
begin
  result := BO.Save(WantXml);
end;

function TMain.GetDBWorkspace: IDBWorkspace;
begin
  result := StoreAdapter.GetDBWorkspace;
end;

function TMain.GetDefaultEventName: string;
begin
  result := C_DefaultEventName;
end;

function TMain.GetDefaultEventType: Integer;
begin
  result := C_DefaultEventType;
end;

function TMain.GetEventNamesDB(KatID: Integer): string;
begin
  result := StoreAdapter.GetDBWorkspace.DBGetEventNames('.txt');
end;

function TMain.GetFolderInfoDataPath: string;
begin
  result := FolderInfo.DataPath;
end;

function TMain.GetStoreAdapter: TStoreAdapterBase;
begin
  result := StoreAdapter;
end;

function TMain.GetPrefix(KatID: Integer): string;
begin
  case KatID of
    TypFREvent: result := 'FR_';
    else
      result := IntToStr(KatID) + '_';
  end;
end;

function TMain.GetSuffix(KatID: Integer): string;
begin
  case KatID of
    TypFREvent: result := '_FRData';
    else
      result := '';
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

function TMain.GetUseUnicode: Boolean;
begin
  result := true;
end;

procedure TMain.SetDBInterface(Value: string);
begin
  if Assigned(IniImage) then
    IniImage.DBInterface := Value;
end;

function TMain.EditDBEvent: Boolean;
begin
  result := false;
  if ChooseDB then
  begin
    if DocManager.DBInterface <> Main.IniImage.DBInterface then
    begin
      DocManager.InitDBInterface(Main.IniImage.DBInterface);
      result := true;
    end;
  end;
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
      i := IniImage.PortIn;
      SL.Add(IntToStr(i));

      i := IniImage.PortOut;
      SL.Add(IntToStr(i));

      i := IniImage.WebServerHomePort;
      SL.Add(IntToStr(i));

      i := IniImage.WebServerRemotePort;
      SL.Add(IntToStr(i));

      i := GuiManager.Race;
      SL.Add(IntToStr(i));

      i := GuiManager.IT;
      SL.Add(IntToStr(i));

      if PeerController.Connected then
        SL.Add('Plugged')
      else
        SL.Add('Unplugged');

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
  if IsWinGUI or WantErrorWindow then
  begin
    InjectorImpl.ShowStartupLog;
  end;
end;

procedure TMain.ShowErrorMsg(msg: string);
begin
  FormAdapter.ShowError(msg);
end;

function TMain.WantOutput: Boolean;
begin
  result := true;
end;

procedure TMain.InjectClientBridgeMsg(s: string);
var
  msg: TBaseMsg;
begin
  msg := BO.NewMsg;
  try
    BridgeLocked := True;
    msg.prot := s;
    if msg.DispatchProt then
    begin
      if Params.WantAutoSync then
        Main.GuiManager.SynchronizeCache;
      if Params.WantBridgeMsgBroadcast then
      begin
        BO.Calc;
        BO.OutputServer.InjectMsg(nil, msBridge, msg.Prot);
      end;
      Main.GuiManager.PlaySound(Sound_Click02);
    end
    else
    begin
      Main.GuiManager.PlaySound(Sound_Recycle);
    end;
  finally
    msg.Free;
    BridgeLocked := false;
  end;
end;

procedure TMain.InjectServerBridgeMsg(cm: TContextMsg);
var
  msg: TBaseMsg;
begin
  msg := BO.NewMsg;
  try
    BridgeLocked := true;
    msg.prot := cm.msg;
    if msg.DispatchProt then
    begin
      if Params.WantAutoSync then
        Main.GuiManager.SynchronizeCache;

      if IniImage.WantInputNCPCalc then
        BO.Calc;

      //if not msg.IsRequest then
        BO.OutputServer.SendMsg(cm);
      Main.GuiManager.PlaySound(Sound_Click02);
    end;
  finally
    msg.Free;
    BridgeLocked := false;
  end;
end;

procedure TMain.InitPeer;
begin
  if IsServerBridge then
  begin
    Params.AutoPlugin := True;
    Params.AutoUpload := True;
    PeerController.Plugin;
    PeerController.Upload(BO.Save(false));
  end
  else
  begin
    Params.AutoPlugin := False;
    Params.AutoUpload := False;
  end;
end;

procedure TMain.EvaluateStartupScenario;
begin
  if ScenarioInfo.PortSet > 0 then
  begin
    IniImage.PortSet := ScenarioInfo.PortSet;
  end;
  case ScenarioInfo.Scenario of
    Scenario_Disconnected: InitDisconnectedScenario;
    Scenario_Readonly: InitReadonlyScenario;
    Scenario_BridgeServer_LH: InitBridgeServerScenario('localhost');
    Scenario_BridgeServer: InitBridgeServerScenario('real-network-interface');
    Scenario_HomeServer: InitBridgeServerScenario('homeserver');
    Scenario_EC2: InitBridgeServerScenario('amazon-ec2');
    Scenario_BridgeClient: InitBridgeClientScenario;
    Scenario_Webonly: InitWebonlyScenario;
    Scenario_OutputBridge: InitOutputClientScenario;
   end;
end;

procedure TMain.NetReport(SL: TStringList);
var
  p: Integer;
  fs: string;

  procedure AE(Prot: string; Port: Integer; Description: string);
  begin
    SL.Add(Format(fs, [Prot, Port, Description]));
  end;

begin
  fs := '%s:%d %s';
  if Assigned(AdapterBO) then
  begin
    p := AdapterBO.InputServer.Server.Port;
    AE('tcp', p, 'InputServer');
    p := AdapterBO.OutputServer.Server.Port;
    AE('tcp', p, 'OutputServer');
  end;

  if IsServerBridge then
  begin
    p := ServerBridge.BridgeNCP.Server.Port;
    AE('tcp', p, 'BridgeServer');
  end;

  if Assigned(PolicyProvider) and Assigned(PolicyProvider.PolicyNCP) then
  begin
    p := PolicyProvider.PolicyNCP.Server.Port;
    AE('tcp', p, 'PolicyServer');
  end;

  if Assigned(GuiManager.WebMotorHome) and GuiManager.WebMotorHome.IsActive then
  begin
    p := GuiManager.WebMotorHome.Port;
    AE('web', p, 'Home Web');
  end;

  if Assigned(GuiManager.WebMotorRemote) and GuiManager.WebMotorRemote.IsActive then
  begin
    p := GuiManager.WebMotorRemote.Port;
    AE('web', p, 'Remote Web');
  end;

  if IsServerBridge and ServerBridge.BridgeWeb.IsActive then
  begin
    p := ServerBridge.BridgeWeb.Port;
    AE('web', p, 'Bridge Web');
  end;

  if Assigned(GuiManager.SilverlightWeb) and GuiManager.SilverlightWeb.IsActive then
  begin
    p := GuiManager.SilverlightWeb.Port;
    AE('web', p, 'Silverlight Web');
  end;
end;

procedure TMain.InitPolicyProvider;
begin
  if not PolicyProvider.IsActive then
  begin
    PolicyProvider.Init;
  end;
end;

procedure TMain.InitReadonlyScenario;
begin
  WorkspaceInfo.AutoSaveIni := false;
  IniImage.WantSockets := false;
  Params.AutoPolicyServer := false;
  IniImage.BridgeProxyType := 0;

  Params.WantWeb := false;
  Params.AutoHomeWeb := true;
  Params.AutoRemoteWeb := false;
  Params.AutoSilverlightWeb := true;
end;

procedure TMain.InitWebonlyScenario;
begin
  WorkspaceInfo.AutoSaveIni := false;
  IniImage.WantSockets := false;
  Params.AutoPolicyServer := false;

  IniImage.BridgeProxyType := 0;

  Params.WantWeb := true;
  Params.AutoHomeWeb := true;
  Params.AutoRemoteWeb := true;
  Params.AutoSilverlightWeb := false;
end;

procedure TMain.InitDisconnectedScenario;
begin
  StartupLogger.Add('Inside TMain.DisconnectedScenario');

  IniImage.WantSockets := false;
  Params.AutoPolicyServer := false;

  IniImage.BridgeProxyType := 0;

  Params.WantWeb := true;
  Params.AutoHomeWeb := false;
  Params.AutoRemoteWeb := false;
  Params.AutoSilverlightWeb := false;
end;

procedure TMain.InitBridgeClientScenario;
begin
  StartupLogger.Add('Inside TMain.InitBridgeClientScenario');

  WorkspaceInfo.AutoSaveIni := false;
  IniImage.WantSockets := false;
  Params.AutoPolicyServer := false;
  IniImage.BridgeProvider := 2;
  IniImage.BridgeProxyType := 3;
  IniImage.BridgeHost := 'localhost';
  IniImage.BridgePort := 4530;
  IniImage.BridgePortHTTP := 8086;
  IniImage.BridgeUrl := '';
  IniImage.TaktIn := 0;
  IniImage.TaktOut := 0;

  Params.WantWeb := false;
  Params.AutoHomeWeb := false;
  Params.AutoRemoteWeb := false;
  Params.AutoSilverlightWeb := false;
end;

procedure TMain.InitOutputClientScenario;
begin
  StartupLogger.Add('Inside TMain.InitOutputClientScenario');

  WorkspaceInfo.AutoSaveIni := false;
  IniImage.WantSockets := false;
  Params.AutoPolicyServer := false;
  IniImage.BridgeProvider := 3;
  IniImage.OutputHost := 'gsmxp';
  IniImage.OutputPort := 3428;

  Params.WantWeb := false;
  Params.AutoHomeWeb := false;
  Params.AutoRemoteWeb := false;
  Params.AutoSilverlightWeb := false;
end;

procedure TMain.InitBridgeServerScenario(BridgeHost: string);
var
  bh: string;
  pd: string;
begin
  StartupLogger.Add('Inside TMain.InitBridgeServerScenario');

  try
    Params.AutoHomeWeb := true;
    Params.AutoRemoteWeb := true;

    if BridgeHost = 'localhost' then
    begin
      StartupLogger.Add('in localhost branch');
      bh := 'localhost';
    end
    else if BridgeHost = 'homeserver' then
    begin
      StartupLogger.Add('in homeserver branch');
      bh := WebUtils.GetLocalHostName;
      Params.UseProxyBase := true;
      Params.AutoRemoteWeb := true;
      IniImage.WebServerHomePort := 8086;
      IniImage.WebServerRemotePort := 9086;
    end
    else if BridgeHost = 'routerhost' then
    begin
      StartupLogger.Add('in routerhost branch');
      bh := WebUtils.GetLocalHostName;
      Params.UseProxyBase := true;
      Params.AutoRemoteWeb := true;
      IniImage.ProxyDomain := IniImage.RouterHost;
      IniImage.HomeProxyAuthority := 'http://' + IniImage.ProxyDomain;
      IniImage.RemoteProxyAuthority := 'http://' + IniImage.ProxyDomain;
      IniImage.HomeProxyBase := '/';
      IniImage.RemoteProxyBase := '/';
    end
    else if BridgeHost = 'amazon-ec2' then
    begin
      StartupLogger.Add('in branch amazon-ec2');
      bh := WebUtils.GetLocalHostName;
      Params.UseProxyBase := true;
      Params.AutoRemoteWeb := true;
      Params.WantNames := false;
      Params.WantAuthentication := true;

      pd := WebUtils.GetPublicIP;
      if pd = '127.0.0.1' then
        pd := WebUtils.GetPublicIPTest;

      if pd <> '127.0.0.1' then
      begin
        IniImage.ProxyDomain := pd;
      IniImage.HomeProxyAuthority := 'http://' + IniImage.ProxyDomain;
      IniImage.RemoteProxyAuthority := 'http://' + IniImage.ProxyDomain;
      end;
      IniImage.HomeProxyBase := '/';
      IniImage.RemoteProxyBase := '/';
      if IniImage.PortSet = 0 then
        IniImage.PortSet := cDefaultPortSet;
    end
    else if BridgeHost = IniImage.WebServerHost then
    begin
      StartupLogger.Add('in BridgeHost = IniImage.WebServerHost branch');
      bh := IniImage.WebServerHost;
    end
    else
    begin
      StartupLogger.Add('in default branch');
      { returns the name of the machine if network interface is available }
      bh := WebUtils.GetLocalHostName;
    end;

    StartupLogger.Add('WebServerHost = BridgeHost = ' + bh);
    IniImage.WebServerHost := bh;
    IniImage.BridgeHost := bh;

    WorkspaceInfo.AutoSaveIni := False;
    IniImage.WantSockets := True;

    if (IniImage.PortSet > 1) and (Scenario = Scenario_EC2) then
      Params.AutoPolicyServer := false
    else
    Params.AutoPolicyServer := true;

    if ScenarioInfo.WantPolicyServer = 1 then
      Params.AutoPolicyServer := true
    else if ScenarioInfo.WantPolicyServer = 0 then
      Params.AutoPolicyServer := false;

    IniImage.BridgeProvider := 2;
    IniImage.BridgeProxyType := 4;

    case IniImage.PortSet of
      3:
    begin
      IniImage.PortIn := 3827;
      IniImage.PortOut := 3828;
      IniImage.WebServerHomePort := 8082;
      IniImage.WebServerRemotePort := 8082;
      IniImage.BridgePort := 4532;
      IniImage.BridgePortHTTP := 8089;
      end;
      2:
    begin
      IniImage.PortIn := 3627;
      IniImage.PortOut := 3628;
      IniImage.WebServerHomePort := 8081;
      IniImage.WebServerRemotePort := 8081;
      IniImage.BridgePort := 4531;
      IniImage.BridgePortHTTP := 8088;
      end;
    else
    begin
      IniImage.PortIn := 3427;
      IniImage.PortOut := 3428;
      IniImage.WebServerHomePort := 8086;
      IniImage.WebServerRemotePort := 8086;
      IniImage.BridgePort := 4530;
      IniImage.BridgePortHTTP := 8087;
    end;
    end;

    IniImage.BridgeUrl := '';
    IniImage.TaktIn := 0;
    IniImage.TaktOut := 0;

    Params.AutoOnline := true;
    Params.AutoSilverlightWeb := true;
  except
    InitReadOnlyScenario;
  end;
end;

function TMain.GetCubeFaceText(n: Integer): string;
var
  l, l2, l3: string;
begin
  l2 := #13#10;
  l3 := l2;

//  l2 := #13#10'abc';
//  l3 := #13#10'http://127.0.0.1:8086';

  case n of
    1: begin //In
    l2 := l2 + 'IPV4 TCP all';
    l3 := l3 + IntToStr(Main.IniImage.PortIn);
    end;
    2: if Assigned(Main.GuiManager.WebMotorRemote) then
    begin //Web
    l2 := l2 + 'IPV4 HTTP ' + Main.IniImage.WebServerHost;
    l := Main.GuiManager.WebMotorRemote.Url;
    l := LeftStr(l, Pos('/remote', l) - 1);
    l3 := l3 + l;
    end;
    3: begin //Client
    l2 := l2 + 'not used';
    l3 := l3;
    end;
    4: begin //Bridge
    l2 := l2 + 'IPV4 TCP all';
    l3 := l3 + IntToStr(Main.IniImage.BridgePort);
    end;
    5: if Assigned(Main.PolicyProvider) then
    begin //Policy
    l2 := l2 + 'IPV4 TCP all';
    if Main.PolicyProvider.PolicyNCP <> nil then
    l3 := l3 + IntToStr(Main.PolicyProvider.PolicyNCP.Server.Port);
    end;
    6: begin //Out
    l2 := l2 + 'IPV4 TCP all';
    l3 := l3 + IntToStr(Main.IniImage.PortOut);
    end;
  end;
  result := l2 + l3;
end;

procedure TMain.InitBatchRequestItems(ML: TStrings);
begin
  ML.Add('FR.*.Request.Report.FinishReport');
  ML.Add('FR.*.Request.Report.PointsReport');
  ML.Add('FR.*.Request.JavaScore.XML');
  ML.Add('FR.*.Request.JavaScore.ProxyXmlInput');
  ML.Add('FR.*.Request.JavaScore.ProxyXmlOutput');
  ML.Add('FR.*.Request.HTM.Web.Event.Report0.Sort0.Finish');
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
