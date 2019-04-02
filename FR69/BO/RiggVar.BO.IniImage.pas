unit RiggVar.BO.IniImage;

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

{
uses mapped units to pull in project specific values
mapped units are units with same name, but in different location

see Unit RiggVar.Config for App-Constants, e.g. DefaultEventType
see Unit RiggVar.Params for App-Vars, e.g. AutoSave

see uses clause in implementation section
}

uses
  System.SysUtils,
  System.Classes,
  System.Inifiles,
  RiggVar.DAL.Redirector,
  RiggVar.BO.FolderInfo;

type
  TConfigSection = (
    csBridge,
    csOutput
  );

  TBaseIniImage = class(TPersistent)
  private
    procedure OnAppStartup;
    procedure OnAppShutDown;
    procedure BuildDefaultIni(SL: TStrings);
    procedure ReadUserConfig;
    procedure WriteUserConfig;
    function GetFolderInfo: TFolderInfo;
    function GetConfigText: string;
    function GetDefaultText: string;
  protected
    procedure CreateDefaultIni(const fn: string);
    procedure CheckDirs; virtual;
    procedure ReadFrameworkDefaults;
    procedure ReadAppConstants; virtual;
    procedure ReadAppVars; virtual;
    procedure ReadAdminConfig; virtual;
    procedure ReadIni(ini: TCustomIniFile); virtual;
    procedure WriteIni(ini: TCustomIniFile); virtual;
    procedure EnableEntries; virtual;
  public
    //--not persisted
    UseIni: Boolean;
    UsePortSet: Boolean;
    DefaultEventName: string;
    DefaultEventType: Integer;
    SearchForUsablePorts: Boolean;
    //WantLocalAccess: Boolean;
    ScoringModuleName: string;

    //--Options
    SafeStartupMode: Boolean;
    AutoLoadAdminConfig: Boolean;
    WantSockets: Boolean;
    //SearchForUsablePorts: Boolean;
    IsMaster: Boolean;
    AutoSave: Boolean;
    NoAutoSave: Boolean;
    CopyRankEnabled: Boolean;
    LogProxyXML: Boolean;
    UseUnicode: Boolean;
    UseDoubleBuffer: Boolean;
    WantXSL: Boolean; //not saved
    AutoSaveIni: Boolean;

    //--Connections
    Host: string;
    PortIn: Integer;
    PortOut: Integer;
    CalcHost: string;
    CalcPort: Integer;
    //FeedbackEnabled: Boolean;
    //FeedbackHost: string;
    //FeedbackPort: Integer;
    //WebServerPort: Integer;
    HomeUrl: string;
    WebApplicationUrl: string;
    AppTitle: string;
    JSDataDir: string;
    RDDataDir: string;
    AngularFR: string;
    AngularFREO: string;
    AngularFRAC: string;

    //--Switch
    SwitchHost: string;
    SwitchPort: Integer;
    SwitchPortHTTP: Integer;
    RouterHost: string;
    UseRouterHost: Boolean;
    //UseAddress: Boolean;
    MaxConnections: Integer;

    //--Bridge
    BridgeProxyType: Integer;
    BridgeHost: string;
    BridgePort: Integer;
    BridgePortHTTP: Integer;
    BridgeUrl: string;
    BridgeHomePage: string;
    TaktIn: Integer;
    TaktOut: Integer;

    //--Output
    OutputHost: string;
    OutputPort: Integer;

    //--Provider
    BridgeProvider: Integer;
    ScoringProvider: Integer;
    DBInterface: string;

    //--Tabs
    WantBrowser: Boolean;
    WantCache: Boolean;
    WantEntries: Boolean;
    WantRace: Boolean;
    WantTiming: Boolean;
    WantRacing: Boolean;
    WantKeying: Boolean;
    WantEvent: Boolean;
    WantWorkspace: Boolean;
    WantReport: Boolean;
    WantMenu: Boolean;
    WantCategory: Boolean;
    WantJson: Boolean;
    WantMobil: Boolean;
    WantWeb: Boolean;
    WantListing: Boolean;
    WantRoundings: Boolean;
    WantCourse: Boolean;
    WantMark: Boolean;

    //[Options]
    S_Options: Boolean;
    W_SafeStartupMode: Boolean;
    W_AutoLoadAdminConfig: Boolean;
    W_WantSockets: Boolean;
    //W_SearchForUsablePorts: Boolean;
    W_IsMaster: Boolean;
    W_AutoSave: Boolean;
    W_NoAutoSave: Boolean;
    W_CopyRankEnabled: Boolean;
    W_LogProxyXML: Boolean;
    W_UseDoubleBuffer: Boolean;
    W_UseUnicode: Boolean;

    //[Connections]
    S_Connections: Boolean;
    W_Host: Boolean;
    W_PortIn: Boolean;
    W_PortOut: Boolean;
    W_CalcHost: Boolean;
    W_CalcPort: Boolean;
    //W_FeedbackPortEnabled: Boolean;
    //W_FeedbackHost: Boolean;
    //W_FeedbackPort: Integer;
    W_HomeUrl: Boolean;
    W_WebApplicationUrl: Boolean;
    W_AppTitle: Boolean;
    W_JSDataDir: Boolean;
    W_RDDataDir: Boolean;
    W_AngularFR: Boolean;
    W_AngularFREO: Boolean;
    W_AngularFRAC: Boolean;

    //[Switch]
    S_Switch: Boolean;
    W_SwitchHost: Boolean;
    W_SwitchPort: Boolean;
    W_SwitchPortHTTP: Boolean;
    W_RouterHost: Boolean;
    W_UseRouterHost: Boolean;
    //W_UseAddress: Boolean;
    W_MaxConnections: Boolean;

    //[Bridge]
    S_Bridge: Boolean;
    W_BridgeProxyType: Boolean;
    W_BridgeHost: Boolean;
    W_BridgePort: Boolean;
    W_BridgePortHTTP: Boolean;
    W_BridgeUrl: Boolean;
    W_BridgeHomePage: Boolean;
    W_TaktIn: Boolean;
    W_TaktOut: Boolean;

    //[Output]
    S_Output: Boolean;
    W_OutputHost: Boolean;
    W_OutputPort: Boolean;

    //[Provider]
    S_Provider: Boolean;
    W_BridgeProvider: Boolean;
    W_ScoringProvider: Boolean;
    W_DataProvider: Boolean;

    //[Info Sections]
    S_ScoringProviderHelp: Boolean;
    S_BridgeProviderHelp: Boolean;
    S_DataProviderHelp: Boolean;
    S_BridgeProxyHelp: Boolean;

    //[Tabs]
    S_Tabs: Boolean;

    W_WantBrowser: Boolean;
    W_WantCache: Boolean;
    W_WantEntries: Boolean;
    W_WantRace: Boolean;
    W_WantTiming: Boolean;
    W_WantRacing: Boolean;
    W_WantKeying: Boolean;
    W_WantEvent: Boolean;
    W_WantWorkspace: Boolean;
    W_WantReport: Boolean;
    W_WantMenu: Boolean;
    W_WantCategory: Boolean;
    W_WantJson: Boolean;
    W_WantMobil: Boolean;
    W_WantWeb: Boolean;
    W_WantListing: Boolean;
    W_WantRoundings: Boolean;
    W_WantCourse: Boolean;
    W_WantMark: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure ForceWrite;
    function ToString: string; override;
    property FolderInfo: TFolderInfo read GetFolderInfo;
    property ConfigText: string read GetConfigText;
    property DefaultText: string read GetDefaultText;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.App.Config;

{ TBaseIniImage }

constructor TBaseIniImage.Create;
begin
  inherited Create;
end;

procedure TBaseIniImage.Init;
begin
  if Main.FolderInfo = nil then
    Main.FolderInfo := TFolderInfo.Create;

  //if RunAsService then
  //FolderInfo.SettingsPath := ExtractFilePath(ParamStr(0));

  EnableEntries;
  OnAppStartup;
end;

destructor TBaseIniImage.Destroy;
begin
  OnAppShutDown;
  inherited Destroy;
end;

procedure TBaseIniImage.EnableEntries;
begin
  //virtual
end;

function TBaseIniImage.GetConfigText: string;
var
  fn: string;
  SL: TStringList;
begin
  result := '';

  fn := FolderInfo.ConfigFileName;
  if Main.UseDB then
    SL := TDBStringList.Create
  else
    SL := TStringList.Create;

  SL.LoadFromFile(fn);
  result := SL.Text;
  SL.Free;
end;

function TBaseIniImage.GetDefaultText: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  BuildDefaultIni(SL);
  result := SL.Text;
  SL.Free;
end;

function TBaseIniImage.GetFolderInfo: TFolderInfo;
begin
  result := Main.FolderInfo;
end;

procedure TBaseIniImage.OnAppStartup;
begin
  ReadFrameworkDefaults;
  ReadAppConstants;
  ReadAppVars;

  if UseIni then
  begin
    Main.StartupLogger.Add('IniImage.UseIni = True');
    ReadUserConfig;
  end
  else
  begin
    Main.StartupLogger.Add('IniImage.UseIni = False');
  end;

  if AutoLoadAdminConfig and not SafeStartupMode then
    ReadAdminConfig;

  CheckDirs;
end;

procedure TBaseIniImage.OnAppShutDown;
begin
  if UseIni then
    WriteUserConfig;
end;

procedure TBaseIniImage.ReadFrameworkDefaults;
begin
  //does initialization with framework defaults
  //values may be collected from a declaration in another unit
  //this method is not virtual
  //this method is called before reading App-Constants and App-Vars
  //see method OnAppStartup
  AppTitle := 'title';
  Host := 'localhost';
  PortIn := 3427;
  PortOut := 3428;
  CalcPort := 3037;
  CalcHost := 'localhost';
  SwitchPort := 4029;
  UseRouterHost := False;
  RouterHost := 'routerip';
  SwitchHost := 'localhost';
  SwitchPortHTTP := 8085;
  WebApplicationUrl := 'http://localhost/WebApp/';
  IsMaster := True;
  MaxConnections := 32;
  HomeUrl := 'about:blank';
  BridgeHost := 'localhost';
  BridgePort := 4030;
  BridgePortHTTP := 8087;
  BridgeUrl := 'http://localhost/FR88/';
  BridgeHomePage := '';
  BridgeProxyType := 0;
  TaktIn := 3;
  TaktOut := 3;
  OutputHost := 'localhost';
  OutputPort := 3428;
  ScoringModuleName := 'JS01.dll';
  AutoSaveIni := True;

  WantBrowser := True;
  WantCache := True;
  WantEntries := True;
  WantRace := True;
  WantTiming := True;
  WantRacing := True;
  WantKeying := True;
  WantEvent := True;
  WantWorkspace := True;
  WantReport := True;
  WantMenu := True;
  WantCategory := True;
  WantJson := True;
  WantMobil := True;
  WantWeb := True;
  WantListing := True;
  WantRoundings := True;
  WantCourse := True;
  WantMark := True;
end;

procedure TBaseIniImage.ReadAppConstants;
begin
  //virtual
  //read group 1 of application specific overrides
  //usually declared in RiggVar.Config
  //these values are often not subject to user or admin configuration
  //this method is called after reading FrameworkDefaults
  WantSockets := C_WantAdapter;
  DefaultEventName := C_DefaultEventName;
  DefaultEventType := C_DefaultEventType;
end;

procedure TBaseIniImage.ReadAppVars;
begin
  //virtual
  //read group 2 of application specific overrides
  //usually declared in RiggVar.Params
  //these values are typically subject to admin configuration
  //this method is called after reading AppConstants
  AutoSave := Main.Params.AutoSave;
  NoAutoSave := Main.Params.NoAutoSave;
  UseUnicode := Main.Params.UseUnicode;
  UseDoubleBuffer := Main.Params.UseDoubleBuffer;
  WantXSL := Main.Params.WantXSL;

  Host := 'localhost';
  WebApplicationUrl := 'http://localhost/FR84/';

  BridgeUrl := 'http://localhost/FR88/';
  BridgeProvider := 0;

  ScoringProvider := 2;

  JSDataDir := 'D:\RiggVar\JDK5\JavaScore\DBEvent\';
  RDDataDir := 'D:\XSL\RDX\';
  AngularFR := 'D:\Angular\FR03A1\dist\FR03A1';
  AngularFREO := 'D:\Angular\FR03E1\dist\FR03E1';
  AngularFRAC := 'D:\Angular\FR05I\dist\FR05I';
end;

procedure TBaseIniImage.ReadUserConfig;
var
  ini: TCustomInifile;
  fn: string;
begin
  { read user configuration }
  { this method is not not virtual, but ReadIni is virtual }
  { may be subject to admin overrides }

  if not UseIni then
    Exit;

  fn := FolderInfo.ConfigFileName;
  if not DBFileExists(fn) then
    Exit;

  if Main.UseDB then
    ini := TDBIniFile.Create(fn)
  else
  ini := TIniFile.Create(fn);

  try
    ReadIni(ini);
  finally
    ini.Free;
  end;
end;

procedure TBaseIniImage.WriteUserConfig;
var
  ini: TCustomInifile;
  fn: string;
begin
  if not UseIni then
    Exit;

  //not virtual, see virtual method WriteIni
  fn := FolderInfo.ConfigFileName;
  if AutoSaveIni then //see WorkspaceInfo / WorkspaceManager
  begin
    if (Main.Params.UseDB = True) or DBDirectoryExists(ExtractFileDir(fn)) then
    begin
      ini := TDBIniFile.Create(fn);
      try
        WriteIni(ini);
        if ini is TMemIniFile then
          ini.UpdateFile;
      finally
        ini.Free;
      end;
    end;
  end;
end;

procedure TBaseIniImage.ReadAdminConfig;
begin
  //read admin configuration automatically at startup from webservice
end;

procedure TBaseIniImage.CreateDefaultIni(const fn: string);
var
  SL: TDBStringList;
  dn: string;
  s: string;
begin
  if UseIni then
  try
    dn := ExtractFileDir(fn);
    if DBDirectoryExists(dn) then
    begin
      SL := TDBStringList.Create;
      BuildDefaultIni(SL);
      try
        SL.SaveToFile(fn);
      finally
        SL.Free;
      end;
    end;
  except
    s := 'Cannot Create DefaultIni in ' + dn;
    Main.ShowErrorMsg(s);
    Main.Logger.Error(s);
  end;
end;

procedure TBaseIniImage.BuildDefaultIni(SL: TStrings);
begin
    if S_Options then
    begin
      SL.Add('[Options]');
      if W_AutoSave then
        SL.Add('AutoSave=0');
      if W_NoAutoSave then
        SL.Add('NoAutoSave=1'); //by default - do not save changed files
      if W_WantSockets then
        SL.Add('WantSockets=0'); //by default - do not open sockets
      if W_IsMaster then
        SL.Add('IsMaster=0');
      if W_CopyRankEnabled then
        SL.Add('CopyRankEnabled=0');
      if W_LogProxyXML then
        SL.Add('LogProxyXML=0');
      if W_UseUnicode then
        SL.Add('UseUnicode=0');
      if W_UseDoubleBuffer then
        SL.Add('UseDoubleBuffer=0');
      SL.Add('');
    end;

    if S_Connections then
    begin
      SL.Add('[Connections]');
      if W_Host then
        SL.Add('Host=localhost');
      if W_PortIn then
        SL.Add('PortIn=' + IntToStr(PortIn));
      if W_PortOut then
        SL.Add('PortOut=' + IntToStr(PortOut));
      if W_CalcHost then
        SL.Add('CalcHost=localhost');
      if W_CalcPort then
        SL.Add('CalcPort=' + IntToStr(CalcPort));
      if W_MaxConnections then
        SL.Add('MaxConnections=' + IntToStr(MaxConnections));
      if W_WebApplicationUrl then
        SL.Add('WebApplicationUrl=' + WebApplicationUrl);
      if W_HomeUrl then
        SL.Add('HomeUrl=' + HomeUrl);
      if W_AppTitle then
        SL.Add('AppTitle=title');
      if W_JSDataDir then
        SL.Add('JSDataDir=EnterPathHere');
      if W_RDDataDir then
        SL.Add('RDDataDir=EnterPathHere');
      if W_AngularFR then
        SL.Add('AngularFR=EnterPathHere');
      if W_AngularFREO then
        SL.Add('AngularFREO=EnterPathHere');
      if W_AngularFRAC then
        SL.Add('AngularFRAC=EnterPathHere');
      SL.Add('');
    end;

    if S_Provider then
    begin
      SL.Add('[Provider]');
      if W_DataProvider then
        SL.Add('DataProvider=TXT');
      if W_ScoringProvider then
        SL.Add('ScoringProvider=0');
      if W_BridgeProvider then
        SL.Add('BridgeProvider=0');
      SL.Add('');
    end;

    if S_Switch then
    begin
      SL.Add('[Switch]');
      if W_SwitchHost then
        SL.Add('SwitchHost=localhost');
      if W_SwitchPort then
        SL.Add('SwitchPort=' + IntToStr(SwitchPort));
      if W_SwitchPortHTTP then
        SL.Add('SwitchPortHTTP=' + IntToStr(SwitchPortHTTP));
      if W_RouterHost then
        SL.Add('RouterHost=routerip');
      if W_UseRouterHost then
        SL.Add('UseRouterHost=0');
      SL.Add('');
    end;

    if S_Bridge then
    begin
      SL.Add('[Bridge]');
      if W_BridgeProxyType then
        SL.Add('BridgeProxyType=' + IntToStr(BridgeProxyType));
      if W_BridgeHost then
        SL.Add('BridgeHost=' + BridgeHost);
      if W_BridgePort then
        SL.Add('BridgePort=' + IntToStr(BridgePort));
      if W_BridgePortHTTP then
        SL.Add('BridgePortHTTP=' + IntToStr(BridgePortHTTP));
      if W_BridgeUrl then
        SL.Add('BridgeUrl=' + BridgeUrl);
      if W_BridgeHomePage then
        SL.Add('BridgeHomePage=' + BridgeHomePage);
      if W_TaktIn then
        SL.Add('TaktIn=' + IntToStr(TaktIn));
      if W_TaktOut then
        SL.Add('TaktOut=' + IntToStr(TaktOut));
      SL.Add('');
    end;

    if S_Output then
    begin
      SL.Add('[Output]');
      if W_OutputHost then
        SL.Add('OutputHost=localhost');
      if W_OutputPort then
        SL.Add('OutputPort=' + IntToStr(OutputPort));
      SL.Add('');
    end;

    if S_DataProviderHelp then
    begin
      SL.Add('[DataProviderInfo]');
      SL.Add('TXT=TXT-Files');
      SL.Add('MDB=Local-DB');
      SL.Add('WEB=WEB-Service');
      SL.Add('');
    end;

    if S_ScoringProviderHelp then
    begin
      SL.Add('[ScoringProviderInfo]');
      SL.Add('0=Default');
      SL.Add('1=SimpleTest');
      SL.Add('2=Inline');
      SL.Add('3=ProxyDLL');
      SL.Add('4=ProxyXML');
      SL.Add('');
    end;

    if S_BridgeProviderHelp then
    begin
      SL.Add('[BridgeProviderInfo]');
      SL.Add('0=Mock');
      SL.Add('1=Switch');
      SL.Add('2=Bridge');
      SL.Add('');
    end;

    if S_BridgeProxyHelp then
    begin
      SL.Add('[BridgeProxyInfo]');
      SL.Add('0=MockBridge');
      SL.Add('1=ASP.NET Web Service');
      SL.Add('2=PHP Web Service');
      SL.Add('3=ClientBridge');
      SL.Add('4=ServerBridge');
      SL.Add('5=SynchronBridge');
      SL.Add('6=RESTBridge');
      SL.Add('7=ProxyBridge');
      SL.Add('8=CombiBridge');
      SL.Add('');
    end;

    if S_Tabs then
    begin
      SL.Add('[Tabs]');
      SL.Add('WantBrowser=1');
      SL.Add('WantCache=1');
      SL.Add('WantEntries=1');
      SL.Add('WantRace=1');
      SL.Add('WantTiming=1');
      SL.Add('WantRacing=1');
      SL.Add('WantKeying=1');
      SL.Add('WantEvent=1');
      SL.Add('WantWorkspace=1');
      SL.Add('WantReport=1');
      SL.Add('WantWeb=0');
      SL.Add('WantMenu=1');
      SL.Add('WantCategory=1');
      SL.Add('WantJson=1');
      SL.Add('WantMobil=1');
      SL.Add('WantListing=0');
      SL.Add('WantRoundings=0');
      SL.Add('WantCourse=0');
      SL.Add('WantMark=0');
    end;
end;

procedure TBaseIniImage.ReadIni(ini: TCustomIniFile);
var
  s: string;
begin
  Main.StartupLogger.Add('Reading Ini');
  if S_Options then
  begin
    s := 'Options';
    AutoSave := ini.ReadBool(s, 'AutoSave', AutoSave);
    NoAutoSave := ini.ReadBool(s, 'NoAutoSave', NoAutoSave);
    WantSockets := ini.ReadBool(s, 'WantSockets', WantSockets);
    IsMaster := ini.ReadBool(s, 'IsMaster', IsMaster);
    CopyRankEnabled := ini.ReadBool(s, 'CopyRankEnabled', CopyRankEnabled);
    LogProxyXML := ini.ReadBool(s, 'LogProxyXML', LogProxyXML);
    UseUnicode := ini.ReadBool(s, 'UseUnicode', UseUnicode);
    UseDoubleBuffer := ini.ReadBool(s, 'UseDoubleBuffer', UseDoubleBuffer);
  end;

  if S_Connections then
  begin
    s := 'Connections';
    Host := ini.ReadString(s, 'Host', Host);
    PortIn := ini.ReadInteger(s, 'PortIn', PortIn);
    PortOut := ini.ReadInteger(s, 'PortOut', PortOut);
    CalcHost := ini.ReadString(s, 'CalcHost', CalcHost);
    CalcPort := ini.ReadInteger(s, 'CalcPort', CalcPort);
    WebApplicationUrl := ini.ReadString(s, 'WebApplicationUrl', WebApplicationUrl);
    MaxConnections := ini.ReadInteger(s, 'MaxConnections', MaxConnections);
    AppTitle := ini.ReadString(s, 'AppTitle', AppTitle);
    HomeUrl := ini.ReadString(s, 'HomeUrl', HomeUrl);
    JSDataDir := ini.ReadString(s, 'JSDataDir', JSDataDir);
    RDDataDir := ini.ReadString(s, 'RDDataDir', RDDataDir);
    AngularFR := ini.ReadString(s, 'AngularFR', AngularFR);
    AngularFREO := ini.ReadString(s, 'AngularFREO', AngularFREO);
    AngularFRAC := ini.ReadString(s, 'AngularFRAC', AngularFRAC);
  end;

  if S_Switch then
  begin
    s := 'Switch';
    SwitchHost := ini.ReadString(s, 'SwitchHost', SwitchHost);
    SwitchPort := ini.ReadInteger(s, 'SwitchPort', SwitchPort);
    SwitchPortHTTP := ini.ReadInteger(s, 'SwitchPortHTTP', SwitchPortHTTP);
    RouterHost := ini.ReadString(s, 'RouterHost', RouterHost);
    UseRouterHost := ini.ReadBool(s, 'UseRouterHost', UseRouterHost);
  end;

  if S_Bridge then
  begin
    s := 'Bridge';
    BridgeProxyType := ini.ReadInteger(s, 'BridgeProxyType', BridgeProxyType);
    BridgeHost := ini.ReadString(s, 'BridgeHost', BridgeHost);
    BridgePort := ini.ReadInteger(s, 'BridgePort', BridgePort);
    BridgePortHTTP := ini.ReadInteger(s, 'BridgePortHTTP', BridgePortHTTP);
    BridgeUrl := ini.ReadString(s, 'BridgeUrl', BridgeUrl);
    BridgeHomePage := ini.ReadString(s, 'BridgeHomePage', BridgeHomePage);
    TaktIn := ini.ReadInteger(s, 'TaktIn', TaktIn);
    TaktOut := ini.ReadInteger(s, 'TaktOut', TaktOut);
  end;

  if S_Output then
  begin
    s := 'Output';
    OutputHost := ini.ReadString(s, 'OutputHost', OutputHost);
    OutputPort := ini.ReadInteger(s, 'OutputPort', OutputPort);
  end;

  if S_Provider then
  begin
    s := 'Provider';
    DBInterface := ini.ReadString(s, 'DataProvider', 'TXT');
    ScoringProvider := ini.ReadInteger(s, 'ScoringProvider', ScoringProvider);
    BridgeProvider := ini.ReadInteger(s, 'BridgeProvider', BridgeProvider);
  end;

  if S_Tabs then
  begin
    s := 'Tabs';
    WantBrowser := ini.ReadBool(s, 'WantBrowser', WantBrowser);
    WantCache := ini.ReadBool(s, 'WantCache', WantCache);
    WantEntries := ini.ReadBool(s, 'WantEntries', WantEntries);
    WantRace := ini.ReadBool(s, 'WantRace', WantRace);
    WantTiming := ini.ReadBool(s, 'WantTiming', WantTiming);
    WantRacing := ini.ReadBool(s, 'WantRacing', WantRacing);
    WantKeying := ini.ReadBool(s, 'WantKeying', WantKeying);
    WantEvent := ini.ReadBool(s, 'WantEvent', WantEvent);
    WantWorkspace := ini.ReadBool(s, 'WantWorkspace', WantWorkspace);
    WantReport := ini.ReadBool(s, 'WantReport', WantReport);
    WantMenu := ini.ReadBool(s, 'WantMenu', WantMenu);
    WantCategory := ini.ReadBool(s, 'WantCategory', WantCategory);
    WantJson := ini.ReadBool(s, 'WantJson', WantJson);
    WantMobil := ini.ReadBool(s, 'WantMobil', WantMobil);
    WantWeb := ini.ReadBool(s, 'WantWeb', WantWeb);
    WantListing := ini.ReadBool(s, 'WantListing', WantListing);
    WantRoundings := ini.ReadBool(s, 'WantRoundings', WantRoundings);
    WantCourse := ini.ReadBool(s, 'WantCourse', WantCourse);
    WantMark := ini.ReadBool(s, 'WantMark', WantMark);
  end;

end;

procedure TBaseIniImage.WriteIni(ini: TCustomIniFile);
var
  s: string;
begin
  if S_Options then
  begin
    s := 'Options';
    if W_AutoSave then
      ini.WriteBool(s, 'AutoSave', AutoSave);
    if W_NoAutoSave then
      ini.WriteBool(s, 'NoAutoSave', NoAutoSave);
    if W_WantSockets then
      ini.WriteBool(s, 'WantSockets', WantSockets);
    if W_IsMaster then
      ini.WriteBool(s, 'IsMaster', IsMaster);
    if W_CopyRankEnabled then
      ini.WriteBool(s, 'CopyRankEnabled', CopyRankEnabled);
    if W_LogProxyXML then
      ini.WriteBool(s, 'LogProxyXML', LogProxyXML);
    if W_UseUnicode then
      ini.WriteBool(s, 'UseUnicode', UseUnicode);
    if W_UseDoubleBuffer then
      ini.WriteBool(s, 'UseDoubleBuffer', UseDoubleBuffer);
  end;

  if S_Connections then
  begin
    s := 'Connections';
    if W_Host then
      ini.WriteString(s, 'Host', Host);
    if W_PortIn then
      ini.WriteInteger(s, 'PortIn', PortIn);
    if W_PortOut then
      ini.WriteInteger(s, 'PortOut', PortOut);
    if W_CalcHost then
      ini.WriteString(s, 'CalcHost', CalcHost);
    if W_CalcPort then
      ini.WriteInteger(s, 'CalcPort', CalcPort);
    if W_WebApplicationUrl then
    ini.WriteString(s, 'WebApplicationUrl', WebApplicationUrl);
    if W_MaxConnections then
      ini.WriteInteger(s, 'MaxConnections', MaxConnections);
    if W_AppTitle then
      ini.WriteString(s, 'AppTitle', AppTitle);
    if W_HomeUrl then
      ini.WriteString(s, 'HomeUrl', HomeUrl);
    if W_JSDataDir then
      ini.WriteString(s, 'JSDataDir', JSDataDir);
    if W_RDDataDir then
      ini.WriteString(s, 'RDDataDir', RDDataDir);
    if W_AngularFR then
      ini.WriteString(s, 'AngularFR', AngularFR);
    if W_AngularFREO then
      ini.WriteString(s, 'AngularFREO', AngularFREO);
    if W_AngularFRAC then
      ini.WriteString(s, 'AngularFRAC', AngularFRAC);
  end;

  if S_Switch then
  begin
    s := 'Switch';
    if W_SwitchHost then
      ini.WriteString(s, 'SwitchHost', SwitchHost);
    if W_SwitchPort then
      ini.WriteInteger(s, 'SwitchPort', SwitchPort);
    if W_SwitchPortHTTP then
      ini.WriteInteger(s, 'SwitchPortHTTP', SwitchPortHTTP);
    if W_RouterHost then
      ini.WriteString(s, 'RouterHost', RouterHost);
    if W_UseRouterHost then
      ini.WriteBool(s, 'UseRouterHost', UseRouterHost);
  end;

  if S_Bridge then
  begin
    s := 'Bridge';
    if W_BridgeProxyType then
      ini.WriteInteger(s, 'BridgeProxyType', BridgeProxyType);
    if W_BridgeHost then
      ini.WriteString(s, 'BridgeHost', BridgeHost);
    if W_BridgePort then
      ini.WriteInteger(s, 'BridgePort', BridgePort);
    if W_BridgePortHTTP then
      ini.WriteInteger(s, 'BridgePortHTTP', BridgePortHTTP);
    if W_BridgeUrl then
      ini.WriteString(s, 'BridgeUrl', BridgeUrl);
    if W_BridgeHomePage then
      ini.WriteString(s, 'BridgeHomePage', BridgeHomePage);
    if W_TaktIn then
      ini.WriteInteger(s, 'TaktIn', TaktIn);
    if W_TaktOut then
      ini.WriteInteger(s, 'TaktOut', TaktOut);
  end;

  if S_Output then
  begin
    s := 'Output';
    if W_OutputHost then
      ini.WriteString(s, 'OutputHost', OutputHost);
    if W_OutputPort then
      ini.WriteInteger(s, 'OutputPort', OutputPort);
  end;

  if S_Provider then
  begin
    s := 'Provider';
    if W_DataProvider then
      ini.WriteString(s, 'DataProvider', DBInterface);
    if W_ScoringProvider then
      ini.WriteInteger(s, 'ScoringProvider', ScoringProvider);
    if W_BridgeProvider then
      ini.WriteInteger(s, 'BridgeProvider', BridgeProvider);
  end;

  if S_Tabs then
  begin
    s := 'Tabs';
    if W_WantBrowser then
      ini.WriteBool(s, 'WantBrowser', WantBrowser);
    if W_WantCache then
      ini.WriteBool(s, 'WantCache', WantCache);
    if W_WantEntries then
      ini.WriteBool(s, 'WantEntries', WantEntries);
    if W_WantRace then
      ini.WriteBool(s, 'WantRace', WantRace);
    if W_WantTiming then
      ini.WriteBool(s, 'WantTiming', WantTiming);
    if W_WantRacing then
      ini.WriteBool(s, 'WantRacing', WantRacing);
    if W_WantKeying then
      ini.WriteBool(s, 'WantKeying', WantKeying);
    if W_WantEvent then
      ini.WriteBool(s, 'WantEvent', WantEvent);
    if W_WantWorkspace then
      ini.WriteBool(s, 'WantWorkspace', WantWorkspace);
    if W_WantReport then
      ini.WriteBool(s, 'WantReport', WantReport);
    if W_WantMenu then
      ini.WriteBool(s, 'WantMenu', WantMenu);
    if W_WantCategory then
      ini.WriteBool(s, 'WantCategory', WantCategory);
    if W_WantJson then
      ini.WriteBool(s, 'WantJson', WantJson);
    if W_WantMobil then
      ini.WriteBool(s, 'WantMobil', WantMobil);
    if W_WantWeb then
      ini.WriteBool(s, 'WantMobil', WantWeb);
    if W_WantListing then
      ini.WriteBool(s, 'WantListing', WantListing);
    if W_WantRoundings then
      ini.WriteBool(s, 'WantRoundings', WantRoundings);
    if W_WantCourse then
      ini.WriteBool(s, 'WantCourse', WantCourse);
    if W_WantMark then
      ini.WriteBool(s, 'WantMark', WantMark);
  end;
end;

procedure TBaseIniImage.CheckDirs;
begin
  if Assigned(Main.StoreAdapter) then
  begin
    Main.StoreAdapter.CheckResultsDir;
    Main.StoreAdapter.CheckConfigDir;
  end;
end;

function TBaseIniImage.ToString: string;
begin
  //virtual
end;

procedure TBaseIniImage.ForceWrite;
var
  _useIni: Boolean;
  _autoSaveIni: Boolean;
begin
  _useIni := UseIni;
  _autoSaveIni := AutoSaveIni;

  UseIni := True;
  AutoSaveIni := True;

  WriteUserConfig;

  UseIni := _useIni;
  AutoSaveIni := _autoSaveIni;
end;

end.

