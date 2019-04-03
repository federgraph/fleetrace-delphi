unit RiggVar.App.Config;

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
  System.IniFiles,
  RiggVar.BO.TemplateIDs,
  RiggVar.BO.IniImage;

const
  C_UseIni = True;
  C_WantAdapter = True;
  C_DefaultEventName: string = 'FR';
  C_DefaultEventType: Integer = TypFREvent;
  C_DefaultScoringProviderID = 2; //see const definitions in RiggVar.CalcEV

type
  TWantPages = class
  public
    WantProviderMenu: Boolean;
    WantPublishMenu: Boolean;
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
    WantMobil: Boolean;
    WantWeb: Boolean;
    WantListing: Boolean;
    WantRoundings: Boolean;
  end;

  TIniImage = class(TBaseIniImage)
  protected
    procedure EnableEntries; override;
    procedure ReadAppVars; override;
    procedure ReadIni(ini: TCustomIniFile); override;
    procedure WriteIni(ini: TCustomIniFile); override;
  public
    WantSimpleParser: Boolean;
    SouthContainerVisible: Boolean;
    NorthContainerVisible: Boolean;
    WantInputNCPCalc: Boolean;
    WantScenarioSelector: Boolean;
    WantLocalAccess: Boolean;
    WebServerHost: string;
    WebServerHomePort: Integer;
    WebServerRemotePort: Integer;
    UseResource: Boolean;
    UsePortSet: Boolean;
    BridgeWebTitle: string;
    procedure InspectorOnLoad(Sender: TObject);
    procedure InspectorOnSave(Sender: TObject);
    function ToString: string; override;
  end;

implementation

uses
  RiggVar.Col.NameValue,
  RiggVar.Util.Classes;

const
  C_Keys: string = 'Keys';
  C_Options: string = 'Options';
  C_Connection: string = 'Connections';
  C_Bridge: string = 'Bridge';
  C_Switch: string = 'Switch';

{ TIniImage }

procedure TIniImage.EnableEntries;
begin
  //[Options]
  S_Options := True;
  W_SafeStartupMode := True;
  W_AutoLoadAdminConfig := True;
  W_AutoSave := False;
  W_NoAutoSave := True;
  W_CopyRankEnabled := False;
  W_LogProxyXML := False;
  W_WantSockets := False;
  W_IsMaster := False;
  W_UseUnicode := True;
  W_UseDoubleBuffer := True;

  //[Connections]
  S_Connections := False;
  W_Host := True;
  W_PortIn := True;
  W_PortOut := True;
  W_CalcHost := False;
  W_CalcPort := False;
  W_WebApplicationUrl := False;
  W_HomeUrl := False;
  W_JSDataDir := False;
  W_RDDataDir := False;
  W_AngularFR := False;
  W_AngularFREO := False;

  //[Provider]
  S_Provider := False;
  W_DataProvider := True;
  W_ScoringProvider := True;
  W_BridgeProvider := True;

  //[Switch]
  S_Switch := False;
  W_SwitchHost := True;
  W_SwitchPort := True;
  W_SwitchPortHTTP := True;
  W_RouterHost := True;
  W_UseRouterHost := True;

  //[Bridge]
  S_Bridge := False;
  W_BridgeProxyType := True;
  W_BridgeHost := True;
  W_BridgePort := True;
  W_BridgePortHTTP := True;
  W_BridgeUrl := True;
  W_BridgeHomePage := True;
  W_TaktIn := True;
  W_TaktOut := True;

  //[Output]
  S_Output := False;
  W_OutputHost := True;
  W_OutputPort := True;

  //[Info Sections]
  S_DataProviderHelp := False;
  S_ScoringProviderHelp := False;
  S_BridgeProviderHelp := False;
  S_BridgeProxyHelp := False;

  //[Tabs]
  S_Tabs := True;
  W_WantBrowser := True;
  W_WantCache := False;
  W_WantEntries := True;
  W_WantRace := False;
  W_WantTiming := False;
  W_WantRacing := True;
  W_WantKeying := True;
  W_WantEvent := True;
  W_WantWorkspace := True;
  W_WantReport := True;
  W_WantMenu := True;
  W_WantCategory := True;
  W_WantMobil := True;
  W_WantWeb := True;
  W_WantListing := True;
  W_WantRoundings := True;
end;

procedure TIniImage.ReadAppVars;
begin
  inherited;
  WantSimpleParser := False;
  NoAutoSave := True;
  PortIn := 3427;
  PortOut := 3428;
  WebServerHost := 'localhost';
  WebServerHomePort := 8086;
  WebServerRemotePort := 9086;
  SearchForUsablePorts := True;
  TaktOut := 3;
  TaktIn := 3;
  UseResource := True;
  DBInterface := 'TXT';
  BridgeWebTitle := 'BridgeWeb';
  ScoringProvider := C_DefaultScoringProviderID;
end;

procedure TIniImage.ReadIni(ini: TCustomIniFile);
begin
  inherited;
  WantLocalAccess := True;

  if S_Options then
  begin
    NorthContainerVisible := ini.ReadBool(C_Options, 'NorthContainerVisible', NorthContainerVisible);
    SouthContainerVisible := ini.ReadBool(C_Options, 'SouthContainerVisible', SouthContainerVisible);
  end;

  if S_Connections then
  begin
    WebServerHost := ini.ReadString(C_Connection, 'WebServerHost', WebServerHost);
    WebServerHomePort := ini.ReadInteger(C_Connection, 'WebServerHomePort', WebServerHomePort);
    WebServerRemotePort := ini.ReadInteger(C_Connection, 'WebServerRemotePort', WebServerRemotePort);
  end;
end;

procedure TIniImage.WriteIni(ini: TCustomIniFile);
begin
  inherited;

  if S_Options then
  begin
    ini.WriteBool(C_Options, 'NorthContainerVisible', NorthContainerVisible);
    ini.WriteBool(C_Options, 'SouthContainerVisible', SouthContainerVisible);
  end;

  if S_Connections then
  begin
    ini.WriteString(C_Connection, 'WebServerHost', WebServerHost);
    ini.WriteInteger(C_Connection, 'WebServerHomePort', WebServerHomePort);
    ini.WriteInteger(C_Connection, 'WebServerRemotePort', WebServerRemotePort);
  end;
end;

procedure TIniImage.InspectorOnLoad(Sender: TObject);
var
  cl: TNameValueRowCollection;
  cr: TNameValueRowCollectionItem;
begin
  if not (Sender is TNameValueRowCollection) then
    exit;

  cl := TNameValueRowCollection(Sender);

  if W_AutoSave then
  begin
    cr := cl.Add;
    cr.Category := C_Options;
    cr.FieldName := 'AutoSave';
    cr.FieldType := nvftBoolean;
    cr.FieldValue := BoolStr[AutoSave];
    cr.Caption := 'AutoSave';
    cr.Description := 'always save, dominant over NoAutoSave';
  end;

  if W_NoAutoSave then
  begin
    cr := cl.Add;
    cr.Category := C_Options;
    cr.FieldName := 'NoAutoSave';
    cr.FieldValue := BoolStr[NoAutoSave];
    cr.FieldType := nvftBoolean;
    cr.Caption := 'NoAutoSave';
    cr.Description := 'do not ask if AutoSave=false';
  end;

  if W_CopyRankEnabled then
  begin
    cr := cl.Add;
    cr.Category := C_Options;
    cr.FieldName := 'CopyRankEnabled';
    cr.FieldValue := BoolStr[CopyRankEnabled];
    cr.FieldType := nvftBoolean;
    cr.Caption := 'CopyRankEnabled';
    cr.Description := 'allow transfer of FinishPos from race to event';
  end;

  if W_WantSockets then
  begin
    cr := cl.Add;
    cr.Category := C_Options;
    cr.FieldName := 'WantSockets';
    cr.FieldValue := BoolStr[WantSockets];
    cr.FieldType := nvftBoolean;
    cr.Caption := 'WantSockets';
    cr.Description := 'yes triggers firewall popup at startup';
  end;

  if W_LogProxyXML then
  begin
    cr := cl.Add;
    cr.Category := C_Options;
    cr.FieldName := 'LogProxyXML';
    cr.FieldValue := BoolStr[LogProxyXML];
    cr.FieldType := nvftBoolean;
    cr.Caption := 'LogProxyXML';
    cr.Description := 'save proxy xml to disk for debugging';
  end;

  if W_IsMaster then
  begin
    cr := cl.Add;
    cr.Category := C_Options;
    cr.FieldName := 'IsMaster';
    cr.FieldValue := BoolStr[IsMaster];
    cr.FieldType := nvftBoolean;
    cr.Caption := 'IsMaster';
    cr.Description := 'master can upload backup';
  end;
end;

procedure TIniImage.InspectorOnSave(Sender: TObject);
var
  cl: TNameValueRowCollection;
  cr: TNameValueRowCollectionItem;
  i: Integer;
begin
  if not (Sender is TNameValueRowCollection) then
    exit;

  cl := TNameValueRowCollection(Sender);

  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    if cr.FieldName = 'AutoSave' then
      AutoSave := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'NoAutoSave' then
      NoAutoSave := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'CopyRankEnabled' then
      CopyRankEnabled := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'WantSockets' then
      WantSockets := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'LogProxyXML' then
      LogProxyXML := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'IsMaster' then
      IsMaster := TUtils.IsTrue(cr.FieldValue)
  end;

end;

function TIniImage.ToString: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;

  if S_Options then
  begin
  SL.Add('[Options]');
  SL.Add('SafeStartupMode=' + BoolStr[SafeStartupMode]);
  SL.Add('AutoLoadAdminConfig=' + BoolStr[AutoLoadAdminConfig]);
  SL.Add('WantSockets=' + BoolStr[WantSockets]);
  SL.Add('SearchForUsablePorts=' + BoolStr[SearchForUsablePorts]);
  SL.Add('IsMaster=' + BoolStr[IsMaster]);
  SL.Add('AutoSave=' + BoolStr[AutoSave]);
  SL.Add('NoAutoSave=' + BoolStr[NoAutoSave]);
  SL.Add('CopyRankEnabled=' + BoolStr[CopyRankEnabled]);
  SL.Add('LogProxyXML=' + BoolStr[LogProxyXML]);
  SL.Add('UseUnicode=' + BoolStr[UseUniCode]);
  SL.Add('UseDoubleBuffer=' + BoolStr[UseDoubleBuffer]);
  SL.Add('UseResource=' + BoolStr[UseResource]);
  SL.Add('');
  end;

  if S_Connections then
  begin
  SL.Add('[Connections]');
  SL.Add('WantAdapter=' + BoolStr[WantSockets]);
  SL.Add('Host=' + Host);
  SL.Add('PortIn=' + IntToStr(PortIn));
  SL.Add('PortOut=' + IntToStr(PortOut));
  SL.Add('CalcHost=' + CalcHost);
  SL.Add('CalcPort=' + IntToStr(CalcPort));
  SL.Add('WebServerHost=' + WebServerHost);
  SL.Add('WebServerHomePort=' + IntToStr(WebServerHomePort));
  SL.Add('WebServerRemotePort=' + IntToStr(WebServerRemotePort));
  SL.Add('WebApplicationUrl=' + WebApplicationUrl);
  SL.Add('HomeUrl=' + HomeUrl);
  SL.Add('AppTitle=' + AppTitle);
  SL.Add('JSDataDir=' + JSDataDir);
  SL.Add('');
  end;

  if S_Switch then
  begin
  SL.Add('[Switch]');
  SL.Add('SwitchHost=' + SwitchHost);
  SL.Add('SwitchPort=' + IntToStr(SwitchPort));
  SL.Add('SwitchPortHTTP=' + IntToStr(SwitchPortHTTP));
  SL.Add('RouterHost=' + RouterHost);
  SL.Add('UseRouterHost=' + BoolStr[UseRouterHost]);
  SL.Add('MaxConnections=' + IntToStr(MaxConnections));
  SL.Add('');
  end;

  if S_Bridge then
  begin
  SL.Add('[Bridge]');
  SL.Add('BridgeProxyType=' + IntToStr(BridgeProxyType));
  SL.Add('BridgeHost=' + BridgeHost);
  SL.Add('BridgePort=' + IntToStr(BridgePort));
  SL.Add('BridgePortHTTP=' + IntToStr(BridgePortHTTP));
  SL.Add('BridgeUrl=' + BridgeUrl);
  SL.Add('BridgeHomePage=' + BridgeHomePage);
  SL.Add('TaktIn=' + IntToStr(TaktIn));
  SL.Add('TaktOut=' + IntToStr(TaktOut));
  SL.Add('');
  end;

  if S_Provider then
  begin
  SL.Add('[Provider]');
  SL.Add('DataProvider=' + DBInterface);
  SL.Add('ScoringProvider=' + IntToStr(ScoringProvider));
  SL.Add('BridgeProvider=' + IntToStr(BridgeProvider));
  end;

  result := SL.Text;
  SL.Free;
end;

end.

