unit RiggVar.Web1.MenuHelper;

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
  RiggVar.Util.WebUtils,
  RiggVar.Web1.EventArgs;

type
  TWebMenuCommand = (
    wcmd_none,

    wcmd_open,
    wcmd_save,
    wcmd_saveas,
    wcmd_delete,

    wcmd_backup,
    wcmd_restore,
    wcmd_clear,
    wcmd_load_test_data,

    wcmd_connect,
    wcmd_disconnect,
    wcmd_plugin,
    wcmd_plugout,
    wcmd_synchronize,
    wcmd_upload,
    wcmd_download,

    wcmd_scoring_module,
    wcmd_bridge_provider,
    wcmd_db_interface,
    wcmd_workspace_location,

    wcmd_bridge_properties,
    wcmd_ini_properties,

    wcmd_event_params,
    wcmd_regatta_props,
    wcmd_fleet_props,
    wcmd_uniqua_props,
    wcmd_name_props,
    wcmd_event_props,

    wcmd_test_jsxml,
    wcmd_test_rdxml,
    wcmd_test_pxmli,
    wcmd_test_pxmlo,

    wcmd_diagnose_status,
    wcmd_diagnose_wsi,
    wcmd_diagnose_ini,
    wcmd_diagnose_src,
    wcmd_diagnose_txt,
    wcmd_diagnose_xml,
    wcmd_diagnose_tmd,
    wcmd_diagnose_tpr,
    wcmd_diagnose_idx,

    wcmd_undo_showlog,
    wcmd_undo_showundo,
    wcmd_undo_showredo,
    wcmd_undo_showcombi,

    wcmd_undo_undo,
    wcmd_undo_redo,
    wcmd_cache_synchro,
    wcmd_strict_mode,
    wcmd_relaxed_mode,
    wcmd_color_cycle
  );

  TMenuBuilder = class
  private
    procedure InitRemoteMenu01(SLA: TStrings);
    procedure InitRemoteMenu02(SLA: TStrings);
    function GetIsAdmin: Boolean;
  public
    CurrentMenu: Integer;

    constructor Create;

    procedure WriteMenu(SL: TStrings);
    procedure WriteMenu01(SL: TStrings);
    procedure WriteMenu02(SL: TStrings);
    procedure WriteMenu03(SL: TStrings);

    procedure WriteMenuHome00(SL: TStrings);
    procedure WriteMenuRemote00(SL: TStrings);

    procedure WriteMenuHome(SL: TStrings);
    procedure WriteMenuRemote(SL: TStrings);

    function ParseCommand(msg: string): TWebMenuCommand;
    function GetReport(cmd: TWebMenuCommand): string;

    procedure HandleStringCommand(msg: string);
    procedure HandleIntCommand(cmd: TWebMenuCommand);
    property IsAdmin: Boolean read GetIsAdmin;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Web1.Proxy;

constructor TMenuBuilder.Create;
begin
  inherited Create;
end;

procedure TMenuBuilder.WriteMenuHome00(SL: TStrings);
begin
  SL.Add('<a href="ED1">ED1</a> | ');
  SL.Add('<a href="ED2">ED2</a> | ');
  SL.Add('<a href="ED3">ED3</a>');
end;

procedure TMenuBuilder.WriteMenuRemote00(SL: TStrings);
begin
  SL.Add('<a href="Load">Load</a> | ');
  SL.Add('<a href="Save">Save</a> | ');
  SL.Add('<a href="Clear">Clear</a>');
end;

function TMenuBuilder.GetIsAdmin: Boolean;
begin
  result := Main.Params.UserLevel > 0;
end;

procedure TMenuBuilder.WriteMenu(SL: TStrings);
begin
  SL.Add('<div id="webmotor-menu"><p>');
  case CurrentMenu of
    1: WriteMenu01(SL);
    2: WriteMenu02(SL);
    3: WriteMenu03(SL);
    else
      WriteMenuHome(SL);
  end;
  SL.Add('</p></div>');
end;

procedure TMenuBuilder.WriteMenu01(SL: TStrings);
begin
  SL.Add('<a href="cache">cache</a> | ');
  SL.Add('<a href="finish">finish</a> | ');
  SL.Add('<a href="points">points</a> | ');
  SL.Add('<a href="race">race</a>');
  //SL.Add('<a href="xml">xml</a> | ');
  //SL.Add('<a href="help">help</a> | ');
  //SL.Add('<a href="info">info</a>');
end;

procedure TMenuBuilder.WriteMenu02(SL: TStrings);
begin
//  SL.Add('<a href="manage">Manage</a> | '); //manually enter url in browser
  SL.Add('<a href="menu">Menu</a> | ');
//  SL.Add('<a href="swap">Swap</a> | '); //does no longer work
  SL.Add('<a href="entries">Entries</a> | ');
  SL.Add('<a href="rv">RaceValue</a> | ');
  SL.Add('<a href="MarkReport">Mark</a> | ');
  SL.Add('<a href="FinishReport">Finish</a> | ');
  SL.Add('<a href="PointsReport">Points</a> | ');
  SL.Add('<a href="Widget">Widget</a>');
end;

procedure TMenuBuilder.WriteMenu03(SL: TStrings);
begin
  SL.Add('<a href="tw01">TW01</a> | ');
  SL.Add('<a href="tw02">TW02</a> | ');
  SL.Add('<a href="tw03">TW03</a>');
end;

procedure TMenuBuilder.WriteMenuHome(SL: TStrings);
begin
  SL.Add('<a href="MarkReport">Mark</a> | ');
  SL.Add('<a href="FinishReport">Finish</a> | ');
  SL.Add('<a href="PointsReport">Points</a>');
end;

procedure TMenuBuilder.InitRemoteMenu01(SLA: TStrings);
var
  ML: TStrings;

  function AddMenu(h: string): TStrings;
  begin
    ML := TStringList.Create;
    SLA.AddObject(h, ML);
    result := ML;
  end;
begin
  ML := AddMenu('Document');
  ML.Add('Open*=menu-open');
  ML.Add('Save=menu-save');
  ML.Add('SaveAs...=menu-saveas');
  ML.Add('Delete...=menu-delete');

  ML := AddMenu('Actions');
  ML.Add('Backup=menu-backup');
  ML.Add('Restore=menu-restore');
  ML.Add('Clear=menu-clear');
	ML.Add('Load TestData=menu-load-test-data');
	ML.Add('Sync Cache=menu-cache-synchro');
	ML.Add('Try StrictMode=menu-strict-mode');
	ML.Add('Set RelaxedMode=menu-relaxed-mode');
	ML.Add('Cycle ColorMode=menu-color-cycle');

  if not IsWebApp then
  begin
    ML := AddMenu('Network');
    ML.Add('Connect=menu-connect');
    ML.Add('Disconnect=menu-disconnect');
    ML.Add('Plugin=menu-plugin');
    ML.Add('Plugout=menu-plugout');
    ML.Add('Synchronize=menu-synchronize');
    ML.Add('Upload=menu-upload');
    ML.Add('Download=menu-download');
  end;

  ML := AddMenu('Provider');
  ML.Add('#Scoring Module*=menu-scoring-module');
  ML.Add('#Bridge Provider*=menu-bridge-provider');
  ML.Add('#DB Interface*=menu-db-interface');
  ML.Add('Workspace Location*=menu-workspace-location');
  ML.Add('#Bridge Props*=menu-bridge-props');
  ML.Add('#Ini Props*=menu-ini-props');

  ML := AddMenu('Options');
  ML.Add('Event Params*=menu-event-params');
  ML.Add('#Regatta Props*=menu-regatta-props');
  ML.Add('Fleet Props*=menu-fleet-props');
  ML.Add('#Uniqua Props*=menu-uniqua-props');
  ML.Add('#Name Props*=menu-name-props');
  ML.Add('#Event Props*=menu-event-props');

  ML := AddMenu('Diagnose');
  ML.Add('Status=menu-diagnose-status');
	ML.Add('WSInfo=menu-diagnose-wsi');
  ML.Add('Ini=menu-diagnose-ini');
  ML.Add('Source=menu-diagnose-src');
  ML.Add('Txt=menu-diagnose-txt');
  ML.Add('Xml=menu-diagnose-xml');
  ML.Add('TimingData=menu-diagnose-tmd');
  ML.Add('TPT=menu-diagnose-tpr');
  ML.Add('Index=menu-diagnose-idx');

  ML := AddMenu('Test');
  ML.Add('JSXML=menu-test-jsxml');
  ML.Add('RDXML=menu-test-rdxml');
//  ML.Add('PxmlI=menu-test-pxmli');
//  ML.Add('PxmlO=menu-test-pxmlo');

  ML := AddMenu('Undo');
  ML.Add('Show Log=menu-undo-showlog');
  ML.Add('Show Undo=menu-undo-showundo');
  ML.Add('Show Redo=menu-undo-showredo');
  ML.Add('Show Combi=menu-undo-showcombi');
  ML.Add('Undo=menu-undo-undo');
  ML.Add('Redo=menu-undo-redo');
end;

procedure TMenuBuilder.InitRemoteMenu02(SLA: TStrings);
var
  ML: TStrings;

  function AddMenu(h: string): TStrings;
  begin
    ML := TStringList.Create;
    SLA.AddObject(h, ML);
    result := ML;
  end;
begin
//  ML := AddMenu('Document');
//  ML.Add('Open*=menu-open');
//  ML.Add('Save=menu-save');
//  ML.Add('SaveAs...=menu-saveas');
//  ML.Add('Delete...=menu-delete');

//  ML := AddMenu('Actions');
//  ML.Add('Backup=menu-backup');
//  ML.Add('Restore=menu-restore');
//  ML.Add('Clear=menu-clear');
//	ML.Add('Load TestData=menu-load-test-data');
//	ML.Add('Sync Cache=menu-cache-synchro');
//  ML.Add('Try StrictMode=menu-strict-mode');
//	ML.Add('Set RelaxedMode=menu-relaxed-mode');
//	ML.Add('Cycle ColorMode=menu-color-cycle');

  if not IsWebApp then
  begin
    ML := AddMenu('Network');
    ML.Add('Connect=menu-connect');
    ML.Add('Disconnect=menu-disconnect');
    ML.Add('Plugin=menu-plugin');
    ML.Add('Plugout=menu-plugout');
//    ML.Add('Synchronize=menu-synchronize');
    ML.Add('Upload=menu-upload');
    ML.Add('Download=menu-download');
  end;

//  ML := AddMenu('Provider');
//  ML.Add('#Scoring Module*=menu-scoring-module');
//  ML.Add('#Bridge Provider*=menu-bridge-provider');
//  ML.Add('#DB Interface*=menu-db-interface');
//  ML.Add('Workspace Location*=menu-workspace-location');
//  ML.Add('#Bridge Props*=menu-bridge-props');
//  ML.Add('#Ini Props*=menu-ini-props');

  ML := AddMenu('Options');
  ML.Add('Event Params*=menu-event-params');
//  ML.Add('#Regatta Props*=menu-regatta-props');
  ML.Add('Fleet Props*=menu-fleet-props');
//  ML.Add('#Uniqua Props*=menu-uniqua-props');
//  ML.Add('#Name Props*=menu-name-props');
//  ML.Add('#Event Props*=menu-event-props');

  ML := AddMenu('Diagnose');
//  ML.Add('Status=menu-diagnose-status');
//	ML.Add('WSInfo=menu-diagnose-wsi');
//  ML.Add('Ini=menu-diagnose-ini');
  ML.Add('Source=menu-diagnose-src');
  ML.Add('Txt=menu-diagnose-txt');
  ML.Add('Xml=menu-diagnose-xml');
//  ML.Add('TimingData=menu-diagnose-tmd');
//  ML.Add('TPT=menu-diagnose-tpr');
//  ML.Add('Index=menu-diagnose-idx');

  ML := AddMenu('Test');
  ML.Add('JSXML=menu-test-jsxml');
  ML.Add('RDXML=menu-test-rdxml');
//  ML.Add('PxmlI=menu-test-pxmli');
//  ML.Add('PxmlO=menu-test-pxmlo');

  ML := AddMenu('Undo');
  ML.Add('Show Log=menu-undo-showlog');
  ML.Add('Show Undo=menu-undo-showundo');
  ML.Add('Show Redo=menu-undo-showredo');
  ML.Add('Show Combi=menu-undo-showcombi');
//  ML.Add('Undo=menu-undo-undo');
//  ML.Add('Redo=menu-undo-redo');
end;

procedure TMenuBuilder.WriteMenuRemote(SL: TStrings);
var
  SLA: TStrings;
  TempSL: TStrings;
  ML: TStrings;
  m: Integer;
  c, r: Integer;
  i: Integer;
  n, a, s: string;

  function AddMenu(h: string): TStrings;
  begin
    ML := TStringList.Create;
    SLA.AddObject(h, ML);
    result := ML;
  end;
begin
  SLA := TStringList.Create;

  if IsAdmin then
    InitRemoteMenu01(SLA)
  else
    InitRemoteMenu02(SLA);

  //get RowCount
  m := 0;
  for i := 0 to SLA.Count - 1 do
  begin
    ML := TStrings(SLA.Objects[i]);
    if ML.Count > m then
      m := ML.Count;
  end;

  //write table
  SL.Add('<table border="1" cellpadding="1" cellspacing="1" id="table1">');

  //header row
  SL.Add('<tr>');
  for c := 0 to SLA.Count - 1 do
  begin
    SL.Add('<th>');
    SL.Add(SLA[c]);
    SL.Add('</th>');
  end;
  SL.Add('</tr>');

  for r := 0 to m - 1 do
  begin
    //body rows
    SL.Add('<tr>');
    for c := 0 to SLA.Count - 1 do
    begin
      TempSL := TStrings(SLA.Objects[c]);
      SL.Add('<td>');
      if r < TempSL.Count then
      begin
        n := TempSL.Names[r];
        a := TempSL.Values[n];
        if Length(n) > 1 then
        begin
          if n[1] = '#' then
          begin
            //Comments start with #
            //render menuitem-name as normal text, start with second char
            s := Copy(n, 2);
          end
          else
          begin
            s := Format('<a href="%s">%s</a>', [a, n]);
          end;
        end
        else
        begin
          s := '-';
        end;
      end
      else
      begin
        s := '&nbsp;';
      end;
      SL.Add(s);
      SL.Add('</td>');
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');

  //free MLs
  for i := 0 to SLA.Count - 1 do
  begin
    ML := TStrings(SLA.Objects[i]);
    ML.Free;
  end;
  SLA.Free;

  SL.Add('<pre>');
  SL.Add(WebProxy.WebStatusString);
  SL.Add('</pre>');
end;

function TMenuBuilder.ParseCommand(msg: string): TWebMenuCommand;
var
  wcmd: TWebMenuCommand;
begin
  if msg = 'open' then
    wcmd := wcmd_open
  else if msg = 'save' then
    wcmd := wcmd_save
  else if msg = 'saveas' then
    wcmd := wcmd_saveas
  else if msg = 'delete' then
    wcmd := wcmd_delete

  else if msg = 'backup' then
    wcmd := wcmd_backup
  else if msg = 'restore' then
    wcmd := wcmd_restore
  else if msg = 'clear' then
    wcmd := wcmd_clear
  else if msg = 'load-test-data' then
    wcmd := wcmd_load_test_data
  else if msg = 'cache-synchro' then
    wcmd := wcmd_cache_synchro
  else if msg = 'strict-mode' then
    wcmd := wcmd_strict_mode
  else if msg = 'relaxed-mode' then
    wcmd := wcmd_relaxed_mode
  else if msg = 'color-cycle' then
    wcmd := wcmd_color_cycle

  else if msg = 'connect' then
    wcmd := wcmd_connect
  else if msg = 'disconnect' then
    wcmd := wcmd_disconnect
  else if msg = 'plugin' then
    wcmd := wcmd_plugin
  else if msg = 'plugout' then
    wcmd := wcmd_plugout
  else if msg = 'upload' then
    wcmd := wcmd_upload
  else if msg = 'download' then
    wcmd := wcmd_download
  else if msg = 'synchronize' then
    wcmd := wcmd_synchronize

  else if msg = 'scoring-module' then
    wcmd := wcmd_scoring_module
  else if msg = 'bridge-provider' then
    wcmd := wcmd_bridge_provider
  else if msg = 'db-interface' then
    wcmd := wcmd_db_interface
  else if msg = 'workspace-location' then
    wcmd := wcmd_workspace_location
  //---
  else if msg = 'bridge-props' then
    wcmd := wcmd_bridge_properties
  else if msg = 'ini-props' then
    wcmd := wcmd_ini_properties

  else if msg = 'event-params' then
    wcmd := wcmd_event_params
  else if msg = 'regatta-props' then
    wcmd := wcmd_regatta_props
  else if msg = 'fleet-props' then
    wcmd := wcmd_fleet_props
  else if msg = 'uniqua-props' then
    wcmd := wcmd_uniqua_props
  else if msg = 'name-props' then
    wcmd := wcmd_name_props
  else if msg = 'event-props' then
    wcmd := wcmd_event_props

  else if msg = 'diagnose-status' then
    wcmd := wcmd_diagnose_status
  else if msg = 'diagnose-wsi' then
    wcmd := wcmd_diagnose_wsi
  else if msg = 'diagnose-ini' then
    wcmd := wcmd_diagnose_ini
  else if msg = 'diagnose-src' then
    wcmd := wcmd_diagnose_src
  else if msg = 'diagnose-txt' then
    wcmd := wcmd_diagnose_txt
  else if msg = 'diagnose-xml' then
    wcmd := wcmd_diagnose_xml
  else if msg = 'diagnose-tmd' then
    wcmd := wcmd_diagnose_tmd
  else if msg = 'diagnose-tpr' then
    wcmd := wcmd_diagnose_tpr
  else if msg = 'diagnose-idx' then
    wcmd := wcmd_diagnose_idx

  else if msg = 'test-jsxml' then
    wcmd := wcmd_test_jsxml
  else if msg = 'test-rdxml' then
    wcmd := wcmd_test_rdxml
  else if msg = 'test-pxmli' then
    wcmd := wcmd_test_pxmli
  else if msg = 'test-pxmlo' then
    wcmd := wcmd_test_pxmlo

  else if msg = 'undo-showlog' then
    wcmd := wcmd_undo_showlog
  else if msg = 'undo-showundo' then
    wcmd := wcmd_undo_showundo
  else if msg = 'undo-showredo' then
    wcmd := wcmd_undo_showredo
  else if msg = 'undo-showcombi' then
    wcmd := wcmd_undo_showcombi
  else if msg = 'undo-undo' then
    wcmd := wcmd_undo_undo
  else if msg = 'undo-redo' then
    wcmd := wcmd_undo_redo

  else
    wcmd := wcmd_none;

  result := wcmd;
end;

procedure TMenuBuilder.HandleStringCommand(msg: string);
var
  wcmd: TWebMenuCommand;
begin
  wcmd := ParseCommand(msg);
  if wcmd <> wcmd_none then
    HandleIntCommand(wcmd);
end;

procedure TMenuBuilder.HandleIntCommand(cmd: TWebMenuCommand);
begin
  case cmd of
    wcmd_save: WebProxy.SaveEventExecute;

    wcmd_backup: WebProxy.BackupExecute;
    wcmd_restore: WebProxy.RecreateExecute;
    wcmd_clear: WebProxy.ClearExecute;
    wcmd_load_test_data: WebProxy.LoadTestDataExecute;
    wcmd_cache_synchro: WebProxy.SynchronizeCacheExecute;
    wcmd_strict_mode: WebProxy.StrictModeExecute;
    wcmd_relaxed_mode: WebProxy.RelaxedModeExecute;
    wcmd_color_cycle: WebProxy.ColorCycleExecute;
    wcmd_undo_undo: WebProxy.UndoExecute;
    wcmd_undo_redo: WebProxy.RedoExecute;

    wcmd_connect: WebProxy.ConnectExecute;
    wcmd_disconnect: WebProxy.DisconnectExecute;
    wcmd_plugin: WebProxy.PluginExecute;
    wcmd_plugout: WebProxy.PlugoutExecute;
    wcmd_synchronize: WebProxy.SynchronizeExecute;
    wcmd_upload: WebProxy.UploadExecute;
    wcmd_download: WebProxy.DownloadExecute;
  end;
end;

function TMenuBuilder.GetReport(cmd: TWebMenuCommand): string;
var
  s: string;
begin
  case cmd of
    wcmd_diagnose_status: s := WebProxy.GetStatusString;
		wcmd_diagnose_wsi: s := WebProxy.GetWorkspaceInfoReport;
    wcmd_diagnose_ini: s := WebProxy.GetIniImage;
    wcmd_diagnose_src: s := WebProxy.GetConvertedEventData;
    wcmd_diagnose_txt: s := WebProxy.ToTXT;
    wcmd_diagnose_xml: s := WebProxy.ToXML;
    wcmd_diagnose_tmd: s := WebProxy.GetReport('FR.*.Output.Report.TimingData');
    wcmd_diagnose_tpr:
    begin
      s := 'FR.*.Output.Report.TW_TimePointTable.R' + IntToStr(WebProxy.Race);
      s := WebProxy.GetReport(s);
    end;
    wcmd_diagnose_idx:
    begin
      s := 'FR.*.Output.Report.IndexData';
      s := WebProxy.GetReport(s);
    end;
    wcmd_test_jsxml: s := WebProxy.GetJavaScoreXML;
    wcmd_test_rdxml: s := WebProxy.GetRaceDataXML;
    wcmd_test_pxmli:
    begin
      s := 'FR.*.Output.JavaScore.ProxyXmlInput';
      s := WebProxy.GetReport(s);
    end;
    wcmd_test_pxmlo:
    begin
      s := 'FR.*.Output.JavaScore.ProxyXmlOutput';
      s := WebProxy.GetReport(s);
    end;
    wcmd_undo_showlog: s := WebProxy.GetUndoManagerLog;
    wcmd_undo_showundo: s := WebProxy.GetUndoManagerUndo;
    wcmd_undo_showredo: s := WebProxy.GetUndoManagerRedo;
    wcmd_undo_showcombi: s := WebProxy.GetUndoManagerUndoRedo;

  end;
  result := '<pre>' + WebUtils.HTMLEncode(s) + '</pre>';
end;

end.
