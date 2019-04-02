unit RiggVar.Web1.MotorRemote;

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
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  IdSync,
  IdContext,
  IdCustomHTTPServer,
  RiggVar.Util.Classes,
  RiggVar.BO.ResourceManager,
  RiggVar.Web1.EventArgs,
  RiggVar.Web1.MenuHelper,
  RiggVar.Web1.TW03,
  RiggVar.Web1.TW05,
  RiggVar.Web1.TW08,
  RiggVar.Web1.CSS,
  RiggVar.Web1.MotorBase;

type
  TWebMotorRemote = class(TWebMotor)
  private
    FMenuCount: Integer;
    FCurrentMenu: Integer;
    TW03: TTW03;
    TW05: TTW05;
    TW08: TTW08;
    procedure SetCurrentMenu(const Value: Integer);
  protected
    function CheckAccess(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo): Boolean;
    function HasLogin(ARequestInfo: TIdHTTPRequestInfo): Boolean;
    function Authenticate(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHttpResponseInfo): Boolean;
    function CheckLogin: Boolean;
    function Authorized: Boolean;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    procedure GetAnswer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetTemp(value: string); override;
    property MenuCount: Integer read FMenuCount;
    property CurrentMenu: Integer read FCurrentMenu write SetCurrentMenu;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Web1.Proxy;

constructor TWebMotorRemote.Create;
begin
  inherited Create;
  FMenuCount := 4;
  CurrentMenu := 2;

  TW03 := TTW03.Create;
  TW03.HL := HL;
  TW03.SL := SL;

  TW05 := TTW05.Create;
  TW05.HL := HL;
  TW05.SL := SL;

  TW08 := TTW08.Create;
  TW08.HL := HL;
  TW08.SL := SL;
end;

destructor TWebMotorRemote.Destroy;
begin
  TW03.Free;
  TW05.Free;
  TW08.Free;
  inherited Destroy;
end;

procedure TWebMotorRemote.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  page: TPageEnum;
  sort: Integer;
  race: Integer;
  it: Integer;
  report: Integer;
  snr: Integer;
  bib: Integer;
  msg: string;
  login: Boolean;
begin
  s := LowerCase(ARequestInfo.Document);

  if IsOffline then
  begin
    AResponseInfo.ContentText := OfflineMsg;
    Exit;
  end;

  sort := 0;
  race := 1;
  it := 0;
  report := 0;
  snr := 0;
  bib := 0;
  msg := '';

  AResponseInfo.ContentType := 'text/html; charset=utf-8';

  if Matches(s, '/stylesheets/fr62.css') then
  begin
    page := pageFR62Css;
    AResponseInfo.ContentType := 'text/css; charset=utf-8';
  end
  else if Matches(s, '/stylesheets/fr42.css') then
  begin
    page := pageFR42Css;
    AResponseInfo.ContentType := 'text/css; charset=utf-8';
  end
  else if Matches(s, '/javascripts/core.js') then
  begin
    page := pageCore;
  end
  else if Matches(s, '/javascripts/rvts.js') then
  begin
    page := pageRvts;
  end
  else if Matches(s, '/cache') then
  begin
    page := pageCache;
  end
  else if Matches(s, '/entries') then
  begin
    page := pageEntries;
    snr := StrToIntDef(ARequestInfo.Params.Values['snr'], 0);
    msg := Trim(ARequestInfo.Params.Values['msg']);
  end
  else if Matches(s, '/rv') then
  begin
    page := pageRV;
    msg := Trim(ARequestInfo.Params.Values['msg']);
  end
  else if Matches(s, '/manage') then
  begin
    page := pageManage;
    msg := Trim(ARequestInfo.Params.Values['msg']);
  end
  else if Matches(s, '/swap') then
  begin
    page := pageMenuSwap;
    it := StrToIntDef(ARequestInfo.Params.Values['value'], CurrentMenu);
  end
  else if StartsWith(s, '/menu-') then
  begin
    page := pageMenuCommand;
    if Length(s) >  Length(Path + '/menu-')then
      msg := Copy(s, 1+ Length(Path + '/menu-'), 20);
  end
  else if Matches(s, '/menu') then
  begin
    page := pageMenu;
  end
  else if Matches(s, '/open') then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageOpenEvent;
    msg := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/saveas') then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageSaveAs;
    msg := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/delete') then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageDelete;
    msg := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/workspace-location') then
  begin
    msg := LowerCase(ARequestInfo.Params.Values['Btn']);
    if msg = 'ok' then
      Authenticate(ARequestInfo, AResponseInfo);
    race := WebProxy.WorkspaceType;
    it := WebProxy.WorkspaceID;
    page := pageWorkspace;
    race := StrToIntDef(ARequestInfo.Params.Values['WorkspaceType'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['WorkspaceID'], it);
  end
  else if Matches(s, '/event-params') then
  begin
    msg := LowerCase(ARequestInfo.Params.Values['Btn']);
    if msg = 'ok' then
      Authenticate(ARequestInfo, AResponseInfo);
    race := WebProxy.RaceCount;
    it := WebProxy.ITCount;
    bib := WebProxy.StartlistCount;
    page := pageEventParams;
    race := StrToIntDef(ARequestInfo.Params.Values['RaceCount'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['ITCount'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['StartlistCount'], bib);
  end
  else if Matches(s, '/fleet-props') then
  begin
    msg := LowerCase(ARequestInfo.Params.Values['Btn']);
    if msg = 'ok' then
      Authenticate(ARequestInfo, AResponseInfo);
    race := 0;
    it := WebProxy.TargetFleetSize;
    bib := WebProxy.FirstFinalRace;
    page := pageFleetProps;
    //checked: 'on' unchecked: ''
    if ARequestInfo.Params.Values['UseFleets'] = 'on' then
      race := 1;
    it := StrToIntDef(ARequestInfo.Params.Values['TargetFleetSize'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['FirstFinalRace'], bib);
  end

  else if Matches(s, '/finish') then
  begin
    page := pageFinish;
    sort := StrToIntDef(ARequestInfo.Params.Values['Sort'], 0);
  end
  else if Matches(s, '/points') then
  begin
    page := pagePoints;
    sort := StrToIntDef(ARequestInfo.Params.Values['Sort'], 0);
  end
  else if Matches(s, '/race') then
  begin
    page := pageRace;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
  end
  else if Matches(s, '/racexml') then
  begin
    page := pageRaceXml;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if IsOffline then
  begin
    AResponseInfo.ContentText := OfflineMsg;
    page := pageOffline;
  end
  else if Matches(s, '/load') then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageLoad;
    msg := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/save') then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageSave;
    msg := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/clear') then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageClear;
    msg := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/xml') then
  begin
    page := pageXml;
    report := StrToIntDef(ARequestInfo.Params.Values['Report'], 0);
  end
  else if Matches(s, '/help') then
    page := pageHelp
  else if Matches(s, '/info') then
    page := pageInfo
  else if Matches(s, '/widget') then
    page := pageWidget

  else if Matches(s, '/finishreport') then
    page := pageFinishReport
  else if Matches(s, '/pointsreport') then
    page := pagePointsReport

  else if Matches(s, '/timepointreport') then
    page := pageTimePointReport
  else if Matches(s, '/stylesheets/rvtp.xsl') then
  begin
    page := pageRvtp;
    AResponseInfo.ContentType := 'text/xsl';
  end

  else if StartsWith(s, '/tw01') then
  begin
    race := CurrentRace;
    it := CurrentIT;
//    if StartsWith(s, '/tw01-ajax') then
//    begin
//      AResponseInfo.ContentType := 'text/xml';
//      page := pageTW01ajax;
//    end
    if StartsWith(s, '/tw01-selector') then
    begin
      page := pageTW01Selector;
    end
    else if StartsWith(s, '/tw01-info') then
    begin
      page := pageTW01Info;
    end
    else //if StartsWith(s, '/tw01-widget') then
    begin
      page := pageTW01;
    end;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if Matches(s, '/javascripts/rvtw05.js') then
  begin
    page := pageTW05JS;
  end
  else if Matches(s, '/stylesheets/rvtw05.css') then
  begin
    page := pageTW05CSS;
    AResponseInfo.ContentType := 'text/css';
  end


  else if StartsWith(s, '/tw02') then
  begin
    race := CurrentRace;
    it := CurrentIT;
//    if StartsWith(s, '/tw02-ajax') then
//    begin
//      AResponseInfo.ContentType := 'text/xml';
//      page := pageTW02Ajax;
//    end
    if StartsWith(s, '/tw02-selector') then
    begin
      page := pageTW02Selector;
    end
    else if StartsWith(s, '/tw02-info') then
    begin
      page := pageTW02Info;
    end
    else //if StartsWith(s, '/tw02-widget') then
    begin
      page := pageTW02;
    end;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if Matches(s, '/javascripts/rvtw08.js') then
  begin
    page := pageTW08JS;
  end
  else if Matches(s, '/stylesheets/rvtw08.css') then
  begin
    page := pageTW08CSS;
    AResponseInfo.ContentType := 'text/css';
  end


  else if StartsWith(s, '/tw03') then
  begin
    race := CurrentRace;
    it := CurrentIT;
    if StartsWith(s, '/tw03-xml') then
    begin
      if not CheckAccess(AContext, ARequestInfo, AResponseInfo) then
        Exit;
      AResponseInfo.ContentType := 'text/xml';
      page := pageTW03XML; //xml update (via ajax)
    end
    else if StartsWith(s, '/tw03-table') then
    begin
      if not CheckAccess(AContext, ARequestInfo, AResponseInfo) then
        Exit;
      page := pageTW03Table; //table update (via ajax)
    end
    else //if StartsWith(s, '/tw03-widget') then
    begin
      page := pageTW03; //the timing-widget-page itself
    end;
    //OutputDebugString(PChar(ARequestInfo.QueryParams));
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
    //time is generated on server
    //time := StrToIntDef(ARequestInfo.Params.Values['Time'], 0);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if Matches(s, '/stylesheets/rvtw03.css') then
  begin
    page := pageTW03CSS;
    AResponseInfo.ContentType := 'text/css';
  end
  else if Matches(s, '/javascripts/rvtw03.js') then
  begin
    page := pageTW03JS
  end


  else if StartsWith(s, '/tw05') then
  begin
    race := CurrentRace;
    it := CurrentIT;
    if StartsWith(s, '/tw05-ajax') then
    begin
      if not CheckAccess(AContext, ARequestInfo, AResponseInfo) then
        Exit;
      AResponseInfo.ContentType := 'text/xml';
      page := pageTW05Ajax;
    end
    else if StartsWith(s, '/tw05-selector') then
    begin
      page := pageTW05Selector;
    end
    else if StartsWith(s, '/tw05-info') then
    begin
      page := pageTW05Info;
    end
    else //if StartsWith(s, '/tw05-widget') then
    begin
      page := pageTW05;
    end;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if Matches(s, '/javascripts/rvtw05.js') then
  begin
    page := pageTW05JS;
  end
  else if Matches(s, '/stylesheets/rvtw05.css') then
  begin
    page := pageTW05CSS;
    AResponseInfo.ContentType := 'text/css';
  end


  else if StartsWith(s, '/tw08') then
  begin
    race := CurrentRace;
    it := CurrentIT;
    if StartsWith(s, '/tw08-ajax') then
    begin
      if not CheckAccess(AContext, ARequestInfo, AResponseInfo) then
        Exit;
      AResponseInfo.ContentType := 'text/xml';
      page := pageTW08Ajax;
    end
    else if StartsWith(s, '/tw08-selector') then
    begin
      page := pageTW08Selector;
    end
    else if StartsWith(s, '/tw08-info') then
    begin
      page := pageTW08Info;
    end
    else //if StartsWith(s, '/tw08-widget') then
    begin
      page := pageTW08;
    end;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    it := StrToIntDef(ARequestInfo.Params.Values['IT'], it);
    bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if Matches(s, '/javascripts/rvtw08.js') then
  begin
    page := pageTW08JS;
  end
  else if Matches(s, '/stylesheets/rvtw08.css') then
  begin
    page := pageTW08CSS;
    AResponseInfo.ContentType := 'text/css';
  end


  else if MatchesPath(s, '/marktable') then
  begin
    if not CheckAccess(AContext, ARequestInfo, AResponseInfo) then
      Exit;
    //table only, no race selector form
    page := pageMarkTable;
    race := CurrentRace;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    AResponseInfo.CacheControl := 'no-cache';
  end
  else if MatchesPath(s, '/markreport') then
  begin
    //with RaceSelectorForm
    page := pageMarkReport;
    race := CurrentRace;
    race := StrToIntDef(ARequestInfo.Params.Values['Race'], race);
    AResponseInfo.CacheControl := 'no-cache';
  end

  else if (Matches(s, '/') or Matches(s, '/index')) then
  begin
    page := pageIndex;
  end

  else if (Matches(s, '/login')) then
  begin
    Authenticate(ARequestInfo, AResponseInfo);
    page := pageIndex;
  end

  else
  begin
    AResponseInfo.Redirect(Path + '/Index');
    Exit;
  end;

  login := HasLogin(ARequestInfo);

  Lock.Acquire;
  try
    EA._login := login;
    EA._page := page;
    EA._sort := sort;
    EA._race := race;
    EA._it := it;
    EA._report := report;
    EA._doc := s;
    EA._msg := msg;
    EA._snr := snr;
    EA._bib := bib;
    EA.RaceCount := WebProxy.RaceCount;
    EA.ITCount := WebProxy.ITCount;
    TThread.Synchronize(nil, GetAnswer);
    AResponseInfo.ContentText := EA._answer;
  finally
    Lock.Release;
  end;
end;

procedure TWebMotorRemote.GetAnswer;
var
  a: string;
  wcmd: TWebMenuCommand;
begin
  HL.Clear;
  ML.Clear;
  SL.Clear;
  HL.Add('<title>FR Remote</title>');

  EA._answer := '-';
  case EA._page of

    pageIndex:
    begin
      EA._answer := InitPage('/index', '', '');
    end;

    pageHelp:
    begin
      EA._answer := InitPage('/help', '', 'Readme Placeholder');
    end;

    pageInfo:
    begin
      EA._answer := InitPage('/info', '', '(c) RiggVar 2009');
    end;

    pageOffline:
    begin
      EA._answer := InitPage('/offline', '', '');
    end;

    pageLoad:
    begin
      if EA._msg = '' then
      begin
        EA._answer := InitPage('/load', '', DateTimeToStr(Now));
      end
      else if CheckLogin then
      begin
        EA._page := pageLoad;
        EA._answer := InitPage('/command', '', DateTimeToStr(Now));
      end;
    end;

    pageSave:
    begin
      if EA._msg = '' then
      begin
        EA._answer := InitPage('/save', '', DateTimeToStr(Now));
      end
      else if CheckLogin then
      begin
        EA._page := pageSave;
        EA._answer := InitPage('/command', '', DateTimeToStr(Now));
      end;
    end;

    pageClear:
    begin
      if EA._msg = '' then
      begin
        EA._answer := InitPage('/clear', '', DateTimeToStr(Now));
      end
      else if CheckLogin then
      begin
        EA._page := pageClear;
        EA._answer := InitPage('/command', '', DateTimeToStr(Now));
      end;
    end;

    pageMenuSwap:
    begin
      CurrentMenu := EA._it;
      EA._answer := InitPage('/swap', '', DateTimeToStr(Now));
    end;

    pageMenu:
    begin
      //just show the menu
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
    end;

    pageMenuCommand:
    begin
      wcmd := MenuBuilder.ParseCommand(EA._msg);
      case wcmd of
        wcmd_open:
        begin
          EA._page := pageOpenEvent;
          EA._answer := InitPage('/open', '', DateTimeToStr(Now));
        end;
        wcmd_saveas:
        begin
          EA._page := pageSaveAs;
          EA._answer := InitPage('/saveas', '', DateTimeToStr(Now));
        end;
        wcmd_delete:
        begin
          EA._page := pageDelete;
          EA._answer := InitPage('/delete', '', DateTimeToStr(Now));
        end;
        wcmd_workspace_location:
        begin
          EA._page := pageWorkspace;
          EA._answer := InitPage('/workspace-location', '', DateTimeToStr(Now));
        end;
        wcmd_event_params:
        begin
          EA._page := pageEventParams;
          EA._answer := InitPage('/event-params', '', DateTimeToStr(Now));
        end;
        wcmd_fleet_props:
        begin
          EA._page := pageFleetProps;
          EA._answer := InitPage('/fleet-props', '', DateTimeToStr(Now));
        end;

        wcmd_diagnose_status,
        wcmd_diagnose_wsi,
        wcmd_diagnose_ini,
        wcmd_diagnose_src,
        wcmd_diagnose_txt,
        wcmd_diagnose_xml,
        wcmd_diagnose_tmd,
        wcmd_diagnose_tpr,
        wcmd_diagnose_idx,
        wcmd_test_jsxml,
        wcmd_test_rdxml,
        wcmd_test_pxmli,
        wcmd_test_pxmlo,
  	  	wcmd_undo_showlog,
	  	  wcmd_undo_showundo,
    		wcmd_undo_showredo,
	      wcmd_undo_showcombi:
        begin
          EA._page := pageMenu;
          a := MenuBuilder.GetReport(wcmd);
          EA._answer := InitPage('/menu', a, DateTimeToStr(Now));
        end

//      wcmd_save
//      wcmd_connect,
//      wcmd_disconnect,
//      wcmd_plugin,
//      wcmd_plugout,
//      wcmd_synchronize,
//      wcmd_upload,
//      wcmd_download,
        else if CheckLogin then
        begin
          MenuBuilder.HandleStringCommand(EA._msg);
          EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
        end;
      end;
    end;

    pageOpenEvent:
    begin
      if EA._msg = '' then
      begin
        EA._answer := InitPage('/open', '', DateTimeToStr(Now));
      end
      else if CheckLogin then
      begin
        WebProxy.OpenEventExecute(EA._msg);
        EA._page := pageMenuCommand;
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
      end;
    end;

    pageSaveAs:
    begin
      if EA._msg = '' then
      begin
        EA._answer := InitPage('/saveas', '', DateTimeToStr(Now));
      end
      else if CheckLogin then
      begin
        WebProxy.SaveEventAsExecute(EA._msg);
        EA._page := pageMenuCommand;
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
      end;
    end;

    pageDelete:
    begin
      if EA._msg = '' then
      begin
        EA._answer := InitPage('/delete', '', DateTimeToStr(Now));
      end
      else if CheckLogin then
      begin
        WebProxy.DeleteEventExecute(EA._msg);
        EA._page := pageMenuCommand;
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
      end;
    end;

    pageWorkspace:
    begin
      if EA._msg = 'ok' then
      begin
        if CheckLogin then
        begin
          WebProxy.UpdateWorkspace(EA._race, EA._it);
          EA._page := pageMenuCommand;
          EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
        end;
      end
      else if EA._msg = 'cancel' then
      begin
        EA._page := pageMenuCommand;
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
      end
      else
      begin
        EA._answer := InitPage('/workspace-location', '', DateTimeToStr(Now));
      end;
    end;

    pageEventParams:
    begin
      if EA._msg = 'ok' then
      begin
        if CheckLogin then
        begin
          WebProxy.UpdateEventParams(EA._race, EA._it, EA._bib);
          EA._page := pageMenuCommand;
          EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
        end;
      end
      else if EA._msg = 'cancel' then
      begin
        EA._page := pageMenuCommand;
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
      end
      else
      begin
        EA._answer := InitPage('/event-params', '', DateTimeToStr(Now));
      end;
    end;

    pageFleetProps:
    begin
      if EA._msg = 'ok' then
      begin
        if CheckLogin then
        begin
          WebProxy.UpdateFleetProps(EA._race = 1, EA._it, EA._bib);
          EA._page := pageMenuCommand;
          EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
        end;
      end
      else if EA._msg = 'cancel' then
      begin
        EA._page := pageMenuCommand;
        EA._answer := InitPage('/menu', '', DateTimeToStr(Now));
      end
      else
      begin
        EA._answer := InitPage('/fleet-props', '', DateTimeToStr(Now));
      end;
    end;

    pageCache:
    begin
      EA._answer := InitPage('/cache', '', WebProxy.CacheReport);
    end;

    pageEntries:
    begin
      //msg is in Form 'FN=abcdefg'
      if (EA._snr > 0) then
      begin
        a := Format('FR.*.SNR%d.%s', [EA._snr, EA._msg])
      end
      else
      begin
        EA._snr := 0; //override with harmless value
        a := ''; //does not process input
      end;
      if a <> '' then
      begin
        if Authorized then
          a := WebProxy.UpdateEntry(a)
        else
          a := '<p>operation skipped, no login present.</p>';
      end;
      EA._answer := InitPage('/entries', a, DateTimeToStr(Now));
    end;

    pageRV:
    begin
      a := '';
      //msg is in Form '*.RV=abcdefg'
      if EA._msg <> '' then
        if Authorized then
          a := WebProxy.UpdateRaceValue(EA._msg)
        else
          a := '<p>operation skipped, no login present.</p>';
      EA._answer := InitPage('/rv', a, DateTimeToStr(Now));
    end;

    pageManage:
    begin
      //msg is given verbatim
      a := '';
      if EA._msg <> '' then
        if Authorized then
          a := WebProxy.ExecuteCommand(EA._msg)
        else
          a := '<p>operation skipped, no login present.</p>';
      EA._answer := InitPage('/manage', a, DateTimeToStr(Now));
    end;

    pageFinish:
    begin
      a := WebProxy.GetEventReport(11, EA._sort, False);
      EA._answer := InitPage('/event/finish/' + IntToStr(EA._sort), a, DateTimeToStr(Now));
    end;

    pagePoints:
    begin
      a := WebProxy.GetEventReport(11, EA._sort, True);
      EA._answer := InitPage('/event/points/' + IntToStr(EA._sort), a, DateTimeToStr(Now));
    end;

    pageRace:
    begin
      a := WebProxy.GetRaceReport(105, EA._race, EA._it);
      EA._answer := InitPage('/race', a, DateTimeToStr(Now));
    end;

    pageRaceXml:
    begin
      EA._answer := WebProxy.GetRaceXml(EA._race, EA._it);
    end;

    pageXml:
    begin
      if EA._report = 0 then
        EA._answer := InitPage('/xml', '', DateTimeToStr(Now))
      else
        EA._answer := WebProxy.GetXmlReport(EA._report);
    end;

    pageWidget:
    begin
      EA._answer := InitPage('/widget', '', '(c) RiggVar 2009');
    end;

    pageFinishReport:
    begin
      EA._answer := WebProxy.FinishReport;
      WrapReport('/finish');
    end;

    pagePointsReport:
    begin
      EA._answer := WebProxy.PointsReport;
      WrapReport('/points');
    end;

    pageFR42CSS:
    begin
      EA._answer := InitFR42Css;
    end;

    pageFR62CSS:
    begin
      EA._answer := InitFR62Css;
    end;

    pageCore:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(core_js)
      else
        EA._answer := ResourceManager.LoadFile(core_js)
    end;

    pageRvts:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvts_js)
      else
        EA._answer := ResourceManager.LoadFile(rvts_js)
    end;

    pageTimePointReport:
    begin
      //TimePointReport is xml with xsl reference
      //partial client-side transform using rvtp.xsl
      //note: the menu is inserted from /stylesheets/rvtp.xsl
      //--> need a maintain a 'copy' of 'WriteMenu' inside rvtp.xsl
      EA._answer := WebProxy.TimePointReport;
    end;

    pageRvtp:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtp_xsl)
      else
        EA._answer := ResourceManager.LoadFile(rvtp_xsl)
    end;

    pageTW01:
    begin
      WriteMenu(ML);
      TW05.WriteEmbeddedWidget(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := MasterPage.Text;
    end;
//    pageTW01CSS:
//    begin
//      if UseResource then
//        EA._answer := ResourceManager.LoadText(rvtw05_css)
//      else
//        EA._answer := ResourceManager.LoadFile(rvtw05_css)
//    end;
//    pageTW01JS:
//    begin
//      if UseResource then
//        EA._answer := ResourceManager.LoadText(rvtw05_js)
//      else
//        EA._answer := ResourceManager.LoadFile(rvtw05_js)
//    end;
//    pageTW01Ajax:
//    begin
//      EA._answer := WebProxy.Handle_TW_Ajax(EA._race, EA._it, EA._bib);
//    end;
    pageTW01Selector:
    begin
      WriteMenu(ML);
      TW05.WriteEmbeddedSelector(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := MasterPage.Text;
    end;
    pageTW01Info:
    begin
      WriteMenu(ML);
      TW05.WriteEmbeddedInfo;
      EA._answer := MasterPage.Text;
    end;

    pageTW02:
    begin
      WriteMenu(ML);
      TW08.WriteEmbeddedWidget(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := MasterPage.Text;
    end;
//    pageTW02CSS:
//    begin
//      if UseResource then
//        EA._answer := ResourceManager.LoadText(rvtw08_css)
//      else
//        EA._answer := ResourceManager.LoadFile(rvtw08_css)
//    end;
//    pageTW02JS:
//    begin
//      if UseResource then
//        EA._answer := ResourceManager.LoadText(rvtw08_js)
//      else
//        EA._answer := ResourceManager.LoadFile(rvtw08_js)
//    end;
//    pageTW02Ajax:
//    begin
//      //same as TW05
//      EA._answer := WebProxy.Handle_TW_Ajax(EA._race, EA._it, EA._bib);
//    end;
    pageTW02Selector:
    begin
      WriteMenu(ML);
      TW08.WriteEmbeddedSelector(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := MasterPage.Text;
    end;
    pageTW02Info:
    begin
      WriteMenu(ML);
      TW08.WriteEmbeddedInfo;
      EA._answer := MasterPage.Text;
    end;

    pageTW03XML:
    begin
      //request may be ajax
      //time for bib is generated in CacheMotor
      EA._answer := WebProxy.Handle_TW_XML(EA._race, EA._it, EA._bib);
    end;
    pageTW03Table:
    begin
      //request may be ajax
      //time for bib is generated in CacheMotor
      EA._answer := WebProxy.Handle_TW_Table(EA._race, EA._it, EA._bib);
    end;
    pageTW03:
    begin
      WriteMenu(ML);
      TW03.WriteWidget(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := MasterPage.Text;
    end;
    pageTW03JS:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtw03_js)
      else
        EA._answer := ResourceManager.LoadFile(rvtw03_js)
    end;
    pageTW03CSS:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtw03_css)
      else
        EA._answer := ResourceManager.LoadFile(rvtw03_css)
    end;

    pageTW05:
    begin
      TW05.WriteWidget(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := SL.Text;
    end;
    pageTW05CSS:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtw05_css)
      else
        EA._answer := ResourceManager.LoadFile(rvtw05_css)
    end;
    pageTW05JS:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtw05_js)
      else
        EA._answer := ResourceManager.LoadFile(rvtw05_js)
    end;
    pageTW05Ajax:
    begin
      EA._answer := WebProxy.Handle_TW_Ajax(EA._race, EA._it, EA._bib);
    end;
    pageTW05Selector:
    begin
      TW05.WriteSelector(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := SL.Text;
    end;
    pageTW05Info:
    begin
      TW05.WriteInfo;
      EA._answer := SL.Text;
    end;

    pageTW08:
    begin
      TW08.WriteWidget(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := SL.Text;
    end;
    pageTW08CSS:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtw08_css)
      else
        EA._answer := ResourceManager.LoadFile(rvtw08_css)
    end;
    pageTW08JS:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtw08_js)
      else
        EA._answer := ResourceManager.LoadFile(rvtw08_js)
    end;
    pageTW08Ajax:
    begin
      //same as TW05
      EA._answer := WebProxy.Handle_TW_Ajax(EA._race, EA._it, EA._bib);
    end;
    pageTW08Selector:
    begin
      TW08.WriteSelector(EA.RaceCount, EA.ITCount, EA._race, EA._it);
      EA._answer := SL.Text;
    end;
    pageTW08Info:
    begin
      TW08.WriteInfo;
      EA._answer := SL.Text;
    end;


    pageMarkReport:
    begin
      WriteHeader;
      SL.Add('<h2>/mark</h2>');
      WriteRaceSelectorForm('markreport');
      EA.RaceCount := WebProxy.RaceCount;
      if (EA._race > 0) and (EA._race <= EA.RaceCount) then
      begin
        //like TimingWidget-TimePointTable, but without time generation for bib
        EA._answer := WebProxy.Handle_TW_Report(EA._race);
        SL.Add(Format('<h3>Race %d</h3>', [EA._race]));
        InsertReport(EA._answer);
      end;
      EA._answer := MasterPage.Text;
    end;

    pageMarkTable:
    begin
      WriteHeader;
      EA.RaceCount := WebProxy.RaceCount;
      if (EA._race > 0) and (EA._race <= EA.RaceCount) then
      begin
        //like TimingWidget-TimePointTable, but without time generation for bib
        EA._answer := WebProxy.Handle_TW_Report(EA._race);
        SL.Add(Format('<h3>Race %d</h3>', [EA._race]));
        InsertReport(EA._answer);
      end
      else begin
        if (EA._page = pageMarkTable) then
        begin
          SL.Add('<p>no valid race-param in request</p>');
        end;
      end;
      EA._answer := MasterPage.Text;
    end;

  end;

  HL.Clear;
  ML.Clear;
  SL.Clear;
end;

procedure TWebMotorRemote.SetCurrentMenu(const Value: Integer);
begin
  if (Value >= 0) and (Value < MenuCount) then
    FCurrentMenu := Value
  else
    FCurrentMenu := 0;
  MenuBuilder.CurrentMenu := FCurrentMenu;
end;

procedure TWebMotorRemote.SetTemp(value: string);
begin
  inherited;
  if value = '/intern' then
    CurrentMenu := 1
  else if value = '/widget' then
    CurrentMenu := 3
  else
    CurrentMenu := 2;
end;

function TWebMotorRemote.CheckAccess(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
var
  o: string;
  h: string;
begin
  result := false;

  h := ARequestInfo.Host;

  o := '';
  if ARequestInfo.RawHeaders.IndexOfName('Origin') > -1 then
    o := ARequestInfo.RawHeaders.Values['Origin'];

//  if Pos('gsmxp', h) > 0 then
//  begin
//    result := true;
//    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', '*');
//  end

  if o = '' then
  begin
    result := true;
  end

  else if Main.IsScenarioEC2 then
  begin
    if Pos('http://script.riggvar.de', o) = 1 then
    begin
      result := true;
      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', o);
    end
    else if Pos('http://widgets.riggvar.de', o) = 1 then
    begin
      result := true;
      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', o);
    end
    else if Pos('http://info.riggvar.de', o) = 1 then
    begin
      result := true;
      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', o);
    end
    else if Pos('http://www.riggvar.de', o) = 1 then
    begin
      result := true;
      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', o);
    end
    else
      AResponseInfo.ContentText := 'not allowed';
  end

  else
  begin
    result := true;
    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', '*');
  end
end;

function TWebMotorRemote.Authenticate(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHttpResponseInfo): Boolean;
var
  AuthOK: Boolean;
begin
  result := false;

  if not Main.Params.WantAuthentication then
  begin
    result := true;
    Exit;
  end;

  AuthOK := (ARequestInfo.AuthUsername = 'gs') and
    (ARequestInfo.AuthPassword = 'abc');

  if (AuthOK) then
  begin
    //Authentication OK
    result := true;
  end
  else if ARequestInfo.AuthExists then
  begin
    //Authentication Required
    //AResponseInfo.ResponseNo := 403;
  end
  else
    //if not ARequestInfo.AuthExists then
    //  show login dialog
    AResponseInfo.AuthRealm := 'RiggVar FR (Remote)';
end;

function TWebMotorRemote.HasLogin(ARequestInfo: TIdHTTPRequestInfo): Boolean;
begin
  result := (ARequestInfo.AuthUsername = 'gs') and
    (ARequestInfo.AuthPassword = 'abc');
end;

function TWebMotorRemote.CheckLogin: Boolean;
begin
  result := EA._login;
  if not result then
  begin
    EA._page := pageAccessDenied;
    EA._answer := InitPage('/Access Control', '', DateTimeToStr(Now));
  end;
end;

function TWebMotorRemote.Authorized: Boolean;
begin
  if not Main.Params.WantAuthentication then
    result := true
  else
    result := EA._login;
end;

end.
