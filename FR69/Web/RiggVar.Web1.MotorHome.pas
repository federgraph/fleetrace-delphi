unit RiggVar.Web1.MotorHome;

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
  IdContext,
  IdCustomHTTPServer,
  RiggVar.Util.Classes,
  RiggVar.BO.ResourceManager,
  RiggVar.Web1.EventArgs,
  RiggVar.Web1.MenuHelper,
  RiggVar.Web1.TW03,
  RiggVar.Web1.TW05,
  RiggVar.Web1.TW08,
  RiggVar.Web1.MotorBase;

type
  TWebMotorHome = class(TWebMotor)
  protected
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    procedure GetAnswer;
    procedure WriteMenu(o: TStrings); override;
  end;

implementation

uses
  RiggVar.Web1.Proxy,
  RiggVar.BO.Def;

procedure TWebMotorHome.HandleRequest(AContext: TIdContext;
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
begin
  s := LowerCase(ARequestInfo.Document);

  sort := 0;
  race := 1;
  it := 0;
  report := 0;
  snr := 0;
  bib := 0;
  msg := '';

  AResponseInfo.ContentType := 'text/html; charset=utf-8';

  if Matches(s, '/ED1') then
  begin
    page := pageData;
    race := 1;
  end
  else if Matches(s, '/ED2') then
  begin
    page := pageData;
    race := 2;
  end
  else if Matches(s, '/ED3') then
  begin
    page := pageData;
    race := 3;
  end
  else if Matches(s, '/help') then
    page := pageHelp
  else if Matches(s, '/info') then
    page := pageInfo

  else if Matches(s, '/stylesheets/fr62.css') then
  begin
    page := pageFR62Css;
    AResponseInfo.ContentType := 'text/css; charset=utf-8';
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
  else if Matches(s, '/xml') then
  begin
    page := pageXml;
    report := StrToIntDef(ARequestInfo.Params.Values['Report'], 0);
  end
  else if Matches(s, '/finishreport') then
    page := pageFinishReport
  else if Matches(s, '/pointsreport') then
    page := pagePointsReport
  else if Matches(s, '/stylesheets/fr42.css') then
  begin
    page := pageFR42Css;
    AResponseInfo.ContentType := 'text/css; charset=utf-8';
  end
  else if Matches(s, '/javascripts/core.js') then
    page := pageCore
  else if Matches(s, '/javascripts/rvts.js') then
    page := pageRvts



  else if Matches(s, '/timepointreport') then
    page := pageTimePointReport
  else if Matches(s, '/stylesheets/rvtp.xsl') then
  begin
    page := pageRvtp;
    AResponseInfo.ContentType := 'text/xsl';
  end


  else if MatchesPath(s, '/marktable') then
  begin
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

  else
  begin
    AResponseInfo.Redirect('/Home/Index');
    Exit;
  end;

  Lock.Acquire;
  try
    EA._page := page;
    EA._sort := sort;
    EA._race := race;
    EA._it := it;
    EA._report := report;
    EA._doc := s;
    EA._msg := msg;
    EA._snr := snr;
    EA._bib := bib;
    TThread.Synchronize(nil, GetAnswer);
    AResponseInfo.ContentText := EA._answer;
  finally
    Lock.Release;
  end;
end;

procedure TWebMotorHome.WriteMenu(o: TStrings);
begin
  o.Add('<div id="webmotor-menu"><p>');
  MenuBuilder.WriteMenuHome(o);
  o.Add('</p></div>');
end;

procedure TWebMotorHome.GetAnswer;
var
  a: string;
begin
  HL.Clear;
  ML.Clear;
  SL.Clear;
  HL.Add('<title>FR Home</title>');

  EA._answer := '-';
  case EA._page of

    pageFinish:
    begin
      a := WebProxy.GetEventReport(11, EA._sort, False);
      EA._answer := InitPage('/finish/' + IntToStr(EA._sort), a, DateTimeToStr(Now));
    end;

    pagePoints:
    begin
      a := WebProxy.GetEventReport(11, EA._sort, True);
      EA._answer := InitPage('/points/' + IntToStr(EA._sort), a, DateTimeToStr(Now));
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

    pageHelp:
    begin
      EA._answer := InitPage('/help', '', 'Readme Placeholder');
    end;

    pageInfo:
    begin
      EA._answer := InitPage('/info', '', '(c) RiggVar 2008');
    end;

    pageIndex:
    begin
      EA._answer := InitPage('/index', '', '');
    end;

    pageData:
    begin
      EA._answer := 'not implemented';
      EA._answer := BO.Data[EA._race];
      WrapReport1;
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
      //--> need to maintain a 'copy' of 'WriteMenu' inside rvtp.xsl
      EA._answer := WebProxy.TimePointReport;
    end;

    pageRvtp:
    begin
      if UseResource then
        EA._answer := ResourceManager.LoadText(rvtp_xsl)
      else
        EA._answer := ResourceManager.LoadFile(rvtp_xsl)
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

end.

