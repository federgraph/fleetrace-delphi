unit RiggVar.Out.FR00;

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
  RiggVar.Util.Classes,
  RiggVar.BO.MsgToken,
  RiggVar.Col.BaseEntry,
  RiggVar.Out.Base,
  RiggVar.Out.Intf,
  RiggVar.Out.GridBlock,
  RiggVar.Out.Json,
  RiggVar.Out.FR01,
  RiggVar.Out.FR02,
  RiggVar.Out.FR03,
  RiggVar.Out.FR04,
  RiggVar.Out.FR05,
  RiggVar.Out.FR06,
  RiggVar.Out.FR07,
  RiggVar.Out.FR08,
  RiggVar.Out.FR09,
  RiggVar.Out.FR10;

type
  TOutput = class(TBaseOutput)
  private
    Output1: TOutput1;
    Output2: TOutput2;
    Output3: TOutput3;
    Output4: TOutput4;
    Output5: TOutput5;
    Output6: TOutput6;
    Output7: TOutput7;
    Output8: TOutput8;
    Output9: TOutput9;
    Output10: TOutput10;
    JsonOut: TOutputJson;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMsg(sRequest: string): string; override;
    function EventReport(AReportID, ASort: Integer; AFinish: Boolean): string;
    function RaceReport(AReportID, ARace, AIT: Integer): string;
  end;

implementation

uses
  RiggVar.BO.Def;

{ TOutput }

constructor TOutput.Create;
begin
  inherited Create;
  JsonOut := TOutputJson.Create;
  Output1 := TOutput1.Create(self);
  Output2 := TOutput2.Create;
  Output3 := TOutput3.Create;
  Output4 := TOutput4.Create;
  Output5 := TOutput5.Create(self);
  Output6 := TOutput6.Create;
  Output7 := TOutput7.Create;
  Output8 := TOutput8.Create;
  Output8.SL := SL;
  Output9 := TOutput9.Create;
  Output10 := TOutput10.Create;
end;

destructor TOutput.Destroy;
begin
  JsonOut.Free;
  Output10.Free;
  Output9.Free;
  Output8.Free;
  Output7.Free;
  Output6.Free;
  Output5.Free;
  Output4.Free;
  Output3.Free;
  Output2.Free;
  Output1.Free;
  inherited Destroy;
end;

function TOutput.GetMsg(sRequest: string): string;
var
  temp: string;
  c: Integer;
  r: Integer;
  s: string;
  s1: string;
begin
  XMLSection := True;
  SL.Clear;
  Inc(MsgID);

  if Pos(cTokenA + '.' +cTokenB + '.Output.XML', sRequest) > 0 then
    WantPageHeader := True;

  if sRequest = cTokenOutput + 'PageHeaderOn' then
  begin
    Self.WantPageHeader := True;
    result := 'PageHeader On';
    exit;
  end
  else if sRequest = cTokenOutput + 'PageHeaderOff' then
  begin
    Self.WantPageHeader := False;
    result := 'PageHeader Off';
    exit;
  end;

  c := Pos(cTokenAnonymousOutput, sRequest);
  if c = 1 then
  begin
    temp := Copy(sRequest, Length(cTokenAnonymousOutput) + 1, Length(sRequest));
  end
  else
  begin
    c := Pos(cTokenOutput, sRequest);
    if c = 1 then
      temp := Copy(sRequest, Length(cTokenOutput) + 1, Length(sRequest));
  end;

  if c = 1 then
  begin

    { CSV, HTM, XML }
    WantHTMEscape := False;
    OutputType := otCSV;
    if Pos('CSV', temp) = 1 then
    begin
      OutputType := otCSV;
      temp := Copy(temp, 5, Length(temp));
    end
    else if Pos('HTM', temp) = 1 then
    begin
      OutputType := otHTM;
      temp := Copy(temp, 5, Length(temp));
    end
    else if Pos('XML', temp) = 1 then
    begin
      OutputType := otXML;
      temp := Copy(temp, 5, Length(temp));
    end
    else if Pos('CSM', temp) = 1 then
    begin
      OutputType := otCSV;
      WantHTMEscape := True;
      temp := Copy(temp, 5, Length(temp));
    end
    else if Pos('XMM', temp) = 1 then
    begin
      OutputType := otXML;
      WantHTMEscape := True;
      temp := Copy(temp, 5, Length(temp));
    end;

    if WantPageHeader then PageHeader;

    { namespace Web }
    if Copy(temp, 1, 4) = 'Web.' then
    begin
      Output7.GetMsg(SL, Copy(temp, 5, Length(temp)));
    end

    { namespace Info }
    else if Copy(temp, 1, 5) = 'Info.' then
    begin
      Output2.GetMsg(SL, Copy(temp, 6, Length(temp)));
    end

    { namespace Race }
    else if Copy(temp, 1, 5) = 'Race.' then
    begin
      Output3.GetMsg(SL, Copy(temp, 6, Length(temp)));
    end

    { namespace IT }
    else if Copy(temp, 1, 3) = 'IT.' then
    begin
      Output4.GetMsg(SL, Copy(temp, 4, Length(temp)));
    end

    { namespace JavaScore }
    else if Copy(temp, 1, 10) = 'JavaScore.' then
    begin
      if Copy(temp, 1, 22) = 'JavaScore.ScoringNotes' then
        BO.CalcEV.GetScoringNotes(SL)
      else if Copy(temp, 1, 13) = 'JavaScore.XML' then
        BO.JavaScoreXML.GetXML(SL)
      else if Copy(temp, 1, 23) = 'JavaScore.ProxyXmlInput' then
        Output5.ProxyXmlInput
      else if Copy(temp, 1, 24) = 'JavaScore.ProxyXmlOutput' then
        Output5.ProxyXmlOutput
    end

    { namespace RiggVar }
    else if Copy(temp, 1, 8) = 'RiggVar.' then
    begin
      if Copy(temp, 1, 14) = 'RiggVar.Params' then
        Output1.Params
      else if Copy(temp, 1, 11) = 'RiggVar.TXT' then
        Output1.BackupPreTXT
      else if Copy(temp, 1, 11) = 'RiggVar.FR.' then
        Output1.RaceXml(temp);
    end

    { namespace ASPNET }
    else if Copy(temp, 1, 7) = 'ASPNET.' then
    begin
      BO.EventNode.EventRowCollection.GetXML(SL);
    end

    { namespace Report }
    else if Copy(temp, 1, 7) = 'Report.' then
    begin
      if Copy(temp, 1, 19) = 'Report.RaceData.SQL' then
        BO.GetRaceDataSQL(SL)
      else if Copy(temp, 1, 21) = 'Report.SeriesData.SQL' then
        BO.GetSeriesDataSQL(SL)
      else if Copy(temp, 1, 27) = 'Report.NarrowRaceTable.json' then
      begin
        JsonOut.NarrowRaceTableJson(SL);
      end
      else if Copy(temp, 1, 25) = 'Report.WideRaceTable.json' then
      begin
        JsonOut.WideRaceTableJson(SL);
      end
      else if Copy(temp, 1, 11) = 'Report.Race' then
      begin
        s := Copy(temp, 11 + 1, Length(temp)-11);
        r := StrToIntDef(s, 1);
        BO.GetReportDataRace(SL, r);
      end
      else if Copy(temp, 1, 17) = 'Report.TimingData' then
      begin
        Output3.GetTimingReport(SL);
      end
      else if Copy(temp, 1, 16) = 'Report.IndexData' then
      begin
        Output6.IndexReport(SL);
      end
      else if Copy(temp, 1, 15) = 'Report.CssTable' then
      begin
        Output6.CssReport(SL);
      end
      else if Copy(temp, 1, 18) = 'Report.FinishTable' then
      begin
        Output6.FinishTable(SL);
      end
      else if Copy(temp, 1, 18) = 'Report.PointsTable' then
      begin
        Output6.PointsTable(SL);
      end
      else if Copy(temp, 1, 24) = 'Report.FinishReport.json' then
      begin
        JsonOut.FinishJson(SL);
      end
      else if Copy(temp, 1, 24) = 'Report.PointsReport.json' then
      begin
        JsonOut.PointsJson(SL);
      end
      else if Copy(temp, 1, 19) = 'Report.FinishReport' then
      begin
        Output6.FinishReport(SL);
      end
      else if Copy(temp, 1, 19) = 'Report.PointsReport' then
      begin
        Output6.PointsReport(SL);
      end
      else if Copy(temp, 1, 22) = 'Report.TimePointReport' then
      begin
        Output9.TimePointReport(SL);
      end
      else if Copy(temp, 1, 22) = 'Report.TW_TimePointXML' then
      begin
        Output10.TimePointXML(SL, Copy(temp, 22+2, MaxInt));
      end
      else if Copy(temp, 1, 24) = 'Report.TW_TimePointTable' then
      begin
        Output10.TimePointTable(SL, Copy(temp, 24+2, MaxInt));
      end
      else
        BO.GetReportData(SL);
    end

    { namespace ?}
    else if Copy(temp, 1, 1) = '?' then
    begin
      Output8.DispatchProt(temp);
    end

    { namespace Data }
    else
    begin
      if Copy(temp, 1, 5) = 'Data.' then
        temp := Copy(temp, 6, Length(temp));

      s1 := Copy(temp, 1, 1);
      if s1 = 'H' then
        Output1.Welcome
      else if s1 = 'A' then
        Output1.Athletes
      else if s1 = 'B' then
        Output1.Backup
      else if s1= 'E' then
        Output1.EventResult(BO.EventNode, True, True)
//    else if s1 = 'R' then
//      RaceResult(temp)
      else if s1 = 'P' then
        Output1.Properties;
    end;

    if WantPageHeader then PageFooter;
    if WantHTMEscape then EscapeHTM;

    result := SL.Text;

    Inc(BO.MsgCounter);
  end
  else
    result := 'RiggVar.Output: RequestError';
end;

function TOutput.RaceReport(AReportID, ARace, AIT: Integer): string;
begin
  SL.Clear;
  Output7.RaceReport(SL, AReportID, ARace, AIT);
  result := SL.Text;
end;

function TOutput.EventReport(AReportID, ASort: Integer;
  AFinish: Boolean): string;
begin
  SL.Clear;
  Output7.EventReport(SL, AReportID, ASort, AFinish);
  result := SL.Text;
end;

end.
