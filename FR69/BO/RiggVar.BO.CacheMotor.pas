unit RiggVar.BO.CacheMotor;

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

//{$define NoColCache}

{$ifdef NoColCache}
uses
  System.SysUtils,
  RiggVar.Conn.Intern;
{$endif}

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Classes, 
  Vcl.Graphics,
  RiggVar.BO.TemplateIDs,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.Col.Cache;

type
  TCacheMotorMock = class
  protected
    function DoRequest3(RequestString: string): string; virtual;
    function GetFinishReport: string;
    function GetPointsReport: string;
  public
    InputConnection: TConnection;
{$ifdef NoColCache}
    CacheRequestToken: string;
{$endif}
    CacheEnabled: Boolean;
    constructor Create;
    procedure DoOnIdle; virtual;
    procedure Synchronize; virtual;
    procedure SwapEvent; virtual;
    property FinishReport: string read GetFinishReport;
    property PointsReport: string read GetPointsReport;
  end;

  TCacheMotor = class(TCacheMotorMock)
  private
//    FCacheEnabled: Boolean;
    LastUpdateTime: TDateTime;
    FIdleDelay: Integer;
    FActive: Boolean;
    FStartCounter: Int64;
    FStopCounter: Int64;
    FPerfFrequence: Int64;
    FMillies: Integer;
    procedure SetIdleDelay(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    procedure StartQuery;
    function StopQuery: Integer;
    procedure DoRequest2(cr: TCacheRowCollectionItem);
//    function GetFinishReport: string;
//    function GetPointsReport: string;
    function GetTimePointReport: string;
  protected
    function DoRequest3(RequestString: string): string; override;
    procedure DoRequest(RequestString: string);
    procedure HandleParams(response: string);
  public
//    InputConnection: TConnection;
    Cache: TOutputCache;
    constructor Create;
    destructor Destroy; override;
    procedure SwapEvent; override;
    procedure DoOnIdle; override;
    procedure Synchronize; override;
    function GetEventReport(Report, Sort: Integer; ShowPoints: Boolean): string;
    function GetRaceReport(Report, Race, IT: Integer): string;
    function GetRaceXml(Race, IT: Integer): string;
    function GetXmlReport(Report: Integer): string;
    function Handle_TW_Ajax(Race, IT, Bib: Integer): string;
    function Handle_TW_TimePointXML(Race, IT, Bib: Integer): string;
    function Handle_TW_TimePointTable(Race, IT, Bib: Integer): string;
    function Handle_TW_TimePointReport(Race: Integer): string;
    function Handle_Entries(input: string): string;
    function Handle_RV(input: string): string;
    function Handle_Manage(input: string): string;
    //property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
    property IdleDelay: Integer read FIdleDelay write SetIdleDelay;
    property Active: Boolean read FActive write SetActive;
    property Millies: Integer read FMillies;
//    property FinishReport: string read GetFinishReport;
//    property PointsReport: string read GetPointsReport;
    property TimePointReport: string read GetTimePointReport;
  end;

implementation

{ TCacheMotorMock }

constructor TCacheMotorMock.Create;
begin
  CacheRequestToken := 'FR.*.Request.';
end;

function TCacheMotorMock.GetFinishReport: string;
var
  s: string;
begin
  s := Format('%sReport.FinishReport', [CacheRequestToken]);
  result := DoRequest3(s);
end;

function TCacheMotorMock.GetPointsReport: string;
var
  s: string;
begin
  s := Format('%sReport.PointsReport', [CacheRequestToken]);
  result := DoRequest3(s);
end;

procedure TCacheMotorMock.DoOnIdle;
begin

end;

procedure TCacheMotorMock.Synchronize;
begin

end;

procedure TCacheMotorMock.SwapEvent;
begin

end;

function TCacheMotorMock.DoRequest3(RequestString: string): string;
var
  answer: string;
begin
  if InputConnection <> nil then
  begin
    answer := InputConnection.HandleMsg(RequestString);
  end
  else
  begin
    answer := '<DoRequest3>no connection</DoRequest3>';
  end;
  result := answer;
end;

{ TCacheMotor }

constructor TCacheMotor.Create;
begin
  inherited Create;
  QueryPerformanceCounter(FStartCounter);
  QueryPerformanceFrequency(FPerfFrequence);
  FIdleDelay := 39;

  Cache := TOutputCache.Create;

  CacheEnabled := True;
end;

destructor TCacheMotor.Destroy;
begin
  CacheEnabled := False;

  Cache.Free;
  Cache := nil;

  inherited;
end;

procedure TCacheMotor.SwapEvent;
var
  answer: string;
  cr: TCacheRowCollectionItem;
begin
  Cache.Synchronized := False;
  if Active then
  begin
    Cache.ProcessInput('');
  end
  else if Cache.Node.OutputCacheRowCollection.Count > 0 then
  begin
    cr := Cache.Node.OutputCacheRowCollection.Items[0];
    if InputConnection <> nil then
    begin
      FMillies := 0;
      StartQuery;
      answer := InputConnection.HandleMsg(cr.Request);
      StopQuery;
      if cr.Report < 0 then //Params
        HandleParams(answer);
      cr := Cache.Node.OutputCacheRowCollection.Items[0];
      cr.Age := cr.ru.Age;
    end;
  end;
end;

procedure TCacheMotor.DoOnIdle;
var
  Hour, Min, Sec, MSec: Word;
begin
  if CacheEnabled then
  begin
    DecodeTime(Now - LastUpdateTime, Hour, Min, Sec, MSec);
    if Active and (IdleDelay > 0) and (Sec * 1000 + MSec > IdleDelay) then
    begin
      Cache.DoOnIdle;
      if Cache.Status = CacheStatus_HaveRequest then
      begin
        DoRequest(Cache.CurrentRequest);
      end;
      LastUpdateTime := Now;
    end;
  end;
end;

procedure TCacheMotor.DoRequest(RequestString: string);
var
  answer: string;
begin
  if InputConnection <> nil then
  begin
    StartQuery;
    answer := InputConnection.HandleMsg(RequestString);
    StopQuery;
    if Length(answer) > 0 then
    begin
      Cache.Status := CacheStatus_WaitingForAnswer;
    end;
  end
  else
  begin
    answer := 'no connection';
    FMillies := 0;
  end;

  if Pos('RiggVar.Params', RequestString) >= 1 then
    HandleParams(answer)
  else
    Cache.StoreAnswer(RequestString, answer, Millies);
end;

procedure TCacheMotor.DoRequest2(cr: TCacheRowCollectionItem);
var
  answer: string;
begin
  if InputConnection <> nil then
  begin
    StartQuery;
    answer := InputConnection.HandleMsg(cr.Request);
    StopQuery;
  end
  else
  begin
    answer := 'no connection';
    FMillies := 0;
  end;
  cr.StoreData(answer, Millies);
end;

function TCacheMotor.DoRequest3(RequestString: string): string;
var
  answer: string;
begin
  if InputConnection <> nil then
  begin
    StartQuery;
    answer := InputConnection.HandleMsg(RequestString);
    StopQuery;
  end
  else
  begin
    answer := '<RaceXml>no connection</RaceXml>';
    FMillies := 0;
  end;
  result := answer;
end;

procedure TCacheMotor.HandleParams(response: string);
var
  SL: TStrings;
  s: string;
  RaceCount: Integer;
  ITCount: Integer;
begin
  RaceCount := Cache.Node.RaceCount;
  ITCount := Cache.Node.ITCount;
  SL := TStringlist.Create;
  try
    SL.Text := response;

    { RaceCount }
    s := SL.Values['RaceCount'];
    if s <> '' then
      RaceCount := StrToIntDef(s, 1);

    { ITCount }
    s := SL.Values['ITCount'];
    if s <> '' then
      ITCount := StrToIntDef(s, 0);
  finally
    SL.Free;
  end;
  Cache.UpdateParams(RaceCount, ITCount);
end;

function TCacheMotor.GetEventReport(Report, Sort: Integer;
  ShowPoints: Boolean): string;
var
  cl: TCacheRowCollection;
  cr, crf: TCacheRowCollectionItem;
  i: Integer;
begin
  result := 'EventReport';
  cl := Cache.Node.OutputCacheRowCollection;
  crf := nil;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if (cr.Report = Report) and (cr.Sort = Sort) then
    begin
      if ShowPoints and (cr.Mode = 1) then
      begin
        crf := cr;
        break;
      end
      else if not ShowPoints and (cr.Mode = 0) then
      begin
          crf := cr;
        break;
      end;
    end;
  end;
  if crf <> nil then
  begin
    if crf.Age > 0 then
      DoRequest2(crf);
    crf.Hits := crf.Hits + 1;
    result := crf.ReportHeader + crf.Data;
  end;
end;

function TCacheMotor.GetTimePointReport: string;
var
  s: string;
begin
  //xml report with xsl reference for client-side execution
  s := Format('%sReport.TimePointReport', [CacheRequestToken]);
  result := DoRequest3(s);
end;

function TCacheMotor.Handle_TW_Ajax(Race, IT, Bib: Integer): string;
var
  s: string;
  t: string;
begin
  //send a single line input msg, then the request for the report,
  //so that the input msg is broadcast to listeners on output.
  t := FormatDateTime('hh:mm:ss.zz', Now);
  s := Format('FR.*.W%d.Bib%d.IT%d=%s', [Race, Bib, IT, t]);
  InputConnection.InjectMsg(s);
  result := Format('R%d.IT%d.Bib%d.Time=%s', [Race, IT, Bib, t]);
end;

function TCacheMotor.Handle_TW_TimePointXML(Race, IT, Bib: Integer): string;
var
  s: string;
  t: string;
begin
  t := FormatDateTime('hh:mm:ss.zz', Now);
  s := Format('%sReport.TW_TimePointXML.R%d.IT%d.Bib%d',
    [CacheRequestToken, Race, IT, Bib]);
  s := s + #13#10 + Format('FR.*.W%d.Bib%d.IT%d=%s'#13#10, [Race, Bib, IT, t]) + s;
  result := DoRequest3(s);
end;

function TCacheMotor.Handle_TW_TimePointTable(Race, IT, Bib: Integer): string;
var
  s: string;
  t: string;
begin
  //send a single line input msg, then the request for the report,
  //so that the input msg is broadcast to listeners on output.
  t := FormatDateTime('hh:mm:ss.zz', Now);
  s := Format('FR.*.W%d.Bib%d.IT%d=%s', [Race, Bib, IT, t]);
  InputConnection.InjectMsg(s);
  s := Format('%sReport.TW_TimePointTable.R%d', [CacheRequestToken, Race]);
  result := DoRequest3(s);
end;

//function TCacheMotor.Handle_TW_TimePointTable(Race, IT, Bib: Integer): string;
//var
//  s: string;
//  t: string;
//begin
    { this variation sends a multiline request,
      --> the timing msg is not broadcast to listeners on output. }
//  t := FormatDateTime('hh:mm:ss.zz', Now);
//  s := Format('%sReport.TW_TimePointTable.R%d', [CacheRequestToken, Race]);
//  s := s + #13#10 + Format('FR.*.W%d.Bib%d.IT%d=%s'#13#10, [Race, Bib, IT, t]) + s;
//  result := DoRequest3(s);
//end;

function TCacheMotor.Handle_TW_TimePointReport(Race: Integer): string;
var
  s: string;
begin
  s := Format('%sReport.TW_TimePointTable.R%d', [CacheRequestToken, Race]);
  result := DoRequest3(s);
end;

function TCacheMotor.Handle_Entries(input: string): string;
var
  s: string;
begin
  s := Format('%sHTM.Data.A', [CacheRequestToken]);
  if (input <> '') then
    s := s + #13#10 + input;
  result := DoRequest3(s);
end;

function TCacheMotor.Handle_RV(input: string): string;
begin
  WebLocked := True;
  if Pos('.RV', input) > 0 then
  begin
    result := DoRequest3(input);
    result := 'OK';
  end
  else
    result := 'invalid cmd';
end;

function TCacheMotor.Handle_Manage(input: string): string;
begin
  if Pos('Manage.', input) = 1 then
  begin
    result := DoRequest3(input);
    result := 'OK';
  end
  else
    result := 'invalid cmd';
end;

function TCacheMotor.GetRaceReport(Report, Race, IT: Integer): string;
var
  cl: TCacheRowCollection;
  cr: TCacheRowCollectionItem;
  i: Integer;
begin
  result := 'RaceReport';
  cl := Cache.Node.OutputCacheRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if (cr.Report = Report) and (cr.Race = Race) and (cr.IT = IT) then
    begin
      if cr.Age > 0 then
        DoRequest2(cr);
      cr.Hits := cr.Hits + 1;
      result := cr.ReportHeader + cr.Data;
      break;
    end;
  end;
end;

function TCacheMotor.GetXmlReport(Report: Integer): string;
var
  cl: TCacheRowCollection;
  cr: TCacheRowCollectionItem;
  i: Integer;
begin
  result := 'Report ' + IntToStr(Report);
  cl := Cache.Node.OutputCacheRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if (cr.Report = Report) then
    begin
      if cr.Age > 0 then
        DoRequest2(cr);
      cr.Hits := cr.Hits + 1;
      result := cr.Data;
      case Report of
        1006: ; //preformatted text
        else
        begin
          if Copy(cr.Data, 1, 5) <> '<?xml' then
            result := '<?xml version="1.0" encoding="ISO-8859-1" ?>'  + result;
        end;
      end;
      break;
    end;
  end;
end;

function TCacheMotor.GetRaceXml(Race: Integer; IT: Integer): string;
var
  s: string;
begin
  s := Format('%sRiggVar.FR.Race%d.IT%d', [CacheRequestToken, Race, IT]);
  result := DoRequest3(s);
  if Copy(result, 1, 5) <> '<?xml' then
    result := '<?xml version="1.0" encoding="ISO-8859-1" ?>'  + result;
end;

procedure TCacheMotor.Synchronize;
begin
  Cache.Synchronized := False;
  Cache.Node.Age := Cache.Node.Age + 1;
end;

procedure TCacheMotor.SetIdleDelay(const Value: Integer);
begin
  if Value > 9 then
    FIdleDelay := Value
  else
    FIdleDelay := 0;
end;

procedure TCacheMotor.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TCacheMotor.StartQuery;
begin
  QueryPerformanceCounter(FStartCounter);
end;

function TCacheMotor.StopQuery: Integer;
begin
  QueryPerformanceCounter(FStopCounter);
  FMillies := Round(((FStopCounter - FStartCounter) / FPerfFrequence) * 1000);
  result := FMillies;
end;

end.
