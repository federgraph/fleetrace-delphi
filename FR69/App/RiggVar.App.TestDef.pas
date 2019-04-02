unit RiggVar.App.TestDef;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.Conn.Intern;

type
  TestOp = (
    TestCounterShow,
    TestCounterClear,

    SendManageClear,
    SendManageClearViaUndo,
    SendEPName,

    SendGER,
    SendITA,
    SendDSQ,
    SendOK,

    SendTime,
    SendTimeRequest,
    SendRV,
    SendRVRequest,

    ShowStartupLog,
    ShowStatus,
    ShowSource,
    ShowNormal,
    ShowCompact,
    ShowTxt,
    ShowXml,
    ShowJson,
    ShowHtml,
    ShowIni,
    ShowConfig,
    ShowDefaultIni,
    ShowHash,
    ShowCheck,
    ShowTimeTable,
    ShowTestData,
    LoadTestData,
    ClearEvent,
    ClearRace,
    ClearIT,
    ResetRace,
    GoBackToRaceOne,
    GoBackToRaceTwo,

    RequestFinishReportJson,
    RequestRaceReport,

    NextR,
    NextE
  );

  TTestDef = class
  private
    msg: string;
    RL: TStringList;
    TestConnection: TConnection;
    procedure ShowRL(ML: TStrings);
    procedure ShowAnswerPlusCounter;
    procedure ShowAnswer;
  protected
    procedure InitInputConnection;
    procedure InitOutputConnection;
    procedure DisposeConnection;
  public
    { ML will be injected, not owned }
    ML: TStrings; //Memo.Lines
    TL: TStrings; //TestMemo.Lines

    constructor Create;
    destructor Destroy; override;

    function GetItemCaption(op: TestOp): string;
    procedure DoTest(op: TestOp);
    procedure DoOp(op: TestOp);
  end;

  TTestList = class
  private
    procedure ClearList;
    procedure Add(op: TestOp);
  public
    OpList: array of TestOp;
    OpCount: Integer;

    { injected dependencies, not owned }
    TestDef: TTestDef; //see above
    LI: TStrings; //Listbox.Items
    ML: TStrings; //Memo.Lines
    TL: TStrings; //TestMemo.Lines

    constructor Create;

    procedure InitMemo;

    { filter the List }
    procedure InitAll;
    procedure InitData;
    procedure InitDefault;
    procedure InitIni;
    procedure InitReport;
    procedure InitStatus;
    procedure InitTest;

    procedure DoOp(op: TestOp);
    procedure DoTest(op: TestOp);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Def,
  RiggVar.Util.Classes;

{ TTestDef }

constructor TTestDef.Create;
begin
  RL := TStringList.Create;
  //ML := TStringList.Create; //injected
end;

destructor TTestDef.Destroy;
begin
  //ML.Free; //not owned
  RL.Free;
  inherited;
end;

procedure TTestDef.InitInputConnection;
begin
  TestConnection := BO.InputServer.Server.Connect('TestForm.Input');
end;

procedure TTestDef.InitOutputConnection;
begin
  TestConnection := BO.OutputServer.Server.Connect('TestForm.Output');
end;

procedure TTestDef.DisposeConnection;
begin
  TestConnection.Free;
  TestConnection := nil;
end;

procedure TTestDef.DoTest(op: TestOp);
begin

  case op of
    TestCounterShow:
    begin
      TL.Text := 'TestCounter.Show';
    end;

    TestCounterClear:
    begin
      TL.Text := 'TestCounter.Clear';
    end;

    SendManageClear:
    begin
      TL.Text := 'Manage.Clear via new internal connection';
    end;

    SendManageClearViaUndo:
    begin
      TL.Text := 'Manage.Clear via existing internal connection';
    end;

    SendEPName:
    begin
      TL.Text := 'set EP.Name to ''Test''';
    end;

    SendGER:
    begin
      TL.Text := 'set NOC to ''GER'' for SNR 1001 in Race 1';
    end;

    SendITA:
    begin
      TL.Text := 'set NOC to ''ITA'' for Bib 1 in Race 1';
    end;

    SendDSQ:
    begin
      TL.Text := 'set status DSQ for Bib 1 in Race 1';
    end;

    SendOK:
    begin
      TL.Text := 'set status OK for Bib 1 in Race 1';
    end;

    ShowNormal:
    begin
      TL.Text := 'Show Normal Text';
    end;

    ShowCompact:
    begin
      TL.Text := 'Show Compact Text';
    end;

    ShowTxt:
    begin
      TL.Text := 'Show Txt';
      TL.Add(Format('(EP.UseCompactFormat is %s)', [BoolStr[BO.UseCompactFormat]]));
    end;

    ShowXml:
    begin
      TL.Text := 'Show Xml';
    end;

    ShowJson:
    begin
      TL.Text := 'Show Json';
    end;

    ShowHtml:
    begin
      TL.Text := 'Show Html';
    end;

    ShowStartupLog:
    begin
      TL.Text := 'Show Startup Logger Content';
    end;

    ShowStatus:
    begin
      TL.Text := 'Show Status (Main.GetStatusString)';
    end;

    ShowSource:
    begin
      TL.Text := 'Show Source (BO.ConvertedData)';
    end;

    ShowIni:
    begin
      TL.Text := 'Show Ini (Main.IniImage.ToString)';
    end;

    ShowDefaultIni:
    begin
      TL.Text := 'Show Default Ini (Main.IniImage.DefaultText)';
    end;

    ShowConfig:
    begin
      TL.Text := 'Show Config (Main.IniImage.ConfigText)';
      TL.Add(Main.FolderInfo.ConfigFileName);
    end;

    ShowHash:
    begin
      TL.Text := 'Show Hash (BO.ResultHash.MemoString)';
    end;

    ShowCheck:
    begin
      TL.Text := 'Show Check (BO.CalcEV.Proxy.CheckList.Text)';
      ML.Text := BO.CalcEV.Proxy.CheckList.Text;
    end;

    ShowTimeTable:
    begin
{$ifdef RoundingsOnly}
      TL.Text := Format('Show Time Table (BO.TimeTable[%d])', [BO.Race]);
{$endif}
    end;

    ShowTestData:
    begin
      TL.Text := 'Show TestData (Main.TestData)';
    end;

    LoadTestData:
    begin
      TL.Text := 'load test data (Main.GuiManager.SwapEvent(Main.TestData))';
    end;

    ClearEvent:
    begin
      TL.Text := 'Clear Event (BO.ClearCommand)';
    end;

    ClearRace:
    begin
      TL.Text := 'Clear Race (from Event and Race)';
      TL.Add('BO.ClearRaceCommand(BO.Race)');
    end;

    ResetRace:
    begin
      TL.Text := 'Reset Race (Event Only)';
      TL.Add('BO.EventNode.EventRowCollection.ResetRace(BO.Race);');
    end;

    ClearIT:
    begin
      TL.Text := 'Clear IT';
      TL.Text := '(BO.ClearTimepointCommand)';
    end;

    GoBackToRaceOne:
    begin
      TL.Text := 'Go back to race one';
    end;

    GoBackToRaceTwo:
    begin
      TL.Text := 'Go back to race two';
    end;

    SendTime:
    begin
      TL.Text := 'send finish time for bib 1 in race 1';
      TL.Add('FR.*.W1.Bib1.IT0=10:58:59.12');
    end;

    SendTimeRequest:
    begin
      TL.Text := 'send time with request';
      TL.Add('FR.*.Request.HTM.Web.Race.Report5.R1.IT0');
      TL.Add('FR.*.W1.Bib1.IT0=10:58:59.23');
    end;

    SendRV:
    begin
      TL.Text := 'send race value for bib 1 in race 1';
      TL.Add('this is a finish position');
      TL.Add('FR.*.W1.Bib1.RV=500');
    end;

    SendRVRequest:
    begin
      TL.Text := 'send race value with request';
      TL.Add('FR.*.Request.Report.FinishReport.json');
      TL.Add('FR.*.W1.Bib1.RV=3');
    end;

    RequestFinishReportJson:
    begin
      TL.Text := 'FR.*.Output.Report.FinishReport.json';
    end;

    RequestRaceReport:
    begin
      TL.Text := 'FR.*.Output.HTM.Web.Race.Report5.R1.IT0';
    end;

    NextE:
    begin
      TL.Text := 'Next E: result of BO.FindCurrentInEvent';
    end;

    NextR:
    begin
      TL.Text := 'Next R: result of BO.FindCurrentInRace';
    end

    else
    begin
      TL.Text := 'Noop (default case)';
    end;
  end;
end;

procedure TTestDef.DoOp(op: TestOp);
begin
  ML.Clear;

  case op of
    TestCounterShow:
    begin
      TL.Text := 'TestCounter.Show';
      Main.TestCounter.Show(ML);
    end;

    TestCounterClear:
    begin
      TL.Text := 'TestCounter.Clear';
      Main.TestCounter.Clear;
      Main.TestCounter.Show(ML);
    end;

    SendManageClear:
    begin
      TL.Text := 'Manage.Clear via new internal connection';

      Main.TestCounter.Clear;
      InitInputConnection;
      TestConnection.HandleMsg('Manage.Clear');
      Main.GuiManager.GuiInterface.HandleInform(ScheduleEventUpdate);
      DisposeConnection;

      Main.TestCounter.Show(ML);
    end;

    SendManageClearViaUndo:
    begin
      TL.Text := 'Manage.Clear via existing internal connection';

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('Manage.Clear');
      //BO.Inform(ScheduleEventUpdate);

      Main.TestCounter.Show(ML);

      { see api/fr-manage-clear in RiggVar.BO.Web2.Router }
    end;

    SendEPName:
    begin
      TL.Text := 'set EP.Name to ''Test''';

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('EP.Name = Test');

      Main.TestCounter.Show(ML);
    end;

    SendGER:
    begin
      TL.Text := 'set NOC to ''GER'' for SNR 1001 in Race 1';

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('FR.*.SNR1001.NC=GER');
      Main.TestCounter.Show(ML);
    end;

    SendITA:
    begin
      TL.Text := 'set NOC to ''ITA'' for Bib 1 in Race 1';

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('FR.*.SNR1001.NC=ITA');
      Main.TestCounter.Show(ML);
    end;

    SendDSQ:
    begin
      TL.Text := 'set status DSQ for Bib 1 in Race 1';

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('FR.*.W1.Bib1.QU=DSQ');
      Main.TestCounter.Show(ML);
    end;

    SendOK:
    begin
      TL.Text := 'set status OK for Bib 1 in Race 1';

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('FR.*.W1.Bib1.QU=OK');
      Main.TestCounter.Show(ML);
    end;

    ShowNormal:
    begin
      TL.Text := 'Show Normal Text';
      ML.Text := BO.ToNormalTXT;
    end;

    ShowCompact:
    begin
      TL.Text := 'Show Compact Text';
      ML.Text := BO.ToCompactTXT;
    end;

    ShowTxt:
    begin
      TL.Text := 'Show Txt';
      TL.Add(Format('(EP.UseCompactFormat is %s)', [BoolStr[BO.UseCompactFormat]]));

      ML.Text := BO.ToTxt;
    end;

    ShowXml:
    begin
      TL.Text := 'Show Xml';
      ML.Text := BO.ToXML;
    end;

    ShowJson:
    begin
      TL.Text := 'Show Json';
      ML.Text := BO.ToJson;
    end;

    ShowHtml:
    begin
      TL.Text := 'Show Html';
      ML.Text := BO.ToHtml;
    end;

    ShowStartupLog:
    begin
      TL.Text := 'Show Startup Logger Content';
      ML.Text := Main.StartupLogger.Text;
    end;

    ShowStatus:
    begin
      TL.Text := 'Show Status (Main.GetStatusString)';

      ML.Text := Main.GetStatusString;
    end;

    ShowSource:
    begin
      TL.Text := 'Show Source (BO.ConvertedData)';
      ML.Text := BO.ConvertedData;
    end;

    ShowIni:
    begin
      TL.Text := 'Show Ini (Main.IniImage.ToString)';
      ML.Text := Main.IniImage.ToString;
    end;

    ShowDefaultIni:
    begin
      TL.Text := 'Show Default Ini (Main.IniImage.DefaultText)';
      ML.Text := Main.IniImage.DefaultText;
    end;

    ShowConfig:
    begin
      TL.Text := 'Show Config (Main.IniImage.ConfigText)';
      TL.Add(Main.FolderInfo.ConfigFileName);
      ML.Text := Main.IniImage.ConfigText;
    end;

    ShowHash:
    begin
      TL.Text := 'Show Hash (BO.ResultHash.MemoString)';
      ML.Text := BO.ResultHash.MemoString;
    end;

    ShowCheck:
    begin
      TL.Text := 'Show Check (BO.CalcEV.Proxy.CheckList.Text)';
      ML.Text := BO.CalcEV.Proxy.CheckList.Text;
    end;

    ShowTimeTable:
    begin
{$ifdef RoundingsOnly}
      TL.Text := Format('Show Time Table (BO.TimeTable[%d])', [BO.Race]);
      ML.Text := BO.TimeTable[BO.Race];
{$endif}
    end;

    ShowTestData:
    begin
      TL.Text := 'Show TestData (Main.TestData)';
      ML.Text := Main.TestData;
    end;

    LoadTestData:
    begin
      TL.Text := 'load test data (Main.GuiManager.SwapEvent(Main.TestData))';

      Main.TestCounter.Clear;
      Main.GuiManager.SwapEvent(Main.TestData);
      Main.TestCounter.Show(ML);
    end;

    ClearEvent:
    begin
      TL.Text := 'Clear Event (BO.ClearCommand)';
      BO.ClearCommand;
    end;

    ClearRace:
    begin
      TL.Text := 'Clear Race (from Event and Race)';
      TL.Add('BO.ClearRaceCommand(BO.Race)');
      BO.ClearRaceCommand(BO.Race);
    end;

    ResetRace:
    begin
      TL.Text := 'Reset Race (Event Only)';
      TL.Add('BO.EventNode.EventRowCollection.ResetRace(BO.Race);');
      BO.EventNode.EventRowCollection.ResetRace(BO.Race);
    end;

    ClearIT:
    begin
      TL.Text := 'Clear IT';
      TL.Text := '(BO.ClearTimepointCommand)';
      BO.ClearTimepointCommand(BO.Race, BO.IT);
    end;

    GoBackToRaceOne:
    begin
      TL.Text := 'Go back to race one';
      BO.EventNode.GoBackToRace(1);
      BO.EventNode.Modified := True;
      BO.Calc;
      { procedure TFormFR62.GoBackToRaceItemClick(Sender: TObject); }
    end;

    GoBackToRaceTwo:
    begin
      TL.Text := 'Go back to race two';
      BO.EventNode.GoBackToRace(2);
    end;

    SendTime:
    begin
      TL.Text := 'send finish time for bib 1 in race 1';
      TL.Add('FR.*.W1.Bib1.IT0=10:58:59.12');

      Main.TestCounter.Clear;
      BO.UndoConnection.HandleMsg('FR.*.W1.Bib1.IT0=10:58:59.12');
      Main.TestCounter.Show(ML);
    end;

    SendTimeRequest:
    begin
      RL.Clear;
      RL.Add('FR.*.Request.HTM.Web.Race.Report5.R1.IT0');
      RL.Add('FR.*.W1.Bib1.IT0=10:58:59.23');

      TL.Text := 'send time with request';
      TL.Add(RL[0]);
      TL.Add(RL[1]);

      Main.TestCounter.Clear;
      RL.Text := BO.UndoConnection.HandleMsg(RL.Text);
      ShowAnswerPlusCounter;
    end;

    SendRV:
    begin
      TL.Text := 'send race value for bib 1 in race 1';
      TL.Add('this is a finish position');
      TL.Add('FR.*.W1.Bib1.RV=500');

      Main.TestCounter.Clear;
      InitInputConnection;
      TestConnection.HandleMsg('FR.*.W1.Bib1.RV=500');
      DisposeConnection;

      Main.TestCounter.Show(ML);
    end;

    SendRVRequest:
    begin
      RL.Clear;
      RL.Add('FR.*.Request.Report.FinishReport.json');
      RL.Add('FR.*.W1.Bib1.RV=3');

      TL.Text := 'send race value with request';
      TL.Add(RL[0]);
      TL.Add(RL[1]);

      Main.TestCounter.Clear;
      RL.Text := BO.UndoConnection.HandleMsg(RL.Text);

      ShowAnswerPlusCounter;
    end;

    RequestFinishReportJson:
    begin
      msg := 'FR.*.Output.Report.FinishReport.json';

      TL.Text := msg;

      RL.Text := BO.Output.GetMsg(msg);

      ShowAnswer;
    end;

    RequestRaceReport:
    begin
      msg := 'FR.*.Output.HTM.Web.Race.Report5.R1.IT0';

      TL.Text := msg;

      RL.Text := BO.Output.GetMsg(msg);

      ShowAnswer;
    end;

    NextE:
    begin
      msg := 'Next E: result of BO.FindCurrentInEvent';

      TL.Text := msg;

      ML.Clear;
      BO.FindCurrentInEvent(BO.CurrentNumbers);
      BO.CurrentNumbers.Fill(ML);
    end;

    NextR:
    begin
      msg := 'Next R: result of BO.FindCurrentInRace';

      TL.Text := msg;

      ML.Clear;
      BO.FindCurrentInRace(BO.CurrentNumbers);
      BO.CurrentNumbers.Fill(ML);
    end

    else
    begin
      TL.Text := 'Noop (default case)';
      ML.Text := '';
    end;
  end;
end;

procedure TTestDef.ShowRL(ML: TStrings);
var
  i: Integer;
begin
  for i := 0 to RL.Count-1 do
    ML.Add(RL[i]);
end;

procedure TTestDef.ShowAnswer;
begin
  ML.Clear;
  ML.BeginUpdate;
  ShowRL(ML);
  ML.EndUpdate;
  RL.Clear;
end;

procedure TTestDef.ShowAnswerPlusCounter;
begin
  ML.BeginUpdate;
  Main.TestCounter.Show(ML);
  ML.Add('');
  ShowRL(ML);
  ML.EndUpdate;
  RL.Clear;
end;

function TTestDef.GetItemCaption(op: TestOp): string;
begin
  case op of
    TestCounterShow: result := 'call TestCounter.Show';
    TestCounterClear: result := 'call TestCounter.Clear';

    SendManageClear: result := 'send Manage.Clear';
    SendManageClearViaUndo: result := 'send Manager.Clear (via Undo)';
    SendEPName: result := 'send EP.Name msg (Test)';
    SendGER: result := 'send Name (GER)';
    SendITA: result := 'send Name (ITA)';
    SendDSQ: result := 'send Status (DSQ)';
    SendOK: result := 'send Status (OK)';

    SendTime: result := 'send time (W1.Bib1.IT0)';
    SendTimeRequest: result := 'send time + request';
    SendRV: result := 'send race value (W1.Bib1.RV)';
    SendRVRequest: result := 'send race value + request';

    ShowStartupLog: result := 'show startup log';
    ShowStatus: result := 'show status';
    ShowSource: result := 'show source';

    ShowNormal: result := 'show normal text';
    ShowCompact: result := 'show compact text';

    ShowTxt: result := 'show text';
    ShowXml: result := 'show xml';
    ShowJson: result := 'show json';
    ShowHtml: result := 'show html';

    ShowIni: result := 'show ini image';
    ShowConfig: result := 'show actual ini';
    ShowDefaultIni: result := 'show default ini';

    ShowHash: result := 'show hash';
    ShowCheck: result := 'show check';

    ShowTimeTable: result := 'show time table';

    ShowTestData: result := 'show test data';
    LoadTestData: result := 'load test data';
    ClearEvent: result := 'clear event';
    ClearRace: result := 'clear race';
    ClearIT: result := 'clear timepoint';
    ResetRace: result := 'reset race';
    GoBackToRaceOne: result := 'go back to race one';
    GoBackToRaceTwo: result := 'go back to race two';

    RequestFinishReportJson : result := 'request FinishReport.json';
    RequestRaceReport : result := 'request Race report';

    NextE: result := 'show Next E';
    NextR: result := 'show Next R';

    else result := '-- Missing Caption --';
  end;
end;

{ TTestList }

constructor TTestList.Create;
var
  c: Integer;
begin
  c := Integer(High(TestOp));
  SetLength(OpList, c + 1 );
end;

procedure TTestList.Add(op: TestOp);
var
  s: string;
begin
  s := TestDef.GetItemCaption(op);
  Inc(OpCount);
  if (OpCount > 0) and (OpCount <= Length(OpList)) then
  begin
    OpList[OpCount-1] := op;
    LI.Add(s);
  end;
end;

procedure TTestList.ClearList;
begin
  ML.Clear;
  LI.Clear;
  OpCount := 0;
end;

procedure TTestList.InitAll;
var
  op: TestOp;
  s: string;
begin
  ClearList;
  op := High(TestOp);
  s := TestDef.GetItemCaption(op);
  for op := Low(TestOp) to High(TestOp) do
    Add(op);
end;

procedure TTestList.InitMemo;
begin
  ML.Text := 'usage:';
  ML.Add('  select item in listbox (left)');
  ML.Add('  then press space-bar or return');
end;

procedure TTestList.InitIni;
begin
  ClearList;

  Add(ShowStartupLog);
  Add(ShowStatus);
  Add(ShowIni);
  Add(ShowConfig);
  Add(ShowDefaultIni);
end;

procedure TTestList.InitData;
begin
  ClearList;

  Add(ShowTestData);
  Add(LoadTestData);
  Add(ResetRace);
  Add(ClearEvent);
  Add(ClearRace);
  Add(ClearIT);
  Add(GoBackToRaceOne);
  Add(GoBackToRaceTwo);
end;

procedure TTestList.InitTest;
begin
  ClearList;

  Add(TestCounterShow);
  Add(TestCounterClear);

  Add(SendManageClear);

  Add(SendEPName);

  Add(SendGER);
  Add(SendITA);

  Add(SendDSQ);
  Add(SendOK);

  Add(SendTime);
  Add(SendTimeRequest);
  Add(SendRV);
  Add(SendRVRequest);
end;

procedure TTestList.InitStatus;
begin
  ClearList;

  Add(ShowSource);

  Add(ShowNormal);
  Add(ShowCompact);

  Add(ShowTxt);
  Add(ShowXml);
  Add(ShowJson);
  Add(ShowHtml);

  Add(NextR);
  Add(NextE);
end;

procedure TTestList.InitDefault;
begin
  ClearList;

  Add(SendManageClear);
  Add(LoadTestData);

  Add(ShowNormal);
  Add(ShowCompact);

  Add(ShowTxt);
  Add(ShowXml);
  Add(ShowJson);
  Add(ShowHtml);

  Add(ShowIni);
  Add(ShowConfig);
  Add(ShowDefaultIni);
end;

procedure TTestList.InitReport;
begin
  ClearList;

  Add(RequestFinishReportJson);
  Add(RequestRaceReport);
end;

procedure TTestList.DoOp(op: TestOp);
begin
  TestDef.DoOp(op);
  ML.Text := TestDef.ML.Text;
end;

procedure TTestList.DoTest(op: TestOp);
begin
  TestDef.DoTest(op);
  TL.Text := TestDef.TL.Text;
end;

end.
