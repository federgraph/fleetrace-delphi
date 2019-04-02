unit RiggVar.BO.Def;

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
  RiggVar.App.Config,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Base,
  RiggVar.BO.EventProps,
  RiggVar.BO.ExcelExport,
  RiggVar.BO.ExcelImport,
  RiggVar.BO.IniImage,
  RiggVar.BO.MsgBase,
  RiggVar.BO.MsgParser,
  RiggVar.BO.MsgToken,
  RiggVar.BO.MsgTree,
  RiggVar.BO.NodeList,
  RiggVar.BO.Params,
  RiggVar.BO.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.BO.ResultHash,
  RiggVar.BO.Time,
  RiggVar.BO.UndoManager,
  RiggVar.BO.Watches,
  RiggVar.Calc.EV,
  RiggVar.Calc.TP,
  RiggVar.Col.Captions,
  RiggVar.Col.Event,
  RiggVar.Col.Listing,
  RiggVar.Col.Race,
  RiggVar.Col.Roundings,
  RiggVar.Col.Stammdaten,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.Conn.IO,
  RiggVar.Conn.StatusFeedback,
  RiggVar.Grid.ColGrid,
  RiggVar.Out.FR00,
  RiggVar.Out.JS00,
  RiggVar.Out.RD00,
  RiggVar.Scoring.Penalty,
  RiggVar.Util.Classes,
  RiggVar.Util.Props;

type
  TCurrentNumbers = class
  private
    withTime: Integer;
    withPenalty: Integer;
    withTimeOrPenalty: Integer;
  public
    race: Integer;
    tp: Integer;
    bib: Integer;
    procedure Clear;
    procedure Fill(ML: TStrings);
  end;

  TBO = class(TBaseBO)
  private
    FModified: Boolean;
    FNodeList: TBaseNodeList;
    FSLBackup: TStrings;
    FLoading: Boolean;
    FGezeitet: Integer;
    FLocalWatches: TLocalWatches;
{$ifdef RoundingsOnly}
    FRaceData: array of TRaceData;
    FCurrentRace: Integer;
{$endif}
    procedure SaveLine(Sender: TObject; s: string);
    procedure ClearList(rd: string);
    procedure ClearResult(rd: string);
    procedure CalcEvent;
    procedure CalcNodes;
    procedure CalcNode(bn: TBaseNode);
    procedure InitStartlistCount(newCount: Integer);
    //
    function GetBib(Index: Integer): Integer;
    function GetSNR(Index: Integer): Integer;
    function GetQU(RaceIndex, Index: Integer): Integer;
    function GetDG(RaceIndex, Index: Integer): Integer;
    function GetOT(RaceIndex, Index: Integer): Integer;
    //
    procedure SetBib(Index: Integer; const Value: Integer);
    procedure SetSNR(Index: Integer; const Value: Integer);
    procedure SetQU(RaceIndex, Index: Integer; const Value: Integer);
    procedure SetDG(RaceIndex, Index: Integer; const Value: Integer);
    procedure SetOT(RaceIndex, Index: Integer; const Value: Integer);
    //
    function GetPenalty(RaceIndex, Index: Integer): TPenalty;
    procedure SetPenalty(RaceIndex, Index: Integer; const Value: TPenalty);
    //
    function GetGemeldet: Integer;
    function GetGesegelt: Integer;
    procedure SetGezeitet(const Value: Integer);
    function GetLocalWatches: TLocalWatches;
    function GetIsRacing(Index: Integer): Boolean;
    procedure SetIsRacing(Index: Integer; const Value: Boolean);
    function GetIT: Integer;
    function GetRace: Integer;
    function GetRaceNode: TRaceNode;
    function GetData(Index: Integer): string;
    procedure SetData(Index: Integer; const Value: string);
{$ifdef RoundingsOnly}
    procedure SetRace(const Value: Integer);
    function GetTimeTable(Index: Integer): string;
    procedure SetTimeTable(Index: Integer; const Value: string);
    procedure LoadTimeTable(const Data: string);
{$endif}
  protected
    procedure BackupPenaltiesRE(SL: TStrings; n: Integer);
    procedure BackupPenaltiesEV(SL: TStrings; n: Integer);
    function GetWatches: TAdapterWatches; override;
    procedure SetModified(Sender: TObject);
  public
    UseOutputFilter: Boolean;
    UseCompactFormat: Boolean;
    ConvertedData: string;
    EventHash: string;
    ResultHash: TResultHashBase;

    BOParams: TBOParams; //downcast of inherited field AdapterParams
    //
    CounterCalc: Integer;
    MsgCounter: Integer;
    //
    EventBO: TEventBO;
    EventNode: TEventNode;
    //
    RaceBO: TRaceBO;
    RNode: array of TRaceNode;
    //
    RoundingsBO: TRoundingsBO;
    RoundingsNode: TRoundingsNode;
    //
    ListingBO: TListingBO;
    ListingNode: TListingNode;
    //
    StammdatenBO: TStammdatenBO;
    StammdatenNode: TStammdatenNode;
    //
    CalcEV: TCalcEvent;
    CalcTP: TCalcTP;
    //
    JavaScoreXML: TJavaScoreXML;
    RaceDataXML: TRaceDataXML;
    EventProps: TEventProps;
    StatusFeedback: TStatusFeedback;
    //
    UndoManager: TUndoManager;
    UndoConnection: TConnection;
    PenaltyService: TPenalty;
    ExcelImporter: TExcelImporter;
    TableExporter: TExcelExporter;

    UndoAgent: TUndoAgent;
    MsgTree: TMsgTree;

    JsonT: TJsonType;
    Data0: string;

    FData1: string;
    FData2: string;
    FData3: string;

    CurrentNumbers: TCurrentNumbers;

    constructor Create(Params: TBOParams);
    destructor Destroy; override;
    procedure Init;
    procedure Inform(ga: TGuiAction);
    //
    function GetHash: string;
    function ToString: string; override;
    function ToSimpleTXT: string;
    function ToNormalTXT: string;
    function ToCompactTXT: string;
    function ToTXT: string;
    function ToXML: string;
    function ToJson: string;
    function ToHtml: string;
    //
    function GetRunIsRacing(RunID: string): Boolean;
    procedure SetRunIsRacing(RunID: string; const Value: Boolean);
    function FindRaceIndex(roName: string): Integer;
    function FindNode(roName: string): TRaceNode;
    function UpdateStartlistCount(roName: string; newCount: Integer): Boolean;
    function UpdateAthlete(SNR: Integer; Cmd, Value: string): Boolean;
    procedure UpdateRace;
    //
    function Save(XML: Boolean = false): string;
    procedure LoadPartial(ML: TStrings);
    procedure Load_(const Data: string; ML: TStrings);
    procedure Load(const Data: string);
    procedure LoadNew(const Data: string); virtual;
    procedure Clear; override;

    procedure Backup;
    procedure BackupPenalties(SL: TStrings; n: Integer);
    procedure BackupAthletes(Model: TMsgTree);
    procedure BackupAthletesHack(Model: TMsgTree);
    procedure BackupToText(SL: TStrings);
    procedure BackupToXML(SL: TStrings);
    procedure BackupToJson(SL: TStrings);
    procedure BackupToSimpleText(SL: TStrings);
    procedure BackupToSimpleJson(ML: TStrings);

    procedure Restore;

    procedure GetReportDataRace(SL: TStrings; j: Integer);
    procedure GetReportData(SL: TStrings);
    procedure GetSeriesDataSQL(SL: TStrings);
    procedure GetRaceDataSQL(SL: TStrings);

    procedure ClearBtnClick;
    //
    function Calc: Boolean; override;
    function NewMsg: TBaseMsg; override;
    procedure OnIdle;
    //
    procedure EditDG(raceIndex, crIndex: Integer; const Value: string);
    procedure EditOTime(raceIndex, crIndex: Integer; const Value: string);
    procedure EditQU(raceIndex, crIndex: Integer; const Value: string);
    //
    procedure ClearCommand;
    procedure ClearRaceCommand(r: Integer);
    procedure ClearTimepointCommand(r: Integer; tp: Integer);
    procedure GoBackToRaceCommand(r: Integer);
    procedure ResetRaceCommand(r: Integer);
    //
    procedure LoadTestStartList;
    //
    procedure CopyFromRaceNode(ru: TRaceNode; MRank: Boolean);
    procedure CopyToRaceNode(ru: TRaceNode);
    procedure CopyOTimes(RaceIndex: Integer);
    procedure InitEventNode;
    procedure UpdateEventNode;
    procedure UpdateRaceNodes;
    procedure RebuildEventNode;
    //
    function FindCurrentInEvent(AResult: TCurrentNumbers): TCurrentNumbers;
    function FindCurrentInRace(AResult: TCurrentNumbers): TCurrentNumbers;
    procedure DoTimingEvent(ARace, AIT, ABib, AOption: Integer);
    //
    property Gesegelt: Integer read GetGesegelt;
    property Gemeldet: Integer read GetGemeldet;
    property Gezeitet: Integer read FGezeitet write SetGezeitet;
    //
    property SNR[Index: Integer]: Integer read GetSNR write SetSNR;
    property Bib[Index: Integer]: Integer read GetBib write SetBib;
    property QU[RaceIndex, Index: Integer]: Integer read GetQU write SetQU;
    property Penalty[RaceIndex, Index: Integer]: TPenalty read GetPenalty write SetPenalty;
    property DG[RaceIndex, Index: Integer]: Integer read GetDG write SetDG;
    property OT[RaceIndex, Index: Integer]: Integer read GetOT write SetOT;
    property Loading: Boolean read FLoading;
    property LocalWatches: TLocalWatches read GetLocalWatches;
    property IsRacing[Index: Integer]: Boolean read GetIsRacing write SetIsRacing;
    property RaceNode: TRaceNode read GetRaceNode;
    property Race: Integer read GetRace;
    property IT: Integer read GetIT;
{$ifdef RoundingsOnly}
    property CurrentRace: Integer read FCurrentRace;
    property TimeTable[Index: Integer]: string read GetTimeTable write SetTimeTable;
{$endif}
    property Data[Index: Integer]: string read GetData write SetData;
  end;

var
  BO: TBO;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Msg,
  RiggVar.BO.Writer,
  RiggVar.BO.WriterJson,
  RiggVar.DAL.Redirector,
  RiggVar.Util.Sound;

{ TBO }

constructor TBO.Create(Params: TBOParams);
begin
  inherited Create;
  BOParams := Params;
end;

procedure TBO.Init;
var
  i: Integer;
  ru: TRaceNode;
  ts: TServerIntern;
begin
  BO := self;
  InitDefaultColCaptions;

  SetDivisionName(BOParams.DivisionName);

  PenaltyService := TPenaltyISAF.Create;
  CurrentNumbers := TCurrentNumbers.Create;

  ExcelImporter := TExcelImporter.Create;
  TableExporter := TExcelExporter.Create;

  CalcEV := TCalcEvent.Create(Main.IniImage.ScoringProvider);
  CalcTP := TCalcTP.Create;

  FNodeList := TBaseNodeList.Create;
  FNodeList.OnCalc := SetModified;

  UndoAgent := TUndoAgent.Create;
  MsgTree := TMsgTree.Create(nil, cTokenA, DynamicActionID);

  { Stammdaten }
  StammdatenBO := TStammdatenBO.Create;
  StammdatenNode := TStammdatenNode.Create(StammdatenBO);
  StammdatenBO.CurrentNode := StammdatenNode;

  { EventNode }
  EventBO := TEventBO.Create;
  EventBO.WantDiffCols := false;
  EventNode := TEventNode.Create(EventBO);
  EventNode.NameID := 'E';
  EventNode.StammdatenRowCollection := StammdatenNode.StammdatenRowCollection;
  EventBO.CurrentNode := EventNode;
  FNodeList.Add(EventNode);

  { Race }
  RaceBO := TRaceBO.Create;
  SetLength(RNode, BOParams.RaceCount + 1);
  for i := 0 to BOParams.RaceCount do
  begin
    ru := TRaceNode.Create(RaceBO);
    ru.BOParams := BOParams;
    ru.StammdatenRowCollection := StammdatenNode.StammdatenRowCollection;
    ru.Index := i;
    ru.NameID := 'W' + IntToStr(i);
    ru.Layout := 1;
    FNodeList.Add(ru);
    RNode[i] := ru;
  end;
  RaceBO.CurrentNode := RNode[1];

  { RaceData }
{$ifdef RoundingsOnly}
  SetLength(FRaceData, BOParams.RaceCount + 1);
  for i := 0 to BOParams.RaceCount do
    FRaceData[i].IsRacing := True;
{$endif}

  { Roundings }
  RoundingsBO := TRoundingsBO.Create;
  RoundingsNode := TRoundingsNode.Create(RoundingsBO);
  RoundingsNode.NameID := 'Q';
  RoundingsBO.CurrentNode := RoundingsNode;

  { Listing }
  ListingBO := TListingBO.Create;
  ListingNode := TListingNode.Create(RoundingsBO);
  ListingNode.NameID := 'L';
  ListingBO.CurrentNode := ListingNode;

  InitStartlistCount(BOParams.StartlistCount);

  Output := TOutput.Create;

  try
    ts := TServerIntern.Create(3027, Function_Input);
    InputServer := TInputNCP.Create(ts);
    ts := TServerIntern.Create(3028, Function_Output);
    OutputServer := TOutputNCP.Create(ts);
  except
    InputServer.Free;
    InputServer := nil;
    OutputServer.Free;
    OutputServer := nil;
  end;

  EventProps := TEventProps.Create;
  JavaScoreXML := TJavaScoreXML.Create;
  RaceDataXML := TRaceDataXML.Create;
  StatusFeedback := TStatusFeedback.Create;
  ResultHash := TResultHash01.Create;

  UndoManager := TUndoManager.Create;
  UndoConnection := InputServer.Server.Connect('Undo.Input');
  FLocalWatches := TLocalWatches.Create;

  UseCompactFormat := True; //temporary default
end;

destructor TBO.Destroy;
var
  i: Integer;
begin
  ResultHash.Free;
  StatusFeedback.Free;
  EventProps.Free;
  JavaScoreXML.Free;
  RaceDataXML.Free;

  ListingNode.Free;
  ListingBO.Free;

  RoundingsNode.Free;
  RoundingsBO.Free;

  for i := 0 to BOParams.RaceCount do
  begin
    RNode[i].Free;
    RNode[i] := nil;
  end;

  RaceBO.Free;

  EventNode.Free;
  EventBO.Free;

  StammdatenNode.Free;
  StammdatenBO.Free;

  FNodeList.Free;

  CalcEV.Free;
  CalcTP.Free;

  UndoConnection := nil;
  UndoManager.Free;
  Watches.Free;
  ExcelImporter.Free;
  TableExporter.Free;

  MsgTree.Free;
  UndoAgent.Free;
  BOParams.Free;
  PenaltyService.Free;
  CurrentNumbers.Free;
  Output := nil;
  inherited;
end;

procedure TBO.SetModified(Sender: TObject);
begin
  FModified := True;
end;

procedure TBO.LoadPartial(ML: TStrings);
begin
  Load_(ML.Text, ML);
end;

procedure TBO.Load_(const Data: string; ML: TStrings);
var
  i: Integer;
  s: string;
  msg: TBOMsg;
begin
  //Clear;
  msg := TBOMsg.Create;
  FLoading := True;
  try
    ExcelImporter.RunImportFilter(Data, ML);
    ConvertedData := ML.Text;
    for i := 0 to ML.Count - 1 do
    begin
      s := ML[i];
      msg.prot := s;
      if not msg.DispatchProt then
        Main.Logger.Info('MessageError: ' + s);
    end;
    InitEventNode;
  finally
    FLoading := False;
    msg.Free;
  end;
//  UndoManager.Clear;
//  UndoManager.UpdateBase(Data);
end;

procedure TBO.Load(const Data: string);
var
  ML: TStringList;
begin
  Clear;

  ML := TStringList.Create;
  Load_(Data, ML);
  ML.Free;

  EventHash := ResultHash.GetTextHash(ConvertedData);
  UndoManager.Clear;
  UndoManager.UpdateBase(Data);
end;

function TBO.Save(XML: Boolean): string;
begin
  FSLBackup := TStringList.Create;
  try
    if XML then
      BackupToXML(nil)
    else
      BackupToText(nil);
    result := FSLBackup.Text;
  finally
    FSLBackup.Free;
    FSLBackup := nil;
  end;
end;

procedure TBO.Backup;
begin
  FSLBackup := TDBStringList.Create;
  try
    BackupToText(nil);
    Main.StoreAdapter.SaveBackup(FSLBackup);
  finally
    FSLBackup.Free;
    FSLBackup := nil;
  end;
end;

procedure TBO.Clear;
begin
  ClearBtnClick;
end;

function TBO.ToString: string;
begin
  result := ToTXT;
end;

function TBO.ToHtml: string;
begin
  result := Output.GetMsg('FR.*.Output.Report.FinishReport');
end;

function TBO.ToNormalTXT: string;
var
  b: Boolean;
begin
  b := UseCompactFormat;
  UseCompactFormat := False;
  result := ToTXT;
  UseCompactFormat := b;
end;

function TBO.ToCompactTXT: string;
var
  b: Boolean;
begin
  b := UseCompactFormat;
  UseCompactFormat := True;
  result := ToTXT;
  UseCompactFormat := b;
end;

function TBO.ToTXT: string;
var
  SL: TStrings;
begin
  try
    SL := TStringList.Create;
    try
      BackupToText(SL);
      result := SL.Text;
    finally
      SL.Free;
    end;
  except
    result := '';
  end;
end;

function TBO.ToXML: string;
var
  SL: TStrings;
begin
  try
    SL := TStringList.Create;
    try
      BackupToXML(SL);
      result := SL.Text;
    finally
      SL.Free;
    end;
  except
    result := '';
  end;
end;

function TBO.ToJson: string;
var
  SL: TStrings;
begin
  try
    SL := TStringList.Create;
    try
      BackupToJson(SL);
      result := SL.Text;
    finally
      SL.Free;
    end;
  except
    result := '';
  end;
end;

procedure TBO.BackupToText(SL: TStrings);
var
  InputAction: TInputAction;

  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;

  rn: TRaceNode;
  wl: TRaceRowCollection;
  wr: TRaceRowCollectionItem;

  i: Integer;
  g: TDivision;
  r: TRun;
  n: Integer;
  t: Integer;
  ExcelExporter: TExcelExporter;
  f: Integer;
  ere: TEventRaceEntry;
begin
  UpdateRaceNodes;
  if SL <> nil then
    FSLBackup := SL;

  InputAction := TInputAction.Create;
  InputAction.OnSend := SaveLine;
  TInputActionManager.DynamicActionRef := InputAction;

  ExcelExporter := TExcelExporter.Create;
  ExcelExporter.Delimiter := ';';

  try
    FSLBackup.Add('#Params');
    FSLBackup.Add('');
    FSLBackup.Add('DP.StartlistCount = ' + IntToStr(BOParams.StartlistCount));
    FSLBackup.Add('DP.ITCount = ' + IntToStr(BOParams.ITCount));
    FSLBackup.Add('DP.RaceCount = ' + IntToStr(BOParams.RaceCount));

    { EventProps }
    FSLBackup.Add('');
    FSLBackup.Add('#Event Properties');
    FSLBackup.Add('');

    EventProps.SaveProps(FSLBackup);

    { CaptionList }
    if ColCaptionBag.IsPersistent and (ColCaptionBag.Count > 0) then
    begin
      FSLBackup.Add('');
      ExcelExporter.AddSection(TableID_CaptionList, FSLBackup);
    end;

    if UseCompactFormat then
    begin
      try
        if Main.Params.WantEntryList then
        begin
          { NameList }
          FSLBackup.Add('');
          ExcelExporter.AddSection(TableID_NameList, FSLBackup);
        end;

        { StartList }
        FSLBackup.Add('');
        ExcelExporter.AddSection(TableID_StartList, FSLBackup);

        { FleetList }
        if EventNode.UseFleets then
        begin
          FSLBackup.Add('');
          ExcelExporter.AddSection(TableID_FleetList, FSLBackup);
        end;

        { FinishList }
        FSLBackup.Add('');
        ExcelExporter.AddSection(TableID_FinishList, FSLBackup);

        { TimeList(s) }
        if (BOParams.ITCount > 0) or EventProps.IsTimed then
        begin
          FSLBackup.Add('');
          ExcelExporter.AddSection(TableID_TimeList, FSLBackup);
        end;

        { CompareList }
        FSLBackup.Add('');
        ResultHash.WriteCompareList(FSLBackup);
      except
      end;
    end
    else
    begin
      if Main.Params.WantEntryList then
      begin
        { Athletes }
        FSLBackup.Add('');
        FSLBackup.Add('#Athletes');
        FSLBackup.Add('');
        if UseOutputFilter then
          BackupAthletesHack(MsgTree)
        else
          BackupAthletes(MsgTree);
      end;

      { Startlist }
      FSLBackup.Add('');
      FSLBackup.Add('#Startlist');
      FSLBackup.Add('');
      rn := RNode[1];
      g := MsgTree.Division;
      wl := rn.RaceRowCollection;
      for i := 0 to wl.Count - 1 do
      begin
        wr := wl.Items[i];
        if (wr.Bib > 0) and (wr.Bib <> wr.BaseID) then
          g.Race1.Startlist.Pos[wr.BaseID].Bib(IntToStr(wr.Bib));
        if wr.SNR > 0 then
          g.Race1.Startlist.Pos[wr.BaseID].SNR(IntToStr(wr.SNR));
      end;
    end;

    { Results }
    for n := 1 to BOParams.RaceCount do
    begin
      FSLBackup.Add('');
      FSLBackup.Add('#' + cTokenRace + IntToStr(n));
      FSLBackup.Add('');

      rn := RNode[n];
      g := MsgTree.Division;
      cl := EventNode.EventRowCollection;
      wl := rn.RaceRowCollection;
      if n = 1 then
        r := g.Race1
      else if (n > 1) and (n <= BOParams.RaceCount) then
        r := g.Race[n]
      else
        r := nil;
      if r = nil then
        Continue;
      if not rn.IsRacing then
        r.IsRacing(BoolStr[False]);
      for i := 0 to wl.Count - 1 do
      begin
        cr := cl.Items[i];
        ere := cr.Race[n];
        wr := wl.Items[i];
        if (i = 0) and wr.ST.TimePresent then
          r.Bib[wr.Bib].ST(wr.ST.AsString);

        if not UseCompactFormat then
        begin
          for t := 1 to BOParams.ITCount do
          begin
            if wr.IT[t].OTime.TimePresent then
              r.Bib[wr.Bib].IT[t] := wr.IT[t].OTime.AsString;
          end;
          if wr.FT.OTime.TimePresent then
            r.Bib[wr.Bib].FT(wr.FT.OTime.AsString);

          if wr.MRank > 0 then
            r.Bib[wr.Bib].Rank(IntToStr(wr.MRank));

          if EventNode.UseFleets then
          begin
            f := ere.Fleet;
            if f > 0 then
              r.Bib[wr.Bib].FM(IntToStr(f));
          end;
        end;

        if EventNode.UseFleets then
        begin
          if not ere.IsRacing then
            r.Bib[wr.Bib].RV('x');
        end;

        if wr.QU.AsInteger <> 0 then
          r.Bib[wr.Bib].QU(wr.QU.ToString);
        if wr.DG > 0 then
          r.Bib[wr.Bib].DG(IntToStr(wr.DG));
      end;
    end;

    { InputMode wiederherstellen }
    FSLBackup.Add('');
    FSLBackup.Add('EP.IM = ' + InputModeStrings[EventProps.InputMode]);

    { Errors }
    EventNode.ErrorList.CheckAll(EventNode);
    if EventNode.ErrorList.HasErrors then
    begin
      FSLBackup.Add('');
      FSLBackup.Add('#Errors');
      FSLBackup.Add('');
      EventNode.ErrorList.GetMsg(FSLBackup);
    end;
  finally
    if SL <> nil then
      FSLBackup := nil;
    TInputActionManager.DynamicActionRef := nil;
    InputAction.Free;
    ExcelExporter.Free;
  end;
end;

procedure TBO.BackupToXML(SL: TStrings);
var
  XmlWriter: TFRXMLWriter;
begin
  UpdateRaceNodes;
  if SL <> nil then
    FSLBackup := SL;
  XmlWriter := TFRXMLWriter.Create();
  try
    XmlWriter.Delimiter := ';';
    XmlWriter.WriteXML(FSLBackup);
  finally
    if SL <> nil then
      FSLBackup := nil;
    XmlWriter.Free;
  end;
end;

procedure TBO.BackupToJson(SL: TStrings);
var
  ji: JsonInfo;
begin
  if SL <> nil then
    FSLBackup := SL;
  ji := JsonInfo.Create;
  try
    ji.WriteJson(FSLBackup);
  finally
    if SL <> nil then
      FSLBackup := nil;
    ji.Free;
  end;
end;

procedure TBO.SaveLine(Sender: TObject; s: string);
begin
  FSLBackup.Add(s);
end;

function TBO.UpdateAthlete(SNR: Integer; Cmd, Value: string): Boolean;
var
  bo: TStammdatenBO;
  cr: TStammdatenRowCollectionItem;
  Key: string;
begin
  bo := StammdatenBO;
  cr := StammdatenNode.StammdatenRowCollection.FindKey(SNR);
  if cr = nil then
  begin
    cr := StammdatenNode.StammdatenRowCollection.Add;
    cr.BaseID := StammdatenNode.StammdatenRowCollection.Count;
    cr.SNR := SNR;
  end;

  if Pos('Prop_', Cmd) > 0 then
  begin
    Key := Copy(Cmd, 6, Length(Cmd));
    cr.Props.Value[Key] := Value;
  end
  else if (Cmd = N_FN) or (Cmd = 'FN') or (Cmd = 'FirstName') then
    bo.EditFN(cr, Value)
  else if (Cmd = N_LN) or (Cmd = 'LN') or (Cmd = 'LastName') then
    bo.EditLN(cr, Value)
  else if (Cmd = N_SN) or (Cmd = 'SN') or (Cmd = 'ShortName') then
    bo.EditSN(cr, Value)
  else if (Cmd = N_NC) or (Cmd = 'NOC') then
    bo.EditNC(cr, Value)
  else if (Cmd = N_GR) or (Cmd = 'Gender') then
    bo.EditGR(cr, Value)
  else if (Cmd = N_PB) or (Cmd = 'PB') then
    bo.EditPB(cr, Value)
  else if (Copy(Cmd, 1, 1) = 'N') then
    bo.EditNameColumn(cr, Value, 'col_' + Cmd);

  result := True;
end;

procedure TBO.BackupAthletes(Model: TMsgTree);
var
  i: Integer;
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  j: Integer;
  prop: TProp;
begin
  cl := StammdatenNode.StammdatenRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    if cr.FN <> '' then
      Model.Division.Athlete[cr.SNR].FN(cr.FN);
    if cr.LN <> '' then
      Model.Division.Athlete[cr.SNR].LN(cr.LN);
    if cr.SN <> '' then
      Model.Division.Athlete[cr.SNR].SN(cr.SN);
    if cr.NC <> '' then
      Model.Division.Athlete[cr.SNR].NC(cr.NC);
    if cr.GR <> '' then
      Model.Division.Athlete[cr.SNR].GR(cr.GR);
    if cr.PB <> '' then
      Model.Division.Athlete[cr.SNR].PB(cr.PB);
    if cl.FieldCount > FixFieldCount then
    begin
      for j := FixFieldCount to cl.FieldCount - 1 do
      begin
        Model.Division.Athlete[cr.SNR].FieldN(j, cr.FieldValue[j]);
      end;
    end
    else
    begin
      for j := 0 to cr.Props.Count-1 do
      begin
        cr.Props.GetProp(j, prop);
        Model.Division.Athlete[cr.SNR].Prop(prop.Key, prop.Value);
      end;
    end;
    if Assigned(FSLBackup) then
      FSLBackup.Add('');
  end;
end;

procedure TBO.BackupAthletesHack(Model: TMsgTree);
var
  i: Integer;
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  j: Integer;
  prop: TProp;
  s: string;
begin
  cl := StammdatenNode.StammdatenRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    if cr.FN <> '' then
      Model.Division.Athlete[cr.SNR].FN(cr.FN);
    if cr.LN <> '' then
      Model.Division.Athlete[cr.SNR].LN(cr.LN);
    if cr.SN <> '' then
      Model.Division.Athlete[cr.SNR].SN(cr.SN);
    if cr.NC <> '' then
      Model.Division.Athlete[cr.SNR].NC(cr.NC);
    if cr.GR <> '' then
    begin
      s := Lowercase(cr.GR);
      if Length(s) > 1 then
      begin
        s[1] := cr.GR[1];
      end;
      Model.Division.Athlete[cr.SNR].GR(s);
    end;
    if cr.PB <> '' then
      Model.Division.Athlete[cr.SNR].PB(UpperCase(cr.PB));
    if cl.FieldCount > FixFieldCount then
    begin
      for j := FixFieldCount to cl.FieldCount - 1 do
      begin
        Model.Division.Athlete[cr.SNR].FieldN(j, cr.FieldValue[j]);
      end;
    end
    else
    begin
      for j := 0 to cr.Props.Count-1 do
      begin
        cr.Props.GetProp(j, prop);
        Model.Division.Athlete[cr.SNR].Prop(prop.Key, prop.Value);
      end;
    end;
    if Assigned(FSLBackup) then
      FSLBackup.Add('');
  end;
end;

procedure TBO.BackupPenalties(SL: TStrings; n: Integer);
begin
  BackupPenaltiesRE(SL, n);
end;

procedure TBO.BackupPenaltiesRE(SL: TStrings; n: Integer);
var
  InputAction: TInputAction;
  g: TDivision;
  r: TRun;

  qn: TRaceNode;
  qc: TRaceRowCollection;
  qr: TRaceRowCollectionItem;

  ere: TEventRaceEntry;
  i: Integer;
begin
  UpdateRaceNodes;

  if SL <> nil then
    FSLBackup := SL;

  InputAction := TInputAction.Create;
  InputAction.OnSend := SaveLine;
  TInputActionManager.DynamicActionRef := InputAction;
  try
    qn := RNode[n];
    g := MsgTree.Division;
    qc := qn.RaceRowCollection;
    if (n = 1) then
      r := g.Race1
    else if (n > 1) and (n <= BOParams.RaceCount) then
      r := g.Race[n]
    else
      r := nil;
    if r <> nil then
    begin
      if not qn.IsRacing then
        r.IsRacing(BoolStr[false]);
      for i := 0 to qc.Count - 1 do
      begin
        qr := qc.Items[i];
        if (i = 0) and qr.ST.TimePresent then
          r.Bib[qr.Bib].ST(qr.ST.AsString);

        if EventNode.UseFleets then
        begin
          ere := EventNode.EventRowCollection.Items[i].Race[n];
          if not ere.IsRacing then
            r.Bib[qr.Bib].RV('x');
        end;

        if qr.QU.AsInteger <> 0 then
          r.Bib[qr.Bib].QU(qr.QU.ToString());
        if (qr.DG > 0) then
          r.Bib[qr.Bib].DG(qr.DG.ToString());
      end;
    end;
  finally
    if SL <> nil then
      FSLBackup := nil;
    TInputActionManager.DynamicActionRef := nil;
    InputAction.Free;
  end;
end;

procedure TBO.BackupPenaltiesEV(SL: TStrings; n: Integer);
var
  InputAction: TInputAction;
  g: TDivision;
  r: TRun;

  qn: TEventNode;
  qc: TEventRowCollection;
  qr: TEventRowCollectionItem;
  ere: TEventRaceEntry;

  i: Integer;
begin
  if SL <> nil then
    FSLBackup := SL;

  InputAction := TInputAction.Create;
  InputAction.OnSend := SaveLine;
  TInputActionManager.DynamicActionRef := InputAction;
  try
    qn := EventNode;
    g := MsgTree.Division;
    qc := qn.EventRowCollection;
    if n = 1 then
      r := g.Race1
    else if (n > 1) and (n <= BOParams.RaceCount) then
      r := g.Race[n]
    else
      r := nil;
    if r <> nil then
    begin
      if not IsRacing[n] then
        r.IsRacing(BoolStr[false]);
      for i := 0 to qc.Count - 1 do
      begin
        qr := qc.Items[i];
        ere := qr.Race[n];

        if EventNode.UseFleets then
        begin
          if not ere.IsRacing then
            r.Bib[qr.Bib].RV('x');
        end;

        if ere.QU <> 0 then
          r.Bib[qr.Bib].QU(ere.Penalty.ToString);
        if ere.DG > 0 then
          r.Bib[qr.Bib].DG(ere.DG.ToString);
      end;
    end;
  finally
    if SL <> nil then
      FSLBackup := nil;
    TInputActionManager.DynamicActionRef := nil;
    InputAction.Free;
  end;
end;

procedure TBO.LoadTestStartList;
begin
  //
end;

function TBO.NewMsg: TBaseMsg;
begin
  result := TBOMsg.Create;
end;

procedure TBO.OnIdle;
begin
  Calc;
  { pass any input message on to output clients }
  if Assigned(InputServer) then
    InputServer.ProcessQueue;
end;

function TBO.Calc: Boolean;
begin
  CalcNodes;
  result := FModified;
  if FModified then
  begin
    CalcEvent;
  end;
end;

procedure TBO.CalcNodes;
var
  i: Integer;
  bn: TBaseNode;
begin
  for i := 0 to FNodeList.Count - 1 do
  begin
    bn := FNodeList.NodeAt(i);
    CalcNode(bn);
  end;
end;

procedure TBO.CalcNode(bn: TBaseNode);
begin
  if bn.Modified then
  begin
    bn.Calc;
  end;
end;

procedure TBO.CalcEvent;
begin
  CalcNodes;
  Inc(CounterCalc);
  FModified := False;
end;

function TBO.FindNode(roName: string): TRaceNode;
var
  i: Integer;
  s: string;
begin
  result := nil;
  if Copy(roName, 1, 1) <> 'W' then
    Exit;
  s := Copy(roName, 2, Length(roName));
  i := StrToIntDef(s, -1);
  if (i < 1) or (i > BOParams.RaceCount) then
    Exit;
  result := RNode[i];
end;

function TBO.FindRaceIndex(roName: string): Integer;
var
  i: Integer;
  s: string;
begin
  result := -1;
  if Copy(roName, 1, 1) <> 'W' then
    Exit;
  s := Copy(roName, 2, Length(roName));
  i := StrToIntDef(s, -1);
  if (i < 1) or (i > BOParams.RaceCount) then
    Exit;
  result := i;
end;

procedure TBO.InitStartlistCount(newCount: Integer);
var
  i: Integer;
begin
  EventNode.Init(newCount);
  for i := 0 to BOParams.RaceCount do
  begin
    RNode[i].Init(newCount);
  end;
  RoundingsNode.Init(newCount * 3);
  ListingNode.Init(newCount * 3);
  Main.Params.TimingGridBibCount := newCount;
end;

function TBO.UpdateStartlistCount(roName: string; newCount: Integer): Boolean;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
  i: Integer;
  c: Integer;
begin
  result := False;

  cl := EventNode.EventRowCollection;

  if (cl.Count < newCount) and (newCount <= BOParams.MaxStartlistCount) then
  begin
    while cl.Count < newCount do
    begin
      cr := cl.Add;
      cr.SNR := 1001 + cr.Index;
      cr.Bib := cr.Index + 1;
      for i := 0 to BOParams.RaceCount do
      begin
        wr := RNode[i].RaceRowCollection.Add;
        wr.SNR := 1001 + wr.Index;
        wr.Bib := wr.Index + 1;
      end;
      for i := 1 to 3 do
      begin
        RoundingsNode.RoundingsCollection.Add;
        ListingNode.ListingCollection.Add;
      end;
    end;
    result := True;
  end;

  if (cl.Count > newCount) and (newCount >= BOParams.MinStartlistCount) then
  begin
    while cl.Count > newCount do
    begin
      c := cl.Count;
      cl.Delete(c-1);
      for i := 0 to BOParams.RaceCount do
        RNode[i].RaceRowCollection.Delete(c-1);
      for i := 1 to 3 do
      begin
        RoundingsNode.RoundingsCollection.Delete(3*c-i);
        ListingNode.ListingCollection.Delete(3*c-i);
      end;
    end;
    result := True;
  end;

  BOParams.StartlistCount := cl.Count;
  Main.Params.TimingGridBibCount := cl.Count;
end;

procedure TBO.ClearBtnClick;
begin
  ClearResult('');
  ClearList('');
  UpdateEventNode;
end;

procedure TBO.ClearCommand;
begin
  ClearBtnClick;
  UndoManager.Clear;
  Inform(TimePointChanged);
end;

procedure TBO.ResetRaceCommand(r: Integer);
begin
  EventNode.EventRowCollection.ResetRace(r);
  Inform(TGuiAction.ScheduleEventUpdate);
  Inform(TGuiAction.ClearAge);
end;

procedure TBO.ClearRaceCommand(r: Integer);
begin
  if (r > 0) and (r <= BO.BOParams.RaceCount) then
  begin
    RNode[r].RaceRowCollection.ClearResult;
    EventNode.EventRowCollection.ResetRace(r);
    EventNode.Modified := True;
    Calc;
    Inform(TimePointChanged);
    Inform(TGuiAction.ClearAge);
  end;
end;

procedure TBO.ClearTimepointCommand(r: Integer; tp: Integer);
var
  rn: TRaceNode;
begin
  if (r > 0) and (r <= BO.BOParams.RaceCount) then
  begin
    rn := RNode[r];
    if (tp >= 0) and (tp <= BO.BOParams.ITCount) then
    begin
      rn.RaceRowCollection.ResetIT(tp);
      if tp = 0 then
        CopyFromRaceNode(rn, False);
      Calc;
      Inform(TimePointChanged);
      Inform(TGuiAction.ClearAge);
    end;
  end;
end;

procedure TBO.GoBackToRaceCommand(r: Integer);
begin
  EventNode.GoBackToRace(r);
  Inform(TimePointChanged);
  Inform(TGuiAction.ClearAge);
end;

procedure TBO.ClearList(rd: string);
var
  bn: TBaseNode;
begin
  EventNode.EventRowCollection.ClearList;
  EventNode.Modified := True;

  bn := FNodeList.Node[rd];
  if Assigned(bn) then
  begin
    bn.BaseRowCollection.ClearList;
    bn.Modified := True;
  end
  else
  begin
    bn := FNodeList.First;
    while Assigned(bn) do
    begin
      bn.BaseRowCollection.ClearList;
      bn := FNodeList.Next;
    end;
  end;
end;

procedure TBO.ClearResult(rd: string);
var
  bn: TBaseNode;
begin
  EventNode.EventRowCollection.ClearResult;
  EventNode.Modified := True;

  bn := FNodeList.Node[rd];
  if Assigned(bn) then
  begin
    bn.BaseRowCollection.ClearResult;
    bn.Calc;
  end
  else
  begin
    bn := FNodeList.First;
    while Assigned(bn) do
    begin
      bn.BaseRowCollection.ClearResult;
      if not FLoading then
        bn.Calc;
      bn := FNodeList.Next;
    end;
  end;
end;

procedure TBO.Restore;
var
  ML: TStringList;
begin
  ML := TDBStringList.Create;
  try
    Main.StoreAdapter.LoadBackup(ML);
    if ML.Count > 0 then
      Load(ML.Text);
  finally
    ML.Free;
  end;
end;

procedure TBO.InitEventNode;
var
  i: Integer;
begin
  for i := 1 to BOParams.RaceCount do
    CopyFromRaceNode(RNode[i], True);
end;

procedure TBO.UpdateEventNode;
var
  i: Integer;
begin
  for i := 1 to BOParams.RaceCount do
    CopyFromRaceNode(RNode[i], False);
end;

procedure TBO.UpdateRaceNodes;
var
  i: Integer;
begin
  for i := 1 to BOParams.RaceCount do
    CopyToRaceNode(RNode[i]);
end;

procedure TBO.RebuildEventNode;
var
  wl: TEventRowCollection;
  wr: TEventRowCollectionItem;
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  wl := EventNode.EventRowCollection;
  wl.Clear;
  cl := RNode[0].RaceRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    wr := wl.Add;
    wr.BaseID := i + 1;
    wr.SNR := cr.SNR;
    wr.Bib := cr.Bib;
  end;
  UpdateEventNode;
end;

function TBO.GetBib(Index: Integer): Integer;
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    result := cr.Bib
  else
    result := -1;
end;

procedure TBO.SetBib(Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
  i: Integer;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    cr.Bib := Value;
  for i := 0 to BOParams.RaceCount do
  begin
    wr := RNode[i].RaceRowCollection.Items[Index];
    if Assigned(wr) then
      wr.Bib := Value;
  end;
end;

function TBO.GetSNR(Index: Integer): Integer;
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    result := cr.SNR
  else
    result := -1;
end;

function TBO.GetWatches: TAdapterWatches;
begin
  result := FLocalWatches;
end;

procedure TBO.SetSNR(Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
  i: Integer;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    cr.SNR := Value;
  for i := 0 to BOParams.RaceCount do
  begin
    wr := RNode[i].RaceRowCollection.Items[Index];
    if Assigned(wr) then
      wr.SNR := Value;
  end;
end;

procedure TBO.SetDG(RaceIndex, Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    cr.Race[RaceIndex].DG := Value;
    cr.Modified := True;
  end;
  wr := RNode[RaceIndex].RaceRowCollection.Items[Index];
  if Assigned(wr) then
  begin
    wr.DG := Value;
    wr.Modified := True;
  end;
end;

procedure TBO.SetOT(RaceIndex, Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    EventBO.RelaxedInputMode := True;
    cr.Race[RaceIndex].OTime := Value; //always relaxed mode if msg is coming from outside
    cr.Modified := True;
  end;
  wr := RNode[RaceIndex].RaceRowCollection.Items[Index];
  if Assigned(wr) then
  begin
    wr.MRank := Value;
    wr.Modified := True;
  end;
end;

function TBO.GetDG(RaceIndex, Index: Integer): Integer;
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    result := cr.Race[RaceIndex].DG
  else
    result := 0;
end;

function TBO.GetOT(RaceIndex, Index: Integer): Integer;
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    result := cr.Race[RaceIndex].OTime
  else
    result := 0;
end;

function TBO.GetQU(RaceIndex, Index: Integer): Integer;
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    result := cr.Race[RaceIndex].QU
  else
  begin
    result := 0;
  end;
end;

procedure TBO.SetQU(RaceIndex, Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    cr.Race[RaceIndex].QU := Value;
    cr.Modified := True;
  end;
  wr := RNode[RaceIndex].RaceRowCollection.Items[Index];
  if Assigned(wr) then
  begin
    wr.QU.AsInteger := Value;
    wr.Modified := True;
  end;
end;

function TBO.GetPenalty(RaceIndex, Index: Integer): TPenalty;
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    result := cr.Race[RaceIndex].Penalty
  else
    result := nil;
end;

procedure TBO.SetPenalty(RaceIndex, Index: Integer; const Value: TPenalty);
var
  cr: TEventRowCollectionItem;
  wr: TRaceRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    cr.Race[RaceIndex].Penalty.Assign(Value);
    cr.Modified := True;
  end;
  wr := RNode[RaceIndex].RaceRowCollection.Items[Index];
  if Assigned(wr) then
  begin
    wr.QU.Assign(Value);
    wr.Modified := True;
  end;
end;

procedure TBO.CopyFromRaceNode(ru: TRaceNode; MRank: Boolean);
var
  wl: TEventRowCollection;
  wr: TEventRowCollectionItem;
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
  RaceIndex: Integer;
begin
  RaceIndex := ru.Index;
  wl := EventNode.EventRowCollection;
  cl := ru.RaceRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    wr := wl.Items[i];
    wr.Race[RaceIndex].Penalty.Assign(cr.QU);
    wr.Race[RaceIndex].DG := cr.DG;
    if MRank then
      wr.Race[RaceIndex].OTime := cr.MRank
    else
      wr.Race[RaceIndex].OTime := cr.FT.ORank;
  end;
  wl.BaseNode.Modified := True;
end;

procedure TBO.CopyToRaceNode(ru: TRaceNode);
var
  wl: TEventRowCollection;
  wr: TEventRowCollectionItem;
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
  RaceIndex: Integer;
begin
  RaceIndex := ru.Index;
  wl := EventNode.EventRowCollection;
  cl := ru.RaceRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    wr := wl.Items[i];
    cr.QU.Assign(wr.Race[RaceIndex].Penalty);
    cr.DG := wr.Race[RaceIndex].DG;
    cr.MRank := wr.Race[RaceIndex].OTime;
  end;
end;

procedure TBO.CopyOTimes(RaceIndex: Integer);
var
  wl: TEventRowCollection;
  wr: TEventRowCollectionItem;
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  wl := EventNode.EventRowCollection;
  cl := RNode[RaceIndex].RaceRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    wr := wl.Items[i];
    cr.MRank := wr.Race[RaceIndex].OTime;
  end;
end;

function TBO.GetGemeldet: Integer;
begin
  result := EventNode.EventRowCollection.Count;
end;

function TBO.GetGesegelt: Integer;
begin
  result := BOParams.RaceCount;
end;

function TBO.GetHash: string;
begin
  result := EventNode.EventRowCollection.GetHashString;
end;

function TBO.GetLocalWatches: TLocalWatches;
begin
  result := Watches as TLocalWatches;
end;

procedure TBO.SetGezeitet(const Value: Integer);
begin
  FGezeitet := Value;
end;

procedure TBO.LoadNew(const Data: string);
begin
  Load(Data);
end;

procedure TBO.GetReportData(SL: TStrings);
var
  j: Integer;
begin
  for j := 1 to EventNode.RaceCount do
  begin
  end;
end;

function TBO.GetRunIsRacing(RunID: string): Boolean;
var
  i: Integer;
begin
  i := FindRaceIndex(RunID);
  if i > -1 then
    result := IsRacing[i]
  else
    result := False;
end;

procedure TBO.SetRunIsRacing(RunID: string; const Value: Boolean);
var
  i: Integer;
begin
  i := FindRaceIndex(RunID);
  if i > -1 then
    IsRacing[i] := Value;
end;

function TBO.GetIsRacing(Index: Integer): Boolean;
begin
  result := False;
  if (Index >= 0) and (Index <= BOParams.RaceCount) then
    result := RNode[Index].IsRacing;
end;

procedure TBO.SetIsRacing(Index: Integer; const Value: Boolean);
begin
  if (Index >= 0) and (Index <= BOParams.RaceCount) then
    RNode[Index].IsRacing := Value;
end;

function TBO.GetIT: Integer;
begin
  if Assigned(Main.GuiManager) then
    result := Main.GuiManager.IT
  else
    result := 0;
end;

function TBO.GetRace: Integer;
begin
  if Assigned(Main.GuiManager) then
    result := Main.GuiManager.Race
  else
    result := 1;
end;

function TBO.GetRaceNode: TRaceNode;
begin
  result := RNode[Race];
end;

procedure TBO.Inform(ga: TGuiAction);
begin
  if Assigned(Main.GuiManager.GuiInterface) then
    Main.GuiManager.GuiInterface.HandleInform(ga);
end;

procedure TBO.GetReportDataRace(SL: TStrings; j: Integer);
var
  rn: TRaceNode;
  rl: TRaceRowCollection;
  rr: TRaceRowCollectionItem;
  cr: TEventRowCollectionItem;
  i, k: Integer;
  Line: Integer;
  LastPos: Integer;
  temp, tempIT: string;
  tBehind: string;
begin
  if (j < 1) or (j > EventNode.RaceCount) then
    Exit;

  rn := RNode[j];
  if rn.IsRacing then
  begin
    rl := rn.RaceRowCollection;
    LastPos := rl.Count;
    SL.Add('StartlistCount=' + IntToStr(rl.Count));
    for i := 0 to rl.Count - 1 do
    begin
      rr := rl.Items[i];
      cr := EventNode.EventRowCollection.Items[i];

      Line := rr.IT[0].ORank;
      if Line = 0 then
      begin
        Line := LastPos;
        LastPos := LastPos - 1;
      end;

      temp := BOParams.DivisionName + '.W' + IntToStr(j) + '.L' + IntToStr(Line);
      SL.Add(temp + '.NOC=' + rr.NC);
      SL.Add(temp + '.LN=' + rr.LN);
      if rr.QU.ToString <> '' then
        SL.Add(temp + '.QU=' + rr.QU.ToString);
      SL.Add(temp + '.Points=' + cr.Race[j].Points);
      SL.Add(temp + '.PosR=' + IntToStr(cr.Race[j].OTime));

      for k := 0 to rr.ITCount - 1 do
      begin
        tempIT := temp + '.IT' + IntToStr(k);
        SL.Add(tempIT + '.PosR=' + IntToStr(rr.IT[k].ORank));
        tBehind := rr.IT[k].Behind.ToString;
        if Copy(tBehind, Length(tBehind) - 3 + 1, 3) = '.00' then
          tBehind := Copy(tBehind, 1, Length(tBehind) - 3);
        SL.Add(tempIT + '.Behind=' + tBehind);
      end;
    end;
  end;
end;

procedure TBO.GetSeriesDataSQL(SL: TStrings);
var
  rn: TRaceNode;
  rl: TRaceRowCollection;
  cr: TEventRowCollectionItem;
  i, j: Integer;
  s: string;
  sValue: string;
begin
  for j := 1 to EventNode.RaceCount do
  begin
    rn := RNode[j];
    if rn.IsRacing then
    begin
      rl := rn.RaceRowCollection;

      for i := 0 to rl.Count - 1 do
      begin
        cr := EventNode.EventRowCollection.Items[i];

        if j = EventNode.RaceCount then
        begin
          s := Format('Update SeriesResults set Netto = %s where Event = ''%s'' and Bib = %d',
            [cr.GPoints, BOParams.DivisionName, cr.Bib]);
          SL.Add(s);

          s := Format('Update SeriesResults set Rank = %d where Event = ''%s'' and Bib = %d',
            [cr.GRank, BOParams.DivisionName, cr.Bib]);
          SL.Add(s);

          s := Format('Update SeriesResults set PosR = %d where Event = ''%s'' and Bib = %d',
            [cr.GPosR, BOParams.DivisionName, cr.Bib]);
          SL.Add(s);
        end;

        sValue := cr.RaceValue[j];
        s := Format('Update SeriesResults set W%d = ''%s'' where Event = ''%s'' and Bib = %d',
          [j, sValue, BOParams.DivisionName, cr.Bib]);
        SL.Add(s);

      end;
    end;
  end;
end;

procedure TBO.GetRaceDataSQL(SL: TStrings);
var
  rn: TRaceNode;
  rl: TRaceRowCollection;
  rr: TRaceRowCollectionItem;
  cr: TEventRowCollectionItem;
  i, j, k: Integer;
  Line: Integer;
  LastPos: Integer;
  s: string;
  sValue: string;
  MarkID: Integer;
begin
  for j := 1 to EventNode.RaceCount do
  begin
    rn := RNode[j];
    if rn.IsRacing then
    begin
      rl := rn.RaceRowCollection;
      LastPos := rl.Count;

      for i := 0 to rl.Count - 1 do
      begin
        rr := rl.Items[i];
        cr := EventNode.EventRowCollection.Items[i];

        //PosR für MarkRoundingReport in Spalte Start schreiben:
        Line := rr.IT[0].PosR;
        if Line = 0 then
        begin
          Line := LastPos;
          LastPos := LastPos - 1;
        end;
        s := Format('Update RaceResults set Val = ''%d'' where Event = ''%s'' and Bib = %d and Race = %d and MarkID = 0',
          [Line, BOParams.DivisionName, cr.Bib, j]);
        SL.Add(s);

        for k := 0 to rr.ITCount - 1 do
        begin
          if k = 0 then
            MarkID := rr.ITCount
          else
            MarkID := k;
          sValue := IntToStr(rr.IT[k].PosR) + '<br/>' + rr.IT[k].Behind.ToString;
          s := Format('Update RaceResults set Val = ''%s'' where Event = ''%s'' and Bib = %d and Race = %d and MarkID = %d',
            [sValue, BOParams.DivisionName, cr.Bib, j, MarkID]);
          SL.Add(s);
        end;
      end;
    end;
  end;
end;

procedure TBO.EditQU(raceIndex: Integer; crIndex: Integer; const Value: string);
begin
  PenaltyService.Clear;
  if Pos(',',Value) > 0 then
    PenaltyService.FromString(Value)
  else
    PenaltyService.Parse(Value);
  Penalty[raceIndex, crIndex] := PenaltyService;
end;

procedure TBO.EditDG(raceIndex: Integer; crIndex: Integer; const Value: string);
var
  t: Integer;
begin
  t := StrToIntDef(Value, -1);
  if t > -1 then
    DG[raceIndex, crIndex] := t;
end;

procedure TBO.EditOTime(raceIndex: Integer; crIndex: Integer; const Value: string);
var
  t: Integer;
begin
  t := StrToIntDef(Value, -1);
  if t > -1 then
    OT[raceIndex, crIndex] := t;
end;

procedure TBO.UpdateRace;
begin
  //SetRace(GetRace);
end;

{$ifdef RoundingsOnly}
procedure TBO.SetRace(const Value: Integer);
var
  s: string;
  FCurrentRace: Integer;
begin
  FCurrentRace := Race;
  if FCurrentRace <> Value then
  begin
    {Save}
    s := TableExporter.GetTimeTableString(FCurrentRace);
    TimeTable[FCurrentRace] := s;

    {Clear}
    RaceNode.RaceRowCollection.ClearResult;

    { Update Current Race}
    //FCurrentRace := Value;
    Main.GuiManager.Race := Value;

    {Load}
    s := TimeTable[Value];
    if s <> '' then
    begin
      LoadTimeTable(s);
      RaceNode.Calc;
    end;
  end;
end;

procedure TBO.LoadTimeTable(const Data: string);
var
  m: TStringList;
  i: Integer;
  s: string;
  msg: TBOMsg;
begin
  RaceNode.RaceRowCollection.ClearResult;
  m := TStringList.Create;
  msg := TBOMsg.Create;
  try
    ExcelImporter.RunImportFilter(Data, m);
    for i := 0 to m.Count - 1 do
    begin
      s := m[i];
      msg.prot := s;
      if not msg.DispatchProt then
        Main.Logger.Info('MessageError: ' + s);
    end;
  finally
    m.Free;
    msg.Free;
  end;
end;

function TBO.GetTimeTable(Index: Integer): string;
begin
  result := '';
  if (Index >= 0) and (Index <= BOParams.RaceCount) then
    result := FRaceData[Index].TimeTable;
end;

procedure TBO.SetTimeTable(Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index <= BOParams.RaceCount) then
    FRaceData[Index].TimeTable := Value;
end;

{$endif}

function TBO.GetData(Index: Integer): string;
begin
  if (Index >= 0) and (Index <= 3) then
  begin
    if Main.JsonCache <> nil then
      Main.JsonCache.Data(Index)
    else
    case Index of
      1: result := FData1;
      2: result := FData2;
      3: result := FData3;
      else result := '{}';
    end;
  end;
end;

procedure TBO.SetData(Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index <= 3) then
  begin
    if Main.JsonCache <> nil then
      Main.JsonCache.Push(Index, Value)
    else
    case Index of
      1: FData1 := Value;
      2: FData2 := Value;
      3: FData3 := Value;
    end;
  end;
end;

function TBO.FindCurrentInRace(AResult: TCurrentNumbers): TCurrentNumbers;
var
  foo: array of Integer;
  rc: Integer;
  tc: Integer;
  rn: TRaceNode;
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  tp: TTimePoint;
  it: Integer;
  i: Integer;
  r: Integer;
  t: Integer;
begin
  result := AResult;

  rc := EventNode.RaceCount;
  tc := BOParams.ITCount;

  SetLength(foo, tc+1);
  for i := 0 to tc-1 do
    foo[i] := i + 1;
  foo[tc] := 0; //finish time point at index 0 is last time point in race

	for r := rc downto 1 do
  begin
		rn := RNode[r];
    if (rn.IsRacing) then
    begin
			for t := tc downto 0 do
      begin
				it := foo[t];
        cl := rn.RaceRowCollection;
				result.Clear;
				for i := 0 to cl.Count-1 do
        begin
					cr := cl.Items[i];
					tp := cr.IT[it]; //access in correct order from last time point to first
					if tp.OTime.TimePresent or cr.QU.IsOut then
          begin
						result.race := r;
            result.tp := it;
						result.bib := cr.Bib;
						if (tp.OTime.TimePresent) then
							Inc(result.withTime);
						if (cr.QU.IsOut) then
							Inc(result.withPenalty);
						Inc(result.withTimeOrPenalty);
          end;
        end;
        if (result.withTimeOrPenalty > 0) then
        begin
					//result is dirty, something was found, a result should be returned from here, exit the loop with a result.

					if (result.withTimeOrPenalty = cl.Count) then
          begin
						//This time point is complete, let's figure out what comes next, before we return.

						if (it = 0) and (result.race < rc) then
            begin
							//this is the finish time point, the last time point in current race,
							//jump to the first time point in next race.
							Inc(result.race);
							result.tp := 1;
						end
            else if (it = 0) and (result.race = rc) then
            begin
							//do nothing, we are at the finish of the last race in the series
            end
            else if (it < tc) then
            begin
							//advance to the next time point in this race
							Inc(result.tp);
            end
            else if (it = tc) then
            begin
							//the finish time point is special, it is at index zero
							result.tp := 0;
							//we are still just advancing to the next time point in the race, the finish!
            end
          end;
          Exit;
				end;
      end;
    end;
  end;
  result.race := 1;
  if (tc > 0) then // and bo.EventProps.IsTimed
	  result.tp := 1;
end;

function TBO.FindCurrentInEvent(AResult: TCurrentNumbers): TCurrentNumbers;
var
  rc: Integer;
  tc: Integer;
  en: TEventNode;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  tp: TEventRaceEntry;

  r: Integer;
  t: Integer;
  i: Integer;
begin
  result := AResult;
  rc := EventNode.RaceCount;
  tc := BOParams.ITCount;

  en := EventNode;
	for r := rc downto 1 do
  begin
    for t := tc downto 0 do
    begin
	    cl := en.EventRowCollection;
			result.Clear();
			for i := 0 to cl.Count-1 do
      begin
			  cr := cl.Items[i];
			  tp := cr.Race[r];
			  if (tp.IsRacing) then
        begin
          if (tp.OTime > 0) or (tp.Penalty.IsOut) then
          begin
            result.race := r;
            result.bib := cr.Bib;
            if (tp.OTime > 0) then
              Inc(result.withTime);
            if (tp.Penalty.IsOut) then
              Inc(result.withPenalty);
            Inc(result.withTimeOrPenalty);
          end;
     	  end;
      end;
			if result.withTimeOrPenalty > 0 then
      begin
			  if (result.withTimeOrPenalty = cl.Count) then
        begin
          if (r < rc) then
          begin
            Inc(result.race);
          end;
        end;
        Exit;
      end;
    end;
  end;
	result.race := 1;
end;

procedure TBO.DoTimingEvent(
  ARace: Integer;
  AIT: Integer;
  ABib: Integer;
  AOption: Integer);
var
  mt: Integer;
  qu: string;
  erase: Boolean;
  te: string;
  tr: string;
  tempTime: string;
begin
  case AOption of
    1: begin mt := 1; qu := 'dns'; end;
    2: begin mt := 2; qu := 'dnf'; end;
    3: begin mt := 3; qu := 'dsq'; end;
    4: begin mt := 4; qu := 'ok'; end;
    else begin mt := 0; qu := ''; end;
  end;

  erase := AOption = 5;

  if mt > 0 then
  begin
    tr := Format('FR.*.W%d.Bib%d.QU=%s', [ARace, ABib, qu]);
    te := tr;
  end
  else if erase then
  begin
    tr := Format('FR.*.W%d.Bib%d.IT%d=-1', [ARace, ABib, AIT]);
    te := Format('FR.*.W%d.Bib%d.RV=0', [ARace, ABib, qu]);
  end
  else
  begin
    tempTime := FormatDateTime('hh:mm:ss.zz', Now);
    tr := Format('FR.*.W%d.Bib%d.IT%d=%s', [ARace, ABib, AIT, tempTime]);
    te := Format('FR.*.W%d.Bib%d.RV=500', [ARace, ABib]);
  end;

  { send only one QU msg, not two }
  if te = tr then
    te := '';

  { do not update finish positions if timepont is not the Finish }
  if AIT > 0 then
    te := '';

  UndoConnection.InjectMsg(tr);
  if te <> '' then
    UndoConnection.InjectMsg(te);

  Main.GuiManager.PlaySound(Sound_Click01);
end;


{ TCurrentNumbers }

procedure TCurrentNumbers.Clear;
begin
  race := 0;
  tp := 0;
  bib := 0;
  withTime := 0;
  withPenalty := 0;
  withTimeOrPenalty := 0;
end;

procedure TCurrentNumbers.Fill(ML: TStrings);
begin
  ML.Add('race = ' + IntToStr(race));
  ML.Add('tp = ' + IntToStr(tp));
  ML.Add('bib = ' + IntToStr(bib));
  ML.Add('');
  ML.Add('withTime = ' + IntToStr(withTime));
  ML.Add('withPenalty = ' + IntToStr(withPenalty));
  ML.Add('withTimeOrPenalty = ' + IntToStr(withTimeOrPenalty));
end;

function TBO.ToSimpleTXT: string;
var
  SL: TStrings;
begin
  try
    SL := TStringList.Create;
    try
      BackupToSimpleText(SL);
      result := SL.Text;
    finally
      SL.Free;
    end;
  except
    result := '';
  end;
end;

procedure TBO.BackupToSimpleText(SL: TStrings);
var
  InputAction: TInputAction;

  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;

  rn: TRaceNode;
  wl: TRaceRowCollection;
  wr: TRaceRowCollectionItem;

  i: Integer;
  g: TDivision;
  r: TRun;
  n: Integer;
  t: Integer;
  ExcelExporter: TExcelExporter;
  ere: TEventRaceEntry;
begin
  UpdateRaceNodes;
  if SL <> nil then
    FSLBackup := SL;

  InputAction := TInputAction.Create;
  InputAction.OnSend := SaveLine;
  TInputActionManager.DynamicActionRef := InputAction;

  ExcelExporter := TExcelExporter.Create;
  ExcelExporter.Delimiter := ';';

  try
    FSLBackup.Add('DP.StartlistCount = ' + IntToStr(BOParams.StartlistCount));
    FSLBackup.Add('DP.ITCount = ' + IntToStr(BOParams.ITCount));
    FSLBackup.Add('DP.RaceCount = ' + IntToStr(BOParams.RaceCount));

    if not EventProps.EventName.IsEmpty then
    begin
      FSLBackup.Add('');
      FSLBackup.Add('EP.Name = ' + BO.EventProps.EventName);
    end;

    { Results }
    for n := 1 to BOParams.RaceCount do
    begin
      FSLBackup.Add('');

      rn := RNode[n];
      g := MsgTree.Division;
      cl := EventNode.EventRowCollection;
      wl := rn.RaceRowCollection;
      if n = 1 then
        r := g.Race1
      else if (n > 1) and (n <= BOParams.RaceCount) then
        r := g.Race[n]
      else
        r := nil;
      if r = nil then
        Continue;
      if not rn.IsRacing then
        r.IsRacing(BoolStr[False]);
      for i := 0 to wl.Count - 1 do
      begin
        cr := cl.Items[i];
        ere := cr.Race[n];
        wr := wl.Items[i];
        if (i = 0) and wr.ST.TimePresent then
          r.Bib[wr.Bib].ST(wr.ST.AsString);

        for t := 1 to BOParams.ITCount do
        begin
          if wr.IT[t].OTime.TimePresent then
            r.Bib[wr.Bib].IT[t] := wr.IT[t].OTime.AsString;
        end;
        if wr.FT.OTime.TimePresent then
          r.Bib[wr.Bib].FT(wr.FT.OTime.AsString);

        if wr.MRank > 0 then
          r.Bib[wr.Bib].Rank(IntToStr(wr.MRank));

        if EventNode.UseFleets then
        begin
          if not ere.IsRacing then
            r.Bib[wr.Bib].RV('x');
        end;

        if wr.QU.AsInteger <> 0 then
          r.Bib[wr.Bib].QU(wr.QU.ToString);
        if wr.DG > 0 then
          r.Bib[wr.Bib].DG(IntToStr(wr.DG));
      end;
    end;

    { InputMode wiederherstellen }
    FSLBackup.Add('');
    FSLBackup.Add('EP.IM = ' + InputModeStrings[EventProps.InputMode]);

  finally
    if SL <> nil then
      FSLBackup := nil;
    TInputActionManager.DynamicActionRef := nil;
    InputAction.Free;
    ExcelExporter.Free;
  end;
end;

procedure TBO.BackupToSimpleJson(ML: TStrings);
var
  SL: TStrings;
  i: Integer;
  s: string;
begin
  ML.Clear;
  try
    SL := TStringList.Create;
    try
      BackupToSimpleText(SL);
      ML.Add('{"EventDataSimpleText":[');
      for i := 0 to SL.Count-1 do
      begin
        s := SL[i];
        if s.IsEmpty then
          Continue;
        if i = SL.Count-1 then
          ML.Add('"' + s + '"')
        else
          ML.Add('"' + s + '",');
      end;
      ML.Add(']}');
    finally
      SL.Free;
    end;
  except
  end;
end;

end.
