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
  RiggVar.App.GuiInterface,
  RiggVar.BO.Base,
  RiggVar.BO.EventProps,
  RiggVar.BO.ExcelExport,
  RiggVar.BO.ExcelImport,
  RiggVar.BO.MsgBase,
  RiggVar.BO.MsgToken,
  RiggVar.BO.MsgTree,
  RiggVar.BO.Params,
  RiggVar.BO.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.BO.ResultHash,
  RiggVar.BO.UndoManager,
  RiggVar.BO.Watches,
  RiggVar.Calc.EV,
  RiggVar.Col.Captions,
  RiggVar.Col.Event,
  RiggVar.Col.Race,
  RiggVar.Col.Stammdaten,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.Conn.IO,
  RiggVar.Grid.ColGrid,
  RiggVar.Out.FR00,
  RiggVar.Util.Classes,
  RiggVar.Util.Props;

type
  TBO = class(TBaseBO)
  private
    FSLBackup: TStrings;
    FLoading: Boolean;
    FGezeitet: Integer;
    FLocalWatches: TLocalWatches;
    FRaceData: array of TRaceData;
    procedure SaveLine(Sender: TObject; s: string);
    procedure ClearList(rd: string);
    procedure ClearResult(rd: string);
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
  protected
    procedure BackupPenaltiesRE(SL: TStrings; n: Integer);
    procedure BackupPenaltiesEV(SL: TStrings; n: Integer);
    function GetWatches: TAdapterWatches; override;
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
    StammdatenBO: TStammdatenBO;
    StammdatenNode: TStammdatenNode;
    //
    CalcEV: TCalcEvent;
    //
    EventProps: TEventProps;
    //
    UndoManager: TUndoManager;
    UndoConnection: TConnection;
    PenaltyService: TPenalty;
    ExcelImporter: TExcelImporter;

    UndoAgent: TUndoAgent;
    MsgTree: TMsgTree;

    constructor Create(Params: TBOParams);
    destructor Destroy; override;
    procedure Init;
    procedure Inform(ga: TGuiAction);
    //
    function GetHash: string;
    function ToString: string; override;
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
    function UpdateStartlistCount(roName: string; newCount: Integer): Boolean;
    function UpdateAthlete(SNR: Integer; Cmd, Value: string): Boolean;
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
    procedure BackupToSL(SL: TStrings);
    procedure BackupToXML(SL: TStrings);
    procedure BackupToJson(SL: TStrings);

    procedure Restore;

    procedure GetReportData(SL: TStrings);

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
    procedure LoadTestStartList;
    //
    procedure InitEventNode;
    procedure UpdateEventNode;
    procedure UpdateRaceNodes;
    procedure RebuildEventNode;
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
  end;

var
  BO: TBO;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Msg,
  RiggVar.BO.Writer,
  RiggVar.BO.WriterJson,
  RiggVar.DAL.Redirector;

{ TBO }

constructor TBO.Create(Params: TBOParams);
begin
  inherited Create;
  BOParams := Params;
end;

procedure TBO.Init;
var
  i: Integer;
  ts: TServerIntern;
begin
  BO := self;
  InitDefaultColCaptions;

  SetDivisionName(BOParams.DivisionName);

  PenaltyService := TPenaltyISAF.Create;

  ExcelImporter := TExcelImporter.Create;

  CalcEV := TCalcEvent.Create(Main.IniImage.ScoringProvider);

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

  { RaceData }
  SetLength(FRaceData, BOParams.RaceCount + 1);
  for i := 0 to BOParams.RaceCount do
    FRaceData[i].IsRacing := True;

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
  ResultHash := TResultHash01.Create;

  UndoManager := TUndoManager.Create;
  UndoConnection := InputServer.Server.Connect('Undo.Input');
  FLocalWatches := TLocalWatches.Create;

  UseCompactFormat := True; //temporary default
end;

destructor TBO.Destroy;
begin
  ResultHash.Free;
  EventProps.Free;

  EventNode.Free;
  EventBO.Free;

  StammdatenNode.Free;
  StammdatenBO.Free;

  CalcEV.Free;

  UndoConnection := nil;
  UndoManager.Free;
  Watches.Free;
  ExcelImporter.Free;

  MsgTree.Free;
  UndoAgent.Free;
  BOParams.Free;
  PenaltyService.Free;
  Output := nil;
  inherited;
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
    BackupToSL(nil);
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
    BackupToSL(nil);
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
      BackupToSL(SL);
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

procedure TBO.BackupToSL(SL: TStrings);
var
  InputAction: TInputAction;

  en: TEventNode;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;

  i: Integer;
  g: TDivision;
  r: TRun;
  n: Integer;
  ExcelExporter: TExcelExporter;
  f: Integer;
  ere: TEventRaceEntry;
begin
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
      en := EventNode;
      g := MsgTree.Division;
      cl := en.EventRowCollection;
      for i := 0 to cl.Count - 1 do
      begin
        cr := cl.Items[i];
        if (cr.Bib > 0) and (cr.Bib <> cr.BaseID) then
          g.Race1.Startlist.Pos[cr.BaseID].Bib(IntToStr(cr.Bib));
        if cr.SNR > 0 then
          g.Race1.Startlist.Pos[cr.BaseID].SNR(IntToStr(cr.SNR));
      end;
    end;

    { Results }
    for n := 1 to BOParams.RaceCount do
    begin
      FSLBackup.Add('');
      FSLBackup.Add('#' + cTokenRace + IntToStr(n));
      FSLBackup.Add('');

      g := MsgTree.Division;
      cl := EventNode.EventRowCollection;
      if n = 1 then
        r := g.Race1
      else if (n > 1) and (n <= BOParams.RaceCount) then
        r := g.Race[n]
      else
        r := nil;
      if r = nil then
        Continue;
      if not FRaceData[n].IsRacing then
        r.IsRacing(BoolStr[False]);
      for i := 0 to cl.Count - 1 do
      begin
        cr := cl.Items[i];
        ere := cr.Race[n];

        if not UseCompactFormat then
        begin
          if ere.OTime > 0 then
            r.Bib[cr.Bib].Rank(IntToStr(ere.OTime));

          if EventNode.UseFleets then
          begin
            f := ere.Fleet;
            if f > 0 then
              r.Bib[cr.Bib].FM(IntToStr(f));
          end;
        end;

        if EventNode.UseFleets then
        begin
          if not ere.IsRacing then
            r.Bib[cr.Bib].RV('x');
        end;

        if ere.QU <> 0 then
          r.Bib[cr.Bib].QU(ere.Penalty.ToString);
        if ere.DG > 0 then
          r.Bib[cr.Bib].DG(IntToStr(ere.DG));
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

procedure  TBO.BackupPenalties(SL: TStrings; n: Integer);
begin
  BackupPenaltiesEV(SL, n);
end;

procedure TBO.BackupPenaltiesRE(SL: TStrings; n: Integer);
begin
  //not implemented;
end;

procedure  TBO.BackupPenaltiesEV(SL: TStrings; n: Integer);
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
  Inc(CounterCalc);
  result := false;
  if EventNode.Modified then
  begin
    EventNode.Calc;
    result := true;
  end;
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
begin
  EventNode.Init(newCount);
  Main.Params.TimingGridBibCount := newCount;
end;

function TBO.UpdateStartlistCount(roName: string; newCount: Integer): Boolean;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
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
    end;
    result := True;
  end;

  if (cl.Count > newCount) and (newCount >= BOParams.MinStartlistCount) then
  begin
    while cl.Count > newCount do
    begin
      c := cl.Count;
      cl.Delete(c-1);
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
  self.ClearBtnClick;
  self.UndoManager.Clear;
end;

procedure TBO.ClearList(rd: string);
begin
  EventNode.EventRowCollection.ClearList;
  EventNode.Modified := True;
end;

procedure TBO.ClearResult(rd: string);
begin
  EventNode.EventRowCollection.ClearResult;
  EventNode.Modified := True;
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
begin
end;

procedure TBO.UpdateEventNode;
begin
end;

procedure TBO.UpdateRaceNodes;
begin
end;

procedure TBO.RebuildEventNode;
begin
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
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    cr.Bib := Value;
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
begin
  cr := self.EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
    cr.SNR := Value;
end;

procedure TBO.SetDG(RaceIndex, Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    cr.Race[RaceIndex].DG := Value;
    cr.Modified := True;
  end;
end;

procedure TBO.SetOT(RaceIndex, Index: Integer; const Value: Integer);
var
  cr: TEventRowCollectionItem;
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    EventBO.RelaxedInputMode := True;
    cr.Race[RaceIndex].OTime := Value; //always relaxed mode if msg is coming from outside
    cr.Modified := True;
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
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    cr.Race[RaceIndex].QU := Value;
    cr.Modified := True;
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
begin
  cr := EventNode.EventRowCollection.Items[Index];
  if Assigned(cr) then
  begin
    cr.Race[RaceIndex].Penalty.Assign(Value);
    cr.Modified := True;
  end;
end;

function TBO.GetGemeldet: Integer;
begin
  result := self.EventNode.EventRowCollection.Count;
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
    result := FRaceData[Index].IsRacing;
end;

procedure TBO.SetIsRacing(Index: Integer; const Value: Boolean);
begin
  if (Index >= 0) and (Index <= BOParams.RaceCount) then
    FRaceData[Index].IsRacing := Value;
end;

procedure TBO.Inform(ga: TGuiAction);
begin
  if Assigned(Main.GuiManager.GuiInterface) then
    Main.GuiManager.GuiInterface.HandleInform(ga);
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

end.
