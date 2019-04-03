unit RiggVar.BO.EventProps;

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
  RiggVar.Col.Event,
  RiggVar.Col.Stammdaten;

type
  TThrowoutScheme = (
    throwoutDefault,
    throwoutBYNUMRACES, // = 1;
    throwoutPERXRACES, // = 2;
    throwoutBESTXRACES, // = 3;
    throwoutNONE // = 4;
    );

  TScoringSystem = (
    LowPoint,
    Bonus,
    BonusDSV
    );

  TInputMode = (
    Strict,
    Relaxed
    );

const
  JavaScore_ThrowoutSchemeStrings: array[TThrowoutScheme] of String = (
    'ByNumRaces', //Default 0
    'ByNumRaces', //1
    'PerXRaces', //2
    'BestXRaces', //3
    'None' //4
    );
  JavaScore_ScoringSystemStrings: array[TScoringSystem] of String = (
    'Low Point System',
    'Bonus Point System',
    'Bonus Point System DSV'
    );
  InputModeStrings: array[TInputMode] of String = (
    'Strict',
    'Relaxed'
    );

type
  TEventProps = class(TLineParser)
  private
    procedure SetGemeldet(const Value: Integer);
    procedure SetGesegelt(const Value: Integer);
    procedure SetGezeitet(const Value: Integer);
    function GetGemeldet: Integer;
    function GetGesegelt: Integer;
    function GetGezeitet: Integer;
    procedure SetFaktor(const Value: double);
    procedure SetDivisionName(const Value: string);
    function GetDivisionName: string;
    function GetInputMode: TInputMode;
    procedure SetInputMode(const Value: TInputMode);
    function GetFieldMap: string;
    procedure SetFieldMap(const Value: string);
    function GetRaceLayout: string;
    procedure SetRaceLayout(const Value: string);
    function GetNameSchema: string;
    procedure SetNameSchema(const Value: string);
    function GetFieldCaptions: string;
    procedure SetFieldCaptions(const Value: string);
    function GetFieldCount: string;
    procedure SetFieldCount(const Value: string);
    function GetColorMode: string;
    procedure SetColorMode(const Value: string);
    function GetTargetFleetSize: Integer;
    procedure SetTargetFleetSize(const Value: Integer);
    function GetUseFleets: Boolean;
    procedure SetUseFleets(const Value: Boolean);
    function GetFirstFinalRace: Integer;
    procedure SetFirstFinalRace(const Value: Integer);
    function GetUseCompactFormat: Boolean;
    procedure SetUseCompactFormat(const Value: Boolean);
    function GetUseOutputFilter: Boolean;
    procedure SetUseOutuptFilter(const Value: Boolean);
    function GetShowPLZColumn: Boolean;
    procedure SetShowPLZColumn(const Value: Boolean);
    function GetShowPosRColumn: Boolean;
    procedure SetShowPosRColumn(const Value: Boolean);
    function GetNameFieldCount: string;
    procedure SetNameFieldCount(const Value: string);
    function GetNameFieldOrder: string;
    procedure SetNameFieldOrder(const Value: string);
    procedure SetWantDiffColumns(const Value: Boolean);
    function GetWantDiffColumns: Boolean;
  protected
    function ParseKeyValue(Key, Value: string): Boolean; override;
  public

    { Regatta Props }
    EventName: string;
    EventDates: string;
    HostClub: string;
    PRO: string; //Principal Race Officer (Wettfahrtleiter)
    JuryHead: string;
    ScoringSystem: TScoringSystem;
    ScoringSystem2: Integer; //external scoring system code, pass on as is via proxy
    Throwouts: Integer;
    ThrowoutScheme: Integer;
    FirstIs75: Boolean;
    ReorderRAF: Boolean;

    { Ranglisten Props }
    ShowCupColumn: Boolean;
    EnableUniquaProps: Boolean; //override calculated values
    UniquaGemeldet: Integer; //Count of Entries
    UniquaGesegelt: Integer; //Count of Races
    UniquaGezeitet: Integer; //Count of Entries at start
    FFaktor: double;

    { Other Props }
    IsTimed: Boolean;

    constructor Create;
    procedure InspectorOnLoad(Sender: TObject);
    procedure InspectorOnSave(Sender: TObject);

    { initialize default props }
    procedure LoadDefaultData;

    { save, load and edit props }
    procedure SaveProps(SLBackup: TStrings);
    function EditRegattaProps: Boolean;
    function EditUniquaProps: Boolean;
    function EditFleetProps: Boolean;

    { get calulated values for Uniqua props }
    function FRGemeldet: Integer;
    function FRGesegelt: Integer;
    function FRGezeitet: Integer;

    property Gemeldet: Integer read GetGemeldet write SetGemeldet;
    property Gesegelt: Integer read GetGesegelt write SetGesegelt;
    property Gezeitet: Integer read GetGezeitet write SetGezeitet;
    property Faktor: double read FFaktor write SetFaktor;
    property DivisionName: string read GetDivisionName write SetDivisionName;
    property InputMode: TInputMode read GetInputMode write SetInputMode;
    property FieldMap: string read GetFieldMap write SetFieldMap;
    property RaceLayout: string read GetRaceLayout write SetRaceLayout;
    property NameSchema: string read GetNameSchema write SetNameSchema;
    property FieldCaptions: string read GetFieldCaptions write SetFieldCaptions;
    property FieldCount: string read GetFieldCount write SetFieldCount;
    property NameFieldCount: string read GetNameFieldCount write SetNameFieldCount;
    property NameFieldOrder: string read GetNameFieldOrder write SetNameFieldOrder;
    property ColorMode: string read GetColorMode write SetColorMode;
    property TargetFleetSize: Integer read GetTargetFleetSize write SetTargetFleetSize;
    property UseFleets: Boolean read GetUseFleets write SetUseFleets;
    property FirstFinalRace: Integer read GetFirstFinalRace write SetFirstFinalRace;
    property UseCompactFormat: Boolean read GetUseCompactFormat write SetUseCompactFormat;
    property UseOutputFilter: Boolean read GetUseOutputFilter write SetUseOutuptFilter;
    property ShowPLZColumn: Boolean read GetShowPLZColumn write SetShowPLZColumn;
    property ShowPosRColumn: Boolean read GetShowPosRColumn write SetShowPosRColumn;
    property WantDiffColumns: Boolean read GetWantDiffColumns write SetWantDiffColumns;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Col.NameValue;

{ TEventProps }

constructor TEventProps.Create;
begin
  inherited Create;
  LoadDefaultData;
end;

procedure TEventProps.LoadDefaultData;
begin
  EventName := '';
  EventDates := '';
  HostClub := '';
  ScoringSystem := LowPoint;
  ScoringSystem2 := 0;
  ThrowoutScheme := 1;
  DivisionName := '*';
  ColorMode := 'Normal';

  ReorderRAF := true;
  ShowCupColumn := false;
  EnableUniquaProps := false;
  Gemeldet := Gemeldet;
  Gezeitet := Gezeitet;
  Gesegelt := Gesegelt;
  Faktor := 1.10;
end;

function TEventProps.EditRegattaProps: Boolean;
begin
  result := Main.FormAdapter.EditRegattaProps(Self);
end;

function TEventProps.EditUniquaProps: Boolean;
begin
  result := Main.FormAdapter.EditUniquaProps(Self);
end;

function TEventProps.EditFleetProps: Boolean;
begin
  result := Main.FormAdapter.EditFleetProps(Self);
end;

procedure TEventProps.SaveProps(SLBackup: TStrings);
begin
  SLBackup.Add('EP.Name = ' + EventName);
  if EventDates <> '' then
  SLBackup.Add('EP.Dates = ' + EventDates);
  if HostClub <> '' then
  SLBackup.Add('EP.HostClub = ' + HostClub);
  if PRO <> '' then
  SLBackup.Add('EP.PRO = ' + PRO);
  if JuryHead <> '' then
  SLBackup.Add('EP.JuryHead = ' + JuryHead);
  SLBackup.Add('EP.ScoringSystem = ' + JavaScore_ScoringSystemStrings[ScoringSystem]);
  if ScoringSystem2 <> 0 then
  SLBackup.Add('EP.ScoringSystem2 = ' + IntToStr(ScoringSystem2));
  SLBackup.Add('EP.Throwouts = ' + IntToStr(Throwouts));
  if ThrowoutScheme <> 1 then
  SLBackup.Add('EP.ThrowoutScheme = ' + JavaScore_ThrowoutSchemeStrings[TThrowoutScheme(ThrowoutScheme)]);
  if FirstIs75 then
    SLBackup.Add('EP.FirstIs75 = True');
  if ReorderRAF = false then
    SLBackup.Add('EP.ReorderRAF = False');
  SLBackup.Add('EP.DivisionName = ' + DivisionName);
  SLBackup.Add('EP.InputMode = ' + InputModeStrings[InputMode]);
  SLBackup.Add('EP.RaceLayout = ' + RaceLayout);
  SLBackup.Add('EP.NameSchema = ' + NameSchema);
  SLBackup.Add('EP.FieldMap = ' + FieldMap);
  SLBackup.Add('EP.FieldCaptions = ' + FieldCaptions);
  SLBackup.Add('EP.FieldCount = ' + FieldCount);
  SLBackup.Add('EP.NameFieldCount = ' + NameFieldCount);
  SLBackup.Add('EP.NameFieldOrder = ' + NameFieldOrder);
  if ShowPosRColumn then
  SLBackup.Add('EP.ShowPosRColumn = ' + BoolStr[ShowPosRColumn]);
  if ShowCupColumn  then
  SLBackup.Add('EP.ShowCupColumn = ' + BoolStr[ShowCupColumn]);
  if ColorMode <> 'Normal' then
  SLBackup.Add('EP.ColorMode = ' + ColorMode);
  SLBackup.Add('EP.UseFleets = ' + BoolStr[UseFleets]);
  SLBackup.Add('EP.TargetFleetSize = ' + IntToStr(TargetFleetSize));
  SLBackup.Add('EP.FirstFinalRace = ' + IntToStr(FirstFinalRace));
  SLBackup.Add('EP.IsTimed = ' + BoolStr[IsTimed]);
  SLBackup.Add('EP.UseCompactFormat = ' + BoolStr[UseCompactFormat]);
  if ShowCupColumn then
  begin
    SLBackup.Add('EP.Uniqua.Faktor = ' + FormatFloat('0.00', Faktor));
    SLBackup.Add('EP.Uniqua.Enabled  = ' + BoolStr[EnableUniquaProps]);
    SLBackup.Add('EP.Uniqua.Gesegelt = ' + IntToStr(Gesegelt));
    SLBackup.Add('EP.Uniqua.Gemeldet = ' + IntToStr(Gemeldet));
    SLBackup.Add('EP.Uniqua.Gezeitet = ' + IntToStr(Gezeitet));
  end;
end;

function TEventProps.ParseKeyValue(Key, Value: string): Boolean;
var
  //i: TScoringSystem;
  j: TThrowoutScheme;
begin
  result := True;

  if Pos('EP.', Key) = 1 then
    Key := Copy(Key, Length('EP.') + 1, Length(Key))
  else if Pos('Event.Prop_', Key) = 1 then
    Key := Copy(Key, Length('Event.Prop_') + 1, Length(Key));

  if Key = 'Name' then
    EventName := Value
  else if Key = 'Dates' then
    EventDates := Value
  else if Key = 'HostClub' then
    HostClub := Value
  else if Key = 'PRO' then
    PRO := Value
  else if Key = 'JuryHead' then
    JuryHead := Value
  else if Key = 'ScoringSystem' then
  begin
    if Pos('DSV', Value) > 0 then
      ScoringSystem := BonusDSV
    else if Pos('onus', Value) > 0 then
      ScoringSystem := Bonus
    else
      ScoringSystem := LowPoint;
    {
    for i := Low(TScoringSystem) to High(TScoringSystem) do
      if Value = JavaScore_ScoringSystemStrings[i] then
      begin
        ScoringSystem := i;
        break;
      end;
    }
  end
  else if Key = 'ScoringSystem' then
    ScoringSystem2 := StrToIntDef(Value, 0)
  else if Key = 'FirstIs75' then
    FirstIs75 := True
  else if Key = 'ReorderRAF' then
    ReorderRAF := TUtils.IsTrue(Value)
  else if Key = 'Throwouts' then
    Throwouts := StrToIntDef(Value, Throwouts)
  else if Key = 'ThrowoutScheme' then
  begin
    for j := throwoutBYNUMRACES to throwoutNONE do
      if Value = JavaScore_ThrowoutSchemeStrings[j] then
      begin
        ThrowoutScheme := Ord(j);
        break;
      end;
  end

  else if Key = 'ColorMode' then
    ColorMode := Value
  else if Key = 'UseFleets' then
    UseFleets := TUtils.IsTrue(Value)
  else if Key = 'TargetFleetSize' then
    TargetFleetSize := StrToIntDef(Value, TargetFleetSize)
  else if Key = 'FirstFinalRace' then
    FirstFinalRace := StrToIntDef(Value, FirstFinalRace)
  else if Key = 'IsTimed' then
    IsTimed := TUtils.IsTrue(Value)
  else if Key = 'UseCompactFormat' then
    UseCompactFormat := TUtils.IsTrue(Value)
  else if Key = 'ShowPosRColumn' then
    ShowPosRColumn := TUtils.IsTrue(Value)
  else if Key = 'ShowCupColumn' then
    ShowCupColumn := TUtils.IsTrue(Value)
  else if Key = 'Uniqua.Enabled' then
    EnableUniquaProps := TUtils.IsTrue(Value)
  else if Key = 'Uniqua.Gesegelt' then
    UniquaGesegelt := StrToIntDef(Value, Gesegelt)
  else if Key = 'Uniqua.Gemeldet' then
    UniquaGemeldet := StrToIntDef(Value, Gemeldet)
  else if Key = 'Uniqua.Gezeitet' then
    UniquaGezeitet := StrToIntDef(Value, Gezeitet)
  else if Key = 'Uniqua.Faktor' then
    Faktor := StrToFloatDef(Value, Faktor)
  else if Key = 'DivisionName' then
    DivisionName := Value
  else if (Key = 'InputMode') or (Key = 'IM') then
  begin
    if LowerCase(Value) = 'strict' then
      InputMode := Strict
    else
      InputMode := Relaxed;
  end
  else if Key = 'RaceLayout' then
    RaceLayout := Value
  else if Key = 'NameSchema' then
    NameSchema := Value
  else if Key = 'FieldCount' then
    FieldCount := Value
  else if Key = 'FieldCaptions' then
    FieldCaptions := Value
  else if Key = 'FieldMap' then
    FieldMap := Value
  else if Key = 'NameFieldCount' then
    NameFieldCount := Value
  else if Key = 'NameFieldOrder' then
    NameFieldOrder := Value
  else
    result := False;
end;

procedure TEventProps.SetGemeldet(const Value: Integer);
begin
  UniquaGemeldet := Value;
end;

procedure TEventProps.SetGesegelt(const Value: Integer);
begin
  UniquaGesegelt := Value;
end;

procedure TEventProps.SetGezeitet(const Value: Integer);
begin
  UniquaGezeitet := Value;
end;

function TEventProps.GetGemeldet: Integer;
begin
  if EnableUniquaProps then
    result := UniquaGemeldet
  else
    result := FRGemeldet;
end;

function TEventProps.GetGesegelt: Integer;
begin
  if EnableUniquaProps then
    result := UniquaGesegelt
  else
    result := FRGesegelt;
end;

function TEventProps.GetGezeitet: Integer;
begin
  if EnableUniquaProps then
    result := UniquaGezeitet
  else
    result := FRGezeitet;
end;

function TEventProps.FRGemeldet: Integer;
begin
  result := BO.Gemeldet;
end;

function TEventProps.FRGesegelt: Integer;
begin
  result := BO.Gesegelt;
end;

function TEventProps.FRGezeitet: Integer;
begin
  result := BO.Gezeitet;
end;

procedure TEventProps.SetFaktor(const Value: double);
begin
  if (Value > 0.1) and (Value < 10) then
    FFaktor := Value;
end;

procedure TEventProps.SetDivisionName(const Value: string);
begin
  cTokenB := Value; //cTokenGender, 420 or W or M
  cTokenSport := 'FR.' + Value + '.';
  cTokenOutput := cTokenSport + 'Output.'; // 'FR.420.Output.'
end;

function TEventProps.GetDivisionName: string;
begin
  result := cTokenB;
end;

function TEventProps.GetInputMode: TInputMode;
begin
  if BO.EventBO.RelaxedInputMode then
    result := Relaxed
  else
    result := Strict;
end;

procedure TEventProps.SetInputMode(const Value: TInputMode);
begin
  if Value = Relaxed then
    BO.EventBO.RelaxedInputMode := True
  else
    BO.EventBO.RelaxedInputMode := False
end;

function TEventProps.GetFieldMap: string;
begin
  result := BO.StammdatenNode.StammdatenRowCollection.FieldMap;
end;

procedure TEventProps.SetFieldMap(const Value: string);
begin
  BO.StammdatenNode.StammdatenRowCollection.FieldMap := Value;
end;

function TEventProps.GetColorMode: string;
begin
  case BO.EventNode.ColorMode of
    ColorMode_Fleet: result := 'Fleet';
    ColorMode_None: result := 'None';
  else
    result := 'Normal';
  end;
end;

procedure TEventProps.SetColorMode(const Value: string);
begin
  if Value = 'Fleet' then
    BO.EventNode.ColorMode := ColorMode_Fleet
  else if Value = 'None' then
    BO.EventNode.ColorMode := ColorMode_None
  else
    BO.EventNode.ColorMode := ColorMode_Error
end;

function TEventProps.GetRaceLayout: string;
begin
  if BO.EventNode.ShowPoints = Layout_Finish then
    result := 'Finish'
  else
    result := 'Points';
end;

procedure TEventProps.SetRaceLayout(const Value: string);
begin
  if Value = 'Finish' then
    BO.EventNode.ShowPoints := Layout_Finish
  else
    BO.EventNode.ShowPoints := Layout_Points;
end;

function TEventProps.GetWantDiffColumns: Boolean;
begin
  result := BO.EventBO.WantDiffCols;
end;

procedure TEventProps.SetWantDiffColumns(const Value: Boolean);
begin
  BO.EventBO.WantDiffCols := Value;
end;

function TEventProps.GetShowPLZColumn: Boolean;
begin
  result := BO.EventNode.ShowPLZColumn;
end;

procedure TEventProps.SetShowPLZColumn(const Value: Boolean);
begin
  BO.EventNode.ShowPLZColumn := Value;
end;

function TEventProps.GetShowPosRColumn: Boolean;
begin
  result := BO.EventNode.ShowPosRColumn;
end;

procedure TEventProps.SetShowPosRColumn(const Value: Boolean);
begin
  BO.EventNode.ShowPosRColumn := Value;
end;

function TEventProps.GetUseCompactFormat: Boolean;
begin
  result := BO.UseCompactFormat;
end;

procedure TEventProps.SetUseCompactFormat(const Value: Boolean);
begin
  BO.UseCompactFormat := Value;
end;

function TEventProps.GetUseFleets: Boolean;
begin
  result := BO.EventNode.UseFleets;
end;

procedure TEventProps.SetUseFleets(const Value: Boolean);
begin
  BO.EventNode.UseFleets := Value;
end;

function TEventProps.GetUseOutputFilter: Boolean;
begin
  result := BO.UseOutputFilter;
end;

procedure TEventProps.SetUseOutuptFilter(const Value: Boolean);
begin
  BO.UseOutputFilter := Value;
end;

function TEventProps.GetTargetFleetSize: Integer;
begin
  result := BO.EventNode.TargetFleetSize;
end;

procedure TEventProps.SetTargetFleetSize(const Value: Integer);
begin
  BO.EventNode.TargetFleetSize := Value;
end;

function TEventProps.GetFirstFinalRace: Integer;
begin
  result := BO.EventNode.FirstFinalRace;
end;

procedure TEventProps.SetFirstFinalRace(const Value: Integer);
begin
  BO.EventNode.FirstFinalRace := Value;
end;

function TEventProps.GetNameFieldCount: string;
begin
  result := IntToStr(BO.EventBO.NameFieldCount);
end;

procedure TEventProps.SetNameFieldCount(const Value: string);
var
  ebo: TEventBO;
begin
  ebo := BO.EventBO;
  ebo.NameFieldCount := StrToIntDef(Value, ebo.NameFieldCount);
end;

function TEventProps.GetNameFieldOrder: string;
begin
  result := BO.EventBO.NameFieldOrder;
end;

procedure TEventProps.SetNameFieldOrder(const Value: string);
var
  ebo: TEventBO;
begin
  ebo := BO.EventBO;
  ebo.NameFieldOrder := Value;
end;

function TEventProps.GetNameSchema: string;
begin
  result := BO.StammdatenNode.NameSchema;
end;

procedure TEventProps.SetNameSchema(const Value: string);
begin
  BO.StammdatenNode.NameSchema := Value;
end;

function TEventProps.GetFieldCaptions: string;
begin
  result := BO.StammdatenNode.StammdatenRowCollection.FieldCaptions;
end;

procedure TEventProps.SetFieldCaptions(const Value: string);
begin
  BO.StammdatenNode.StammdatenRowCollection.FieldCaptions := Value;
end;

function TEventProps.GetFieldCount: string;
begin
  result := IntToStr(BO.StammdatenNode.StammdatenRowCollection.FieldCount);
end;

procedure TEventProps.SetFieldCount(const Value: string);
var
  cl: TStammdatenRowCollection;
begin
  cl := BO.StammdatenNode.StammdatenRowCollection;
  cl.FieldCount := StrToIntDef(Value, cl.FieldCount);
end;

procedure TEventProps.InspectorOnLoad(Sender: TObject);
var
  cl: TNameValueRowCollection;
  cr: TNameValueRowCollectionItem;
begin
  if not (Sender is TNameValueRowCollection) then
    exit;

  cl := TNameValueRowCollection(Sender);

  cr := cl.Add;
  cr.Category := 'File';
  cr.FieldName := 'UseCompactFormat';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[UseCompactFormat];
  cr.Caption := 'UseCompactFormat';
  cr.Description := 'use delimited-value tables';

  cr := cl.Add;
  cr.Category := 'File';
  cr.FieldName := 'IsTimed';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[IsTimed];
  cr.Caption := 'IsTimed';
  cr.Description := 'save space if event is not timed';

  cr := cl.Add;
  cr.Category := 'Scoring';
  cr.FieldName := 'ReorderRAF';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[ReorderRAF];
  cr.Caption := 'ReorderRAF';
  cr.Description := 'if false, do not shuffle finish position';

  cr := cl.Add;
  cr.Category := 'Layout';
  cr.FieldName := 'ShowPLZColumn';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[ShowPLZColumn];
  cr.Caption := 'ShowPLZColumn';
  cr.Description := 'show index column for debugging...';

  cr := cl.Add;
  cr.Category := 'Layout';
  cr.FieldName := 'ShowPosRColumn';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[ShowPosRColumn];
  cr.Caption := 'ShowPosRColumn';
  cr.Description := 'show unique ranking';

  cr := cl.Add;
  cr.Category := 'Layout';
  cr.FieldName := 'WantDiffColumns';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[WantDiffColumns];
  cr.Caption := 'WantDiffColumns';
  cr.Description := 'want diff columns';

  cr := cl.Add;
  cr.Category := 'File';
  cr.FieldName := 'UseOutputFilter';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[UseOutputFilter];
  cr.Caption := 'UseOutputFilter';
  cr.Description := 'apply filter when saving once';

  cr := cl.Add;
  cr.Category := 'Layout';
  cr.FieldName := 'NameFieldCount';
  cr.FieldType := nvftInteger;
  cr.FieldValue := NameFieldCount;
  cr.Caption := 'NameFieldCount';
  cr.Description := 'count of name columns in event table display';

  cr := cl.Add;
  cr.Category := 'Layout';
  cr.FieldName := 'NameFieldOrder';
  cr.FieldType := nvftString;
  cr.FieldValue := NameFieldOrder;
  cr.Caption := 'NameFieldOrder';
  cr.Description := 'namefield index string';

end;

procedure TEventProps.InspectorOnSave(Sender: TObject);
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
    if cr.FieldName = 'UseCompactFormat' then
      UseCompactFormat := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'IsTimed' then
      IsTimed := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'ReorderRAF' then
      ReorderRAF := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'ShowPLZColumn' then
      ShowPLZColumn := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'ShowPosRColumn' then
      ShowPosRColumn := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'WantDiffColumns' then
      WantDiffColumns := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'UseOutputFilter' then
      UseOutputFilter := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'NameFieldCount' then
      NameFieldCount := cr.FieldValue
    else if cr.FieldName = 'NameFieldOrder' then
      NameFieldOrder := cr.FieldValue
  end;
end;

end.


