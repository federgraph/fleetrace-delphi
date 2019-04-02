unit RiggVar.BO.Time;

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
  System.UITypes,
  System.Math,
  RiggVar.BO.Penalty;

const
  cRun1 = 1;
  cRun2 = 2;
  cRun3 = 3;
  eaDSQGate = 1;
  eaStatus = 2;
  eaOTime = 3;

  TimeStatus_None = 0;  //noch keine Zeit da, kann aber noch kommen
  TimeStatus_Auto = 1; //Zeit vorhanden, zuletzt automatisch gesetzt
  TimeStatus_Man = 2; //Zeit vorhanden, zuletzt von Hand gesetzt
  TimeStatus_TimePresent = 3; //Zeit vorhanden, it is not known how
  TimeStatus_Penalty = 4; //Penaltytime wurde automatisch eingetragen

  TimeNULL = MaxInt;

  channel_QA_ST = 0;
  channel_QA_IT = 1;
  channel_QA_FT = 2;
  channel_QB_ST = 3;
  channel_QB_IT = 4;
  channel_QB_FT = 5;
  channel_QC_ST = 6;
  channel_QC_IT = 7;
  channel_QC_FT = 8;


type
  TTimeStatus = (
    tsNone,
    tsAuto,
    tsMan,
    tsTimePresent,
    tsPenalty
    );

const
  TimeStatusStrings: array[TTimeStatus] of string = (
    '',
    'Auto',
    'Man',
    'Time',
    'Pen'
    );

type
  TTimeSplit = record
  private
    FValue: Int64;
    function GetHour: string;
    function GetMin: string;
    function GetSec: string;
    function GetSubSec: string;
    function GetSign: string;
    function FormatNumber2(aNumber: Integer): string;
    function FormatNumber4(aNumber: Integer): string;
  public
    procedure Split(var minus: Boolean; var h, m, s, ss: Integer);
    procedure SplitToStrings(var sSign, sHour, sMin, sSec, sSubSec: string);
    function AsString: string;
    function AsString3: string;
    function AsString4: string;
    property Value: Int64 read FValue write FValue;
    property Hour: string read GetHour;
    property Min: string read GetMin;
    property Sec: string read GetSec;
    property SubSec: string read GetSubSec;
    property Sign: string read GetSign;
  end;

  TTimeType = Int64;

  TNTime = class(TPersistent)
  private
    FTime: TTimeType;
    FStatus: TTimeStatus;
    FDisplayPrecision: Integer;
    FPrecision: Integer;
    //
    function EnsureLeadingZero(Value: Integer): string;
    function LeadingZeros(anz: Integer; const sIn: string): string;
    function CheckTime(TimeStr: string): string;
    function ConvertTimeToStr3(TimeVal: TTimeType): string;
    function ConvertStrToTime1(TimeStr: string): TTimeType;
    function ConvertStrToTime2(TimeStr: string): TTimeType;
    //
    function GetTimeSplit: TTimeSplit;
    function GetTimePresent: Boolean;
    function GetTime: TTimeType;
    procedure SetTime(const Value: TTimeType);
    procedure SetStatus(const Value: TTimeStatus);
    function GetStatus: TTimeStatus;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    procedure SetDisplayPrecision(const Value: Integer);
  protected
    property Time: TTimeType read GetTime write SetTime;
    function GetAsInteger: Integer; virtual;
    procedure SetAsInteger(const Value: Integer); virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    function Parse(const Value: string): Boolean; virtual;
    function ToString: string; override;
    procedure UpdateQualiTimeBehind(aBestTime, aOTime: Integer);
    function IsValidTimeString(TimeStr: string): Boolean; virtual;
    function StatusAsString: string; virtual;
    property TimeSplit: TTimeSplit read GetTimeSplit;
    property TimePresent: Boolean read GetTimePresent;
    property AsString: string read GetAsString write SetAsString;
    property DisplayPrecision: Integer read FDisplayPrecision write SetDisplayPrecision;
  published
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property Status: TTimeStatus read GetStatus write SetStatus;
  end;

  TQTime = class(TNTime);

  TPTime = class(TNTime)
  protected
    function GetAsInteger: Integer; override;
    procedure SetAsInteger(const Value: Integer); override;
  public
    function ToString: string; override;
    procedure SetPenalty(PenaltyTime: Integer);
  end;

const
  Status_OK = 0;
  Status_DNS = 1;
  Status_DNF = 2;
  Status_DSQ = 3;
  Status_DSQPending = 4;

type
  TStatusEnum = (
    csOK,
    csDNS,
    csDNF,
    csDSQ,
    csDSQPending
    );

const
  StatusEnumStrings: array[TStatusEnum] of string = (
    'ok',
    'dns',
    'dnf',
    'dsq',
    '*'
    );

type
  TStatus = class(TPenalty)
  private
    FStatus: TStatusEnum;
  protected
    function GetIsDSQPending: Boolean; override;
    function GetIsOK: Boolean; override;
    function GetIsOut: Boolean; override;
    procedure SetIsDSQPending(const Value: Boolean); override;
    function GetAsInteger: Integer; override;
    procedure SetAsInteger(const Value: Integer); override;
    //
    function GetStatus: TStatusEnum;
    procedure SetStatus(const Value: TStatusEnum);
    function GetAsEnum: TStatusEnum;
    procedure SetAsEnum(const Value: TStatusEnum);
    property Status: TStatusEnum read GetStatus write SetStatus;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Parse(Value: string): Boolean; override;
    function ToString: string; override;
    function FromString(Value: string): Boolean; override;
    //
    function CompareStatus(A, B: TStatus; GateA: Integer = 0; GateB: Integer = 0): Integer;
    function IsBetter(Partner: TStatus; GateA: Integer = 0; GateB: Integer = 0): Boolean;
    function IsEqual(Partner: TStatus; GateA: Integer = 0; GateB: Integer = 0): Boolean;
    function IsWorse(Partner: TStatus; GateA: Integer = 0; GateB: Integer = 0): Boolean;
    property AsEnum: TStatusEnum read GetAsEnum write SetAsEnum;
  end;

const
  crsNone = 0;
  crsStarted = 1;
  crsResults = 2;

type
  TRunStatusEnum = (
    rsNone,
    rsStarted,
    rsResults
    );

  TRunStatus = class
  private
    FStatus: TRunStatusEnum;
    procedure SetStatus(const Value: TRunStatusEnum);
    procedure SetAsInteger(const Value: Integer);
    function GetAsInteger: Integer;
  public
    procedure Assign(Source: TRunStatus);
    class function CheckInput(Value: string): Boolean;
    class function StrToRunStatusEnum(Value: string): TRunStatusEnum;
    function Parse(Value: string): Boolean;
    function ToString: string; override;
    function ToColor(aDefault: TColor): TColor;
    property Status: TRunStatusEnum read FStatus write SetStatus;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

implementation

{ TTimeSplit }

procedure TTimeSplit.Split(var minus: Boolean; var h, m, s, ss: Integer);
var
  TimeVal: Int64;
begin
  TimeVal := FValue;
  minus := False;
  if TimeVal < 0 then
  begin
    minus := true;
    TimeVal := Abs(TimeVal);
  end;
  ss := TimeVal mod 10000;
  TimeVal := TimeVal div 10000;
  s := TimeVal mod 60;
  TimeVal := TimeVal div 60;
  m := TimeVal mod 60;
  TimeVal := TimeVal div 60;
  h := TimeVal;
end;

procedure TTimeSplit.SplitToStrings(var sSign, sHour, sMin, sSec, sSubSec: string);
var
  h, m, s, ss: Integer;
  minus: boolean;
begin
  Split(minus, h, m, s, ss);

  if minus then
    sSign := 'M'
  else
    sSign := 'P';

  sHour := FormatNumber2(h);
  sMin := FormatNumber2(m);
  sSec := FormatNumber2(s);
  sSubSec := FormatNumber2(ss div 100); //Hundertstel
end;

function TTimeSplit.AsString: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  ss := ss div 100;
  result :=
    FormatNumber2(h) + ':' +
    FormatNumber2(m) + ':' +
    FormatNumber2(s) + '.' +
    FormatNumber2(ss);
  if minus then
    result := '-' + result;
end;

function TTimeSplit.AsString3: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  ss := ss div 100;
  result :=
    FormatNumber2(h) + ':' +
    FormatNumber2(m) + ':' +
    FormatNumber2(s) + '.' +
    FormatNumber2(ss) + '0';
  if minus then
    result := '-' + result;
end;

function TTimeSplit.AsString4: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  result :=
    FormatNumber2(h) + ':' +
    FormatNumber2(m) + ':' +
    FormatNumber2(s) + '.' +
    FormatNumber4(ss);
  if minus then
    result := '-' + result;
end;

function TTimeSplit.GetHour: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  result := FormatNumber2(h);
end;

function TTimeSplit.GetMin: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  result := FormatNumber2(m);
end;

function TTimeSplit.GetSec: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  result := FormatNumber2(s);
end;

function TTimeSplit.GetSubSec: string;
var
  minus: boolean;
  h, m, s, ss: Integer;
begin
  Split(minus, h, m, s, ss);
  result := FormatNumber2(ss);
end;

function TTimeSplit.GetSign: string;
begin
  if FValue < 0 then
    result := 'M'
  else
    result := 'P';
end;

function TTimeSplit.FormatNumber2(aNumber: Integer): string;
begin
  if aNumber < 10 then
    result := '0' + IntToStr(aNumber)
  else
    result := IntToStr(aNumber);
end;

function TTimeSplit.FormatNumber4(aNumber: Integer): string;
begin
  result := Format('%.4d', [aNumber]);
end;

{ TNTime }

function TNTime.GetTime: TTimeType;
begin
  Result := FTime;
end;

procedure TNTime.SetTime(const Value: TTimeType);
begin
  if (Value <> FTime) then
  begin
    FTime := Value;
  end;
end;

function TNTime.GetStatus: TTimeStatus;
begin
  Assert(FTime >= 0, 'Zeit darf nicht negativ sein');
  if FTime < 0 then
  begin
    FTime := 0;
    FStatus := tsNone;
  end;
  result := FStatus;
end;

procedure TNTime.SetStatus(const Value: TTimeStatus);
begin
  if FStatus <> Value then
  begin
    FStatus := Value;
  end;
end;

procedure TNTime.Assign(Source: TPersistent);
var
  temp: TNTime;
begin
  if Source is TNTime then
  begin
    temp := Source as TNTime;
    AsInteger := temp.AsInteger;
  end
  else
    inherited Assign(Source);
end;

function TNTime.GetTimeSplit: TTimeSplit;
begin
  result.Value := Time;
end;

function TNTime.GetTimePresent: Boolean;
begin
  if (FStatus = tsNone) {or (FStatus = tsDNA)} then
    result := False
  else
    result := True;
end;

function TNTime.StatusAsString: string;
begin
  result := TimeStatusStrings[Status];
end;

function TNTime.ConvertTimeToStr3(TimeVal: TTimeType): string;
// Konvertiert einen numerischen Wert in einen String -> Format (-)HH:MM:SS.mm
// wobei fehlende leading Stunden/Minuten entfallen: 674326 -> '1:07.43'
// leading Null wird nicht ausgebeben
// Tausendstel und Zehntausendstel werden nicht ausgegeben
var
  hours, min, sec, msec: longint;
  help: string;
  minus: boolean;
begin
  minus := False;
  if timeval < 0 then
  begin
    minus := true;
    TimeVal := abs(timeVal);
  end;
  msec := TimeVal mod 10000;
  TimeVal := TimeVal div 10000;
  sec := TimeVal mod 60;
  TimeVal := TimeVal div 60;
  min := TimeVal mod 60;
  TimeVal := TimeVal div 60;
  hours := TimeVal;

  //fehlende leading Bestandteile nicht mit ausgeben
  help := '';
  if hours > 0 then help := help + EnsureLeadingZero(hours) + ':';
  if min + hours > 0 then help := help + EnsureLeadingZero(min) + ':';
  if sec + min + hours > 0 then
    help := help + EnsureLeadingZero(sec)
  else
    help := help + '00'; //Sekunden immer ausgeben, niemals nur mit Punkt beginnen

  //append Nachkommastellen erstmal komplett,
  //davon wird am Ende nur bis zur wanted Stelle gelesen
  help := help + '.' + LeadingZeros(4, inttostr(msec));

  //leading Null wird nicht ausgebeben
  if help[1] = '0' then help := Copy(help, 2, length(help));

  if minus then help := '-' + help;

  //Tausendstel und Zehntausendstel nicht ausgeben
  help := Copy(help, 1, length(help) - (4-FDisplayPrecision));

  Result := help;
end;

function TNTime.EnsureLeadingZero(Value: Integer): string;
// Setzt eine Null vor einstellige Zahlen
begin
  if Value < 10 then
    result := '0' + IntToStr(Value)
  else
    result := IntToStr(Value);
end;

function TNTime.LeadingZeros(anz: Integer; const sIn: string): string;
{ Fills einen String auf die length 'anz' mit leading Nullen auf }
var
  hs: string; //helpstring
  i: integer;
begin
  hs := '';
  { erstmal anz Nullen holen }
  for i := 1 to anz do
    hs := hs + '0';
  { davorsetzen }
  hs := hs + sIn;
  { und rightaligned auslesen }
  Result := Copy(hs, length(hs) - anz + 1, anz);
end;

function TNTime.CheckTime(TimeStr: string): string;
var
  dotpos: Integer;
  TimeStr2: string;
  lastdd, i: Integer;
  sNachkomma: string;
  sNachkommaChecked: string;
begin
  if TimeStr = '' then
  begin
    result := '';
    exit;
  end;
  TimeStr2 := '';
  { ersten Punkt suchen }
  dotpos := pos('.', TimeStr);
  lastdd := dotpos;
  { wurde entgegen den Regeln doch ein Komma eingegeben }
  dotpos := pos(',', TimeStr);

  if (lastdd = 0) and (dotpos = 0) then
  begin
    TimeStr := TimeStr + '.0';
    dotpos := pos('.', TimeStr);
  end;
  if (lastdd = 0) and (dotpos > 0) then
    lastdd := dotpos //es war ein Komma
  else if (lastdd > 0) then
    dotpos := lastdd; //war es ein Punkt

  { check Zeichen vor dem Komma/Punkt }
  for i := dotpos - 1 downto 1 do
  begin
    case TimeStr[i] of //check characters
      ':':
        begin
          if lastdd > 0 then //have seen point or colon already
            if lastdd - i < 3 then //2 digits
              TimeStr2 := '0' + TimeStr2; //no, add zeros
          lastdd := i; //Position speichern
        end;
      '0'..'9':
        begin //war es wenigstens eine Zahl
          TimeStr2 := TimeStr[i] + TimeStr2; //consume char
        end;
    end;
  end;
  TimeStr2 := LeadingZeros(6, TimeStr2); //append leading zeros

  { check chars after comma/point }
  sNachkommaChecked := '';
  sNachkomma := Copy(TimeStr, dotpos, length(TimeStr));
  for i := 2 to Length(sNachkomma) do
  begin
    case sNachkomma[i] of //check chars
      '0'..'9': sNachkommaChecked := sNachkommaChecked + sNachkomma[i];
    end;
  end;
  if sNachkommaChecked = '' then
    sNachkommaChecked := '0';

  result := TimeStr2 + '.' + sNachkommaChecked;
end;

function TNTime.ConvertStrToTime2(TimeStr: string): TTimeType;
// Konvertiert TimeStr in einen numerischen Wert
var
  V: TTimeType; //VorkommaTeil
  N: TTimeType; //NachkommaTeil
  Value: TTimeType; //Value
  pos1, pos2, posi: longint;
  str: string;
  i: Integer;
begin
  pos1 := pos('.', TimeStr); //Sonderzeichen vorhanden?
  pos2 := pos(',', TimeStr);
  posi := pos1 + pos2;
  if (posi > 0) then
  begin

    { Vorkommastellen V }
    str := '000000' + Copy(TimeStr, 1, posi - 1) + '00'; //fill Vorkommastellen with zeros
    str := Copy(str, length(str) - 7, 8); //letzten 8 Zeichen nehmen
    V := ConvertStrToTime1(str) * 100; //diese Konvertieren
    //V jetzt in Zehntausendstel

    { Nachkommastellen N }
    str := Copy(TimeStr, 8, length(TimeStr));
    if length(str) = 0 then str := '0';
    //Runden auf Precision
    N := Round(StrToIntDef(str, 0) * power(10, FPrecision - length(str)));
    //aber intern immer Tausendstel speichern
    for i := FPrecision to 3 do
      N := N * 10;

    Value := V + N;
  end
  else
    Value := 0;
  Result := Value;
end;

function TNTime.ConvertStrToTime1(TimeStr: string): TTimeType;
{
// Konvertiert TimeStr in einen numerischen Wert
// wurde mit 'Zeit holen' verwendet
// Liefert Zeit als Anzahl Hundertstel
// Beispiel( Eine Miute, zwei Sekunden, 3 Hundertstel; also 1:02.03
// ConvertStrToTime2 passes nur den VorkommaAnteil an diese Funktion -
// 00010200 --> 6200
}
var
  i, j, k: longint;
begin
  if TimeStr[3] = ':' then //Sonderzeichen ignorieren, wenn vorhanden
    k := 1
  else
    k := 0;
  i := StrToIntDef(Copy(TimeStr, 1, 2), 0); //Stunden
  j := i;
  i := StrToIntDef(Copy(TimeStr, 3 + k, 2), 0); //Minuten
  j := j * 60 + i;
  i := StrToIntDef(Copy(TimeStr, 5 + k * 2, 2), 0); //Sekunden
  j := j * 60 + i;
  i := StrToIntDef(Copy(TimeStr, 7 + k * 3, 2), 0); //Hundertstel
  j := j * 100 + i;
  Result := j;
end;

function TNTime.IsValidTimeString(TimeStr: string): Boolean;
var
  i: Integer;
  dotpos: Integer;
  lastcolon: Integer;
  //TimeStr2: string;
  sNachkomma: string;
  //sNachkommaChecked: string;
begin
  result := False;
  if TimeStr = '' then
    exit;

  { ersten Punkt suchen }
  dotpos := pos('.', TimeStr);
  lastcolon := dotpos;

  { wurde entgegen den Regeln doch ein Komma eingegeben }
  dotpos := pos(',', TimeStr);

  { add Punkt, falls notwendig }
  if (lastcolon = 0) and (dotpos = 0) then
  begin
    TimeStr := TimeStr + '.0';
    dotpos := pos('.', TimeStr);
  end;

  if (lastcolon = 0) and (dotpos > 0) then
    //lastcolon := dotpos //es war ein Komma
  else if (lastcolon > 0) then
    dotpos := lastcolon; //war es ein Punkt

  //Bemerkung: lastcolon und dotps enthalten jetzt den gleichen Wert
  //lastcolon ist die Position der Dezimalstelle, die 'zuletzt' gefunden wurde)
  //dotpos wird von hier ab nicht mehr changed
  //lastcolon wird auf den zuletzt gefundenen Doppelpunkt gestellt
  //von Dotpos ausgehend werden der Vorkommateil nach links geparst
  //Sekunden zuerst, dann Minuten, dann Stunden
  //der Nachkommateil wird von dotpos aus nach rechts geparst

  { check Zeichen vor dem Komma/Punkt
    dabei die Sonderzeichen entfernen,
    gegebenenfalls fill with Nullen (Sekunden, Minuten, Stunden 2-Stellig }
  //TimeStr2 := '';
  for i := dotpos - 1 downto 1 do
  begin
    case TimeStr[i] of //check chars
      ':':
        begin
          {
          if lastcolon > 0 then //gab es schon einen Punkt oder Doppelpunkt
            if lastcolon - i < 3 then //waren es 2 Zeichen
              TimeStr2 := '0' + TimeStr2; //nein, prepend zero
          lastcolon := i; //Position speichern
          }
        end;
      '0'..'9':
        begin //war es wenigstens eine Zahl
          {
          TimeStr2 := TimeStr[i] + TimeStr2; //consume char
          }
        end;
      else
      begin
        exit;
      end;
    end;
  end;
  //TimeStr2 := LeadingZeros(6, TimeStr2);

  { check Zeichen nach dem Komma/Punkt }
  //sNachkommaChecked := '';
  sNachkomma := Copy(TimeStr, dotpos, length(TimeStr));
  for i := 2 to Length(sNachkomma) do
  begin
    case sNachkomma[i] of //check chars
      '0'..'9': ; //sNachkommaChecked := sNachkommaChecked + sNachkomma[i];
      else
      begin
        exit;
      end;
    end;
  end;
  result := True;
end;

function TNTime.GetAsString: string;
begin
  result := ToString;
end;

procedure TNTime.SetAsString(const Value: string);
begin
  Parse(Value);
end;

procedure TNTime.Clear;
begin
  Status := tsNone;
  Time := 0;
end;

function TNTime.Parse(const Value: string): Boolean;
begin
  if not IsValidTimeString(Value) then
  begin
    result := False;
    Exit;
  end;
  Status := tsAuto;
  Time := ConvertStrtoTime2(CheckTime(Value));
  result := True;
end;

function TNTime.ToString: string;
begin
  if (FTime = 0) and not TimePresent then
    result := ''
  else if (FTime = 0) then
  begin
    case DisplayPrecision of
      1: result := '0.0';
      2: result := '0.00';
      3: result := '0.000';
      4: result := '0.0000'
    end;
  end
  else
    result := ConvertTimeToStr3(FTime)
end;

function TNTime.GetAsInteger: Integer;
begin
  if FStatus = tsNone then
    result := 0
  else
    result := FTime;
end;

procedure TNTime.SetAsInteger(const Value: Integer);
begin
  if Value = TimeNULL then
  begin
    Status := tsNone;
    Time := 0;
  end
  else
  begin
    Status := tsAuto;
    Time := Value;
  end;
end;

procedure TNTime.UpdateQualiTimeBehind(aBestTime, aOTime: Integer);
begin
  if aBestTime = TimeNULL then
    AsInteger := TimeNULL
  else if aOTime > 0 then
    AsInteger :=  aOTime - aBestTime
  else
    AsInteger := TimeNULL;
end;

procedure TNTime.SetDisplayPrecision(const Value: Integer);
begin
  if (DisplayPrecision > 0) and (DisplayPrecision <= 4) then
    FDisplayPrecision := Value;
end;

constructor TNTime.Create;
begin
  inherited Create;
  FDisplayPrecision := 2;
  FPrecision := 2;
end;

{ TPTime }

function TPTime.GetAsInteger: Integer;
begin
  if FStatus = tsNone then
    result := -1
  else
    result := FTime;
end;

procedure TPTime.SetAsInteger(const Value: Integer);
begin
  if (Value = TimeNULL) or (Value < 0) then
  begin
    Status := tsNone;
    Time := 0;
  end
  else
  begin
    Status := tsAuto;
    Time := Value;
  end;
end;

function TPTime.ToString: string;
begin
  if FTime < 0 then
    result := ''
  else if (FTime = 0) and not TimePresent then
    result := ''
  else if (FTime = 0) then
  begin
    case DisplayPrecision of
      1: result := '0.0';
      2: result := '0.00';
      3: result := '0.000';
      4: result := '0.0000'
    end;
  end
  else
    result := ConvertTimeToStr3(FTime)
end;

procedure TPTime.SetPenalty(PenaltyTime: Integer);
begin
  if PenaltyTime < 100 then
    PenaltyTime := 100;
  if PenaltyTime > 595900 then
    PenaltyTime := 595900;
  Status := tsPenalty;
  Time := PenaltyTime;
end;

{ TRunStatus }

procedure TRunStatus.Assign(Source: TRunStatus);
begin
  FStatus := Source.Status;
end;

function TRunStatus.GetAsInteger: Integer;
begin
  result := Ord(FStatus);
end;

function TRunStatus.Parse(Value: string): Boolean;
var
  s: string;
begin
  result := True;
  s := LowerCase(Value);
  if (s = 'none') or (s = 'n') then
    FStatus := rsNone
  else if (s = 'started') or (s = 's') then
    FStatus := rsStarted
  else if (s = 'results') or (s = 'r') then
    FStatus := rsResults
  else
    result := False;
end;

class function TRunStatus.CheckInput(Value: string): Boolean;
var
  s: string;
begin
  result := False;
  s := LowerCase(Value);
  if (s = 'none') or (s = 'n') then
    result := True
  else if (s = 'list') or (s = 'l') then
    result := True
  else if (s = 'started') or (s = 's') then
    result := True
  else if (s = 'results') or (s = 'r') then
    result := True;
end;

class function TRunStatus.StrToRunStatusEnum(Value: string): TRunStatusEnum;
var
  s: string;
begin
  result := rsNone;
  s := LowerCase(Value);
  if (s = 'none') or (s = 'n') then
    result := rsNone
  else if (s = 'started') or (s = 's') then
    result := rsStarted
  else if (s = 'results') or (s = 'r') then
    result := rsResults;
end;

procedure TRunStatus.SetAsInteger(const Value: Integer);
begin
  if (Value >= 0) and (Value < Ord(rsResults)) then
    FStatus := TRunStatusEnum(Value);
end;

procedure TRunStatus.SetStatus(const Value: TRunStatusEnum);
begin
  FStatus := Value;
end;

function TRunStatus.ToString: string;
begin
  case FStatus of
    rsNone: result := '';
    rsStarted: result := 'S';
    rsResults: result := 'R';
  else
    result := '';
  end;
end;

function TRunStatus.ToColor(aDefault: TColor): TColor;
begin
  case FStatus of
    rsNone: result := aDefault;
    rsStarted: result := TColorRec.Lime;
    rsResults: result := TColorRec.Aqua;
  else
    result := TColorRec.White;
  end;
end;

{ TStatus }

function TStatus.GetStatus: TStatusEnum;
begin
  result := FStatus;
end;

procedure TStatus.SetStatus(const Value: TStatusEnum);
begin
  if Value <> FStatus then
  begin
    FStatus := Value;
  end;
end;

procedure TStatus.Assign(Source: TPersistent);
var
  temp: TStatus;
begin
  if Source is TStatus then
  begin
    temp := Source as TStatus;
    Status := temp.AsEnum;
  end
  else
    inherited Assign(Source);
end;

procedure TStatus.Clear;
begin
  Status := csOK;
end;

function TStatus.GetAsInteger: Integer;
begin
  case Status of
    csOK: result := Status_OK;
    csDSQ: result := Status_DSQ;
    csDNF: result := Status_DNF;
    csDNS: result := Status_DNS;
    csDSQPending: result := Status_DSQPending;
  else
    result := Status_OK;
  end;
end;

procedure TStatus.SetAsInteger(const Value: Integer);
begin
  case Value of
    Status_OK: Status := csOK;
    Status_DSQ: Status := csDSQ;
    Status_DNF: Status := csDNF;
    Status_DNS: Status := csDNS;
    Status_DSQPending: Status := csDSQPending;
  end;
end;

function TStatus.Parse(Value: string): Boolean;
var
  temp: string;
begin
  temp := LowerCase(Value);
  result := True;
  if temp = 'dsq' then
    Status := csDSQ
  else if temp = 'dns' then
    Status := csDNS
  else if temp = 'dnf' then
    Status := csDNF
  else if temp = 'ok' then
    Status := csOK
  else if temp = '*' then
    Status := csDSQPending
  else
    result := False;
end;

function TStatus.ToString: string;
begin
  result := StatusEnumStrings[Status];
end;

procedure TStatus.SetIsDSQPending(const Value: Boolean);
begin
  if Value then
    Status := csDSQPending
  else
    Status := csOK;
end;

function TStatus.GetIsDSQPending: Boolean;
begin
  result := Status = csDSQPending;
end;

function TStatus.GetIsOK: Boolean;
begin
  result := (FStatus = csOK) or (FStatus = csDSQPending);
end;

function TStatus.GetIsOut: Boolean;
begin
  result := (FStatus = csDSQ) or (FStatus = csDNF) or (FStatus = csDNS);
end;

function TStatus.GetAsEnum: TStatusEnum;
begin
  result := Status;
end;

procedure TStatus.SetAsEnum(const Value: TStatusEnum);
begin
  Status := Value;
end;

function TStatus.CompareStatus(A, B: TStatus;
  GateA: Integer = 0; GateB: Integer = 0): Integer;
begin
  { beide gleich }
  if A.Status = B.Status then
  begin
    result := 0;
    if ((A.Status = csDNF) and (B.Status = csDNF))
      or (A.Status = csDSQ) and (B.Status = csDSQ)
    then
    begin
      if (GateA > GateB) then
        result := 1
      else if (GateB > GateA) then
        result := 2;
    end;
  end
  { beide ok, niemand besser }
  else if A.IsOK and B.IsOK then
    result := 0
  { A ok, B out }
  else if A.IsOK and B.IsOut then
    result := 1
  { A out, B ok }
  else if A.IsOut and B.IsOK then
    result := 2
  { beide Out aber nicht gleich }
  else { if A.IsOut and B.IsOut then }
  begin
    if A.Status = csDNF then
      result := 1
    else if B.Status = csDNF then
      result := 2
    else if A.Status = csDSQ then
      result := 1
    else if B.Status = csDSQ then
      result := 2
    else { if (A.Status = csDNS) and (B.Status = cdDNS) }
      result := 0
  end;
end;

function TStatus.FromString(Value: string): Boolean;
begin
  result := Parse(Value);
end;

function TStatus.IsBetter(Partner: TStatus;
  GateA: Integer = 0; GateB: Integer = 0): Boolean;
begin
  result := CompareStatus(Self, Partner, GateA, GateB) = 1;
end;

function TStatus.IsEqual(Partner: TStatus;
 GateA: Integer = 0; GateB: Integer = 0): Boolean;
begin
  result := CompareStatus(Self, Partner, GateA, GateB) = 0;
end;

function TStatus.IsWorse(Partner: TStatus;
 GateA: Integer = 0; GateB: Integer = 0): Boolean;
begin
  result := CompareStatus(Self, Partner, GateA, GateB) = 2;
end;

end.
