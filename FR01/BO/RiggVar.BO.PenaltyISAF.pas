unit RiggVar.BO.PenaltyISAF;

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
  RiggVar.BO.Penalty,
  RiggVar.Scoring.Penalty;

type
  TPenaltyISAF = class(TPenalty)
  protected
    FPenaltyDSQ: TISAFPenaltyDSQ;
    FPenaltyNoFinish: TISAFPenaltyNOFINISH;
    FPenaltyOther: TISAFPenaltySet;
    FDSQPending: Boolean;
    FPoints: double; //contains the Points to be awarded for RDG and MAN and STP penalties
    FPercent: Integer; //contains the percentage assigned if a SCP penalty is set
    FTimePenalty: Int64; //contains the amount (if any) of a elapsed time penalty
    FNote: string; //contains a user-entered note to be saved in accordance with this penalty
    function OtherPenaltyString(Value: TISAFPenaltyOther): string;
    function GetIsDSQPending: Boolean; override;
    function GetIsOK: Boolean; override;
    function GetIsOut: Boolean; override;
    procedure SetIsDSQPending(const Value: Boolean); override;
    function GetAsInteger: Integer; override;
    procedure SetAsInteger(const Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;  override;
    function ToString: string; override;
    function Get3LA: string;
    function FromString(Value: string): Boolean; override;
    function Parse(Value: string): Boolean; override;
    function Invert(Value: string): string;

    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property IsOK: Boolean read GetIsOK;
    property IsOut: Boolean read GetIsOut;
    property IsDSQPending: Boolean read GetIsDSQPending write SetIsDSQPending;
    property PenaltyDSQ: TISAFPenaltyDSQ read FPenaltyDSQ write FPenaltyDSQ;
    property PenaltyNoFinish: TISAFPenaltyNOFINISH read FPenaltyNoFinish write FPenaltyNoFinish;
    property PenaltyOther: TISAFPenaltySet read FPenaltyOther write FPenaltyOther;

    property Points: double read FPoints write FPoints;
    property Percent: Integer read FPercent write FPercent;
    property TimePenalty: Int64 read FTimePenalty write FTimePenalty;
    property Note: string read FNote write FNote;
  end;

  TISAFPenalty = class(TPenaltyISAF)
  public
    constructor Create; overload;
    constructor Create(NoFinishValue: Integer); overload;
    constructor Create(inPen: TISAFPenaltyNoFinish); overload;
    constructor Create(inPen: TISAFPenaltyDSQ); overload;

    function Equals(obj: TObject): Boolean; override;

    function IsDSQPenalty: Boolean;
    function IsFinishPenalty: Boolean; overload;
    class function IsFinishPenalty(pen: Integer): Boolean; overload;
    class function ParsePenalty(Value: string): Integer;

    function ParseCommaText(Value: string): Integer;

    function HasPenalty(pen: TISAFPenaltyDSQ): Boolean; overload;
    function HasPenalty(pen: TISAFPenaltyNoFinish): Boolean; overload;
    function HasPenalty(pen: TISAFPenaltyOther): Boolean; overload;
    function HasPenalty(pen: Integer): Boolean; overload;
    function IsOtherPenalty: Boolean;
  end;

implementation

var
  PenaltyFormatSettings: TFormatSettings;
  SLPenalty: TStringList; //static, see initialization section of unit

{ TPenaltyISAF }

procedure TPenaltyISAF.Assign(Source: TPersistent);
var
  o: TPenaltyISAF;
begin
  if Source = self then
    exit; //wichtig, wenn portiert nach C#
  if Source is TPenaltyISAF then
  begin
    o := Source as TPenaltyISAF;
    FPenaltyDSQ := o.FPenaltyDSQ;
    FPenaltyNoFinish := o.FPenaltyNoFinish;
    FPenaltyOther := o.FPenaltyOther;
    FDSQPending := o.FDSQPending;
    //
    FPoints := o.FPoints;
    FPercent := o.FPercent;
    FTimePenalty := o.FTimePenalty;
    FNote := o.FNote;
  end
  else
    inherited Assign(Source);
end;

procedure TPenaltyISAF.Clear;
begin
  FPenaltyDsQ := NoDSQ;
  FPenaltyNoFinish := NoFinishBlank;
  FPenaltyOther := [];
  FDSQPending := False;

  FPoints := 0;
  FPercent := 0;
  FTimePenalty := 0;
  FNote := '';
end;

procedure TPenaltyISAF.SetIsDSQPending(const Value: Boolean);
begin
  FDSQPending := Value;
end;

function TPenaltyISAF.GetIsDSQPending: Boolean;
begin
  result := FDSQPending;
end;

function TPenaltyISAF.GetIsOK: Boolean;
begin
  result := (FPenaltyDSQ = NoDSQ)
    and (FPenaltyNoFinish = NoFinishBlank)
    and (FPenaltyOther = []);
    //and (FDSQPending = False);
end;

function TPenaltyISAF.GetIsOut: Boolean;
begin
  result := not IsOK;
end;

procedure TPenaltyISAF.SetAsInteger(const Value: Integer);
var
  i: Integer;
begin
  Clear;
  if Value = ISAF_NOP then
    Exit;

  i := Value and ISAF_DSQ_MASK;
  case i of
    ISAF_DSQ: FPenaltyDSQ := DSQ;
    ISAF_DNE: FPenaltyDSQ := DNE;
    ISAF_RAF: FPenaltyDSQ := RAF;
    ISAF_OCS: FPenaltyDSQ := OCS;
    ISAF_BFD: FPenaltyDSQ := BFD;
    ISAF_UFD: FPenaltyDSQ := UFD;
    ISAF_DGM: FPenaltyDSQ := DGM;
  end;

  i := Value and ISAF_NOF_MASK;
  case i of
    ISAF_TLE: FPenaltyNoFinish := TLE;
    ISAF_DNF: FPenaltyNoFinish := DNF;
    ISAF_DNS: FPenaltyNoFinish := DNS;
    ISAF_DNC: FPenaltyNoFinish := DNC;
  else
    FPenaltyNoFinish := NoFinishBlank;
  end;

  i := Value and ISAF_OTH_MASK;
  if i and ISAF_STP = ISAF_STP then Include(FPenaltyOther, STP);
  if i and ISAF_TIM = ISAF_TIM then Include(FPenaltyOther, TIM);
  if i and ISAF_ZFP = ISAF_ZFP then Include(FPenaltyOther, ZFP);
  if i and ISAF_AVG = ISAF_AVG then Include(FPenaltyOther, AVG);
  if i and ISAF_SCP = ISAF_SCP then Include(FPenaltyOther, SCP);
  if i and ISAF_RDG = ISAF_RDG then Include(FPenaltyOther, RDG);
  if i and ISAF_MAN = ISAF_MAN then Include(FPenaltyOther, MAN);
  if i and ISAF_CNF = ISAF_CNF then Include(FPenaltyOther, CNF);
  if i and ISAF_TMP = ISAF_TMP then Include(FPenaltyOther, TMP);
  if i and ISAF_DPI = ISAF_DPI then Include(FPenaltyOther, DPI);
end;

function TPenaltyISAF.GetAsInteger: Integer;
var
  other: TISAFPenaltyOther;
begin
  result := 0;
  case self.FPenaltyDSQ of
    DSQ: result := result or ISAF_DSQ;
    DNE: result := result or ISAF_DNE;
    RAF: result := result or ISAF_RAF;
    OCS: result := result or ISAF_OCS;
    BFD: result := result or ISAF_BFD;
    UFD: result := result or ISAF_UFD;
    DGM: result := result or ISAF_DGM;
  end;
  case self.FPenaltyNoFinish of
    TLE: result := result or ISAF_TLE;
    DNF: result := result or ISAF_DNF;
    DNS: result := result or ISAF_DNS;
    DNC: result := result or ISAF_DNC;
    NoFinishBlank: result := result;
  end;
  for other := Low(TISAFPenaltyOther) to High(TISAFPenaltyOther) do
    if (other in FPenaltyOther) then
      case other of
        STP: result := result or ISAF_STP;
        TIM: result := result or ISAF_TIM;
        ZFP: result := result or ISAF_ZFP;
        AVG: result := result or ISAF_AVG;
        SCP: result := result or ISAF_SCP;
        RDG: result := result or ISAF_RDG;
        MAN: result := result or ISAF_MAN;
        CNF: result := result or ISAF_CNF;
        TMP: result := result or ISAF_TMP;
        DPI: result := result or ISAF_DPI;
      end;
end;

function TPenaltyISAF.ToString: string;
var
  i: TISAFPenaltyOther;
begin
  SLPenalty.Clear;
  result := '';
  if FPenaltyDSQ <> NoDSQ then
    SLPenalty.Add(PenaltyDSQStrings[FPenaltyDSQ]);
  if FPenaltyNoFinish <> NoFinishBlank then
    SLPenalty.Add(PenaltyNoFinishStrings[FPenaltyNoFinish]);
  for i := Low(TISAFPenaltyOther) to High(TISAFPenaltyOther) do
  begin
    if (i in FPenaltyOther) then
      SLPenalty.Add(OtherPenaltyString(i));
  end;
  result := SLPenalty.DelimitedText;
end;

function TPenaltyISAF.Get3LA: string;
var
  i: TISAFPenaltyOther;
begin
  SLPenalty.Clear;
  result := '';
  if FPenaltyDSQ <> NoDSQ then
    SLPenalty.Add(PenaltyDSQStrings[FPenaltyDSQ]);
  if FPenaltyNoFinish <> NoFinishBlank then
    SLPenalty.Add(PenaltyNoFinishStrings[FPenaltyNoFinish]);
  for i := Low(TISAFPenaltyOther) to High(TISAFPenaltyOther) do
  begin
    if (i in FPenaltyOther) then
      SLPenalty.Add(PenaltyOtherStrings[i]);
  end;
  result := SLPenalty.DelimitedText;
end;

function TPenaltyISAF.OtherPenaltyString(Value: TISAFPenaltyOther): string;
var
  s: string;
  showPts: Boolean;
begin
  showPts := true;
  s := PenaltyOtherStrings[Value];
  if (Value = STP) and showPts then
    s := s + '/' + FormatFloat('0.0#', Points, PenaltyFormatSettings)
  else if (Value = MAN) and showPts then
    s := s + '/' + FormatFloat('0.0#', Points, PenaltyFormatSettings)
  else if (Value = RDG) and showPts then
    s := s + '/' + FormatFloat('0.0#', Points, PenaltyFormatSettings)
  else if (Value = DPI) and showPts then
    s := s + '/' + FormatFloat('0.0#', Points, PenaltyFormatSettings)
  else if (Value = TIM) and showPts then
    s := s + '/' + IntToStr(TimePenalty) //SailTime.ToString(TimePenalty)
  else if (Value = TMP) and showPts then
    s := s + '/' + IntToStr(Percent) + '%'
  else if (Value = SCP) and showPts then
    s := s + '/' + IntToStr(Percent) + '%';
  result := s;
end;

function TPenaltyISAF.FromString(Value: string): Boolean;
var
  i: Integer;
  s: string;
begin
  result := True;
  s := StringReplace(Value, '"', '#', [rfReplaceAll]);
  SLPenalty.DelimitedText := s;
  for i := 0 to SLPenalty.Count-1 do
    result := result and Parse(SLPenalty[i]);
end;

function TPenaltyISAF.Invert(Value: string): string;
var
  pen: string;
begin
  result := 'ok';
  pen := LowerCase(Value);
  if pen = '' then exit;

  if Value[1] = '-' then
  begin
    if pen = '-dsq' then result := PenaltyDSQStrings[FPenaltyDSQ]
    else if pen = '-f' then result := PenaltyNoFinishStrings[FPenaltyNoFinish]

    else if pen = '-stp' then result := OtherPenaltyString(STP)
    else if pen = '-tim' then result := OtherPenaltyString(TIM)
    else if pen = '-zfp' then result := OtherPenaltyString(ZFP)
    else if pen = '-avg' then result := OtherPenaltyString(AVG)
    else if pen = '-scp' then result := OtherPenaltyString(SCP)
    else if pen = '-rdg' then result := OtherPenaltyString(RDG)
    else if pen = '-man' then result := OtherPenaltyString(MAN)
    else if pen = '-cnf' then result := OtherPenaltyString(CNF)
    else if pen = '-tmp' then result := OtherPenaltyString(TMP)
    else if pen = '-dpi' then result := OtherPenaltyString(DPI)
  end

  else
  begin
    pen := Copy(pen, 1, 3);

    { disqualification penalty enum }
    if pen = 'dsq' then result := '-dsq'
    else if pen = 'dne' then result := '-dsq'
    else if pen = 'raf' then result := '-dsq'
    else if pen = 'ocs' then result := '-dsq'
    else if pen = 'bfd' then result := '-dsq'
    else if pen = 'ufd' then result := '-dsq'
    else if pen = 'dgm' then result := '-dsq'

    { nofinish penalty enum }
    else if pen = 'tle' then result := '-f'
    else if pen = 'dnf' then result := '-f'
    else if pen = 'dns' then result := '-f'
    else if pen = 'dnc' then result := '-f'

    { other penalties set }
    else if pen = 'stp' then result := '-stp'
    else if pen = 'tim' then result := '-tim'
    else if pen = 'zfp' then result := '-zfp'
    else if pen = 'avg' then result := '-avg'
    else if pen = 'scp' then result := '-scp'
    else if pen = 'rdg' then result := '-rdg'
    else if pen = 'man' then result := '-man'
    else if pen = 'cnf' then result := '-cnf'
    else if pen = 'tmp' then result := '-tmp'
    else if pen = 'dpi' then result := '-dpi'
  end
end;

function TPenaltyISAF.Parse(Value: string): Boolean;
var
  s, pen, val: string;
  i: Integer;
begin
  result := True;
  s := LowerCase(Value);
  pen := s;

  i := Pos('/', s);
  if i > 0 then
  begin
    pen := Trim(Copy(s, 1, i-1));
    val := Trim(Copy(s, i+1, Length(s)));
  end;

  if pen = '' then exit

  else if pen = 'ok' then Clear

  else if pen = '*' then FDSQPending := True

  { disqualification penalty enum }
  else if pen = '-dsq' then FPenaltyDsQ := NoDSQ
  else if pen = 'dsq' then FPenaltyDsQ := DSQ
  else if pen = 'dne' then FPenaltyDsQ := DNE
  else if pen = 'raf' then FPenaltyDsQ := RAF
  else if pen = 'ocs' then FPenaltyDsQ := OCS
  else if pen = 'bfd' then FPenaltyDsQ := BFD
  else if pen = 'ufd' then FPenaltyDsQ := UFD
  else if pen = 'dgm' then FPenaltyDsQ := DGM

  { nofinish penalty enum }
  else if pen = '-f' then FPenaltyNoFinish := NoFinishBlank
  else if pen = 'tle' then FPenaltyNoFinish := TLE
  else if pen = 'dnf' then FPenaltyNoFinish := DNF
  else if pen = 'dns' then FPenaltyNoFinish := DNS
  else if pen = 'dnc' then FPenaltyNoFinish := DNC

  { other penalties set, for some see further below... }
  //else if pen = 'tim' then Include(FPenaltyOther, TIM)
  else if pen = 'zfp' then Include(FPenaltyOther, ZFP)
  else if pen = 'avg' then Include(FPenaltyOther, AVG)
  //else if pen = 'scp' then Include(FPenaltyOther, SCP)
  //else if pen = 'rdg' then Include(FPenaltyOther, RDG)
  //else if pen = 'man' then Include(FPenaltyOther, MAN)
  else if pen = 'cnf' then Include(FPenaltyOther, CNF)
  //else if pen = 'tmp' then Include(FPenaltyOther, TMP)
  //else if pen = 'dpi' then Include(FPenaltyOther, DPI)

  else if pen = '-stp' then Exclude(FPenaltyOther, STP)
  else if pen = '-tim' then Exclude(FPenaltyOther, TIM)
  else if pen = '-zfp' then Exclude(FPenaltyOther, ZFP)
  else if pen = '-avg' then Exclude(FPenaltyOther, AVG)
  else if pen = '-scp' then Exclude(FPenaltyOther, SCP)
  else if pen = '-rdg' then Exclude(FPenaltyOther, RDG)
  else if pen = '-man' then Exclude(FPenaltyOther, MAN)
  else if pen = '-cnf' then Exclude(FPenaltyOther, CNF)
  else if pen = '-tmp' then Exclude(FPenaltyOther, TMP)
  else if pen = '-dpi' then Exclude(FPenaltyOther, DPI)

  //all of the rest should have <pen>/<number>

  else if (val <> '') and  (val[Length(val)] = '%') then
  begin
    Include(FPenaltyOther, SCP);
    Percent := StrToIntDef(Copy(val, 1, Length(pen)-1), 0)
  end

  else if pen[1] = 'p' then
  begin
    Include(FPenaltyOther, SCP);
    Percent := StrToIntDef(Copy(pen, 2, Length(pen)), 0)
  end

  else if pen = 'tim' then
  begin
    Include(FPenaltyOther, TIM);
    TimePenalty := StrToIntDef(val, 0); // SailTime.ForceToLong(val);
  end

  else if (pen = 'rdg') or (pen = 'rdr') or (pen = 'man') or (pen = 'dpi') or (pen = 'stp')then
  begin
    if Copy(pen, 1, 3) = 'stp' then
      Include(FPenaltyOther, STP)
    else if Copy(pen, 1, 3) = 'man' then
      Include(FPenaltyOther, MAN)
    else if Copy(pen, 1, 3) = 'dpi' then
      Include(FPenaltyOther, DPI)
    else
      Include(FPenaltyOther, RDG);
    //assume is form "MAN/<pts>"
    val := StringReplace(val, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
    Points := StrToFloatDef(val, 0);
  end

  else if (pen = 'tmp') or (pen = 'scp') or (pen = 'pct') then
  begin
    if Copy(pen, 1, 3) = 'tmp' then
      Include(FPenaltyOther, TMP)
    else
      Include(FPenaltyOther, SCP);
    //assume is form "MAN/<pts>"
    Percent := StrToIntDef(val, 0);
  end

  else result := False;
end;

{ TISAFPenalty }

constructor TISAFPenalty.Create;
begin
  Create(0);
end;

constructor TISAFPenalty.Create(NoFinishValue: Integer);
begin
  inherited Create;
  AsInteger := NoFinishValue;
end;

constructor TISAFPenalty.Create(inPen: TISAFPenaltyNoFinish);
begin
  inherited Create;
  PenaltyNoFinish := inPen;
end;

constructor TISAFPenalty.Create(inPen: TISAFPenaltyDSQ);
begin
  inherited Create;
  PenaltyDSQ := inPen;
end;

function TISAFPenalty.HasPenalty(pen: TISAFPenaltyOther): Boolean;
begin
  result := pen in self.FPenaltyOther;
end;

function TISAFPenalty.HasPenalty(pen: TISAFPenaltyDSQ): Boolean;
begin
  result := FPenaltyDSQ = pen;
end;

function TISAFPenalty.HasPenalty(pen: TISAFPenaltyNoFinish): Boolean;
begin
  result := FPenaltyNoFinish = pen;
end;

function TISAFPenalty.HasPenalty(pen: Integer): Boolean;
begin
  result := AsInteger = pen;
end;

function TISAFPenalty.IsOtherPenalty: Boolean;
begin
	result := not (FPenaltyOther = []);
end;

function TISAFPenalty.IsDSQPenalty: Boolean;
begin
  result := PenaltyDSQ <> NoDSQ;
end;

function TISAFPenalty.IsFinishPenalty: Boolean;
begin
  result := AsInteger and ISAF_NOF_MASK <> 0;
end;

class function TISAFPenalty.IsFinishPenalty(pen: Integer): Boolean;
begin
  result := pen and ISAF_NOF_MASK <> 0; //see TFinish.xmlRead
end;

function TISAFPenalty.ParseCommaText(Value: string): Integer;
var
  SL: TStringList;
  i: Integer;
  s: string;
  hold: Integer;
begin
  //parse Value, including Comma separated List of Penalty-Values
  hold := AsInteger;
  try
    Clear;
    if Pos(',', Value) > 0 then
    begin
      SL := TStringList.Create;
      SL.CommaText := Value;
      for i := 0 to SL.Count-1 do
      begin
        s := SL[i];
        Parse(s);
      end;
      SL.Free;
    end
    else
      Parse(Value);
    result := AsInteger;
  except
    AsInteger := hold;
    result := ISAF_NOF;
  end;
end;

class function TISAFPenalty.ParsePenalty(Value: string): Integer;
var
  temp: TISAFPenalty;
begin
  temp := TISAFPenalty.Create;
  try
    result := temp.ParseCommaText(Value);
  finally
    temp.Free;
  end;
end;

function TISAFPenalty.Equals(obj: TObject): Boolean;
var
  that: TISAFPenalty;
begin
  result := True;
  if (self = obj) then
    result := true
  else if obj is TISAFPenalty then
  try
    that := TISAFPenalty(obj);
    if (self.AsInteger <> that.AsInteger) then
      result := false
    else if (self.FPercent <> that.FPercent) then
      result := false
    else if (self.FPoints <> that.FPoints) then
      result := false
    else
      result := true;
  except
    result := false;
  end;
end;

initialization
  SLPenalty := TStringList.Create;
  SLPenalty.QuoteChar := '#';
  PenaltyFormatSettings.DecimalSeparator := '.';
finalization
  SLPenalty.Free;

end.

