unit RiggVar.Scoring.Domain;

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
  RiggVar.Grid.ColBase,
  RiggVar.Scoring.Base,
  RiggVar.Scoring.Penalty,
  RiggVar.Scoring.Time,
  RiggVar.Scoring.Utils,
  RiggVar.BO.PenaltyISAF;

type
  TEntry = class;
  TEntryList = class;

  TRace = class;
  TRaceList = class;

  TFinishPosition = class;
  TFinish = class;
  TFinishList = class;

  TRegatta = class;

  TEntry = class(TBaseObject)
  protected
    FID: Integer;
    function GetID: string;
  public
    SailID: Integer;
    class var NextEntryID: Integer;
    constructor Create;
    destructor Destroy; override;

    function ToString: string; override;
    function CompareTo(obj: TBaseObject): Integer; override;

    class function CompareSailID(left, right: TEntry): Integer;

    property ID: string read GetID;
    property EntryID: Integer read FID;
  end;

  TEntryList = class(TBaseList)
  private
    function GetEntry(i: Integer): TEntry;
    procedure SetEntry(i: Integer; const Value: TEntry);
  public
    constructor Create(aOwnsObjects: Boolean);
    function GetEntryByID(id: Integer): TEntry;
    function CloneEntries: TEntryList;
    property Entry[i: Integer]: TEntry read GetEntry write SetEntry;
  end;

  TRace = class(TBaseObject)
  protected
    FID: Integer;
    FRegatta: TRegatta;
    FFinishList: TFinishList;
    function CompareRaces(left, right: TBaseObject): Integer;
  public
    NameID: Integer; //starts with 1 for new series, counts up
    IsRacing: Boolean;
    HasFleets: Boolean;
    TargetFleetSize: Integer;
    IsFinalRace: Boolean;
    class var NextRaceID: Integer;
    constructor Create(inReg: TRegatta; inNameID: Integer);
    destructor Destroy; override;

    function ToString: string; override;
    function CompareTo(obj: TBaseObject): Integer; override;

    function GetFinish(e: TEntry): TFinish;

    property Regatta: TRegatta read FRegatta;
    property FinishList: TFinishList read FFinishList;
  end;

  TRaceList = class(TBaseList)
  private
    function GetRace(i: Integer): TRace;
    procedure SetRace(i: Integer; const Value: TRace);
  public
    constructor Create(aOwnsObjects: Boolean);
    property Race[i: Integer]: TRace read GetRace write SetRace;
  end;

  TFinishPosition = class(TBaseObject)
  private
    FPosition: Integer;
    procedure SetPosition(const Value: Integer);
  public
    constructor Create; overload;
    constructor Create(inPen: Integer); overload;

    function CompareTo(obj: TBaseObject): Integer; override;
    function ToString: string; override;
    class function ParseString(value: string): Integer;

    function IsFinisher: Boolean;
    function IsNoFinish: Boolean;
    function IsValidFinish: Boolean; overload;
    class function IsValidFinish(order: Integer): Boolean; overload;
    function IntValue: Integer;
    property Position: Integer read FPosition write SetPosition;
  end;

  TFinish = class(TBaseObject)
  protected
    FEntry: TEntry;
    FRace: TRace;
    FPosition: TFinishPosition;
    FPenalty: TISAFPenalty;
    FFinishTime: Int64;
    procedure SetEntry(const Value: TEntry);
    procedure SetRace(const Value: TRace);
    function GetElapsedTime: Int64;
    function GetCorrectedTime: Int64;
  public
    Fleet: Integer;
    IsRacing: Boolean;
    constructor Create; overload;
    constructor Create(inRace: TRace; inEntry: TEntry); overload;
    constructor Create(inRace: TRace; inEntry: TEntry;
      inTime: Int64; inOrder: TFinishPosition; inPenalty: TISAFPenalty); overload;

    destructor Destroy; override;

    function ToString: string; override;

    function hasPenalty: Boolean;

    property Entry: TEntry read FEntry write SetEntry;
    property Race: TRace read FRace write SetRace;

    property FinishPosition: TFinishPosition read FPosition;
    property Penalty: TISAFPenalty read FPenalty;

    property ElapsedTime: Int64 read GetElapsedTime;
    property CorrectedTime: Int64 read GetCorrectedTime;
  end;

  TFinishList = class(TBaseList)
  private
    function GetNumberOfFinishers: Integer;
    function GetFinish(i: Integer): TFinish;
  public
    constructor Create(aOwnsObjects: Boolean);

    function findEntry(e: TEntry): TFinish; virtual;

    property NumberOfFinishers: Integer read GetNumberOfFinishers;
    property Finish[i: Integer]: TFinish read GetFinish;
  end;

  TRegatta = class(TBaseObject)
  protected
    FEntries: TEntryList;
    FRaces: TRaceList;
    FScoringManager: IScoringManager;
  public
    IsInFinalPhase: Boolean;
    constructor Create(inScoringManager: IScoringManager);
    destructor Destroy; override;

    procedure ScoreRegatta;

    property Entries: TEntryList read FEntries;
    property Races: TRaceList read FRaces;
    property ScoringManager: IScoringManager read FScoringManager;
  end;

implementation

{ TEntry }

constructor TEntry.Create;
begin
  inherited Create;
  Inc(NextEntryID);
  FID := NextEntryID;
end;

destructor TEntry.Destroy;
begin
  inherited Destroy;
end;

function TEntry.GetID: string;
begin
  result := IntToStr(FID);
end;

function TEntry.ToString: string;
begin
  result := IntToStr(SailID);
end;

function TEntry.CompareTo(obj: TBaseObject): Integer;
begin
  if (Obj = nil) then
    result := 1
  else
    result := CompareSailID(self, Obj as TEntry);
end;

class function TEntry.CompareSailID(left, right: TEntry): Integer;
begin
  if (left = nil) and (right = nil) then
    result := 0
  else if (left = nil) then
    result := -1
  else if (right = nil) then
    result := 1
  else if left.SailID = right.SailID then
    result := 0
  else if left.SailID < right.SailID then
    result := -1
  else
    result := 1
end;

{ TEntryList }

function TEntryList.GetEntry(i: Integer): TEntry;
begin
  result := TypedItem[i] as TEntry;
end;

function TEntryList.GetEntryByID(id: Integer): TEntry;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count-1 do
  begin
    if Entry[i].FID = id then
    begin
      result := Entry[i];
      break;
    end;
  end;
end;

procedure TEntryList.SetEntry(i: Integer; const Value: TEntry);
begin
  self.TypedItem[i] := Value;
end;

constructor TEntryList.Create(aOwnsObjects: Boolean);
begin
  inherited Create(TEntry, aOwnsObjects);
end;

function TEntryList.CloneEntries: TEntryList;
var
  i: Integer;
begin
  result := TEntryList.Create(False);
  for i := 0 to Count-1 do
    result.Add(Entry[i]);
end;

{ TRace }

constructor TRace.Create(inReg: TRegatta; inNameID: Integer);
begin
  inherited Create;
  Inc(NextRaceID);
  FID := NextRaceID;
  FRegatta := inReg;
  NameID := inNameID;
  FFinishList := TFinishList.Create(true);
end;

destructor TRace.Destroy;
begin
  FFinishList.Free;
  inherited;
end;

function TRace.GetFinish(e: TEntry): TFinish;
begin
  result := nil;
  if not IsRacing then
    exit;
  result := FFinishList.FindEntry(e);
  if not Assigned(result) then
  begin
    result := TFinish.Create(self, e);
    result.Penalty.PenaltyNoFinish := NoFinishBlank;
  end;
end;

function TRace.ToString: string;
begin
  result := 'R' + IntToStr(NameID);
end;

function TRace.CompareTo(obj: TBaseObject): Integer;
var
  that: TRace;
begin
  if (not (obj is TRace)) then
  begin
    result := 1;
    exit;
  end;
  if (self.Equals(obj)) then
  begin
    result := 0;
    exit;
  end;
  that := TRace(obj);

  if self.FID < that.FID then
    result := -1
  else if self.FID > that.FID then
    result := 1
  else
    result := 0;
end;

function TRace.CompareRaces(left, right: TBaseObject): Integer;
var
  dLeft, dRight: TRace;
begin
  if (left = nil) and (right = nil) then
  begin
    result := 0;
    exit;
  end;
  if (left = nil)then
  begin
    result := -1;
    exit;
  end;
  if (right = nil) then
  begin
    result := 1;
    exit;
  end;

  dLeft := TRace(left);
  dRight := TRace(right);

  if (dLeft.IsRacing) then
    result := -1
  else if (dRight.IsRacing) then
    result := 1
  else
    result := 0;
end;

{ TRaceList }

constructor TRaceList.Create(aOwnsObjects: Boolean);
begin
  inherited Create(TRace, aOwnsObjects);
end;

function TRaceList.GetRace(i: Integer): TRace;
begin
  result := TypedItem[i] as TRace;
end;

procedure TRaceList.SetRace(i: Integer; const Value: TRace);
begin
  self.TypedItem[i] := Value;
end;

{ TFinishPosition }

constructor TFinishPosition.Create;
begin
  Create(0);
end;

constructor TFinishPosition.Create(inPen: Integer);
begin
  inherited Create;
  Position := inPen; //--> SetFinishPosition
end;

procedure TFinishPosition.SetPosition(const Value: Integer);
begin
  if Value and ISAF_NOF_MASK <> 0 then
  begin
    //copy (only) the non-finish-penalties
    //mask out other bits
    FPosition := Value and ISAF_NOF_MASK;
  end
  else
    FPosition := Value;
end;

function TFinishPosition.intValue: Integer;
begin
  result := FPosition;
end;

function TFinishPosition.CompareTo(obj: TBaseObject): Integer;
var
  that: TFinishPosition;
begin
  if (not (obj is TFinishPosition)) then
  begin
    result := - 1;
    exit;
  end;

  that := TFinishPosition(obj);
  if (self.FPosition < that.FPosition) then
    result := - 1
  else if (self.FPosition > that.FPosition) then
    result := 1
  else
    result := 0;
end;

class function TFinishPosition.ParseString(value: string): Integer;
begin
  try
    result := StrToIntDef(value, -1);
    if result = -1 then
      result := TISAFPenalty.parsePenalty(value);
  except
    result := ISAF_NOF;
  end;
end;

function TFinishPosition.ToString: string;
begin
  case (FPosition) of
    ISAF_NOF: result := 'nof';
    ISAF_DNC: result := 'dnc';
    ISAF_DNS: result := 'dns';
    ISAF_DNF: result := 'dnf';
    ISAF_TLE: result := 'tle';
  else
    result := IntToStr(FPosition);
  end;
end;

function TFinishPosition.IsFinisher: Boolean;
begin
  result := (FPosition > 0) and (FPosition <= ISAF_HIF);
end;

function TFinishPosition.IsNoFinish: Boolean;
begin
  result := FPosition = ISAF_NOF;
end;

function TFinishPosition.IsValidFinish: Boolean;
begin
  result := isValidFinish(FPosition);
end;

class function TFinishPosition.IsValidFinish(order: Integer): Boolean;
begin
  //true if the passed in value is not a Penalty Value
  //or explicitly assigned NoFinish
  result := order <= ISAF_HIF;
end;

{ TFinish }

constructor TFinish.Create(
  inRace: TRace;
  inEntry: TEntry;
  inTime: Int64;
  inOrder: TFinishPosition;
  inPenalty: TISAFPenalty);
begin
  inherited Create;
  Race := inRace;
  Entry := inEntry;

  FFinishTime := inTime;
  FPosition := inOrder;
  IsRacing := True;

  if Assigned(inPenalty) then
    FPenalty := inPenalty
  else
    FPenalty := TISAFPenalty.Create;

  if FPenalty.IsFinishPenalty then
    FPosition.Position := FPenalty.AsInteger;
end;

constructor TFinish.Create(inRace: TRace; inEntry: TEntry);
begin
  Create(inRace, inEntry, SailTime_NOTIME, TFinishPosition.Create(ISAF_NOF), TISAFPenalty.Create(ISAF_NOF));
end;

constructor TFinish.Create;
begin
  Create(nil, nil);
end;

destructor TFinish.Destroy;
begin
  //FRace.Free; //not owned
  //FEntry.Free; //not owned
  //FFinishTime: Int64; //not an object
  FPosition.Free;
  FPenalty.Free;
  inherited;
end;

function TFinish.GetCorrectedTime: Int64;
begin
  if (FEntry = nil) then
    result := SailTime_NOTIME
  else
    result := ElapsedTime
end;

function TFinish.GetElapsedTime: Int64;
var
  pct: Integer;
  elapsed: Int64;
  penTime: Int64;
begin
  if (fEntry = nil) then
  begin
    result := SailTime_NOTIME;
    exit;
  end;

  elapsed := FFinishTime;

  if (fPenalty.hasPenalty(TIM)) then
  begin
    penTime := FPenalty.TimePenalty;
    elapsed := elapsed + penTime;
  end
  else if (FPenalty.HasPenalty(TMP)) then
  begin
    pct := FPenalty.Percent;
    elapsed := Round (elapsed * (1 + pct) / 100);
  end;

  result := elapsed;
end;

function TFinish.hasPenalty: Boolean;
begin
	result := not fPenalty.IsOK;
end;

procedure TFinish.SetEntry(const Value: TEntry);
begin
  FEntry := Value;
end;

procedure TFinish.SetRace(const Value: TRace);
begin
  FRace := Value;
end;

function TFinish.ToString: string;
var
  sb: StringBuilder;
begin
  sb := StringBuilder.Create;
  if (fEntry = nil) then
  begin
    sb.Append('<null entry>');
    sb.Append(' @ ');
    sb.Append(SailTime.Int64ToString(fFinishTime));
  end
  else
  begin
    sb.Append(FEntry.ToString());
    sb.Append('/ ');
    if (fPosition <> nil) then
      sb.Append(FPosition.ToString());
    if (fPenalty <> nil) then
    begin
      sb.Append('[');
      sb.Append(FPenalty.ToString());
      sb.Append(']');
    end;
    sb.Append(' @ ');
    sb.Append(SailTime.Int64ToString(FFinishTime));
  end;
  result := sb.ToString();
  sb.Free;
end;

{ TFinishList }

constructor TFinishList.Create(aOwnsObjects: Boolean);
begin
  inherited Create(TFinish, aOwnsObjects);
end;

function TFinishList.FindEntry(e: TEntry): TFinish;
var
  i: Integer;
  f: TFinish;
begin
  result := nil;
  if (Count = 0) then
    exit;
  for i := 0 to Count-1 do
  begin
    f := Finish[i];
    if ((f.Entry <> nil) and (f.Entry.Equals(e))) then
    begin
      result := f;
      break;
    end;
  end;
end;

function TFinishList.GetFinish(i: Integer): TFinish;
begin
  result := TypedItem[i] as TFinish;
end;

function TFinishList.GetNumberOfFinishers: Integer;
var
  i: Integer;
  f: TFinish;
begin
  result := 0;
  for i := 0 to Count-1 do
  begin
    f := Finish[i];
    if ((f.Entry <> nil) and (f.FinishPosition.IsValidFinish())) then
      Inc(result);
  end;
end;

{ TRegatta }

procedure TRegatta.ScoreRegatta;
begin
  try
    FScoringManager.ScoreRegatta(Self);
  except
    on e: Exception do
      Utils.WriteStackTrace(e);
  end
end;

constructor TRegatta.Create(inScoringManager: IScoringManager);
begin
  inherited Create;
  FRaces := TRaceList.Create(true);
  FEntries := TEntryList.Create(true);

  FScoringManager := inScoringManager;
end;

destructor TRegatta.Destroy;
begin
  FRaces.Free;
  FEntries.Free;
  FScoringManager.Free;
  inherited;
end;

end.
