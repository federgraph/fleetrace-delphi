unit RiggVar.Scoring.Classes;

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
  System.Math,
  RiggVar.Grid.ColBase,
  RiggVar.Scoring.Domain,
  RiggVar.Scoring.Base,
  RiggVar.Scoring.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Utils;

type
  TPoints = class(TBaseObject)
  private
	FPoints: double;
	FEntry: TEntry; //not owned
    procedure SetPosition(const value: Integer);
    procedure SetPoints(const value: double);
  protected
		FPosition: Integer; //may have penalty type encoded
  public
    constructor Create(entry: TEntry; points: double; pos: Integer);

    function CompareTo(obj: TBaseObject): Integer; override;
    function Equals(obj: TObject): Boolean; override;

    property Entry: TEntry read FEntry;
    property Points: double read FPoints write SetPoints;
    property Position: Integer read FPosition write SetPosition;
  end;

  TRacePoints = class(TPoints)
  private
    FFinish: TFinish; //not owned
    FRace: TRace; //not owned
    FThrowout: boolean;
    function GetFinish: TFinish;
    function ToString1(showPts: Boolean): string;
  public
    constructor Create(
      race: TRace;
      entry: TEntry;
      points: double;
      throwout: Boolean
      ); overload;
    constructor Create(f: TFinish); overload;
    constructor Create; overload;

    function ToString: string; override;

    function IsTied(lastrp: TRacePoints): Boolean; virtual;

    property Finish: TFinish read GetFinish;
    property Race: TRace read FRace;
    property IsThrowout: Boolean read FThrowout write FThrowout;
  end;

  TSeriesPoints = class(TPoints)
  private
    FTied: Boolean;
    procedure SetTied(const value: Boolean);
  public
    constructor Create; overload;
    constructor Create(entry: TEntry); overload;
    constructor Create(entry: TEntry; points: double; pos: Integer; tied: Boolean); overload;

    function ToString: string; override;
    function CompareEntryID(other: TSeriesPoints): Integer;

    property IsTied: Boolean read FTied write SetTied;
  end;

  TRacePointsList = class(TBaseList)
  private
    FNumberOfFinishers: Integer;
    function GetRacePoints(i: Integer): TRacePoints;
    procedure SetRacePoints(i: Integer; const value: TRacePoints);
    procedure SetNumberOfFinishers(const value: Integer);
    function ComparePoints(const left, right: TBaseObject): Integer;
    function CompareRace(const left, right: TBaseObject): Integer;
    function CompareTimePosition(const left, right: TBaseObject): Integer;
  public
    constructor Create(aOwnsObjects: Boolean);
    function GetNumberWithPenalty(pen: Integer): Integer; overload;
    function GetNumberWithPenalty(pen: TISAFPenaltyDSQ): Integer; overload;
    function GetNumberWithPenalty(pen: TISAFPenaltyNoFinish): Integer; overload;
    function FindAll(entry: TEntry): TRacePointsList; overload;
    function Find(r: TRace; e: TEntry): TRacePoints;
    procedure SortRace();
    procedure SortPoints();
    procedure SortCorrectedTimePosition();
    procedure Dump;
    property RacePoints[i: Integer]: TRacePoints read GetRacePoints write SetRacePoints;
    property NumberOfFinishers: Integer read FNumberOfFinishers write SetNumberOfFinishers;
  end;

  TSeriesPointsList = class(TBaseList)
  private
    function ComparePoints(const left, right: TBaseObject): Integer;
    function GetPoints(i: Integer): TSeriesPoints;
    procedure SetPoints(i: Integer; const value: TSeriesPoints);
  public
    constructor Create(aOwnsObjects: Boolean);
    procedure InitPoints(entries: TEntryList);

    procedure SortPoints;
    function FindAll(e: TEntry): TSeriesPointsList; overload;

    property SeriesPoints[i: Integer]: TSeriesPoints read GetPoints write SetPoints;
  end;

  IScoringModel = class(TBaseObject)
  public
		function GetName: string; virtual; abstract;
		procedure ScoreRace(race: TRace; points: TRacePointsList; positionOnly: boolean);  virtual; abstract;
		procedure ScoreSeries(races: TRaceList; entries: TEntryList; points: TRacePointsList; series: TSeriesPointsList); virtual; abstract;
		procedure CalcTiebreakers(races: TRaceList; entries: TEntryList; points: TRacePointsList; series: TSeriesPointsList); virtual; abstract;
		function GetPenaltyPoints(p: TISAFPenalty; rpList: TRacePointsList; BasePts: double): double; virtual; abstract;
		procedure CalcThrowouts(racePoints: TRacePointsList); virtual; abstract;
		procedure SortSeries(seriesPoints: TSeriesPointsList); virtual; abstract;
	end;

  TScoringTiebreaker = class
  protected
    FModel: IScoringModel;
    Races: TRaceList;
    RacePoints: TRacePointsList;
    SeriesPoints: TSeriesPointsList;
  public
		constructor Create(
        model: IScoringModel;
				rlist: TRaceList;
				rpl: TRacePointsList;
				spl: TSeriesPointsList);
    procedure UseAgain(
        model: IScoringModel;
				rlist: TRaceList;
				rpl: TRacePointsList;
				spl: TSeriesPointsList);
    procedure Process;
    procedure BreakTies(tiedBunch: TEntryList); virtual;
  end;

  //stores the minimum number of races for i'th throwout.
  TThrowouts = array of Integer;

  TScoringModel = class(IScoringModel)
  private
    function GetIsLongSeries: Boolean;
  protected
    FThrowouts: TThrowouts;
		FThrowoutScheme: Integer;
		FThrowoutPerX: Integer;
		FThrowoutBestX: Integer;
		FIsLongSeries: Boolean;
		FCheckinPercent: Integer; // = 20; // Default percentage penalty for failure to check-in
    FTimeLimitPenalty: Integer;
    FFirstIs75: Boolean;
    FHasFleets: Boolean;
    FTargetFleetSize: Integer;
    procedure SetThrowoutScheme(const value: Integer);
    procedure SetThrowoutPerX(const value: Integer);
    procedure SetThrowoutBestX(const value: Integer);
    procedure SetTimeLimitPenalty(const value: Integer);
    procedure SetCheckinPercent(const value: Integer);
    procedure SetFirstIs75(const value: Boolean);
  published
  public
    IsFinalRace: Boolean;
  	TiebreakerMode: Integer;
    ReorderRAF: Boolean;

    constructor Create;

    procedure SetFixedNumberOfThrowouts(value: Integer);

    property LongSeries: Boolean read FIsLongSeries write FIsLongSeries;
    property ThrowoutScheme: Integer read FThrowoutScheme write SetThrowoutScheme;
    property ThrowoutPerX: Integer read FThrowoutPerX write SetThrowoutPerX;
    property ThrowoutBestX: Integer read FThrowoutBestX write SetThrowoutBestX;
    property TimeLimitPenalty: Integer read FTimeLimitPenalty write SetTimeLimitPenalty;
    property CheckinPercent: Integer read FCheckinPercent write SetCheckinPercent;
    property FirstIs75: Boolean read FFirstIs75 write SetFirstIs75;
    property HasFleets: Boolean read FHasFleets write FHasFleets;
    property TargetFleetSize: Integer read FTargetFleetSize write FTargetFleetSize;
    property IsLongSeries: Boolean read GetIsLongSeries;
  end;

  TScoringManager = class(IScoringManager)
  private
    procedure ScoreSeries2(divRaces: TRaceList; entries: TEntryList;
      divPointsList: TRacePointsList; posOffset: Integer);
    procedure ScoreQualiFinalSeries(divRaces: TRaceList; entries: TEntryList;
      points: TRacePointsList);
  protected
    FScoringModelType: TScoringModelType;

    FScoringModelLowPoint: TScoringModel;
    FScoringModelBonusPoint: TScoringModel;
    FScoringModelBonusPointDSV: TScoringModel;

    RacePL: TRacePointsList;
    SeriesPL: TSeriesPointsList;

    Tiebreaker: TScoringTiebreaker;

    function GetScoringModel: TScoringModel;
  public
    constructor Create;
    destructor Destroy; override;

	procedure CalcAveragePoints(divRacePoints: TRacePointsList); overload;
	procedure CalcAveragePoints(divRacePoints: TRacePointsList; throwoutsIncluded: Boolean); overload;

    procedure ScoreRegatta(ev: TObject); override;
    procedure ScoreDivision(regatta: TRegatta);
    procedure ScoreRace(race: TRace; racePoints: TRacePointsList; positionOnly: Boolean); overload;
    procedure ScoreRace(race: TRace; racePoints: TRacePointsList); overload;
    procedure ScoreSeries(divRaces: TRaceList; entries: TEntryList; divPointsList: TRacePointsList);

    property RacePointsList: TRacePointsList read RacePL;
    property SeriesPointsList: TSeriesPointsList read SeriesPL;
    property ScoringModel: TScoringModel read GetScoringModel;
  end;

implementation

uses
  RiggVar.Scoring.Bonus,
  RiggVar.Scoring.LowPoint,
  RiggVar.Scoring.BonusDSV;

{ TRacePointsList }

constructor TRacePointsList.Create(aOwnsObjects: Boolean);
begin
  inherited Create(TRacePoints, aOwnsObjects);
end;

function TRacePointsList.Find(r: TRace; e: TEntry): TRacePoints;
var
  i: Integer;
  p: TRacePoints;
begin
  result := nil;
  for i := 0 to Count-1 do
  begin
    p := self.RacePoints[i];
    if (((e = nil) or (p.Entry <> nil) and (p.Entry.Equals(e)))
      and ((r = nil) or (p.Race <> nil) and (p.Race.Equals(r))))
    then
    begin
      result := p;
      break;
    end;
  end;
end;

function TRacePointsList.FindAll(entry: TEntry): TRacePointsList;
var
  i: Integer;
  p: TRacePoints;
begin
  result := TRacePointsList.Create(false);
  for i := 0 to Count-1 do
  begin
    p := RacePoints[i];
    if (p.Entry <> nil) and (p.Entry.Equals(entry)) then
      result.Add(p);
  end;
end;

function TRacePointsList.GetNumberWithPenalty(pen: Integer): Integer;
var
  f: TFinish;
  i: Integer;
  pts: TRacePoints;
begin
  result := 0;
  for i := 0 to Count-1 do
  begin
    pts := Items[i] as TRacePoints;
    try
      f := pts.Race.GetFinish(pts.Entry);
      if f.Penalty.HasPenalty(pen) then
        Inc(result);
    except
       // trop and ignore
    end;
  end;
end;

function TRacePointsList.GetNumberWithPenalty(pen: TISAFPenaltyDSQ): Integer;
var
  f: TFinish;
  i: Integer;
  pts: TRacePoints;
begin
  result := 0;
  for i := 0 to Count-1 do
  begin
    pts := Items[i] as tRacePoints;
    try
      f := pts.Race.getFinish(pts.Entry);
      if f.Penalty.PenaltyDSQ = pen then
        Inc(result);
    except
      // trop and ignore
    end;
  end;
end;

function TRacePointsList.GetNumberWithPenalty(pen: TISAFPenaltyNoFinish): Integer;
var
  f: TFinish;
  i: Integer;
  pts: TRacePoints;
begin
  result := 0;
  for i := 0 to Count-1 do
  begin
    pts := Items[i] as tRacePoints;
    try
      f := pts.Race.getFinish(pts.Entry);
      if f.Penalty.PenaltyNoFinish = pen then
        Inc(result);
    except
     // trop and ignore
    end;
  end;
end;

function TRacePointsList.GetRacePoints(i: Integer): TRacePoints;
begin
  result := TypedItem[i] as TRacePoints;
end;

procedure TRacePointsList.SetNumberOfFinishers(const value: Integer);
begin
  FNumberOfFinishers := value;
end;

procedure TRacePointsList.SetRacePoints(i: Integer;
  const value: TRacePoints);
begin
  self.TypedItem[i] := value;
end;

procedure TRacePointsList.SortCorrectedTimePosition;
begin
  Sort(CompareTimePosition);
end;

function TRacePointsList.CompareTimePosition(const left, right: TBaseObject): Integer;
var
  pLeft, pRight: TRacePoints;
  ileft, iright: Int64;
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

  pLeft := nil;
  if left is TRacePoints then
    pLeft := left as TRacePoints;
  pRight := nil;
  if right is TRacePoints then
    pRight := TRacePoints(right);

  if (pleft = nil) and (pright = nil) then
  begin
    result := 0;
    exit;
  end;
  if (pLeft = nil)then
  begin
    result := -1;
    exit;
  end;
  if (pRight = nil) then
  begin
    result := 1;
    exit;
  end;

  if (pLeft.Finish = nil) and (pRight.Finish = nil) then
  begin
    result := 0;
    exit;
  end;
  if (pLeft.Finish = nil)then
  begin
    result := -1;
    exit;
  end;
  if (pRight.Finish = nil) then
  begin
    result := 1;
    exit;
  end;

  ileft := pLeft.Finish.CorrectedTime;
  iright := pRight.Finish.CorrectedTime;

  if (ileft <> SailTime_NOTIME) and (iright <> SailTime_NOTIME) then
  begin
    if (ileft < iright) then
    begin
      result := - 1;
      exit;
    end;
    if (ileft > iright) then
    begin
      result := 1;
      exit;
    end;
  end;

  if (pLeft.Finish.FinishPosition = nil)then
  begin
    result := -1;
    exit;
  end;
  if (pRight.Finish.FinishPosition = nil) then
  begin
    result := 1;
    exit;
  end;

  result := pLeft.Finish.FinishPosition.CompareTo(pRight.Finish.FinishPosition);
end;

procedure TRacePointsList.SortPoints;
begin
  Sort(ComparePoints);
end;

function TRacePointsList.ComparePoints(const left, right: TBaseObject): Integer;
var
  r1, r2: TRacePoints;
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

  r1 := left as TRacePoints;
  r2 := right as TRacePoints;
  result := r1.CompareTo(r2);
end;

procedure TRacePointsList.SortRace;
begin
  Sort(CompareRace);
end;

function TRacePointsList.CompareRace(const left, right: TBaseObject): Integer;
var
  r1, r2: TRacePoints;
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

  r1 := left as TRacePoints;
  r2 := right as TRacePoints;
  result := r1.Race.CompareTo(r2.Race);
end;

procedure TRacePointsList.Dump;
var
  i: Integer;
  rp: TRacePoints;
  s: string;
begin
  for i := 0 to Count-1 do
  begin
    rp := self.RacePoints[i];
    s := rp.Race.ToString + ',' + rp.Entry.ToString;
    Utils.WriteLine(s);
  end;
end;

{ TPoints }

function TPoints.CompareTo(obj: TBaseObject): Integer;
var
  that: TPoints;
begin
  if (obj = nil) then
  begin
    result := -1;
    exit;
  end;
  try
    that := TPoints(obj);
    if (self.fPoints < that.fPoints) then
      result := - 1
    else if (self.fPoints > that.fPoints) then
      result := 1
    else
      result := 0;
  except //(InvalidCastException e)
    result := - 1;
  end;
end;

constructor TPoints.Create(entry: TEntry; points: double; pos: Integer);
begin
  inherited Create;
  FEntry := entry;
  FPoints := points;
  FPosition := pos;
end;

function TPoints.Equals(obj: TObject): Boolean;
var
  that: TPoints;
begin
  result := true;

  if (self = obj) then
    result := true
  else if (not (obj is TPoints)) then
    result := false
  else
  begin
    that := TPoints(obj);
    if (self.fPoints <> that.fPoints) then
      result := false
    else if (self.fPosition <> that.fPosition) then
      result := false
    else if (not Utils.EqualsWithNull(self.fEntry, that.fEntry)) then
      result := false;
  end;
end;

procedure TPoints.SetPoints(const value: double);
begin
  if value = 1.0 then
    FPoints := value //<- debug breakpoint
  else
    FPoints := value;
end;

procedure TPoints.SetPosition(const value: Integer);
begin
  FPosition := value;
end;

{ TRacePoints }

constructor TRacePoints.Create(race: TRace; entry: TEntry;
  points: double; throwout: Boolean);
begin
  inherited Create(entry, points, 0);
  FRace := race;
  FThrowout := throwout;
  FFinish := nil;
end;

constructor TRacePoints.Create(f: TFinish);
begin
  Create(f.Race, f.Entry, NaN, false);
end;

constructor TRacePoints.Create;
begin
  Create(nil, nil, NaN, false);
end;

function TRacePoints.GetFinish: TFinish;
begin
  if (FFinish = nil) then
  begin
    if Assigned(FRace) and Assigned(Entry) then
      FFinish := FRace.getFinish(Entry);
  end;
  result := FFinish;
end;

function TRacePoints.IsTied(lastrp: TRacePoints): Boolean;
var
  f: TFinish;
begin
  f := Finish;
  result := (lastrp <> nil)
    and (f.CorrectedTime <> SailTime_NOTIME)
    and (not f.hasPenalty)
    and (not lastrp.Finish.hasPenalty)
    and (lastrp.Finish.CorrectedTime = f.CorrectedTime);
end;

function TRacePoints.ToString: string;
begin
  result := ToString1(true);
end;

function TRacePoints.ToString1(showPts: Boolean): string;
var
  didPts: Boolean;
  fin: TFinish;
  pen: TISAFPenalty;
  sb: StringBuilder;
  s: string;
  ptemp: TISAFPenalty;
begin
  fin := Finish;
  pen := finish.Penalty;
  sb := StringBuilder.Create;

  didPts := false;
  if (showPts or not fin.hasPenalty()) or (pen.isOtherPenalty()) then
  begin
    s := FormatFloat('0.00', Points);
    sb.Append(s);
    didPts := true;
  end;

  if (pen.isDsqPenalty()) then
  begin
    ptemp := TISAFPenalty.Create;
    try
      ptemp.Assign(pen);
      ptemp.PenaltyNoFinish := NoFinishBlank;
      if (didPts) then
        sb.Append('/');
      sb.Append(ptemp.ToString);
    finally
      ptemp.Free;
    end;
  end
  else if (finish.hasPenalty()) then
  begin
    if (didPts) then
      sb.Append('/');
    sb.Append(pen.ToString);
  end;

  result := sb.ToString();
  sb.Free;

  if (FThrowout) then
    result := '[' + result + ']';
end;

{ TSeriesPointsList }

constructor TSeriesPointsList.Create(aOwnsObjects: Boolean);
begin
  inherited Create(TSeriesPoints, aOwnsObjects);
end;

procedure TSeriesPointsList.InitPoints(entries: TEntryList);
var
  i: Integer;
  p: TSeriesPoints;
  e: TEntry;
begin
  Clear();
  for i := 0 to entries.Count-1 do
  begin
    e := entries.Entry[i];
    p := TSeriesPoints.Create(e);
    self.Add(p);
  end;
end;

function TSeriesPointsList.FindAll(e: TEntry): TSeriesPointsList;
var
  i: Integer;
  p: TSeriesPoints;
begin
  result := TSeriesPointsList.Create(false);
  for i := 0 to Count-1 do
  begin
    p := self.SeriesPoints[i];
    if ((p.Entry <> nil) and (p.Entry.Equals(e))) then
      result.Add(p);
  end;
end;

procedure TSeriesPointsList.SortPoints;
begin
	Sort(ComparePoints);
end;

function TSeriesPointsList.ComparePoints(const left, right: TBaseObject): Integer;
var
  spleft, spright: TSeriesPoints;
  ileft, iright: double;
begin
  if (left = nil) and (right = nil) then
  begin
    result := 0;
    exit;
  end
  else if (left = nil) then
  begin
    result := - 1;
    exit;
  end
  else if (right = nil) then
  begin
    result := 1;
    exit;
  end;

  spleft := TSeriesPoints(left);
  spright := TSeriesPoints(right);

  ileft := spleft.Points;
  iright := spright.Points;

  if (ileft < iright) then
    result := - 1
  else if (ileft > iright) then
    result := 1
  else
    result := spleft.CompareEntryID(spright);
end;

function TSeriesPointsList.GetPoints(i: Integer): TSeriesPoints;
begin
  result := TypedItem[i] as TSeriesPoints;
end;

procedure TSeriesPointsList.SetPoints(i: Integer;
  const value: TSeriesPoints);
begin
  TypedItem[i] := value;
end;

{ TSeriesPoints }

constructor TSeriesPoints.Create;
begin
  Create(nil);
end;

constructor TSeriesPoints.Create(entry: TEntry);
begin
  Create(entry, NaN, MaxInt, false)
end;

function TSeriesPoints.CompareEntryID(other: TSeriesPoints): Integer;
begin
  if (self.Entry.EntryID < other.Entry.EntryID) then
    result := -1
  else if (self.Entry.EntryID > other.Entry.EntryID) then
    result := 1
  else
    result := 0;
end;

constructor TSeriesPoints.Create(entry: TEntry;
  points: double; pos: Integer; tied: Boolean);
begin
  inherited Create(entry, points, pos);
  FTied := tied;
end;

procedure TSeriesPoints.SetTied(const value: Boolean);
begin
  FTied := value;
end;

function TSeriesPoints.ToString: string;
begin
	result := FormatFloat('0.00',Points);
	if (isTied) then
	  result := result + 'T';
end;

{ TScoringTiebreaker }

constructor TScoringTiebreaker.Create(model: IScoringModel;
  rlist: TRaceList; rpl: TRacePointsList; spl: TSeriesPointsList);
begin
  inherited Create;
  FModel := model;
  Races := rlist;
  RacePoints := rpl;
  SeriesPoints := spl;
end;

procedure TScoringTiebreaker.Process;
var
  i: Integer;
  tiedBunch: TEntryList;
  BasePoints: TSeriesPoints;
  newPoints: TSeriesPoints;
begin
  tiedBunch := TEntryList.Create(False);
  try
    BasePoints := SeriesPoints.SeriesPoints[0];

    for i := 1 to SeriesPoints.Count-1 do
    begin
      newPoints := SeriesPoints.SeriesPoints[i];

      if (BasePoints.Points = newPoints.Points) then
      begin
        // have a tie, see if starting a new group
        if (tiedBunch.Count = 0) then
        begin
          tiedBunch.Add(BasePoints.Entry);
        end;
        tiedBunch.Add(newPoints.Entry);
      end
      else
      begin
        // this one not tied, send bunch to tiebreaker resolution
        if (tiedBunch.Count > 0) then
        begin
          BreakTies(tiedBunch);
          tiedBunch.Clear();
        end;
        BasePoints := newPoints;
      end;
    end;

    // at end of loop, see if we are tied at the bottom
    if (tiedBunch.Count > 0) then
    begin
      BreakTies(tiedBunch);
    end;
  finally
    tiedBunch.Free;
  end;
end;

procedure TScoringTiebreaker.UseAgain(model: IScoringModel; rlist: TRaceList;
  rpl: TRacePointsList; spl: TSeriesPointsList);
begin
  FModel := model;
  self.Races := rlist;
  self.RacePoints := rpl;
  self.SeriesPoints := spl;
end;

procedure TScoringTiebreaker.BreakTies(tiedBunch: TEntryList);
begin
  FModel.CalcTieBreakers(Races, tiedBunch, RacePoints, SeriesPoints);
end;

{ TScoringModel}

constructor TScoringModel.Create;
begin
  inherited Create;

  { Throwouts }
  SetLength(FThrowouts, 3);
  FThrowouts[0] := 2;
	FThrowouts[1] := 0;
	FThrowouts[2] := 0;

  FThrowoutScheme := THROWOUT_BYNUMRACES;
	FTimeLimitPenalty := TLE_DNF;
  FCheckinPercent := 20;

  TiebreakerMode := TIE_RRS_DEFAULT;
  ReorderRAF := True;
end;

function TScoringModel.GetIsLongSeries: Boolean;
begin
  result := FIsLongSeries;
end;

procedure TScoringModel.SetCheckinPercent(const value: Integer);
begin
  FCheckinPercent := value;
end;

procedure TScoringModel.SetThrowoutBestX(const value: Integer);
begin
  FThrowoutBestX := value;
end;

procedure TScoringModel.SetThrowoutPerX(const value: Integer);
begin
  FThrowoutPerX := value;
end;

procedure TScoringModel.SetThrowoutScheme(const value: Integer);
begin
  FThrowoutScheme := value;
end;

procedure TScoringModel.SetFixedNumberOfThrowouts(value: Integer);
begin
  if (value > 0) and (value < 100) then
  begin
    if Length(FThrowouts) < value then
      SetLength(FThrowouts, value);
    FThrowouts[value-1] := value;
  end;
end;

procedure TScoringModel.SetFirstIs75(const value: Boolean);
begin
  FFirstIs75 := value;
end;

procedure TScoringModel.SetTimeLimitPenalty(const value: Integer);
begin
  FTimeLimitPenalty := value;
end;

{ TScoringManager }

constructor TScoringManager.Create;
begin
  inherited Create;
  RacePL := TRacePointsList.Create(true);
  SeriesPL := TSeriesPointsList.Create(true);

  FScoringModelLowPoint := TScoringLowPoint.Create;
  FScoringModelBonusPoint := TScoringBonusPoint.Create;
  FScoringModelBonusPointDSV := TScoringBonusPointDSV.Create;
end;

destructor TScoringManager.Destroy;
begin
  FScoringModelLowPoint.Free;
  FScoringModelBonusPoint.Free;
  FScoringModelBonusPointDSV.Free;

  Tiebreaker.Free;
  SeriesPL.Free;
  RacePL.Free;
  inherited;
end;

function TScoringManager.GetScoringModel: TScoringModel;
begin
  case ScoringModelType of
    ScoringModel_BonusPoint: result := FScoringModelBonusPoint;
    ScoringModel_BonusPointDSV: result := FScoringModelBonusPointDSV;
    ScoringModel_LowPoint: result := FScoringModelLowPoint;
  else
    result := FScoringModelLowPoint;
  end;
end;

procedure TScoringManager.CalcAveragePoints(divRacePoints: TRacePointsList);
begin
  CalcAveragePoints(divRacePoints, true);
end;

procedure TScoringManager.CalcAveragePoints(divRacePoints: TRacePointsList;
  throwoutsIncluded: Boolean);
var
  i, j, k: Integer;
  eWithAvg: TEntryList;
  rp: TRacePoints;
  e: TEntry;
  list: TRacePointsList;

  pts: double;
  n: double;
  hasAvg: boolean;
  tempPts: array of double;
  t: Integer;

  p: TRacePoints;
  finish: TFinish;
  davg: double;
begin
	if (TraceIsOn) then
	  Utils.WriteLine('ScoringManager: calculating average points...');

  eWithAvg := TEntryList.Create(false);
  try
    for i := 0 to divRacePoints.Count-1 do
    begin
      rp := divRacePoints.RacePoints[i];
      if rp.Finish = nil then
        Continue;
      if rp.Finish.Penalty.HasPenalty(AVG) then
      begin
        e := rp.Entry;
        if not eWithAvg.Contains(e) then
          eWithAvg.Add(e);
      end;
    end;

    for i := 0 to eWithAvg.Count-1 do
    begin
      e := eWithAvg.Entry[i];

      list := divRacePoints.FindAll(e);
      try
        pts := 0;
        n := 0;
        hasAvg := false;

        SetLength(tempPts, list.Count);
        t := 0;

        for j := 0 to list.Count-1 do
        begin
          p := list.RacePoints[j];
          finish := p.Race.GetFinish(p.Entry);

          tempPts[t] := p.Points;
          Inc(t);

          if ((not p.IsThrowout) or throwoutsIncluded)
            and Assigned(finish) and not finish.Penalty.HasPenalty(AVG)
          then
          begin
            pts := pts + p.Points;
            n := n + 1;
          end
          else if ((finish <> nil) and finish.Penalty.HasPenalty(AVG)) then
          begin
            hasAvg := true;
          end;
        end;

        if (hasAvg) then
        begin
          davg := pts / n;
          davg := Round(davg * 10); //(long) Math.Round(avg * 10);
          davg := davg / 10.0;
          for k := 0 to list.Count-1 do
          begin
            p := list.RacePoints[k];
            finish := p.Race.GetFinish(p.Entry);
            if ((finish <> nil) and finish.Penalty.hasPenalty(AVG)) then
            begin
              p.Points := davg;
            end;
          end;
        end; // loop setting average points
      finally
        list.Free;
      end;
    end; // loop thru entries
  finally
    eWithAvg.Free;
  end;
end;

procedure TScoringManager.ScoreDivision(regatta: TRegatta);
var
  i, j: Integer;
  racePoints: TRacePointsList;
  divRaces: TRaceList;
  r: TRace;
  e: TEntry;
  entries: TEntryList;
begin
  entries := regatta.Entries;
  divRaces := regatta.Races;

  for i := 0 to divRaces.Count-1 do
  begin
    r := divRaces.Race[i];
    if not r.IsRacing then
      Continue;
    racePoints := TRacePointsList.Create(false);
    try
      for j := 0 to entries.Count-1 do
      begin
        e := entries.Entry[j];
        racePoints.Add(TRacePoints.Create(r, e, NaN, false));
      end;
      scoreRace(r, racePoints);
      RacePL.AddAll(racePoints);
    finally
      racePoints.Free;
    end;
  end;
  scoreSeries(divRaces, entries, RacePL);
end;

procedure TScoringManager.ScoreRace(race: TRace; racePoints: TRacePointsList; positionOnly: Boolean);
begin
  if Assigned(ScoringModel) and Assigned(race) then
    ScoringModel.ScoreRace(race, racepoints, positionOnly);
end;

procedure TScoringManager.ScoreRace(race: TRace; racePoints: TRacePointsList);
begin
  scoreRace(race, racePoints, false);
end;

procedure TScoringManager.ScoreRegatta(ev: TObject);
var
  regatta: TRegatta;
begin
  regatta := ev as TRegatta;

  //SyncObject.acquireScoringLock();
  try
    if TraceIsOn then
      Utils.WriteLine('ScoringManager: scoring started...');

    if (ScoringModel = nil) or
      (regatta = nil) or
      (regatta.Races.Count = 0) or
      (regatta.Entries.Count = 0)
    then
    begin
      if TraceIsOn then
        Utils.WriteLine('ScoringManager: (empty) done.');
      exit;
    end;

    //regatta.Races.Sort(); ### not necessary if sorted earliest first already

    RacePL.Clear;
    SeriesPL.Clear;

    ScoreDivision(regatta);

    if TraceIsOn then
      Utils.WriteLine('ScoringManager: scoring completed.');

  finally
    //SyncObject.releaseScoringLock();
  end;
end;

procedure TScoringManager.ScoreSeries(
  divRaces: TRaceList; entries: TEntryList;
  divPointsList: TRacePointsList);
var
  r: TRace;
begin
  if (divRaces.Count > 0) then
  begin
    r := divRaces.Race[0];
    if r.HasFleets and r.Regatta.IsInFinalPhase then
      ScoreQualiFinalSeries(divRaces, entries, divPointsList)
    else
      ScoreSeries2(divRaces, entries, divPointsList, 0);
  end;
end;

procedure TScoringManager.ScoreQualiFinalSeries(
  divRaces: TRaceList; entries: TEntryList;
  points: TRacePointsList);
var
  i, j: Integer;
  fc: Integer;
  rp: TRacePoints;
  el: TEntryList;
  fr: TRace;
  e: TEntry;
  f: TFinish;
  posOffset: Integer;
begin
  { Find the number of fleets in the race }
  fc := 0;
  for i := 0 to points.Count - 1 do
  begin
    rp := points.RacePoints[i];
    if (rp.Finish <> nil) and (rp.Finish.Fleet > fc) then
      fc := rp.Finish.Fleet;
  end;

  fr := nil;
  if (divRaces <> nil) and (divRaces.Count > 0) then
    fr := divRaces.Race[divRaces.Count-1];

  if fr = nil then
    exit;

  if not fr.IsFinalRace then
  begin
    ScoreSeries2(divRaces, entries, points, 0);
  end

  else
  begin
    el := TEntryList.Create(false);

    posOffset := 0;
    { call ScoreSeries1 for each fleet }
    for j := 0 to fc do
    begin
      { get the entries for the fleet }
      for i := 0 to fr.FinishList.Count - 1 do
      begin
        f := fr.FinishList.Finish[i];
        e := f.Entry;
        if f.Fleet = j then
          el.Add(e);
      end;

      if (el.Count > 0) and (points.Count > 0) then
      begin
        ScoreSeries2(divRaces, el, points, posOffset);
        posOffset := posOffset + el.Count;
      end;

      { clear the temp lists for the next run of the loop }
      el.Clear;
    end;
    el.Free;
  end;
end;

procedure TScoringManager.ScoreSeries2(
  divRaces: TRaceList; entries: TEntryList;
  divPointsList: TRacePointsList; posOffset: Integer);
var
  i: Integer;
  e: TEntry;
  divSeriesPoints: TSeriesPointsList;

  position: Integer;
  lastpoints: double;
  tied: Boolean;
  j: Integer;
  sp: TSeriesPoints;
  thispoints: double;
  nextpoints: double;
  tempRPL: TRacePointsList;
begin
  // calc throwouts,
  for i := 0 to entries.Count-1 do
  begin
    e := entries.Entry[i];
    tempRPL := divPointsList.FindAll(e);
    try
      ScoringModel.CalcThrowouts(tempRPL);
    finally
      tempRPL.Free;
    end;
  end;

  // run thru looking for average points
  CalcAveragePoints(divPointsList);

  divSeriesPoints := TSeriesPointsList.Create(false);
  try
    divSeriesPoints.InitPoints(entries);

    if (divSeriesPoints.Count <> 0) then
    begin
      ScoringModel.ScoreSeries(divRaces, entries, divPointsList, divSeriesPoints);

      ScoringModel.SortSeries(divSeriesPoints);

      //doties is the helper object for tiebreak resolution
      if not Assigned(Tiebreaker) then
        Tiebreaker := TScoringTiebreaker.Create(
        ScoringModel, divRaces, divPointsList, divSeriesPoints)
      else
      begin
        Tiebreaker.UseAgain(ScoringModel, divRaces, divPointsList, divSeriesPoints)
      end;
      // now run through looking for clumps of tied boats
      // pass the clumps of tied boats on to scoringmodel for resolution
      Tiebreaker.Process();

      // now set series position
      divSeriesPoints.SortPoints();
      position := 1;
      lastpoints := 0;
      for j := 0 to divSeriesPoints.Count-1 do
      begin
        sp := divSeriesPoints.Items[j] as TSeriesPoints;
        thispoints := sp.Points;
        if (j + 1 < divSeriesPoints.Count) then
          nextpoints := (TSeriesPoints(divSeriesPoints.Items[j + 1])).Points
        else
          nextpoints := 99999999.0;
        tied := not ((thispoints <> lastpoints) and (thispoints <> nextpoints));
        if not tied then
        begin
          position := j + 1;
        end
        else
        begin
          // position is same if tied with last
          if (thispoints <> lastpoints) then
            position := j + 1;
        end;
        sp.Position := position + posOffset;
        sp.IsTied := tied;
        lastpoints := thispoints;
      end;

      SeriesPL.AddAll(divSeriesPoints);

    end;
  finally
    divSeriesPoints.Free;
  end;
end;

end.

