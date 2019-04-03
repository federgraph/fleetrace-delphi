unit RiggVar.Calc.EventProxy00;

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

const
  LowPoint = 0;
  Bonus = 1;
  BonusDSV = 2;

  p_FRProxy = 'FRProxy';
  p_Gezeitet = 'Gezeitet';

  p_EventProps = 'EventProps';

  p_RaceProps = 'RaceProps';
  p_RCount = 'RCount';

  p_Race = 'Race';
  p_IsRacing = 'IsRacing';
  p_Index = 'Index';
  p_FResult = 'FResult';
  p_OTime = 'OTime';
  p_QU = 'QU';
  p_Penalty_Points = 'P_Points';
  p_Penalty_Note = 'P_Note';
  p_Penalty_Percent = 'P_Percent';
  p_Penalty_Time = 'P_Time';

  p_Entry = 'Entry';
  p_CPoints = 'CPoints';
  p_Rank = 'Rank';
  p_PosR = 'PosR';
  p_PLZ = 'PLZ';
  p_Drop = 'Drop';
  p_SNR = 'SNR';
  p_IsGezeitet = 'IsGezeitet';
  p_IsTied = 'IsTied';

  p_ScoringSystem = 'ScoringSystem';
  p_ScoringSystem2 = 'ScoringSystem2';
  p_ThrowoutScheme = 'ThrowoutScheme';
  p_Throwouts = 'Throwouts';
  p_FirstIs75 = 'FirstIs75';
  p_ReorderRAF = 'ReorderRAF';
  p_DivisionName = 'DivisionName';

  p_UseFleets = 'UseFleets';
  p_TargetFleetSize = 'TargetFleetSize';
  p_FirstFinalRace = 'FirstFinalRace';
  p_Fleet = 'Fleet';

type
  TJSEventProps = class
  protected
    procedure Assign(ep: TJSEventProps);
  public
    ScoringSystem: Integer;
    ScoringSystem2: Integer;
    ThrowoutScheme: Integer;
    Throwouts: Integer;
    FirstIs75: Boolean;
    ReorderRAF: Boolean;
    constructor Create;
    function Equals(o: TObject): Boolean; override;
  end;

  TRaceInfo = class
  protected
    procedure Assign(ri: TRaceInfo);
  public
    //Input
    OTime: Integer;
    QU: Integer;
    Penalty_Points: double;
    Penalty_Note: string; //string[80];
    Penalty_Percent: Integer;
    Penalty_TimePenalty: Int64;
    Fleet: Integer;
    IsRacing: Boolean;

    //Output
    CPoints: double; //Calculated Points
    Rank: Integer;
    PosR: Integer; //eindeutiges Ranking
    PLZ: Integer; //PlatzZiffer
    Drop: Boolean; //IsThrowout
    function Equals(o: TObject): Boolean; override;
    constructor Create;
  end;

  TEntryInfo = class
  private
    FIndex: Integer;
    function GetRaceCount: Integer;
    function GetRCount: Integer;
  protected
    procedure Assign(ei: TEntryInfo);
  public
    SNR: Integer;
    IsGezeitet: Boolean;
    IsTied: Boolean;
    Race: array of TRaceInfo;
    constructor Create;
    destructor Destroy; override;
    function Equals(o: TObject): Boolean; override;
    property RaceCount: Integer read GetRaceCount;
    property RCount: Integer read GetRCount;
    property Index: Integer read FIndex write FIndex;
  end;

  TEntryInfoCollection = class
  private
    function GetCount: Integer;
  public
    Items: array of TEntryInfo;
    destructor Destroy; override;
    function Equals(o: TObject): Boolean; override;
    function FindKey(SNR: Integer): TEntryInfo;
    property Count: Integer read GetCount;
  end;

  TFRProxy = class
  private
    function GetRCount: Integer;
    function GetRaceCount: Integer;
  public
    EventProps: TJSEventProps;
    EntryInfoCollection: TEntryInfoCollection;
    IsRacing: array of Boolean;
    Gezeitet: Integer;
    FResult: Integer;

    UseFleets: Boolean;
    TargetFleetSize: Integer;
    FirstFinalRace: Integer;
    constructor Create;
    destructor Destroy; override;
    function Equals(o: TObject): Boolean; override;
    procedure Assign(p: TFRProxy);
    property RaceCount: Integer read GetRaceCount;
    property RCount: Integer read GetRCount;
  end;

implementation

{ TEntryInfoCollection }

destructor TEntryInfoCollection.Destroy;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  SetLength(Items, 0);
  inherited;
end;

function TEntryInfoCollection.Equals(o: TObject): Boolean;
var
  i: Integer;
  ei: TEntryInfo;
  eic: TEntryInfoCollection;
begin
  eic := TEntryInfoCollection(o);
  result := true;
  if Count <> eic.Count then
  begin
    result := false;
    exit;
  end;
  for i := 0 to eic.Count-1 do
  begin
    ei := Items[i];
    result := result and ei.Equals(eic.Items[i]);
  end;
end;

function TEntryInfoCollection.FindKey(SNR: Integer): TEntryInfo;
var
  i: Integer;
  o: TEntryInfo;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.SNR = SNR) then
    begin
      result := o;
      break;
    end;
  end;
end;

function TEntryInfoCollection.GetCount: Integer;
begin
  if Items <> nil then
    result := Length(Items)
  else
    result := 0;
end;

{ TEntryInfo }

procedure TEntryInfo.Assign(ei: TEntryInfo);
var
  i: Integer;
  ri: TRaceInfo;
begin
  SNR := ei.SNR;
  IsGezeitet := ei.IsGezeitet;
  IsTied := ei.IsTied;

  for i := RCount-1 downto 0 do
    Race[i].Free;
  SetLength(Race, ei.RCount);
  for i := 0 to ei.GetRCount - 1 do
  begin
    ri := TRaceInfo.Create;
    ri.Assign(ei.Race[i]);
    Race[i] := ri;
  end;
end;

constructor TEntryInfo.Create;
begin
  inherited Create;
end;

destructor TEntryInfo.Destroy;
var
  i: Integer;
begin
  for i := RCount-1 downto 0 do
    Race[i].Free;
  SetLength(Race, 0);
  inherited;
end;

function TEntryInfo.Equals(o: TObject): Boolean;
var
  ei: TEntryInfo;
var
  i: Integer;
  ri: TRaceInfo;
begin
  ei := TEntryInfo(o);
  result := true;
  if FIndex <> ei.Index then
    result := false;
  if SNR <> ei.SNR then
    result := false;
  if IsGezeitet <> ei.IsGezeitet then
    result := false;
  if IsTied <> ei.IsTied then
    result := false;
  if RCount <> ei.RCount then
  begin
    result := false;
    exit;
  end;
  for i := 0 to ei.RCount - 1 do
  begin
    ri := Race[i];
    result := result and ri.Equals(ei.Race[i]);
  end;
end;

function TEntryInfo.GetRaceCount: Integer;
begin
  result := RCount-1;
end;

function TEntryInfo.GetRCount: Integer;
begin
  result := Length(Race);
end;

{ TFRProxy }

procedure TFRProxy.Assign(p: TFRProxy);
var
  i: Integer;
  ei: TEntryInfo;
begin
  EventProps.Assign(p.EventProps);

  SetLength(IsRacing, p.RCount);
  for i := 0 to RCount - 1 do
  begin
    IsRacing[i] := p.IsRacing[i];
  end;

  EntryInfoCollection.Free;
  EntryInfoCollection := TEntryInfoCollection.Create;
  SetLength(EntryInfoCollection.Items, p.EntryInfoCollection.Count);
  for i := 0 to p.EntryInfoCollection.Count - 1 do
  begin
    ei := TEntryInfo.Create;
    ei.Index := i;
    ei.Assign(p.EntryInfoCollection.Items[i]);
    EntryInfoCollection.Items[i] := ei;
  end;

  Gezeitet := p.Gezeitet;
  FResult := p.FResult;
  UseFleets := p.UseFleets;
  TargetFleetSize := p.TargetFleetSize;
  FirstFinalRace := p.FirstFinalRace;
end;

constructor TFRProxy.Create;
begin
  inherited Create;
  EventProps := TJSEventProps.Create;
  EntryInfoCollection := TEntryInfoCollection.Create;
end;

destructor TFRProxy.Destroy;
begin
  EventProps.Free;
  EntryInfoCollection.Free;
  inherited;
end;

function TFRProxy.Equals(o: TObject): Boolean;
var
  i: Integer;
  p: TFRProxy;
begin
  p := TFRProxy(o);
  result := true;
  if RCount <> p.RCount then
  begin
    result := false;
    exit;
  end;
  if not EventProps.Equals(p.EventProps) then
    result := false;
  if Gezeitet <> p.Gezeitet then
    result := false;
  if self.FResult <> p.FResult then
    result := false;
  if UseFleets <> p.UseFleets then
    result := false;
  if TargetFleetSize <> p.TargetFleetSize then
    result := false;
  if FirstFinalRace <> p.FirstFinalRace then
    result := false;

  if not EntryInfoCollection.Equals(p.EntryInfoCollection) then
    result := false;
  for i := 0 to RCount - 1 do
    if IsRacing[i] <> p.IsRacing[i] then
      result := false;
end;

function TFRProxy.GetRaceCount: Integer;
begin
  result := RCount - 1;
end;

function TFRProxy.GetRCount: Integer;
begin
  result := Length(IsRacing);
end;

{ TRaceInfo }

procedure TRaceInfo.Assign(ri: TRaceInfo);
begin
  //Input
  OTime := ri.OTime;
  QU := ri.QU;
  Penalty_Points := ri.Penalty_Points;
  Penalty_Note := ri.Penalty_Note;
  Penalty_Percent := ri.Penalty_Percent;
  Penalty_TimePenalty := ri.Penalty_TimePenalty;
  Fleet := ri.Fleet;
  IsRacing := ri.IsRacing;
  //Output
  CPoints := ri.CPoints;
  Rank := ri.Rank;
  PosR := ri.PosR;
  PLZ := ri.PLZ;
  Drop := ri.Drop;
end;

constructor TRaceInfo.Create;
begin
  inherited Create;
  IsRacing := true;
end;

function TRaceInfo.Equals(o: TObject): Boolean;
var
  ri: TRaceInfo;
begin
  ri := TRaceInfo(o);
  result := true;
  if OTime <> ri.OTime then
    result := false;
  if QU <> ri.QU then
    result := false;
  if Abs(Penalty_Points - ri.Penalty_Points) > 0.00001 then
    result := false;
  if Penalty_Note <> ri.Penalty_Note then
     result := false;
  if Penalty_Percent <> ri.Penalty_Percent then
     result := false;
  if Penalty_TimePenalty <> ri.Penalty_TimePenalty then
    result := false;
  if Fleet <> ri.Fleet then
    result := false;
  if IsRacing <> ri.IsRacing then
    result := false;

    //Output
  if Abs(CPoints - ri.CPoints) > 0.00001 then
    result := false;
  if Rank <> ri.Rank then
    result := false;
  if PosR <> ri.PosR then
    result := false;
  if PLZ <> ri.PLZ then
    result := false;
  if Drop <> ri.Drop then
    result := false;
end;

{ TJSEventProps }

procedure TJSEventProps.Assign(ep: TJSEventProps);
begin
  ScoringSystem := ep.ScoringSystem;
  ScoringSystem2 := ep.ScoringSystem2;
  ThrowoutScheme := ep.ThrowoutScheme;
  Throwouts := ep.Throwouts;
  FirstIs75 := ep.FirstIs75;
  ReorderRAF := ep.ReorderRAF;
end;

constructor TJSEventProps.Create;
begin
  inherited Create;
  //ScoringSystem := 0;
  //ScoringSystem2 := 0;
  //ThrowoutScheme := 0;
  Throwouts := 1;
  //FirstIs75 := false;
end;

function TJSEventProps.Equals(o: TObject): Boolean;
var
  ep: TJSEventProps;
begin
  Assert(o is TJSEventProps);
  ep := TJSEventProps(o);
  result := true;
  if ScoringSystem <> ep.ScoringSystem then
    result := false;
  if ScoringSystem2 <> ep.ScoringSystem2 then
    result := false;
  if ThrowoutScheme <> ep.ThrowoutScheme then
    result := false;
  if Throwouts <> ep.Throwouts then
    result := false;
  if FirstIs75 <> ep.FirstIs75 then
    result := false;
  if ReorderRAF <> ep.ReorderRAF then
    result := false;
end;

end.
