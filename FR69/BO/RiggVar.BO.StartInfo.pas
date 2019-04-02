unit RiggVar.BO.StartInfo;

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
  System.DateUtils;

type
  TStartInfo = class;

  TRaceInfo = class
  private
    function GetRaceName: string;
  public
    Owner: TStartInfo;
    Race: Integer;
    IsFinalRace: Boolean; //as opposed to qualification races
    IsLastRace: Boolean; //this one contains the medal fleet
    IsRacing: Boolean;
    property RaceName: string read GetRaceName;
  end;

  TStartInfo = class
  private
    FIsRacing: Boolean;
    FLength: Integer;
    FWeight: Integer;
    FNonDiscardable: Integer;
    FStartTime: TDateTime;
    FGroupRank: Integer;
    function GetWeight: string;
    function GetLength: string;
    function GetNonDiscardable: Boolean;
    function GetRace: Integer;
    function GetIsRacing: Boolean;
    function GetFakeStartTime: string;
    function GetStartTime: string;
    function GetGroupRank: string;
    function GetFleetName: string;
    function GetLongFleetName: string;
    procedure SetLength(const Value: string);
    procedure SetGroupRank(const Value: string);
    procedure SetStartTime(const Value: string);
    procedure SetWeight(const Value: string);
    function GetShortFleetName: string;
  public
    UseLongFleetName: Boolean;
    RaceInfo: TRaceInfo;
    Fleet: Integer;
    IsFinalFleet: Boolean; //Gold Fleet can be 'Qual' if there is a medal race
    IsMedal: Boolean;
    IsConsolation: Boolean;
    IsGold: Boolean;

    procedure ParseNonDiscardable(const Value: string);
    property Race: Integer read GetRace;
    property FleetName: string read GetFleetName;

    property GroupRank: string read GetGroupRank write SetGroupRank;
    property StartTime: string read GetStartTime write SetStartTime;
    property Weight: string read GetWeight write SetWeight;
    property Length: string read GetLength write SetLength;
    property NonDiscardable: Boolean read GetNonDiscardable;
    property IsRacing: Boolean read GetIsRacing write FIsRacing;
  end;

  TDivisionInfo = class
  private
    FleetInfoList: TStringList;
    RaceInfoList: TStringList;

    function GetRaceCount: Integer;
    function GetFleetCount: Integer;
    function GetStartCount: Integer;

    function GetRaceInfo(Index: Integer): TRaceInfo;
    function GetFleetInfo(Index: Integer): TStartInfo;
    function GetStartInfo(Index: Integer): TStartInfo;
  public
    StartInfoList: TStringList;
    DivisionName: string;
    EventName: string;

    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Clear;
    function FindStartInfo(r, f: Integer): TStartInfo;
    function FindStartInfoByName(r, f: Integer; fn: string): TStartInfo;

    property RaceInfo [Index: Integer]: TRaceInfo read GetRaceInfo;
    property FleetInfo [Index: Integer]: TStartInfo read GetFleetInfo;
    property StartInfo [Index: Integer]: TStartInfo read GetStartInfo;

    property FleetCount: Integer read GetFleetCount;
    property RaceCount: Integer read GetRaceCount;
    property StartCount: Integer read GetStartCount;
  end;

implementation

uses
  RiggVar.BO.Def,
  RiggVar.Col.Event;

{ TRaceInfo }

function TRaceInfo.GetRaceName: string;
begin
  result := IntToStr(Race);
end;

{ TStartInfo}

function TStartInfo.GetFleetName: string;
begin
  if UseLongFleetName then
    result := GetLongFleetName
  else
    result := GetShortFleetName;
end;

function TStartInfo.GetShortFleetName: string;
begin
  if IsConsolation then
    result := 'C'
  else if IsFinalFleet or IsGold then
  begin
    case Fleet of
      0: result := 'M';
      1: result := 'G';
      2: result := 'S';
      3: result := 'B';
      4: result := 'E';
      else
        result := 'F' + IntToStr(Fleet);
    end;
  end
  else
  begin
    case Fleet of
      0: result := 'N';
      1: result := 'Y';
      2: result := 'B';
      3: result := 'R';
      4: result := 'G';
      else
        result := 'Q' + IntToStr(Fleet);
    end;
    result := IntToStr(Race) + result;
  end;
end;

function TStartInfo.GetLongFleetName: string;
begin
  if IsConsolation then
    result := 'Gold Consolation'
  else if IsFinalFleet then
  begin
    case Fleet of
      0: result := 'Medal';
      1: result := 'Gold';
      2: result := 'Silver';
      3: result := 'Bronze';
      4: result := 'Esmerald';
      else
        result := 'FinalFleet' + IntToStr(Fleet);
    end;
  end
  else
  begin
    case Fleet of
      0: result := 'Normal';
      1: result := 'Yellow';
      2: result := 'Blue';
      3: result := 'Red';
      4: result := 'Green';
      else
        result := 'QualiFleet' + IntToStr(Fleet);
    end;
    result := IntToStr(Race) + ' ' + result;
  end;
end;

function TStartInfo.GetGroupRank: string;
var
  gr: Integer;
begin
  if FGroupRank = 0 then
  begin
    if RaceInfo.IsFinalRace then
      gr := Fleet + 1
    else
      gr := 10 + Fleet + 1;
    result := FormatFloat('0.0#', gr / 10);
  end
  else
  begin
    result := FormatFloat('0.0#', FGroupRank / 10);
  end;
end;

function TStartInfo.GetIsRacing: Boolean;
begin
  result := FIsRacing and RaceInfo.IsRacing;
end;

function TStartInfo.GetStartTime: string;
begin
  if FStartTime = 0 then
  begin
    if IsRacing then
      result := GetFakeStartTime
    else
      result := 'No Time';
  end
  else
  begin
    result := FormatDateTime('hh:mm:ss', FStartTime);
  end;
end;

function TStartInfo.GetFakeStartTime: string;
var
  dt: TDateTime;
begin
  dt := 0;
  dt := IncHour(dt, 11);
  dt := IncMinute(dt, Fleet * 5);
  result := FormatDateTime('hh:mm:ss', dt);
end;

function TStartInfo.GetRace: Integer;
begin
  result := RaceInfo.Race;
end;

function TStartInfo.GetNonDiscardable: Boolean;
begin
  if FNonDiscardable = -1 then
    result := False
  else if FNonDiscardable = 1 then
    result := True
  else //if FNonDiscardable = 0 then
    result := IsMedal
end;

function TStartInfo.GetWeight: string;
begin
  if FWeight = 0 then
  begin
    if IsMedal then
      result := '2.0'
    else
      result := '1.0';
  end
  else
    result := FormatFloat('0.0#', FWeight / 10);
end;

procedure TStartInfo.SetGroupRank(const Value: string);
begin
  FGroupRank := StrToIntDef(Value, 0) * 10 ;
end;

procedure TStartInfo.SetLength(const Value: string);
begin
  FLength := StrToIntDef(Value, 0) * 10;
end;

procedure TStartInfo.ParseNonDiscardable(const Value: string);
var
  i: Integer;
begin
  i := StrToIntDef(Value, 0);
  if (i = -1) or (i = 0) or (i = 1) then
    FNonDiscardable := i;
end;

procedure TStartInfo.SetStartTime(const Value: string);
begin
  if Value = '0' then
    FStartTime := 0
  else
    FStartTime := StrToDateTimeDef(Value, FStartTime);
end;

procedure TStartInfo.SetWeight(const Value: string);
begin
  FWeight := StrToIntDef(Value, FWeight);
end;

function TStartInfo.GetLength: string;
begin
  if FLength = 0 then
    result := '1.0'
  else
    result := FormatFloat('0.0#', FLength / 10);
end;

{ TDivisionInfo }

constructor TDivisionInfo.Create;
begin
  inherited Create;
  FleetInfoList := TStringList.Create;
  RaceInfoList := TStringList.Create;
  StartInfoList := TStringList.Create;
end;

destructor TDivisionInfo.Destroy;
begin
  Clear;
  FleetInfoList.Free;
  RaceInfoList.Free;
  StartInfoList.Free;
  inherited;
end;

procedure TDivisionInfo.Clear;
var
  i: Integer;
begin
  for i := 0 to StartInfoList.Count - 1 do
    StartInfoList.Objects[i].Free;
  for i := 0 to RaceInfoList.Count - 1 do
    RaceInfoList.Objects[i].Free;
  RaceInfoList.Clear;
  FleetInfoList.Clear;
  StartInfoList.Clear;
end;

procedure TDivisionInfo.Init;
var
  en: TEventNode;
  fmax: Integer;
  fmin: Integer;
  r, f: Integer;
  rn, fn, sn: string;
  ri: TRaceInfo;
  si: TStartInfo;
begin
  Clear;
  en := BO.EventNode;
  DivisionName := BO.EventProps.DivisionName;
  EventName := BO.EventProps.EventName;

  for r := 1 to en.RaceCount do
  begin
    ri := TRaceInfo.Create;
    ri.Race := r;
    ri.IsFinalRace := en.IsFinalRace(r);
    ri.IsLastRace := r = en.RaceCount;
    ri.IsRacing := en.IsRacing(r);
    rn := ri.RaceName;
    RaceInfoList.AddObject(rn, ri);

    fmin := en.FleetMin(r);
    fmax := en.FleetMax(r);
    for f := fmin to fmax do
    begin
      si := TStartInfo.Create;
      si.RaceInfo := ri;
      ri.Owner := si;
      si.IsRacing := True;
      si.Fleet := f;
      si.IsMedal := (f = 0) and ri.IsLastRace;
      si.IsConsolation := (f = 1) and ri.IsLastRace;
      si.IsGold := (f = 1) and ri.IsFinalRace and not si.IsConsolation;
      si.IsFinalFleet := ri.IsFinalRace and not si.IsGold;

      sn := Format('R%d-F%d', [r, f]);
      StartInfoList.AddObject(sn, si);

      fn := si.FleetName;
      if FleetInfoList.IndexOf(fn) = -1 then
        FleetInfoList.AddObject(fn, si);
    end;
  end;
end;

function TDivisionInfo.GetRaceCount: Integer;
begin
  result := RaceInfoList.Count;
end;

function TDivisionInfo.GetFleetCount: Integer;
begin
  result := FleetInfoList.Count;
end;

function TDivisionInfo.GetStartCount: Integer;
begin
  result := StartInfoList.Count;
end;

function TDivisionInfo.GetFleetInfo(Index: Integer): TStartInfo;
var
  si: TStartInfo;
begin
  si := TStartInfo(FleetInfoList.Objects[Index]);
  result := si;
end;

function TDivisionInfo.GetRaceInfo(Index: Integer): TRaceInfo;
var
  ri: TRaceInfo;
begin
  ri := TRaceInfo(RaceInfoList.Objects[Index]);
  result := ri;
end;

function TDivisionInfo.GetStartInfo(Index: Integer): TStartInfo;
var
  si: TStartInfo;
begin
  si := TStartInfo(StartInfoList.Objects[Index]);
  result := si;
end;

function TDivisionInfo.FindStartInfo(r, f: Integer): TStartInfo;
var
  i: Integer;
  si: TStartInfo;
begin
  result := nil;
  for i := 0 to StartInfoList.Count - 1 do
  begin
    si := StartInfo[i];
    if (si.Race = r) and (si.Fleet = f) then
    begin
      result := si;
      break;
    end;
  end;
end;

function TDivisionInfo.FindStartInfoByName(r, f: Integer; fn: string): TStartInfo;
var
  i: Integer;
  si: TStartInfo;
begin
  result := nil;
  for i := 0 to StartInfoList.Count - 1 do
  begin
    si := StartInfo[i];
    if (si.Race = r) and (si.Fleet = f) and (si.FleetName = fn) then
    begin
      result := si;
      break;
    end;
  end;
end;
end.
