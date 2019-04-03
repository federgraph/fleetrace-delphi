unit RiggVar.Scoring.LowPoint;

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
  System.Classes, 
  System.Math,
  RiggVar.Grid.ColBase,
  RiggVar.Scoring.Domain,
  RiggVar.Scoring.Classes,
  RiggVar.Scoring.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Utils;

type
  TScoringLowPoint = class(TScoringModel)
  private
    procedure CalcTieBreakersDefault(races: TRaceList; entries: TEntryList;
      racepoints: TRacePointsList; seriespoints: TSeriesPointsList);
    procedure CalcTieBreakersAlternate(races: TRaceList;
      entries: TEntryList; racepoints: TRacePointsList;
      seriespoints: TSeriesPointsList);
  protected
    FName: string;
    procedure SetTiedPoints(const Value: TBaseList);
    function GetNumberThrowouts(pointsList: TRacePointsList): Integer; virtual;
    function CalcPercent(pct: Integer; basePts, nEntries, maxPoints: double): double;
    procedure IncrementSeriesScores(eLists: TBaseList; amount: double;
      series: TSeriesPointsList);
    procedure IncrementSeriesScore(e: TEntry; amount: double;
      series: TSeriesPointsList);
    procedure CompareWhoBeatWhoLast(stillTied: TBaseList;
      series: TSeriesPointsList);
    function ComparePointsBestToWorst(inLeft, inRight: TRacePointsList): Integer;
    function PrepBestToWorst(rpList: TRacePointsList): TRacePointsList;
    procedure ScoreRace1(race: TRace; points: TRacePointsList; positionOnly: Boolean);
    procedure ScoreRace2(race: TRace; points: TRacePointsList; positionOnly: Boolean);
  public
    constructor Create;

    function ToString: string; override;
    function IsBlank: Boolean; override;
    function Equals(obj: TObject): Boolean; override;

    function GetName: String; override;
    function GetPenaltyPoints(p: TISAFPenalty; rpList: TRacePointsList; basePts: Double): Double; override;
    procedure ScoreRace(race: TRace; points: TRacePointsList; positionOnly: Boolean); override;
    procedure ScoreSeries(races: TRaceList; entries: TEntryList; points: TRacePointsList; series: TSeriesPointsList); override;
    procedure SortSeries(series: TSeriesPointsList); override;
    procedure CalcThrowouts(pointsList: TRacePointsList); override;
    procedure CalcTiebreakers(races: TRaceList; entries: TEntryList;
      racepoints: TRacePointsList; seriespoints: TSeriesPointsList); override;

    property Name: string read FName;
    property TiedPoints: TBaseList write SetTiedPoints;
  end;

implementation

{ TScoringLowPoint }

procedure TScoringLowPoint.CalcThrowouts(pointsList: TRacePointsList);
var
  i, j: Integer;
  nThrows: Integer;
  worstRace: TRacePoints;
  thisRace: TRacePoints;
begin
  // look through the fThrowouts array and determine how many throwouts to award
  nThrows := getNumberThrowouts(pointsList);

  for i := 0 to nThrows-1 do
  begin
    worstRace := nil;
    for j := 0 to pointsList.Count-1 do
    begin
      thisRace := pointsList.RacePoints[j];
      if thisRace.Finish = nil then
        continue;
      if (thisRace.Finish.Fleet = 0) and (thisRace.Race.IsFinalRace) then
        Continue; //do not discard medal race scores

      if (not thisRace.Finish.Penalty.hasPenalty(DGM))
        and (not thisRace.Finish.Penalty.hasPenalty(AVG))
        and (not (thisRace.Finish.Penalty.PenaltyDSQ = DNE))
      then
      begin
        if ( (not thisRace.IsThrowout)
          and ((worstRace = nil) or (thisRace.Points > worstRace.Points)) )
        then
        begin
          worstRace := thisRace;
        end;
      end;
    end;
    if (worstRace <> nil) then
    begin
      worstRace.IsThrowout := true;
    end;
  end;
end;

procedure TScoringLowPoint.CalcTiebreakers(
  races: TRaceList;
  entries: TEntryList;
  racepoints: TRacePointsList;
  seriespoints: TSeriesPointsList);
var
  i, j: Integer;
  e: TEntry;
  r: TRace;
  rp: TRacePoints;
begin
  TiebreakerMode := TIE_RRS_Default;

  for i := races.Count - 1 downto 0 do
  begin
    r := races.Race[i];
    if r.IsFinalRace then
    begin
      for j := 0 to entries.Count - 1 do
      begin
        e := entries.Entry[j];
        rp := racepoints.find(r, e);
        if rp <> nil then
        begin
          if rp.Finish <> nil then
          begin
            if rp.Finish.Fleet = 0 then
              TiebreakerMode := TIE_RRS_A82_ONLY
            else
              TiebreakerMode := TIE_RRS_Default;
          end;
        end;
      end;
    end;
    break; //only look into last race
  end;

  if (TiebreakerMode = TIE_RRS_DEFAULT) then
    CalcTiebreakersDefault(races, entries, racepoints, seriespoints)
  else
    CalcTiebreakersAlternate(races, entries, racepoints, seriespoints);
end;

// <summary>
// resolve ties among a group of tied boats.
// A tie that is breakable should have .01 point increments added as appropriate.
// Assume that each individual race and series points have calculated, and that
// throwouts have already been designated in the points objects.
// </summary>
// <param name="races">
// races involved (not used here, but is defined in interface)
// </param>
// <param name="entriesIn">
// list of tied entries
// </param>
// <param name="points">
// list of points for all races and entries (and maybe more!)
// </param>
// <param name="series">
// map containing series points for the entries, prior to
// handling ties (and maybe more than just those entries)
// </param>
procedure TScoringLowPoint.CalcTiebreakersDefault(
  races: TRaceList;
  entries: TEntryList;
  racepoints: TRacePointsList;
  seriespoints: TSeriesPointsList);
var
  rows: TBaseList;
  e: TEntry;
  row: TRacePointsList;
  tiedWithBest: TBaseList;
  incAmount: double;
  leftRow, bestRow: TRacePointsList;
  c: Integer;
  i: Integer;
begin
  if (entries = nil) then
    exit;

  //next create separate lists of finishes for each of the tied boats.
  //store these lists in rows, a list of RacePointLists
  //each row item is a sorted list of racepoints that are not throwouts
  //there is one row item per tied entry,
  rows := TBaseList.Create(TBaseObject, True);
  for i := 0 to entries.Count-1 do
  begin
    e := entries.Entry[i];
    row := racepoints.findAll(e);
    rows.Add(row); //add the row of RacePoints for entry e
  end;

  tiedWithBest := TBaseList.Create(TBaseObject, False); //holds rows
  // Pull out best of the bunch of tied rows one at a time.
  // After each scan, the best row is dropped with no more change in points
  // Each remaining row gets .01 added to total
  // Continue til no more rows left to play.
  while (rows.Count > 1) do
  begin
    bestRow := rows.Items[0] as TRacePointsList;
    tiedWithBest.Clear();

    // loop thru entries, apply tiebreaker method
    // and keep the best (winner)
    for i := 1 to rows.Count-1 do
    begin
      leftRow := TRacePointsList(rows.Items[i]);
      // compare for ties by A8.1
      c := comparePointsBestToWorst(leftRow, bestRow);
      if (c < 0) then //if leftRow is better then rightRow (bestRow)
      begin
        bestRow := leftRow; //leftRow becomes bestRow
        tiedWithBest.Clear();
      end
      else if (c = 0) then
      begin
        tiedWithBest.Add(leftRow);
      end;
    end;
    if (tiedWithBest.Count > 0) then
    begin
      // have boats tied after applying A8.1
      // send them into next tiebreakers clauses
      tiedWithBest.Add(bestRow);
      compareWhoBeatWhoLast(tiedWithBest, seriespoints);
    end;

    //increment value for the rest, not tied with the best
    incAmount := (tiedWithBest.Count + 1) * TIEBREAK_INCREMENT;

    // drop the best row from the list
    //(local variable bestRow should now reference the best row)
    //conversion issue, slightly changed code:
    //  rows owns the rows, see above; make sure you do not free a list twice.
    if tiedWithBest.Count > 0 then
      rows.removeAll(tiedWithBest) //all tied rows, including! the best
    else
      rows.Remove(bestRow); //only the best

    //apply the increment to the rest
    incrementSeriesScores(rows, incAmount, seriespoints);

    //and loop (seaching for the best row in the remaining rows)
  end;
  // we be done

  tiedWithBest.Free;
  rows.Free;
end;

procedure TScoringLowPoint.CalcTiebreakersAlternate(
  races: TRaceList;
  entries: TEntryList;
  racepoints: TRacePointsList;
  seriespoints: TSeriesPointsList);
var
  rows: TBaseList;
  e: TEntry;
  row: TRacePointsList;
  i: Integer;
begin
  if (entries = nil) then
    exit;

  //create separate lists of racepoints for each of the tied entries.
  //store these lists in a list of lists of RacePointLists
  rows := TBaseList.Create(TBaseObject, True);
  for i := 0 to entries.Count-1 do
  begin
    e := entries.Entry[i];
    row := racepoints.findAll(e);
    rows.Add(row); //add the row of RacePoints for entry e
  end;

  //each item of rows is a sorted list of racepoints that are not throwouts
  compareWhoBeatWhoLast(rows, seriespoints);

  rows.Free;
end;

// returns percent of number of entries, to nearest 10th, .5 going up
// with a maximum points of those for DNC
// or in other words limit to dnc points, as passed via maxPoints param
function TScoringLowPoint.calcPercent(pct: Integer; basePts, nEntries, maxPoints: double): double;
var
  points: double;
begin
  // this gives points * 10
  points := Round(nEntries * (pct / 10.0));
  points := points / 10.0;
  result := Min(basePts + points, maxPoints);
end;

procedure TScoringLowPoint.incrementSeriesScore(
  e: TEntry; amount: double; series: TSeriesPointsList);
var
  list: TSeriesPointsList;
  eSeries: TSeriesPoints;
begin
  // find all series points for e, should be exactly 1
  list := series.findAll(e);
  if Assigned(list) then
  begin
    eSeries := TSeriesPoints(list.SeriesPoints[0]);
    if Assigned(eSeries) then
    begin
      // add TIEBREAK_INCREMENT to its score
      eSeries.Points := eSeries.Points + amount;
    end;
    list.Free;
  end;
end;

procedure TScoringLowPoint.incrementSeriesScores(
  eLists: TBaseList; amount: double; series: TSeriesPointsList);
var
  i: Integer;
  pl: TRacePointsList;
begin
  // add TIEBREAK_INCREMENT to series points of remaining tied boats
  for i := 0 to eLists.Count-1 do
  begin
    // pull entry from 1st element of the i'th eLists
    pl := TRacePointsList(eLists.Items[i]);
    if (pl.Count = 0) then
      Utils.WriteLine('ScoringMessageInvalidSeries')
    else
      incrementSeriesScore(pl.RacePoints[0].Entry, amount, series);
  end;
end;

function TScoringLowPoint.PrepBestToWorst(rpList: TRacePointsList): TRacePointsList;
var
  i: Integer;
  ePoints: TRacePointsList;
  p: TRacePoints;
begin
  ePoints := TRacePointsList(rpList.Clone);

  // delete throwouts from the list
  for i := ePoints.Count-1 downto 0 do
  begin
    p := ePoints.RacePoints[i];
    if (p <> nil) then
      if (p.IsThrowout) then
        ePoints.Remove(p);
  end;
  ePoints.sortPoints();
  result := ePoints;
end;

// <summary>
// <p>compares two sets of race points for tie breaker resolution.</p>
// <p>RRS2001 A8.1: "If there is a series score tie between two or more boats,
// race scores of each boat shall be listed in order of best to worst,
// and at the first point(s) where there is a difference the tie
// shall be broken in favour of the boat(s) with the best score(s).
// No excluded scores shall be used."</p>
// </summary>
// <param name="inLeft">racepointslist of lefty</param>
// <param name="inRight">racepointslist of righty</param>
// <returns>
// -1 if "left" wins tiebreaker, 1 if right wins, 0 if tied.
// </returns>
function TScoringLowPoint.ComparePointsBestToWorst(inLeft, inRight: TRacePointsList): Integer;
var
  left, right: TRacePointsList;
  lp, rp: double;
  i: Integer;
begin
  left := PrepBestToWorst(inLeft);
  right := PrepBestToWorst(inRight);

  result := 0;
  // we know they are sorted by finish points, look for first non-equal points
  for i := 0 to left.Count-1 do
  begin
    lp := left.RacePoints[i].Points;
    rp := right.RacePoints[i].Points;
    if (lp < rp) then
    begin
      result := - 1;
      break;
    end
    else if (rp < lp) then
    begin
      result := 1;
      break;
    end;
  end;
  left.Free;
  right.Free;
end;

// <summary>
// applies the remaining tiebreakers of RRS2001 A8 to set of boats tied after
// comparing their list of scores.  This is the new 2002+ formula after ISAF
// deleted 8.2 and renumbered 8.3 to 8.2
// <p>
// RRS2001 modified A8.2 (old 8.3): "If a tie still remains between two or more
// boats, they shall be ranked in order of their scores in the last race.
// Any remaining ties shall be broken by using the scores of the tied boat in the
// next-to-last race and so on until all ties are broken.
// These scores shall be used even if some of them are excluded scores."</p>
// </summary>
// <param name="stillTied">
// list of boat scores of the group for which A8.1 does not resolve the tie
// </param>
// <param name="series">list of series scores</param>
procedure TScoringLowPoint.CompareWhoBeatWhoLast(
  stillTied: TBaseList; series: TSeriesPointsList);
var
  nRaces: Integer;
  nTied: Integer;
  tiedEntries: TEntryList;
  i: Integer;
  list: TRacePointsList;
  e: Integer;
  o: Integer;
  r: Integer;
  ePts, oPts: double;
  entry: TEntry;
begin
  tiedEntries := TEntryList.Create(false);
  try

    nRaces := TRacePointsList(stillTied.Items[0]).Count;
    nTied := stillTied.Count;

    // sort rows by Race and build list of tied entries
    for i := 0 to nTied-1 do
    begin
      list := stillTied.Items[i] as TRacePointsList;
      if (list.Count = 0) then
        continue;
      list.sortRace();
      tiedEntries.Add(TRacePoints(list.Items[0]).Entry);
    end;

    // compare round robin
    for e := 0 to nTied-1 do
    begin
      entry := tiedEntries.Entry[e];
      for o := 0 to nTied-1 do
      begin
        if e = o then
          continue;
        for r := nRaces-1 downto 0 do //right to left (sorted RacePoints)
        begin
          ePts := (TRacePointsList(stillTied.Items[e]).RacePoints[r]).Points;
          oPts := (TRacePointsList(stillTied.Items[o]).RacePoints[r]).Points;
          { if beaten entry (bigger points) is found
            increment points by Tiebreak_increment for it and break }
          if (ePts > oPts) then
          begin
            incrementSeriesScore(entry, TIEBREAK_INCREMENT, series);
            break;
          end
          else if ePts < oPts then
            break;
        end;
      end;
    end;

  finally
    tiedEntries.Free;
  end;
end;

constructor TScoringLowPoint.Create;
begin
  inherited Create;
  FName := 'ISAF Low Point 2005-2008';
end;

function TScoringLowPoint.GetName: String;
begin
  result := FName;
end;

function TScoringLowPoint.GetNumberThrowouts(
  pointsList: TRacePointsList): Integer;
var
  nThrows: Integer;
  nRaces: Integer;
  i: Integer;
  minRaces: Integer;
begin
  nThrows := 0;
  nRaces := pointsList.Count;

  case fThrowoutScheme of

    THROWOUT_NONE: nThrows := 0;

    THROWOUT_BYNUMRACES:
      for i := 0 to Length(fThrowouts)-1 do
      begin
        minRaces := fThrowouts[i];
        if (nRaces >= minRaces) and (minRaces > 0) then
          nThrows := i + 1;
      end;

    THROWOUT_PERXRACES:
      if (fThrowoutPerX > 0) then
      begin
        nThrows := Floor(nRaces / fThrowoutPerX);
      end;

    THROWOUT_BESTXRACES:
      if (fThrowoutBestX > 0) and (nRaces > fThrowoutBestX) then
      begin
        nThrows := nRaces - fThrowoutBestX;
      end;
  end;
  result := nThrows;
end;

function TScoringLowPoint.GetPenaltyPoints(p: TISAFPenalty;
  rpList: TRacePointsList; basePts: Double): Double;
var
  dsqPoints: double;
  nFinishers: Integer;
  nEntries: Integer;
  pen: TISAFPenalty;
begin
  nEntries := 0;
  if (rpList <> nil) then
  begin
    nEntries := rpList.Count;
  end;

  if HasFleets and (TargetFleetSize > nEntries) and (not IsFinalRace)
  then
  begin
    nEntries := TargetFleetSize;
  end;

  // if MAN, RDG, DPI, STP - return fixed points and be gone
  if p.hasPenalty(MAN) or p.hasPenalty(RDG) or p.hasPenalty(DPI) or p.HasPenalty(STP) then
  begin
    result := p.Points;
    exit;
  end;

  //A9 RACE SCORES IN A SERIES LONGER THAN A REGATTA
  //For a series that is held over a period of time longer than a regatta, a boat that
  //came to the starting area but did not start (DNS), did not finish (DNF), retired after finishing (RAF)
  //or was disqualified (allDSQ) shall be scored points for the finishing place one more than
  //the number of boats that came to the starting area. A boat that did not come to
  //the starting area (DNC) shall be scored points for the finishing place one more than the
  //number of boats entered in the series.

  // if a DSQ, return DSQ points and be gone
  if p.isDsqPenalty then
  begin
    if rpList = nil then
    begin
      result := 0;
      exit;
    end;
    if isLongSeries then
      result := nEntries - rpList.getNumberWithPenalty(DNC) + 1
    else
      result := nEntries + 1;
    exit;
  end;

  // if a DNC, return DNC points and be gone
  if (p.hasPenalty(DNC)) then
  begin
    result := nEntries + 1;
    exit;
  end;

  // any finish penalty other than TLE, return entries + 1 and be gone
  if p.isFinishPenalty and (not p.hasPenalty(TLE)) then
  begin
    if rpList = nil then
    begin
      result := 0;
      exit;
    end;
    if isLongSeries then
      result := nEntries - rpList.getNumberWithPenalty(DNC) + 1
    else
      result := nEntries + 1;
    exit;
  end;

  if p.hasPenalty(TLE) then
  begin
    if rpList = nil then
    begin
      nFinishers := 0;
    end
    else
    begin
      nFinishers := rpList.NumberOfFinishers;
    end;

    // set the basepts to the appropriate TLE points
    case fTimeLimitPenalty of

      TLE_FINISHERSPLUS1:
        basePts := nFinishers + 1;

      TLE_FINISHERSPLUS2:
        basePts := nFinishers + 2;

      TLE_AVERAGE:
        basePts := nFinishers + ((nEntries - nFinishers) / 2.0);

      else
      begin
        pen := TISAFPenalty.Create(DNF);
        basePts := getPenaltyPoints(pen, rpList, basePts);
      end;
    end;
  end;

  // ADD in other non-finish penalties
  pen := TISAFPenalty.Create(DSQ);
  dsqPoints := getPenaltyPoints(pen, rpList, basePts);
  if (p.hasPenalty(CNF)) then
    basePts := Round(calcPercent(fCheckinPercent, basePts, nEntries, dsqPoints));

  if (p.hasPenalty(ZFP)) then
    basePts := Round(calcPercent(20, basePts, nEntries, dsqPoints));

  if (p.hasPenalty(SCP)) then
    basePts := Round(calcPercent(p.Percent, basePts, nEntries, dsqPoints));

  result := basePts;

  if Assigned(pen) then
    pen.Free;
end;

function TScoringLowPoint.isBlank: Boolean;
begin
  result := false;
end;

procedure TScoringLowPoint.scoreRace(race: TRace; points: TRacePointsList;
  positionOnly: Boolean);
begin
  if race.HasFleets then
    scoreRace2(race, points, positionOnly)
  else
    scoreRace1(race, points, positionOnly);
end;

procedure TScoringLowPoint.scoreRace2(race: TRace; points: TRacePointsList;
  positionOnly: Boolean);
var
  i, j: Integer;
  fc: Integer;
  rp: TRacePoints;
  rpl: TRacePointsList;
begin
  self.HasFleets := race.HasFleets;
  self.TargetFleetSize := race.TargetFleetSize;
  self.IsFinalRace := race.IsFinalRace;


  { find the number of fleets in the race }
  fc := 0;
  for i := 0 to points.Count - 1 do
  begin
    rp := points.RacePoints[i];
    if (rp.Finish <> nil) and (rp.Finish.Fleet > fc) then
      fc := rp.Finish.Fleet;
  end;

  rpl := TRacePointsList.Create(false);

  { call scoreRace1 for each fleet }
  for j := 0 to fc do
  begin
    for i := 0 to points.Count - 1 do
    begin
      rp := points.RacePoints[i];
      if (rp.Finish <> nil) and (rp.Finish.Fleet = j) then
      begin
        rpl.Add(rp);
      end;
    end;
    if rpl.Count > 0 then
      scoreRace1(race, rpl, positionOnly);
    rpl.Clear;
  end;

  rpl.Free;
end;

procedure TScoringLowPoint.scoreRace1(race: TRace; points: TRacePointsList;
  positionOnly: Boolean);
var
  f: TFinish;
  pts, basePts: double;
  lastrp, rp: TRacePoints;
  i: Integer;
  tied: TBaseList;
  divPosition: Integer;
  valid: boolean;
  isdsq: boolean;
  israf: boolean;
  NormalCountup: Boolean;
begin
  if not race.IsRacing then
  begin
    for i := 0 to points.Count-1 do
    begin
      rp := points.RacePoints[i];
      rp.Points := 0;
      rp.Position := 1;
    end;
    exit;
  end;

  // sort points on finishposition sorted top to bottom by finish
  points.sortCorrectedTimePosition();

  // init local variable pts
  if firstIs75 then
    pts := 0.75
  else
    pts := 1.0;

  // position within the divsion (as opposed to within the fleet)
  divPosition := 1;

  // loop thru the race's finishes, for each finish in the list, set the points
  for i := 0 to points.Count-1 do
  begin
    rp := points.RacePoints[i];
    f := rp.Finish;

    basePts := pts;

    valid := f.FinishPosition.isValidFinish;
    isdsq := f.Penalty.isDsqPenalty;
    israf := f.Penalty.HasPenalty(RAF);

    NormalCountup := valid and (not isdsq);

    //normally RAF should alter points for finishers behind
    //but RAF did not alter points in Cascais 2007 results!
    if not ReorderRAF then
      NormalCountup := valid and (israf or not isdsq);

    if NormalCountup then
    begin
      //normal countup for position
      rp.Position := divPosition;

      // increment points to be assigned to next guy
      if (pts = 0.75) then
        pts := 1.0;
      pts := pts + 1.0;
    end
    else
    begin
      //leave the penalty type encoded in the position
      //just copy from Finish / Finishposition
      rp.Position := f.FinishPosition.intValue();
    end;

    Inc(divPosition);

    if (f.hasPenalty()) then
    begin
      //override basePts for 'penalty' entries
      basePts := getPenaltyPoints(f.Penalty, points, basePts);
    end;

    //if (rp = null)
    //  throw TScoringException.Create('ScoringMessageNoPoints');

    if (not positionOnly) then
    begin
      //if IsMedalRace, then double points (could also setup rp.Weigth)
      if (rp.Finish.Fleet = 0) and (rp.Race.IsFinalRace) then
        rp.Points := basePts * 2
      else if not rp.Finish.IsRacing then
        rp.Points := 0.0
      else
        rp.Points := basePts;
    end;
  end;

  if (not positionOnly) then
  begin
    // look for ties - must be done with correctedtime
    lastrp := nil;
    tied := TBaseList.Create(TBaseObject, false);
    try
      for i := 0 to points.Count-1 do
      begin
        rp := points.RacePoints[i];
        if (rp.IsTied(lastrp)) then
        begin
          // boats are tied if neither has a penalty and the current boat
          // has a valid corrected time, and its the same as the last corrected time
          if (tied.Count = 0) then
            tied.Add(lastrp);
          tied.Add(rp);
        end
        else if (tied.Count > 0) then
        begin
          // coming out of set of tied boats, reset their points and clear out
          setTiedPoints(tied);
          tied.Clear();
        end;
        lastrp := rp;
      end;
      // finally, process group of ties that still exists at end of loop
      if (tied.Count > 0) then
        setTiedPoints(tied);
    finally
      tied.Free;
    end;
  end;
end;

// <summary>
// calculates the overall series points.
// Assume that each individual race has already been calculated, and that
// throwouts have already be designated in the points objects
// </summary>
// <param name="races">list of races involved in the series</param>
// <param name="entries">list of entries to be considered in this series</param>
// <param name="points">
// list of points for all races and entries (and maybe more)
// </param>
// <param name="series">
// seriespoinstslist whose objects are updated with total value
// </param>
procedure TScoringLowPoint.scoreSeries(races: TRaceList;
  entries: TEntryList; points: TRacePointsList; series: TSeriesPointsList);
var
  e: TEntry;
  ePoints: TRacePointsList;
  tot: double;
  p: TRacePoints;
  i, j: Integer;
  sp: TSeriesPoints;
begin
  for i := 0 to series.Count-1 do
  begin
    sp := series.SeriesPoints[i];
    e := sp.Entry;
    ePoints := points.findAll(e); // list of e's finishes
    try
      tot := 0;
      for j := 0 to ePoints.Count-1 do
      begin
        p := ePoints.RacePoints[j];

        //### debug breakpoints:
//        if p.Finish = nil then
//          tot := tot
//        else if not p.Finish.IsRacing then
//           tot := tot;

        if not p.IsThrowout then
          tot := tot + p.Points;
      end;
    finally
      ePoints.Free;
    end;
    sp.Points := tot;
  end;
end;

procedure TScoringLowPoint.SetTiedPoints(const Value: TBaseList);
var
  pts: double;
  i: Integer;
  rp: TRacePoints;
begin
  pts := 0;
  for i := 0 to Value.Count-1 do
  begin
    rp := Value.Items[i] as TRacePoints;
    pts := pts + rp.Points;
  end;
  pts := pts / value.Count;
  for i := 0 to Value.Count-1 do
  begin
    rp := Value.Items[i] as TRacePoints;
    rp.Points := pts;
  end;
end;

procedure TScoringLowPoint.sortSeries(series: TSeriesPointsList);
begin
  series.sortPoints();
end;

function TScoringLowPoint.ToString: string;
begin
  result := 'ScoringLowPoint';
end;

function TScoringLowPoint.Equals(obj: TObject): Boolean;
var
  that: TScoringLowPoint;
begin
  if (not (obj is TScoringLowPoint)) then
  begin
    result := false;
    exit;
  end
  else if (self = obj) then
  begin
    result := true;
    exit;
  end;

  that := obj as TScoringLowPoint;

  if not (self.ToString = that.ToString) then
    result := false
  else if (self.fCheckinPercent <> that.fCheckinPercent) then
    result := false
  else if (self.fTimeLimitPenalty <> that.fTimeLimitPenalty) then
    result := false
  else
  begin
    if Length(self.FThrowouts) <> Length(that.FThrowouts) then
      result := false
    else
      result := true; //result := self.fThrowouts.Equals(that.fThrowouts);
  end;
end;

end.

