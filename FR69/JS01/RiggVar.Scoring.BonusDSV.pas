unit RiggVar.Scoring.BonusDSV;

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
  RiggVar.Grid.ColBase,
  RiggVar.Scoring.Domain,
  RiggVar.Scoring.Classes,
  RiggVar.Scoring.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.LowPoint;

type
  TScoringBonusPointDSV = class(TScoringLowPoint)
  public
    constructor Create;
    procedure ScoreRace(race: TRace; points: TRacePointsList; positionOnly: Boolean); override;
    function GetPenaltyPoints(p: TISAFPenalty; rpList: TRacePointsList; basePts: Double): Double; override;
  end;

implementation

{ TScoringBonusPoint }

constructor TScoringBonusPointDSV.Create;
begin
  inherited Create;
  FName := 'Bonus Point DSV';
end;

procedure TScoringBonusPointDSV.ScoreRace(race: TRace;
  points: TRacePointsList; positionOnly: Boolean);
var
  f: TFinish;
  pts: Integer;
  basePts: double;
  lastrp, rp: TRacePoints;
  i: Integer;
  tied: TBaseList;
  divPosition: Integer;
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

  pts := 0;

  // position within the divsion (as opposed to within the fleet)
  divPosition := 1;

  // loop thru the race's finishes, for each finish in the list, set the points
  for i := 0 to points.Count-1 do
  begin
    rp := points.RacePoints[i];
    f := rp.Finish;
    basePts := pts / 10;
    rp.Position := divPosition;
    Inc(divPosition);

    if (f.FinishPosition.isValidFinish and (not f.Penalty.isDsqPenalty)) then
    begin
      // increment points to be assigned to next guy if this
      // guy is a valid finisher and not disqualified
      if (pts = 0) then
        pts := 16
      else if (pts = 16) then
        pts := 29
      else if (pts = 29) then
        pts := 40
      else
        pts := pts + 10;
    end
    else
    begin
      rp.Position := f.FinishPosition.intValue();
    end;
    if (f.hasPenalty()) then
    begin
      basePts := getPenaltyPoints(f.Penalty, points, basePts);
    end;
//				if (rp = null)
//				{
//					throw TScoringException.Create('ScoringMessageNoPoints');
//				}
    if (not positionOnly) then
      rp.Points := basePts;
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
      // if processing tieds at end of loop
      if (tied.Count > 0) then
        setTiedPoints(tied);
    finally
      tied.Free;
    end;
  end;
end;

function TScoringBonusPointDSV.GetPenaltyPoints(p: TISAFPenalty;
  rpList: TRacePointsList; basePts: Double): Double;
var
  nEntries: Integer;
begin
  nEntries := 0;
  if (rpList <> nil) then
  begin
    nEntries := rpList.Count;
  end;

  result := inherited getPenaltyPoints(p, rpList, basePts);

  if (p.hasPenalty(DNS)) then
    result := nEntries
  else if (p.hasPenalty(DNC)) then
    result := nEntries
  else if (p.hasPenalty(DNF)) then
    result := nEntries
  else if p.isDsqPenalty then
    result := nEntries;

end;

end.
