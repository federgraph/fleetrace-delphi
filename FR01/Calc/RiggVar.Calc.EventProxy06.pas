unit RiggVar.Calc.EventProxy06;

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
  RiggVar.Scoring.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Base,
  RiggVar.Scoring.Domain,
  RiggVar.Scoring.Classes,
  RiggVar.Calc.EV,
  RiggVar.Col.Event,
  RiggVar.BO.Def,
  RiggVar.BO.EventProps;

type
  { Proxy for inline scoring module }
  TInlineCalcEventProxy = class(TCalcEventProxy)
  private
    FSL: TStrings;
    procedure InitThrowoutScheme;
    procedure CheckSeries(PL: TSeriesPointsList; ML: TStrings);
  public
    regatta: TRegatta;
    qn: TEventNode;
    eventProps: TEventProps;

    constructor Create;
    destructor Destroy; override;

    procedure LoadProxy;
    procedure UnLoadProxy;

    procedure Calc(aqn: TEventNode); override;
    procedure GetScoringNotes(SL: TStrings); override;
  end;

implementation

{ TCalcEventProxy6 }

constructor TInlineCalcEventProxy.Create;
begin
  inherited;
  FSL := TStringList.Create;
  Debug := True;
end;

destructor TInlineCalcEventProxy.Destroy;
begin
  FSL.Free;
  inherited;
end;

procedure TInlineCalcEventProxy.Calc(aqn: TEventNode);
begin
  { Note: wird beim Laden eines events zweimal aufgerufen
    einmal bei Clear, hier wird Gezeitet 0, da noch keine Daten da sind,
    dann mit OnIdle/Modified = false.
    wenn Gezeitet 0 no Ranglistenpunkte can be ausgerechnet. }

  qn := aqn;
  EventProps := BO.EventProps;

  if qn.EventRowCollection.Count = 0 then exit;

  regatta := TRegatta.Create(TScoringManager.Create);

  try
    InitThrowoutScheme;
    LoadProxy;
    regatta.ScoreRegatta;
    UnLoadProxy;

  finally
    regatta.Free;
    regatta := nil;
  end;
end;

procedure TInlineCalcEventProxy.InitThrowoutScheme;
var
  sm: TScoringManager;
  smodel: TScoringModel;
  throwouts: Integer;
  racesSailed: Integer;
begin
  if not Assigned(regatta) then exit;
  if not Assigned(qn) then exit;
  if qn.EventRowCollection.Count = 0 then exit;

  sm := regatta.ScoringManager as TScoringManager;
  if EventProps.ScoringSystem = Bonus then
    sm.ScoringModelType := ScoringModel_BonusPoint
  else if EventProps.ScoringSystem = BonusDSV then
    regatta.ScoringManager.ScoringModelType := ScoringModel_BonusPointDSV
  else
    regatta.ScoringManager.ScoringModelType := ScoringModel_LowPoint;

  smodel := sm.ScoringModel;
  smodel.FirstIs75 := EventProps.FirstIs75;
  smodel.ReorderRAF := EventProps.ReorderRAF;

  { calculate the actual count of sailed races }
  racesSailed := qn.CountRacesSailed;

  { limit throwouts to racesSailed-1 }
  throwouts := EventProps.Throwouts;
  if throwouts >= racesSailed then
    throwouts := racesSailed - 1;

  if throwouts <= 0 then
    smodel.ThrowoutScheme := THROWOUT_NONE
  else
  begin
    smodel.SetFixedNumberOfThrowouts(Throwouts);
    smodel.ThrowoutPerX := throwouts;
    smodel.ThrowoutBestX := throwouts;
    smodel.ThrowoutScheme := EventProps.ThrowoutScheme;
  end;
end;

procedure TInlineCalcEventProxy.LoadProxy;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i, j: Integer;
  e: TEntry;
  r: TRace;
  f: TFinish;
  fp: TFinishPosition;
  p: TISAFPenalty;
  er: TEventRaceEntry;
  ft: Int64;
begin
  if regatta = nil then exit;
  if qn = nil then exit;

  cl := qn.EventRowCollection;

  if qn.RaceCount >= qn.FirstFinalRace then
    regatta.IsInFinalPhase := True;

  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    cr.isGezeitet := false;
    e := TEntry.Create;
    e.SailID := cr.SNR;
    regatta.Entries.Add(e);

    for j := 1 to cr.RCount-1 do
    begin
      if (qn.PartialCalcLastRace > 0) and (j > qn.PartialCalcLastRace) then
        break;

      if i = 0 then
      begin
        r := TRace.Create(regatta, j);
        regatta.Races.Add(r);
        r.IsRacing := BO.IsRacing[j];
        r.HasFleets := cr.ru.UseFleets;
        r.TargetFleetSize := cr.ru.TargetFleetSize;
        if j >= qn.FirstFinalRace then
          r.IsFinalRace := True;
      end
      else
        r := regatta.Races.Race[j-1];
      er := cr.Race[j];
      if er.OTime = 0 then
        fp := TFinishPosition.Create(ISAF_DNC)
      else
        fp := TFinishPosition.Create(er.OTime);

      { need Finishtime ft for raceties }
      if fp.isValidFinish then
      begin
        cr.isGezeitet := true;
        ft := 1000 * er.OTime;
      end
      else
        ft := SailTime_NOTIME;

      if er.QU = 0 then
        p := nil
      else
      begin
        p := TISAFPenalty.Create(er.QU);
        p.Points := er.Penalty.Points;
        p.Percent := er.Penalty.Percent;
        p.TimePenalty := er.Penalty.TimePenalty;
      end;
      f := TFinish.Create(r, e, ft, fp, p);
      f.Fleet := er.Fleet;
      f.IsRacing := er.IsRacing;
      r.FinishList.Add(f);
    end;
  end;
  { count Gezeitet }
  j := 0;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if cr.isGezeitet then
      Inc(j);
  end;
  BO.Gezeitet := j;
end;

procedure TInlineCalcEventProxy.UnLoadProxy;
var
  e, i: Integer;

  seriesPoints: TSeriesPointsList;
  sp: TSeriesPoints;

  race: TRace;
  racepts: TRacePoints;

  eventPos: Integer;
  isTied: Boolean;

  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  crPLZ: TEventRowCollectionItem;
  ere: TEventRaceEntry;
  sm: TScoringManager;
begin
  if regatta = nil then exit;
  if qn = nil then exit;

  try
  sm := regatta.ScoringManager as TScoringManager;
  seriesPoints := sm.SeriesPointsList;
  sm.ScoringModel.SortSeries(seriesPoints);
  if Debug then
    CheckSeries(seriesPoints, CheckList);

  cl := qn.EventRowCollection;
  for e := 0 to seriesPoints.Count-1 do
  begin
    sp := seriesPoints.Items[e] as TSeriesPoints;
    cr := cl.FindKey(sp.Entry.SailID);
    if (cr = nil) or (sp = nil) then
      Continue;
    eventPos := sp.Position;
    isTied := sp.IsTied;

    cr.isTied := isTied;
    cr.Race[0].CPoints := sp.Points;
    cr.Race[0].Rank := eventPos;
    cr.Race[0].PosR := e + 1;

    crPLZ := cl.Items[e];
    if Assigned(crPLZ) then
      crPLZ.Race[0].PLZ := cr.Index;

    // loop thru races display points for each race
    for i := 0 to regatta.Races.Count-1 do
    begin
      if (qn.PartialCalcLastRace > 0) and (i > qn.PartialCalcLastRace - 1) then
        break;

      race := regatta.Races.Race[i];
      racepts := sm.RacePointsList.find(race, sp.Entry);
      ere := cr.Race[i+1];
      if Assigned(racepts) then
      begin
        try
          if IsNAN(racepts.Points) then
            ere.CPoints := 0.0
          else if not ere.IsRacing then
            ere.CPoints := 0.0
          else
            ere.CPoints := racepts.Points;
        except
          on e: EInvalidOp do
            ere.CPoints := 0.0;
        end;
        ere.Drop := racepts.IsThrowout;
      end
      else if not race.IsRacing then
      begin
        ere.CPoints := 0;
        ere.Drop := False;
      end;
    end;
  end;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy6.UnLoadProxy';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

procedure TInlineCalcEventProxy.GetScoringNotes(SL: TStrings);
var
  i: Integer;
begin
  for i := 0 to FSL.Count-1 do
    SL.Add(FSL[i]);
end;

procedure TInlineCalcEventProxy.CheckSeries(PL: TSeriesPointsList; ML: TStrings);
var
  e: Integer;
  sp: TSeriesPoints;

  isTied: Boolean;
  tied: Integer;
  points: double;
  position: Integer;
  id: string;
  sailID: Integer;

  s: string;
begin
  ML.Clear;
  for e := 0 to PL.Count-1 do
  begin
    sp := PL.Items[e] as TSeriesPoints;

    isTied := sp.IsTied;
    points := sp.Points;
    position := sp.Position;
    id := sp.Entry.ID;
    sailID := sp.Entry.SailID;

    if isTied then
      tied := 1
    else
      tied := 0;

    s := Format('%d: %s (%d) %d; %d = %.15f', [e, id, sailID, tied, position, points]);
    ML.Add(s);
  end;
end;

end.
