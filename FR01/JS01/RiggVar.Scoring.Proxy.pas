unit RiggVar.Scoring.Proxy;

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
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Base,
  RiggVar.Scoring.Classes,
  RiggVar.Scoring.Penalty,
  RiggVar.Scoring.Domain,
  RiggVar.Calc.EventProxy00;

type
  TProxyLoader = class
  private
    FSL: TStrings;
    procedure InitThrowoutScheme;
  protected
    regatta: TRegatta;
    qn: TFRProxy;
    EventProps: TJSEventProps;
    procedure LoadProxy;
    procedure UnLoadProxy;
    procedure GetScoringNotes(SL: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Calc(proxy: TFRProxy);
  end;

procedure ScoreRegatta(proxy: TFRProxy);

implementation

procedure ScoreRegatta(proxy: TFRProxy);
var
  c: TProxyLoader;
begin
  try
    //DivisionToken := proxy.EventProps.DivisionName;
    c := TProxyLoader.Create;
    c.Calc(proxy);
    c.Free;
    //DivisionToken := '*';
  except
    on e: Exception do
      proxy.FResult := -1;
  end;
end;

{ TProxyLoader }

constructor TProxyLoader.Create;
begin
  FSL := TStringList.Create;
end;

destructor TProxyLoader.Destroy;
begin
  FSL.Free;
  inherited;
end;

procedure TProxyLoader.Calc(proxy: TFRProxy);
//var
//  rv: TReportViewer;
begin
  { Note: wird beim Laden eines events zweimal aufgerufen
    einmal bei Clear, hier wird Gezeitet 0, da noch keine Daten da sind,
    dann mit OnIdle/Modified = false.
    wenn Gezeitet 0 ist then no Ranglistenpunkte can be ausgerechnet. }

  qn := proxy;
  EventProps := qn.EventProps;

  if qn.EntryInfoCollection.Count = 0 then exit;

	regatta := TRegatta.Create(TScoringManager.Create);

  try
    InitThrowoutScheme;
    LoadProxy;
    regatta.ScoreRegatta;

    {
    rv := TReportViewer.Create(regatta);
    try
      regatta.IsFinal := False;
      rv.GetMsg(FSL); //rv.UpdateReports(false);
    finally
      rv.Free;
    end;
    }

    UnLoadProxy;

  finally
    regatta.Free;
    regatta := nil;
  end;
end;

procedure TProxyLoader.InitThrowoutScheme;
var
  sm: TScoringManager;
  smodel: TScoringModel;
  throwouts: Integer;
  r: Integer;
  IsRacingCount: Integer;
begin
  if not Assigned(regatta) then exit;
  if not Assigned(qn) then exit;

  if qn.EntryInfoCollection.Count = 0 then exit;

  sm := regatta.ScoringManager as TScoringManager;
  if EventProps.ScoringSystem = Bonus then
    sm.ScoringModelType := ScoringModel_BonusPoint
  else if EventProps.ScoringSystem = BonusDSV then
    sm.ScoringModelType := ScoringModel_BonusPointDSV
  else
    sm.ScoringModelType := ScoringModel_LowPoint;

  smodel := sm.ScoringModel;
  throwouts := qn.EventProps.Throwouts;
  smodel.FirstIs75 := EventProps.FirstIs75;

  { calculate the actual count of sailed races }
  IsRacingCount := 0;
  for r := 1 to qn.RCount-1 do
    if qn.IsRacing[r] then
      Inc(IsRacingCount);

  { limit to IsRacingCount-1 }
  if Throwouts >= IsRacingCount then
    Throwouts := IsRacingCount - 1;

  if Throwouts <= 0 then
    smodel.ThrowoutScheme := THROWOUT_NONE
  else
  begin
    smodel.setFixedNumberOfThrowouts(Throwouts);
    smodel.ThrowoutPerX := Throwouts;
    smodel.ThrowoutBestX := Throwouts;
    smodel.ThrowoutScheme := EventProps.ThrowoutScheme;
  end;
end;

procedure TProxyLoader.LoadProxy;
var
  e: TEntry;
  r: TRace;
  //d: TDivision;
  f: TFinish;
  fp: TFinishPosition;
  p: TISAFPenalty;

  i, j: Integer;

  cl: TEntryInfoCollection;
  cr: TEntryInfo;
  er: TRaceInfo;
  ft: Int64;
begin
  if regatta = nil then exit;
  if qn = nil then exit;

  cl := qn.EntryInfoCollection;

  if qn.RaceCount >= qn.FirstFinalRace then
    regatta.IsInFinalPhase := True;

  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    cr.IsGezeitet := false;
    e := TEntry.Create;
    e.SailID := cr.SNR;
    regatta.Entries.Add(e);

    for j := 1 to cr.RCount-1 do
    begin
      if i = 0 then
      begin
        r := TRace.Create(regatta, j);
        regatta.Races.Add(r);
        r.IsRacing := qn.IsRacing[j];
        r.HasFleets := qn.UseFleets;
        r.TargetFleetSize := qn.TargetFleetSize;
        if (qn.FirstFinalRace > 0) and (j >= qn.FirstFinalRace) then
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
        p.Points := er.Penalty_Points;
        p.Percent := er.Penalty_Percent;
        p.TimePenalty := er.Penalty_TimePenalty;
      end;
      f := TFinish.Create(r, e, ft, fp, p);
      f.Fleet := er.Fleet;
      if not er.IsRacing then
        f.IsRacing := false;
      r.FinishList.Add(f);
    end;
  end;
  { count Gezeitet }
  j := 0;
  for i := 0 to cl.Count-1 do
  begin
    if cl.Items[i].isGezeitet then
      Inc(j);
  end;
  qn.Gezeitet := j;
end;

procedure TProxyLoader.UnLoadProxy;
var
  e, i: Integer;

  seriesPoints: TSeriesPointsList;
  sp: TSeriesPoints;

  race: TRace;
  racepts: TRacePoints;

  eventPos: Integer;
  isTied: Boolean;

  cl: TEntryInfoCollection;
  cr: TEntryInfo;
  crPLZ: TEntryInfo;
  sm: TScoringManager;
begin
  if regatta = nil then exit;
  if qn = nil then exit;

  try
  sm := regatta.ScoringManager as TScoringManager;
  seriesPoints := sm.SeriesPointsList;
  sm.ScoringModel.sortSeries( seriesPoints);

  cl := qn.EntryInfoCollection;
  for e := 0 to seriesPoints.Count-1 do
  begin
    sp := seriesPoints.Items[e] as TSeriesPoints;
    cr := cl.FindKey(sp.Entry.SailID);
    if (cr = nil) or (sp = nil) then continue;
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
      race := regatta.Races.Race[i];
      racepts := sm.RacePointsList.find(race, sp.Entry);
      if Assigned(racepts) then
      begin
        cr.Race[i+1].CPoints := racepts.Points;
        cr.Race[i+1].Drop := racepts.IsThrowout;
      end
      else if not race.IsRacing then
      begin
        cr.Race[i+1].CPoints := 0;
        cr.Race[i+1].Drop := False;
      end;
    end;
  end;
  except
  end;
end;

procedure TProxyLoader.GetScoringNotes(SL: TStrings);
var
  i: Integer;
begin
  for i := 0 to FSL.Count-1 do
    SL.Add(FSL[i]);
end;

end.
