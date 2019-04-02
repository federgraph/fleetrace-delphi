unit RiggVar.Calc.EventProxy03;

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
  SysUtils, Classes,
  RiggVar.Calc.EventProxy00,
  RiggVar.Calc.EventProxyCall,
  RiggVar.Calc.EV,
  RiggVar.Col.Event,
  RiggVar.BO.Time,
  RiggVar.BO.Def,
  RiggVar.BO.EventProps;

type
  { Proxy for dll scoring module }
  TDLLCalcEventProxy = class(TCalcEventProxy)
  private
    FSL: TStrings;
  protected
    procedure CheckResult(p: TFRProxy);
  public
    en: TEventNode;
    p: TFRProxy;
    EventProps: TEventProps;
    constructor Create;
    destructor Destroy; override;

    procedure LoadProxy;
    procedure UnLoadProxy;
    procedure Calc(aqn: TEventNode); override;
    procedure GetScoringNotes(SL: TStrings); override;
    function GetProxyXmlInput(aqn: TEventNode): string; virtual;
    function GetProxyXmlOutput(aqn: TEventNode): string; virtual;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Calc.EventProxyWriter;

{ TDLLCalcEventProxy }

constructor TDLLCalcEventProxy.Create;
begin
  inherited Create;
  FSL := TStringList.Create;
end;

destructor TDLLCalcEventProxy.Destroy;
begin
  FSL.Free;
  inherited;
end;

procedure TDLLCalcEventProxy.Calc(aqn: TEventNode);
begin
  en := aqn;
  EventProps := BO.EventProps;

  if en.EventRowCollection.Count = 0 then exit;

  p := TFRProxy.Create;
  LoadProxy;
  ScoreRegatta(p);
  if WithTest then
  begin
    WithTest := false;
    CheckResult(p);
  end;
  UnLoadProxy;
  p.Free;
  p := nil;
end;

procedure TDLLCalcEventProxy.CheckResult(p: TFRProxy);
var
  t: TFRProxyTestWriter;
  fn: string;
  dn: string;
begin
  try
    fn := Main.FolderInfo.TracePath + EventName + '.xml';
    dn := ExtractFilePath(fn);
    if DirectoryExists(dn) then
    begin
      t := TFRProxyTestWriter.Create;
      t.WriteXml(p, fn);
      t.Free;
    end;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy3.CheckResult';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

procedure TDLLCalcEventProxy.LoadProxy;
var
  i, r: Integer;

  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  er: TEventRaceEntry;

  ri: TRaceInfo;
  ei: TEntryInfo;
  ep: TJsEventProps;

  rcount: Integer;
begin
  if en = nil then exit;

  rcount := en.RCount;
  if (en.PartialCalcLastRace > 0) and (en.PartialCalcLastRace < en.RCount - 1) then
    rcount := en.PartialCalcLastRace;

  EventName := BO.EventProps.EventName;

  ep := p.EventProps;
  ep.ScoringSystem := Integer(EventProps.ScoringSystem);
  ep.ThrowoutScheme := EventProps.ThrowoutScheme;
  ep.Throwouts := EventProps.Throwouts;
  ep.FirstIs75 := EventProps.FirstIs75;
  ep.ReorderRAF := EventProps.ReorderRAF;

  p.UseFleets := en.UseFleets;
  p.TargetFleetSize := en.TargetFleetSize;
  p.FirstFinalRace := en.FirstFinalRace;

  SetLength(p.IsRacing, rcount); // RCount = RaceCount+1

  for r := 1 to rcount-1 do
    if BO.RNode[r].IsRacing then
      p.IsRacing[r] := true
    else
      p.IsRacing[r] := false;

  cl := en.EventRowCollection;
  SetLength(p.EntryInfoCollection.Items, cl.Count);
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    ei := TEntryInfo.Create;
    p.EntryInfoCollection.Items[i] := ei;
    ei.Index := i;
    ei.SNR := cr.SNR;

    SetLength(ei.Race, rcount);
    for r := 0 to rcount-1 do
    begin

      ri := TRaceInfo.Create;
      ei.Race[r] := ri;
      if r = 0 then
        continue;

      er := cr.Race[r];
      ri.OTime := er.OTime;
      ri.QU := er.QU;
      ri.Penalty_Points := er.Penalty.Points;
      ri.Penalty_Note := er.Penalty.Note;
      ri.Penalty_Percent := er.Penalty.Percent;
      ri.Penalty_TimePenalty := er.Penalty.TimePenalty;
      ri.Fleet := er.Fleet;
      ri.IsRacing := er.IsRacing;
    end;
  end;
end;

procedure TDLLCalcEventProxy.UnLoadProxy;
var
  i, r: Integer;

  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  er: TEventRaceEntry;

  ei: TEntryInfo;
  ri: TRaceInfo;

  rcount: Integer;
begin
  if en = nil then exit;

  rcount := en.RCount;
  if (en.PartialCalcLastRace > 0) and (en.PartialCalcLastRace < en.RCount - 1) then
    rcount := en.PartialCalcLastRace;

  try

  cl := en.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if (p.EntryInfoCollection.Count = i) then
      break;
    ei := p.EntryInfoCollection.Items[i];

    cr.isTied := ei.IsTied;
    cr.isGezeitet := ei.IsGezeitet;

    if ei.RCount < rcount then
      break;
    for r := 0 to rcount-1 do
    begin
      ri := ei.Race[r];
      er := cr.Race[r];
      er.CPoints := ri.CPoints;
      er.Drop := ri.Drop;
      er.Rank := ri.Rank;
      er.PosR := ri.PosR;
      er.PLZ := ri.PLZ;
    end;
  end;

  BO.Gezeitet := p.Gezeitet;

  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy3.UnLoadProxy';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

procedure TDLLCalcEventProxy.GetScoringNotes(SL: TStrings);
//var
//  i: Integer;
begin
//  for i := 0 to FSL.Count-1 do
//    SL.Add(FSL[i]);
end;

function TDLLCalcEventProxy.GetProxyXmlInput(aqn: TEventNode): string;
var
  t: TFRProxyTestWriter;
begin
  en := aqn;
  EventProps := BO.EventProps;

  if en.EventRowCollection.Count = 0 then exit;

  t := nil;
  p := TFRProxy.Create;
  try
    LoadProxy;
    t := TFRProxyTestWriter.Create;
    result := t.WriteXml1(p);
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy3.GetProxyXml1';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
  p.Free;
  p := nil;
  t.Free;
end;

function TDLLCalcEventProxy.GetProxyXmlOutput(aqn: TEventNode): string;
var
  t: TFRProxyTestWriter;
begin
  en := aqn;
  EventProps := BO.EventProps;
  if en.EventRowCollection.Count = 0 then exit;

  t := nil;
  p := TFRProxy.Create;
  try
    LoadProxy;
    ScoreRegatta(p); //<-- added line (not in GetProxyXmlInput)
    t := TFRProxyTestWriter.Create;
    result := t.WriteXml1(p);
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy3.GetProxyXmlOutput';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
  p.Free;
  p := nil;
  t.Free;
end;

end.
