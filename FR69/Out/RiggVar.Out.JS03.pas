unit RiggVar.Out.JS03;

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
  System.DateUtils,
  RiggVar.Grid.ColBase,
  RiggVar.Col.Event,
  RiggVar.BO.EventProps,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Penalty,
  RiggVar.Out.JS01;

type
  TJavaScoreXMLWriter03 = class(TJavaScoreXMLWriter)
  private
    CRList: TCRList;
    procedure WriteRaceDivInfo(r: Integer);
    procedure WriteFinishList(r: Integer);
  protected
    procedure WriteRegatta; override;
    procedure WriteSubDivisionList; override;
    procedure WriteRaceList; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowSchedule; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BO.StartInfo,
  RiggVar.BO.FormAdapter;

const
  BoolStr: array[Boolean] of string = ('false', 'true');

{ TJavaScoreXMLWriter03 }

constructor TJavaScoreXMLWriter03.Create;
begin
  inherited Create;
  CRList := TCRList.Create(false);
end;

destructor TJavaScoreXMLWriter03.Destroy;
begin
  CRList.Free;
  inherited;
end;

procedure TJavaScoreXMLWriter03.ShowSchedule;
begin
  ep := BO.EventProps;
  en := BO.EventNode;
  cl := en.EventRowCollection;

  DivisionInfo.Init;

  Main.FormAdapter.EditSchedule(self);
end;

procedure TJavaScoreXMLWriter03.WriteRegatta;
var
  s: string;
begin
  ep := BO.EventProps;
  en := BO.EventNode;
  cl := en.EventRowCollection;

  if ep.UseFleets then
    DivisionInfo.Init
  else
    DivisionInfo.Clear;

  s := '<Regatta';
  s := s + ' Dates="' +  ep.EventDates + '"';
  s := s + ' Final="false"';
  s := s + ' HostClub="' +  ep.HostClub + '"';
  s := s + ' JSVersion="5.2"';
  s := s + ' JuryChair="' +  ep.JuryHead + '"';
  s := s + ' Name="' + ep.EventName + '"';
  s := s + ' Pro="' + ep.PRO + '"';
  s := s + ' SplitFleet="' +  BoolStr[ep.UseFleets] + '"';

  if en.UseFleets and (en.FirstFinalRace > 0) then
  s := s + ' FirstFinalRace="' +  IntToStr(en.FirstFinalRace) + '"';

  if en.UseFleets and (en.TargetFleetSize <> 0) then
  s := s + ' TargetFleetSize="' +  IntToStr(en.TargetFleetSize) + '"';

  s := s + ' UseBowNumbers="' +  BoolStr[UseBowNumbers] + '"';
  s := s + ' Version="521"';
  if ep.UseFleets then
    s := s + ' lastRaceBeforeSplit="' +  IntToStr(ep.FirstFinalRace - 1) + '"';
  s := s + '>';
  SL.Add(s);

  WriteDivisionList;
  WriteEntryList;
  WriteFleetList;
  WriteSubDivisionList;
  WriteRaceList;
  WriteScoringSystem;
  //WriteReportOptionList;

  SL.Add('</Regatta>');
end;

procedure TJavaScoreXMLWriter03.WriteSubDivisionList;
var
  s: string;
  cr: TEventRowCollectionItem;
  L: TArrayList;
  i, j: Integer;
  fi: TStartInfo;
begin
  if en.UseFleets then
  begin
    SL.Add('<SubDivisionList>');
    L := TArrayList.Create;
    for i := 0 to DivisionInfo.FleetCount-1 do
    begin
      fi := DivisionInfo.FleetInfo[i];
      s := '<SubDivision DivName="' + fi.FleetName + '"';
      if fi.IsFinalFleet then
        s := s + ' Group="Final"'
      else
        s := s + ' Group="Qual"';
      s := s + ' GroupRank="' + fi.GroupRank + '"';
      s := s + ' Monopoly="false"';
      s := s + ' Parent="' +  ep.DivisionName + '"';
      s := s + ' ParentType="Division"';
      s := s + ' ScoreSeparately="false"';
      s := s + '>';
      SL.Add(s);
      SL.Add('  <EntryList>');
      L.Clear;
      en.FillFleetList(fi.Race, fi.Fleet, L);
      for j := 0 to L.Count - 1 do
      begin
        cr := TEventRowCollectionItem(L.Items[j]);
        s := '    <Entry Id="' + IntToStr(cr.BaseID) + '"></Entry>';
        SL.Add(s);
      end;
      SL.Add('  </EntryList>');
      SL.Add('</SubDivision>');
    end;
    L.Free;
    SL.Add('</SubDivisionList>');
  end
  else
    inherited WriteSubDivisionList;
end;

procedure TJavaScoreXMLWriter03.WriteRaceList;
var
  i: Integer;
  ri: TRaceInfo;
begin
  if ep.UseFleets then
  begin
    SL.Add('<RaceList>');
    for i := 0 to DivisionInfo.RaceCount - 1 do
    begin
      ri := DivisionInfo.RaceInfo[i];
      SL.Add('<Race ' +
      ' BFactor="550" Comment=""' +
      ' LongDistance="false"' +
      ' Name="' + ri.RaceName + '"' +
      ' NonDiscardable="' + BoolStr[ri.Owner.NonDiscardable] + '"' +
      ' RaceId="' + IntToStr(i + 1) + '"' +
      ' StartDate="' + StartDate(ri.Race) + '"' +
      ' Weight="' + ri.Owner.Weight + '">');
      WriteRaceDivInfo(ri.Race);
      WriteFinishList(ri.Race);
      SL.Add('</Race>');
    end;
    SL.Add('</RaceList>');
  end
  else
    inherited WriteRaceList;
end;

procedure TJavaScoreXMLWriter03.WriteRaceDivInfo(r: Integer);
var
  i: Integer;
  fi: TStartInfo;
  si: TStartInfo;
  s: string;
begin
  SL.Add('<DivInfo>');
  SL.Add('  <DivStart' +
  ' Div="' + ep.DivisionName + '"' +
  ' Length="1.0"' +
  ' StartTime="No Time"' +
  ' isRacing="false"' +
  '></DivStart>');
  for i := 0 to DivisionInfo.FleetCount - 1 do
  begin
    fi := DivisionInfo.FleetInfo[i];
    si := DivisionInfo.FindStartInfoByName(r, fi.Fleet, fi.FleetName);
    if Assigned(si) and fi.IsRacing then
    begin
      s := '  <DivStart' +
      ' Div="' + si.FleetName + '"' +
      ' Length="' + si.Length + '"' +
      ' StartTime="' + si.StartTime + '"' +
      ' isRacing="true"' +
      '></DivStart>';
      SL.Add(s);
    end
    else
    begin
      SL.Add('  <DivStart' +
      ' Div="' + fi.FleetName + '"' +
      ' Length="1.0"' +
      ' StartTime="No Time"' +
      ' isRacing="false"' +
      '></DivStart>')
    end;
  end;
  SL.Add('</DivInfo>');
end;

procedure TJavaScoreXMLWriter03.WriteFinishList(r: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
  re: TEventRaceEntry;
  Ent: string;
  Race: string;
  Pos: string;
  Penalty: string;
  s: string;
begin
  SL.Add('<FinishList>');
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    re := cr.Race[r];
    FPenalty.AsInteger := re.QU;

    Ent := IntToStr(cr.BaseID);
    Race := IntToStr(r);
    Pos := IntToStr(re.OTime);
    if (re.OTime = 0) or (FPenalty.PenaltyNoFinish <> NoFinishBlank) then
      Pos := PenaltyNoFinishStrings[FPenalty.PenaltyNoFinish];
    if Pos = '' then //e.g FinishPosition = 0 and DPI
      Pos := 'DNC'; //'NoFin'; //DNF


    Penalty := re.Penalty.ToString;
    s := '<Fin';
    s := s + ' Ent="' + Ent + '"';
    s := s + ' Pos="' + Pos + '"';
    s := s + ' Race="' + Race + '"';
    if re.Fleet <> 1  then
      s := s + ' Fleet="' + IntToStr(re.Fleet) + '"';
    if re.IsRacing = false  then
      s := s + ' IsRacing="' + BoolStr[re.IsRacing] + '"';
    s := s + '>';
    SL.Add(s);

    if Penalty <> '' then
    begin
      s := '  <Penalty';
      s := s + ' Penalty="' + FPenalty.Get3LA + '"'; //3LA, ohne Anhang
      if re.Penalty.Points > 0.001 then
        s := s + ' Points="' +  FormatFloat('0.0#', re.Penalty.Points) + '"';
      if re.Penalty.Percent > 0 then
        s := s + ' Percent="' +  IntToStr(re.Penalty.Percent) + '"';
      if re.Penalty.TimePenalty > 0 then
        ;
      s := s + '></Penalty>';
      SL.Add(s);
    end;

    SL.Add('</Fin>');
  end;
  SL.Add('</FinishList>');
end;

end.
