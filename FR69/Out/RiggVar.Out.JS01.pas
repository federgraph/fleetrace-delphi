unit RiggVar.Out.JS01;

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
  RiggVar.Col.Event,
  RiggVar.BO.EventProps,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Penalty,
  RiggVar.Out.JS00,
  RiggVar.DAL.Redirector;

type
  TReportOptions = record
	  FontName: string;
    FontSize: Integer;
    HidePenaltyPoints: Boolean;
    LastXRaces: Integer;
    NameOption: Integer;
    Show1DTimes: Boolean;
    ShowLastXRaces: Boolean;
    TabName: string;
    Template: string;
  	LO1: string;
	  LO2: string;
  	LO3: string;
    LO4: string;
    LO5: string;
    LO6: string;
    LO7: string;
    LO8: string;
  end;

  TJavaScoreXMLWriter = class(IJavaScoreXMLWriter)
  private
    procedure WriteRaceDivInfo(r: Integer);
    procedure WriteFinishList(r: Integer);
  protected
    ep: TEventProps;
    en: TEventNode;
    cl: TEventRowCollection;

    SL: TStrings;
    FPenalty: TPenaltyISAF;

    WantSingleQuotes: Boolean;
    UseBowNumbers: Boolean;

    function StartsWith(const t, s: string): Boolean;
    function StartDate(r: Integer): string;
    function FinishTime(t: Integer): string;
    function StartTimeRace(r: Integer): string;

    procedure DoCreateXML;
    procedure WriteRegatta; virtual;
    procedure WriteDivisionList;
    procedure WriteEntryList;
    procedure WriteFleetList;
    procedure WriteSubDivisionList; virtual;
    procedure WriteRaceList; virtual;
    procedure WriteScoringSystem;
    procedure WriteReportOptionList;
    procedure WriteReportOption(ro: TReportOptions);
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetXML(Memo: TStrings); override;
    procedure WriteXML; override;
    function ToString: string; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

const
  BoolStr: array[Boolean] of string = ('false', 'true');

{ TJavaScoreXMLWriter }

constructor TJavaScoreXMLWriter.Create;
begin
  inherited Create;

  SL := TDBStringList.Create;
  FPenalty := TPenaltyISAF.Create;

  WantSingleQuotes := False;
  UseBowNumbers := False;
end;

destructor TJavaScoreXMLWriter.Destroy;
begin
  FPenalty.Free;
  SL.Free;
  inherited;
end;

procedure TJavaScoreXMLWriter.WriteXML;
var
  fn: string;
begin
  SL.Clear;
  DoCreateXML;

  { save locally }
  fn := Main.FolderInfo.TracePath + '_JS.xml';
  Main.StoreAdapter.StringListSaveToFile(SL, fn);

  { and also in deploy directory }
  if (Main.Params.UseDB = false) then
    Main.StoreAdapter.StringListSaveIfDirExists(SL, Dir, Dir + ep.EventName + Extension);
end;

function TJavaScoreXMLWriter.ToString: string;
begin
  SL.Clear;
  DoCreateXML;

  result := SL.Text;
end;

procedure TJavaScoreXMLWriter.GetXML(Memo: TStrings);
begin
  SL.Clear;
  DoCreateXML;

  Memo.Assign(SL);
  SL.Clear;
end;

procedure TJavaScoreXMLWriter.DoCreateXML;
begin
  SL.Add('<?xml version="1.0" encoding="ISO-8859-1"?>');
  WriteRegatta;
  if WantSingleQuotes then
    SL.Text := StringReplace(SL.Text, '"', '''', [rfReplaceAll]);
end;

procedure TJavaScoreXMLWriter.WriteRegatta;
var
  s: string;
begin
  ep := BO.EventProps;
  en := BO.EventNode;
  cl := en.EventRowCollection;

  DivisionInfo.DivisionName := ep.DivisionName;

  s := '<Regatta';
  s := s + ' Dates="' +  ep.EventDates + '"';
  s := s + ' Final="false"';
  s := s + ' HostClub="' +  ep.HostClub + '"';
  s := s + ' JSVersion="5.2"';
  s := s + ' JuryChair="' +  ep.JuryHead + '"';
  s := s + ' Name="' + ep.EventName + '"';
  s := s + ' Pro="' + ep.PRO + '"';
  s := s + ' SplitFleet="' +  BoolStr[ep.UseFleets] + '"';
  s := s + ' UseBowNumbers="' +  BoolStr[UseBowNumbers] + '"';
  s := s + ' Version="521"';
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

procedure TJavaScoreXMLWriter.WriteDivisionList;
begin
  SL.Add('<DivisionList>');
  SL.Add('<Division DivName="' + DivisionInfo.DivisionName + '">');
  SL.Add('<MinRating ClassName="' + DivisionInfo.DivisionName + '" System="OneDesign"></MinRating>');
  SL.Add('<MaxRating ClassName="' + DivisionInfo.DivisionName + '" System="OneDesign"></MaxRating>');
  SL.Add('</Division>');
  SL.Add('</DivisionList>');
end;

procedure TJavaScoreXMLWriter.WriteEntryList;
var
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  SL.Add('<EntryList>');
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SL.Add('<Entry Bow="' + IntToStr(cr.Bib) + '" Division="' + DivisionInfo.DivisionName + '" EntryId="' + IntToStr(cr.BaseID) + '">');
    SL.Add('  <Boat SailId="' + IntToStr(cr.SNR) + '">');
    //SL.Add('  <Owner FirstName="" LastName="" SailorId=""></Owner>');
    SL.Add('  <RatingList>');
    SL.Add('     <Rating ClassName="' + DivisionInfo.DivisionName + '" System="OneDesign"></Rating>');
    SL.Add('  </RatingList>');
    SL.Add('  </Boat>');
    SL.Add('  <Skipper FirstName="' + cr.FN + '" LastName="' + cr.LN + '" SailorId=""></Skipper>');
    SL.Add('<CrewList>');
    SL.Add('  <Crew FirstName="' + cr.GR +
                '" LastName="' + cr.PB +
                '" SailorId="">' +
              '</Crew>');
    SL.Add('</CrewList>');
    SL.Add('</Entry>');
  end;
  SL.Add('</EntryList>');
end;

procedure TJavaScoreXMLWriter.WriteFleetList;
begin
  SL.Add('<FleetList></FleetList>');
end;

procedure TJavaScoreXMLWriter.WriteSubDivisionList;
begin
  SL.Add('<SubDivisionList>');
  SL.Add('</SubDivisionList>');
end;

procedure TJavaScoreXMLWriter.WriteRaceList;
var
  r: Integer;
  RaceID: string;
begin
  SL.Add('<RaceList>');
  for r := 1 to en.RaceCount do
  begin
    RaceID := IntToStr(r);
    SL.Add('<Race ' +
    ' BFactor="550" Comment=""' +
    ' LongDistance="false"' +
    ' Name="' + RaceID + '"' +
    ' NonDiscardable="false"' +
    ' RaceId="' + RaceID + '"' +
    ' StartDate="' + StartDate(r) + '"' +
    ' Weight="1.0">');
    WriteRaceDivInfo(r);
    WriteFinishList(r);
    SL.Add('</Race>');
  end;
  SL.Add('</RaceList>');
end;

procedure TJavaScoreXMLWriter.WriteRaceDivInfo(r: Integer);
var
  fn: string;
begin
  SL.Add('<DivInfo>');
  fn := ep.DivisionName;
  SL.Add('  <DivStart' +
  ' Div="' + fn + '"' +
  ' Length="1.0"' +
  ' StartTime="' + StartTimeRace(r) + '"' +
  ' isRacing="' + BoolStr[en.IsRacing(r)] + '"' +
  '></DivStart>');
  SL.Add('</DivInfo>');
end;

procedure TJavaScoreXMLWriter.WriteFinishList(r: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
  re: TEventRaceEntry;
  Ent: string;
  Race: string;
  Pos: string;
  Penalty: string;
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

    Penalty := FPenalty.ToString;
    if Penalty = '' then
    begin
      if re.OTime > 0 then
      begin
        SL.Add('<Fin' +
        ' Ent="' + Ent + '"' +
        ' Pos="' + Pos + '"' +
        ' Race="' + Race + '"' +
        '></Fin>')
      end;
    end
    else
    begin
      SL.Add('<Fin Ent="' + Ent + '" Pos="' + Pos + '" Race="' + Race + '">');
      SL.Add('  <Penalty Penalty="' + Penalty + '"></Penalty>');
      SL.Add('</Fin>');
    end;
  end;
  SL.Add('</FinishList>');
end;

procedure TJavaScoreXMLWriter.WriteScoringSystem;
//var
//  racesSailed: Integer;
//  throwouts: Integer;
//  s: string;
begin
//  { calculate the actual count of sailed races }
//  racesSailed := en.CountRacesSailed;
//
//  { limit throwouts to racesSailed-1 }
//  throwouts := ep.Throwouts;
//  if throwouts >= racesSailed then
//    throwouts := racesSailed - 1;

//  if throwouts <= 0 then
//    smodel.ThrowoutScheme := THROWOUT_NONE
//  else
//  begin
//    smodel.SetFixedNumberOfThrowouts(Throwouts);
//    smodel.ThrowoutPerX := throwouts;
//    smodel.ThrowoutBestX := throwouts;
//    smodel.ThrowoutScheme := EventProps.ThrowoutScheme;
//  end;

  if en.UseFleets then
    SL.Add('<Scores SystemName="ISAF Low Point Medal">')
  else
    SL.Add('<Scores SystemName="ISAF Low Point 2005-2008">');

	SL.Add('<System CheckinPercent="20" LongSeries="false"');
  SL.Add(' Throwout1="2" Throwout2="0" Throwout3="0"');
  SL.Add(' ThrowoutBestX="0" ThrowoutPerX="0" ThrowoutScheme="1"');
  if not ep.ReorderRAF then
    SL.Add(' TimeLimitPenalty="0" tiebreaker="1" ReorderRAF="' + BoolStr[ep.ReorderRAF] + '">')
  else
    SL.Add(' TimeLimitPenalty="0" tiebreaker="1">');
  SL.Add('</System>');

  SL.Add('</Scores>');
end;

procedure TJavaScoreXMLWriter.WriteReportOptionList;
var
  ro: TReportOptions;
begin
  SL.Add('<ReportOptionList>');

	ro.FontName := 'Verdana';
  ro.FontSize := 2;
  ro.HidePenaltyPoints := false;
  ro.LastXRaces := 5;
  ro.NameOption := 0;
  ro.Show1DTimes := true;
  ro.ShowLastXRaces := false;
  ro.TabName :='Series';
  ro.Template := '';
	ro.LO1 := '2.1';
  ro.LO2 := 'none';
  ro.LO3 := '1.1';
  ro.LO4 := 'none';
  ro.LO5 := 'none';
  ro.LO6 := 'none';
  ro.LO7 := 'none';
  ro.LO8 := 'none';
  WriteReportOption(ro);

	ro.FontName := 'Verdana';
  ro.FontSize := 2;
  ro.HidePenaltyPoints := false;
  ro.LastXRaces := 5;
  ro.NameOption := 0;
  ro.Show1DTimes := true;
  ro.ShowLastXRaces := false;
  ro.TabName :='Race';
  ro.Template := '';
	ro.LO1 := '2.1';
  ro.LO2 := 'none';
  ro.LO3 := '1.1';
  ro.LO4 := 'none';
  ro.LO5 := 'none';
  ro.LO6 := 'none';
  ro.LO7 := 'none';
  ro.LO8 := 'none';
  WriteReportOption(ro);

	ro.FontName := 'Verdana';
  ro.FontSize := 2;
  ro.HidePenaltyPoints := false;
  ro.LastXRaces := 5;
  ro.NameOption := 0;
  ro.Show1DTimes := true;
  ro.ShowLastXRaces := false;
  ro.TabName :='Entries';
  ro.Template := '';
	ro.LO1 := '2.1';
  ro.LO2 := 'none';
  ro.LO3 := '1.1';
  ro.LO4 := 'none';
  ro.LO5 := 'none';
  ro.LO6 := 'none';
  ro.LO7 := 'none';
  ro.LO8 := 'none';
  WriteReportOption(ro);

	ro.FontName := 'Verdana';
  ro.FontSize := 2;
  ro.HidePenaltyPoints := false;
  ro.LastXRaces := 5;
  ro.NameOption := 0;
  ro.Show1DTimes := true;
  ro.ShowLastXRaces := false;
  ro.TabName :='Checkin';
  ro.Template := '';
	ro.LO1 := '2.1';
  ro.LO2 := 'none';
  ro.LO3 := '1.1';
  ro.LO4 := 'none';
  ro.LO5 := 'none';
  ro.LO6 := 'none';
  ro.LO7 := 'none';
  ro.LO8 := 'none';
  WriteReportOption(ro);

	ro.FontName := 'Verdana';
  ro.FontSize := 2;
  ro.HidePenaltyPoints := false;
  ro.LastXRaces := 5;
  ro.NameOption := 0;
  ro.Show1DTimes := true;
  ro.ShowLastXRaces := false;
  ro.TabName :='FinishSheet';
  ro.Template := '';
	ro.LO1 := '2.1';
  ro.LO2 := 'none';
  ro.LO3 := '1.1';
  ro.LO4 := 'none';
  ro.LO5 := 'none';
  ro.LO6 := 'none';
  ro.LO7 := 'none';
  ro.LO8 := 'none';
  WriteReportOption(ro);

  ro.TabName :='Proofing';
	ro.FontName := 'Verdana';
  ro.FontSize := 2;
  ro.HidePenaltyPoints := false;
  ro.LastXRaces := 5;
  ro.NameOption := 0;
  ro.Show1DTimes := true;
  //ro.ShowLastXRaces := false;
  ro.Template := '';
	ro.LO1 := '2.1';
  ro.LO2 := 'none';
  ro.LO3 := '1.1';
  ro.LO4 := 'none';
  ro.LO5 := 'none';
  ro.LO6 := 'none';
  ro.LO7 := 'none';
  ro.LO8 := 'none';
  WriteReportOption(ro);

  SL.Add('</ReportOptionList>');
end;

procedure TJavaScoreXMLWriter.WriteReportOption(ro: TReportOptions);
begin
  SL.Add('<Options FontName="' + ro.FontName + '"' +
  ' FontSize="' + IntToStr(ro.FontSize) + '"' +
  ' HidePenaltyPoints="' + BoolStr[ro.HidePenaltyPoints] + '"' +
  ' LastXRaces="' + IntToStr(ro.LastXRaces) + '"' +
  ' NameOption="' + IntToStr(ro.NameOption) + '"' +
  ' Show1DTimes="' + BoolStr[ro.Show1DTimes] + '"' +
  ' ShowLastXRaces="' + BoolStr[ro.ShowLastXRaces] + '"' +
  ' TabName="' + ro.TabName + '"' +
  ' Template="' + ro.Template + '">');
	SL.Add('<OptionLocations>');
	SL.Add('  <item value="' + ro.LO1 + '"></item>');
	SL.Add('  <item value="' + ro.LO2 + '"></item>');
	SL.Add('  <item value="' + ro.LO3 + '"></item>');
	SL.Add('  <item value="' + ro.LO4 + '"></item>');
	SL.Add('  <item value="' + ro.LO5 + '"></item>');
	SL.Add('  <item value="' + ro.LO6 + '"></item>');
	SL.Add('  <item value="' + ro.LO7 + '"></item>');
	SL.Add('  <item value="' + ro.LO8 + '"></item>');
    SL.Add('</OptionLocations>');
	SL.Add('</Options>');
end;

function TJavaScoreXMLWriter.FinishTime(t: Integer): string;
var
  dt: TDateTime;
begin
  dt := 0;
  dt := IncHour(dt, 9);
  dt := IncSecond(dt, t);
  result := FormatDateTime('hh:mm:ss.z', dt);
end;

function TJavaScoreXMLWriter.StartDate(r: Integer): string;
begin
  result := Format('%.2d-Jul-2007', [r]);
end;

function TJavaScoreXMLWriter.StartsWith(const t, s: string): Boolean;
begin
  if (s <> '') and (t <> '') and (Length(t) <= Length(s)) then
  begin
    result := Pos(t, s) = 1;
    result := result and (Copy(s, 1, Length(t)) = t);
  end
  else
    result := false;
end;

function TJavaScoreXMLWriter.StartTimeRace(r: Integer): string;
var
  dt: TDateTime;
begin
  dt := 0;
  dt := IncHour(dt, 9);
  dt := IncMinute(dt, r * 5);
  result := FormatDateTime('hh:mm:ss', dt);
end;

end.
