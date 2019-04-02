unit RiggVar.Out.FR09;

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
  RiggVar.Col.Race,
  RiggVar.BO.EventProps,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Penalty,
  RiggVar.Out.RD00;

type
  TOutput9 = class
  private
    SL: TStrings;
    ep: TEventProps;
    en: TEventNode;
    cl: TEventRowCollection;
    FPenalty: TPenaltyISAF;
    procedure DoCreateXML;
    procedure WriteRace(r: Integer);
    procedure WriteRootNode;
    procedure WriteStartlist;
    procedure WriteTimePoint(r, tp: Integer);
  public
    WantDTD: Boolean;
    WantXSL: Boolean;
    WantSingleQuotes: Boolean;
    CurrentRace: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure TimePointReport(ASL: TStrings);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Util.Classes;

constructor TOutput9.Create;
begin
  //SL := TStringList.Create;
  FPenalty := TPenaltyISAF.Create;

  WantDTD := false;
  WantXSL := true;
  WantSingleQuotes := False;
  CurrentRace := 1;
end;

destructor TOutput9.Destroy;
begin
  FPenalty.Free;
  //SL.Free;
  inherited;
end;

procedure TOutput9.TimePointReport(ASL: TStrings);
begin
  SL := ASL;
  SL.Clear;
  CurrentRace := Main.GuiManager.Race;
  DoCreateXML;
  SL := nil;
end;

procedure TOutput9.DoCreateXML;
begin
  WantXSL := Main.IniImage.WantXSL;

  SL.Add('<?xml version="1.0" encoding="utf-8"?>');
//  if WantDTD then
//  begin
//    SL.Add('<!DOCTYPE race-data SYSTEM "rvtp.dtd">');
//  end;
  if WantXSL then
  begin
    SL.Add('<?xml-stylesheet type="text/xsl" href="stylesheets/rvtp.xsl"?>');
  end;
  WriteRootNode;
  if WantSingleQuotes then
  begin
    SL.Text := StringReplace(SL.Text, '"', '''', [rfReplaceAll]);
  end;
end;

procedure TOutput9.WriteRootNode;
var
  s: string;
  slc: Integer;
begin
  ep := BO.EventProps;
  en := BO.EventNode;
  cl := en.EventRowCollection;
  slc := BO.BOParams.StartlistCount;

  s := '<race-data';
  s := s + ' version="1.0"';
  s := s + ' startlist-count="' + IntToStr(slc) + '"';
  s := s + '>';
  SL.Add(s);
  SL.Add('<updated>' + DateTimeToStr(Now) + '</updated>');
  SL.Add('<origin>riggvar-fr62-2008</origin>');
	SL.Add('<current-race>' + IntToStr(CurrentRace) + '</current-race>');

  WriteStartlist;
  //WriteRaces;
  WriteRace(CurrentRace);
  //WriteSeriesResult;
  SL.Add('</race-data>');
end;

procedure TOutput9.WriteStartlist;
var
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  SL.Add('<startlist>');
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SL.Add('<entry bib="' + IntToStr(cr.Bib) + '">');
    SL.Add('<noc>' + cr.NC + '</noc>');
    SL.Add('<crew pos="1">');
    SL.Add('<first-name>' + cr.FN + '</first-name>');
    SL.Add('<last-name>' + cr.LN + '</last-name>');
    SL.Add('</crew>');
    SL.Add('</entry>');
  end;
  SL.Add('</startlist>');
end;

//procedure TOutput9.WriteRaces;
//var
//  r: Integer;
//begin
//  for r := 1 to en.RaceCount do
//  begin
//    WriteRace(r);
//  end;
//end;

procedure TOutput9.WriteRace(r: Integer);
var
  tp: Integer;
  RaceID: string;
begin
  RaceID := IntToStr(r);
  SL.Add('<race ' +
  ' race-name="R' + RaceID + '"' +
  ' race-order="' + RaceID + '">');

  for tp := 1 to BO.BOParams.ITCount do
  begin
    WriteTimePoint(r, tp);
  end;
  WriteTimePoint(r, 0);
  //WriteRaceResult(r);
  SL.Add('</race>');
end;

procedure TOutput9.WriteTimePoint(r: Integer; tp: Integer);
var
  i: Integer;
  rn: TRaceNode;
  rl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  TimePoint: TTimePoint;
  v: string;
  mark_name: string;
  mark_time: string;
  mark_order: Integer;
begin
  rn := BO.RNode[r];
  rl := rn.RaceRowCollection;
  if tp = 0 then
  begin
    mark_name := 'Finish';
    mark_order := rn.BOParams.ITCount + 1;
  end
  else
  begin
    mark_name := 'M' + IntToStr(tp);
    mark_order := tp;
  end;
  mark_time := '';
  cr := rl.Items[rn.BestIndex[tp]];
  if Assigned(cr)  then
  begin
    TimePoint := cr.IT[tp];
    if Assigned(TimePoint) then
    begin
      mark_time := TimePoint.OTime.ToString;
    end;
  end;

  SL.Add('<time-point mark-name="' + mark_name +
  '" mark-order="' + IntToStr(mark_order) + '">');

  //SL.Add('<wind-speed></wind-speed>');
  //SL.Add('<wind-direction></wind-direction>');
  SL.Add('<time>' + mark_time + '</time>');
  for i := 0 to rl.Count-1 do
  begin
    cr := rl.Items[i];
    if cr.QU.IsOut then
      Continue;
    TimePoint := cr.IT[tp];
    v := TimePoint.Behind.ToString;
    SL.Add('<tb pos="' + IntToStr(TimePoint.PosR) +
    '" noc="' + cr.NC + '">' + v +'</tb>')
  end;
  SL.Add('</time-point>');
end;

end.
