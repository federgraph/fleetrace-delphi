unit RiggVar.Out.RD01;

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
  RiggVar.Out.RD00,
  RiggVar.DAL.Redirector,
  Forms;

type
  TRaceDataXMLWriter = class(IRaceDataXMLWriter)
  private
    procedure WriteTimePoint(r: Integer; tp: Integer);
    procedure WriteRaceResult(r: Integer);
    procedure WriteSeriesResult;
  protected
    ep: TEventProps;
    en: TEventNode;
    cl: TEventRowCollection;

    SL: TDBStringList;
    FPenalty: TPenaltyISAF;

    WantSingleQuotes: Boolean;

    procedure DoCreateXML;
    procedure WriteRootNode; virtual;
    procedure WriteStartlist;
    procedure WriteRace; virtual;
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

{ TRaceDataXMLWriter }

constructor TRaceDataXMLWriter.Create;
begin
  inherited Create;

  SL := TDBStringList.Create;
  FPenalty := TPenaltyISAF.Create;

  WantSingleQuotes := False;
end;

destructor TRaceDataXMLWriter.Destroy;
begin
  FPenalty.Free;
  SL.Free;
  inherited;
end;

procedure TRaceDataXMLWriter.WriteXML;
var
  fn: string;
begin
  SL.Clear;
  DoCreateXML;

  { save locally }
  fn := Main.FolderInfo.TracePath + '_RD.xml';
  Main.StoreAdapter.StringListSaveToFileUTF8(SL, fn);

  { and also in deploy directory }
  if (Main.Params.UseDB = false) then
    Main.StoreAdapter.StringListSaveIfDirExistsUTF8(SL, Dir, Dir + ep.EventName + Extension);
end;

function TRaceDataXMLWriter.ToString: string;
begin
  SL.Clear;
  DoCreateXML;

  result := SL.Text;
end;

procedure TRaceDataXMLWriter.GetXML(Memo: TStrings);
begin
  SL.Clear;
  DoCreateXML;

  Memo.Assign(SL);
  SL.Clear;
end;

procedure TRaceDataXMLWriter.DoCreateXML;
begin
  //SL.Add('<?xml version="1.0" encoding="ISO-8859-1"?>');
  SL.Add('<?xml version="1.0" encoding="utf-8"?>');
  SL.Add('<!DOCTYPE race-data SYSTEM "race-data.dtd">');
  WriteRootNode;
  if WantSingleQuotes then
    SL.Text := StringReplace(SL.Text, '"', '''', [rfReplaceAll]);
end;

procedure TRaceDataXMLWriter.WriteRootNode;
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
  WriteStartlist;
  WriteRace;
  WriteSeriesResult;
  SL.Add('</race-data>');
end;

procedure TRaceDataXMLWriter.WriteStartlist;
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

procedure TRaceDataXMLWriter.WriteRace;
var
  r: Integer;
  tp: Integer;
  RaceID: string;
begin
  for r := 1 to en.RaceCount do
  begin
    RaceID := IntToStr(r);
    SL.Add('<race ' +
    ' race-name="R' + RaceID + '"' +
    ' race-order="' + RaceID + '">');

    for tp := 1 to BO.BOParams.ITCount do
    begin
      WriteTimePoint(r, tp);
    end;
    WriteRaceResult(r);
    SL.Add('</race>');
  end;
end;

procedure TRaceDataXMLWriter.WriteTimePoint(r: Integer; tp: Integer);
var
  i: Integer;
  rn: TRaceNode;
  rl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  TimePoint: TTimePoint;
  v: string;
  mark_name: string;
  mark_time: string;
begin
  rn := BO.RNode[r];
  rl := rn.RaceRowCollection;
  mark_name := 'M' + IntToStr(tp);
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
  '" mark-order="' + IntToStr(tp) + '">');

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

procedure TRaceDataXMLWriter.WriteRaceResult(r: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
  p: Integer;
  v: string;
begin
  SL.Add('<race-result>');
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    p := cr.Race[r].OTime;
    v := cr.RaceValue[r];
    SL.Add('<rp bib="' + IntToStr(cr.Bib) +
    '" pos="' + IntToStr(p) +
    '" noc="' + cr.NC + '">' + v +'</rp>')
  end;
  SL.Add('</race-result>');
end;

procedure TRaceDataXMLWriter.WriteSeriesResult;
var
  i: Integer;
  cr: TEventRowCollectionItem;
begin
  SL.Add('<series-result>');
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    SL.Add('<sp bib="' + IntToStr(cr.Bib) +
    '" pos="' + IntToStr(cr.GRank) +
    '" noc="' + cr.NC + '">' + cr.GPoints +'</sp>');
  end;
  SL.Add('</series-result>');
end;

end.
