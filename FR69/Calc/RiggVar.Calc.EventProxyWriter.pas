unit RiggVar.Calc.EventProxyWriter;

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
  RiggVar.Util.Classes,
  RiggVar.Calc.EventProxy00;

type
  TFRProxyTestWriter = class
  private
    p: TFRProxy;
    SLA: TStrings;
    SL: TStrings;
    FormatSettings: TFormatSettings;
    procedure AddA(name, value: string);
    function AttributeString: string;
    procedure WriteElement(name: string);
    procedure WriteEndElement(name: string);
    procedure WriteStartElement(name: string);
    procedure ReadProxyAttributs;
    procedure ReadEventProps;
    procedure WriteEntry(ei: TEntryInfo);
    procedure WriteRace(ri: TRaceInfo; Index: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteXml(aProxy: TFRProxy; aFileName: string);
    procedure WriteXml2(aProxy: TFRProxy; Memo: TStrings);
    function WriteXml1(aProxy: TFRProxy): string;
    procedure WriteXml0(aProxy: TFRProxy);
  end;

implementation

uses
  RiggVar.App.Main;

{ TFRProxyTestWriter }

constructor TFRProxyTestWriter.Create;
begin
  inherited Create;
  SLA := TStringList.Create;
  SL := TStringList.Create;
  FormatSettings.DecimalSeparator := '.';
end;

destructor TFRProxyTestWriter.Destroy;
begin
  SLA.Free;
  SL.Free;
  inherited;
end;

procedure TFRProxyTestWriter.WriteXml0(aProxy: TFRProxy);
var
  i: Integer;
  ei: TEntryInfo;
begin
  if aProxy = nil then exit;
  p := aProxy;
  SLA.Clear;
  SL.Clear;
  SL.Add('<?xml version="1.0" ?>');
  ReadProxyAttributs;
  WriteStartElement(p_FRProxy);

  ReadEventProps;
  WriteElement(p_EventProps);

  AddA(p_RCount, IntToStr(p.RCount));
  WriteStartElement(p_RaceProps);
  for i := 0 to p.RCount-1 do
  begin
    if p.IsRacing[i] = false then
    begin
      AddA(p_Index, IntToStr(i));
      AddA(p_IsRacing, BoolStr[false]);
      WriteElement(p_Race);
    end;
  end;
  WriteEndElement(p_RaceProps);

  for i := 0 to p.EntryInfoCollection.Count-1 do
  begin
    ei := p.EntryInfoCollection.Items[i];
    WriteEntry(ei);
  end;

  WriteEndElement(p_FRProxy);
end;

procedure TFRProxyTestWriter.WriteEntry(ei: TEntryInfo);
var
  i: Integer;
  ri: TRaceInfo;
begin
  AddA(p_Index, IntToStr(ei.Index));
  AddA(p_SNR, IntToStr(ei.SNR));
  if ei.IsGezeitet = false then
    AddA(p_IsGezeitet, BoolStr[ei.IsGezeitet]);
  if ei.IsTied then
    AddA(p_IsTied, BoolStr[ei.IsTied]);
  WriteStartElement(p_Entry);
  for i := 0 to p.RCount-1 do
  begin
    ri := ei.Race[i];
    WriteRace(ri, i);
  end;
  WriteEndElement(p_Entry);
end;

procedure TFRProxyTestWriter.WriteRace(ri: TRaceInfo; Index: Integer);
begin
  AddA(p_Index, IntToStr(Index));

  //Input
  if ri.OTime <> 0 then
    AddA(p_OTime, IntToStr(ri.OTime));
  if ri.QU <> 0 then
    AddA(p_QU, IntToStr(ri.QU));
  if ri.Penalty_Points <> 0.0 then
    AddA(p_Penalty_Points, FloatToStr(ri.Penalty_Points, FormatSettings));
  if ri.Penalty_Note <> '' then
    AddA(p_Penalty_Note, ri.Penalty_Note);
  if ri.Penalty_Percent <> 0 then
    AddA(p_Penalty_Percent, IntToStr(ri.Penalty_Percent));
  if ri.Penalty_TimePenalty <> 0 then
    AddA(p_Penalty_Time, IntToStr(ri.Penalty_TimePenalty));
  if ri.Fleet <> 0 then
    AddA(p_Fleet, IntToStr(ri.Fleet));
  if ri.IsRacing = false then
    AddA(p_IsRacing, BoolStr[ri.IsRacing]);

  //Output
  AddA(p_CPoints, FloatToStr(ri.CPoints, FormatSettings));
  if ri.Rank <> 0 then
    AddA(p_Rank, IntToStr(ri.Rank));
  if ri.PosR <> ri.Rank then
    AddA(p_PosR, IntToStr(ri.PosR));
  if Index = 0 then
    AddA(p_PLZ, IntToStr(ri.PLZ));
  if ri.Drop then
    AddA(p_Drop, BoolStr[ri.Drop]);

  WriteElement(p_Race);
end;

procedure TFRProxyTestWriter.ReadProxyAttributs;
begin
  AddA(p_Gezeitet, IntToStr(p.Gezeitet));
  if p.FResult <> 0 then
    AddA(p_FResult, IntToStr(p.FResult));
  if p.UseFleets then
    AddA(p_UseFleets, BoolStr[p.UseFleets]);
  if p.TargetFleetSize <> 0 then
    AddA(p_TargetFleetSize, IntToStr(p.TargetFleetSize));
  if p.FirstFinalRace <> 0 then
    AddA(p_FirstFinalRace, IntToStr(p.FirstFinalRace));
end;

procedure TFRProxyTestWriter.ReadEventProps;
begin
  AddA(p_ScoringSystem, SysUtils.IntToStr(p.EventProps.ScoringSystem));
  AddA(p_ScoringSystem2, SysUtils.IntToStr(p.EventProps.ScoringSystem2));
  AddA(p_ThrowoutScheme, IntToStr(p.EventProps.ThrowoutScheme));
  AddA(p_Throwouts, IntToStr(p.EventProps.Throwouts));
  if p.EventProps.FirstIs75 then
    AddA(p_FirstIs75, BoolStr[p.EventProps.FirstIs75]);
  if p.EventProps.ReorderRAF then
    AddA(p_ReorderRAF, BoolStr[p.EventProps.ReorderRAF]);
end;

procedure TFRProxyTestWriter.AddA(name, value: string);
begin
  SLA.Add(name);
  SLA.Add('="');
  SLA.Add(value);
  SLA.Add('" ');
end;

procedure TFRProxyTestWriter.WriteStartElement(name: string);
begin
  SL.Add('<' + name  + ' ' + AttributeString + '>');
end;

procedure TFRProxyTestWriter.WriteEndElement(name: string);
begin
  SL.Add('</' + name + '>');
end;

procedure TFRProxyTestWriter.WriteElement(name: string);
begin
  SL.Add('<' + name + ' ' + AttributeString + '/>');
end;

function TFRProxyTestWriter.AttributeString: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to SLA.Count-1 do
  begin
    result := result + SLA[i];
  end;
  SLA.Clear;
end;

procedure TFRProxyTestWriter.WriteXml2(aProxy: TFRProxy; Memo: TStrings);
begin
  WriteXml0(aProxy);
  Memo.Assign(SL);
end;

procedure TFRProxyTestWriter.WriteXml(aProxy: TFRProxy; aFileName: string);
begin
  WriteXml0(aProxy);
  Main.StoreAdapter.StringListSaveToFile(SL, aFileName);
end;

function TFRProxyTestWriter.WriteXml1(aProxy: TFRProxy): string;
begin
  WriteXml0(aProxy);
  result := SL.Text;
end;

end.
