unit RiggVar.BO.Writer;

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
  Xml.XmlDoc,
  Xml.XmlIntf,
  RiggVar.BO.ExcelExport,
  RiggVar.BO.ExcelImport,
  RiggVar.Grid.ColGrid;

type
  TFRXMLWriter = class(TExcelExporter)
  protected
    procedure AddEventProps(n: IXMLNode);
    procedure AddColCaptions(n: IXMLNode);
    procedure AddNameList(n: IXMLNode);
    procedure AddStartList(n: IXMLNode);
    procedure AddFleetList(n: IXMLNode);
    procedure AddFinishList(n: IXMLNode);
    procedure AddTimeList(n: IXMLNode);
    procedure AddErrorList(n: IXMLNode);
    procedure AddMsgList(node: IXMLNode);
  protected
    procedure SaveLine(Sender: TObject; s: string);
    procedure AddEP(n: IXMLNode; K: string; V: string);
    procedure AddEntries(n: IXMLNode);
    procedure WriteContent(FR: IXMLNode); virtual;
  public
    procedure WriteXML(Memo: TStrings);
  end;

  TFRXMLWriter02 = class(TFRXMLWriter)
  private
    procedure AddRaces(n: IXMLNode);
  protected
    procedure WriteContent(FR: IXMLNode); override;
  end;


implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BO.MsgToken,
  RiggVar.BO.MsgTree,
  RiggVar.BO.EventProps,
  RiggVar.Col.Stammdaten,
  RiggVar.Col.Event,
  RiggVar.Col.Race,
  RiggVar.Util.Classes;

{ TFRXMLWriter }

procedure TFRXMLWriter.WriteXML(Memo: TStrings);
var
  doc: IXMLDocument;
  root: IXMLNode;
begin
  doc := NewXMLDocument();
  doc.Options := doc.Options + [doNodeAutoIndent];
  doc.Active := True;

  root := doc.AddChild('FR');
  WriteContent(root);
  Memo.Text := doc.XML.Text;
  doc.Active := false;
end;

procedure TFRXMLWriter.WriteContent(FR: IXMLNode);
var
  n: IXMLNode;
begin
  n := FR.AddChild('Properties');
  AddEventProps(n);

  if ColCaptionBag.IsPersistent and (ColCaptionBag.Count > 0) then
  begin
    n := FR.AddChild('ColCaptions');
    AddColCaptions(n);
  end;

//  n := FR.AddChild('NameList');
//  AddNameList(n);

  if Main.Params.WantEntryList then
  begin
    n := FR.AddChild('Entries');
    AddEntries(n);
  end;

  n := FR.AddChild('StartList');
  AddStartList(n);

  if BO.EventNode.UseFleets then
  begin
    n := FR.AddChild('FleetList');
    AddFleetList(n);
  end;

  n := FR.AddChild('FinishList');
  AddFinishList(n);

  if (BO.BOParams.ITCount > 0) or BO.EventProps.IsTimed then
  begin
    AddTimeList(FR);
  end;

  n := FR.AddChild('MsgList');
  AddMsgList(n);

  BO.EventNode.ErrorList.CheckAll(BO.EventNode);
  if BO.EventNode.ErrorList.HasErrors then
  begin
    n := FR.AddChild('ErrorList');
    AddErrorList(n);
  end;
end;

procedure TFRXMLWriter.AddEventProps(n: IXMLNode);
var
  ep: TEventProps;
begin
  n.Attributes['StartlistCount'] := IntToStr(BO.BOParams.StartlistCount);
  n.Attributes['ITCount'] := IntToStr(BO.BOParams.ITCount);
  n.Attributes['RaceCount'] := IntToStr(BO.BOParams.RaceCount);
  n.Attributes['DivisionName'] := BO.EventProps.DivisionName;
  n.Attributes['InputMode'] := InputModeStrings[BO.EventProps.InputMode];

  ep := BO.EventProps;
  AddEP(n, 'Name', ep.EventName);
  if ep.EventDates <> '' then
    AddEP(n, 'Dates',ep.EventDates);
  if ep.HostClub <> '' then
    AddEP(n, 'HostClub',ep.HostClub);
  if ep.PRO <> '' then
    AddEP(n, 'PRO',ep.PRO);
  if ep.JuryHead <> '' then
    AddEP(n, 'JuryHead',ep.JuryHead);
  AddEP(n, 'ScoringSystem',JavaScore_ScoringSystemStrings[ep.ScoringSystem]);
  if ep.ScoringSystem2 <> 0 then
    AddEP(n, 'ScoringSystem2',IntToStr(ep.ScoringSystem2));
  AddEP(n, 'Throwouts',IntToStr(ep.Throwouts));
  if ep.ThrowoutScheme <> 1 then
    AddEP(n, 'ThrowoutScheme',JavaScore_ThrowoutSchemeStrings[TThrowoutScheme(ep.ThrowoutScheme)]);
  if ep.FirstIs75 then
    AddEP(n, 'FirstIs75','True');
  if ep.ReorderRAF = false then
    AddEP(n, 'ReorderRAF','False');
  AddEP(n, 'DivisionName',ep.DivisionName);
  AddEP(n, 'InputMode',InputModeStrings[ep.InputMode]);
  AddEP(n, 'RaceLayout',ep.RaceLayout);
  AddEP(n, 'NameSchema',ep.NameSchema);
  AddEP(n, 'FieldMap',ep.FieldMap);
  AddEP(n, 'FieldCaptions',ep.FieldCaptions);
  AddEP(n, 'FieldCount',ep.FieldCount);
  AddEP(n, 'NameFieldCount',ep.NameFieldCount);
  AddEP(n, 'NameFieldOrder',ep.NameFieldOrder);
  if ep.ShowPosRColumn then
    AddEP(n, 'ShowPosRColumn',BoolStr[ep.ShowPosRColumn]);
  if ep.ShowCupColumn then
    AddEP(n, 'ShowCupColumn',BoolStr[ep.ShowCupColumn]);
  if ep.ColorMode <> 'Normal'  then
    AddEP(n, 'ColorMode',ep.ColorMode);
  AddEP(n, 'UseFleets',BoolStr[ep.UseFleets]);
  AddEP(n, 'TargetFleetSize',IntToStr(ep.TargetFleetSize));
  AddEP(n, 'FirstFinalRace',IntToStr(ep.FirstFinalRace));
  AddEP(n, 'IsTimed',BoolStr[ep.IsTimed]);
  AddEP(n, 'UseCompactFormat',BoolStr[ep.UseCompactFormat]);
  if ep.ShowCupColumn then
  begin
    AddEP(n, 'Uniqua.Faktor',FormatFloat('0.00', ep.Faktor));
    AddEP(n, 'Uniqua.Enabled ',BoolStr[ep.EnableUniquaProps]);
    AddEP(n, 'Uniqua.Gesegelt',IntToStr(ep.Gesegelt));
    AddEP(n, 'Uniqua.Gemeldet',IntToStr(ep.Gemeldet));
    AddEP(n, 'Uniqua.Gezeitet',IntToStr(ep.Gezeitet));
  end;
end;

procedure TFRXMLWriter.AddEP(n: IXMLNode; K: string; V: string);
var
  o: IXMLNode;
begin
  o := n.AddChild('EP');
  o.Attributes['K'] := K;
  o.Attributes['V'] := V;
end;

procedure TFRXMLWriter.AddColCaptions(n: IXMLNode);
var
  i: Integer;
begin
  GetCaptionList;
  for i := 0 to SL.Count - 1 do
  begin
    n.AddChild('CL').Text := SL[i];
  end;
end;

procedure TFRXMLWriter.AddNameList(n: IXMLNode);
var
  i: Integer;
begin
  GetNameList;
  for i := 0 to SL.Count - 1 do
  begin
    n.AddChild('NL').Text := SL[i];
  end;
end;

procedure TFRXMLWriter.AddStartList(n: IXMLNode);
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  o: IXMLNode;
begin
  cl := BO.EventNode.EventRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    o := n.AddChild('Pos');
    o.Attributes['oID'] := IntToStr(i+1);
    o.Attributes['Bib'] := IntToStr(cr.Bib);
    o.Attributes['SNR'] := IntToStr(cr.SNR);
  end;
end;

procedure TFRXMLWriter.AddFleetList(n: IXMLNode);
var
  i: Integer;
begin
  GetFleetList;
  for i := 0 to SL.Count - 1 do
  begin
    n.AddChild('FL').Text := SL[i];
  end;
end;

procedure TFRXMLWriter.AddFinishList(n: IXMLNode);
var
  i: Integer;
begin
  GetFinishList;
  for i := 0 to SL.Count - 1 do
  begin
    n.AddChild('FL').Text := SL[i];
  end;
end;

procedure TFRXMLWriter.AddTimeList(n: IXMLNode);
var
  r: Integer;
  o: IXMLNode;
  i: Integer;
begin
  for r := 1 to BO.BOParams.RaceCount do
  begin
    o := n.AddChild('TimeList');
    o.Attributes['RaceID'] := 'R' + IntToStr(r);
    GetTimeList(r);
    for i := 0 to SL.Count - 1 do
    begin
      o.AddChild('TL').Text := SL[i];
    end;
  end;
end;

procedure TFRXMLWriter.AddErrorList(n: IXMLNode);
var
  i: Integer;
begin
  SL.Clear;
  BO.EventNode.ErrorList.GetMsg(SL);
  for i := 0 to SL.Count - 1 do
  begin
    n.AddChild('EL').Text := SL[i];
  end;
end;

procedure TFRXMLWriter.AddMsgList(node: IXMLNode);
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  g: TDivision;
  InputAction: TInputAction;
  r: TRun;
  ere: TEventRaceEntry;
  n: Integer;
begin
  SL.Clear;
  InputAction := TInputAction.Create;
  InputAction.OnSend := SaveLine;
  TInputActionManager.DynamicActionRef := InputAction;
  try
    { rest of messages... }
    cl := BO.EventNode.EventRowCollection;
    for n := 1 to BO.BOParams.RaceCount do
    begin
      g := BO.MsgTree.Division;
      if n = 1 then
        r := g.Race1
      else if (n > 1) and (n <= BO.BOParams.RaceCount) then
        r := g.Race[n]
      else
        r := nil;
      if r = nil then Continue;
      if not BO.IsRacing[n] then
        r.IsRacing(BoolStr[False]);
      for i := 0 to cl.Count - 1 do
      begin
        cr := cl.Items[i];
        ere := cr.Race[n];

        if BO.EventNode.UseFleets then
        begin
          if not ere.IsRacing then
            r.Bib[cr.Bib].RV('x');
        end;

        if ere.Penalty.AsInteger <> 0 then
          r.Bib[cr.Bib].QU(ere.Penalty.ToString);
        if ere.DG > 0 then
          r.Bib[cr.Bib].DG(IntToStr(ere.DG));
      end;
    end;
  finally
    TInputActionManager.DynamicActionRef := nil;
    InputAction.Free;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    node.AddChild('ML').Text := SL[i];
  end;
end;

procedure TFRXMLWriter.SaveLine(Sender: TObject; s: string);
begin
  SL.Add(s);
end;

procedure TFRXMLWriter.AddEntries(n: IXMLNode);
var
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  i: Integer;
  j: Integer;
  o: IXMLNode;
begin
  cl := BO.StammdatenNode.StammdatenRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    o := n.AddChild('SNR');
    o.Attributes['oID'] := IntToStr(cr.SNR);
    for j := 0 to cl.FieldCount - 1 do
      o.Attributes['N' + IntToStr(j+1)] := cr.FieldValue[j+1];
  end;
end;

{ TFRXMLWriter02 }

procedure TFRXMLWriter02.WriteContent(FR: IXMLNode);
var
  n: IXMLNode;
begin
  n := FR.AddChild('Properties');
  AddEventProps(n);

  n := FR.AddChild('Entries');
  AddEntries(n);

  AddRaces(FR);
end;

procedure TFRXMLWriter02.AddRaces(n: IXMLNode);
var
  w: IXMLNode;
  b: IXMLNode;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  ere: TEventRaceEntry;
  r: Integer;
  i: Integer;
begin
  cl := BO.EventNode.EventRowCollection;
  for r := 1 to cl.RCount - 1 do
  begin
    w := n.AddChild('W');
    w.Attributes['oID'] := r;
    for i := 0 to cl.Count - 1 do
    begin
      cr := cl.Items[i];
      ere := cr.Race[r];
      if ere.Rank > 0 then
      begin
        b := w.AddChild('Bib');
        b.Attributes['oID'] := IntToStr(cr.Bib);
        b.Attributes['Rank'] := IntToStr(ere.Rank);
      end;
      if not ere.Penalty.IsOK then
      begin
        b := w.AddChild('Bib');
        b.Attributes['oID'] := IntToStr(cr.Bib);
        b.Attributes['QU'] := ere.Penalty.ToString;
      end;
    end;
  end;
end;

end.
