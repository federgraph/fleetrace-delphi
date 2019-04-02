unit RiggVar.Scoring.Xml;

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
  SysUtils,
  RiggVar.Grid.ColBase,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Domain,
  RiggVar.Scoring.Classes,
  RiggVar.Scoring.Time,
  RiggVar.Scoring.Defs,
  RiggVar.Scoring.Utils,
  Variants,
  XmlDoc, XmlIntf;


type
  TRaceXML = class(TRace);
  TFinishXML = class(TFinish);
  TEntryXML = class(TEntry);
  TISAFPenaltyXML = class(TISAFPenalty);
  TRegattaXML = class(TRegatta);

  TJSXML = class
  private
    procedure TBaseList_XmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
    function TBaseList_FromXmlElement(o1: TBaseObject; n: IXMLNode; rootObject: TBaseObject): TBaseObject;
    procedure TRace_xmlRead(o1: TBaseObject; n: IXmlNode;
      rootObject: TBaseObject);
    procedure TFinish_xmlRead(o1: TBaseObject; n: IXmlNode;
      rootObject: TBaseObject);
    procedure TEntry_xmlRead(o1: TBaseObject; n: IXmlNode;
      rootObject: TBaseObject);
    procedure TISAFPenalty_xmlRead(o1: TBaseObject; n: IXmlNode;
      rootObject: TBaseObject);
    procedure TRegatta_xmlRead(o1: TBaseObject; n: IXmlNode;
      rootObject: TBaseObject);
  public
    procedure ReadXml(o: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
  end;

function ReadXml(obj: TBaseObject; fn: string; toproot: TBaseObject): Boolean;

implementation

var
  JSXML: TJSXML; //Singleton

function ReadXml(obj: TBaseObject; fn: string; toproot: TBaseObject): Boolean;
var
  doc: IXmlDocument;
  root: IXmlNode;
begin
  if not Assigned(JSXML) then
    JSXML := TJSXML.Create;

  result := false;
  try
    doc := TXmlDocument.Create(fn);
    root := doc.DocumentElement;
    //root.Normalize;
    JSXML.ReadXml(obj, root, toproot);
    result := true;
  except
    //Util.CleanInternationalChars(fn);
  end;
end;

procedure TJSXML.ReadXml(o: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
begin
  if o is TBaseList then
    TBaseList_XmlRead(o, n, rootObject)
  else if o is TRace then
    TRace_XmlRead(o, n, rootObject)
  else if o is TFinish then
    TFinish_XmlRead(o, n, rootObject)
  else if o is TEntry then
    TEntry_XmlRead(o, n, rootObject)
  else if o is TISAFPenalty then
    TISAFPenalty_XmlRead(o, n, rootObject)
  else if o is TRegatta then
    TRegatta_XmlRead(o, n, rootObject)
end;

procedure TJSXML.TBaseList_XmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
var
  k: Integer;
  kids2: IXMLNodeList;
  n2: IXMLNode;
  obj: TBaseObject;
  o: TBaseList;
begin
  o := o1 as TBaseList;
  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    if (n2.NodeType = ntElement) then
    begin
      obj := TBaseList_FromXmlElement(o, n2, rootObject);
      if Assigned(obj) then
        o.Add(obj)
      else
        Utils.WriteLine('FromXmlElement returned nil for ' + o.ElementType.ClassName);
    end;
  end;
end;

function TJSXML.TBaseList_FromXmlElement(o1: TBaseObject; n: IXMLNode; rootObject: TBaseObject): TBaseObject;
var
  o: TBaseList;
begin
  o := o1 as TBaseList;
  try
    result := nil;
    if o.ElementType = TEntry then
      result := TEntry.Create
    else if o.ElementType = TFinish then
      result := TFinish.Create
    else if o.ElementType = TRace then
      result := TRace.Create(nil, 0);

    ReadXml(result, n, rootObject);
  except
    result := nil;
  end;
end;

procedure TJSXML.TRace_xmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
var
  k: Integer;
  kids, kids2: IXmlNodeList;
  n2: IXmlNode;
  xname, value_Renamed: string;
  v: OleVariant;
  o: TRaceXML;
begin
  o := o1 as TRaceXML;
  o.FRegatta := rootObject as TRegatta;

  kids := n.AttributeNodes;

  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    v := n2.NodeValue;
    if VarIsNull(v) then continue;
    value_Renamed := n2.NodeValue;
    if (xname = ID_PROPERTY) then
      o.FID := StrToIntDef(value_Renamed, o.FID)
  end;

  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    xname := n2.NodeName;
    if (xname = FINISHLIST_PROPERTY) then
      ReadXml(o.FFinishList, n2, o);
  end;

end;

procedure TJSXML.TFinish_xmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
var
  k: Integer;
  kids, kids2: IXmlNodeList;
  n2: IXmlNode;
  xname, value_Renamed: string;
  v: OleVariant;
  id: Integer;
  o: TFinishXML;
begin
  o := o1 as TFinishXML;
  o.Penalty.Clear;
  o.FRace := rootObject as TRace;

  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    v := n2.NodeValue;
    if VarIsNull(v) then continue;
    value_Renamed := n2.NodeValue;
    if (xname = ENTRY_PROPERTY) then
    begin
      id := StrToIntDef(value_Renamed, -1);
      if (o.race <> nil) and (id <> -1) then
        o.Entry := o.race.Regatta.Entries.GetEntryByID(id);
    end
    else if (xname = FINISHTIME_PROPERTY) then
      o.FFinishTime := SailTime.forceToLong(value_Renamed)
    else if (xname = FINISHPOSITION_PROPERTY) then
    begin
      o.fPosition.Position := o.FinishPosition.parseString(value_Renamed);
      if (TISAFPenalty.isFinishPenalty(o.fPosition.Position)) then
      begin
        o.Penalty.Clear;
        o.Penalty.AsInteger := o.FinishPosition.Position;
      end;
    end;
  end;

  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    xname := n2.NodeName;
    if (xname = PENALTY_PROPERTY) then
      ReadXml(o.Penalty, n2, rootObject);
  end;
end;

procedure TJSXML.TEntry_xmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
var
  k: Integer;
  kids: IXmlNodeList;
  n2: IXmlNode;
  xname, value_Renamed: string;
  v: OleVariant;
  o: TEntryXML;
begin
  o := o1 as TEntryXML;
  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    v := n2.NodeValue;
    if VarIsNull(v) then continue;
    value_Renamed := n2.NodeValue;
    if (xname = ID_PROPERTY) then
      o.fID := StrToIntDef(value_Renamed, o.fID);
  end;
end;

procedure TJSXML.TISAFPenalty_xmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
var
  k: Integer;
  kids: IXmlNodeList;
  n2: IXmlNode;
  xname, value_Renamed: string;
  v: OleVariant;
  o: TISAFPenaltyXML;
begin
  o := o1 as TISAFPenaltyXML;
  o.Clear;

  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    v := n2.NodeValue;
    if VarIsNull(v) then continue;
    value_Renamed := n2.NodeValue;
    if (xname = PENALTY_PROPERTY) then
    try
      o.parseCommaText(value_Renamed);
    except
    end
    else if (xname = PERCENT_PROPERTY) then
      o.fPercent := StrToIntDef(value_Renamed, o.fPercent)
    else if (xname = POINTS_PROPERTY) then
      o.fPoints := StrToFloatDef(value_Renamed, o.fPoints)
    else if (xname = TIMEPENALTY_PROPERTY) then
      o.fTimePenalty := SailTime.forceToLong(value_Renamed);
  end;
end;

procedure TJSXML.TRegatta_xmlRead(o1: TBaseObject; n: IXmlNode; rootObject: TBaseObject);
var
  k: Integer;
  kids2: IXmlNodeList;
  n2: IXmlNode;
  xname: string;
  o: TRegattaXML;
begin
  o := o1 as TRegattaXML;

  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    xname := n2.NodeName;
    if (xname = ENTRYLIST_PROPERTY) then
    begin
      o.fEntries.Clear();
      ReadXml(o.fEntries, n2, o);
    end
    else if (xname = RACELIST_PROPERTY) then
    begin
      o.fRaces.Clear();
      ReadXml(o.fRaces, n2, o);
    end;
  end;
end;

end.
