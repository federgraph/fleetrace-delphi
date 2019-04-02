unit RiggVar.Calc.EventProxyReader;

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
  XmlDoc, XmlIntf, Variants,
  RiggVar.Util.Classes,
  RiggVar.Calc.EventProxy00;

type
  TFRProxyTestReader = class
  private
    p: TFRProxy;
    RCount: Integer;
    FormatSettings: TFormatSettings;
    procedure Read_EventProps(n: IXmlNode);
    procedure Read_RaceProps(n: IXmlNode);
    procedure Read_FRProxy(n: IXmlNode);
    procedure Read_Entry(n: IXmlNode);
    procedure Read_Race(n: IXmlNode; ei: TEntryInfo);
    procedure Read_IsRacing(n: IXmlNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadXML(fn: string);
    procedure ReadXMLString(xml: string);
    property Proxy: TFRProxy read p;
  end;

  TFRProxyTester = class
  public
    function Test(fn: string): Boolean;
  end;

implementation

{ TFRProxyTestReader }

constructor TFRProxyTestReader.Create;
begin
  inherited Create;
  FormatSettings.DecimalSeparator := '.';
end;

destructor TFRProxyTestReader.Destroy;
begin
  p.Free;
  inherited;
end;

procedure TFRProxyTestReader.ReadXMLString(xml: string);
var
  doc: IXmlDocument;
  root: IXmlNode;
begin
  p.Free;
  p := TFRProxy.Create;
  p.FResult := -1;
  try
    doc := LoadXmlData(xml); //doc := TXmlDocument.Create(fn);
    root := doc.DocumentElement;
    Read_FRProxy(root);
    p.FResult := 0;
  except
  end;
end;

procedure TFRProxyTestReader.ReadXML(fn: string);
var
  doc: IXmlDocument;
  root: IXmlNode;
begin
  p.Free;
  p := TFRProxy.Create;
  p.FResult := -1;
  try
    doc := TXmlDocument.Create(fn);
    root := doc.DocumentElement;
    Read_FRProxy(root);
    p.FResult := 0;
  except
  end;
end;

procedure TFRProxyTestReader.Read_FRProxy(n: IXmlNode);
var
  k: Integer;
  kids, kids2: IXmlNodeList;
  n2: IXmlNode;
  xname, xvalue: string;
begin
  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    xvalue := n2.NodeValue;
    if (xname = p_Gezeitet) then
      p.Gezeitet := StrToInt(xvalue);

    if (xname = p_UseFleets) then
      p.UseFleets := TUtils.IsTrue(xvalue);
    if (xname = p_TargetFleetSize) then
      p.TargetFleetSize := StrToInt(xvalue);
    if (xname = p_FirstFinalRace) then
      p.FirstFinalRace := StrToInt(xvalue);
  end;

  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    xname := n2.NodeName;
    if (xname = p_EventProps) then
    begin
      Read_EventProps(n2);
    end
    else if (xname = p_RaceProps) then
    begin
      Read_RaceProps(n2);
    end
    else if (xname = p_Entry) then
    begin
      Read_Entry(n2);
    end
  end;
end;

procedure TFRProxyTestReader.Read_EventProps(n: IXmlNode);
var
  k: Integer;
  kids: IXmlNodeList;
  n2: IXmlNode;
  xname, xvalue: string;
begin
  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    xvalue := n2.NodeValue;
    if (xname = p_ScoringSystem) then
      p.EventProps.ScoringSystem := StrToInt(xvalue)
    else if (xname = p_ScoringSystem2) then
      p.EventProps.ScoringSystem2 := StrToInt(xvalue)
    else if (xname = p_ThrowoutScheme) then
      p.EventProps.ThrowoutScheme := StrToInt(xvalue)
    else if (xname = p_Throwouts) then
      p.EventProps.Throwouts := StrToInt(xvalue)
    else if (xname = p_FirstIs75) then
      p.EventProps.FirstIs75 := TUtils.IsTrue(xvalue)
    else if (xname = p_ReorderRAF) then
      p.EventProps.ReorderRAF := TUtils.IsTrue(xvalue)
  end;
end;

procedure TFRProxyTestReader.Read_RaceProps(n: IXmlNode);
var
  k, i: Integer;
  kids, kids2: IXmlNodeList;
  n2: IXmlNode;
  xname, xvalue: string;
begin
  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    xvalue := n2.NodeValue;
    if (xname = p_RCount) then
    begin
      RCount := StrToInt(xvalue);
      SetLength(p.IsRacing, RCount);
      for i := 0 to p.RCount - 1 do
        p.IsRacing[i] := true;
    end;
  end;

  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    Read_IsRacing(n2);
  end;
end;

procedure TFRProxyTestReader.Read_IsRacing(n: IXmlNode);
var
  k: Integer;
  kids: IXmlNodeList;
  n2: IXmlNode;
  xname, xvalue: string;
  Index: Integer;
  IsRacing: Boolean;
begin
  IsRacing := true;
  Index := -1;
  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    xvalue := n2.NodeValue;
    if (xname = p_Index) then
      Index := StrToInt(xvalue)
    else if (xname = p_IsRacing) then
      IsRacing := TUtils.IsTrue(xvalue);
  end;

  if (Index > -1) and (IsRacing = false) and (Length(p.IsRacing) > Index) then
  begin
    p.IsRacing[Index] := false;
  end;
end;

procedure TFRProxyTestReader.Read_Entry(n: IXmlNode);
var
  k: Integer;
  kids, kids2: IXmlNodeList;
  n2: IXmlNode;
  xname, xvalue: string;
  ei: TEntryInfo;
  oldCount: Integer;
  i: Integer;
begin
  oldCount := p.EntryInfoCollection.Count;
  ei := TEntryInfo.Create;
  ei.Index := oldCount;
  ei.IsGezeitet := true; //Default
  SetLength(p.EntryInfoCollection.Items, oldCount + 1);
  p.EntryInfoCollection.Items[oldCount] := ei;
  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    xvalue := n2.NodeValue;
    if (xname = p_Index) then
    begin
      if ei.Index <> oldCount then
        exit;
    end
    else if (xname = p_SNR) then
      ei.SNR := StrToInt(xvalue)
    else if (xname = p_IsGezeitet) then
      ei.IsGezeitet := TUtils.IsTrue(xvalue)
    else if (xname = p_IsTied) then
      ei.IsTied := TUtils.IsTrue(xvalue);

  end;

  kids2 := n.ChildNodes;
  for k := 0 to kids2.Count-1 do
  begin
    n2 := kids2.Nodes[k];
    xname := n2.NodeName;
    if (xname = p_Race) then
    begin
      if (k = 0) and (kids2.Count > RCount) then
      begin
        SetLength(p.IsRacing, kids2.Count);
        for i := 0 to p.RCount - 1 do
          p.IsRacing[i] := true;
      end;
      Read_Race(n2, ei);
    end;
  end;
end;

procedure TFRProxyTestReader.Read_Race(n: IXmlNode; ei: TEntryInfo);
var
  k: Integer;
  kids: IXmlNodeList;
  n2: IXmlNode;
  xname, xvalue: string;
  ri: TRaceInfo;
  oldCount: Integer;
  temp: string;
begin
  oldCount := ei.RCount;
  ri := TRaceInfo.Create;
  SetLength(ei.Race, oldCount + 1);
  ei.Race[oldCount] := ri;

  kids := n.AttributeNodes;
  for k := 0 to kids.Count-1 do
  begin
    n2 := kids.Nodes[k];
    xname := n2.NodeName;
    xvalue := n2.NodeValue;
    if (xname = p_Index) then
    begin
    end

    //Input
    else if (xname = p_OTime) then
      ri.OTime := StrToInt(xvalue)
    else if (xname = p_QU) then
      ri.QU := StrToInt(xvalue)
    else if (xname = p_Penalty_Points) then
    begin
      temp := StringReplace(xvalue, ',', '.', []);
      ri.Penalty_Points := StrToFloat(temp, FormatSettings);
    end
    else if (xname = p_Penalty_Note) then
      ri.Penalty_Note := xvalue
    else if (xname = p_Penalty_Percent) then
      ri.Penalty_Percent := StrToInt(xvalue)
    else if (xname = p_Penalty_Time) then
      ri.Penalty_TimePenalty := StrToInt(xvalue)
    else  if (xname = p_Fleet) then
      ri.Fleet := StrToInt(xvalue)
    else  if (xname = p_IsRacing) then
      ri.IsRacing := TUtils.IsTrue(xvalue)

    //Output
    else if (xname = p_CPoints) then
    begin
      temp := StringReplace(xvalue, ',', '.', []);
      ri.CPoints := StrToFloat(temp, FormatSettings);
    end
    else if (xname = p_Rank) then
    begin
      ri.Rank := StrToInt(xvalue);
      ri.PosR := ri.Rank; //default
    end
    else if (xname = p_PosR) then
      ri.PosR := StrToInt(xvalue) //read after Rank!
    else if (xname = p_PLZ) then
      ri.PLZ := StrToInt(xvalue)
    else if (xname = p_Drop) then
      ri.Drop := TUtils.IsTrue(xvalue)
  end;
end;

{ TFRProxyTester }

function TFRProxyTester.Test(fn: string): Boolean;
var
  TestReader: TFRProxyTestReader;
  p: TFRProxy;
begin
  TestReader := TFRProxyTestReader.Create();
  TestReader.ReadXml(fn);
  p := TFRProxy.Create;
  result := TestReader.Proxy.Equals(p);
  p.Free;
  TestReader.Free;
end;

end.
