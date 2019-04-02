unit RiggVar.EM.TransformerHtml;

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
  System.Math,
  System.RegularExpressions,
  RiggVar.EM.Transformer,
  RiggVar.Util.Classes,
  RiggVar.EM.XmlVerySimple;

type
  THtmlConverter = class(TEventDataTransformer)
  private
    Xml: TSimpleXml;

    upchar: char;
    downchar: char;

    PropertyRows: TXmlNodeList;
    HeaderFields: TXmlNodeList;
    BodyRows: TXmlNodeList;

    SL: TStrings; //not owned, do not free

    FieldList: TStringList;
    Props: TStringList;
    FLA: TStringList;

    StartListCount: Integer;
    RaceCount: Integer;
    TargetFleetSize: Integer;
    UseFleets: Boolean;

    R1Col: Integer;
    EndCol: Integer;
    BibCol: Integer;

    Bib: Integer;
    Race: Integer;

    WantPreTags: Boolean;

    procedure WriteNameListHeaderLine(
      StartIndex: Integer;
      StopIndex: Integer);

    procedure WriteFirstLine(FieldList: TStringList;
      StartIndex: Integer;
      StopIndex: Integer;
      RaceIndex: Integer;
      EndIndex: Integer);

    function GenQU(value: string): string;
    function CalcFleetSize(rCol: Integer; gClass: string): Integer;
    function GetRaceIndex(r: Integer): Integer;
    function CountGroupElements(Group: Integer): Integer;
    function RetrieveGroupMatch(mc: TMatchCollection): string;
    function IsRaceCol(th: string): Boolean;

    procedure ExtractParams;
    function ExtractGroup(td: TXmlNode): Integer;
    function ExtractFinishPosition(s: string): string;
    function ExtractQU(s: string): string;
    function ExtractNoRace(s: string): string;

    procedure MakeNameList;
    procedure MakeStartList;
    procedure MakeFleetList;
    procedure MakeFinishList;
    procedure MakeMsgList;

    procedure ReadTables;
    procedure ReadProperties(table: TXmlNode);
    procedure ReadResults(table: TXmlNode);

    procedure Reset;

    procedure Transform;
    procedure ReadTable(table: TXmlNode);
    procedure FindTables(node: TXmlNode);
  public
    constructor Create;
    destructor Destroy; override;

    procedure TransformSL(SLXML, ATL, ASL: TStrings); override;
  end;

implementation

constructor THtmlConverter.Create;
begin
  Xml := TSimpleXml.Create;
  Xml.Header.Attribute['encoding'] := 'utf-8';

  FieldList := TStringList.Create;
  Props := TStringList.Create;
  FLA := TStringList.Create;
  FLA.Delimiter := ';';
end;

destructor THtmlConverter.Destroy;
begin
  Reset; //will free some TXmlNodeLists

  Xml.Free;

  //SL.Free; not owned!
  FieldList.Free;
  Props.Free;
  FLA.Free;
  inherited;
end;

procedure THtmlConverter.WriteNameListHeaderLine(StartIndex: Integer; StopIndex: Integer);
var
  n: Integer;
  i: Integer;
  fl: string;
begin
  n := 0;
  FLA.Clear;
  FLA.Add('ID');
  FLA.Add('SNR');
  FLA.Add('Bib');
  for i := StartIndex to StopIndex - 1 do
  begin
    Inc(n);
    FLA.Add('N' + IntToStr(n));
  end;
  fl := FLA.DelimitedText;
  SL.Add(fl);
  FLA.Clear;
end;

procedure THtmlConverter.WriteFirstLine(FieldList: TStringList; StartIndex: Integer;
  StopIndex: Integer; RaceIndex: Integer; EndIndex: Integer);
var
  i: Integer;
  fl: string;
  s: string;
begin
  if FieldList.Count = 0 then
  begin
    FError := 'FieldList.Count = 0';
    Exit;
  end;

  FLA.Clear;
  for i := 0 to EndIndex - 1 do
  begin
    if ((i >= StartIndex) and (i <= StopIndex)) or (i >= RaceIndex) then
    begin
      if i > FieldList.Count -1 then
      begin
        FError := 'WriteFirstLine: AFieldList.Count < Index';
        Exit;
      end;
      s := FieldList[i];
      if s <> '' then
      begin
        s := StringReplace(s, upchar, '', []);
        s := StringReplace(s, downchar, '', []);
        FLA.Add(s);
      end;
    end;
  end;
  fl := FLA.DelimitedText;
  SL.Add(fl);
end;

procedure THtmlConverter.Reset;
begin
  FError := '';
  FOK := False;

  HeaderFields.Free;
  HeaderFields := nil;

  BodyRows.Free;
  BodyRows := nil;

  PropertyRows.Free;
  PropertyRows := nil;

  Upchar := char(9650); //String.fromCharCode(160, 9650);
  Downchar := char(9660); //String.fromCharCode(160, 9660);

  StartListCount := 0;
  R1Col := 0;
  RaceCount := 0;
  EndCol := 0;
  BibCol := 2;
  Bib := 0;

  FieldList.Clear;
  Props.Clear;
  FLA.Clear;

  Props.Values['EP.UseFleets'] := 'false';
  Props.Values['EP.TargetFleetSize'] := '20';
  Props.Values['EP.FirstFinalRace'] := '0';
  Props.Values['EP.NameFieldCount'] := '0';
  Props.Values['EP.NameFieldOrder'] := '';
  Props.Values['EP.FieldCaptions'] := '';
  Props.Values['EP.FieldCount'] := '0';
  Props.Values['EP.ScoringSystem'] := 'Low Point System';
  Props.Values['EP.Throwouts'] := '1';
  Props.Values['EP.ReorderRAF'] := 'false';
  Props.Values['EP.ColorMode'] := 'N';
end;

function THtmlConverter.RetrieveGroupMatch(mc: TMatchCollection): string;
var
  m: TMatch;
begin
  result := '';
  if mc.Count > 0 then
  begin
    m := mc.Item[0];
    if m.Groups.Count > 1 then
      result := m.Groups.Item[1].Value;
  end;
end;

procedure THtmlConverter.ExtractParams;
var
  th: TXmlNode;
  s: string;
  i: Integer;
  g0, g1: Integer;
begin
  if HeaderFields = nil then
  begin
    FError := 'HeaderFields = nil in ExtractParams';
    FOK := False;
    Exit;
  end;

  i := 0;
  for th in HeaderFields do
  begin
    s := th.Text;
    if (R1Col > 0) and (EndCol = 0) and (not IsRaceCol(s)) then
    begin
      RaceCount := i - R1Col;
      EndCol := i;
    end
    else if (i > 0) and (R1Col = 0) and (IsRaceCol(s)) then
    begin
      R1Col := i;
    end;
    if (EndCol = 0) then
    begin
      FieldList.Add(s);
    end;
    Inc(i);
  end;

  StartListCount := BodyRows.Count;

  UseFleets := CountGroupElements(0) > 0;
  UseFleets := UseFleets or (CountGroupElements(1) > 0);

  g0 := calcFleetSize(GetRaceIndex(1), 'g0');
  g1 := calcFleetSize(GetRaceIndex(1), 'g1');
  TargetFleetSize := Max(g0, g1);
end;

function THtmlConverter.ExtractGroup(td: TXmlNode): Integer;
var
  cl: string;
begin
  cl := td.Attribute['class'];
  if cl = 'g0' then
    result := 0
  else if cl = 'g1' then
    result := 1
  else if cl = 'g2' then
    result := 2
  else if cl = 'g3' then
    result := 3
  else if cl = 'g4' then
    result := 4
  else
    result := 0;
end;

function THtmlConverter.ExtractFinishPosition(s: string): string;
var
  regex: TRegEx;
  mc: TMatchCollection;
  t: string;
begin
  result := '0';

  // '12' --> 12
  regex.Create('^(\d+)$');
  if regex.IsMatch(s) then
  begin
    result := s;
    Exit;
  end;

  // '[35]' --> 35
  regex.Create('^\[(\d+)\]$');
  mc := regex.Matches(s);
  t := RetrieveGroupMatch(mc);
  if t <> '' then
  begin
    result := t;
    Exit;
  end;

  // '[0/DNF]' --> 0
  regex.Create('^\[(\d+)\/\S+\]');
  mc := regex.Matches(s);
  t := RetrieveGroupMatch(mc);
  if t <> '' then
  begin
    result := t;
    Exit;
  end;

  // '5/DSQ' --> 5
  regex.Create('^(\d+)\/\S+');
  mc := regex.Matches(s);
  t := RetrieveGroupMatch(mc);
  if t <> '' then
  begin
    result := t;
    Exit;
  end;
end;

function THtmlConverter.ExtractQU(s: string): string;
var
  regex: TRegEx;
  mc: TMatchCollection;
  c: Integer;
  t: string;
begin
  // '12' --> 12
  regex.Create('^(\d+)$');
  if regex.IsMatch(s) then
  begin
    result := '';
    Exit;
  end;

  // '[35]' --> 35
  regex.Create('^\[(\d+)\]$');
  if regex.IsMatch(s) then
  begin
    result := '';
    Exit;
  end;

  // '[0/DNF]' --> 0
  regex.Create('^\[\d+\/(\S+)\]');
  mc := regex.Matches(s);
  t := RetrieveGroupMatch(mc);
  if t <> '' then
  begin
    result := GenQU(t);
    Exit;
  end;

  // '5/DSQ' --> 5
  regex.Create('^\d+\/(\S+)');
  mc := regex.Matches(s);
  t := RetrieveGroupMatch(mc);
  if t <> '' then
  begin
    result := GenQU(t);
    Exit;
  end;

  // 'dnf' --> dnf
  regex.Create('dns|dnf|dsq', [roIgnoreCase]);
  mc := regex.Matches(s);
  c := mc.Count;
  if c >= 1 then
  begin
    t := mc.Item[0].Value;
    result := GenQU(t);
    Exit;
  end;

  result := '';
end;

function THtmlConverter.ExtractNoRace(s: string): string;
var
  regex: TRegEx;
begin
  result := '';

  // '(0)'
  regex.Create('^\(0\)$');
  if regex.IsMatch(s) then
    result := 'FR.*.W' + IntToStr(Race) + '.Bib' + IntToStr(Bib) + '.RV=x';
end;

function THtmlConverter.GenQU(value: string): string;
begin
  result := 'FR.*.W' + IntToStr(Race) + '.Bib' + IntToStr(Bib) + '.QU=' + value;
end;

function THtmlConverter.IsRaceCol(th: string): Boolean;
var
  regex: TRegEx;
  mc: TMatchCollection;
  t: string;
begin
  //var pattern = /R(\d+)/;
  //return pattern.test($(th).text());

  result := false;
  regex.Create('R(\d+)');
  mc := regex.Matches(th);
  t := RetrieveGroupMatch(mc);
  if t <> '' then
  begin
    result := true;
    Exit;
  end;
end;

function THtmlConverter.GetRaceIndex(r: Integer): Integer;
begin
  result := r + R1Col - 1;
end;

function THtmlConverter.CalcFleetSize(rCol: Integer; gClass: string): Integer;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  i: Integer;
  c: Integer;
begin
  c := 0;
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    i := 0;
    for td in tds do
    begin
      if (i = rCol) then
      begin
        if td.HasAttribute(gClass) then
          Inc(c);
      end;
      Inc(i);
    end;
    tds.Free;
  end;
  result := c;
end;

function THtmlConverter.CountGroupElements(Group: Integer): Integer;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  cls: string;
  c: Integer;
  gn: string;
begin
  c := 0;
  gn := 'g'+ IntToStr(Group);
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    for td in tds do
    begin
      if td.HasAttribute('class') then
      begin
        cls := td.Attribute['class'];
        if Pos(gn, cls) > 0 then
          Inc(c);
      end;
    end;
    tds.Free;
  end;
  result := c;
end;

procedure THtmlConverter.MakeNameList;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  r, s: string;
  i: Integer;
begin
  WriteNameListHeaderLine(3, R1Col);
  s := '';
  i := 0;
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    for td in tds do
    begin
      r := td.Text;
      if r = '&nbsp;' then
        r := '';
      if (i >= 0) and (i < R1Col) then
      begin
        if s <> '' then
          s := s + ';';
        s := s + r;
      end;
      Inc(i);
    end;
    tds.Free;
    SL.Add(s);
    s := '';
    i := 0;
  end;
end;

procedure THtmlConverter.MakeStartList;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  r, s: string;
  i: Integer;
  e: Integer;
begin
  e := 3;
  WriteFirstLine(FieldList, 0, e, e, e);
  s := '';
  i := 0;
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    for td in tds do
    begin
      r := td.Text;
      if (i < e) then
      begin
        if s <> '' then
          s := s + ';';
        s := s + r;
      end;
      Inc(i);
    end;
    tds.Free;
    SL.Add(s);
    s := '';
    i := 0;
  end;
end;

procedure THtmlConverter.MakeFleetList;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  r, s: string;
  i: Integer;
begin
  WriteFirstLine(FieldList, 1, 2, R1Col, EndCol);
  s := '';
  i := 0;
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    for td in tds do
    begin
      r := td.Text;
      if (i > 0) and (i < 3) then
      begin
        if s <> '' then
          s := s + ';';
        s := s + r;
      end
      else if (i >= R1Col) and (i < EndCol) then
      begin
        if s <> '' then
          s := s + ';';
        s := s + IntToStr(ExtractGroup(td));
      end;
      Inc(i);
    end;
    tds.Free;
    SL.Add(s);
    s := '';
    i := 0;
  end;
end;

procedure THtmlConverter.MakeFinishList;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  r, s: string;
  i: Integer;
begin
  WriteFirstLine(FieldList, 0, 2, R1Col, EndCol);
  s := '';
  i := 0;
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    for td in tds do
    begin
      r := td.Text;
      if ((i >= 0) and (i <= 2)) or ((i >= R1Col) and (i < EndCol)) then
      begin
        if s <> '' then
          s := s + ';';
        s := s + ExtractFinishPosition(r);
      end;
      Inc(i);
    end;
    tds.Free;
    SL.Add(s);
    s := '';
    i := 0;
  end;
end;

procedure THtmlConverter.MakeMsgList;
var
  tds: TXmlNodeList;
  tr, td: TXmlNode;
  r, s: string;
  i: Integer;
begin
  s := '';
  i := 0;
  for tr in BodyRows do
  begin
    tds := tr.FindNodes('td');
    for td in tds do
    begin
      r := td.Text;
      if (i < EndCol)then
      begin
        if (i = BibCol) then
          Bib := StrToIntDef(r, 0);
        if i >= R1Col then
        begin
          Race := i - R1Col + 1;
          s := ExtractQU(r);
          if s <> '' then
          begin
            SL.Add(s);
            s := '';
          end;
          s := ExtractNoRace(r);
          if s <> '' then
          begin
            SL.Add(s);
            s := '';
          end;
        end;
      end;
      Inc(i);
    end;
    tds.Free;
    i := 0;
  end;
end;

procedure THtmlConverter.TransformSL(SLXML, ATL, ASL: TStrings);
begin
  if SLXML.Count > 0 then
  try
    SL := ASL;
    Xml.Text := SLXML.Text;

    Transform;

    FOK := true;
  except
    on E: Exception do
    begin
      FOK := false;
      FError := E.Message;
    end;
  end
  else
  begin
    FOK := false;
    FError := 'SL is empty';
  end;
end;

procedure THtmlConverter.Transform;
begin
  Reset;

  ReadTables;

  //HeaderFields = $('table.results thead tr th');
  if HeaderFields = nil then
  begin
    FError := 'HeaderFields = nil';
    FOK := False;
    Exit;
  end;

  //BodyRows = $('table.results tbody tr');
  if BodyRows = nil then
  begin
    FError := 'BodyRows = nil';
    FOK := False;
    Exit;
  end;

  //PropertyRows = $('table.properties tr');
  if PropertyRows = nil then
  begin
    FError := 'PropertyRows = nil';
    FOK := False;
    Exit;
  end;

  ExtractParams;

  if WantPreTags then
    SL.Add('<pre>');

  SL.Add('#Params');

  SL.Add('');

  SL.Add('DP.StartlistCount =' + IntToStr(StartListCount));
  SL.Add('DP.ITCount = 0');
  SL.Add('DP.RaceCount = ' + IntToStr(RaceCount));

  SL.Add('');

  SL.Add('#Event Properties');

  SL.Add('');

  SL.Add('EP.Name = ' + Props.Values['EP.Name']);
  SL.Add('EP.ScoringSystem = ' + Props.Values['EP.ScoringSystem']);
  SL.Add('EP.Throwouts = ' + Props.Values['EP.Throwouts']);
  SL.Add('EP.ReorderRAF = ' + Props.Values['EP.ReorderRAF']);
  SL.Add('EP.DivisionName = *');
  SL.Add('EP.InputMode = Strict');
  SL.Add('EP.RaceLayout = Finish');
  SL.Add('EP.NameSchema = NX');
  //t.Add('EP.FieldMap = ' + Props.Values['EP.FieldMap']);
  SL.Add('EP.FieldCaptions = ' + Props.Values['EP.FieldCaptions']);
  SL.Add('EP.FieldCount = ' + Props.Values['EP.FieldCount']);
  SL.Add('EP.NameFieldCount = ' + Props.Values['EP.NameFieldCount']);
  SL.Add('EP.NameFieldOrder = ' + Props.Values['EP.NameFieldOrder']);

  SL.Add('EP.ColorMode = ' + Props.Values['EP.ColorMode']);
  SL.Add('EP.UseFleets = ' + Props.Values['EP.UseFleets']);
  SL.Add('EP.TargetFleetSize = ' + Props.Values['EP.TargetFleetSize']);
  SL.Add('EP.FirstFinalRace = ' + Props.Values['EP.FirstFinalRace']);
  SL.Add('EP.IsTimed = false');
  SL.Add('EP.UseCompactFormat = true');

  SL.Add('');

  SL.Add('NameList.Begin');
  MakeNameList;
  SL.Add('NameList.End');

  SL.Add('');

  SL.Add('StartList.Begin');
  MakeStartList;
  SL.Add('StartList.End');

  if UseFleets then
  begin
    SL.Add('');

    SL.Add('FleetList.Begin');
    MakeFleetList;
    SL.Add('FleetList.End');
  end;

  SL.Add('');

  SL.Add('FinishList.Begin');
  MakeFinishList;
  SL.Add('FinishList.End');

  SL.Add('');

  //t.Add('MsgList.Begin');
  MakeMsgList;
  //t.Add('MsgList.End');

  if WantPreTags then
    SL.Add('</pre>');
end;

procedure THtmlConverter.ReadTables;
var
  body: TXmlNode;
  dive: TXmlNode;

  nl, divs: TXmlNodeList;
  s: string;
begin
  nl := Xml.Root.FindNodes('body');
  if nl.Count > 0 then
  begin
    body := nl.First;

    { find the tables as direct descendants of body tag }
    FindTables(body);

    if not Assigned(BodyRows) then
    begin
      { find tables inside the results div }
      divs := body.FindNodes('div');
      for dive in divs do
      begin
        s := dive.Attribute['id'];
        if s = 'results' then
          FindTables(dive);
      end;
      divs.Free;
    end;

  end;
  nl.Free;
end;

procedure THtmlConverter.FindTables(node: TXmlNode);
var
  table: TXmlNode;
  tables: TXmlNodeList;
begin
  tables := node.FindNodes('table');
  for table in tables do
  begin
    ReadTable(table);
  end;
  tables.Free;
end;

procedure THtmlConverter.ReadTable(table: TXmlNode);
var
  s: string;
begin
  s := table.Attribute['class'];
  if s = 'fr properties' then
  begin
    ReadProperties(table);
  end
  else if s = 'sortable fr results' then
  begin
    ReadResults(table);
  end;
end;

procedure THtmlConverter.ReadProperties(table: TXmlNode);
var
  tr, td: TXmlNode;
  trs, tds: TXmlNodeList;
  s: string;
  i: Integer;
  k, v: string;
begin
  trs := table.FindNodes('tr');
  for tr in trs do
  begin
    tds := tr.FindNodes('td');
    i := 1;
    for td in tds do
    begin
      s := td.Text;
      if i = 1 then
        k := s
      else if i = 2 then
        v := s;
      Inc(i);
      if i = 3 then
      begin
        Props.Values[k] := v;
        break;
      end;
    end;
    tds.Free;
  end;
  PropertyRows := trs; //trs.Free;
  { remember for now and free later in Reset method }
end;

procedure THtmlConverter.ReadResults(table: TXmlNode);
var
  nl, trs: TXmlNodeList;
begin
  Assert(HeaderFields = nil);
  nl := table.FindNodes('thead');
  if nl.Count = 1 then
  begin
    trs := nl.First.FindNodes('tr');
    if trs.Count = 1 then
      HeaderFields := trs.First.FindNodes('th');
    trs.Free;
  end;
  nl.Free;
  BodyRows := table.FindNodes('tr');
end;

end.
