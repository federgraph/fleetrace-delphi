unit RiggVar.EM.XmlVerySimple;

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

{ VerySimpleXML v1.0 - a lightweight, one-unit XML reader/writer
  by Dennis Spreen
  http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/

  (c) Copyrights 2011 Dennis D. Spreen <dennis@spreendigital.de>
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations. Only requirement:
  This text must be present without changes in all modifications of library.

  * The contents of this file are used with permission, subject to
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *
}

uses
  System.Classes,
  Generics.Defaults,
  Generics.Collections;

type
  TXmlNodeList = class;

  TXmlAttribute = class(TObject)
  public
    Name: string; // Attribute name
    Value: string; // Attribute value (always as string)
  end;

  TXmlAttributeList = class(TObjectList<TXmlAttribute>)
  public
    function Find(AttrName: string): TXmlAttribute;
    // Find an Attribute by Name (not case sensitive)
  end;

  TXmlNode = class(TObject)
  private
    FAttributes: TXmlAttributeList;
    function GetAttribute(const AttrName: string): string;
    procedure SetAttr(const AttrName: string; const Value: string);
  public
    Parent: TXmlNode; // nil only for Root-Node
    NodeName: string; // Node name
    Text: string; // Node text
    ChildNodes: TXmlNodeList; // Child nodes, never nil
    constructor Create; virtual;
    destructor Destroy; override;
    // Find a childnode by its name
    function Find(Name: string): TXmlNode; overload;
    // Find a childnode by Name/Attribute
    function Find(Name, Attribute: string): TXmlNode; overload;
    // Find a childnode by Name/Attribute/Value
    function Find(Name, Attribute, Value: string): TXmlNode; overload;
    // Return a list of childodes with given Name
    function FindNodes(Name: string): TXmlNodeList; virtual;
    // Returns True if the Attribute exits
    function HasAttribute(const Name: string): Boolean; virtual;
    // Add a child node and return it
    function AddChild(const Name: string): TXmlNode; virtual;
    function SetText(Value: string): TXmlNode; virtual;
    function SetAttribute(const AttrName: string; const Value: string)
      : TXmlNode; virtual;
    property Attribute[const AttrName: string]: string read GetAttribute
      write SetAttr; // Attributes of a Node, accessible by attribute name
  end;

  TXmlNodeList = class(TObjectList<TXmlNode>);

  TSimpleXml = class(TObject)
  private
    SL: TStringList;
    procedure Walk(Lines: TstringList; Prefix: string; Node: TXmlNode);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure Parse;
  public
    Root: TXmlNode; // There is only one Root Node
    Header: TXmlNode; // XML declarations are stored in here as Attributes
    Ident: string; // Set Ident:='' if you want a compact output

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    property Text: string read GetText write SetText;
  end;

implementation

uses
  SysUtils;

{ TSimpleXml }

constructor TSimpleXml.Create;
begin
  inherited;
  SL := TstringList.Create;
  Clear;
end;

destructor TSimpleXml.Destroy;
begin
  Root.Free;
  Header.Free;
  SL.Free;
  inherited;
end;

procedure TSimpleXml.Clear;
begin
  Root.Free;
  Header.Free;
  Root := TXmlNode.Create;
  Header := TXmlNode.Create;
  Header.NodeName := '?xml'; // Default XML Header
  Header.Attribute['version'] := '1.0'; // Default XML Version
  Ident := '  '; // Set Ident:='' if you want a compact output
  SL.Clear;
end;

procedure TSimpleXml.Parse;
var
  Line: string;
  IsTag, IsText: Boolean;
  Tag, Text: string;
  Parent, Node: TXmlNode;
  I: Integer;
  Attribute: TXmlAttribute;
  ALine, Attr, AttrText: string;
  P: Integer;
  IsselfClosing: Boolean;
  IsQuote: Boolean;

  // Return a text ended by StopChar, respect quotation marks
  function GetText(var Line: string; StartStr: string; StopChar: Char): string;
  var
    Chr: Char;
  begin
    while (Length(Line) > 0) and ((Line[1] <> StopChar) or (IsQuote)) do
    begin
      Chr := Line[1];
      if Chr = '"' then
        IsQuote := not IsQuote;
      StartStr := StartStr + Chr;
      delete(Line, 1, 1);
    end;
    Result := StartStr;
  end;

begin
  if Assigned(Root) then // Release previous nodes (if set)
    Root.Free;

  IsTag := False;
  IsText := False;
  IsQuote := False;
  Node := nil;

  for I := 0 to SL.Count - 1 do
  begin
    Line := SL[I];

    while (Length(Line) > 0) do
    begin
      if (not IsTag) and (not IsText) then
      begin
        while (Length(Line) > 0) and (Line[1] <> '<') do
          delete(Line, 1, 1);

        if Length(Line) > 0 then
        begin
          IsTag := True;
          delete(Line, 1, 1); // Delete openining tag
          Tag := '';
        end;
      end;

      if IsTag then
      begin
        Tag := GetText(Line, Tag, '>');

        if (Length(Line) > 0) and (Line[1] = '>') then
        begin
          delete(Line, 1, 1);
          IsTag := False;

          if (Length(Tag) > 0) and (Tag[1] = '/') then
            Node := Node.Parent
          else
          begin
            Parent := Node;
            IsText := True;
            IsQuote := False;

            Node := TXmlNode.Create;
            if Lowercase(copy(Tag, 1, 4)) = '?xml' then // check for xml header
            begin
              Header.Free;
              Header := Node;
            end;

            // self-Closing Tag
            if (Length(Tag) > 0) and (Tag[Length(Tag)] = '/') then
            begin
              IsselfClosing := True;
              delete(Tag, Length(Tag), 1);
            end
            else
              IsselfClosing := False;

            P := pos(' ', Tag);
            if P <> 0 then // Tag name has attributes
            begin
              ALine := Tag;
              delete(Tag, P, Length(Tag));
              delete(ALine, 1, P);

              while Length(ALine) > 0 do
              begin
                Attr := GetText(ALine, '', '='); // Get Attribute Name
                AttrText := GetText(ALine, '', ' '); // Get Attribute Value

                if Length(AttrText) > 0 then
                begin
                  delete(AttrText, 1, 1); // Remove blank

                  if AttrText[1] = '"' then // Remove start/end quotation marks
                  begin
                    delete(AttrText, 1, 1);
                    if AttrText[Length(AttrText)] = '"' then
                      delete(AttrText, Length(AttrText), 1);
                  end;
                end;

                if Length(ALine) > 0 then
                  delete(ALine, 1, 1);

                // Header node (Attr='?') does not support Attributes
                if not((Node = Header) and (Attr = '?')) then
                begin
                  Attribute := TXmlAttribute.Create;
                  Attribute.Name := Attr;
                  Attribute.Value := AttrText;
                  Node.FAttributes.Add(Attribute);
                end;
                IsQuote := False;
              end;
            end;

            Node.NodeName := Tag;
            Node.Parent := Parent;
            if Assigned(Parent) then
              Parent.ChildNodes.Add(Node)
            else if Node = Header then
            begin
              IsText := False;
              Node := nil;
            end
            else
              Root := Node;

            Text := '';
            if IsselfClosing then
              Node := Node.Parent;
          end;
        end;
      end;

      if IsText then
      begin
        Text := GetText(Line, Text, '<');
        if (Length(Line) > 0) and (Line[1] = '<') then
        begin
          IsText := False;
          while (Length(Text) > 0) and (Text[1] = ' ') do
            delete(Text, 1, 1);
          Node.Text := Text;
        end;
      end;

    end;
  end;
end;

procedure TSimpleXml.Walk(Lines: TStringList; Prefix: string;
  Node: TXmlNode);
var
  Child: TXmlNode;
  Attribute: TXmlAttribute;
  OriginalPrefix: string;
  s: string;
  IsselfClosing: Boolean;
begin
  s := Prefix + '<' + Node.NodeName;
  for Attribute in Node.FAttributes do
    s := s + ' ' + Attribute.Name + '="' + Attribute.Value + '"';

  if Node = Header then
    s := s + ' ?';

  IsselfClosing := (Length(Node.Text) = 0) and (Node.ChildNodes.Count = 0) and
    (Node <> Header);
  if IsselfClosing then
    s := s + ' /';

  s := s + '>';
  if Length(Node.Text) > 0 then
    s := s + Node.Text;

  if (Node.ChildNodes.Count = 0) and (Length(Node.Text) > 0) then
  begin
    s := s + '</' + Node.NodeName + '>';
    Lines.Add(s);
  end
  else
  begin
    Lines.Add(s);
    OriginalPrefix := Prefix;
    Prefix := Prefix + Ident;
    for Child in Node.ChildNodes do
      Walk(Lines, Prefix, Child);
    if (Node <> Header) and (not IsselfClosing) then
      Lines.Add(OriginalPrefix + '</' + Node.NodeName + '>');
  end;
end;

function TSimpleXml.GetText: string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  if Ident = '' then
    Lines.LineBreak := '';

  Walk(Lines, '', Header);
  Walk(Lines, '', Root);

  result := Lines.Text;
  Lines.Free;
end;

procedure TSimpleXml.SetText(const Value: string);
begin
  Clear;
  SL.Text := Value;
  Parse;
end;

{ TXmlNode }

function TXmlNode.AddChild(const Name: string): TXmlNode;
begin
  result := TXmlNode.Create;
  result.NodeName := Name;
  result.Parent := self;
  ChildNodes.Add(result);
end;

constructor TXmlNode.Create;
begin
  ChildNodes := TXmlNodeList.Create;
  Parent := nil;
  FAttributes := TXmlAttributeList.Create;
end;

destructor TXmlNode.Destroy;
begin
  FAttributes.Free;
  ChildNodes.Free;
  inherited;
end;

function TXmlNode.Find(Name: string): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := nil;
  Name := Lowercase(Name);
  for Node in ChildNodes do
    if Lowercase(Node.NodeName) = Name then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNode.Find(Name, Attribute, Value: string): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := nil;
  Name := Lowercase(Name);
  for Node in ChildNodes do
    if (Lowercase(Node.NodeName) = Name) and (Node.HasAttribute(Attribute)) and
      (Node.Attribute[Attribute] = Value) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNode.Find(Name, Attribute: string): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := nil;
  Name := Lowercase(Name);
  for Node in ChildNodes do
    if (Lowercase(Node.NodeName) = Name) and (Node.HasAttribute(Attribute)) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNode.FindNodes(Name: string): TXmlNodeList;
var
  Node: TXmlNode;
begin
  Result := TXmlNodeList.Create(False);
  Name := Lowercase(Name);
  for Node in ChildNodes do
    if (Lowercase(Node.NodeName) = Name) then
      Result.Add(Node);
end;

function TXmlNode.GetAttribute(const AttrName: string): string;
var
  Attribute: TXmlAttribute;
begin
  Attribute := FAttributes.Find(AttrName);
  if Assigned(Attribute) then
    Result := Attribute.Value
  else
    Result := '';
end;

function TXmlNode.HasAttribute(const Name: string): Boolean;
begin
  Result := Assigned(FAttributes.Find(Name));
end;

procedure TXmlNode.SetAttr(const AttrName, Value: string);
begin
  SetAttribute(AttrName, Value);
end;

function TXmlNode.SetAttribute(const AttrName, Value: string): TXmlNode;
var
  Attribute: TXmlAttribute;                begin
  Attribute := FAttributes.Find(AttrName); // Search for given name
  if not Assigned(Attribute) then // If attribute is not found, create one
  begin
    Attribute := TXmlAttribute.Create;
    FAttributes.Add(Attribute);
  end;
  Attribute.Name := AttrName; // this allows "name-style" rewriting
  Attribute.Value := Value;
  Result := self;
end;

function TXmlNode.SetText(Value: string): TXmlNode;
begin
  Text := Value;
  Result := self;
end;

{ TXmlAttributeList }

function TXmlAttributeList.Find(AttrName: string): TXmlAttribute;
var
  Attribute: TXmlAttribute;
begin
  Result := nil;
  AttrName := Lowercase(AttrName);
  for Attribute in self do
    if Lowercase(Attribute.Name) = AttrName then
    begin
      Result := Attribute;
      Break;
    end;
end;

end.
