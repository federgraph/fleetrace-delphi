unit RiggVar.EM.EventData;

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
  RiggVar.EM.WorkspaceListBase,
  RiggVar.EM.Parser,
  RiggVar.EM.Transformer,
  RiggVar.EM.TransformerMSXML,
  RiggVar.EM.TransformerXml,
  RiggVar.EM.TransformerHtml;

type
  TEventData = class
  private
    TransformerXml: TEventDataTransformer;
    TransformerHtml: THtmlConverter;
    SL: TStrings;
    XSL: TStrings;
    TXT: TStrings;
    procedure InitXSL;
    function IsHtml: Boolean;
    function IsXml: Boolean;
    function RemovePreamble(const s: string): string;
    function GetText: string;
    function GetHasError: Boolean;
    function GetIsOK: Boolean;
    function MoveToHtml(const s: string): string;
    function MoveToFirstAngleBracket(const s: string): string;
  public
    WantSimpleParser: Boolean;
    UseXSL: Boolean;
    XSLOK: Boolean;
    Error: string;
    DataFormat: TDataFormat;
    constructor Create;
    destructor Destroy; override;
    procedure Load(ed: string);
    property Text: string read GetText;
    property HasError: Boolean read GetHasError;
    property IsOK: Boolean read GetIsOK;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.ResourceManager;

{ TEventData }

constructor TEventData.Create;
begin
  inherited Create;
  SL := TStringList.Create;
  XSL := TStringList.Create;
  TXT := TStringList.Create;
  WantSimpleParser := Main.IniImage.WantSimpleParser;
  if WantSimpleParser then
  begin
    TransformerXml := TSimpleTransformer.Create;
  end
  else
  begin
    TransformerXml := TXslTransformer.Create;
    InitXSL;
  end;
  TransformerHtml := THtmlConverter.Create;
end;

destructor TEventData.Destroy;
begin
  TransformerHtml.Free;
  TransformerXml.Free;
  TXT.Free;
  XSL.Free;
  SL.Free;
  inherited Destroy;
end;

function TEventData.GetHasError: Boolean;
begin
  result := Error <> '';
end;

function TEventData.GetIsOK: Boolean;
begin
  result := (TXT.Count > 4) and not HasError;
end;

function TEventData.GetText: string;
begin
  if Error = '' then
    result := TXT.Text
  else
    result := '';
end;

procedure TEventData.Load(ed: string);
var
   s: string;
begin
  if UseXSL and not XSLOK then
  begin
    TXT.Text := ed;
    exit;
  end;

  TXT.Clear;
  SL.Text := ed;

  if IsHtml then
  begin
    DataFormat := ffHtml;
    s := MoveToHtml(SL.Text);
    SL.Text := s;
    TransformerHtml.TransformSL(SL, XSL, TXT);
    if TransformerHtml.OK then
      Error := ''
    else
      Error := TransformerHtml.Error;
  end
  else if IsXml then
  begin
    DataFormat := ffXml;
    s := RemovePreamble(ed);
    SL.Text := s;
    TransformerXml.TransformSL(SL, XSL, TXT);
    if TransformerXml.OK then
      Error := ''
    else
      Error := TransformerXml.Error;
  end
  else
  begin
    DataFormat := ffTxt;
    s := RemovePreamble(ed);
    SL.Text := s;
    TXT.Assign(SL);
    SL.Clear;
  end;
end;

procedure TEventData.InitXSL;
begin
  XSL.Text := Main.ResourceManager.LoadText(FRXML20_xsl);
  XSLOK := true;
//  if FileExists('FRXML20.xsl') then
//  begin
//    XSL.LoadFromFile('FRXML20.xsl');
//    XSLOK := true;
//  end;
end;

function TEventData.RemovePreamble(const s: string): string;
var
  up: string;
begin
  result := s;
  up := StringOf(TEncoding.UTF8.GetPreamble);
  if Pos(up, s) = 1 then
  begin
    if Length(s) > 3 then
      result := Copy(s, 4);
  end
  else if Pos('o;?', s) = 1 then
  begin
    if Length(s) > 3 then
      result := Copy(s, 4);
  end;
end;

function TEventData.IsXml: Boolean;
var
  i: Integer;
  s: string;
  c: Integer;
begin
  result := false;
  c := Min(SL.Count, 10); //max 10 lines
  for i := 0 to c-1 do
  begin
    s := SL[i];
    if Pos('<?', SL[i]) > 0 then
      result := true;
    if Pos('<', SL[i]) = 1 then
      result := true;
    if Pos('/>', SL[i]) > 1 then
      result := true;
    if result = true then
      break;
  end;
end;

function TEventData.IsHtml: Boolean;
var
  i: Integer;
  s: string;
  c: Integer;
  p: Integer;
  IsFragment: Boolean;
begin
  IsFragment := false;
  result := false;
  c := Min(SL.Count, 10); //max 10 lines
  for i := 0 to c-1 do
  begin
    s := SL[i];
    p := Pos('<html', s);
    if p > 0 then
    begin
      result := true;
      break;
    end;
    p := Pos('<thead', s);
    if p > 0 then
    begin
      result := true;
      IsFragment := true;
      break;
    end;
    p := Pos('<h2>', s);
    if p > 0 then
    begin
      result := true;
      IsFragment := true;
      break;
    end;
  end;
  if IsFragment then
  begin
    if SL.Count > 0 then
    begin
      s := SL[0];
      s := MoveToFirstAngleBracket(s);
      SL[0] := s;
    end;
    SL.Insert(0, '<html><body>');
    SL.Add('</body></html>')
  end;
end;

function TEventData.MoveToHtml(const s: string): string;
var
  t: string;
  p: Integer;
begin
  result := s;
  t := '<html';
  p := Pos(t, s);
  if p > 0 then
  begin
    if Length(s) > 3 then
      result := Copy(s, p, MaxInt);
  end
end;

function TEventData.MoveToFirstAngleBracket(const s: string): string;
var
  t: string;
  p: Integer;
begin
  result := s;
  t := '<';
  p := Pos(t, s);
  if p > 0 then
  begin
    if Length(s) > 3 then
      result := Copy(s, p, MaxInt);
  end
end;

end.
