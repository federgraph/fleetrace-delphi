unit RiggVar.Util.Classes;

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
  RiggVar.DAL.Redirector;

const
  BoolStr: array[Boolean] of string = ('False', 'True');
  BoolInt: array[Boolean] of Integer = (0, 1);

type
  TTokenParser = class
  public
    sToken: string;
    sRest: string;
    procedure NextToken;
    function NextTokenX(TokenName: string): Integer;
  end;

  TLineParser = class
  private
    SL: TStringList; //helper object, Commatext used to parse line when loading
  protected
    function ParseKeyValue(Key, Value: string): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseLine(const s: string): Boolean;
  end;

  TextWriter = class
  private
    fFileName: string;
    SL: TStringList;
    function GetStrings: TStrings;
  public
    constructor Create(fn: string);
    destructor Destroy; override;
    procedure Write(s: string);
    procedure WriteLine(s: string);
    procedure Flush;
    procedure Close;
    property Strings: TStrings read GetStrings;
  end;

  TWebContentLoader = class
  private
    SL: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetContent(fn: string): string;
  end;

  TUtils = class
  public
    class function IsTrue(const Value: string): Boolean; static;
    class function StartsWith(const s, substring: string): Boolean; static;
    class function Cut(delim: string; s: string; var token: string): string; static;
    class function IncludeTrailingSlash(const s: string): string; static;
    class function PrettyFormat(const s: String; const AsHTML: Boolean = false): String; static;
  end;

implementation

uses
  System.Character,
  RiggVar.App.Main;

{ Utils }

class function TUtils.StartsWith(const s, substring: string): Boolean;
begin
  result := Copy(s, 1, Length(substring)) = substring;
end;

class function TUtils.IsTrue(const Value: string): Boolean;
var
  s: string;
begin
  result := False;
  s := UpperCase(Value);
  if (s = 'TRUE') or (s = 'T') or (s = '1') then
    result := True
end;

class function TUtils.Cut(delim: string; s: string; var token: string): string;
//Trennt einen String beim Trennzeichen
// delim : Trennzeichen -> erstes Auftreten = Trennposition
// parameter s : input
// var parameter token : output, vorn abgeschnitten
// Result liefert den rest
var
  posi: integer; // Trennposition
begin
  posi := pos(delim, s);
  if posi > 0 then
  begin
    token := trim(copy(s, 1, posi - 1));
    result := trim(copy(s, posi + 1, length(s)));
  end
  else
  begin
    token := s;
    result := '';
  end;
end;

class function TUtils.IncludeTrailingSlash(const s: string): string;
begin
  if s = '' then
    result := '/'
  else if not (s[Length(s)] = '/') then
    result := s + '/'
  else
    result := s;
end;

class function TUtils.PrettyFormat(const s: string; const AsHTML: Boolean): string;
// Written by Lars Fosdal, September 2018
var
  sEOL: string;
  sINDENT: string;
  LIndent: string;
  nIndent: Integer;

  procedure Dent;
  var
    ix: Integer;
  begin
    LIndent := '';
    for ix := 1 to nIndent do
      LIndent := LIndent + sINDENT;
  end;
  procedure Indent;
  begin
    Inc(nIndent);
    Dent;
  end;
  procedure Outdent;
  begin
    Dec(nIndent);
    Dent;
  end;

var
  c: char;
  isInString: Boolean;
  isEscape: Boolean;
  isUnhandled: Boolean;
begin
  result := '';
  nIndent := 0;
  if AsHTML then
  begin
    sEOL := '<br>';
    sINDENT := '&nbsp;&nbsp;';
  end
  else
  begin
    sEOL := #13#10;
    sINDENT := '  ';
  end;
  isInString := false;
  isEscape := false;
  LIndent := '';
  for c in s do
  begin
    if not isInString then
    begin
      isUnhandled := True;
      if ((c = '{') or (c = '[')) then
      begin
        Indent;
        Result := Result + c + sEOL + LIndent;
        isUnhandled := false;
      end
      else if (c = ',') then
      begin
        Result := Result + c + sEOL + LIndent;
        isUnhandled := false;
      end
      else if ((c = '}') or (c = ']')) then
      begin
        Outdent;
        Result := Result + sEOL + LIndent + c;
        isUnhandled := false;
      end;

      if isUnhandled and not c.IsWhiteSpace then
        Result := Result + c;
    end
    else
    begin
      Result := Result + c;
    end;

    if (isEscape = false) and (c = '"') then
      isInString := not isInString;

    isEscape := (c = '\') and (isEscape = false);
  end;
end;

{ TTokenParser }

procedure TTokenParser.NextToken;
begin
  sRest := TUtils.Cut('.', sRest, sToken);
end;

function TTokenParser.NextTokenX(TokenName: string): Integer;
var
  l: Integer;
begin
  NextToken;
  result := -1;
  l := Length(TokenName);
  if Copy(sToken, 1, l) = TokenName then
  begin
    sToken := Copy(sToken, l+1, Length(sToken) - l);
    result := StrToIntDef(sToken, -1);
  end;
end;

{ TextWriter }

procedure TextWriter.Close;
begin
  if fFileName <> '' then
    Main.StoreAdapter.StringListSaveToFile(SL, fFileName);
end;

constructor TextWriter.Create(fn: string);
begin
  inherited Create;
  fFileName := fn;
  SL := TDBStringList.Create;
end;

destructor TextWriter.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TextWriter.Flush;
begin
  //
end;

function TextWriter.GetStrings: TStrings;
begin
  result := SL;
end;

procedure TextWriter.Write(s: string);
begin
  SL[SL.Count-1] := SL[SL.Count-1] + s;
end;

procedure TextWriter.WriteLine(s: string);
begin
  SL.Add(s);
end;

{ TLineParser }

constructor TLineParser.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TLineParser.Destroy;
begin
  SL.Free;
  inherited;
end;

function TLineParser.ParseLine(const s: string): Boolean;
var
  sK: string;
  sV: string;
  temp: string;
  i: Integer;
begin
  SL.Clear;
  i := Pos('=', s);
  if i > 0 then
    temp := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)))
  else
    temp := StringReplace(Trim(s), ' ', '_', [rfReplaceAll]);

  if Pos('=', temp) = 0 then
    temp := temp + '=';
  SL.Add(temp);
  sK := SL.Names[0];
  sV := SL.Values[sK];
  //StringReplace(sV, '_', ' ', [rfReplaceAll]);
  result := ParseKeyValue(sK, sV);
end;

function TLineParser.ParseKeyValue(Key, Value: string): Boolean;
begin
  //virtual, this implementation only used in unit test.
  result := (Key = 'Key') and (Value = 'Value');
end;

{ TWebContentLoader }

constructor TWebContentLoader.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TWebContentLoader.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TWebContentLoader.GetContent(fn: string): string;
begin
  try
    SL.LoadFromFile(fn);
    result := SL.Text;
    SL.Clear;
  except
    result := '';
  end;
end;

end.
