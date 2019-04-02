unit RiggVar.Out.Base;

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
  System.Classes,
  RiggVar.Util.WebUtils,
  RiggVar.Col.BaseEntry,
  RiggVar.Out.Intf,
  RiggVar.Util.Classes;

type
  TBaseOutput = class(TInterfacedObject, IOutput)
  public
    TokenParser: TTokenParser;
    SL: TStrings;

    sep: string;
    MsgID: Integer;
    OutputType: TOutputType;
    XMLSection: Boolean;

    WantHTMEscape: Boolean;
    WantPageHeader: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure PageHeader;
    procedure PageFooter;
    procedure SectionHeader(s: string);
    procedure EscapeHTM;

    function GetMsg(sRequest: string): string; virtual;
    function GetAll(OutputRequestList: TStringList): string; virtual;
  end;

implementation

{ TBaseOutput }

constructor TBaseOutput.Create;
begin
  inherited Create;
  TokenParser := TTokenParser.Create;
  SL := TStringList.Create;
  sep := ';';
end;

destructor TBaseOutput.Destroy;
begin
  SL.Free;
  TokenParser.Free;
  inherited;
end;

procedure TBaseOutput.EscapeHTM;
var
  i: Integer;
  s: string;
begin
  for i := 0 to SL.Count -1 do
  begin
    s := SL[i];
    SL[i] := WebUtils.HTMLEncode(s);
  end;
  SL.Insert(0, '<pre>');
  SL.Add('</pre>')
end;

function TBaseOutput.GetAll(OutputRequestList: TStringList): string;
begin
  result := 'not implemented';
end;

function TBaseOutput.GetMsg(sRequest: string): string;
begin
  result := '';
end;

procedure TBaseOutput.PageFooter;
begin
  case OutputType of
    otCSV:
    begin
    end;

    otHTM:
    begin
      SL.Add('</body>');
      SL.Add('</html>');
    end;

    otXML:
    begin
    end;
  end;
end;

procedure TBaseOutput.PageHeader;
begin
  case OutputType of
    otCSV:
    begin
    end;

    otHTM:
    begin
      SL.Add('<html>');
      SL.Add('<head>');
      SL.Add('<style><!-- pre {color="maroon"} --> </style>');
      SL.Add('</head>');
      SL.Add('<body>');
    end;

    otXML:
    begin
      SL.Add('<?xml version="1.0" encoding="ISO-8859-1" ?>');
    end;
  end;
end;

procedure TBaseOutput.SectionHeader(s: string);
begin
  if OutputType = otCSV then
  begin
    SL.Add('');
    SL.Add('#' + s);
    SL.Add('');
  end;
end;

end.
