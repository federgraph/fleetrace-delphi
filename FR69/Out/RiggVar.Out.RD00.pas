unit RiggVar.Out.RD00;

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
  RiggVar.BO.StartInfo;

type
  IRaceDataXMLWriter = class
  public
    Dir: string;
    Extension: string;
    constructor Create;
    destructor Destroy; override;
    procedure GetXML(Memo: TStrings); virtual;
    procedure WriteXML; virtual;
    function ToString: string; override;
  end;

  TRaceDataXML = class
  private
    FWriter: IRaceDataXMLWriter;
    procedure SetWriter(const Value: IRaceDataXMLWriter);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetXML(Memo: TStrings);
    procedure WriteXML;
    function ToString: string; override;
    property Writer: IRaceDataXMLWriter read FWriter write SetWriter;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Out.RD01;

const
  XML_NOT_AVAILABLE = '<?xml version="1.0" ?>' +
  '<race-data>not available</race-data>';

{ TRaceDataXML }

procedure TRaceDataXML.SetWriter(const Value: IRaceDataXMLWriter);
begin
  FWriter := Value;
end;

constructor TRaceDataXML.Create;
begin
  FWriter := TRaceDataXMLWriter.Create;
end;

destructor TRaceDataXML.Destroy;
begin
  Writer.Free;
  Writer := nil;
  inherited;
end;

procedure TRaceDataXML.GetXML(Memo: TStrings);
begin
  if Assigned(Writer) then
    Writer.GetXML(Memo);
end;

function TRaceDataXML.ToString: string;
begin
  if Assigned(Writer) then
    result := Writer.ToString
  else
    result := '';
end;

procedure TRaceDataXML.WriteXML;
begin
  if Assigned(Writer) then
    Writer.WriteXML;
end;

{ IRaceDataXMLWriter }

constructor IRaceDataXMLWriter.Create;
begin
  Dir := IncludeTrailingPathDelimiter(Main.IniImage.RDDataDir);
  Extension := '.race-data.xml';
end;

destructor IRaceDataXMLWriter.Destroy;
begin
  inherited;
end;

procedure IRaceDataXMLWriter.GetXML(Memo: TStrings);
begin
  Memo.Text := XML_NOT_AVAILABLE;
end;

function IRaceDataXMLWriter.ToString: string;
begin
  result := XML_NOT_AVAILABLE;
end;

procedure IRaceDataXMLWriter.WriteXML;
begin

end;

end.
