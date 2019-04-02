unit RiggVar.Out.JS00;

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
  IJavaScoreXMLWriter = class
  public
    Dir: string;
    Extension: string;
    DivisionInfo: TDivisionInfo;
    constructor Create;
    destructor Destroy; override;
    procedure GetXML(Memo: TStrings); virtual;
    procedure WriteXML; virtual;
    function ToString: string; override;
    procedure ShowSchedule; virtual;
  end;

  TJavaScoreXML = class
  private
    FWriter: IJavaScoreXMLWriter;
    procedure SetWriter(const Value: IJavaScoreXMLWriter);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetXML(Memo: TStrings);
    procedure WriteXML;
    function ToString: string; override;
    procedure ShowSchedule;
    property Writer: IJavaScoreXMLWriter read FWriter write SetWriter;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Out.JS01,
  RiggVar.Out.JS03;

const
  XML_NOT_AVAILABLE = '<?xml version="1.0" ?>' +
  '<Regatta>not implemented</Regatta>';

{ TJavaScoreXML }

procedure TJavaScoreXML.SetWriter(const Value: IJavaScoreXMLWriter);
begin
  FWriter := Value;
end;

procedure TJavaScoreXML.ShowSchedule;
begin
  if Assigned(Writer) then
    Writer.ShowSchedule;
end;

constructor TJavaScoreXML.Create;
begin
  FWriter := TJavaScoreXMLWriter03.Create;
end;

destructor TJavaScoreXML.Destroy;
begin
  Writer.Free;
  Writer := nil;
  inherited;
end;

procedure TJavaScoreXML.GetXML(Memo: TStrings);
begin
  if Assigned(Writer) then
    Writer.GetXML(Memo);
end;

function TJavaScoreXML.ToString: string;
begin
  if Assigned(Writer) then
    result := Writer.ToString
  else
    result := '';
end;

procedure TJavaScoreXML.WriteXML;
begin
  if Assigned(Writer) then
    Writer.WriteXML;
end;

{ IJavaScoreXMLWriter }

constructor IJavaScoreXMLWriter.Create;
begin
  DivisionInfo := TDivisionInfo.Create;
  Dir := IncludeTrailingPathDelimiter(Main.IniImage.JSDataDir);
  Extension := '.regatta';
end;

destructor IJavaScoreXMLWriter.Destroy;
begin
  DivisionInfo.Free;
  inherited;
end;

procedure IJavaScoreXMLWriter.GetXML(Memo: TStrings);
begin
  Memo.Text := XML_NOT_AVAILABLE;
end;

procedure IJavaScoreXMLWriter.ShowSchedule;
begin

end;

function IJavaScoreXMLWriter.ToString: string;
begin
  result := XML_NOT_AVAILABLE;
end;

procedure IJavaScoreXMLWriter.WriteXML;
begin

end;

end.

