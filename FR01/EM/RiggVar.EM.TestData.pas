unit RiggVar.EM.TestData;

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
  System.Classes;

type
  TEventMenuTestData = class
  private
    SL: TStringList;
    procedure InitXml;
    procedure InitTxt;
    function GetTxt: string;
    function GetXml: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Transform(data: string): string;
    property Xml: string read GetXml;
    property Txt: string read GetTxt;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.EM.Parser,
  RiggVar.EM.ParserSimple,
  RiggVar.EM.ParserXslt;

{ TEventMenuTestData }

constructor TEventMenuTestData.Create;
begin
  SL := TStringList.Create;
end;

destructor TEventMenuTestData.Destroy;
begin
  SL.Free;
  inherited;
end;

function TEventMenuTestData.GetTxt: string;
begin
  SL.Clear;
  InitTxt;
  result := SL.Text;
  SL.Clear;
end;

function TEventMenuTestData.GetXml: string;
begin
  SL.Clear;
  InitXml;
  result := SL.Text;
  SL.Clear;
end;

procedure TEventMenuTestData.InitXml;
begin
  SL.Add('<EventMenu Root="http://gsmac/CubeImages/">');
    SL.Add('<ComboEntry Caption="Athen 2004">');
      SL.Add('<DataFolder Url="Results05/" >');
        SL.Add('<ImgFolder Url="Images02/">');
          SL.Add('<Btn Data="Event-01.xml" Img="Seite-1.png" Text="49er" />');
          SL.Add('<Btn Data="Event-02.xml" Img="Seite-2.png" Text="470 W" />');
          SL.Add('<Btn Data="Event-03.xml" Img="Seite-3.png" Text="470 M" />');
          SL.Add('<Btn Data="Event-04.xml" Img="Seite-4.png" Text="Europe" />');
          SL.Add('<Btn Data="Event-05.xml" Img="Seite-5.png" Text="Finn" />');
          SL.Add('<Btn Data="Event-06.xml" Img="Seite-6.png" Text="Laser" />');
        SL.Add('</ImgFolder>');
        SL.Add('<ImgFolder Url="Images03/">');
          SL.Add('<Btn Data="Event-07.xml" Img="Seite-1.png" Text="Mistral M" />');
          SL.Add('<Btn Data="Event-08.xml" Img="Seite-2.png" Text="Mistral W" />');
          SL.Add('<Btn Data="Event-09.xml" Img="Seite-3.png" Text="Star" />');
          SL.Add('<Btn Data="Event-10.xml" Img="Seite-4.png" Text="Tornado" />');
          SL.Add('<Btn Data="Event-11.xml" Img="Seite-5.png" Text="Yngling" />');
        SL.Add('</ImgFolder>');
      SL.Add('</DataFolder>');
    SL.Add('</ComboEntry>');
  SL.Add('</EventMenu>');
end;

procedure TEventMenuTestData.InitTxt;
begin
  SL.Add('Root=http://gsmac/CubeImages/');
  SL.Add('Caption=Athen 2004');
  SL.Add('DataFolder=Results05/');
  SL.Add('ImgFolder=Images02/');

  SL.Add('Data=Event-01.xml');
  SL.Add('Img=Seite-1.png');
  SL.Add('Text=49er');
  SL.Add('Data=Event-02.xml');
  SL.Add('Img=Seite-2.png');
  SL.Add('Text=470 W');
  SL.Add('Data=Event-03.xml');
  SL.Add('Img=Seite-3.png');
  SL.Add('Text=470 M');
  SL.Add('Data=Event-04.xml');
  SL.Add('Img=Seite-4.png');
  SL.Add('Text=Europe');
  SL.Add('Data=Event-05.xml');
  SL.Add('Img=Seite-5.png');
  SL.Add('Text=Finn');
  SL.Add('Data=Event-06.xml');
  SL.Add('Img=Seite-6.png');
  SL.Add('Text=Laser');

  SL.Add('ImgFolder=Images03/');

  SL.Add('Event-07.xml');
  SL.Add('Img=Seite-1.png');
  SL.Add('Text=Mistral M');
  SL.Add('Data=Event-08.xml');
  SL.Add('Img=Seite-2.png');
  SL.Add('Text=Mistral W');
  SL.Add('Data=Event-09.xml');
  SL.Add('Img=Seite-3.png');
  SL.Add('Text=Star');
  SL.Add('Data=Event-10.xml');
  SL.Add('Img=Seite-4.png');
  SL.Add('Text=Tornado');
  SL.Add('Data=Event-11.xml');
  SL.Add('Img=Seite-5.png');
  SL.Add('Text=Yngling');

  SL.Add('Caption=Cascais 2007');
  SL.Add('DataFolder=Results06/');
  SL.Add('ImgFolder=Images08/');

  SL.Add('Data=Event-01.xml');
  SL.Add('Img=Seite-1.png');
  SL.Add('Text=49er');
  SL.Add('Data=Event-02.xml');
  SL.Add('Img=Seite-2.png');
  SL.Add('Text=470 W');
  SL.Add('Data=Event-03.xml');
  SL.Add('Img=Seite-3.png');
  SL.Add('Text=470 M');
  SL.Add('Data=Event-04.xml');
  SL.Add('Img=Seite-4.png');
  SL.Add('Text=Europe');
  SL.Add('Data=Event-05.xml');
  SL.Add('Img=Seite-5.png');
  SL.Add('Text=Finn');
  SL.Add('Data=Event-06.xml');
  SL.Add('Img=Seite-6.png');
  SL.Add('Text=Laser');

end;

function TEventMenuTestData.Transform(data: string): string;
var
  p: TEventMenuParser;
begin
  SL.Text := data;
  if Main.IniImage.WantSimpleParser then
    p := TSimpleEventMenuParser.Create
  else
    p := TEventMenuParserXslt.Create;
  try
    if p.IsXml(SL) then
      p.ReadXml(SL);
  finally
    p.Free;
  end;
  result := SL.Text;
end;

end.
