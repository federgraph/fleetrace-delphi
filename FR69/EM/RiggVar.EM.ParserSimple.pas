unit RiggVar.EM.ParserSimple;

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
  RiggVar.EM.Parser,
  RiggVar.EM.XmlVerySimple;

type
  TSimpleEventMenuParser = class(TEventMenuParser)
  private
    Xml: TSimpleXml;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadXml(SL: TStrings); override;
  end;

implementation

constructor TSimpleEventMenuParser.Create;
begin
  Xml := TSimpleXml.Create;
  Xml.Header.Attribute['encoding'] := 'utf-8';
end;

destructor TSimpleEventMenuParser.Destroy;
begin
  Xml.Free;
  inherited;
end;

(*
procedure TSimpleEventMenuParser.Generate(SL: TStrings);
var
  menu, combo, data, image, btn: TXmlNode;
begin
{
<?xml version="1.0" encoding="utf-8" ?>
<EventMenu Root="http://www.riggvar.de/results/">
  <ComboEntry Caption="P 2011">
    <DataFolder Url="xml/">
      <ImgFolder Url="">
        <Btn Data="P-2011-49er.xml" Img="" Text="49er" />
        <Btn Data="P-2011-470-M.xml" Img="" Text="470 M" />
        <Btn Data="P-2011-470-W.xml" Img="" Text="470 W" />
        <Btn Data="P-2011-Finn.xml" Img="" Text="Finn" />
        <Btn Data="P-2011-Laser-M.xml" Img="" Text="Laser M" />
        <Btn Data="P-2011-Laser-W.xml" Img="" Text="Laser W" />
        <Btn Data="P-2011-RSX-M.xml" Img="" Text="RSX M" />
        <Btn Data="P-2011-RSX-W.xml" Img="" Text="RSX W" />
        <Btn Data="P-2011-Star.xml" Img="" Text="Star" />
      </ImgFolder>
    </DataFolder>
  </ComboEntry>
</EventMenu>
}

  Xml.Root.NodeName := 'EventMenu';
  Xml.Root.SetAttribute('Root', 'http://www.riggvar.de/results/');

  combo := Xml.Root.AddChild('ComboEntry');
  combo.SetAttribute('Caption', 'P 2001');

  data := combo.AddChild('DataFolder');
  data.SetAttribute('Url', 'xml');

  image := data.AddChild('ImgFolder');
  image.SetAttribute('Url', '');

  btn := image.AddChild('Btn');
  btn.SetAttribute('Data', 'P-2011-49er.xml');
  btn.SetAttribute('Img', '');
  btn.SetAttribute('Text', '49er');

  btn := image.AddChild('Btn');
  btn.SetAttribute('Data', 'P-2011-470-W.xml');
  btn.SetAttribute('Img', '');
  btn.SetAttribute('Text', '470 W');

  btn := image.AddChild('Btn');
  btn.SetAttribute('Data', 'P-2011-470-M.xml');
  btn.SetAttribute('Img', '');
  btn.SetAttribute('Text', '470 M');
end;
*)

procedure TSimpleEventMenuParser.ReadXml(SL: TStrings);
var
  combo, data, image, btn: TXmlNode;
  CNodes, DNodes, INodes, BNodes: TXmlNodeList;
  s: string;
begin
  Xml.Text := SL.Text;
  SL.Clear;

  s := Xml.Root.Attribute['Root'];
  SL.Add('Root=' + s); //Root=http://www.riggvar.de/results/

  CNodes := Xml.Root.FindNodes('ComboEntry');
  for combo in CNodes do
  begin
    s := combo.Attribute['Caption'];
    SL.Add('Caption=' + s); //Caption=P 2011
    DNodes := combo.FindNodes('DataFolder');
    for data in DNodes do
    begin
      s := data.Attribute['Url'];
      SL.Add('DataFolder=' + s); //DataFolder=xml/
      INodes := data.FindNodes('ImgFolder');
      for image in INodes do
      begin
        s := image.Attribute['Url'];
        SL.Add('ImgFolder=' + s); //ImgFolder=
        BNodes := image.FindNodes('Btn');
        for btn in BNodes do
        begin
          s := btn.Attribute['Data'];
          SL.Add('Data=' + s); //Data=P-2011-49er.xml
          s := btn.Attribute['Img'];
          SL.Add('Img=' + s); //Img=
          s := btn.Attribute['Text'];
          SL.Add('Text=' + s); //Text=49er
        end;
        BNodes.Free;
      end;
      INodes.Free;
    end;
    DNodes.Free;
  end;
  CNodes.Free;
end;

end.
