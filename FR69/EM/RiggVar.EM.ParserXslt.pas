unit RiggVar.EM.ParserXslt;

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
  RiggVar.EM.Parser,
  RiggVar.EM.TransformerMSXML;

type
  TEventMenuParserXSLT = class(TEventMenuParser)
  private
    procedure InitXSL(XSL: TStrings);
    function RemovePreamble(const s: string): string;
  public
    procedure ReadXml(SL: TStrings); override;
  end;

implementation

{ TEventMenuParserXSLT }

procedure TEventMenuParserXSLT.ReadXml(SL: TStrings);
var
  Transformer: TXslTransformer;
  XSL: TStrings;
  TXT: TStrings;
begin
  SL[0] := RemovePreamble(SL[0]);
  XSL := TStringList.Create;
  TXT := TStringList.Create;
  Transformer := TXslTransformer.Create;
  try
    InitXSL(XSL);
    Transformer.TransformSL(SL, XSL, TXT);
    if Transformer.OK then
    begin
      SL.Assign(TXT);
    end;
  finally
    Transformer.Free;
    XSL.Free;
    TXT.Free;
  end;
end;

function TEventMenuParserXSLT.RemovePreamble(const s: string): string;
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

procedure TEventMenuParserXSLT.InitXSL(XSL: TStrings);
begin
  with XSL do
  begin
    Add('<?xml version="1.0" encoding="UTF-8"?>');
    Add('<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">');
    Add('<xsl:output method="text" encoding="iso-8859-1"/>');

    Add('<xsl:template match="/">');
      Add('<xsl:text>Root=</xsl:text>');
      Add('<xsl:value-of select="EventMenu/@Root" />');
      Add('<xsl:text>&#xA;</xsl:text>');

      Add('<xsl:apply-templates select="EventMenu/ComboEntry" />');
    Add('</xsl:template>');

    Add('<xsl:template match="ComboEntry">');
      Add('<xsl:text>Caption=</xsl:text>');
      Add('<xsl:value-of select="@Caption" />');
      Add('<xsl:text>&#xA;</xsl:text>');

      Add('<xsl:apply-templates select="DataFolder" />');
    Add('</xsl:template>');

    Add('<xsl:template match="DataFolder">');
      Add('<xsl:text>DataFolder=</xsl:text>');
      Add('<xsl:value-of select="@Url" />');
      Add('<xsl:text>&#xA;</xsl:text>');

      Add('<xsl:apply-templates select="ImgFolder" />');
    Add('</xsl:template>');

    Add('<xsl:template match="ImgFolder">');
      Add('<xsl:text>ImgFolder=</xsl:text>');
      Add('<xsl:value-of select="@Url" />');
      Add('<xsl:text>&#xA;</xsl:text>');

      Add('<xsl:apply-templates select="Btn" />');
    Add('</xsl:template>');

    Add('<xsl:template match="Btn">');
      Add('<xsl:text>Data=</xsl:text>');
      Add('<xsl:value-of select="@Data" />');
      Add('<xsl:text>&#xA;</xsl:text>');

      Add('<xsl:text>Img=</xsl:text>');
      Add('<xsl:value-of select="@Img" />');
      Add('<xsl:text>&#xA;</xsl:text>');

      Add('<xsl:text>Text=</xsl:text>');
      Add('<xsl:value-of select="@Text" />');
      Add('<xsl:text>&#xA;</xsl:text>');
    Add('</xsl:template>');

    Add('</xsl:stylesheet>');
  end;
end;

end.
