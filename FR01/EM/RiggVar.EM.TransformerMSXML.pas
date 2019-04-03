unit RiggVar.EM.TransformerMSXML;

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
  RiggVar.EM.Transformer,
  MSXML;

type
  TXslTransformer = class(TEventDataTransformer)
  private
    function DocFromString(const s: WideString): IXMLDOMDocument;
  public
    procedure TransformSL(SLXML, SLXSL, SLTXT: TStrings); override;
  end;

implementation

{ TXsltTransformer }

function TXslTransformer.DocFromString(const s: WideString): IXMLDOMDocument;
begin
  result := CoDomDocument.Create();
  result.async := false;
  result.loadXML(s);
end;

procedure TXslTransformer.TransformSL(SLXML, SLXSL, SLTXT: TStrings);
var
  doc: IXMLDOMDocument;
  xsl: IXMLDOMDocument;
begin
  FOK := false;
  if (SLXML.Count > 0) and (SLXSL.Count > 0) then
  try
    doc := DocFromString(SLXML.Text);
    xsl := DocFromString(SLXSL.Text);
    //SLTXT.Text := Utf8ToAnsi(doc.transformNode(xsl));
    SLTXT.Text := doc.transformNode(xsl);
    FOK := true;
  except
    on E: Exception do
    begin
      FOK := false;
      FError := E.Message
    end;
  end
  else
  begin
    FOK := false;
    FError := 'SL is empty';
  end;
end;

end.

