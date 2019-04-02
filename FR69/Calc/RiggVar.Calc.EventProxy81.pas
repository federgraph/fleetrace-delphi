unit RiggVar.Calc.EventProxy81;

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
  SysUtils,
  ScoringService,
  RiggVar.Calc.EV,
  RiggVar.Calc.EventProxy04,
  RiggVar.BO.IniImage,
  RiggVar.App.Config,
  RiggVar.BO.Def;

type
  { Scoring via WebService using Xml }
  TCalcEventProxyFR81 = class(TCalcEventProxy4)
  protected
    function ScoreRegattaRemote(xml: string): string; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TCalcEventProxyFR81 }

function TCalcEventProxyFR81.ScoreRegattaRemote(xml: string): string;
begin
  try
    //WebApplicationUrl := 'http://vmxphome:mshome.net:8080/';
    ScoringService.ScoringServerName := Main.IniImage.WebApplicationUrl;
    //result := ScoringService.GetScoringServiceSoap.HelloWorld();
    ScoringService.InitScoringServiceSoap(ScoringService.ServerType_Java);
    result := ScoringService.GetScoringServiceSoap.ScoreRegatta(xml);
    if Main.IniImage.LogProxyXML then
      BO.UndoManager.ProxyXML.Text := result;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxyFR81.ScoreRegattaRemote';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

end.
