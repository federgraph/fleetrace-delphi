unit RiggVar.Calc.EventProxy05;

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
  TCalcEventProxy5 = class(TCalcEventProxy4)
  protected
    function ScoreRegattaRemote(xml: string): string; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TCalcEventProxy5 }

function TCalcEventProxy5.ScoreRegattaRemote(xml: string): string;
begin
  try
    //ScoringService.ScoringServerName := 'http://thinkpad/FR42/';
    ScoringService.ScoringServerName := Main.IniImage.WebApplicationUrl;
    ScoringService.InitScoringServiceSoap(ScoringService.ServerType_ASPNET);
    result := ScoringService.GetScoringServiceSoap.ScoreRegatta(xml);
    if Main.IniImage.LogProxyXML then
      BO.UndoManager.ProxyXML.Text := result;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy5.ScoreRegattaRemote';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

end.
