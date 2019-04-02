unit RiggVar.Calc.EventProxyCall;

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

//{$DEFINE SCORING_DLL_IMPLICIT_LOADING}
{$DEFINE SCORING_DLL_EXPLICIT_LOADING}

uses
  RiggVar.Calc.EventProxy00;

{$IFDEF SCORING_DLL_IMPLICIT_LOADING}
  procedure ScoreRegatta(p: TFRProxy); external 'JS01.dll';
{$ELSE}
  procedure ScoreRegatta(p: TFRProxy);
{$ENDIF}

implementation

{$IFDEF SCORING_DLL_EXPLICIT_LOADING}
uses
  RiggVar.App.Main;

procedure ScoreRegatta(p: TFRProxy);
begin
  if Assigned(Main.ScoringModule) then
  begin
    Main.ScoringModule.ScoreRegatta(p);
  end;
end;

{$ENDIF}

end.
