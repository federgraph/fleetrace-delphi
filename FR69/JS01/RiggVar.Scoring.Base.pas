unit RiggVar.Scoring.Base;

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

type
  TScoringModelType = (
  ScoringModel_LowPoint,
  ScoringModel_BonusPoint,
  ScoringModel_BonusPointDSV
  );

  IScoringManager = class
  private
    FScoringModelType: TScoringModelType;
    procedure SetScoringModelType(const Value: TScoringModelType);
  public
    procedure ScoreRegatta(ev: TObject); virtual; abstract;
    property ScoringModelType: TScoringModelType read FScoringModelType write SetScoringModelType;
  end;

implementation

{ IScoringManager }

procedure IScoringManager.SetScoringModelType(
  const Value: TScoringModelType);
begin
  FScoringModelType := Value;
end;

end.

