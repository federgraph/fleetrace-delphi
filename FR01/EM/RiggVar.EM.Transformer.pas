unit RiggVar.EM.Transformer;

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
  TEventDataTransformer = class
  protected
    FOK: Boolean;
    FError: string;
  public
    procedure Reset;
    procedure TransformSL(SLXML, SLXSL, SLTXT: TStrings); virtual; abstract;

    property OK: Boolean read FOK;
    property Error: string read FError;
  end;

implementation

{ TEventDataTransformer }

procedure TEventDataTransformer.Reset;
begin
  FOK := false;
  FError := '';
end;

end.

