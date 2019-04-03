unit RiggVar.BO.SDI;

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

{
  TSDIBO works with BOContainer to make sure the BO (itself)
  is properly recreated with new params.
}

uses
  RiggVar.BO.Def;

type
  TSDIBO = class(TBO)
  public
    procedure LoadNew(const Data: string); override;
    procedure Clear; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TSDIBO }

procedure TSDIBO.Clear;
begin
  Self.StammdatenNode.StammdatenRowCollection.Clear;
  inherited Clear;
end;

procedure TSDIBO.LoadNew(const Data: string);
begin
  Main.LoadNew(Data);
end;

end.
