unit RiggVar.EM.Connection;

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
  System.SysUtils;

type
  EEventMenuException = class(Exception);

  TEventMenuConnection = class
  public
    Url: string;
    function Get: string; virtual;
    procedure Post(const s: string); virtual;
  end;

implementation

{ TEventMenuConnection }

function TEventMenuConnection.Get: string;
begin
  result := '';
end;

procedure TEventMenuConnection.Post(const s: string);
begin
  //not implemented
end;

end.
