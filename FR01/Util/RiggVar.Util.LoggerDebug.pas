unit RiggVar.Util.LoggerDebug;

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
  Winapi.Windows,
  RiggVar.Util.Logger;

type
  TDebugLogger = class(TLogger)
  public
    procedure Log(s: string);
  end;

implementation

procedure TDebugLogger.Log(s: string);
begin
  OutputDebugString(PChar(s));
end;

end.

