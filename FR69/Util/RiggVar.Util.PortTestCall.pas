unit RiggVar.Util.PortTestCall;

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

//{$DEFINE PortTest_DLL_IMPLICIT_LOADING}
{$DEFINE PortTest_DLL_EXPLICIT_LOADING}

{$IFDEF PortTest_DLL_IMPLICIT_LOADING}
  function TestConnection(Host: string; Port, Timeout: Integer): Boolean;
  external 'PT01.dll';
{$ELSE}
  function TestConnection(Host: string; Port, Timeout: Integer): Boolean;
{$ENDIF}

implementation

{$IFDEF PortTest_DLL_EXPLICIT_LOADING}
uses
  RiggVar.App.Main;

function TestConnection(Host: string; Port, Timeout: Integer): Boolean;
begin
  result := false; //assume port is free
  if Assigned(Main.PortTester) then
    result := Main.PortTester.TestConnection(Host, Port, Timeout);
end;

{$ENDIF}



end.
