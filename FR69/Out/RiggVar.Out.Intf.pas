unit RiggVar.Out.Intf;

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
  IOutput = interface
    function GetMsg(sRequest: string): string;
    function GetAll(OutputRequestList: TStringList): string;
  end;

implementation

end.
