unit RiggVar.Web2.Intf;

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
  System.Classes,
  IdContext,
  IdCustomHTTPServer;

type
  IWebController = interface
    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function IsOnline: Boolean;
    procedure SetTemp(value: string);
    function GetVirtualDir: string;
  end;

implementation

end.
