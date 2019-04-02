unit RiggVar.EM.Intf;

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
  IEventMenu = class
    function ComboCaption: string; virtual; abstract;
    function Count: Integer; virtual; abstract;
    function GetCaption(i: Integer): string; virtual; abstract;
    function GetImageUrl(i: Integer): string; virtual; abstract;
    function GetDataUrl(i: Integer): string; virtual; abstract;
    function IsMock: Boolean; virtual; abstract;
  end;

implementation

end.
