unit RiggVar.BO.BridgeAdapter;

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
  System.SysUtils,
  System.Classes,
  RiggVar.BR.BridgeAbstract;

type
  TBridgeAdapterBase = class
  public
    function GetBridgeProxy(ProxyType: Integer): TBridge; virtual;
  end;

implementation

function TBridgeAdapterBase.GetBridgeProxy(ProxyType: Integer): TBridge;
begin
  result := nil;
end;

end.
