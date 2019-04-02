unit RiggVar.BR.BridgeAdapter;

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
  RiggVar.BO.BridgeAdapter,
  RiggVar.BR.BridgeAbstract;

type
  TBridgeAdapter = class(TBridgeAdapterBase)
  public
    function GetBridgeProxy(ProxyType: Integer): TBridge; override;
  end;

implementation

uses
  BridgeServiceFR,
  BridgeServiceSKK,
  FR88_FRService,
  FR88_SKKService;

function TBridgeAdapter.GetBridgeProxy(ProxyType: Integer): TBridge;
begin
  case ProxyType of
    1: result := TFRBridgeProxy.Create;
    2: result := TFR88BridgeProxyFR.Create;
    else
      result := nil;
  end;
end;

end.
