unit RiggVar.BR.BridgeProxy;

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

{ TBridge <- TAsynchronBridge <- ClientBridge <- ProxyBridge <- CombiBridge

  * Proxy and CombiBridge add transport for retrieving reports to ClientBridge
    recognizes a Request msg in DispatchMsg
    and implements the SendAnswer method

  * ProxyBridge disables Upload and Download
  * CombiBridge does not disable Upload and Download

  * target id is bundled in the answer msg
  * report is calculated, not taken from cache
}

uses
  System.SysUtils,
  RiggVar.BR.PeerController,
  RiggVar.BR.BridgeAbstract,
  RiggVar.BR.BridgeClient,
  Graphics;

type
  TProxyBridge = class(TClientBridge)
  protected
    procedure DispatchMsg(s: string); override;
  public
    function IsEnabled(Op: TSwitchOp): Boolean; override;
    procedure SendAnswer(Target: Integer; Answer: string); override;
  end;

  TCombiBridge = class(TProxyBridge)
  public
    function IsEnabled(Op: TSwitchOp): Boolean; override;
  end;

implementation

{ TProxyBridge }

function TProxyBridge.IsEnabled(Op: TSwitchOp): Boolean;
begin
  case Op of
    SwitchOp_Upload:
      result := false;
    SwitchOp_Download:
      result := false;
    else
      result := inherited IsEnabled(Op);
  end;
end;

procedure TProxyBridge.DispatchMsg(s: string);
var
  Target: Integer;
begin
  if s = 'Request' then
  begin
    Target := PopInt;
    BridgeController.DoOnRequest(Target, SL.Text);
  end
  else
    inherited DispatchMsg(s);
end;

procedure TProxyBridge.SendAnswer(Target: Integer; Answer: string);
begin
  SL.Text := Answer;
  SL.Insert(0, IntToStr(Target));
  SL.Insert(0,'Response');
  Send(SL.Text);
end;

{ TCombiBridge }

function TCombiBridge.IsEnabled(Op: TSwitchOp): Boolean;
begin
  case Op of
    SwitchOp_Plugin:
      result := LED_Brush_Color <> clLime; //not ResultClient.Connected;
    SwitchOp_Plugout:
      result := LED_Brush_Color = clLime; //ResultClient.Connected;
    SwitchOp_Synchronize:
      result := false;
    SwitchOp_Upload:
      result := LED_Brush_Color = clLime; //ResultClient.Connected;
    SwitchOp_Download:
      result := LED_Brush_Color = clLime; //ResultClient.Connected;
    else
      result := false;
  end;
end;

end.
