unit RiggVar.BR.BridgeClient;

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

{  adds a ClientSocket to AsynchronBridge (transport)

   * base class for ProxyBridge
     TBridge <- TAsynchronBridge <- ClientBridge <- ProxyBridge <- CombiBridge

}

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  RiggVar.BR.PeerController,
  RiggVar.BR.BridgeController,
  RiggVar.BR.BridgeAsynchron,
  RiggVar.BR.BridgeAbstract,
  RiggVar.Conn.ResultClient;

type
  TClientBridge = class(TAsynchronBridge)
  protected
    PluginScheduled: Boolean;
    LED_Brush_Color: TColor;
    procedure CreateClient;
    procedure Cleanup;
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure HandleConnectedChanged(Sender: TObject);
    procedure PaintLED;
    //
    procedure Send(s: string); override;
  public
    ResultClient: TResultClient;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function Plugin(): Integer; override;
    procedure Plugout(SwitchID: Integer); override;
    //
    function IsEnabled(Op: TSwitchOp): Boolean; override;
    procedure GetStatusReport(Memo: TStrings); override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TClientBridge }

constructor TClientBridge.Create;
begin
  inherited Create;
  CreateClient;
  LED_Brush_Color := clRed;
end;

destructor TClientBridge.Destroy;
begin
  Cleanup;
  inherited Destroy;
end;

function TClientBridge.Plugin: Integer;
begin
  ConnectBtnClick(nil);
  if ResultClient.Connected then
  begin
    PluginScheduled := False;
    result := inherited Plugin;
  end
  else
  begin
    PluginScheduled := True;
    result := 0;
  end;
end;

procedure TClientBridge.Plugout(SwitchID: Integer);
begin
  inherited Plugout(SwitchID);
  DisconnectBtnClick(nil);
end;

procedure TClientBridge.CreateClient;
begin
  if Assigned(ResultClient) then
    ResultClient.Free;
  ResultClient := nil;

  ResultClient := TResultClient.Create(Main.IniImage.BridgeHost, Main.IniImage.BridgePort);
  ResultClient.Client.OnHandleMsg := SetOutputMsg;
  ResultClient.Client.OnConnectedChanged := HandleConnectedChanged;
end;

procedure TClientBridge.Cleanup;
begin
  ResultClient.Free;
end;

procedure TClientBridge.ConnectBtnClick(Sender: TObject);
begin
  if (Main.IniImage.BridgeHost = ResultClient.Client.Host)
    and (Main.IniImage.BridgePort = ResultClient.Client.Port)then
    ResultClient.Connect
  else
  begin
    CreateClient;
    ResultClient.Connect;
  end;
  LED_Brush_Color := clYellow;
end;

procedure TClientBridge.HandleConnectedChanged(Sender: TObject);
begin
  if ResultClient.Connected then
  begin
    if PluginScheduled then
    begin
      PluginScheduled := False;
      inherited Plugin;
    end;
  end
  else
  begin
    FSwitchID := 0;
    BridgeController.DoOnPlugout;
  end;
  PaintLED;
end;

function TClientBridge.IsEnabled(Op: TSwitchOp): Boolean;
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

procedure TClientBridge.DisconnectBtnClick(Sender: TObject);
begin
  ResultClient.Disconnect;
  PaintLED;
end;

procedure TClientBridge.PaintLED;
begin
  if ResultClient.Connected then
    LED_Brush_Color := clLime
  else
    LED_Brush_Color := clRed;
end;

procedure TClientBridge.Send(s: string);
begin
  if ResultClient.Connected then
  begin
    Inc(MsgCounterOut);
    ResultClient.Client.SendMsg(s);
  end;
end;

procedure TClientBridge.GetStatusReport(Memo: TStrings);
begin
  Memo.Add('Host: ' + ResultClient.Client.Host);
  Memo.Add('Port: ' + IntToStr(ResultClient.Client.Port));
  Memo.Add('In: ' + IntToStr(MsgCounterIn));
  Memo.Add('Out: ' + IntToStr(MsgCounterOut));
end;

end.
