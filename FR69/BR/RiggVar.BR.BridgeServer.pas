unit RiggVar.BR.BridgeServer;

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
  RiggVar.BR.BridgeLocal,
  RiggVar.BR.BridgeProt,
  RiggVar.BR.BridgeNCP,
  RiggVar.BR.BridgeWeb,
  RiggVar.Conn.Def,
  RiggVar.Conn.Sockets,
  RiggVar.BR.PeerController;

type
  TServerBridge = class(TLocalBridge)
  private
  protected
    StartupTime: TDateTime;
  public
    SL: TStringList; //wie bei AsynchronBridge für ausgehende msg
    BridgeProt: TBridgeProt;
    BridgeNCP: TBridgeNCP;
    BridgeWeb: TBridgeWeb;
    constructor Create(Capacity: Integer);
    destructor Destroy; override;
    function GetWebStatusString: string; override;
    procedure GetStatusReport(Memo: TStrings); override;
    procedure SendContextMsg(SwitchID: Integer; cm: TContextMsg); override;
    function IsSameBridge: Boolean; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TServerBridge }

constructor TServerBridge.Create(Capacity: Integer);
var
  ts: TBaseServer;
begin
  inherited Create(Capacity);
  Startuptime := Now;
  SL  := TStringList.Create;

  BridgeProt := TBridgeProt.Create;
  BridgeProt.LocalBridge := self;

  try
    ts := CreateServer(Main.IniImage.BridgePort, Function_Bridge);
    BridgeNCP := TBridgeNCP.Create(ts);
    BridgeNCP.BridgeProt := BridgeProt;
  except
    BridgeNCP.Free;
    BridgeNCP := nil;
  end;

  BridgeWeb := TBridgeWeb.Create;
  BridgeWeb.LocalBridge := self;

  BridgeProt.OnBroadcast := BridgeNCP.Broadcast;
end;

destructor TServerBridge.Destroy;
begin
  BridgeWeb.Free;
  BridgeProt.Free;
  BridgeNCP.Free;
  SL.Free;
  inherited;
end;

function TServerBridge.GetWebStatusString: string;
var
  i: Integer;
begin
  result := '';
  SLReport.Clear;
  try
    SLReport.Add('StartupTime: ' + DateTimeToStr(StartupTime));
    SLReport.Add('DateTimeToStr(Now): ' + DateTimeToStr(Now));
    SLReport.Add('BridgeWeb.HomeUrl: ' + BridgeWeb.Url);
    i := BridgeNCP.Server.ConnectionCount;
    SLReport.Add('Connections: ' + IntToStr(i));
    result := SLReport.Text;
  except
  end;
end;

function TServerBridge.IsSameBridge: Boolean;
begin
  result := inherited IsSameBridge;

  //parameter BridgeHost is not checked because
  //the Delphi TServerSocket opens BridgePort on ip_address_any

  //return true if Port matches
  //this will prevent error: Port already open
  if Assigned(BridgeNCP.Server) then
  begin
    if BridgeNCP.Server.Port = Main.IniImage.BridgePort then
      result := true;
  end;
end;

procedure TServerBridge.GetStatusReport(Memo: TStrings);
var
  i: Integer;
begin
  inherited;
  Memo.Add('-- TServerBridge.StatusReport');
  Memo.Add('StartupTime: ' + DateTimeToStr(StartupTime));
  Memo.Add('DateTimeToStr(Now): ' + DateTimeToStr(Now));
  Memo.Add('BridgeWeb.HomeUrl: ' + BridgeWeb.Url);
  i := BridgeNCP.Server.ConnectionCount;
  Memo.Add('Connections: ' + IntToStr(i));
end;

procedure TServerBridge.SendContextMsg(SwitchID: Integer; cm: TContextMsg);
begin
  //incoming and outgoing messages are added to the msg-ring
  //this step is direction-agnostic
  //it is acomplished by calling the inherited SendMsg method
  SendMsg(SwitchID, cm.msg);

  //in addition, the embedded server-bridge must also
  //- process incoming messages and broadcast them via normal output
  //- broadcast outgoing messages to conneced bridge-clients
  if cm.IsIncomingMsg then
  begin
    //process cm.msg within the 'application'
    //and broadcast to normal output clients connected to OutputServer
    if cm.MsgSource <> msBridge then
      cm.MsgSource := msBridge;
    Main.InjectServerBridgeMsg(cm);
  end
  else if cm.IsOutgoingMsg then
  begin
    //broadcast to client bridges via BridgeServer
    SL.Clear;
    SL.Add('GetNewMessages');
    SL.Add(cm.msg);
    BridgeNCP.Broadcast(nil, SL.Text);
    DoOnUpdateUI(UUIAction_Msg, cm.msg);
    SL.Clear;
  end;
end;

end.
