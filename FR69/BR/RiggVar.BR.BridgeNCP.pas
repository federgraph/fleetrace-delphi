unit RiggVar.BR.BridgeNCP;

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

{ a Bridge NCP (Network Connection Point) used in a ServerBridge

  * see unit RiggVar.BR.BridgeServer

  * the Container (ServerBridge) contains a
      BridgeProt: TBridgeProt; (has Data/LocalBridge and implements Protocol)
      BridgeNCP: TBridgeServer; <-- this one (for connectivity)
      BridgeWeb: TBridgeWeb; (for web access, references a LocalBridge)

  * the reference to BridgeProt in BridgeNCP (here) is the same instance
     as in the containing class
}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.BR.BridgeLocal,
  RiggVar.BR.BridgeProt,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.Conn.Sockets;

type
  TBridgeNCP = class(TAdapterBaseNCP)
  private
    FBridgeProt: TBridgeProt;
    procedure SetBridgeProt(const Value: TBridgeProt);
  public
    constructor Create(aServer: TBaseServer);
    destructor Destroy; override;
    procedure HandleMsg(cm: TContextMsg); override;
    procedure Broadcast(Sender: TObject; s: string);
    property BridgeProt: TBridgeProt read FBridgeProt write SetBridgeProt;
  end;

implementation

{ TBridgeNCP }

constructor TBridgeNCP.Create(aServer: TBaseServer);
begin
  inherited Create(aServer);
end;

destructor TBridgeNCP.Destroy;
begin
  Server.Free;
  Server := nil;
  inherited;
end;

procedure TBridgeNCP.HandleMsg(cm: TContextMsg);
var
  answer: string;
begin
  if BridgeProt <> nil then
  begin
    answer := BridgeProt.Calc(cm.Sender, cm.msg);
    if (answer <> '') then
      Server.Reply(cm.Sender, answer);
  end;
end;

procedure TBridgeNCP.Broadcast(Sender: TObject; s: string);
begin
  Server.Broadcast(Sender, s);
end;

procedure TBridgeNCP.SetBridgeProt(const Value: TBridgeProt);
begin
  FBridgeProt := Value;
end;

end.
