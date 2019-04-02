unit RiggVar.Conn.ResultClient;

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
  System.Win.ScktComp;

type
  THandleMsgEvent = procedure(Sender: TObject; s: string) of object;

  TClient = class
  private
    FBuffer: string;
    FOnHandleMsg: THandleMsgEvent;
    FOnConnectedChanged: TNotifyEvent;
  protected
    ClientSocket: TClientSocket;
    procedure ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure HandleMsg(s: string);
    procedure ClientSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  public
    constructor Create(AHost: string; APort: Integer);
    destructor Destroy; override;
    function Host: string;
    function Port: Integer;
    function SendMsg(s: string): Boolean;
    property OnHandleMsg: THandleMsgEvent read FOnHandleMsg write FOnHandleMsg;
    property OnConnectedChanged: TNotifyEvent read FOnConnectedChanged write FOnConnectedChanged;
  end;

  TResultClient = class
  private
    procedure SetMsg(Sender: TObject; s: string);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  public
    Client: TClient;
    constructor Create(AHost: string; APort: Integer);
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure NewClient(AHost: string; APort: Integer);
    property Connected: Boolean read GetConnected write SetConnected;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Conn.Utils;

{ TClient }

constructor TClient.Create(AHost: string; APort: Integer);
begin
  ClientSocket := TClientSocket.Create(nil);
  ClientSocket.Host := AHost;
  ClientSocket.Port := APort;
  ClientSocket.OnRead := ClientSocketRead;
  ClientSocket.OnError := ClientSocketError;
  ClientSocket.OnConnect := ClientSocketConnect;
  ClientSocket.OnDisconnect := ClientSocketDisconnect;
end;

destructor TClient.Destroy;
begin
  Clientsocket.Active := False;
  ClientSocket.Close;
  ClientSocket.Free;
  inherited;
end;

procedure TClient.ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if Assigned(OnConnectedChanged) then
    OnConnectedChanged(Self);
end;

procedure TClient.ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if Assigned(OnConnectedChanged) then
    OnConnectedChanged(Self);
end;

procedure TClient.ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
var
  s: string;
  c: char;
  i: Integer;
begin
  s := RiggVar.Conn.Utils.ReceiveText(Sender, Socket);
  for i := 1 to Length(s) do
  begin
    c := s[i];
    if c = #2 then
      FBuffer := ''
    else if c = #3 then
    begin
      HandleMsg(FBuffer);
      FBuffer := '';
    end
    else
      FBuffer := FBuffer + c;
  end;
end;

procedure TClient.HandleMsg(s: string);
begin
  if Assigned(OnHandleMsg) then
    OnHandleMsg(Self, s);
end;

function TClient.SendMsg(s: string): Boolean;
begin
  result := RiggVar.Conn.Utils.SendMsg(ClientSocket.Socket, s);
end;

procedure TClient.ClientSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  Main.Logger.Error('Error connecting to ' + ClientSocket.Host);
  ErrorCode := 0;
end;

function TClient.Host: string;
begin
  result := ClientSocket.Host;
end;

function TClient.Port: Integer;
begin
  result := ClientSocket.Port;
end;

{ TResultClient }

constructor TResultClient.Create(AHost: string; APort: Integer);
begin
  inherited Create;
  Client := TClient.Create(AHost, APort);
  Client.OnHandleMsg := SetMsg;
end;

destructor TResultClient.Destroy;
begin
  Client.Free;
  inherited;
end;

procedure TResultClient.Connect;
begin
  Client.ClientSocket.Active := True;
end;

procedure TResultClient.Disconnect;
begin
  if Client.ClientSocket.Active then
    Client.ClientSocket.Active := False;
end;

function TResultClient.GetConnected: Boolean;
begin
  result := False;
  if Assigned(Client) then
    result := Client.ClientSocket.Active;
end;

procedure TResultClient.NewClient(AHost: string; APort: Integer);
var
  ne: TNotifyEvent;
  hme: THandleMsgEvent;
begin
  if not Connected then
  begin
    ne := Client.OnConnectedChanged;
    hme := Client.OnHandleMsg;
    Client.Free;
    Client := TClient.Create(AHost, APort);
    Client.OnHandleMsg := SetMsg;
    Client.OnConnectedChanged := ne;
    Client.OnHandleMsg := hme;
  end;
end;

procedure TResultClient.SetConnected(const Value: Boolean);
begin
  Connect;
end;

procedure TResultClient.SetMsg(Sender: TObject; s: string);
begin
  {
  Default do nothing Eventhandler,
  dort wo die Instanz von TResultClient erzeugt wird
  wird normalerweise der spezielle Handler angehangen.
  }
end;

end.
