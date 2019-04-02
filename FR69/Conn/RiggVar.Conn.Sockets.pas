unit RiggVar.Conn.Sockets;

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
  Vcl.Dialogs,
  System.Win.ScktComp,
  RiggVar.Conn.Def,
  RiggVar.Util.PortTestCall,
  RiggVar.BO.Watches;

type
  TAdapterServer = class(TBaseServer)
  private
    ServerSocket: TServerSocket;
    procedure ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  public
    constructor Create(aPort: Integer; aServerFunction: TServerFunction); override;
    destructor Destroy; override;
    function Port: Integer; override;
    procedure TopicBroadcast(SenderList: TList; s: string); override;
    procedure Broadcast(Sender: TObject; s: string); override;
    procedure SendMsg(cm: TContextMsg); override;
    procedure Reply(Connection: TObject; s: string); override;
    function ConnectionCount: Integer; override;
    function IsDummy: Boolean; override;
  end;

  TAdapterBaseNCP = class
  protected
    Counter: Integer;
    MaxInputLen: Integer;
    LastSender: TObject;

    BufferString: string;
    BufferList: TStringList;
    BufferIndex: Integer;
    procedure GetBuffer(Sender: TObject);
    function GetSenderIndex(Sender: TObject): Integer;
    procedure DeleteBuffer(Sender: TObject);
  public
    Server: TBaseServer;
    constructor Create(aServer: TBaseServer);
    destructor Destroy; override;
    procedure HandleMsg(cm: TContextMsg); virtual; abstract;
    procedure InjectMsg(Sender: TObject; ms: TMsgSource; s: string); virtual;
  end;

  TAdapterInputNCP = class(TAdapterBaseNCP)
    procedure HandleMsg(cm: TContextMsg); override;
  end;

  TAdapterOutputNCP = class(TAdapterBaseNCP)
    procedure HandleMsg(cm: TContextMsg); override;
  end;

function CreateServer(aPort: Integer; aServerFunction: TServerFunction): TBaseServer;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Conn.Utils,
  RiggVar.Util.Sound;

{ TAdapterServer }

function TAdapterServer.ConnectionCount: Integer;
begin
  if Assigned(ServerSocket) then
    result := ServerSocket.Socket.ActiveConnections
  else
    result := 0;
end;

constructor TAdapterServer.Create(aPort: Integer; aServerFunction: TServerFunction);
begin
  inherited;
  MsgSource := msTCP;
  ServerSocket := TServerSocket.Create(nil);
  ServerSocket.Port := aPort;
  ServerSocket.OnClientRead := ServerSocketClientRead;
  ServerSocket.Socket.OnClientError := ServerSocketError;
  ServerSocket.Active := True;
  Active := True;
end;

destructor TAdapterServer.Destroy;
begin
  ServerSocket.Active := False;
  ServerSocket.Close;
  ServerSocket.Free;
  inherited;
end;

function TAdapterServer.IsDummy: Boolean;
begin
  result := false;
end;

function TAdapterServer.Port: Integer;
begin
  result := ServerSocket.Port;
end;

procedure TAdapterServer.Reply(Connection: TObject; s: string);
var
  i: Integer;
  so: TCustomWinSocket;
begin
  for i := 0 to ServerSocket.Socket.ActiveConnections-1 do
  begin
    so := ServerSocket.Socket.Connections[i];
    if so = Connection then
    begin
      if s = '' then
      begin

//        s := IntToStr(Self.ServerSocket.Socket.ActiveConnections) + ' Connection';
//        if Self.ServerSocket.Socket.ActiveConnections > 1 then
//          s := s + 's';

        s := 'empty';

      end;
      //so.SendText(Char(#2) + s + Char(#3));
      RiggVar.Conn.Utils.SendMsg(so, s);
    end;
  end;
end;

procedure TAdapterServer.Broadcast(Sender: TObject; s: string);
var
  i: Integer;
begin
  for i := 0 to ServerSocket.Socket.ActiveConnections-1 do
  begin
    if ServerSocket.Socket.Connections[i] <> Sender then
    begin
      //ServerSocket.Socket.Connections[i].SendText(Char(#2) + s + Char(#3));
      RiggVar.Conn.Utils.SendMsg(ServerSocket.Socket.Connections[i], s);
    end;
  end;
end;

procedure TAdapterServer.TopicBroadcast(SenderList: TList; s: string);
var
  i, j: Integer;
begin
  if SenderList <> nil then
    for i := 0 to ServerSocket.Socket.ActiveConnections-1 do
    begin
      j := SenderList.IndexOf(ServerSocket.Socket.Connections[i]);
      if j > -1 then
      begin
        //ServerSocket.Socket.Connections[i].SendText(Char(#2) + s + Char(#3));
        RiggVar.Conn.Utils.SendMsg(ServerSocket.Socket.Connections[i], s);
      end;
    end;
end;

procedure TAdapterServer.SendMsg(cm: TContextMsg);
var
  i: Integer;
begin
  for i := 0 to ServerSocket.Socket.ActiveConnections-1 do
  begin
    //ServerSocket.Socket.Connections[i].SendText(Char(#2) + s + Char(#3));
    //RiggVar.Conn.Utils.SendMsg(ServerSocket.Socket.Connections[i], cm.msg);
    RiggVar.Conn.Utils.SendMsg(ServerSocket.Socket.Connections[i], cm.EncodedMsg);
  end;
end;

procedure TAdapterServer.ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  s: string;
begin
  s := RiggVar.Conn.Utils.ReceiveText(Sender, Socket);
  InjectMsg(Socket, s);
end;

procedure TAdapterServer.ServerSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  Main.Logger.Error('ServerSocketError (' + IntToStr(ErrorCode) + ')');
  ErrorCode := 0;
end;

function CreateServer(aPort: Integer; aServerFunction: TServerFunction): TBaseServer;
var
  ServerType: Integer;
begin
  if Main.Params.SkipTestForListeningSocket then
  begin
    try
      result := TAdapterServer.Create(aPort, aServerFunction);
    except
      result := TBaseServer.Create(aPort, aServerFunction);
    end;
  end

  else
  begin
    ServerType := 1;
    //wenn schon ein Socket auf diesem Port hört, dann DummySocket öffnen
    if (ServerType <> 0) and TestConnection('127.0.0.1', aPort, 200) then
    begin
      ServerType := 0;
      ShowMessage('Port ' + IntToStr(aPort) + ' already open!');
    end;

    try
      case ServerType of
        0: result := TBaseServer.Create(aPort, aServerFunction);
        1: result := TAdapterServer.Create(aPort, aServerFunction);
      else
        result := nil;
      end;
    except
      //in case TestConnection returned wrong Information,
      //can happen if Registry-Value "EnablePortCheck" does prevent the test
      //or because a port scan is not allowed.
      result := TBaseServer.Create(aPort, aServerFunction);
    end;
  end;

end;

{ TAdapterBaseNCP }

constructor TAdapterBaseNCP.Create(aServer: TBaseServer);
begin
  inherited Create;
  Server := aServer;
  BufferList := TStringList.Create;
  LastSender := nil;
  MaxInputLen := 300 * 1024;
  Server.OnInjectMsg := InjectMsg;
  Server.OnHandleMsg := HandleMsg;
end;

destructor TAdapterBaseNCP.Destroy;
begin
  Server.Free;
  BufferList.Free;
  inherited;
end;

procedure TAdapterBaseNCP.DeleteBuffer(Sender: TObject);
var
  i: Integer;
begin
  i := GetSenderIndex(Sender);
  if i <> -1 then
    BufferList.Delete(i);
  LastSender := nil;
end;

procedure TAdapterBaseNCP.GetBuffer(Sender: TObject);
begin
  if Sender = LastSender then
    exit;

  //write back
  If (BufferIndex >= 0) and (BufferIndex <= BufferList.Count-1) then
    BufferList[BufferIndex] := BufferString;

  BufferIndex := GetSenderIndex(Sender);
  if BufferIndex = -1 then
    BufferIndex := BufferList.AddObject('', Sender);

  //read out
  BufferString := BufferList[BufferIndex];

  //remember Sender
  LastSender := Sender;
end;

function TAdapterBaseNCP.GetSenderIndex(Sender: TObject): Integer;
var
  i: Integer;
begin
  result := -1;

  if Sender = nil then exit;

  for i := 0 to BufferList.Count- 1 do
  begin
    if (BufferList.Objects[i] = Sender) then
    begin
      result := i;
      break;
    end
  end;
end;

procedure TAdapterBaseNCP.InjectMsg(Sender: TObject; ms: TMsgSource; s: string);
var
  i: Word;
  ch: char;
  cm: TContextMsg;
begin
  Inc(Counter);
  GetBuffer(Sender);
  for i := 1 to Length(s) do
  begin
    ch := s[i];
    if ch = #2 then
      BufferString := ''
    else if ch = #3 then
    begin
      cm := TContextMsg.Create;
      cm.MsgSource := ms;
      cm.Sender := Sender;
      cm.IsAdapterMsg := true;
      cm.msg := BufferString;
      cm.DecodeHeader;
      HandleMsg(cm);
      BufferString := '';
      if not cm.IsQueued then
        cm.Free;
    end
    else
      BufferString := BufferString + ch;
  end;

  if BufferString = '' then
    DeleteBuffer(Sender)
  else if Length(BufferString) > MaxInputLen then
  begin
    //detected malicious input: too long
    DeleteBuffer(Sender);
  end;
end;

{ TAdapterInputNCP }

procedure TAdapterInputNCP.HandleMsg(cm: TContextMsg);
begin
  if cm.MsgType = 'W' then
  begin
    Server.Reply(cm.Sender, Main.GuiManager.WebReceiver.Receive(cm.msg));
    Exit;
  end;

  if cm.msg = 'switch connect' then
  begin
    SwitchSender := cm.Sender;
    cm.MsgSource := msSwitch;
    Exit;
  end
  else if cm.msg = 'switch disconnect' then
  begin
    SwitchSender := nil;
    cm.MsgSource := msSwitch;
    Exit;
  end;
  if (cm.Sender = SwitchSender) and (SwitchSender <> nil) then
    cm.MsgSource := msSwitch;

  if Assigned(Main.AdapterBO.AdapterInputConnection) then
  begin
    //auf mehrzeilige msg (die einen Request enthält) antworten
    if Pos('.Request.', cm.msg) > 0 then
    begin
      //BO.InputServer.HandleMsg(cm); //functioniert nicht

      //Variante 1 mit neuer Instanz TContextMsg
      //cm.Answer := Main.AdapterBO.AdapterInputConnection.HandleMsg(cm.msg);
      //Server.Reply(cm.Sender, cm.Answer);

      //Variante 2 verwendet cm direkt, reply in OnIdle/ProcessQueue
      Main.AdapterBO.AdapterInputConnection.HandleContextMsg(cm);
    end
    else
    begin
      GlobalWatches.MsgIn := cm.msg;
      //lock the adapter-output
      SwitchLocked := True; //for generated messages (Redo)
      BO.InputServer.HandleMsg(cm);
      //Main.AdapterBO.AdapterInputConnection.HandleContextMsg(cm);
      Main.GuiManager.PlaySound(Sound_Click01);
    end;
  end;
end;

{ TAdapterOutputNCP }

procedure TAdapterOutputNCP.HandleMsg(cm: TContextMsg);
begin
  //eventuell hier den allgemeinen Token durch den speziellen ersetzen
  //direkt antworten auf Anfragen am Ausgang (ohne Berechnung)
  Assert(cm.Sender = LastSender);
  Server.Reply(cm.Sender, BO.Output.GetMsg(cm.msg));
end;

end.
