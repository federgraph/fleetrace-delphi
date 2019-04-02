unit RiggVar.Util.PolicyServer;

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
  SysUtils, Classes, ScktComp,
  RiggVar.Conn.Def,
  RiggVar.Conn.Sockets;

type
  TPolicyServer = class(TBaseServer)
  private
    ServerSocket: TServerSocket;
    procedure ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    function SendText(Socket: TCustomWinSocket; s: string): Boolean;
    function ReceiveText(Sender: TObject; Socket: TCustomWinSocket): string;
  public
    constructor Create(aPort: Integer; aServerFunction: TServerFunction); override;
    destructor Destroy; override;
    function Port: Integer; override;
    procedure Reply(Connection: TObject; s: string); override;
    function ConnectionCount: Integer; override;
    function IsDummy: Boolean; override;
  end;

  TPolicyNCP = class(TAdapterBaseNCP)
  private
    RequestCounter: Integer;
    ps: string;
    SL: TStringList;
    procedure Calc;
    procedure CalcNormal;
    procedure CalcPortSet;
  public
    constructor Create(aServer: TBaseServer);
    destructor Destroy; override;
    procedure HandleMsg(cm: TContextMsg); override;
    procedure InjectMsg(Sender: TObject; ms: TMsgSource; s: string); override;
  end;

  TPolicyProvider = class
  private
    function GetIsActive: Boolean;
  public
    PolicyNCP: TPolicyNCP;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    property IsActive: Boolean read GetIsActive;
  end;

implementation

uses
  RiggVar.App.Main;

const
  ExpectedRequestString: string = '<policy-file-request/>';

{ TPolicyNCP }

constructor TPolicyNCP.Create(aServer: TBaseServer);
begin
  SL := TStringList.Create;
  Calc;
  inherited Create(aServer);
end;

destructor TPolicyNCP.Destroy;
begin
  Server.Free;
  Server := nil;
  SL.Free;
  inherited;
end;

procedure TPolicyNCP.HandleMsg(cm: TContextMsg);
begin
  if cm.msg = ExpectedRequestString then
  begin
    Server.Reply(cm.Sender, ps);
    Inc(RequestCounter);
  end
  else
    cm.Sender.Free;
end;

procedure TPolicyNCP.Calc;
begin
  if Main.IniImage.UsePortSet then
    CalcPortSet
  else
    CalcNormal;  
end;

procedure TPolicyNCP.CalcPortSet;
var
  i: Integer;
begin
  SL.Add('<?xml version="1.0" encoding="utf-8"?>');
  SL.Add('<access-policy><cross-domain-access><policy>');

  SL.Add('<allow-from>');
  SL.Add('<domain uri="*"/>');
  SL.Add('</allow-from>');

  SL.Add('<grant-to>');

  if Main.IsScenarioEC2 then
  begin
    SL.Add('<socket-resource port="4530" protocol="tcp"/>'); //FR, Portset 1
    SL.Add('<socket-resource port="4531" protocol="tcp"/>'); //SK, Portset 2
    SL.Add('<socket-resource port="4532" protocol="tcp"/>'); //AB, Portset 3
  end
  else
  begin
    SL.Add('<socket-resource port="4530" protocol="tcp"/>'); //FR, Portset 1
    SL.Add('<socket-resource port="4531" protocol="tcp"/>'); //SK, Portset 2
    SL.Add('<socket-resource port="4532" protocol="tcp"/>'); //AB, Portset 3
  end;

  SL.Add('</grant-to>');
  SL.Add('</policy></cross-domain-access></access-policy>');

  ps := '';
  for i := 0 to SL.Count-1 do
  begin
    ps := ps + SL[i];
  end;
end;

procedure TPolicyNCP.CalcNormal;
var
  p: Integer;
  i: Integer;
begin
  if Assigned(Main.ServerBridge) then
    p := Main.ServerBridge.BridgeNCP.Server.Port
  else
    p := 4530;

  SL.Add('<?xml version="1.0" encoding="utf-8"?>');
  SL.Add('<access-policy><cross-domain-access><policy>');
  SL.Add('<allow-from>');
  SL.Add('<domain uri="*"/>');
  SL.Add('</allow-from>');
  SL.Add('<grant-to>');
  SL.Add(Format('<socket-resource port="%d" protocol="tcp"/>', [p]));
  SL.Add('</grant-to>');
  SL.Add('</policy></cross-domain-access></access-policy>');

  ps := '';
  for i := 0 to SL.Count-1 do
  begin
    ps := ps + SL[i];
  end;
end;

procedure TPolicyNCP.InjectMsg(Sender: TObject; ms: TMsgSource; s: string);
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
    if ch = '<' then
      BufferString := ''
    else if ch = '>' then
    begin
      cm := TContextMsg.Create;
      cm.MsgSource := ms;
      cm.Sender := Sender;
      cm.IsAdapterMsg := true;
      cm.msg := '<' + BufferString + '>';
      //cm.DecodeHeader;
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

{ TPolicyServer }

constructor TPolicyServer.Create(aPort: Integer;
  aServerFunction: TServerFunction);
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

destructor TPolicyServer.Destroy;
begin
  ServerSocket.Active := False;
  ServerSocket.Close;
  ServerSocket.Free;
  inherited;
end;

function TPolicyServer.ConnectionCount: Integer;
begin
  if Assigned(ServerSocket) then
    result := ServerSocket.Socket.ActiveConnections
  else
    result := 0;
end;

function TPolicyServer.IsDummy: Boolean;
begin
  result := false;
end;

function TPolicyServer.Port: Integer;
begin
  result := 943;
end;

procedure TPolicyServer.ServerSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  Main.Logger.Error('ServerSocketError (' + IntToStr(ErrorCode) + ')');
  ErrorCode := 0;
end;

procedure TPolicyServer.ServerSocketClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
  s: string;
begin
  s := ReceiveText(Sender, Socket);
  InjectMsg(Socket, s);
end;

procedure TPolicyServer.Reply(Connection: TObject; s: string);
var
  i: Integer;
  so: TCustomWinSocket;
begin
  for i := 0 to ServerSocket.Socket.ActiveConnections-1 do
  begin
    so := ServerSocket.Socket.Connections[i];
    if so = Connection then
    begin
      SendText(so, s);
      so.Free;
    end;
  end;
end;

function TPolicyServer.ReceiveText(Sender: TObject; Socket: TCustomWinSocket): string;
var
  s: AnsiString;
begin
  //s := string(Socket.ReceiveText); //broken
  SetLength(s, Socket.ReceiveLength);
  SetLength(s, Socket.ReceiveBuf(Pointer(s)^, Length(s)));
  result := string(s);
end;

function TPolicyServer.SendText(Socket: TCustomWinSocket; s: string): Boolean;
var
  i: Integer;
  utf8: Utf8String;
  ansi: AnsiString;
begin
  result := false;
  utf8 := UTF8Encode(s);
  ansi := AnsiString(utf8);
  if Assigned(Socket)  then
  begin
    i := Socket.SendText(ansi); //takes an AnsiString param
    result := i = Length(ansi);
  end;
end;

{ TPolicyProvider }

constructor TPolicyProvider.Create;
begin

end;

procedure TPolicyProvider.Init;
var
  ts: TBaseServer;
begin
  try
    try
      ts := TPolicyServer.Create(943, Function_Policy);
    except
      ts := TBaseServer.Create(943, Function_Policy);
    end;
    PolicyNCP := TPolicyNCP.Create(ts);
  except
    PolicyNCP.Free;
    PolicyNCP := nil;
  end;
end;

destructor TPolicyProvider.Destroy;
begin
  PolicyNCP.Free;
  inherited;
end;

function TPolicyProvider.GetIsActive: Boolean;
begin
  result := Assigned(PolicyNCP);
end;

end.
