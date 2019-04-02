unit RiggVar.Conn.Def;

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
  System.Classes;

const
  ServerStatus_Active = 1;

  ServerFunction_Input = 0;
  ServerFunction_Output = 1;
  ServerFunction_Bridge = 2;
  ServerFunction_Policy = 3;

type
  TServerFunction = (
    Function_Input,
    Function_Output,
    Function_Bridge,
    Function_Policy
  );

  TMsgSource = (
    msUnknown,
    msInternalInput,
    msInternalOutput,
    msUndoRedo,
    msBridge,
    msSwitch,
    msTCP
  );

  TMsgDirection = (
    mdUnknown,
    mdOutgoing,
    mdIncoming
    );

  TContextMsg = class
  private
    function GetIsSwitchMsg: Boolean;
    function GetEncodedMsg: string;
    function GetIsBridgeMsg: Boolean;
    function GetIsOutgoingMsg: Boolean;
    function GetIsIncomingMsg: Boolean;
    procedure SetIsOutgoingMsg(const Value: Boolean);
    procedure SetIsIncomingMsg(const Value: Boolean);
  public
    Sender: TObject;
    IsOwned: Boolean;
    IsAdapterMsg: Boolean;
    IsQueued: Boolean;
    MsgSource: TMsgSource;
    MsgDirection: TMsgDirection;
    MsgType: char;
    msg: string;
    HasRequest: Boolean;
    RequestString: string;
    OutputRequestList: TStringList; //initially nil by default
    Answer: String;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DecodeHeader;
    property EncodedMsg: string read GetEncodedMsg;
    property IsSwitchMsg: Boolean read GetIsSwitchMsg;
    property IsBridgeMsg: Boolean read GetIsBridgeMsg;
    property IsOutgoingMsg: Boolean read GetIsOutgoingMsg write SetIsOutgoingMsg;
    property IsIncomingMsg: Boolean read GetIsIncomingMsg write SetIsIncomingMsg;
  end;

  THandleTextMsgEvent = procedure(Sender: TObject; s: string) of object;
  THandleContextMsgEvent = procedure(Sender: TObject; cm: TContextMsg) of object;
  TInjectMsgEvent = procedure(Sender: TObject; MsgSource: TMsgSource; s: string) of object;
  THandleMsgEvent = procedure(cm: TContextMsg) of object;

  TBaseServer = class
  private
    FOnInjectMsg: TInjectMsgEvent;
    FOnHandleMsg: THandleMsgEvent;
    FServerFunction: TServerFunction;
    FPort: Integer;
    FActive: Boolean;
    FMsgSource: TMsgSource;
  public
    constructor Create(aPort: Integer; aServerFunction: TServerFunction); virtual;

    procedure HandleMsg(cm: TContextMsg); virtual;
    procedure SendMsg(cm: TContextMsg); virtual;
    procedure InjectMsg(Sender: TObject; s: string); virtual;
    procedure Reply(Connection: TObject; s: string); virtual;
    procedure Broadcast(Sender: TObject; s: string); virtual;
    procedure TopicBroadcast(SenderList: TList; s: string); virtual;

    function Port: Integer; virtual;
    function ConnectionCount: Integer; virtual;
    function Status: Integer; virtual;
    function IsDummy: Boolean; virtual;

    property OnInjectMsg: TInjectMsgEvent read FOnInjectMsg write FOnInjectMsg;
    property OnHandleMsg: THandleMsgEvent read FOnHandleMsg write FOnHandleMsg;
    property ServerFunction: TServerFunction read FServerFunction write FServerFunction;
    property Active: Boolean read FActive write FActive;
    property MsgSource: TMsgSource read FMsgSource write FMsgSource;
  end;

var
  SwitchSender: TObject;
  SwitchLocked: Boolean;
  BridgeLocked: Boolean;
  WebLocked: Boolean;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.TemplateIDs;

{ TBaseServer }

constructor TBaseServer.Create(aPort: Integer; aServerFunction: TServerFunction);
begin
  inherited Create;
  //constructor is virtual
  MsgSource := msUnknown;
  FPort := aPort;
  FServerFunction := aServerFunction;
end;

procedure TBaseServer.HandleMsg(cm: TContextMsg);
begin
  if Assigned(FOnHandleMsg) then
    FOnHandleMsg(cm);
end;

function TBaseServer.IsDummy: Boolean;
begin
  result := true;
end;

procedure TBaseServer.InjectMsg(Sender: TObject; s: string);
begin
  if Assigned(OnInjectMsg) then
    OnInjectMsg(Sender, MsgSource, s);
end;

function TBaseServer.Port: Integer;
begin
  //virtual
  result := FPort;
end;

procedure TBaseServer.Reply(Connection: TObject; s: string);
begin
  //virtual
end;

procedure TBaseServer.SendMsg(cm: TContextMsg);
begin
  //virtual
end;

procedure TBaseServer.TopicBroadcast(SenderList: TList; s: string);
begin
  //virtual
end;

procedure TBaseServer.Broadcast(Sender: TObject; s: string);
begin
  //virtual
end;

function TBaseServer.Status: Integer;
begin
  result := 0;
  if Active then
    result := 1;
end;

function TBaseServer.ConnectionCount: Integer;
begin
  result := 0;
end;

{ TContextMsg }

constructor TContextMsg.Create;
begin
  Inc(Main.TestCounter.ContextMsgCreateCounter);
end;

destructor TContextMsg.Destroy;
begin
  Inc(Main.TestCounter.ContextMsgDestroyCounter);
  inherited;
end;

function TContextMsg.GetIsBridgeMsg: Boolean;
begin
  result := MsgSource = msBridge;
end;

function TContextMsg.GetIsOutgoingMsg: Boolean;
begin
  result := MsgDirection = mdOutgoing;
end;

function TContextMsg.GetIsSwitchMsg: Boolean;
begin
  result := MsgSource = msSwitch;
end;

procedure TContextMsg.SetIsIncomingMsg(const Value: Boolean);
begin
  MsgDirection := mdIncoming;
end;

procedure TContextMsg.SetIsOutgoingMsg(const Value: Boolean);
begin
  MsgDirection := mdOutgoing;
end;

function TContextMsg.GetIsIncomingMsg: Boolean;
begin
  result := MsgDirection = mdIncoming;
end;

function TContextMsg.GetEncodedMsg: string;
begin
  result := '12';
  result[1] := MsgType;
  result[2] := #4;
  result := result + msg;
end;

procedure TContextMsg.Clear;
begin
  Sender := nil;
  MsgSource := msUnknown;
  MsgDirection := mdUnknown;
  MsgType := '-';
  msg := '';
  HasRequest := false;
  RequestString := '';
  OutputRequestList := nil;
end;

procedure TContextMsg.DecodeHeader;
var
  p: Integer;
  MsgHeader: string;
begin
  MsgHeader := MsgTypeUnspecified;
  p := Pos(#4, msg);
  if p > 1 then
  begin
    MsgHeader := Copy(msg, 1, p-1);
    MsgType := MsgHeader[1];
  end;
  if p > 0 then
    msg := Copy(msg, p+1, Length(msg)-p)
end;

end.
