unit RiggVar.Conn.Intern;

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
  RiggVar.Conn.Def;

type
  TServerIntern = class;

  TConnection = class(TCollectionItem)
  private
    FAnswer: string;
    FOnSendMsg: THandleContextMsgEvent;
    FRefCountingEnabled: Boolean;
    FName: string;
    function GetServer: TServerIntern;
    function GetPort: Integer;
    function GetIsOutput: Boolean;
    procedure SetOnSendMsg(const Value: THandleContextMsgEvent);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure InjectMsg(s: string);
    function HandleMsg(s: string): string;
    procedure HandleContextMsg(cm: TContextMsg);
    procedure SetAnswer(s: string);
    procedure SendMsg(cm: TContextMsg);
    property Server: TServerIntern read GetServer;
    property Port: Integer read GetPort;
    property IsOutput: Boolean read GetIsOutput;
    property OnSendMsg: THandleContextMsgEvent read FOnSendMsg write SetOnSendMsg;
    property Name: string read FName write FName;
  end;

  TConnections = class(TCollection)
  private
    function GetConnection(Index: Integer): TConnection;
  public
    Server: TServerIntern;
    function Add: TConnection;
    property Connection[Index: Integer]: TConnection read GetConnection; default;
  end;

  TServerIntern = class(TBaseServer)
  private
    FConnections: TConnections;
  public
    constructor Create(aPort: Integer; aServerFunction: TServerFunction); override;
    destructor Destroy; override;

    procedure SendMsg(cm: TContextMsg); override;
    procedure Reply(Connection: TObject; s: string); override;
    function ConnectionCount: Integer; override;
    procedure ConnectionReport(Memo: TStrings);

    function Connect(aName: string): TConnection;
    property Connections: TConnections read FConnections;
  end;

  TBaseNCP = class
  protected
    Counter: Integer;
  public
    Server: TServerIntern;
    constructor Create(aServer: TServerIntern);
    destructor Destroy; override;
    procedure InjectMsg(Sender: TObject; ms: TMsgSource; s: string); virtual; abstract;
    procedure HandleMsg(cm: TContextMsg); virtual; abstract;
  end;

implementation

uses
  RiggVar.BO.Def;

{ TServerIntern }

function TServerIntern.Connect(aName: string): TConnection;
begin
  result := Connections.Add;
  result.Name := aName;
end;

function TServerIntern.ConnectionCount: Integer;
begin
  result := Connections.Count;
end;

procedure TServerIntern.ConnectionReport(Memo: TStrings);
var
  i: Integer;
begin
  for i := 0 to Connections.Count - 1 do
    Memo.Add(Connections[i].FName);
end;

constructor TServerIntern.Create(aPort: Integer; aServerFunction: TServerFunction);
begin
  inherited;
  if aServerFunction = Function_Input then
    MsgSource := msInternalInput
  else
    MsgSource := msInternalOutput;
  FConnections := TConnections.Create(TConnection);
  FConnections.Server := Self;
  Active := True;
end;

destructor TServerIntern.Destroy;
begin
  FConnections.Free;
  inherited;
end;

procedure TServerIntern.Reply(Connection: TObject; s: string);
var
  i: Integer;
  so: TConnection;
begin
  for i := 0 to FConnections.Count-1 do
  begin
    so := FConnections[i];
    if so = Connection then
      so.SetAnswer(s); //selective
  end;
end;

procedure TServerIntern.SendMsg(cm: TContextMsg);
var
  i: Integer;
begin
  for i := 0 to FConnections.Count-1 do
    FConnections[i].SendMsg(cm)
end;

{ TConnections }

function TConnections.Add: TConnection;
begin
  result := inherited Add as TConnection;
end;

function TConnections.GetConnection(Index: Integer): TConnection;
begin
  if (Index < 0) or (Index >= Count) then
    result := nil
  else
    Result := TConnection(Items[Index]);
end;

{ TConnection }

constructor TConnection.Create(ACollection: TCollection);
begin
  inherited;
  FRefCountingEnabled := True;
end;

destructor TConnection.Destroy;
begin
  inherited;
end;

function TConnection.GetServer: TServerIntern;
begin
  result := (Collection as TConnections).Server;
end;

function TConnection.GetPort: Integer;
begin
  result := Integer(ID);
end;

procedure TConnection.InjectMsg(s: string);
begin
  Server.InjectMsg(Self, s);
end;

procedure TConnection.HandleContextMsg(cm: TContextMsg);
begin
  Server.HandleMsg(cm);
  if not IsOutput then
    BO.OnIdle; //--> Calc, ProcessQueue --> FAnswer := ...
end;

function TConnection.HandleMsg(s: string): string;
begin
  result := '';
  FAnswer := '';
  Server.InjectMsg(Self, s);
  WebLocked := false; //used to prevent outgoing msg from Undo/Redo
  if not IsOutput then
  begin
    BO.OnIdle; //--> Calc, ProcessQueue --> FAnswer := ...
  end;
  result := FAnswer;
end;

procedure TConnection.SetAnswer(s: string);
begin
  FAnswer := s; //siehe HandleMsg, wird dort zugewiesen an result
end;

procedure TConnection.SendMsg(cm: TContextMsg);
begin
  if IsOutput and Assigned(OnSendMsg) then
    OnSendMsg(Self, cm);
end;

function TConnection.GetIsOutput: Boolean;
begin
  result := Server.ServerFunction = Function_Output;
end;

procedure TConnection.SetOnSendMsg(const Value: THandleContextMsgEvent);
begin
  FOnSendMsg := Value;
end;

{ TBaseNCP }

constructor TBaseNCP.Create(aServer: TServerIntern);
begin
  inherited Create;
  Server := aServer;
  Server.OnInjectMsg := InjectMsg;
  Server.OnHandleMsg := HandleMsg;
end;

destructor TBaseNCP.Destroy;
begin
  Server.Free;
  inherited;
end;

end.
