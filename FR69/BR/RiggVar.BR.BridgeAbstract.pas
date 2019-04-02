unit RiggVar.BR.BridgeAbstract;

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
  RiggVar.BR.PeerController,
  RiggVar.Conn.Def;

  {
  - jeder EventType hat seine eigene Bridge, EventType ist daher kein Parameter
  - SwitchID wird mit LogMsg gespeichert
  - was mit SendMsg zur Bridge gesendet wird soll mit GetLog nicht
    wieder abgeholt werden
  - GetLog holt nur Nachrichten mit MsgID > paramMsgID, damit Nachrichten nicht
    mehrfach vom gleichen Switch abgeholt werden.
  - MsgID >= 0, -1 holt alle Nachrichten ab, in diesem Spezialfall auch die
    eigenen
  - GetBackup holt Backup + Log
  - IsOK true wenn kein Überlauf erfolgt ist (Kapazität des Msg Speichers ist
    normalerweise begrenzt)
  - upload is queued, timed
  - download by polling, unter Verwendung von CheckUpdate

  - Plugin() liefert BridgeID zurück
  }

const
  crlf = #13#10;

  BridgeServiceType_null = 0;
  BridgeServiceType_asmx = 1;
  BridgeServiceType_php  = 2;
  BridgeServiceType_client = 3;
  BridgeServiceType_server = 4;
  BridgeServiceType_synchron = 5;
  BridgeServiceType_rest = 6;
  BridgeServiceType_proxy = 7;
  BridgeServiceType_combi = 8;

type
  TBridge = class
  private
    FOnUpdateUI: TUpdateUIEvent;
    procedure SetOnUpdateUI(const Value: TUpdateUIEvent);
  public
    function Plugin(): Integer; virtual; abstract;
    procedure Plugout(SwitchID: Integer); virtual; abstract;
    //
    function SendBackupAndLog(SwitchID: Integer; Backup: string; Log: string): Integer;  virtual; abstract;
    procedure SendDiffLog(SwitchID: Integer; DiffLog: string); virtual; abstract;
    procedure SendContextMsg(SwitchID: Integer; cm: TContextMsg); virtual;
    procedure SendMsg(SwitchID: Integer; msg: string); virtual; abstract;
    procedure SendAnswer(Target: Integer; Answer: string); virtual; //added for ProxyBridge,  13.06.2007
    //
    function GetBackup(): string; virtual; abstract;
    function GetNewMessages(SwitchID: Integer; StartMsgID: Integer): string; virtual; abstract;
    //
    function LogValid(): Boolean; virtual; abstract;
    //
    function GetBackupSwitchID: Integer; virtual; abstract;
    function GetLastBackupID: Integer; virtual; abstract;
    function GetLastMsgID: Integer; virtual; abstract;
    function CheckForBackup(SwitchID: Integer; StartBackupID: Integer): Boolean; virtual; abstract;
    function CheckForLog(SwitchID: Integer; StartMsgID: Integer): Boolean; virtual; abstract;

    function GetServerUrl: string; virtual;
    procedure SetServerUrl(const Value: string); virtual;

    procedure GetStatusReport(Memo: TStrings); virtual;
    function IsEnabled(Op: TSwitchOp): Boolean; virtual;
    function HasError: Boolean; virtual;
    function IsSameBridge: Boolean; virtual;
    procedure DoOnUpdateUI(Action: Integer; s: string);
    property OnUpdateUI: TUpdateUIEvent read FOnUpdateUI write SetOnUpdateUI;
  end;

implementation

{ TBridge }

procedure TBridge.DoOnUpdateUI(Action: Integer; s: string);
begin
  if Assigned(FOnUpdateUI) then
    FOnUpdateUI(Action, s);
end;

function TBridge.GetServerUrl: string;
begin
  result := '';
end;

procedure TBridge.GetStatusReport(Memo: TStrings);
begin
  Memo.Add('-- TBridge.StatusReport');
end;

function TBridge.HasError: Boolean;
begin
  result := False;
end;

function TBridge.IsEnabled(Op: TSwitchOp): Boolean;
begin
  result := true;
end;

function TBridge.IsSameBridge: Boolean;
begin
  //PeerController.AllowRecreate may need to detect configuration changes

  //check instance properties against configuration for changed parameters
  //return true to suppress recreation of bridge with same params
  //which might fail, e.g. because of 'port already open'
  result := false;
end;

procedure TBridge.SendAnswer(Target: Integer; Answer: string);
begin

end;

procedure TBridge.SendContextMsg(SwitchID: Integer; cm: TContextMsg);
begin
  SendMsg(SwitchID, cm.msg);
end;

procedure TBridge.SetOnUpdateUI(const Value: TUpdateUIEvent);
begin
  FOnUpdateUI := Value;
end;

procedure TBridge.SetServerUrl(const Value: string);
begin

end;

end.
