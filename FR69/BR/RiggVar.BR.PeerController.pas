unit RiggVar.BR.PeerController;

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
  //(U)pdate (U)ser (I)nterface Action constants...
  UUIAction_Port = 0;
  UUIAction_SwitchID = 1;
  UUIAction_MsgID = 2;
  UUIAction_BackupID = 3;
  UUIAction_BackupMsgID = 4;
  UUIAction_BackupSwitchID = 5;
  UUIAction_MsgCounterIn = 6;
  UUIAction_Msg = 7;
  UUIAction_Clear = 8;
  UUIAction_TopicCount = 9;
//  UUIAction_ClientCounter = 10;
//  UUIAction_TopicBroadcast = 11;
  UUIAction_TopicRequest = 12;

type
  TSwitchOp = (
    SwitchOp_Plugin,
    SwitchOp_Plugout,
    SwitchOp_Synchronize,
    SwitchOp_Upload,
    SwitchOp_Download
  );

  TUpdateUIAction = (
    UpdateUIAction_Port,
    UpdateUIAction_SwitchID,
    UpdateUIAction_MsgID,
    UpdateUIAction_BackupID,
    UpdateUIAction_BackupMsgID,
    UpdateUIAction_BackupSwitchID,
    UpdateUIAction_MsgCounterIn,
    UpdateUIAction_Msg,
    UpdateUIAction_Clear
  );

  TBridgeMsgEvent = procedure(Sender: TObject; s: string) of object;
  TUpdateUIEvent = procedure(Action: Integer; s: string) of object;

  TPeerController = class
  private
    FPlugTouched: Boolean;
    FOnBackup: TBridgeMsgEvent;
    FOnUpdateUI: TUpdateUIEvent;
    procedure SetOnBackup(const Value: TBridgeMsgEvent);
    procedure SetOnUpdateUI(const Value: TUpdateUIEvent);
  protected
    function GetConnected: Boolean; virtual;
    function GetHomeUrl: string; virtual;
    function GetIsMaster: Boolean; virtual;
  public
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure DoOnIdle; virtual;
    procedure DoOnBackup(s: string); virtual;
    procedure DoOnUpdateUI(Action: Integer; s: string);
    //
    procedure EditProps; virtual;
    function IsEnabled(Op: TSwitchOp): Boolean; virtual;
    function AllowRecreate: Boolean; virtual;
    //
    procedure Plugin; virtual;
    procedure Plugout; virtual;
    procedure Synchronize; virtual;
    procedure Upload(s: string); virtual;
    function Download: string; virtual;
    //
    procedure Close; virtual;
    procedure GetStatusReport(Memo: TStrings); virtual;
    //
    property IsMaster: Boolean read GetIsMaster;
    property Connected: Boolean read GetConnected;
    property PlugTouched: Boolean read FPlugTouched write FPlugTouched;
    property HomeUrl: string read GetHomeUrl;
    property OnBackup: TBridgeMsgEvent read FOnBackup write SetOnBackup;
    property OnUpdateUI: TUpdateUIEvent read FOnUpdateUI write SetOnUpdateUI;
  end;

implementation

{ TPeerController }

function TPeerController.AllowRecreate: Boolean;
begin
  result := true;
end;

procedure TPeerController.Close;
begin

end;

procedure TPeerController.Connect;
begin
  //connect interne Verbindung
end;

procedure TPeerController.Disconnect;
begin
  //disconnect interne Verbindung
end;

procedure TPeerController.DoOnBackup(s: string);
begin
  if Assigned(FOnBackup) and (s <> '') then
    FOnBackup(self, s);
end;

procedure TPeerController.DoOnIdle;
begin

end;

procedure TPeerController.DoOnUpdateUI(Action: Integer; s: string);
begin
  if Assigned(FOnUpdateUI) then
    FOnUpdateUI(Action, s);
end;

function TPeerController.Download: string;
begin

end;

procedure TPeerController.EditProps;
begin

end;

function TPeerController.GetConnected: Boolean;
begin
  result := false;
end;

function TPeerController.GetHomeUrl: string;
begin
  result := 'about:blank';
end;

function TPeerController.GetIsMaster: Boolean;
begin
  result := false;
end;

procedure TPeerController.GetStatusReport(Memo: TStrings);
begin

end;

function TPeerController.IsEnabled(Op: TSwitchOp): Boolean;
begin
  result := false;
end;

procedure TPeerController.Plugin;
begin

end;

procedure TPeerController.Plugout;
begin

end;

procedure TPeerController.SetOnBackup(const Value: TBridgeMsgEvent);
begin
  FOnBackup := Value;
end;

procedure TPeerController.SetOnUpdateUI(const Value: TUpdateUIEvent);
begin
  FOnUpdateUI := Value;
end;

procedure TPeerController.Synchronize;
begin

end;

procedure TPeerController.Upload(s: string);
begin

end;

end.
