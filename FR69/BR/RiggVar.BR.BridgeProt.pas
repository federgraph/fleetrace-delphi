unit RiggVar.BR.BridgeProt;

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

{ BridgeProt, alias ProxyProt,
  used in FR62 when configured as ServerBridge

  * marshalls calls on and off the wire,
    from server perspective (deserializes messages sent from AsnchronBridge)

  * a reference to the data (LocalBridge) is injected by the container

  * public Calc method processes the msg

  * protected DispatchMsg method deserializes the msg
    and delegates to LocalBridge

  * an OnBroadcast event is used with SendMsg calls
    so msg is passed on to all connected parties except the sender

  * is base class for TProxyProt = class(TBridgeProt)
    used in RA21

}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.Util.Classes,
  RiggVar.BR.PeerController,
  RiggVar.BR.BridgeAbstract,
  RiggVar.BR.BridgeLocal,
  RiggVar.Conn.Def;

type
  TBridgeProt = class
  private
    FLocalBridge: TLocalBridge;
    FOnBroadcast: THandleTextMsgEvent;
    procedure SetLocalBridge(const Value: TLocalBridge);
    procedure SetOnBroadcast(const Value: THandleTextMsgEvent);
  protected
    SL: TStringList;
    MsgCounterIn: Integer;
    MsgText: string;
    CM: TContextMsg;
    function PopInt: Integer;
    function PopString: string;
    function DispatchMsg(Sender: TObject; s: string): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Calc(Sender: TObject; msg: string): string;
    property LocalBridge: TLocalBridge read FLocalBridge write SetLocalBridge;
    property OnBroadcast: THandleTextMsgEvent read FOnBroadcast write SetOnBroadcast;
  end;

implementation

{ TBridgeProt }

constructor TBridgeProt.Create;
begin
  inherited Create;
  SL := TStringList.Create;
  CM := TContextMsg.Create;
  CM.IsOwned := True;
end;

destructor TBridgeProt.Destroy;
begin
  SL.Free;
  CM.Free;
  inherited Destroy;
end;

function TBridgeProt.PopInt: Integer;
begin
  result := -1;
  if SL.Count > 0 then
  begin
    result := StrToIntDef(SL[0], -1);
    SL.Delete(0);
  end;
end;

function TBridgeProt.PopString: string;
begin
  result := '';
  if SL.Count > 0 then
  begin
    result := Trim(SL[0]);
    SL.Delete(0);
  end;
end;

procedure TBridgeProt.SetLocalBridge(const Value: TLocalBridge);
begin
  FLocalBridge := Value;
end;

procedure TBridgeProt.SetOnBroadcast(const Value: THandleTextMsgEvent);
begin
  FOnBroadcast := Value;
end;

function TBridgeProt.Calc(Sender: TObject; msg: string): string;
var
  s: string;
begin
  result := '';
  Inc(MsgCounterIn);
  LocalBridge.DoOnUpdateUI(UUIAction_MsgCounterIn, IntToStr(MsgCounterIn));
  MsgText := msg;
  if msg <> '' then
  begin
    SL.Text := msg;
    if SL.Count > 0 then
    begin
      s := SL[0];
      SL.Delete(0);
      result := DispatchMsg(Sender, s);
    end;
  end;
end;

function TBridgeProt.DispatchMsg(Sender: TObject; s: string): string;
var
  m: string;
  MsgID: Integer;
  SwitchID: Integer;
  BackupID: Integer;
  BackupMsgID: Integer;
  StartMsgID: Integer;
  StartBackupID: Integer;
begin
  result := '';
  if s = 'Plugin' then
  begin
    SwitchID := LocalBridge.Plugin;
    SL.Clear;
    SL.Add(s);
    SL.Add(IntToStr(SwitchID));
    result := SL.Text;
  end

  else if s = 'Plugout' then
  begin
    SwitchID := PopInt;
    if SwitchID > 0 then
      LocalBridge.Plugout(SwitchID);
  end

  else if s = 'SendBackup' then
  begin
    SwitchID := PopInt;
    s := PopString;
    if SwitchID > -1 then
    begin
      BackupMsgID := LocalBridge.SendBackupAndLog(SwitchID, SL.Text, '');
      SL.Clear;
      SL.Add(s);
      SL.Add(IntToStr(BackupMsgID));
      result := SL.Text;
    end;
  end

  else if s = 'SendDiffLog' then
  begin
    SwitchID := PopInt;
    if SwitchID > -1 then
      LocalBridge.SendDiffLog(SwitchID, SL.Text);
  end

  else if s = 'SendMsg' then
  begin
    SwitchID := PopInt;
    if SwitchID > -1 then
    begin
      m := PopString;
      if (m <> '') and (Length(m) < 200) then
      begin

        //pass msg on to all except the sender
        if Assigned(OnBroadcast) then
        begin
          SL.Clear;
          SL.Add('GetNewMessages');
          SL.Add(m);
          OnBroadcast(Sender, SL.Text);
          LocalBridge.DoOnUpdateUI(UUIAction_Msg, m);
        end;

        //add to MsgRing, update UI, update local App 
        CM.msg := m;
        CM.IsIncomingMsg := true;
        LocalBridge.SendContextMsg(SwitchID, CM);
        CM.Clear;
      end;
    end;
  end

  else if s = 'GetBackup' then
  begin
    SL.Text := LocalBridge.GetBackup;
    SL.Insert(0, s);
    result := SL.Text;
  end

  else if s = 'GetNewMessages' then
  begin
    SwitchID := PopInt;
    if SwitchID > 0 then
    begin
      StartMsgID := PopInt;
      if StartMsgID > -1 then
      begin
        SL.Text := LocalBridge.GetNewMessages(SwitchID, StartMsgID);
        SL.Insert(0, s);
        result := SL.Text;
      end;
    end;
  end

  else if s = 'LogValid' then
  begin
    SL.Text := s;
    SL.Add(BoolStr[LocalBridge.LogValid]);
    result := SL.Text;
  end

  else if s = 'GetLastMsgID' then
  begin
    MsgID := LocalBridge.GetLastMsgID;
    SL.Text := s;
    SL.Add(IntToStr(MsgID));
    result := SL.Text;
  end

  else if s = 'GetLastBackupID' then
  begin
    BackupID := LocalBridge.GetLastBackupID;
    SL.Text := s;
    SL.Add(IntToStr(BackupID));
    result := SL.Text;
  end

  else if s = 'GetBackupSwitchID' then
  begin
    SwitchID := LocalBridge.GetBackupSwitchID;
    SL.Text := s;
    SL.Add(IntToStr(SwitchID));
    result := SL.Text;
  end

  else if s = 'CheckForBackup' then
  begin
    SwitchID := PopInt;
    if SwitchID > 0 then
    begin
      StartBackupID := PopInt;
      if StartBackupID > -1 then
      begin
        SL.Text := s;
        SL.Add(BoolStr[LocalBridge.CheckForBackup(SwitchID, StartBackupID)]);
        result := SL.Text;
      end;
    end;
  end

  else if s = 'CheckForLog' then
  begin
    SwitchID := PopInt;
    if SwitchID > 0 then
    begin
      StartMsgID := PopInt;
      if StartMsgID > -1 then
      begin
        SL.Text := s;
        SL.Add(BoolStr[LocalBridge.CheckForBackup(SwitchID, StartMsgID)]);
        result := SL.Text;
      end;
    end;
  end

end;

end.
