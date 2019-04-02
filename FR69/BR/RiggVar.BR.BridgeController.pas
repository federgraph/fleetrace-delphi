unit RiggVar.BR.BridgeController;

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
  RiggVar.BR.PeerController,
  RiggVar.BR.BridgeAbstract,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern;

type
  TBridgeController = class(TPeerController)
  private
    crlf: string;
    FEnabled: Boolean;

    FSwitchID: Integer;
    FLastBackupID: Integer;
    FBackupMsgID: Integer;
    FLastMsgID: Integer;
    FLastOutTime: TDateTime;
    FLastInTime: TDateTime;

    FLogCapacity: Integer;
    FProxyType: Integer;
    FTaktIn: Integer;
    FTaktOut: Integer;
    FEventType: Integer;

    procedure SendMessages;
    procedure GetNewMessages;
    procedure MergeLine(const s: string);
    procedure SetTaktIn(const Value: Integer);
    procedure SetTaktOut(const Value: Integer);
    function GetServerUrl: string;
    procedure SetServerUrl(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
    procedure SetProxyType(const Value: Integer);
    function CheckTakt(Value: Integer): Integer;
    procedure AddLine(const s: string);
  protected
    Log: TStringList;
    SL: TStringList;
    SLMerge: TStringList;
    FBridge: TBridge;
    function GetIsMaster: Boolean; override;
    function GetConnected: Boolean; override;
    function GetHomeUrl: string; override;
  public
    InputConnection: TConnection;
    OutputConnection: TConnection;
    constructor Create;
    destructor Destroy; override;
    //
    procedure DoOnIdle; override;
    procedure DoOnBackup(s: string); override;
    procedure DoOnPlugin(SwitchID: Integer);
    procedure DoOnPlugout;
    procedure DoOnNewMessages(NewMessages: string);
    procedure DoOnGetLastMsgID(Value: Integer);
    procedure DoOnGetLastBackupID(Value: Integer);
    procedure DoOnRequest(Target: Integer; Request: string);
    //
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Add(Sender: TObject; cm: TContextMsg);
    procedure Clear;
    //
    procedure EditProps; override;
    function IsEnabled(Op: TSwitchOp): Boolean; override;
    function AllowRecreate: Boolean; override;
    //
    procedure Plugin; override;
    procedure Plugout; override;
    procedure Synchronize; override;
    procedure Upload(s: string); override;
    function Download: string; override;
    //
    procedure Close; override;
    procedure GetStatusReport(Memo: TStrings); override;
    //
    property Bridge: TBridge read FBridge;
    property ServerUrl: string read GetServerUrl write SetServerUrl;
    property TaktIn: Integer read FTaktIn write SetTaktIn;
    property TaktOut: Integer read FTaktOut write SetTaktOut;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ProxyType: Integer read FProxyType write SetProxyType;
    property EventType: Integer read FEventType;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.IniImage,
  RiggVar.BO.Def,
  RiggVar.App.Config,
  RiggVar.BO.Base,
  RiggVar.BO.MsgBase,
  RiggVar.BR.BridgeServer,
  RiggVar.BR.BridgeClient,
  RiggVar.BR.BridgeAsynchron,
  RiggVar.BR.BridgeSynchron,
  RiggVar.BR.BridgeREST,
  RiggVar.BR.BridgeProxy,
  RiggVar.BR.BridgeLocal;

{ TBridgeController }

constructor TBridgeController.Create;
begin
  FEventType := C_DefaultEventType;

  crlf := #13#10;
  FTaktOut := 30;
  FTaktIn := 30;
  Enabled := False;
  FLogCapacity := 2000;

  Log := TStringList.Create;
  SL := TStringList.Create;
  SLMerge := TStringList.Create;

  ProxyType := Main.IniImage.BridgeProxyType;

//  BridgeServiceType_null = 0;
//  BridgeServiceType_asmx = 1;
//  BridgeServiceType_php  = 2;
//  BridgeServiceType_tcpc = 3;
//  BridgeServiceType_tcps = 4;
//  BridgeServiceType_sync = 5;
//  BridgeServiceType_rest = 6;
//  BridgeServiceType_proxy = 7;

  case ProxyType of
    1: FBridge := Main.BridgeAdapter.GetBridgeProxy(1);
    2: FBridge := Main.BridgeAdapter.GetBridgeProxy(2);
    3: FBridge := TClientBridge.Create;
    4: FBridge := TServerBridge.Create(FLogCapacity);
    5: FBridge := TSynchronBridge.Create(FLogCapacity);
    6: FBridge := TRESTBridge.Create(Main.IniImage.DefaultEventType);
    7: FBridge := TProxyBridge.Create;
    8: FBridge := TCombiBridge.Create;
  else
    FBridge := TLocalBridge.Create(FLogCapacity);
  end;

  if FBridge = nil then
    FBridge := TLocalBridge.Create(FLogCapacity);

  if FBridge is TAsynchronBridge then
  begin
    (FBridge as TAsynchronBridge).BridgeController := self;
  end;

  if FBridge is TLocalBridge then
  begin
    if Main.IniImage.DefaultEventType = 400 then
      (FBridge as TLocalBridge).FMsgRing.UseMerge := False
    else if Main.IniImage.DefaultEventType = 600 then
      (FBridge as TLocalBridge).FMsgRing.UseMerge := True;
  end;

  FBridge.SetServerUrl(Main.IniImage.BridgeUrl);
  TaktIn := Main.IniImage.TaktIn;
  TaktOut := Main.IniImage.TaktOut;
end;

destructor TBridgeController.Destroy;
begin
  Enabled := False;
  Plugout;

  FBridge.Free;
  SL.Free;
  SLMerge.Free;
  Log.Free;
  inherited;
end;

procedure TBridgeController.Connect;
begin
  InputConnection := BO.InputServer.Server.Connect('Bridge.Input');
  OutputConnection := BO.OutputServer.Server.Connect('Bridge.Output');
  OutputConnection.OnSendMsg := Add;

  //notwendig, wenn bei Download SwapEvent verwendet wird anstelle von Load
  if (FSwitchID > 0) then
    Enabled := true;
end;

procedure TBridgeController.Disconnect;
begin
  //ok, wird bei Programmende zweimal aufgerufen,
  //einmal ausgehend von FormXX.DisposeViews
  //zum anderen über BridgeManager.Free -> BridgeController.Close
  InputConnection.Free;
  OutputConnection.Free;
  InputConnection := nil;
  OutputConnection := nil;
  Enabled := False;
end;

procedure TBridgeController.Clear;
begin
  Log.Clear;
  FBackupMsgID := FBridge.SendBackupAndLog(FSwitchID, '', '');
  FLastOutTime := Now;
  FLastInTime := FLastOutTime;
end;

procedure TBridgeController.Add(Sender: TObject; cm: TContextMsg);
begin
  if (BridgeLocked = False) and (cm.msg <> '') then
  begin
    //IsBuiltInServerBridge == 'TaktOut = 0' !
    if TaktOut = 0 then
    begin
      if FSwitchID > 0 then
        if not cm.IsOutgoingMsg then
          cm.IsOutgoingMsg := true;
        Bridge.SendContextMsg(FSwitchID, cm);
    end
    else
    begin
      if EventType = 400 then
        AddLine(cm.msg)
      else
        MergeLine(cm.msg);
    end;
  end;
end;

procedure TBridgeController.AddLine(const s: string);
begin
  if Log.Count > FLogCapacity then
    Log.Delete(0);
  Log.Add(s);
end;

function TBridgeController.AllowRecreate: Boolean;
begin
  result := true;
  //Main.IniImage.BridgeProxyType is new ProxyType
  if Main.IniImage.BridgeProxyType = 4 then
  begin
  //cannot recreate ServerBridge,
  //because in PeerManager PortOpenCheck returns 'Port already open'
  //even short after destruction of old ServerBridge
    if ProxyType = 4 then //ProxyType = current (old) ProxyType
  begin
    if Bridge.IsSameBridge then
      result := false;
  end;
end;
end;

procedure TBridgeController.MergeLine(const s: string);
var
  sK: string;
  sV: string;
  temp: string;
  i: Integer;
  LogMsgIndex: Integer;
begin
  SLMerge.Clear;
  i := Pos('=', s);
  if i > 0 then
    temp := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)))
  else
    temp := StringReplace(Trim(s), ' ', '_', [rfReplaceAll]);

  if Pos('=', temp) = 0 then
    temp := temp + '=';
  SLMerge.Add(temp);

  sK := SLMerge.Names[0];
  sV := SLMerge.Values[sK];
  //StringReplace(sV, '_', ' ', [rfReplaceAll]);

  LogMsgIndex := Log.IndexOfName(sK);
  if LogMsgIndex >= 0 then
  begin
    Log[LogMsgIndex] := sK + '=' + sV;
  end
  else
  begin
    if Log.Count > FLogCapacity then
      Log.Delete(0);
    Log.Add(sK + '=' + sV);
  end;
end;

procedure TBridgeController.DoOnIdle;
var
  Hour, Min, Sec, MSec: Word;
begin
  if Enabled then
  begin
    //Out
    DecodeTime(Now - FLastOutTime, Hour, Min, Sec, MSec);
    if (TaktOut > 0) and (Sec >= TaktOut) then
    begin
      SendMessages;
    end;

    //In
    DecodeTime(Now - FLastInTime, Hour, Min, Sec, MSec);
    if (TaktIn > 0) and (Sec >= TaktIn) then
    begin
      GetNewMessages;
    end;
  end;
end;

procedure TBridgeController.SendMessages;
begin
  if Log.Count > 1 then
  begin
    FBridge.SendDiffLog(FSwitchID, Log.Text);
    Log.Clear;
  end
  else if Log.Count = 1 then
  begin
    FBridge.SendMsg(FSwitchID, Log[0]);
    Log.Clear;
  end;
  FLastOutTime := Now;
end;

procedure TBridgeController.GetNewMessages;
var
  s: string;
  temp: Integer;
begin
  temp := FLastMsgID;
  FLastMsgID := FBridge.GetLastMsgID;
  s := FBridge.GetNewMessages(FSwitchID, temp);
  if (s <> '') then
    DoOnNewMessages(s);
  FLastInTime := Now;
end;

procedure TBridgeController.DoOnRequest(Target: Integer; Request: string);
var
  s: string;
begin
  s := BO.Output.GetMsg(Request);
  Bridge.SendAnswer(Target, s);
end;

procedure TBridgeController.DoOnNewMessages(NewMessages: string);
var
  i: Integer;
  s: string;
begin
  SL.Text := NewMessages;
  for i := 0 to SL.Count - 1 do
  begin
    s := SL[i];
    Main.InjectClientBridgeMsg(s)
  end;
end;

procedure TBridgeController.DoOnPlugin(SwitchID: Integer);
begin
  FSwitchID := SwitchID;
  if FSwitchID > 0 then
    Enabled := True;
end;

procedure TBridgeController.DoOnPlugout;
begin
  FSwitchID := 0;
  Enabled := False;
end;

procedure TBridgeController.DoOnGetLastMsgID(Value: Integer);
begin
  FLastMsgID := Value;
end;

procedure TBridgeController.DoOnBackup(s: string);
begin
  if (s <> '') then
  begin
    Log.Clear;
    inherited DoOnBackup(s);
  end;
end;

procedure TBridgeController.DoOnGetLastBackupID(Value: Integer);
begin
  FLastBackupID := Value;
end;

function TBridgeController.GetConnected: Boolean;
begin
  result := FSwitchID > 0;
end;

procedure TBridgeController.EditProps;
begin
  if Bridge is TClientBridge then
  begin
    Main.FormAdapter.EditConnectionProps(csBridge);
    //FrmConnectionProps.EditConnectionProps(Main.IniImage);
    { change will automatically be detected at plugin time...
    if FrmConnectionProps.EditConnectionProps(Main.IniImage) then
      (Bridge as TProxyBridge).ResultClient.NewClient(
        Main.IniImage.BridgeHost, Main.IniImage.BridgePort);
    }
  end
  else
  begin
    //edit ServerName, TaktIn, TaktOut
    Main.FormAdapter.EditBridgeProps(self);
    //FrmBridge.EditBridgeProps(self);
  end;
end;

procedure TBridgeController.Plugin;
begin
  if FSwitchID = 0 then
  begin
    FSwitchID := FBridge.PlugIn();
  end;
  if FSwitchID > 0 then
    Enabled := True;
end;

procedure TBridgeController.Plugout;
begin
  if FSwitchID > 0 then
    Enabled := False;

  FBridge.Plugout(FSwitchID);
  FSwitchID := 0;
end;

procedure TBridgeController.Synchronize;
begin
  if FSwitchID < 1 then
    FSwitchID := FBridge.PlugIn();
  if FSwitchID > 0 then
  begin
    SendMessages;
    GetNewMessages;
  end;
end;

procedure TBridgeController.Upload(s: string);
begin
  if s <> '' then
  begin
    Log.Clear;
    FBackupMsgID := FBridge.SendBackupAndLog(FSwitchID, s, '');
  end
end;

function TBridgeController.Download: string;
var
  s: string;
begin
  FLastBackupID := FBridge.GetLastBackupID;
  s := FBridge.GetBackup();
  if (s <> '') then
  begin
    Log.Clear;
    //### Backup + Log vom Web, aber DiffLog nicht enthalten
  end;
  result := s;
end;

procedure TBridgeController.Close;
begin
  Disconnect;
end;

function TBridgeController.GetServerUrl: string;
begin
  result := FBridge.GetServerUrl;
end;

procedure TBridgeController.GetStatusReport(Memo: TStrings);
begin
  Memo.Add('ClassType: TBridgeController');
  FBridge.GetStatusReport(Memo);
end;

procedure TBridgeController.SetServerUrl(const Value: string);
begin
  FBridge.SetServerUrl(Value);
  Main.IniImage.BridgeUrl := Value;
end;

procedure TBridgeController.SetProxyType(const Value: Integer);
begin
  FProxyType := Value;
end;

function TBridgeController.CheckTakt(Value: Integer): Integer;
begin
  result := Value;

  if Value < -1 then
    result := -1

  else if ProxyType = BridgeServiceType_asmx then
  begin
    if Value < 2 then
      result := 2
  end

  else if ProxyType = BridgeServiceType_php then
  begin
    //bei php auf Takt nicht kleiner als 10 Sekunden
    if Value < 10 then
      result := 10
  end

  else if ProxyType = BridgeServiceType_client then
  begin
  end

  else if ProxyType = BridgeServiceType_server then
  begin
  end;

end;

procedure TBridgeController.SetTaktIn(const Value: Integer);
begin
  if Value <> FTaktIn then
    FTaktIn := CheckTakt(Value);
  Main.IniImage.TaktIn := FTaktIn;
end;

procedure TBridgeController.SetTaktOut(const Value: Integer);
begin
  if Value <> FTaktOut then
    FTaktOut := CheckTakt(Value);
  Main.IniImage.TaktOut := FTaktOut;
end;

function TBridgeController.GetEnabled: Boolean;
begin
  result := FEnabled and not FBridge.HasError;
end;

function TBridgeController.GetHomeUrl: string;
var
  t: string;
  p: Integer;
begin
  t := Main.IniImage.BridgeUrl;

  if Main.IniImage.BridgeHomePage <> '' then
  begin
    //ASP.NET Handler requires special handling
    //e.g http://gsvista/FR88/REST/Bridge/
    p := Pos('/REST/Bridge/', t);
    if p > 0 then
    begin
      t := Copy(t, 0, p);
      result := t + Main.IniImage.BridgeHomePage
    end
    else
      result := t + Main.IniImage.BridgeHomePage
  end

  else if ProxyType = BridgeServiceType_asmx then
  begin
    if EventType = 400 then
      result := t + 'FRBridge.aspx'
    else
      result := t + 'SKKBridge.aspx'
  end

  else if ProxyType = BridgeServiceType_php then
  begin
    if EventType = 400 then
      result := t + 'FR88_FRPage.php'
    else
      result := t + 'FR88_SKKPage.php'
  end

  else if ProxyType = BridgeServiceType_client then
    result := t

  else if ProxyType = BridgeServiceType_server then
    result := t

  else if ProxyType = BridgeServiceType_proxy then
    result := t

  else if ProxyType = BridgeServiceType_combi then
    result := t

  else
    result := 'about:blank';
end;

procedure TBridgeController.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TBridgeController.IsEnabled(Op: TSwitchOp): Boolean;
begin
  if Bridge = nil then
    result := false
  else
  begin

    case Op of
      SwitchOp_Plugin:
        result := true;
      SwitchOp_Plugout:
        result := true;
      SwitchOp_Synchronize:
        result := IsMaster;
      SwitchOp_Upload:
        result := Connected and IsMaster;
      SwitchOp_Download:
        result := true;
      else
        result := false;
    end;

    result := result and Bridge.IsEnabled(Op);

  end;

end;

function TBridgeController.GetIsMaster: Boolean;
begin
  result := Main.IniImage.IsMaster;
end;

end.
