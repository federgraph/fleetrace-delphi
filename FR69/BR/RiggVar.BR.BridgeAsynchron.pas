unit RiggVar.BR.BridgeAsynchron;

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

{
  marshalls calls on and off the wire,
  from client perspective

  TBridge <- TAsynchronBridge <- ClientBridge <- ProxyBridge
}

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  RiggVar.BR.BridgeAbstract,
  RiggVar.BR.BridgeController,
  RiggVar.Util.Classes;

type
  TAsynchronBridge = class(TBridge)
  protected
    FSwitchID: Integer;
    FMsgID: Integer;
    FBackupID: Integer;
    FBackupSwitchID: Integer;
    FLogValid: Boolean;
    FCheckForBackup: Boolean;
    FCheckForLog: Boolean;

    SL: TStringList;
    MsgCounterIn: Integer;
    MsgCounterOut: Integer;
    MsgText: string;
    function PopInt: Integer;
    function PopStr: string;
    function PopBool: Boolean;
    procedure Send(s: string); virtual;
    procedure SetOutputMsg(Sender: TObject; msg: string);
    procedure DispatchMsg(s: string); virtual;
  public
    BridgeController: TBridgeController;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function Plugin(): Integer; override;
    procedure Plugout(SwitchID: Integer); override;
    //
    function SendBackupAndLog(SwitchID: Integer; Backup: string; Log: string): Integer; override;
    procedure SendDiffLog(SwitchID: Integer; DiffLog: string); override;
    procedure SendMsg(SwitchID: Integer; msg: string); override;
    //
    function GetBackup: string; override;
    function GetNewMessages(SwitchID: Integer; StartMsgID: Integer): string; override;
    //
    function LogValid: Boolean; override;
    //
    function GetBackupSwitchID: Integer; override;
    function GetLastBackupID: Integer; override;
    function GetLastMsgID: Integer; override;
    function CheckForBackup(SwitchID: Integer; StartBackupID: Integer): Boolean; override;
    function CheckForLog(SwitchID: Integer; StartMsgID: Integer): Boolean; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TAsynchronBridge }

constructor TAsynchronBridge.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TAsynchronBridge.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TAsynchronBridge.Plugin: Integer;
begin
  SL.Clear;
  SL.Add('Plugin');
  SL.Add('');
  Send(SL.Text);
  result := FSwitchID;
end;

procedure TAsynchronBridge.Plugout(SwitchID: Integer);
begin
  SL.Clear;
  SL.Add('Plugout');
  SL.Add(IntToStr(SwitchID));
  SL.Add('');
  Send(SL.Text);
end;

function TAsynchronBridge.SendBackupAndLog(SwitchID: Integer; Backup,
  Log: string): Integer;
begin
  //Log is always '' (in case of ResultServer)
  SL.Text := Backup;
  SL.Insert(0, IntToStr(SwitchID));
  SL.Insert(0,'SendBackup');
  Send(SL.Text);
  result := FBackupID;
end;

procedure TAsynchronBridge.SendDiffLog(SwitchID: Integer; DiffLog: string);
begin
  SL.Text := DiffLog;
  SL.Insert(0, IntToStr(SwitchID));
  SL.Insert(0,'SendDiffLog');
  Send(SL.Text);
end;

procedure TAsynchronBridge.SendMsg(SwitchID: Integer; msg: string);
begin
  SL.Clear;
  SL.Add('SendMsg');
  SL.Add(IntToStr(SwitchID));
  SL.Add(msg);
  Send(SL.Text);
end;

function TAsynchronBridge.GetBackup: string;
begin
  Send('GetBackup');
  result := ''; //asynchron, notify later when backup has arrived
end;

function TAsynchronBridge.GetNewMessages(SwitchID, StartMsgID: Integer): string;
begin
  SL.Clear;
  SL.Add('GetNewMessages');
  SL.Add(IntToStr(SwitchID));
  SL.Add(IntToStr(StartMsgID));
  Send(SL.Text);

  result := ''; //asynchron, call OnNewMessages later
end;

function TAsynchronBridge.LogValid: Boolean;
begin
  SL.Clear;
  SL.Add('LogValid');
  SL.Add('');
  Send(SL.Text);
  result := FLogValid;
end;

function TAsynchronBridge.GetBackupSwitchID: Integer;
begin
  SL.Clear;
  SL.Add('GetBackupSwitchID');
  SL.Add('');
  Send(SL.Text);
  result := FBackupSwitchID;
end;

function TAsynchronBridge.GetLastBackupID: Integer;
begin
  SL.Clear;
  SL.Add('GetLastBackupID');
  SL.Add('');
  Send(SL.Text);
  result := FBackupID;
end;

function TAsynchronBridge.GetLastMsgID: Integer;
begin
  SL.Clear;
  SL.Add('GetLastMsgID');
  SL.Add('');
  Send(SL.Text);
  result := FMsgID;
end;

function TAsynchronBridge.CheckForBackup(SwitchID,
  StartBackupID: Integer): Boolean;
begin
  SL.Add('CheckForBackup');
  SL.Add(IntToStr(SwitchID));
  SL.Add(IntToStr(StartBackupID));
  Send(SL.Text);
  result := FCheckForBackup;
end;

function TAsynchronBridge.CheckForLog(SwitchID, StartMsgID: Integer): Boolean;
begin
  result := FCheckForLog;
end;

procedure TAsynchronBridge.Send(s: string);
begin
  //virtual
end;

procedure TAsynchronBridge.SetOutputMsg(Sender: TObject; msg: string);
var
  s: string;
begin
  Inc(MsgCounterIn);
  MsgText := msg;
  if msg <> '' then
  begin
    SL.Text := msg;
    s := PopStr;
    if s <> '' then
    begin
      DispatchMsg(s);
    end;
  end;
end;

procedure TAsynchronBridge.DispatchMsg(s: string);
begin
  if s = 'Plugin' then
  begin
    FSwitchID := PopInt;
    BridgeController.DoOnPlugin(FSwitchID);
  end

  else if s = 'SendBackup' then //SendBackupAndLog
  begin
    FBackupID := PopInt;
    BridgeController.DoOnGetLastBackupID(FBackupID);
  end

  else if s = 'GetBackup' then
  begin
    if SL.Count > 0 then
    begin
      BridgeController.DoOnBackup(SL.Text);
    end;
  end

  else if s = 'GetNewMessages' then
  begin
    BridgeController.DoOnNewMessages(SL.Text);
  end

  else if s = 'LogValid' then
  begin
    FLogValid := PopBool;
  end

  else if s = 'GetLastBackupID' then
  begin
    FBackupID := PopInt;
    BridgeController.DoOnGetLastBackupID(FBackupID);
  end

  else if s = 'GetLastMsgID' then
  begin
    FMsgID := PopInt;
    BridgeController.DoOnGetLastMsgID(FMsgID);
  end

  else if s = 'GetLastBackupSwitchID' then
  begin
    FBackupSwitchID := PopInt;
    //BridgeController.
  end

  else if s = 'CheckForBackup' then
  begin
    FCheckForBackup := PopBool;
    //BridgeController.
  end

  else if s = 'CheckForLog' then
  begin
    FCheckForLog := PopBool;
    //BridgeController.
  end
end;

function TAsynchronBridge.PopInt: Integer;
begin
  result := -1;
  if SL.Count > 0 then
  begin
    result := StrToIntDef(SL[0], -1);
    SL.Delete(0);
  end;
end;

function TAsynchronBridge.PopStr: string;
begin
  result := '';
  if SL.Count > 0 then
  begin
    result := SL[0];
    SL.Delete(0);
  end;
end;

function TAsynchronBridge.PopBool: Boolean;
begin
  result := False;
  if SL.Count > 0 then
  begin
    result := TUtils.IsTrue(SL[0]);
    SL.Delete(0);
  end;
end;

end.
