unit RiggVar.BR.BridgeLocal;

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
  RiggVar.Conn.MsgRingIntf,
  RiggVar.BR.PeerController,
  RiggVar.BR.BridgeAbstract;

type
  TLocalBridge = class(TBridge)
  private
    FDiffLog: TStringList;
  protected
    SLReport: TStringList;
  public
    FNextBridgeID: Integer;
    FBackup: string;
    FLog: string;
    FBackupID: Integer;
    FBackupSwitchID: Integer;
    FBackupMsgID: Integer;
    FMsgRing: TMsgRing;

    constructor Create(Capacity: Integer);
    destructor Destroy; override;

    procedure Clear;
    function GetIDReport: string;
    procedure GetStatusReport(Memo: TStrings); override;
    function GetWebStatusString: string; virtual;
    procedure AddIDReport;

    function Plugin(): Integer; override;
    procedure Plugout(SwitchID: Integer); override;
    //
    function SendBackupAndLog(SwitchID: Integer; Backup: string; Log: string): Integer; override;
    procedure SendDiffLog(SwitchID: Integer; DiffLog: string); override;
    procedure SendMsg(SwitchID: Integer; msg: string); override;
    //
    function GetBackup(): string; override;
    function GetNewMessages(SwitchID: Integer; StartMsgID: Integer): string; override;
    //
    function LogValid(): Boolean; override;
    //
    function GetBackupSwitchID: Integer; override;
    function GetLastBackupID: Integer; override;
    function GetLastMsgID: Integer; override;
    function CheckForBackup(SwitchID: Integer; StartBackupID: Integer): Boolean; override;
    function CheckForLog(SwitchID: Integer; StartMsgID: Integer): Boolean; override;
  end;

implementation

uses
  RiggVar.Conn.MsgRingImpl;

{ TLocalBridge }

constructor TLocalBridge.Create(Capacity: Integer);
begin
  FDiffLog := TStringList.Create;
  FMsgRing := TMsgRing01.Create(Capacity);
  SLReport := TStringList.Create;
end;

destructor TLocalBridge.Destroy;
begin
  FDiffLog.Free;
  FMsgRing.Free;
  SLReport.Free;
  inherited;
end;

function TLocalBridge.Plugin(): Integer;
begin
  Inc(FNextBridgeID);
  result := FNextBridgeID;

  DoOnUpdateUI(UUIAction_SwitchID, IntToStr(FNextBridgeID));
end;

procedure TLocalBridge.Plugout(SwitchID: Integer);
begin
  //do nothing
end;

function TLocalBridge.SendBackupAndLog(SwitchID: Integer; Backup,
  Log: string): Integer;
begin
  FBackup := Backup;
  FLog := Log;
  FMsgRing.Clear;

  Inc(FBackupID);
  FBackupSwitchID := SwitchID;
  FBackupMsgID := FMsgRing.MsgID;
  result := FBackupMsgID;

  DoOnUpdateUI(UUIAction_BackupID, IntToStr(FBackupID));
  DoOnUpdateUI(UUIAction_BackupMsgID, IntToStr(FBackupMsgID));
  DoOnUpdateUI(UUIAction_BackupSwitchID, IntToStr(FBackupSwitchID));
end;

procedure TLocalBridge.SendDiffLog(SwitchID: Integer; DiffLog: string);
var
  i: Integer;
begin
  FDiffLog.Text := DiffLog;
  for i := 0 to FDiffLog.Count - 1 do
  begin
    Assert(FDiffLog[i] <> '');
    Assert(Pos(crlf, FDiffLog[i]) = 0);
    Assert(Pos(#13, FDiffLog[i]) = 0);

    SendMsg(SwitchID, FDiffLog[i]);
  end;
end;

procedure TLocalBridge.SendMsg(SwitchID: Integer; msg: string);
begin
  FMsgRing.AddMsg(SwitchID, msg);

  DoOnUpdateUI(UUIAction_MsgID, IntToStr(FMsgRing.MsgID));
end;

function TLocalBridge.GetBackup: string;
begin
  result := FBackup + crlf + FLog + crlf + GetNewMessages(0,0);
end;

function TLocalBridge.GetBackupSwitchID: Integer;
begin
  result := FBackupSwitchID;
end;

procedure TLocalBridge.GetStatusReport(Memo: TStrings);
begin
  inherited;
  Memo.Add('-- TLocalBridge.StatusReport');
  Memo.Add('Backup.Length = ' + IntToStr(Length(FBackup)));
  Memo.Add('Log.Length = ' + IntToStr(Length(FLog)));
  Memo.Add('DiffLog.Count = ' + IntToStr(FDiffLog.Count));
  Memo.Add('- IDReport');
  Memo.Add('SwitchID: ' + IntToStr(FNextBridgeID));
  Memo.Add('MsgID: ' + IntToStr(FMsgRing.MsgID));
  Memo.Add('BackupID: ' + IntToStr(FBackupID));
  Memo.Add('BackupMsgID: ' + IntToStr(FBackupMsgID));
  Memo.Add('BackupSwitchID: ' + IntToStr(FBackupSwitchID));
end;

procedure TLocalBridge.AddIDReport;
begin
  SLReport.Add('SwitchID: ' + IntToStr(FNextBridgeID));
  SLReport.Add('MsgID: ' + IntToStr(FMsgRing.MsgID));
  SLReport.Add('BackupID: ' + IntToStr(FBackupID));
  SLReport.Add('BackupMsgID: ' + IntToStr(FBackupMsgID));
  SLReport.Add('BackupSwitchID: ' + IntToStr(FBackupSwitchID));
  SLReport.Add('ClassName: ' + ClassName);
end;

function TLocalBridge.GetIDReport: string;
begin
  SLReport.Clear;
  AddIDReport;
  result := SLReport.Text;
  SLReport.Clear;
end;

function TLocalBridge.GetNewMessages(SwitchID: Integer; StartMsgID: Integer): string;
begin
  FDiffLog.Clear;
  FMsgRing.GetDelta(SwitchID, FDiffLog, StartMsgID);
  //result := IntToStr(GetLastMsgID) + crlf + DiffLog.Text;
  result := FDiffLog.Text;
end;

function TLocalBridge.GetLastBackupID: Integer;
begin
  result := FBackupID;
end;

function TLocalBridge.GetLastMsgID: Integer;
begin
  result := FMsgRing.MsgID;
end;

function TLocalBridge.LogValid(): Boolean;
begin
  result := FMsgRing.LogValid();
end;

function TLocalBridge.CheckForBackup(SwitchID: Integer; StartBackupID: Integer): Boolean;
begin
  result := false;
  if (StartBackupID < FBackupID) then
    result := true;
end;

function TLocalBridge.CheckForLog(SwitchID: Integer; StartMsgID: Integer): Boolean;
begin
  //todo: check C# vs Delphi
  //return FMsgRing.GetDiffCount(SwitchID, StartMsgID) > 0; //C#
  //there is no GetDiffCount function here yet

  result := false;

  FDiffLog.Clear;
  FMsgRing.GetDelta(SwitchID, FDiffLog, StartMsgID);
  if FDiffLog.Count > 0 then
    result := true;
end;

procedure TLocalBridge.Clear;
begin
  SLReport.Clear;
  FDiffLog.Clear;
  FNextBridgeID := 0;
  FBackup := '';
  FLog := '';
  FBackupID := 0;
  FBackupSwitchID := 0;
  FBackupMsgID := 0;
  FMsgRing.Clear;

  DoOnUpdateUI(UUIAction_Clear, '');
end;

function TLocalBridge.GetWebStatusString: string;
begin
  result := '';
end;

end.
