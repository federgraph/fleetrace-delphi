unit RiggVar.BR.BridgeREST;

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

{ the client part of a REST Bridge

  * uses TRESTConnection for http transport
}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.BR.BridgeAbstract,
  RiggVar.Conn.REST,
  IdMultipartFormData;

type
  TRESTBridge = class(TBridge)
  private
    Connection: TRESTConnection;
    SL: TStringList;
  public
    constructor Create(EventType: Integer);
    destructor Destroy; override;

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

    function GetServerUrl: string; override;
    procedure SetServerUrl(const Value: string); override;
  end;

implementation

uses
  RiggVar.Util.Classes;

constructor TRESTBridge.Create(EventType: Integer);
begin
  Connection := TRESTConnection.Create(EventType);
  SL := Connection.SL;
end;

destructor TRESTBridge.Destroy;
begin
  SL := nil;
  Connection.Free;
  inherited;
end;

procedure TRESTBridge.SetServerUrl(const Value: string);
begin
  Connection.SetServerUrl(Value);
end;

function TRESTBridge.GetServerUrl: string;
begin
  result := Connection.GetServerUrl;
end;

function TRESTBridge.Plugin: Integer;
var
  s: string;
  i: Integer;
begin
  s := Connection.Get('Plugin');
  i := StrToIntDef(s, -1);
  result := i;
end;

procedure TRESTBridge.Plugout(SwitchID: Integer);
begin
  SL.Clear;
  SL.Add('SwitchID=' + IntToStr(SwitchID));
  Connection.Post('Plugout');
end;

function TRESTBridge.SendBackupAndLog(SwitchID: Integer; Backup,
  Log: string): Integer;
var
  s: string;
  i: Integer;
  M: TIdMultiPartFormDataStream;
begin
  M := TIdMultiPartFormDataStream.Create;
  M.AddFormField('SwitchID', IntToStr(SwitchID));
  M.AddFormField('Backup', Backup);
  M.AddFormField('Log', ''); //Log is always '' (in case of ResultServer)

  s := Connection.MultiLinePost('SendBackupAndLog', M);
  i := StrToIntDef(s, -1);
  result := i;

  M.Free;
end;

procedure TRESTBridge.SendDiffLog(SwitchID: Integer; DiffLog: string);
var
  M: TIdMultiPartFormDataStream;
begin
  M := TIdMultiPartFormDataStream.Create;
  M.AddFormField('SwitchID', IntToStr(SwitchID));
  M.AddFormField('DiffLog', DiffLog);
  Connection.MultiLinePost('SendDiffLog', M);
  M.Free;
end;

procedure TRESTBridge.SendMsg(SwitchID: Integer; msg: string);
begin
  SL.Clear;
  SL.Add('SwitchID=' + IntToStr(SwitchID));
  SL.Add('msg=' + msg);
  Connection.Post('SendMsg');
end;

function TRESTBridge.GetBackup: string;
var
  s: string;
begin
  s := Connection.Get('GetBackup');
  if (s = 'e') then
    result := ''
  else
    result := s;
end;

function TRESTBridge.GetNewMessages(SwitchID, StartMsgID: Integer): string;
var
  s: string;
begin
  SL.Clear;
  SL.Add('SwitchID=' + IntToStr(SwitchID));
  SL.Add('StartMsgID=' + IntToStr(StartMsgID));
  s := Connection.Post('GetNewMessages');
  if (s = 'e') then
    result := ''
  else
    result := s;
end;

function TRESTBridge.LogValid: Boolean;
var
  s: string;
begin
  SL.Clear;
  s := Connection.Get('LogValid');
  result := TUtils.IsTrue(s);
end;

function TRESTBridge.GetBackupSwitchID: Integer;
var
  s: string;
  i: Integer;
begin
  s := Connection.Get('GetBackupSwitchID');
  i := StrToIntDef(s, -1);
  result := i;
end;

function TRESTBridge.GetLastBackupID: Integer;
var
  s: string;
  i: Integer;
begin
  s := Connection.Get('GetLastBackupID');
  i := StrToIntDef(s, -1);
  result := i;
end;

function TRESTBridge.GetLastMsgID: Integer;
var
  s: string;
  i: Integer;
begin
  s := Connection.Get('GetLastMsgID');
  i := StrToIntDef(s, -1);
  result := i;
end;

function TRESTBridge.CheckForBackup(SwitchID,
  StartBackupID: Integer): Boolean;
var
  s: string;
begin
  SL.Clear;
  SL.Add('SwitchID=' + IntToStr(SwitchID));
  SL.Add('StartBackupID=' + IntToStr(StartBackupID));
  s := Connection.Post('CheckForBackup');
  result := TUtils.IsTrue(s);
end;

function TRESTBridge.CheckForLog(SwitchID, StartMsgID: Integer): Boolean;
var
  s: string;
begin
  SL.Clear;
  SL.Add('SwitchID=' + IntToStr(SwitchID));
  SL.Add('StartMsgID=' + IntToStr(StartMsgID));
  s := Connection.Post('CheckForLog');
  result := TUtils.IsTrue(s);
end;

end.
