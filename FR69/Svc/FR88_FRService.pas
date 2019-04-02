unit FR88_FRService;

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
  System.Types,
  Soap.InvokeRegistry,
  Soap.SOAPHTTPClient,
  Soap.XSBuiltIns,
  Soap.OPConvert,
  RiggVar.BR.BridgeAbstract;

type
  BridgeServicePortType = interface(IInvokable)
  ['{11B6591F-05FE-44E7-B10B-8406ED3F580F}']
    function  HelloWorld: WideString; stdcall;
    function  Plugin: Integer; stdcall;
    procedure Plugout(const SwitchID: Integer); stdcall;
    function  SendBackupAndLog(const SwitchID: Integer; const Backup: WideString; const Log: WideString): Integer; stdcall;
    procedure SendDiffLog(const SwitchID: Integer; const DiffLog: WideString); stdcall;
    procedure SendMsg(const SwitchID: Integer; const msg: WideString); stdcall;
    function  GetBackup: WideString; stdcall;
    function  GetNewMessages(const SwitchID: Integer; const StartMsgID: Integer): WideString; stdcall;
    function  LogValid: Boolean; stdcall;
    function  GetBackupSwitchID: Integer; stdcall;
    function  GetLastBackupID: Integer; stdcall;
    function  GetLastMsgID: Integer; stdcall;
    function  CheckForBackup(const SwitchID: Integer; const StartBackupID: Integer): Boolean; stdcall;
    function  CheckForLog(const SwitchID: Integer; const StartMsgID: Integer): Boolean; stdcall;
  end;

  TFR88BridgeProxyFR = class(TBridge)
  private
    FHasError: Boolean;
    function Proxy: BridgeServicePortType; virtual;
  public
    constructor Create;
    //
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

    function GetServerUrl: string; override;
    procedure SetServerUrl(const Value: string); override;
    function HasError: Boolean; override;
  end;

function GetBridgeServicePortType(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): BridgeServicePortType;

var
  ServerName: string = 'http://localhost:3569/';

implementation

uses
  RiggVar.Util.WebUtils;

function GetBridgeServicePortType(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): BridgeServicePortType;
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := ServerName + 'FR88_FRService.php?wsdl'
    else
      Addr := ServerName + 'FR88_FRService.php';
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as BridgeServicePortType);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := 'BridgeService';
      RIO.Port := 'BridgeServicePort';
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;

{ TFRBridgeProxy }

constructor TFR88BridgeProxyFR.Create;
begin
end;

function TFR88BridgeProxyFR.Proxy : BridgeServicePortType;
begin
  result := GetBridgeServicePortType(False);
end;

function TFR88BridgeProxyFR.Plugin: Integer;
begin
  try
    result := Proxy.Plugin;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

procedure TFR88BridgeProxyFR.Plugout(SwitchID: Integer);
begin
  try
    Proxy.Plugout(SwitchID);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

function TFR88BridgeProxyFR.CheckForBackup(SwitchID, StartBackupID: Integer): Boolean;
begin
  try
    result := Proxy.CheckForBackup(
    SwitchID, StartBackupID);
    FHasError := False;
  except
    FHasError := True;
    result := false;
  end;
end;

function TFR88BridgeProxyFR.CheckForLog(SwitchID, StartMsgID: Integer): Boolean;
begin
  try
    result := Proxy.CheckForLog(
    SwitchID, StartMsgID);
    FHasError := False;
  except
    FHasError := True;
    result := false;
  end;
end;

function TFR88BridgeProxyFR.GetBackup: string;
begin
  try
    result := Proxy.GetBackup;
    result := WebUtils.EnsureCRLF(result);
    FHasError := False;
  except
    FHasError := True;
    result := '';
  end;
end;

function TFR88BridgeProxyFR.GetBackupSwitchID: Integer;
begin
  try
    result := Proxy.GetBackupSwitchID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TFR88BridgeProxyFR.GetLastBackupID: Integer;
begin
  try
    result := Proxy.GetLastBackupID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TFR88BridgeProxyFR.GetLastMsgID: Integer;
begin
  try
    result := Proxy.GetLastMsgID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TFR88BridgeProxyFR.GetNewMessages(SwitchID, StartMsgID: Integer): string;
begin
  try
    result := Proxy.GetNewMessages(
    SwitchID, StartMsgID);
    result := WebUtils.EnsureCRLF(result);
    FHasError := False;
  except
    FHasError := True;
    result := '';
  end;
end;

function TFR88BridgeProxyFR.GetServerUrl: string;
begin
  result := ServerName;
end;

function TFR88BridgeProxyFR.HasError: Boolean;
begin
  result := FHasError;
end;

function TFR88BridgeProxyFR.LogValid(): Boolean;
begin
  try
    result := Proxy.LogValid();
    FHasError := False;
  except
    FHasError := True;
    result := false;
  end;
end;

function TFR88BridgeProxyFR.SendBackupAndLog(SwitchID: Integer; Backup,
  Log: string): Integer;
begin
  try
    result := Proxy.SendBackupAndLog(
    SwitchID, Backup, Log);
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

procedure TFR88BridgeProxyFR.SendDiffLog(SwitchID: Integer; DiffLog: string);
begin
  try
    Proxy.SendDiffLog(SwitchID, DiffLog);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

procedure TFR88BridgeProxyFR.SendMsg(SwitchID: Integer; msg: string);
begin
  try
    Proxy.SendMsg(SwitchID, msg);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

procedure TFR88BridgeProxyFR.SetServerUrl(const Value: string);
begin
  FR88_FRService.ServerName := Value;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(BridgeServicePortType), 'http://riggvar.net/fr88/bridge/fr', 'ISO-8859-1');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(BridgeServicePortType), 'http://riggvar.net/fr88/bridge/skk/%operationName%');

end.
