unit BridgeServiceSKK;

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
  SKKBridgeServiceSoap = interface(IInvokable)
  ['{1EE98678-4AD4-4274-A90C-456A02C354A8}']
    function  HelloWorld: WideString; stdcall;
    function  Plugin: Integer; stdcall;
    procedure Plugout(const SwitchID: Integer); stdcall;
    function  SendBackupAndLog(const SwitchID: Integer; const Backup: WideString; const Log: WideString): Integer; stdcall;
    procedure SendDiffLog(const SwitchID: Integer; const DiffLog: WideString); stdcall;
    procedure SendMsg(const SwitchID: Integer; const msg: WideString); stdcall;
    function  GetBackup: WideString; stdcall;
    function  GetNewMessages(const SwitchID: Integer; const StartMsgID: Integer): WideString; stdcall;
    function  LogValid(): Boolean; stdcall;
    function  GetBackupSwitchID: Integer; stdcall;
    function  GetLastBackupID: Integer; stdcall;
    function  GetLastMsgID: Integer; stdcall;
    function  CheckForBackup(const SwitchID: Integer; const StartBackupID: Integer): Boolean; stdcall;
    function  CheckForLog(const SwitchID: Integer; const StartMsgID: Integer): Boolean; stdcall;
  end;

  TSKKBridgeProxy = class(TBridge)
  private
    FHasError: Boolean;
    function Proxy: SKKBridgeServiceSoap; virtual;
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

function GetSKKBridgeServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): SKKBridgeServiceSoap;

var
  ServerName: string = 'http://localhost/FR88/';

implementation

uses
  RiggVar.Util.WebUtils;

function GetSKKBridgeServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): SKKBridgeServiceSoap;
var
  RIO: THTTPRIO;
  o: TSoapConvertOptions;

begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := ServerName + 'SKKBridgeService.asmx?WSDL'
    else
      Addr := ServerName + 'SKKBridgeService.asmx';
  end;
  if HTTPRIO = nil then
  begin
    RIO := THTTPRIO.Create(nil);
    o := RIO.Converter.Options;
    //Exclude(o, soUTF8InHeader);
    RIO.Converter.Options := o;
    RIO.HTTPWebNode.UseUTF8InHeader := True;
  end
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as SKKBridgeServiceSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := 'SKKBridgeService';
      RIO.Port := 'SKKBridgeServiceSoap';
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;

{ TSKKBridgeProxy }

constructor TSKKBridgeProxy.Create;
begin
end;

function TSKKBridgeProxy.Proxy : SKKBridgeServiceSoap;
begin
  result := GetSKKBridgeServiceSoap(False);
end;

function TSKKBridgeProxy.Plugin: Integer;
begin
  try
    result := Proxy.Plugin;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

procedure TSKKBridgeProxy.Plugout(SwitchID: Integer);
begin
  try
    Proxy.Plugout(SwitchID);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

function TSKKBridgeProxy.CheckForBackup(SwitchID, StartBackupID: Integer): Boolean;
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

function TSKKBridgeProxy.CheckForLog(SwitchID, StartMsgID: Integer): Boolean;
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

function TSKKBridgeProxy.GetBackup: string;
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

function TSKKBridgeProxy.GetBackupSwitchID: Integer;
begin
  try
    result := Proxy.GetBackupSwitchID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TSKKBridgeProxy.GetLastBackupID: Integer;
begin
  try
    result := Proxy.GetLastBackupID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TSKKBridgeProxy.GetLastMsgID: Integer;
begin
  try
    result := Proxy.GetLastMsgID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TSKKBridgeProxy.GetNewMessages(SwitchID, StartMsgID: Integer): string;
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

function TSKKBridgeProxy.GetServerUrl: string;
begin
  result := BridgeServiceSKK.ServerName;
end;

function TSKKBridgeProxy.HasError: Boolean;
begin
  result := FHasError;
end;

function TSKKBridgeProxy.LogValid(): Boolean;
begin
  try
    result := Proxy.LogValid();
    FHasError := False;
  except
    FHasError := True;
    result := false;
  end;
end;

function TSKKBridgeProxy.SendBackupAndLog(SwitchID: Integer; Backup,
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

procedure TSKKBridgeProxy.SendDiffLog(SwitchID: Integer; DiffLog: string);
begin
  try
    Proxy.SendDiffLog(SwitchID, DiffLog);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

procedure TSKKBridgeProxy.SendMsg(SwitchID: Integer; msg: string);
begin
  try
    Proxy.SendMsg(SwitchID, msg);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

procedure TSKKBridgeProxy.SetServerUrl(const Value: string);
begin
  BridgeServiceSKK.ServerName := Value;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(SKKBridgeServiceSoap), 'http://riggvar.net/bridge/skk', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(SKKBridgeServiceSoap), 'http://riggvar.net/bridge/skk/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(SKKBridgeServiceSoap), ioDocument);

end.
