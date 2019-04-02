unit BridgeServiceFR;

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
  FRBridgeServiceSoap = interface(IInvokable)
  ['{764F60F4-944E-4C22-A00A-19A579301875}']
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

  TFRBridgeProxy = class(TBridge)
  private
    FHasError: Boolean;
    function Proxy: FRBridgeServiceSoap; virtual;
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

function GetFRBridgeServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): FRBridgeServiceSoap;

var
  ServerName: string = 'http://localhost/FR88/';

implementation

uses
  RiggVar.Util.WebUtils;

function GetFRBridgeServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): FRBridgeServiceSoap;
var
  RIO: THTTPRIO;
  o: TSoapConvertOptions;

begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := ServerName + 'FRBridgeService.asmx?WSDL'
    else
      Addr := ServerName + 'FRBridgeService.asmx';
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
    Result := (RIO as FRBridgeServiceSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := 'FRBridgeService';
      RIO.Port := 'FRBridgeServiceSoap';
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;

{ TFRBridgeProxy }

constructor TFRBridgeProxy.Create;
begin
end;

function TFRBridgeProxy.Proxy : FRBridgeServiceSoap;
begin
  result := GetFRBridgeServiceSoap(False);
end;

function TFRBridgeProxy.Plugin: Integer;
begin
  try
    result := Proxy.Plugin;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

procedure TFRBridgeProxy.Plugout(SwitchID: Integer);
begin
  try
    Proxy.Plugout(SwitchID);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

function TFRBridgeProxy.CheckForBackup(SwitchID, StartBackupID: Integer): Boolean;
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

function TFRBridgeProxy.CheckForLog(SwitchID, StartMsgID: Integer): Boolean;
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

function TFRBridgeProxy.GetBackup: string;
begin
  try
    result := Proxy.GetBackup;
    WebUtils.EnsureCRLF(result);
    FHasError := False;
  except
    FHasError := True;
    result := '';
  end;
end;

function TFRBridgeProxy.GetBackupSwitchID: Integer;
begin
  try
    result := Proxy.GetBackupSwitchID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TFRBridgeProxy.GetLastBackupID: Integer;
begin
  try
    result := Proxy.GetLastBackupID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TFRBridgeProxy.GetLastMsgID: Integer;
begin
  try
    result := Proxy.GetLastMsgID;
    FHasError := False;
  except
    FHasError := True;
    result := -1;
  end;
end;

function TFRBridgeProxy.GetNewMessages(SwitchID, StartMsgID: Integer): string;
begin
  try
    result := Proxy.GetNewMessages(
    SwitchID, StartMsgID);
    WebUtils.EnsureCRLF(result);
    FHasError := False;
  except
    FHasError := True;
    result := '';
  end;
end;

function TFRBridgeProxy.GetServerUrl: string;
begin
  result := BridgeServiceFR.ServerName;
end;

function TFRBridgeProxy.HasError: Boolean;
begin
  result := FHasError;
end;

function TFRBridgeProxy.LogValid(): Boolean;
begin
  try
    result := Proxy.LogValid();
    FHasError := False;
  except
    FHasError := True;
    result := false;
  end;
end;

function TFRBridgeProxy.SendBackupAndLog(SwitchID: Integer; Backup,
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

procedure TFRBridgeProxy.SendDiffLog(SwitchID: Integer; DiffLog: string);
begin
  try
    Proxy.SendDiffLog(SwitchID, DiffLog);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

procedure TFRBridgeProxy.SendMsg(SwitchID: Integer; msg: string);
begin
  try
    Proxy.SendMsg(SwitchID, msg);
    FHasError := False;
  except
    FHasError := True;
  end;
end;

procedure TFRBridgeProxy.SetServerUrl(const Value: string);
begin
  BridgeServiceFR.ServerName := Value;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(FRBridgeServiceSoap), 'http://riggvar.net/bridge/fr', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(FRBridgeServiceSoap), 'http://riggvar.net/bridge/fr/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(FRBridgeServiceSoap), ioDocument);

end.
