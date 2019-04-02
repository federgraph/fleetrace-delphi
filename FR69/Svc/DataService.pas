unit DataService;

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
  Soap.OPConvert;

type
  DataServiceSoap = interface(IInvokable)
  ['{2CA99421-CE2D-4069-B0CB-66BE3B92A405}']
    function  HelloWorld: WideString; stdcall;
    function  TestUTF8(const s: WideString): Boolean; stdcall;
    function  LoadField(const KatID: Integer; const EventName: WideString; const FieldName: WideString): WideString; stdcall;
    function  LoadEventNames(const KatID: Integer; const EventFilter: WideString): WideString; stdcall;
    function  LoadEventData(const KatID: Integer; const EventName: WideString): WideString; stdcall;
    function  SaveEventData(const KatID: Integer; const EventName: WideString; const EventData: WideString; const Password: WideString): Boolean; stdcall;
    function  ExecuteCommand(const KatID: Integer; const EventName: WideString; const CommandName: WideString): Boolean; stdcall;
  end;

function GetDataServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): DataServiceSoap;

var
  DataServerName: string = 'http://localhost/FleetRace/';

implementation

function GetDataServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): DataServiceSoap;
const
  defWSDL = 'FR42_FRService.asmx?wsdl';
  defURL  = 'FR42_FRService.asmx';
  defSvc  = 'FRService';
  defPrt  = 'FRServiceSoap';
var
  RIO: THTTPRIO;
  o: TSoapConvertOptions;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := DataServerName + defWSDL
    else
      Addr := DataServerName + defURL;
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
    Result := (RIO as DataServiceSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(DataServiceSoap), 'http://riggvar.net/data', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(DataServiceSoap), 'http://riggvar.net/data/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(DataServiceSoap), ioDocument);

end.
