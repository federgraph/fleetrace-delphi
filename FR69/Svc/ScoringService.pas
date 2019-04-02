unit ScoringService;

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
  ScoringServiceSoap = interface(IInvokable)
  ['{E92240CA-C3EF-441A-AF7F-AFDFE3A2A30D}']
    function  HelloWorld: WideString; stdcall;
    function  ScoreRegatta(const xml: WideString): WideString; stdcall;
  end;

function GetScoringServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): ScoringServiceSoap;
procedure InitScoringServiceSoap(ServerType: Integer);

var
  ScoringServerName: string = 'http://localhost/FR42/';
  defWSDL: string = 'FR42_ScoringService.asmx?WSDL';
  defURL: string  = 'FR42_ScoringService.asmx';
  defSvc: string  = 'FR42_ScoringService';
  defPrt: string  = 'FR42_ScoringServiceSoap';

const
  ServerType_ASPNET = 0;
  defWSDL_ASPNET = 'FR42_ScoringService.asmx?WSDL';
  defURL_ASPNET  = 'FR42_ScoringService.asmx';
  defSvc_ASPNET  = 'FR42_ScoringService';
  defPrt_ASPNET  = 'FR42_ScoringServiceSoap';

  ServerType_Java = 1;
  defWSDL_Java = 'ScoringService?WSDL';
  defURL_Java  = 'ScoringService';
  defSvc_Java  = 'ScoringService';
  defPrt_Java  = 'FR42_ScoringServiceSoapPort';

implementation

procedure InitScoringServiceSoap(ServerType: Integer);
begin
  if ServerType = ServerType_ASPNET then
  begin
    defWSDL := defWSDL_ASPNET;
    defURL := defURL_ASPNET;
    defSvc := defSvc_ASPNET;
    defPrt := defPrt_ASPNET;
  end
  else if ServerType = ServerType_Java then
  begin
    defWSDL := defWSDL_Java;
    defURL := defURL_Java;
    defSvc := defSvc_Java;
    defPrt := defPrt_Java;
  end;
end;

function GetScoringServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): ScoringServiceSoap;
var
  RIO: THTTPRIO;
  o: TSoapConvertOptions;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := ScoringServerName + defWSDL
    else
      Addr := ScoringServerName + defURL;
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
    Result := (RIO as ScoringServiceSoap);
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
  InvRegistry.RegisterInterface(TypeInfo(ScoringServiceSoap), 'http://riggvar.net/scoring', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(ScoringServiceSoap), 'http://riggvar.net/scoring/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(ScoringServiceSoap), ioDocument);

end.
