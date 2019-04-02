unit WorkspaceService;

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
  WorkspaceFilesSoap = interface(IInvokable)
  ['{C9FD3ECD-28E3-1160-6D67-9F1088F37ECE}']
    function  HelloWorld: WideString; stdcall;
    function  DBFileExists(const WorkspaceID: Integer; const fn: WideString): Boolean; stdcall;
    function  DBDirectoryExists(const WorkspaceID: Integer; const dn: WideString): Boolean; stdcall;
    function  DBGetEventNames(const WorkspaceID: Integer; const ExtensionFilter: WideString): WideString; stdcall;
    function  DBLoadFromFile(const WorkspaceID: Integer; const fn: WideString): WideString; stdcall;
    procedure DBSaveToFile(const WorkspaceID: Integer; const fn: WideString; const data: WideString); stdcall;
    function  DBDeleteFile(const WorkspaceID: Integer; const fn: WideString): Boolean; stdcall;
    function  DBDeleteWorkspace(const WorkspaceID: Integer): Boolean; stdcall;
  end;

function GetWorkspaceFilesSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): WorkspaceFilesSoap;

var
  WorkspaceServerName: string = 'http://localhost/Workspace/';

implementation

function GetWorkspaceFilesSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): WorkspaceFilesSoap;
const
  defWSDL = 'WorkspaceFiles.asmx?WSDL';
  defURL  = 'WorkspaceFiles.asmx';
  defSvc  = 'WorkspaceFiles';
  defPrt  = 'WorkspaceFilesSoap';
var
  RIO: THTTPRIO;
  o: TSoapConvertOptions;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := WorkspaceServerName + defWSDL
    else
      Addr := WorkspaceServerName + defURL;
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
    Result := (RIO as WorkspaceFilesSoap);
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
  InvRegistry.RegisterInterface(TypeInfo(WorkspaceFilesSoap), 'http://riggvar.net/workspace', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(WorkspaceFilesSoap), 'http://riggvar.net/workspace/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(WorkspaceFilesSoap), ioDocument);

end.
