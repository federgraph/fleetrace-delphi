unit RiggVar.Web2.Indy;

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
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer,
  IdStack,
  IdGlobal;

type
  TWebRouterIndy = class
  private
    FMimeTableLoaded: Boolean;
    FHost: string;
    FPort: Integer;
    FUrl: string;
    function GetHost: string;
    function GetIP: string;
    function GetIsActive: Boolean;
    procedure MimeTableOnBuildCache(Sender: TObject);
  protected
    HTTPServer: TIdHTTPServer;
    function GetPort: Integer; virtual;
    procedure InitRoot; virtual;
    function GetUrl: string; virtual;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); virtual;
  public
    IsDummy: Boolean;
    dn: string;
    constructor Create;
    destructor Destroy; override;
    function Init: Boolean;
    property Port: Integer read GetPort write FPort;
    property Host: string read GetHost write FHost;
    property IP: string read GetIP;
    property IsActive: Boolean read GetIsActive;
    property Url: string read GetUrl;
  end;

implementation

constructor TWebRouterIndy.Create;
begin
  inherited Create;
  dn := ExtractFilePath(ParamStr(0));
end;

destructor TWebRouterIndy.Destroy;
begin
  if Assigned(HttpServer) then
  begin
    HttpServer.Active := false;
    HttpServer.Free;
  end;
  inherited Destroy;
end;

function TWebRouterIndy.GetHost: string;
begin
  result := FHost;
end;

function TWebRouterIndy.GetPort: Integer;
begin
  result := FPort;
end;

function TWebRouterIndy.GetIP: string;
begin
  result := GStack.ResolveHost(Host, Id_IPv4);
end;

function TWebRouterIndy.GetIsActive: Boolean;
begin
  result := Assigned(HttpServer) and HttpServer.Active;
end;

function TWebRouterIndy.GetUrl: string;
begin
  //HttpServer can be nil if WebMotor was not manually initialized yet...
  if HttpServer = nil then
    result := ''
  else if HttpServer.Active and (FUrl <> '') then
    result := FUrl
  else
    result := 'about:blank';
end;

function TWebRouterIndy.Init: Boolean;
var
  tempIP: string;
begin
  result := false;
  if (IsDummy = false) and (HttpServer = nil) then
  try
    HttpServer := TIdHTTPServer.Create(nil);
    HttpServer.MIMETable.OnBuildCache := MimeTableOnBuildCache;
    HttpServer.KeepAlive := False;
    HttpServer.DefaultPort := Port;
    if HttpServer.Bindings.Count > 1 then
      HttpServer.Bindings.Clear;
    if HttpServer.Bindings.Count = 0 then
      HttpServer.Bindings.Add;
    tempIP := IP;
    HttpServer.Bindings[0].IP := tempIP;
    HttpServer.OnCommandGet := HandleRequest;
    HttpServer.Active := True;
    FUrl := Format('http://%s:%d/', [tempIP,HttpServer.DefaultPort]);
    result := true;
  except
    HttpServer.Free;
    HttpServer := nil;
    IsDummy := True;
  end;
end;

procedure TWebRouterIndy.InitRoot;
begin
  dn := ExtractFilePath(ParamStr(0));
end;

procedure TWebRouterIndy.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  //virtual;
end;

procedure TWebRouterIndy.MimeTableOnBuildCache(Sender: TObject);
begin
  if not FMimeTableLoaded then
  begin
    HttpServer.MIMETable.AddMimeType('.png','x-png');
    HttpServer.MIMETable.AddMimeType('.jpg','x-jpg');
    HttpServer.MIMETable.AddMimeType('.gif','x-gif');
    HttpServer.MIMETable.AddMimeType('.xap','x-application-silverlight-2');
    FMimeTableLoaded := true;
  end;
end;

end.
