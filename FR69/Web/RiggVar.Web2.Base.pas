unit RiggVar.Web2.Base;

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
  System.SyncObjs,
  RiggVar.Util.Classes,
  RiggVar.BO.ResourceManager,
  RiggVar.Web1.Images,
  IdContext,
  IdCustomHTTPServer,
  RiggVar.Web2.Intf,
  RiggVar.Web2.Router,
  RiggVar.Web2.Master;

type
  TWebControllerBase = class(TInterfacedObject, IWebController)
  private
    SiteCSS: string;
    JQueryJS: string;
    function GetHost: string;
    function GetPort: Integer;
    function GetIsActive: Boolean;
    function GetUrl: string;
    function GetPath: string;
    function GetUseResource: Boolean;
    function GetImageManager: TImageManager;
    function GetResourceManager: TResourceManager;
  protected
    HL: TStringList;
    ML: TStringList;
    SL: TStringList;
    Lock: TCriticalSection;
    function ResourcePath(s: string): string;
    function FilePath(s: string): string;
    function NettoPath(s: string): string;
    function Matches(s, t: string): Boolean;
    function MatchesPath(s, t: string): Boolean;
    function StartsWith(s, t: string): Boolean;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); virtual; abstract;
    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    IsOffline: Boolean;
    FPath: string;
    FTemp: string;
    HttpServer: TWebRouter;
    MasterPage: TMasterPage;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function IsOnline: Boolean;
    procedure SetTemp(value: string); virtual;
    function GetVirtualDir: string;
    property Port: Integer read GetPort;
    property Host: string read GetHost;
    property Url: string read GetUrl;
    property Path: string read GetPath write FPath;
    property IsActive: Boolean read GetIsActive;
    property UseResource: Boolean read GetUseResource;
    property ResourceManager: TResourceManager read GetResourceManager;
    property ImageManager: TImageManager read GetImageManager;
  end;

implementation

{ TWebControllerBase }

constructor TWebControllerBase.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
  MasterPage := TMasterPage.Create;
  HL := MasterPage.HL;
  ML := MasterPage.ML;
  SL := MasterPage.SL;
end;

destructor TWebControllerBase.Destroy;
begin
  MasterPage.Free;
  Lock.Free;
  Lock := nil;
  inherited Destroy;
end;

function TWebControllerBase.GetHost: string;
begin
  result := HttpServer.Host;
end;

function TWebControllerBase.GetImageManager: TImageManager;
begin
  result := HttpServer.ImageManager;
end;

function TWebControllerBase.GetIsActive: Boolean;
begin
  result := HttpServer.IsActive;
end;

function TWebControllerBase.GetPath: string;
begin
  if FTemp <> '' then
    result := FTemp
  else
    result := FPath;
end;

function TWebControllerBase.GetPort: Integer;
begin
  result := HttpServer.Port;
end;

function TWebControllerBase.GetResourceManager: TResourceManager;
begin
  result := HttpServer.ResourceManager;
end;

function TWebControllerBase.GetUrl: string;
begin
  //HttpServer can be nil if WebMotor was not manually initialized yet
  if HttpServer = nil then
    result := ''
  else if HttpServer.IsActive and (Path <> '') then
  begin
    result := HttpServer.Url;
    if Length(Path) > 1 then
      result := result + Copy(Path, 2) + '/index';
  end
  else
    result := 'about:blank';
end;

function TWebControllerBase.GetUseResource: Boolean;
begin
  result := HttpServer.UseResource;
end;

function TWebControllerBase.GetVirtualDir: string;
begin
  result := MasterPage.VirtualDir;
end;

procedure TWebControllerBase.Init;
begin
  IsOffline := false;
end;

function TWebControllerBase.IsOnline: Boolean;
begin
  result := not IsOffline;
end;

function TWebControllerBase._AddRef: Integer;
begin
  result := -1;
end;

function TWebControllerBase._Release: Integer;
begin
  result := -1;
end;

procedure TWebControllerBase.SetTemp(value: string);
begin
  FTemp := value;
end;

function TWebControllerBase.StartsWith(s, t: string): Boolean;
begin
  if Path = '/' then
    result := TUtils.StartsWith(s, t)
  else
  result := TUtils.StartsWith(s, Path + t);
end;

function TWebControllerBase.MatchesPath(s, t: string): Boolean;
var
  i, j: Integer;
begin
  if Path = '/' then
    result := TUtils.StartsWith(s, t)
  else
  begin
    result := TUtils.StartsWith(s, Path + t);
    i := Length(s);
    j := Length(Path + t);
    if (i > j) then
    begin
      //then it must have query string
      if LastDelimiter('?', s) <= j then
        result := false; //query string expected
    end;
    //no subdirectory is allowed
    if LastDelimiter('/', s) > LastDelimiter('/', Path + t) then
      result := false
  end;
end;

function TWebControllerBase.Matches(s, t: string): Boolean;
begin
  //result := s = Path + t;
  result := CompareText(s, Path + t) = 0;
end;

function TWebControllerBase.NettoPath(s: string): string;
begin
  if Path = '/' then
    result := s
  else
  result := Copy(s, 1 + Length(Path));
end;

function TWebControllerBase.FilePath(s: string): string;
var
  t: string;
begin
  t := NettoPath(s);
  t := StringReplace(t, '/', '\', [rfReplaceAll]);
  result := ExcludeTrailingPathDelimiter(HttpServer.dn) + t;
end;

function TWebControllerBase.ResourcePath(s: string): string;
begin
  result := NettoPath(s);
  result := StringReplace(result, '/', '\', [rfReplaceAll]);
end;

procedure TWebControllerBase.HandleCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
begin
  s := LowerCase(ARequestInfo.Document);

  if StartsWith(s, '/docroot/') then
  begin
    s := FilePath(s);
    if FileExists(s) then
      AResponseInfo.ServeFile(AContext, s);
    Exit;
  end;

  if StartsWith(s, '/images/') then
  begin
    if ImageManager.UseResources then
      ImageManager.ServeImage(AContext, AResponseInfo, NettoPath(s))
    else
      ImageManager.ServeImage(AContext, AResponseInfo, ResourcePath(s));
    Exit;
  end;

  if Matches(s, '/stylesheets/site.css') then
  begin
    if SiteCSS = '' then
    begin
      if UseResource then
        SiteCSS := ResourceManager.LoadText(site_css)
      else
        SiteCSS := ResourceManager.LoadFile(site_css);
    end;
    AResponseInfo.ContentType := 'text/css; charset=utf-8';
    AResponseInfo.ContentText := SiteCSS;
  end
  else if Matches(s, '/javascripts/jquery.js') then
  begin
    if JQueryJS = '' then
    begin
      if UseResource then
        JQueryJS := ResourceManager.LoadText(jquery_js)
      else
        JQueryJS := ResourceManager.LoadFile(jquery_js);
    end;
    AResponseInfo.ContentText := JQueryJS;
  end
  else
  begin
    if ARequestInfo.UserAgent = 'FR62' then
      MasterPage.EmbeddedFlag := true
    else
      MasterPage.EmbeddedFlag := false;
    HandleRequest(AContext, ARequestInfo, AResponseInfo);
  end;
end;

end.
