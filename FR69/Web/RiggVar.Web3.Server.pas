unit RiggVar.Web3.Server;

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
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  IdContext,
  IdCustomHTTPServer,
  RiggVar.BO.ResourceManager,
  RiggVar.Util.Classes,
  RiggVar.Web3.EventArgs,
  RiggVar.Web3.EventMenu,
  RiggVar.Web3.FeatureMap,
  RiggVar.Web2.Intf,
  RiggVar.Web2.Base;

type
  TSilverlightWeb = class(TWebControllerBase)
  private
    FeatureMap: TFeatureMap;
    EventMenuXml: TWorkspaceEventMenu;
    DocrootEventMenu: TDocrootEventMenu;
    function GetInitParams: string;
    procedure ServeXap(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function GetXapFileName(s, FRXX: string; DefaultNN: Integer): string;
    procedure InitPlainHostPage;
    procedure InitStyledHostPage;
    procedure InitNormalHostPage;
    procedure InitTemplatedHostPage;
    function WriteMenu: string;
  protected
    EA: TWebEventArgs;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    procedure GetAnswer;
  public
    UseFRIA: Boolean;
    ResourceManager: TResourceManager;
    OfflineMsg: string;
    SkipSync: Boolean;
    constructor Create;
    destructor Destroy; override;
    function MakeAbsoluteUrl(ResourcePath: string): string;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

constructor TSilverlightWeb.Create;
begin
  inherited Create;
  UseFRIA := true;
  OfflineMsg := 'offline';
  EA := TWebEventArgs.Create;
  FeatureMap := TFeatureMap.Create;
  EventMenuXml := TWorkspaceEventMenu.Create;
  DocrootEventMenu := TDocrootEventMenu.Create;
end;

destructor TSilverlightWeb.Destroy;
begin
  EA.Free;
  FeatureMap.Free;
  EventMenuXml.Free;
  DocrootEventMenu.Free;
  inherited Destroy;
end;

procedure TSilverlightWeb.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  page: TPageEnum;
  doc: string;
  filter: string;
  key: string;
  fn: string;
  wsid: Integer;
begin
  s := ARequestInfo.Document;

  if IsOffline then
  begin
    AResponseInfo.ContentText := OfflineMsg;
    Exit;
  end;

  if Pos('.xap', s) > 1 then
  begin
    ServeXap(AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;

  wsid := -1;
  doc := '';
  filter := '';
  fn := '';
  key := '';

  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  AResponseInfo.CacheControl := 'no-cache';

  if Pos('FRIAS', UpperCase(s)) > 1 then
  begin
    page := pageFullHost;
    key := 'FRIAS'
  end

  else if Pos('RGG14', UpperCase(s)) > 1 then
  begin
    page := pageNormalHost;
    key := 'Rgg14';
  end

  else if Pos('SKK74', UpperCase(s)) > 1 then
  begin
    page := pageNormalHost;
    key := 'SKK74';
  end

  else if Pos('SKK75', UpperCase(s)) > 1 then
  begin
    page := pageNormalHost;
    key := 'SKK75';
  end

  else if Pos('FRFG', UpperCase(s)) > 1 then
  begin
    page := pageNormalHost;
    key := 'FRFG01'; //default
    if Pos('FRFG01', s) > 0 then key := 'FRFG01'
    else if Pos('FRFG02', s) > 0 then key := 'FRFG02'
  end

  else if Pos('FRIA73', UpperCase(s)) > 1 then
  begin
    page := pageFullHost;
    wsid := 73;
    key := 'FRIA73';
  end

  else if Pos('FRIA', UpperCase(s)) > 1 then
  begin
    page := pageFullHost;
    wsid := 7;
    if Pos('FRIA00', s) > 0 then wsid := 0
    else if Pos('FRIA01', s) > 0 then wsid := 1
    else if Pos('FRIA02', s) > 0 then wsid := 2
    else if Pos('FRIA03', s) > 0 then wsid := 3
    else if Pos('FRIA04', s) > 0 then wsid := 4
    else if Pos('FRIA05', s) > 0 then wsid := 5
    else if Pos('FRIA06', s) > 0 then wsid := 6
    else if Pos('FRIA07', s) > 0 then wsid := 7
    else if Pos('FRIA08', s) > 0 then wsid := 8
    else if Pos('FRIA09', s) > 0 then wsid := 9
    else if Pos('FRIA10', s) > 0 then wsid := 10
    else if Pos('FRIA11', s) > 0 then wsid := 11
    else if Pos('FRIA73', s) > 0 then wsid := 73;
    if UseFRIA then
      key := 'FRIA'
    else
      key := Format('FRIA%.2d', [wsid]);
  end

  else if Matches(LowerCase(s), '/index') then
  begin
    page := pageIndex;
  end

  else if Pos('/clientaccesspolicy.xml', s) = 1 then
  begin
    s := HttpServer.dn + 'ClientAccessPolicy.xml';
    AResponseInfo.ContentType := 'text/xml';
    if FileExists(s) then
    begin
      AResponseInfo.ServeFile(AContext, s);
      Exit;
    end
    else
    begin
      page := pagePolicy;
    end;
  end

  else if Pos('FeatureMap', s) > 1  then
  begin
    AResponseInfo.ContentType := 'text/xml';
    page := pageFeatureMap;
  end

  else if Pos('TestResults', s) > 1  then
  begin
    Lock.Acquire;
    try
      DocrootEventMenu.Root := MakeAbsoluteUrl('/docroot/Results/');
      AResponseInfo.ContentText := DocrootEventMenu.Text;
      AResponseInfo.CharSet := 'utf-8';
    finally
      Lock.Release;
    end;
    exit;
  end

  else if Pos('EventMenu/', s) > 1  then
  begin
    page := pageIndex;
    if Matches(s, '/EventMenu/Xml')  then
    begin
      AResponseInfo.ContentType := 'text/xml';
      page := pageEventMenuXml;
    end
    else if Pos('EventMenu/Data/', s) > 1 then
    begin
      wsid := Pos('/EventMenu/Data/', s);
      key := Copy(s, wsid + Length('/EventMenu/Data/'));
      wsid := StrToIntDef(key, -1);
      page := pageEventMenuData;
    end
    else if Pos('EventMenu/Img/', s) > 1 then
    begin
      wsid := Pos('/EventMenu/Img/', s);
      key := Copy(s, wsid + Length('/EventMenu/Img/'));
      page := pageEventMenuImg;
    end
  end

  else if Matches(s, '/FileExists') then
  begin
    page := pageFileExists;
  end
  else if Matches(s, '/DirectoryExists') then
  begin
    page := pageDirectoryExists;
    key := ARequestInfo.Params.Values['EventName'];
  end
  else if Matches(s, '/GetEventNames') then
  begin
    page := pageGetEventNames;
    wsid := StrToIntDef(ARequestInfo.Params.Values['wsid'], wsid);
  end
  else if Matches(s, '/LoadFromFile') then
  begin
    page := pageLoadFromFile;
    wsid := StrToIntDef(ARequestInfo.Params.Values['wsid'], wsid);
    fn := ARequestInfo.Params.Values['fn'];
  end
  else if Matches(s, '/SaveToFile') then
  begin
    page := pageSaveToFile;
    wsid := StrToIntDef(ARequestInfo.Params.Values['wsid'], wsid);
    fn := ARequestInfo.Params.Values['fn'];
  end
  else if (Matches(s, '/') or Matches(s, '/index')) then
  begin
    page := pageIndex;
  end
  else
  begin
    AResponseInfo.Redirect(Path + '/Index');
    Exit;
  end;

  Lock.Acquire;
  try
    EA._page := page;
    EA._doc := doc;
    EA._filter := filter;
    EA._wsid := wsid;
    EA._key := key;
    EA._fn := fn;

    if SkipSync then
    begin
      SkipSync := false;
      GetAnswer;
    end
    else
    begin
      TThread.Synchronize(nil, GetAnswer);
    end;

    AResponseInfo.ContentText := EA._answer;
  finally
    Lock.Release;
  end;
end;

procedure TSilverlightWeb.GetAnswer;
begin
  HL.Clear;
  ML.Clear;
  SL.Clear;
  HL.Add('<title>FR Silverlight</title>');

  EA._answer := '-';
  case EA._page of

    pageNormalHost:
    begin
      if MasterPage.IsPlain then
        InitNormalHostPage
      else if MasterPage.UseTemplate then
        InitTemplatedHostPage
      else
        case Main.IniImage.WebsiteTheme of
          1: InitNormalHostPage;
        else
          InitTemplatedHostPage;
        end;

      EA._answer := MasterPage.Text;
    end;

    pageFullHost:
    begin
      if MasterPage.IsPlain then
        InitPlainHostPage
      else if MasterPage.UseTemplate then
        InitTemplatedHostPage
      else
        case Main.IniImage.WebsiteTheme of
          1: InitStyledHostPage;
        else
          InitTemplatedHostPage;
        end;
      EA._answer := MasterPage.Text;
    end;

    pageIndex:
    begin
      WriteMenu;

      SL.Add('<h2>/index</h2>');

      SL.Add('<h4>FRFG</h4>');
      SL.Add('<p>');
      SL.Add('<a href="FRFG01.htm">01</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRFG02.htm">02</a>');
      SL.Add('</p>');

      SL.Add('<h4>FRIA</h4>');
      SL.Add('<p>');
      if UseFRIA then
      begin
        SL.Add('<a href="FRIA00.htm">00</a>&nbsp;|&nbsp;');
      end;
      SL.Add('<a href="FRIA01.htm">01</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA02.htm">02</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA03.htm">03</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA04.htm">04</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA05.htm">05</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA06.htm">06</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA07.htm">07</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA08.htm">08</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA09.htm">09</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA10.htm">10</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA11.htm">11</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FRIA73.htm">73</a>');
      SL.Add('</p>');

      SL.Add('<h4>XML</h4>');
      SL.Add('<p>');
      SL.Add('<a href="FRIAS.htm">Applet</a>&nbsp;|&nbsp;');
      SL.Add('<a href="FeatureMap/Xml">FeatureMap</a>&nbsp;|&nbsp;');
      SL.Add('<a href="EventMenu/Xml">EventMenu</a>');
      SL.Add('</p>');
      EA._answer := MasterPage.Text;
    end;

    pagePolicy:
    begin
      SL.Add('<?xml version="1.0" encoding="utf-8"?>');
      SL.Add('<access-policy><cross-domain-access><policy>');
      SL.Add('<allow-from>');
      if Main.IsScenarioEC2 then
      begin
        SL.Add('<domain uri="http://www.riggvar.de"/>');
        SL.Add('<domain uri="http://silverlight.riggvar.de"/>');
      end
      else
        SL.Add('<domain uri="*"/>');
      SL.Add('</allow-from>');
      SL.Add('<grant-to>');

      SL.Add('<socket-resource port="4530" protocol="tcp"/>');
      SL.Add('<resource path="/" include-subpaths="true"/>');

      SL.Add('</grant-to>');
      SL.Add('</policy></cross-domain-access></access-policy>');
      EA._answer := SL.Text;
    end;

    pageGetEventNames:
    begin
      if EA._wsid = -1 then
      begin
        EA._answer := 'workspace id not specified';
      end
      else
      begin
        Main.DocManager.FillEventNameList(SL);
        EA._answer := SL.Text;
      end;
    end;

    pageLoadFromFile:
    begin
      if (EA._wsid = -1) then
      begin
        EA._answer := 'workspace id not specified';
      end
      else if EA._fn = '' then
      begin
        EA._answer := 'file name is empty';
      end
      else
      begin
        EA._answer := Main.DocManager.DownloadByName(EA._fn);
      end;
    end;

    pageFeatureMap:
    begin
      EA._answer := FeatureMap.Xml;
    end;

    pageEventMenuXml:
    begin
      EventMenuXml.DataUrl := MakeAbsoluteUrl('/EventMenu/Data/');
      EventMenuXml.ImgUrl := MakeAbsoluteUrl('/EventMenu/Img/');
      EA._answer := EventMenuXml.Text;
    end;

    pageEventMenuData:
    begin
      if EA._wsid = 0 then
      begin
        EA._answer := BO.ToXML;
      end
      else
      begin
        Main.DocManager.FillEventNameList(SL);
        if (EA._wsid-1 >= 0) and (EA._wsid-1 < SL.Count) then
          EA._answer := Main.DocManager.DocDownloadByName(SL[EA._wsid-1])
        else
          EA._answer := '/EventMenu/Data/ID (id is out of range)';
      end;
    end;

    pageEventMenuImg:
    begin
      EA._answer := 'not implemented';
    end;

  end;

  HL.Clear;
  ML.Clear;
  SL.Clear;
end;

function TSilverlightWeb.GetInitParams: string;
var
  BridgeHost: string;
  BridgePort: Integer;
  MenuLocation: string;
  s: string;
  XapType: Integer;
begin
  XapType := EA._wsid;
  BridgeHost := Main.IniImage.BridgeHost;
  BridgePort := Main.IniImage.BridgePort;
  if Main.Params.UseProxyBase then
  begin
    BridgeHost := Main.IniImage.ProxyDomain; //'riggvar.homeserver.com';
    MenuLocation := MakeAbsoluteUrl('/EventMenu/Xml')
  end
  else if Main.Params.WantDocrootResults then
    MenuLocation := MakeAbsoluteUrl('/TestResults/EventMenu.xml')
  else
    MenuLocation := MakeAbsoluteUrl('/EventMenu/Xml');
  if UseFRIA then
    s := Format('at=%d,dock=xy,', [XapType])
  else
    s := 'dock=xy,';
  s := s + Format('BridgeHost=%s,', [BridgeHost]);
  s := s + Format('BridgePort=%d,', [BridgePort]);
  s := s + Format('menuLocation=%s', [MenuLocation]);
  result := s;
end;

function TSilverlightWeb.MakeAbsoluteUrl(ResourcePath: string): string;
var
  s: string;
begin
  if Main.Params.UseProxyBase then
  begin
    if Main.IsScenarioEC2 then
      s := Format('%s:%s/',
      [Main.IniImage.HomeProxyAuthority,
      IntToStr(Main.IniImage.WebServerHomePort)])
    else
      s := Main.IniImage.HomeProxyAuthority +  Main.IniImage.HomeProxyBase;
    s := s + 'silverlight/index';
    result := StringReplace(s, '/index', ResourcePath, []);
  end
  else
  begin
    result := StringReplace(Url, '/index', ResourcePath, []);
  end;
end;

procedure TSilverlightWeb.ServeXap(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  fn: string;
begin
  s := ARequestInfo.Document;
  fn := '';
  if Pos('FRFG', s) > 1 then
    fn := GetXapFileName(s, 'FRFG', 2)
  else if Pos('FRIA73', s) > 1 then
    fn := Format('%sClientBin\FRIA73.xap', [HttpServer.dn])
  else if Pos('FRIA', s) > 1 then
  begin
    if UseFRIA then
      fn := Format('%sClientBin\FRIA.xap', [HttpServer.dn])
    else
      fn := GetXapFileName(s, 'FRIA', 7)
  end
  else if Pos('Rgg14', s) > 1 then
    fn := Format('%sClientBin\Rgg14.xap', [HttpServer.dn])
  else if Pos('SKK74', s) > 1 then
    fn := Format('%sClientBin\SKK74.xap', [HttpServer.dn])
  else if Pos('SKK75', s) > 1 then
    fn := Format('%sClientBin\SKK75.xap', [HttpServer.dn]);

  if FileExists(fn) then
    AResponseInfo.ServeFile(AContext, fn);
end;

function TSilverlightWeb.GetXapFileName(s: string; FRXX: string; DefaultNN: Integer): string;
var
  p: Integer;
  key: string;
  nn: Integer;
begin
  //1234567890
  //FRXXnn.xap
  p := Pos('.xap', s) - Pos(FRXX, s);
  if p = 6 then
  begin
    p := Pos(FRXX, s);
    key := Copy(s, p + Length(FRXX), 2);
    nn := StrToIntDef(key, DefaultNN);
    result := Format('%sClientBin\%s%.2d.xap', [HttpServer.dn, FRXX, nn]);
  end;
end;

function TSilverlightWeb.WriteMenu: string;
begin
  SL.Add('<p id="menu">');
  SL.Add('<a href="FRIA05.htm">EventBrowser</a>&nbsp;|&nbsp;');
  SL.Add('<a href="FRIA03.htm">RemoteControl</a>&nbsp;|&nbsp;');
  SL.Add('<a href="FRIA11.htm">RaceMonitor</a>');
  SL.Add('</p>');
end;

procedure TSilverlightWeb.InitNormalHostPage;
begin
  HL.Add('<style type="text/css">');
  HL.Add('html, body {height: 100%;overflow: hidden;}');
  HL.Add('#silverlightControlHost {height: 100%;overflow: hidden;}');
  HL.Add('</style>');

  WriteMenu;

  SL.Add('<div id="silverlightControlHost">');
  SL.Add('<object type="application/x-silverlight-2" width="100%" height="600">');

  SL.Add(Format('<param name="source" value="%s"/>', [EA._key + '.xap']));
  SL.Add('<param name="background" value="white" />');
  SL.Add('<param name="minRuntimeVersion" value="2.0.31005.0" />');
  SL.Add('<param name="autoUpgrade" value="true" />');
  SL.Add(Format('<param name="initParams" value="%s" />', [GetInitParams]));

  SL.Add('<a href="http://go.microsoft.com/fwlink/?LinkID=124807" style="text-decoration: none;">');
  SL.Add('<img src="http://go.microsoft.com/fwlink/?LinkId=108181" alt="Get Microsoft Silverlight" style="border-style: none"/>');
  SL.Add('</a>');
  SL.Add('</object>');

  SL.Add('<iframe style=''visibility:hidden;height:0;width:0;border:0px''></iframe></div>');
end;

procedure TSilverlightWeb.InitPlainHostPage;
begin
  HL.Add('<script src="/javascripts/jquery.js" type="text/javascript"></script>');
  HL.Add('<style type="text/css">');
  HL.Add('html, body {height:100%;margin:0;padding:0}');
  HL.Add('</style>');
  HL.Add('<script type="text/javascript" id="SilverlightResizeScript">');
  HL.Add('$(pageReady);');
  HL.Add('function pageReady() {');
  HL.Add('$(window).resize(onWindowResize);');
  HL.Add('};');
  HL.Add('function onWindowResize() {');
  HL.Add('var b = $("body");');
  HL.Add('var d = $("#silverlightControlHost");');
  HL.Add('d.height(b.height() - d.offset().top);');
  HL.Add('d.width(b.width() - d.offset().left);');
  HL.Add('}');
  HL.Add('</script>');
  SL.Add('<div id="silverlightControlHost">');
  SL.Add('<object data="data:application/x-silverlight-2," type="application/x-silverlight-2" width="100%" height="100%">');
  SL.Add(Format('<param name="source" value="%s"/>', [EA._key + '.xap']));

  SL.Add('<param name="minRuntimeVersion" value="4.0.50401.0" />');

  SL.Add(Format('<param name="initParams" value="%s" />', [GetInitParams]));
  SL.Add('</object></div>');
end;

procedure TSilverlightWeb.InitStyledHostPage;
begin
  HL.Add('<script src="javascripts/jquery.js" type="text/javascript"></script>');

  HL.Add('<style type="text/css">');
  HL.Add('html, body, .page {height:100%;overflow:hidden;}');
  HL.Add('#main {height:100%;}');
  HL.Add('#silverlightControlHost {');
  HL.Add('height:100%;width:100%;');
  HL.Add('margin:-10px;');
  HL.Add('position:absolute;');
  HL.Add('}');
  HL.Add('</style>');

  HL.Add('<script type="text/javascript" id="SilverlightResizeScript">');
  HL.Add('$(pageReady);');
  HL.Add('function pageReady() {');
  HL.Add('$(window).resize(onWindowResize);');
  HL.Add('};');
  HL.Add('function onWindowResize() {');
  HL.Add('var b = $("body");');
  HL.Add('var d = $("#silverlightControlHost");');
  HL.Add('d.height(b.height() - d.offset().top);');
  HL.Add('d.width(b.width() - d.offset().left);');
  HL.Add('}');
  HL.Add('</script>');

  WriteMenu;

  SL.Add('<div id="silverlightControlHost">');
  SL.Add('<object type="application/x-silverlight-2" width="100%" height="100%">');

  SL.Add(Format('<param name="source" value="%s"/>', [EA._key + '.xap']));
  SL.Add('<param name="background" value="white" />');
  SL.Add('<param name="minRuntimeVersion" value="2.0.31005.0" />');
  SL.Add('<param name="autoUpgrade" value="true" />');
  SL.Add(Format('<param name="initParams" value="%s" />', [GetInitParams]));

  SL.Add('<a href="http://go.microsoft.com/fwlink/?LinkID=124807" style="text-decoration: none;">');
  SL.Add('<img src="http://go.microsoft.com/fwlink/?LinkId=108181" alt="Get Microsoft Silverlight" style="border-style: none"/>');
  SL.Add('</a>');
  SL.Add('</object>');
  SL.Add('<iframe style=''visibility:hidden;height:0;width:0;border:0px''></iframe></div>');
end;

procedure TSilverlightWeb.InitTemplatedHostPage;
begin
  HL.Add('<script src="javascripts/jquery.js" type="text/javascript"></script>');

  HL.Add('<style type="text/css">');
  HL.Add('#silverlightControlHost { width:800px;height:600px; }');
  HL.Add('</style>');

  WriteMenu;

  SL.Add('<div id="silverlightControlHost">');
  SL.Add('<object type="application/x-silverlight-2" width="100%" height="100%">');

  SL.Add(Format('<param name="source" value="%s"/>', [EA._key + '.xap']));
  SL.Add('<param name="background" value="white" />');
  SL.Add('<param name="minRuntimeVersion" value="2.0.31005.0" />');
  SL.Add('<param name="autoUpgrade" value="true" />');
  SL.Add(Format('<param name="initParams" value="%s" />', [GetInitParams]));

  SL.Add('<a href="http://go.microsoft.com/fwlink/?LinkID=124807" style="text-decoration: none;">');
  SL.Add('<img src="http://go.microsoft.com/fwlink/?LinkId=108181" alt="Get Microsoft Silverlight" style="border-style: none"/>');
  SL.Add('</a>');
  SL.Add('</object>');
  SL.Add('<iframe style=''visibility:hidden;height:0;width:0;border:0px''></iframe></div>');
end;

end.
