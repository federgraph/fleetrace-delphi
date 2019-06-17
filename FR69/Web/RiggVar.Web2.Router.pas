unit RiggVar.Web2.Router;

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
  System.Classes,
  IdContext,
  IdCustomHTTPServer,
  RiggVar.Web2.Intf,
  RiggVar.Web2.Indy,
  RiggVar.BO.ResourceManager,
  RiggVar.Web1.Images,
  RiggVar.Web1.CSS,
  RiggVar.Web4.EventArgs;

type
  TWebRouter = class(TWebRouterIndy)
  private
    FUseResource: Boolean;
    SL: TStringList;

    function Matches(s, t: string): Boolean;
    procedure InitEventMenuXml(EventCount: Integer = 3);
    function Wrap(s: string): string;

    function GetShowHomeTab: Boolean;
    function GetShowRemoteTab: Boolean;
    function GetShowBridgeTab: Boolean;
    function GetShowSilverlightTab: Boolean;
    function GetShowAngularTab: Boolean;
    procedure SetUseResource(const Value: Boolean);
    function GetEventMenuXml: string;
    function GetPayload(PostStream: TStream): string;
    procedure ServeAngularAsset(fn: string; AContext: TIdContext;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure ServeAngularFR(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ServeAngularFREO(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ServeAngularFRAC(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ServeAngularXX(
      ABaseRef, ADir: string;
      AWantHardCodedDir: Boolean;
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);

    function ResolveApi(const s: string): TApiEnum;
    procedure HandleApi(
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleApiGet(
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleApiPost(
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleWidget(
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  protected
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
  public
    AngularFR: string;
    AngularFREO: string;
    AngularFRAC: string;
    WantHardcodedAngularFR: Boolean;
    WantHardcodedAngularFREO: Boolean;
    WantHardcodedAngularFRAC: Boolean;
    HomeBtnFlag: Boolean;
    IndexPage: IWebController;
    HomeController: IWebController;
    RemoteController: IWebController;
    BridgeController: IWebController;
    SilverlightController: IWebController;
    AngularController: IWebController;
    ResourceManager: TResourceManager;
    ImageManager: TImageManager;
    ApiHelper: TWidgetApiHelper;
    constructor Create;
    destructor Destroy; override;
    property ShowHomeTab: Boolean read GetShowHomeTab;
    property ShowRemoteTab: Boolean read GetShowRemoteTab;
    property ShowBridgeTab: Boolean read GetShowBridgeTab;
    property ShowSilverlightTab: Boolean read GetShowSilverlightTab;
    property ShowAngularTab: Boolean read GetShowAngularTab;
    property UseResource: Boolean read FUseResource write SetUseResource;
    property EventMenuXml: string read GetEventMenuXml;
  end;

implementation

uses
  System.SysUtils,
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Def,
  RiggVar.BO.ExcelImport,
  RiggVar.BO.WriterJSON,
  RiggVar.Util.Classes,
  RiggVar.Web2.Page,
  RiggVar.Web4.EventMenuJson;

{ TWebRouter }

constructor TWebRouter.Create;
begin
  inherited Create;
  SL := TStringList.Create;

  BO.Data[1] := 'abc';
  BO.Data[2] := 'def';
  BO.Data[3] := 'ghi';

//  AngularFR := 'D:\Angular\FR03A1\dist\FR03A1';
//  AngularFREO := 'D:\Angular\FR03E1\dist\FR03E1';
//  AngularFRAC := 'D:\Angular\FR05I\dist\FR05I';

  AngularFR := Main.IniImage.AngularFR;
  AngularFREO := Main.IniImage.AngularFREO;
  AngularFRAC := Main.IniImage.AngularFRAC;

  WantHardcodedAngularFR := DirectoryExists(AngularFR);
  WantHardcodedAngularFREO := DirectoryExists(AngularFREO);
  WantHardcodedAngularFRAC := DirectoryExists(AngularFRAC);

  ApiHelper := TWidgetApiHelper.Create;

end;

destructor TWebRouter.Destroy;
begin
  SL.Free;
  ApiHelper.Free;
  inherited;
end;

function TWebRouter.GetShowBridgeTab: Boolean;
begin
  result := Assigned(BridgeController) and BridgeController.IsOnline;
end;

function TWebRouter.GetShowHomeTab: Boolean;
begin
  result := Assigned(HomeController) and HomeController.IsOnline;
end;

function TWebRouter.GetShowRemoteTab: Boolean;
begin
  result := Assigned(RemoteController) and RemoteController.IsOnline;
end;

function TWebRouter.GetShowSilverlightTab: Boolean;
begin
  result := Assigned(SilverlightController) and SilverlightController.IsOnline;
end;

function TWebRouter.GetShowAngularTab: Boolean;
begin
  result := Assigned(AngularController) and AngularController.IsOnline;
end;

function TWebRouter.Matches(s, t: string): Boolean;
begin
  result := CompareText(s, t) = 0;
end;

procedure TWebRouter.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s, t: string;
  Handled: Boolean;
  i: Integer;
  d: string;
begin
  Handled := false;
  s := LowerCase(ARequestInfo.Document);

  if (s = '/') or TUtils.StartsWith(s, '/index') then
  begin
    IndexPage.HandleCommand(AContext, ARequestInfo, AResponseInfo);
    Handled := true;
  end

  else if not TUtils.StartsWith(s, '/') then
  begin
    AResponseInfo.Redirect('/Home/Index');
    Handled := true;
  end

  else if TUtils.StartsWith(s, '/frac') then
  begin
    ServeAngularFRAC(AContext, ARequestInfo, AResponseInfo);
    Handled := true;
  end

  else if TUtils.StartsWith(s, '/freo') then
  begin
    ServeAngularFREO(AContext, ARequestInfo, AResponseInfo);
    Handled := true;
  end

  else if TUtils.StartsWith(s, '/fr') then
  begin
    ServeAngularFR(AContext, ARequestInfo, AResponseInfo);
    Handled := true;
  end

  else if TUtils.StartsWith(s, '/em') then
  begin
    AResponseInfo.ContentType := 'text/xml';
    AResponseInfo.ContentText := EventMenuXml;
    Handled := true;
  end

  else if TUtils.StartsWith(s, '/api') then
  begin
    ApiHelper.Api := ResolveApi(s);
    HandleApi(AContext, ARequestInfo, AResponseInfo);
    Handled := True;
  end

  else if TUtils.StartsWith(s, '/ud') then
  begin
    i := 0;
    if Matches(s, '/ud/1') then
      i := 1
    else if Matches(s, '/ud/2') then
      i := 2
    else if Matches(s, '/ud/3') then
      i := 3;

    if ARequestInfo.Command = 'POST' then
    begin
      d := GetPayLoad(ARequestInfo.PostStream);
      if d = '' then
      begin
        AResponseInfo.ContentType := 'application/json';
        AResponseInfo.ContentText := '{"retvalue": "payload is empty"}';
        Handled := true;
      end
      else
      begin
        BO.Data[i] := d;
        AResponseInfo.ContentType := 'application/json';
        AResponseInfo.ContentText := '{"retvalue": "ok"}';
        Handled := true;
      end;
    end;

    if ARequestInfo.Command = 'GET' then
    begin
      AResponseInfo.ContentType := 'text/plain';
      d := BO.Data[i];
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentText := Wrap(d);
      Handled := true;
    end;

  end

  else if TUtils.StartsWith(s, '/ed') then
  begin
    i := 0;
    if Matches(s, '/ed/1') then
      i := 1
    else if Matches(s, '/ed/2') then
      i := 2
    else if Matches(s, '/ed/3') then
      i := 3;

    if ARequestInfo.Command = 'POST' then
    begin
      d := ARequestInfo.Params.Text;
      if d = '' then
      begin
        AResponseInfo.ContentType := 'text/plain; charset=utf-8';
        AResponseInfo.ContentText := 'FormParam ED is empty!'#13#10;
        Handled := true;
      end
      else
      begin
        BO.Data[i] := d;
        AResponseInfo.ContentType := 'text/plain; charset=utf-8';
        AResponseInfo.ContentText := 'ok'#13#10;
        Handled := true;
      end;
    end;

    if ARequestInfo.Command = 'GET' then
    begin
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      d := BO.Data[i];
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentText := d;
      Handled := true;
    end;

  end

  else if TUtils.StartsWith(s, '/eventdata') then
  begin
    i := 0;
    if Matches(s, '/eventdata/1') then
      i := 1
    else if Matches(s, '/eventdata/2') then
      i := 2
    else if Matches(s, '/eventdata/3') then
      i := 3;

    if ARequestInfo.Command = 'GET' then
    begin
      AResponseInfo.ContentType := 'text/html; charset=utf-8';
      AResponseInfo.ContentText := Wrap(BO.Data[i]);
      Handled := true;
    end;

  end

  else if Pos('/styles/', s) > 0 then
  begin
    if UseResource then
    begin
    if Pos('/styles/style.css', s) > 0 then
        t := ResourceManager.LoadText(style_css)
    else if Pos('/styles/style-h.css', s) > 0 then
      t := InitHomeStyleCss(SL, HomeController.GetVirtualDir)
    else if Pos('/styles/style-r.css', s) > 0 then
      t := InitRemoteStyleCss(SL, RemoteController.GetVirtualDir)
    else if Pos('/styles/style-p.css', s) > 0 then
        t := ResourceManager.LoadText(style_p_css)
    end
    else
    begin
      if Pos('/styles/style.css', s) > 0 then
        t := ResourceManager.LoadFile(style_css)
      else if Pos('/styles/style-h.css', s) > 0 then
        t := ResourceManager.LoadFile(style_h_css)
      else if Pos('/styles/style-r.css', s) > 0 then
        t := ResourceManager.LoadFile(style_r_css)
      else if Pos('/styles/style-p.css', s) > 0 then
        t := ResourceManager.LoadFile(style_p_css)
    end;

    AResponseInfo.ContentType := 'text/css; charset=utf-8';
    AResponseInfo.ContentText := t;
    Handled := true;
  end

  else if Pos('/stylesheets/site.css', s) > 0 then
  begin
    if UseResource then
      t := ResourceManager.LoadText(site_css)
    else
      t := ResourceManager.LoadFile(site_css);
    AResponseInfo.ContentType := 'text/css; charset=utf-8';
    AResponseInfo.ContentText := t;
    Handled := true;
  end

  else if Pos('/javascripts/jquery.js', s) > 0 then
  begin
    if UseResource then
      t := ResourceManager.LoadText(jquery_js)
    else
      t := ResourceManager.LoadFile(jquery_js);
    AResponseInfo.ContentText := t;
    Handled := true;
  end

  else if TUtils.StartsWith(s, '/home/') then
  begin
    if Assigned(HomeController) then
    begin
      HomeController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/remote/') then
  begin
    if Assigned(RemoteController) then
    begin
      RemoteController.SetTemp('');
      RemoteController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/intern/') then
  begin
    if Assigned(RemoteController) then
    begin
      RemoteController.SetTemp('/intern');
      RemoteController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/widget/') then
  begin
    if Assigned(RemoteController) then
    begin
      RemoteController.SetTemp('/widget');
      RemoteController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/bridge/') then
  begin
    if Assigned(BridgeController) then
    begin
      BridgeController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/silverlight/') then
  begin
    if Assigned(SilverlightController) then
    begin
      SilverlightController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/angular/') then
  begin
    if Assigned(AngularController) then
    begin
      AngularController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if Pos('/clientaccesspolicy.xml', s) = 1 then
  begin
    if SilverlightController <> nil then
    begin
      SilverlightController.HandleCommand(AContext, ARequestInfo, AResponseInfo);
      Handled := true;
    end;
  end

  else if TUtils.StartsWith(s, '/images/') then
  begin
    IndexPage.HandleCommand(AContext, ARequestInfo, AResponseInfo);
    Handled := True;
  end

  else if TUtils.StartsWith(s, '/stylesheets/') then
  begin
    IndexPage.HandleCommand(AContext, ARequestInfo, AResponseInfo);
    Handled := True;
  end

  else if TUtils.StartsWith(s, '/javascripts/') then
  begin
    IndexPage.HandleCommand(AContext, ARequestInfo, AResponseInfo);
    Handled := True;
  end

  else if TUtils.StartsWith(s, '/robots.txt') then
  begin
    AResponseInfo.ContentType := 'text/plain';
    AResponseInfo.ContentText := 'User-Agent: *'#13#10'Disallow: /'#13#10;
    Handled := true;
  end;

  if not Handled then
  begin
    AResponseInfo.Redirect('/Index')
  end;
end;

procedure TWebRouter.SetUseResource(const Value: Boolean);
begin
  FUseResource := Value;
  ImageManager.UseResources := Value;
  ResourceManager.UseResources := Value;
end;

procedure TWebRouter.InitEventMenuXml(EventCount: Integer);
var
  i: Integer;
begin
  SL.Clear;
  SL.Add('<?xml version="1.0" encoding="utf-8" ?>');
  SL.Add(Format('<EventMenu Root="%s">', [Url]));
  SL.Add('  <ComboEntry Caption="FRED Server">');
  SL.Add('    <DataFolder Url="ED/">');
  SL.Add('      <ImgFolder Url="">');
  for i := 1 to EventCount do
  begin
  SL.Add(Format('        <Btn Data="%d" Img="" Text="ED %d" />', [i, i]));
  end;
  SL.Add('      </ImgFolder>');
  SL.Add('    </DataFolder>');
  SL.Add('  </ComboEntry>');
  SL.Add('</EventMenu>');
end;

function TWebRouter.Wrap(s: string): string;
begin
  result := '<!DOCTYPE html>'#13#10'<html><body>' + s + '</body></html>';
end;

function TWebRouter.GetEventMenuXml: string;
begin
  if Url <> '' then
  begin
    InitEventMenuXml;
    result := SL.Text;
  end;
end;

function TWebRouter.GetPayload(PostStream: TStream): string;
var
  s: TStringStream;
begin
  if Assigned(PostStream) then
  begin
    if PostStream is TMemoryStream then
    begin
      //PostStream.Position := 0;
      s := TStringStream.Create;
      try
        s.CopyFrom(PostStream, PostStream.Size);
        result := s.DataString;
      finally
        s.Free;
      end;
    end;
  end
  else
    result := '';
end;

//function TWebRouter.GetPayload2(ARequestInfo: TIdHTTPRequestInfo): string;
//var
//  Stream: TStream;
//begin
//  Stream := ARequestInfo.PostStream;
//  if assigned(Stream) then
//  begin
//    Stream.Position := 0;
//    result := ReadStringFromStream(Stream);
//  end;
//end;

procedure TWebRouter.ServeAngularFR(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  ServeAngularXX('fr', AngularFR, WantHardcodedAngularFR, AContext, ARequestInfo, AResponseInfo);
end;

procedure TWebRouter.ServeAngularFREO(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  ServeAngularXX('freo', AngularFREO, WantHardcodedAngularFREO, AContext, ARequestInfo, AResponseInfo);
end;

procedure TWebRouter.ServeAngularFRAC(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  ServeAngularXX('frac', AngularFRAC, WantHardcodedAngularFRAC, AContext, ARequestInfo, AResponseInfo);
end;

procedure TWebRouter.ServeAngularXX(
  ABaseRef: string; ADir: string; AWantHardCodedDir: Boolean;
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  p: Integer;
  fn: string;
begin
  s := ARequestInfo.Document;
  fn := '';
  p := Pos('/' + ABaseRef, s);
  if p >= 1 then
  begin
    fn := Copy(s, p + Length('/' + ABaseRef + '/'));
    if fn = '' then
      fn := 'index.html';

    if AWantHardCodedDir then
      fn := Format('%s\%s', [ADir, fn])
    else
      fn := Format('%sdist\FR03\%s', [dn, fn]) //dn is field in parent class
  end;

  ServeAngularAsset(fn, AContext, AResponseInfo);
end;

procedure TWebRouter.ServeAngularAsset(fn: string;
  AContext: TIdContext;
  AResponseInfo: TIdHTTPResponseInfo);
var
  e: string;
begin
  if FileExists(fn) then
  begin
    e := ExtractFileExt(fn);

    if e = '.html' then
    begin
      SL.LoadFromFile(fn);
      AResponseInfo.ContentType := 'text/html; charset=utf-8';
      AResponseInfo.ContentText := SL.Text;
      Exit;
    end
    else if e = '.css' then
      AResponseInfo.ContentType := 'text/css; charset=utf-8'
    else if e = '.js' then
      AResponseInfo.ContentType := 'text/javascript'
    else  if e = '.png' then
      AResponseInfo.ContentType := 'image/png'
    else
      AResponseInfo.ContentType := '';

    AResponseInfo.ServeFile(AContext, fn);
  end;
end;

function TWebRouter.ResolveApi(const s: string): TApiEnum;
var
  j: TApiEnum;
begin
  if Matches(s, '/api/replace-race-data') then
    j := api_race_data_json
  else if Matches(s, '/api/rd.json') then
    j := api_rd_json
  else if Matches(s, '/api/ed.json') then
    j := api_ed_json
  else if Matches(s, '/api/event-data') then
    j := api_event_data
  else if Matches(s, '/api/get-input-connection-status') then
    j := api_get_input_connection_status
  else if Matches(s, '/api/get-output-connection-status') then
    j := api_get_output_connection_status
  else if Matches(s, '/api/input-wire-connect') then
    j := api_input_wire_connect
  else if Matches(s, '/api/input-wire-disconnect') then
    j := api_input_wire_disconnect
  else if Matches(s, '/api/output-wire-connect') then
    j := api_output_wire_connect
  else if Matches(s, '/api/output-wire-disconnect') then
    j := api_output_wire_disconnect
  else if Matches(s, '/api/send-msg') then
    j := api_send_msg
  else if Matches(s, '/api/manage-go-back-to-race') then
    j := api_manage_go_back_to_race
  else if Matches(s, '/api/manage-clear-timepoint') then
    j := api_manage_clear_timepoint
  else if Matches(s, '/api/manage-clear-race') then
    j := api_manage_clear_race
  else if Matches(s, '/api/manage-clear') then
    j := api_manage_clear
  else if Matches(s, '/api/query-params') then
    j := api_query_params

  else if Matches(s, '/api/widget/test') then
    j := api_widget_test
  else if Matches(s, '/api/widget/time') then
    j := api_widget_time
  else if Matches(s, '/api/widget/netto') then
    j := api_widget_netto

  else if Matches(s, '/api/get-simple-text') then
    j := api_get_simple_text
  else if Matches(s, '/api/get-simple-json') then
    j := api_get_simple_json
  else if Matches(s, '/api/event-data-json') then
    j := api_event_data_json
  else if Matches(s, '/api/event-data-html') then
    j := api_event_data_html
  else if Matches(s, '/api/race-data-json') then
    j := api_race_data_json
  else if Matches(s, '/api/event-menu-json') then
    j := api_event_menu_json

  else if Matches(s, '/api/widget/do-time') then
    j := api_widget_do_time
  else if Matches(s, '/api/widget/do-finish') then
    j := api_widget_do_finish
  else if Matches(s, '/api/widget/do-time-for-table') then
    j := api_widget_do_time_for_table
  else if Matches(s, '/api/widget/do-finish-for-table') then
    j := api_widget_do_finish_for_table

  else if Matches(s, '/api/widget/do-timing-event') then
    j := api_widget_do_timing_event
  else if Matches(s, '/api/widget/do-timing-event-for-table') then
    j := api_widget_do_timing_event_for_table
  else if Matches(s, '/api/widget/do-timing-event-quick') then
    j := api_widget_do_timing_event_quick

  else if Matches(s, '/api/widget/get-race-table-html') then
    j := api_widget_get_race_table_html
  else if Matches(s, '/api/widget/get-race-table-json') then
    j := api_widget_get_race_table_json
  else if Matches(s, '/api/widget/get-narrow-race-table-json') then
    j := api_widget_get_narrow_race_table_json
  else if Matches(s, '/api/widget/get-wide-race-table-json') then
    j := api_widget_get_wide_race_table_json

  else if Matches(s, '/api/widget/get-event-table-json') then
    j := api_widget_get_event_table_json
  else if Matches(s, '/api/widget/get-finish-table-json') then
    j := api_widget_get_finish_table_json
  else if Matches(s, '/api/widget/get-points-table-json') then
    j := api_widget_get_points_table_json
  else
    j := api_none;

  result := j;
end;

procedure TWebRouter.HandleApi(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  if ApiHelper.Api = api_none then
  begin
    //do nothing
  end
  else if ARequestInfo.Command = 'GET' then
  begin
    HandleApiGet(AContext, ARequestInfo, AResponseInfo);
  end
  else
  begin
    HandleApiPost(AContext, ARequestInfo, AResponseInfo);
  end;
end;

procedure TWebRouter.HandleApiGet(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  case ApiHelper.Api of

    api_get_simple_json:
    begin
      SL.Clear;
      BO.BackupToSimpleJson(SL);
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.ContentText := SL.Text;
      SL.Clear;
    end;

    api_get_simple_text:
    begin
      SL.Clear;
      BO.BackupToSimpleText(SL);
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := SL.Text;
      SL.Clear;
    end;

    api_event_data:
    begin
      SL.Clear;
      BO.BackupToText(SL);
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := SL.Text;
      SL.Clear;
    end;

    api_get_input_connection_status,
    api_get_output_connection_status:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.ContentText := '{ "connected": true, "websockets": false }';
    end;

    api_input_wire_connect:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'input-wire-connecting...';
    end;

    api_input_wire_disconnect:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'input-wire-disconnecting...';
    end;

    api_output_wire_connect:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'output-wire-connecting...';
    end;

    api_output_wire_disconnect:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'output-wire-disconnecting...';
    end;

    api_send_msg:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'ok';

      TThread.Synchronize(nil,
      procedure
      var
        msg: string;
      begin
        msg := Trim(ARequestInfo.Params.Values['value']);
        BO.UndoConnection.InjectMsg(msg);
        BO.Calc;
        BO.Inform(ScheduleEventUpdate);
      end);
    end;

    api_manage_clear:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'ok';

      TThread.Synchronize(nil,
      procedure
      begin
        BO.UndoConnection.InjectMsg('Manage.Clear');
      end);
    end;

    api_manage_clear_race:
    begin
      ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 99);

      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'ok';

      TThread.Synchronize(nil,
      procedure
      begin
        BO.ClearRaceCommand(ApiHelper.race);
      end);
    end;

    api_manage_go_back_to_race:
    begin
      ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 99);

      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'ok';

      TThread.Synchronize(nil,
      procedure
      begin
        BO.GoBackToRaceCommand(ApiHelper.race);
      end);
    end;

    api_manage_clear_timepoint:
    begin
      ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 99);
      ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 99);

      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'ok';

      TThread.Synchronize(nil,
      procedure
      begin
        BO.ClearTimepointCommand(ApiHelper.race, ApiHelper.it);
      end);
    end;

    api_query_params:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.ContentText := Format(
      '{ "raceCount": %d, "itCount": %d, "startlistCount": %d }',
      [ BO.BOParams.RaceCount,
        BO.BOParams.ITCount,
        BO.BOParams.StartlistCount
      ]);
    end;

    api_widget_test:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := 'test via get';
    end;

    api_widget_time:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/plain; charset=utf-8';
        ApiHelper.DoTime;
        AResponseInfo.ContentText := ApiHelper.Time;
      end);
    end;

    api_widget_netto:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetFinishReportJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_event_data_json:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
      AResponseInfo.ContentText := TUtils.PrettyFormat(BO.Data[2]);
    end;

    api_event_data_html:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentText := Wrap(TUtils.PrettyFormat(BO.Data[2], True));
      AResponseInfo.ContentType := 'text/html; charset=utf-8';
    end;

    api_race_data_json:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentText := TUtils.PrettyFormat(BO.Data[3]);
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
    end;

    api_event_menu_json:
    begin
      AResponseInfo.CacheControl := 'no-cache';
      AResponseInfo.ContentText := TUtils.PrettyFormat(BO.Data[3]);
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';

      TThread.Synchronize(nil,
      procedure
      begin
        AResponseInfo.ContentType := 'application/json';
        Main.DocManager.FillEventNameList(SL);
        AResponseInfo.ContentText := TEventMenuJson.InitJson(SL);
        SL.Clear;
      end);
    end;

    api_widget_do_time..api_widget_get_points_table_json:
    begin
       HandleWidget(AContext, ARequestInfo, AResponseInfo);
    end;
  end;
end;

procedure TWebRouter.HandleApiPost(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  d: string;
begin
  d := GetPayLoad(ARequestInfo.PostStream);
  if d = '' then
  begin
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.ContentText := '{"retvalue": "payload is empty"}';
  end
  else
  begin
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.ContentText := '{"retvalue": "ok"}';
    case ApiHelper.Api of

      api_race_data_json:
      begin
        { got RaceDataJSON with race-param from angular client }
        BO.Data0 := TUtils.PrettyFormat(d);
        BO.JsonT := RaceDataJSON;
        Main.AngularPost.JsonType := TJsonType.RaceDataJson;
        Main.AngularPost.Json := BO.Data0;
        Main.AngularPost.Race := Main.GuiManager.Race;
        Main.AngularPost.WantFilter := false;
        Main.AngularPost.WantConversion := false;
        Main.AngularPost.ProcessRace(StrToIntDef(ARequestInfo.Params.Values['race'], 0));
      end;

      api_rd_json:
      begin
        { got RaceDataJSON from angular client }
        BO.Data0 := TUtils.PrettyFormat(d);
        BO.JsonT := RaceDataJSON;
        Main.AngularPost.JsonType := TJsonType.RaceDataJson;
        Main.AngularPost.Json := BO.Data0;
        Main.AngularPost.Race := Main.GuiManager.Race;
        Main.AngularPost.WantFilter := false;
        Main.AngularPost.WantConversion := false;
        Main.AngularPost.ProcessRace;
      end;

      api_ed_json:
      begin
        { got EventDataJSON from angular client }
        BO.Data0 := TUtils.PrettyFormat(d);
        BO.JsonT := EventDataJSON;
        Main.AngularPost.JsonType := TJsonType.EventDataJson;
        Main.AngularPost.Json := BO.Data0;
        Main.AngularPost.Race := Main.GuiManager.Race;
        Main.AngularPost.WantFilter := false;
        Main.AngularPost.WantConversion := false;
        Main.AngularPost.ProcessEvent;
      end;

      api_widget_test:
      begin
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/plain; charset=utf-8';
        AResponseInfo.ContentText := 'test via post';
      end;

      api_widget_time:
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        TThread.Synchronize(nil,
        procedure
        begin
          AResponseInfo.CacheControl := 'no-cache';
          AResponseInfo.ContentType := 'text/plain; charset=utf-8';
          ApiHelper.DoTime;
          AResponseInfo.ContentText := ApiHelper.Time;
        end);
      end;

    end;
  end;
end;

procedure TWebRouter.HandleWidget(
  AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin

{
  api_widget_do_time,
  api_widget_do_finish,
  api_widget_do_time_for_table,
  api_widget_do_finish_for_table,

  api_widget_do_timing_event
  api_widget_do_timing_event_quick
  api_widget_do_timing_event_for_table

  api_widget_get_race_table_html,
  api_widget_get_race_table_json,
  api_widget_get_narrow_race_table_json,
  api_widget_get_wide_race_table_json,

  api_widget_get_event_table_json,
  api_widget_get_finish_table_json,
  api_widget_get_points_table_json
}

{ case ApiHelper.Api of api_widget_do_time .. api_widget_get_points_table_json }
  case ApiHelper.Api of

    api_widget_do_time:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoTime;
        AResponseInfo.ContentText := ApiHelper.Time;
      end);
    end;

    api_widget_do_finish:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoFinish;
        AResponseInfo.ContentText := IntToStr(ApiHelper.FinishPosition);
      end);
    end;

    api_widget_do_time_for_table:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoTimeForTable;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_do_finish_for_table:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoFinishForTable;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_do_timing_event_for_table:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        ApiHelper.option := StrToIntDef(ARequestInfo.Params.Values['Option'], 1);
        ApiHelper.mode := StrToIntDef(ARequestInfo.Params.Values['Mode'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoTimingEventForTable;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
        BO.Calc;
        BO.Inform(ScheduleEventUpdate);
      end);
    end;

    api_widget_do_timing_event:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        ApiHelper.option := StrToIntDef(ARequestInfo.Params.Values['Option'], 0);
        //ApiHelper.mode := StrToIntDef(ARequestInfo.Params.Values['Mode'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoTimingEvent;
        AResponseInfo.ContentText := 'ok';
        BO.Calc;
        BO.Inform(ScheduleEventUpdate);
      end);
    end;

    api_widget_do_timing_event_quick:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        ApiHelper.bib := StrToIntDef(ARequestInfo.Params.Values['Bib'], 0);
        //ApiHelper.option := StrToIntDef(ARequestInfo.Params.Values['Option'], 0);
        //ApiHelper.mode := StrToIntDef(ARequestInfo.Params.Values['Mode'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.DoTimingEventQuick;
        AResponseInfo.ContentText := 'ok';
      end);
    end;

    api_widget_get_race_table_html:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetRaceTableHtml;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_get_race_table_json:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.mode := StrToIntDef(ARequestInfo.Params.Values['Mode'], 1);
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetRaceTableJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_get_narrow_race_table_json:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetNarrowRaceTableJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_get_wide_race_table_json:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.race := StrToIntDef(ARequestInfo.Params.Values['Race'], 1);
        ApiHelper.it := StrToIntDef(ARequestInfo.Params.Values['IT'], 0);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetWideRaceTableJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_get_event_table_json:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        ApiHelper.mode := StrToIntDef(ARequestInfo.Params.Values['Mode'], 1);
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetEventTableJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_get_finish_table_json:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetFinishReportJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

    api_widget_get_points_table_json:
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        AResponseInfo.CacheControl := 'no-cache';
        AResponseInfo.ContentType := 'text/html; charset=utf-8';
        ApiHelper.GetPointsReportJson;
        AResponseInfo.ContentText := ApiHelper.RL.Text;
      end);
    end;

  end;
end;

end.
