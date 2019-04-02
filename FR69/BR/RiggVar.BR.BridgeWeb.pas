unit RiggVar.BR.BridgeWeb;

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

{
  Used to monitor a LocalBridge with a browser,
  see ServerBridge in unit BridgeServer.
}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.BR.BridgeLocal,
  IdContext,
  IdCustomHTTPServer,
  RiggVar.Web2.Base;

type
  TBridgeWeb = class(TWebControllerBase)
  private
    _operation: Integer;
    _eventType: Integer;
    _page: Integer;
    _answer: string;
    FLocalBridge: TLocalBridge;
    function InitPage(const h, p, pre: string): string;
    function GetLocalBridge: TLocalBridge;
    procedure SetLocalBridge(Value: TLocalBridge);
    procedure GetAnswer;
  protected
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
  public
    property LocalBridge: TLocalBridge read GetLocalBridge write SetLocalBridge;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.MsgToken,
  RiggVar.Util.Classes;

const
  pageIndex = 0;
  pageData = 1;
  pageContent = 2;
  pageConnections = 3;
  pageStatus = 4;
  pageBackup = 5;
  pageDiffLog = 6;

  operationClear=1;

function TBridgeWeb.GetLocalBridge: TLocalBridge;
begin
  result := FLocalBridge;
end;

procedure TBridgeWeb.SetLocalBridge(Value: TLocalBridge);
begin
  FLocalBridge := Value;;
end;

procedure TBridgeWeb.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  page: Integer;
  eventType: Integer;
  operation: Integer;
  op: string;
  idx: Integer;
begin
  eventType := 0;
  operation := 0;

  idx := ARequestInfo.Params.IndexOfName('EventType');
  if idx > -1 then
    eventType := StrToIntDef(ARequestInfo.Params.ValueFromIndex[idx], 0);

  idx := ARequestInfo.Params.IndexOfName('Op');
  if idx > -1 then
    op := ARequestInfo.Params.ValueFromIndex[idx];

  if (op = 'clear') then
    operation := operationClear;

  s := LowerCase(ARequestInfo.Document);

  if StartsWith(s, '/docroot/') then
  begin
    s := FilePath(s);
    if FileExists(s) then
      AResponseInfo.ServeFile(AContext, s);
    Exit;
  end;

  if Matches(s, 'data') then //without leading slash! (Client-MenuItem Actions/Download)
    page := pageData
  else if Matches(s, '/data') then
    page := pageData
  else if Matches(s, '/backup') then
    page := pageBackup
  else if Matches(s, '/difflog') then
    page := pageDiffLog
  else if Matches(s, '/connections') then
    page := pageConnections
  else if Matches(s, '/status') then
    page := pageStatus
  else if (Matches(s, '/') or Matches(s, '/index')) then
  begin
    page := pageIndex;
  end
  else
  begin
    AResponseInfo.Redirect(Path + '/index');
    Exit;
  end;

  Lock.Acquire;
  try
    _page := page;
    _eventType := eventType;
    _operation := operation;
    TThread.Synchronize(nil, GetAnswer);
    AResponseInfo.ContentText := _answer;
    AResponseInfo.CharSet := 'utf-8';
  finally
    Lock.Release;
  end;
end;

procedure TBridgeWeb.GetAnswer;
begin
  case _operation of
    operationClear: LocalBridge.Clear;
  end;

  HL.Clear;
  ML.Clear;
  SL.Clear;
  HL.Add('<title>' + cAppTitle + ' Bridge</title>');

  _answer := '-';
  case _page of
    pageData:
    begin
      _answer := LocalBridge.FBackup; //GetSwitchContent(_eventType).Content;
    end;

    pageConnections:
    begin
      _answer := InitPage('/connections', '', LocalBridge.GetWebStatusString);
    end;

    pageBackup:
    begin
      _answer := InitPage('/backup', '', LocalBridge.FBackup);
    end;

    pageDiffLog:
    begin
      _answer := InitPage('/difflog', '', LocalBridge.GetNewMessages(0, 0));
    end;

    pageStatus:
    begin
      _answer := InitPage('/status', '', LocalBridge.GetIDReport);
    end;

    pageIndex:
    begin
      _answer := InitPage('/index', '', '');
    end;
  end;

  HL.Clear;
  ML.Clear;
  SL.Clear;
end;

function TBridgeWeb.InitPage(const h, p, pre: string): string;
begin
  ML.Add('<div id="webmotor-menu"><p>');
  ML.Add('<a href="connections">connections</a> | ');
  //SL.Add('<a href="content?EventType=400">content_FR</a> | ');
  //SL.Add('<a href="content?EventType=600">content_SKK</a> | ');
  ML.Add('<a href="backup">backup</a> | ');
  ML.Add('<a href="difflog">difflog</a> | ');
  ML.Add('<a href="status">status</a>');
  ML.Add('</p></div>');

  if h = '' then
    SL.Add('<h3>RiggVar Switch</h3>')
  else
  begin
    SL.Add('<h2>');
    SL.Add(h);
    SL.Add('</h2>');
  end;

  if (p <> '') then
    SL.Add(p);

  if (pre <> '') then
  begin
    SL.Add('<pre>');
    SL.Add(pre);
    SL.Add('</pre>');
  end;

  result := MasterPage.Text;
end;

end.
