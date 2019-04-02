unit RiggVar.Web2.Page;

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
  RiggVar.Web2.Intf,
  RiggVar.Web2.Base;

type
  TIndexPage = class(TWebControllerBase)
  protected
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    function AddImage(Index: Integer): string;
  public
    constructor Create;
  end;

implementation

constructor TIndexPage.Create;
begin
  inherited Create;
end;

function TIndexPage.AddImage(Index: Integer): string;
begin
  SL.Add(Format('<p><img src="/images/msgflow-%.2d.jpg" alt="RiggVar.Msgflow.%.2d" /></p>', [Index, Index]));
end;

procedure TIndexPage.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
begin
  inherited;

  HL.Clear;
  ML.Clear;
  SL.Clear;
  HL.Add('<title>FR Index</title>');

  Init;

  if MasterPage.IsPlain then
  begin
    ML.Add('<p>');
    ML.Add('Root');
    if Assigned(HttpServer.BridgeController) then
      ML.Add(' | <a href="/Bridge/Index">Bridge</a>');
    if Assigned(HttpServer.HomeController) then
      ML.Add(' | <a href="/Home/Index">Home</a>');
    if Assigned(HttpServer.RemoteController) then
      ML.Add(' | <a href="/Remote/Index">Remote</a>');
    if not MasterPage.IsEmbedded and Assigned(HttpServer.SilverlightController) then
      ML.Add(' | <a href="/Silverlight/Index">Silverlight</a>');
    if (not MasterPage.IsEmbedded) and MasterPage.HasAngularClient and Assigned(HttpServer.AngularController) then
      ML.Add(' | <a href="/fr/index.html">Angular</a>');
    ML.Add('</p>');
  end;

  SL.Add('<h2>');
  SL.Add('/index');
  SL.Add('</h2>');

  if MasterPage.IsPlain then
  begin
    SL.Add('<ul>');
    SL.Add('<li><a href="/Index/MsgFlow01">Msgflow 01</a></li>');
    SL.Add('<li><a href="/Index/MsgFlow02">New Msgflow 02</a></li>');
    SL.Add('</ul>');
  end
  else
  begin
    ML.Add('<p>');
      ML.Add('<a href="/Index/MsgFlow01">Msgflow 01</a>');
    if Assigned(HttpServer.HomeController) then
      ML.Add(' | <a href="/Index/MsgFlow02">New Msgflow 02</a>');
    ML.Add('</p>');
  end;

  s := LowerCase(ARequestInfo.Document);
  if Matches(s, 'index/msgflow01') then
    AddImage(1)
  else if Matches(s, 'index/msgflow02') then
    AddImage(2);

  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  AResponseInfo.ContentText := MasterPage.Text;

  HL.Clear;
  ML.Clear;
  SL.Clear;
end;

end.
