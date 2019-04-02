unit RiggVar.Web4.Server;

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
  RiggVar.Util.Classes,
  RiggVar.Web4.EventArgs,
  RiggVar.Web2.Base;

type
  TAngularWeb = class(TWebControllerBase)
  private
    function Wrap(s: string): string;
  protected
    EA: TWebEventArgs;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    procedure GetAnswer;
  public
    AL: TStringList;
    OfflineMsg: string;
    SkipSync: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Web4.EventMenuJson;

constructor TAngularWeb.Create;
begin
  inherited Create;
  OfflineMsg := 'offline';
  AL := TStringList.Create;
  EA := TWebEventArgs.Create;
end;

destructor TAngularWeb.Destroy;
begin
  AL.Free;
  EA.Free;
  inherited Destroy;
end;

procedure TAngularWeb.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  page: TPageEnum;
  p: Integer;
  key: string;
  id: Integer;
begin
  s := ARequestInfo.Document;

  AResponseInfo.ContentType := 'text/html; charset=utf-8';

  id := -1;

  if IsOffline then
  begin
    AResponseInfo.ContentType := 'text/plain; charset=utf-8';
    AResponseInfo.ContentText := OfflineMsg;
    Exit;
  end;

  //AResponseInfo.CacheControl := 'no-cache';

  if (Matches(s, '/') or Matches(LowerCase(s), '/index')) then
  begin
    page := pageIndex;
  end

  else if Matches(LowerCase(s), '/event-data') then
  begin
    page := pageDownloadCurrentEventData;
    AResponseInfo.ContentType := 'text/plain; charset=utf-8';
  end

  else if Matches(LowerCase(s), '/event-menu-n.json') then
  begin
    page := pageEventMenuJsonN;
    AResponseInfo.ContentType := 'application/json';
  end

  else if Matches(LowerCase(s), '/event-menu.json') then
  begin
    page := pageEventMenuJson;
    AResponseInfo.ContentType := 'application/json';
  end

  else if Pos('EventMenu/', s) > 1  then
  begin
    page := pageIndex;
    if Matches(s, '/EventMenu/Json')  then
    begin
      AResponseInfo.ContentType := 'application/json';
      page := pageEventMenuJson;
    end
    else if Pos('EventMenu/Data/', s) > 1 then
    begin
      p := Pos('/EventMenu/Data/', s);
      key := Copy(s, p + Length('/EventMenu/Data/'));
      id := StrToIntDef(key, -1);
      page := pageEventMenuData;
      AResponseInfo.ContentType := 'text/plain; charset=utf-8';
    end
  end

  else if Matches(LowerCase(s), '/event-data-json') then
  begin
    //page := pageEventDataJson;
    AResponseInfo.ContentText := TUtils.PrettyFormat(BO.Data[2]);
    AResponseInfo.ContentType := 'text/plain; charset=utf-8';
    Exit;
  end

  else if Matches(LowerCase(s), '/event-data-json.html') then
  begin
    //page := pageEventDataJsonAsHtml;
    AResponseInfo.ContentText := Wrap(TUtils.PrettyFormat(BO.Data[2], True));
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    Exit;
  end

  else if Matches(LowerCase(s), '/race-data-json') then
  begin
    //page := pageRaceDataJson;
    AResponseInfo.ContentText := TUtils.PrettyFormat(BO.Data[3]);
    AResponseInfo.ContentType := 'text/plain; charset=utf-8';
    Exit;
  end

  else
  begin
    AResponseInfo.Redirect(Path + '/Index');
    Exit;
  end;

  Lock.Acquire;
  try
    EA._page := page;
    EA._id := id;

    if SkipSync then
    begin
      SkipSync := false;
      GetAnswer;
    end
    else
      TThread.Synchronize(nil, GetAnswer);

    AResponseInfo.ContentText := EA._answer;
  finally
    Lock.Release;
  end;
end;

procedure TAngularWeb.GetAnswer;
var
  i: Integer;
  en: string;
begin
  HL.Clear;
  ML.Clear;
  SL.Clear;
  HL.Add('<title>FR Angular</title>');

  EA._answer := '-';
  case EA._page of

    pageIndex:
    begin
      SL.Add('<p>');
      SL.Add('<a href="/fr/">FR</a>&nbsp;|&nbsp;');
      SL.Add('<a href="/freo/">FREO</a>&nbsp;|&nbsp;');
      SL.Add('<a href="/frac/">FRAC</a>&nbsp;|&nbsp;');
      SL.Add('<a href="/angular/event-menu.json">event-menu</a>&nbsp;|&nbsp;');
      SL.Add('<a href="/angular/event-data">event-data</a>&nbsp;|&nbsp;');
      SL.Add('<a href="/angular/EventMenu/Json">EM JSON</a>&nbsp;|&nbsp;');
      SL.Add('<a href="/angular/EventMenu/Data/1">ED 1</a>&nbsp;|&nbsp;');
      SL.Add('</p>');
      SL.Add('<h2>angular/index</h2>');
      EA._answer := MasterPage.Text;
    end;

    pageEventMenuJsonN:
    begin
      i := Main.DocManager.EventCount;
      if i < 1 then
        i := 1
      else if i > 8 then
        i := 8;
      EA._answer := TEventMenuJson.InitJsonN(i);
    end;

    pageEventMenuJson:
    begin
      Main.DocManager.FillEventNameList(AL);
      EA._answer := TEventMenuJson.InitJson(AL);
      AL.Clear;
    end;

    pageEventMenuData:
    begin
      if EA._id = 0 then
      begin
        EA._answer := BO.ToTXT;
      end
      else
      begin
        Main.DocManager.FillEventNameList(SL);
        i := EA._id-1;
        if (i >= 0) and (i < SL.Count) then
        begin
          en := SL[i];
          EA._answer := Main.DocManager.DocDownloadByName(en);
        end
        else
          EA._answer := '/EventMenu/Data/ID (id is out of range)';
      end;
    end;

    pageDownloadCurrentEventData:
    begin
      AL.Clear;
      BO.BackupToText(AL);
      EA._answer := AL.Text;
      AL.Clear;
    end;

    pageDownloadEventMenuJSON:
    begin
      AL.Clear;
      AL.Add('{');
      AL.Add('  "Path": "angular/EventMenu",');
      AL.Add('  "Menu": [');
      AL.Add('    {');
      AL.Add('        "Folder" : "Data",');
      AL.Add('        "Items" : [');
      AL.Add('            "ED 1",');
      AL.Add('            "ED 2",');
      AL.Add('            "ED 3"');
      AL.Add('        ]');
      AL.Add('    }');
      AL.Add('  ]');
      AL.Add('}');
      EA._answer := AL.Text;
      AL.Clear;
    end;

  end;

  HL.Clear;
  ML.Clear;
  SL.Clear;
end;

function TAngularWeb.Wrap(s: string): string;
begin
  result := '<!DOCTYPE html>'#13#10'<html><body>' + s + '</body></html>';
end;

end.
