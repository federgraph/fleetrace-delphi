unit RiggVar.EM.ConHttp;

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
  System.Math,
  IdGlobal,
  IdHttp,
  IdUri,
  RiggVar.EM.Connection;

type
  THttpCon = class(TEventMenuConnection)
  private
    Response_CharSet: string;
    function EnsureCRLF(const Input: string): string;
    function RemovePreamble(const s: string): string;
    function HasUtf8EncodingAttribute(b: TBytes; l: Integer): Boolean;
  protected
    function CheckForHomeCall: Boolean;
  public
    function Get: string; override;
    procedure Post(const s: string); override;
  end;

implementation

uses
  RiggVar.App.Main;

{ THttpCon }

function THttpCon.CheckForHomeCall: Boolean;
var
  u1, u2: TIdUri;
begin
  result := false;
  u1 := nil;
  u2 := nil;
  if Assigned(Main.GuiManager.SilverlightWeb) then
  try
    u1 := TIdUri.Create(Url);
    u2 := TIdUri.Create(Main.GuiManager.HomeRouter.Url);
    if u1.Host = u2.Host then
    begin
      Main.GuiManager.SilverlightWeb.SkipSync := True;
      result := true;
    end;
  finally
    if Assigned(u1)then
      u1.Free;
    if Assigned(u2) then
      u2.Free;
  end;
end;

function THttpCon.Get: string;
var
  c: TIdHttp;
  bs: TBytesStream;
  ss: TStringStream;
  b: TBytes;
  u: TBytes;
  isUTF8: Boolean;
  encodedUrl: string;
  l: Integer;
begin
  result := '';
  bs := TBytesStream.Create;
  c := TIdHTTP.Create(nil);
  c.ConnectTimeout := 2000;
  c.ReadTimeout := 5000;
  try
    try
      //result := c.Get(Url); //relies on CharSet in HttpHeader or HttpMetaEquiv

      encodedUrl := TIdUri.UrlEncode(Url);
      c.Get(encodedUrl, bs);
      b := bs.Bytes;
      l := bs.Size;
      if l > c.Response.ContentLength then
        l := c.Response.ContentLength;

      //1st attempt to detect utf-8 encoding
      Response_CharSet := LowerCase(c.Response.CharSet);
      isUTF8 := Response_CharSet = 'utf-8';

      //2nd attempt to detect utf-8 encoding
      if not isUTF8 then
      begin
        u := TEncoding.UTF8.GetPreamble;
        if (bs.Size > 2) and (Length(u) = 3)
          and (b[0] = u[0]) and (b[1] = u[1]) and (b[2] = u[2])then
        isUTF8 := true;
      end;

      //3rd attempt, look for encoding-attribute
      if not isUTF8 then
      begin
        if (b <> nil) and (l <> -1) then
        if HasUtf8EncodingAttribute(b, l) then
            isUTF8 := true;
      end;

      if isUTF8 then
      begin
        result := TEncoding.UTF8.GetString(b);
        result := RemovePreamble(result);
      end
      else
      begin
        ss := TStringStream.Create(b);
        result := ss.DataString;
        ss.Free;
      end;
      result := EnsureCRLF(result);
    finally
      c.Free;
      bs.Free;
    end;
    except
     on e: Exception do
   begin
     raise EEventMenuException.Create(e.Message);
    end;
  end;
end;

procedure THttpCon.Post(const s: string);
begin
  //not implemented
end;

function THttpCon.HasUtf8EncodingAttribute(b: TBytes; l: Integer): Boolean;
var
  i: Integer;
  c: Integer;
  s: string;
begin
  //look for encoding attribute
  //abc<?xml version="1.0" encoding="utf-8" ?>
  result := false;
  if Assigned(b) then
  begin
    c := System.Math.Min(40, l);
    s := TEncoding.UTF8.GetString(b, 0, c);
    s := LowerCase(s);
    i := Pos('encoding="utf-8"', s);
    result := i > 0;
  end;
end;

function THttpCon.RemovePreamble(const s: string): string;
var
  up: string;
begin
  result := s;
  up := StringOf(TEncoding.UTF8.GetPreamble);
  if (Length(s) > 0) and (s[1] = #$FEFF) then
  begin
    result := Copy(s, 2);
  end
  else if Pos(up, s) = 1 then
  begin
    if Length(s) > 3 then
      result := Copy(s, 4);
  end
  else if Pos('o;?', s) = 1 then
  begin
    if Length(s) > 3 then
      result := Copy(s, 4);
  end;
end;

function THttpCon.EnsureCRLF(const Input: string): string;
begin
  result := AdjustLineBreaks(Input, tlbsCRLF);
end;

end.

