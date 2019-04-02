unit RiggVar.Util.WebUtils;

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
  Winapi.Windows,
  Winapi.WinSock,
  System.AnsiStrings,
  System.SysUtils,
  System.Math,
  IdHTTP,
  IdCoderMIME,
  IdStack,
  IdGlobal;

type
  WebUtils = class
    class function Base64ToStr(const s: string): string;
    class function StrToBase64(const s: string): string;

    class function GetDisplayString(const s: string): string;
    class function RemoveCtrlChars(const s: string): string;

    class function HasUtf8EncodingAttribute(b: TBytes; l: Integer): Boolean; static;
    class function IsUtf8(const s: string): Boolean; static;
    class function RemovePreamble(const s: string): string; static;
    class function EnsureCRLF(const Input: string): string;

    class function GetLocalHostName: string;
    class function ResolveHost(Host: string): string;

    class function HTMLEncode(AStr: string): string;
    class function Get(AUrl: string): string; static;
    class function GetPublicIP: string; static;
    class function GetPublicIPTest: string; static;
  end;

implementation

class function WebUtils.StrToBase64(const s: string): string;
var
  Encoder: TIdEncoderMime;
begin
  Encoder := TIdEncoderMime.Create(nil);
  try
    try
  result := Encoder.Encode(s);
    except
      result := '';
    end;
  finally
  Encoder.Free;
end;
end;

class function WebUtils.Base64ToStr(const s: string): string;
var
  Decoder: TIdDecoderMime;
begin
  Decoder := TIdDecoderMime.Create(nil);
  result := Decoder.DecodeString(s);
  Decoder.Free;
end;

class function WebUtils.RemoveCtrlChars(const s: string): string;
var
  i: Integer;
  C: Char;
  b: Byte;
  temp: string;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    C := s[i];
    b := Byte(C);
    if b < 32 then
      temp := '' //temp := Format('<%d>', [b])
    else
      temp := C;
    Result := Result + temp;
  end;
end;

class function WebUtils.GetDisplayString(const s: string): string;
var
  i: Integer;
  C: Char;
  b: Byte;
  temp: string;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    C := s[i];
    b := Byte(C);
    if b < 32 then
      temp := Format('<%d>', [b])
    else
      temp := C;
    Result := Result + temp;
  end;
end;

class function WebUtils.IsUtF8(const s: string): Boolean;
begin
  result := (Length(s) > 0) and (s[1] = #$FEFF);
end;

class function WebUtils.RemovePreamble(const s: string): string;
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

class function WebUtils.EnsureCRLF(const Input: string): string;
begin
  result := AdjustLineBreaks(Input, tlbsCRLF);
end;

class function WebUtils.GetLocalHostName: string;
var
  LocalName: array [0..255] of AnsiChar;
  HWSAData: TWSADATA;
  ret: Integer;
begin
  result := 'localhost';
  if WSAStartup($0101, HWSAData) = 0 then
  begin
    ret := GetHostname(@LocalName, SizeOf(LocalName));
    if ret = SOCKET_ERROR then  //if ret = -1
    begin
      ret := WSAGetLastError;
      OutputDebugString(PChar('GetLocalHostName: ret= ' + IntToStr(ret)));
    end
    else if ret = 0 then
      result := string(LocalName);
    WSACleanup;
  end;
end;

class function WebUtils.ResolveHost(Host: string): string;
begin
  result := GStack.ResolveHost(Host, Id_IPv4);
end;

class function WebUtils.HasUtf8EncodingAttribute(b: TBytes; l: Integer): Boolean;
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
    c := Min(40, l);
    s := TEncoding.UTF8.GetString(b, 0, c);
    s := LowerCase(s);
    i := Pos('encoding="utf-8"', s);
    result := i > 0;
  end;
end;

class function WebUtils.HTMLEncode(AStr: string): string;
{
QC 73179

Create a new function and change the calls to FormatBuf(...)
From:
FormatBuf(Rp^, 5, '&amp;', 5, []);
To
FormatBuf(Rp^, 5*2, '&amp;', 5*2, []);
}
const
  Convert = ['&','<','>','"'];
var
  Sp, Rp: PChar;
begin
  SetLength(Result, Length(AStr) * 10);
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '&': begin
             System.AnsiStrings.FormatBuf(Rp^, 5*2, '&amp;', 5*2, []);
             Inc(Rp,4);
           end;
      '<',
      '>': begin
             if Sp^ = '<' then
               System.AnsiStrings.FormatBuf(Rp^, 4*2, '&lt;', 4*2, [])
             else
               System.AnsiStrings.FormatBuf(Rp^, 4*2, '&gt;', 4*2, []);
             Inc(Rp,3);
           end;
      '"': begin
             System.AnsiStrings.FormatBuf(Rp^, 6*2, '&quot;', 6*2, []);
             Inc(Rp,5);
           end;
    else
      Rp^ := Sp^
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

class function WebUtils.Get(AUrl: string): string;
var
  d: TIdHTTP;
begin
  result := 'error';
  d := TIdHTTP.Create(nil);
  try
    d.ConnectTimeout := 5000;
    try
      d.Request.ContentType := '';
      result := d.Get(AUrl);
    except
     on e: Exception do
      result := e.Message;
    end;
  finally
    d.Free;
  end;
end;

function ProcessIPQueryAnswer(a: string): string;
var
  s, t: string;
  t1, t2: string;
  p1, p2: Integer;
begin
  result := '127.0.0.1';
  s := LowerCase(a);
  t1 := '<pre>';
  t2 := '</pre>';
  p1 := Pos(t1, s);
  p2 := Pos(t2, s);
  if (p1 > 0) and (p2 > 0) and (p2 - p1 > 10) then
  begin
    p1 := p1 + Length(t1);
    t := Copy(s, p1, p2 - p1);
    result := t;
  end;
end;

class function WebUtils.GetPublicIPTest: string;
var
  r, a, t: string;
begin
  result := '127.0.0.1';
  r := 'http://www.riggvar.de/public-ip.html';
  a := Get(r);
  t := ProcessIPQueryAnswer(a);
  t := MakeCanonicalIPv4Address(t);
  if t <> '' then
    result := t;
end;

class function WebUtils.GetPublicIP: string;
var
  r, s, t: string;
begin
  result := '127.0.0.1';
  r := 'http://169.254.169.254/latest/meta-data/public-ipv4';
  s := Get(r);
  t := MakeCanonicalIPv4Address(s);
  if t <> '' then
    result := t;
end;

end.

