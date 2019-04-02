unit RiggVar.Conn.Utils;

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
  System.Win.ScktComp;

function SendMsg(Socket: TCustomWinSocket;
  s: string; UseUnicode: Boolean = True): Boolean;

function ReceiveText(Sender: TObject; Socket: TCustomWinSocket): string;

implementation

uses
  RiggVar.App.Main;

function ReceiveText(Sender: TObject; Socket: TCustomWinSocket): string;
var
  s: AnsiString;
begin
  SetLength(s, Socket.ReceiveLength);
  SetLength(s, Socket.ReceiveBuf(Pointer(s)^, Length(s)));
  result := string(s);
end;

function SendMsg_Ansi(Socket: TCustomWinSocket; s: string): Boolean;
var
  ansi: AnsiString;
  i: Integer;
begin
  result := false;
  ansi := AnsiString(s);
  if Assigned(Socket)  then
  begin
    i := Socket.SendText(AnsiChar(#2) + ansi + AnsiChar(#3));
    result := i = Length(s) + 2;
  end;
end;

function SendMsg_UTF8(Socket: TCustomWinSocket; s: string): Boolean;
var
  i: Integer;
  utf8: Utf8String;
  ansi: AnsiString;
begin
  result := false;
  utf8 := UTF8Encode(s);
  utf8 := AnsiChar(#2) + utf8 + AnsiChar(#3);
  ansi := AnsiString(utf8);
  if Assigned(Socket)  then
  begin
    i := Socket.SendText(ansi);
    result := i = Length(ansi);
  end;
end;

function SendMsg_MemStream(Socket: TCustomWinSocket; s: string): Boolean;
var
  utf8: Utf8String;
  bom: TBytes;
  b: byte;
  ms: TMemoryStream;
begin
  result := false;
  ms := TMemoryStream.Create;
  try
    b := 2;
    ms.Write(b, SizeOf(b));
    bom := TEncoding.UTF8.GetPreamble;
    ms.Write(bom[0], Length(bom));
    utf8 := UTF8Encode(s);
    ms.Write(Pointer(utf8)^, Length(utf8));
    b := 3;
    ms.Write(b, SizeOf(b));
    if Assigned(Socket)  then
    begin
      result := Socket.SendStream(ms);
    end;
  finally
    ms.Free;
  end;
end;

function SendMsg(Socket: TCustomWinSocket; s: string; UseUnicode: Boolean): Boolean;
var
  uni: Boolean;
begin
  uni := UseUnicode and Main.IniImage.UseUnicode;
  if uni then
    result := SendMsg_UTF8(Socket, s)
  else
    result := SendMsg_ANSI(Socket, s);
end;

end.
