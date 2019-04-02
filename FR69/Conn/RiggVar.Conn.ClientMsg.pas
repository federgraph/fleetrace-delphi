unit RiggVar.Conn.ClientMsg;

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
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient;

function SendMsg(Host: string; Port: Integer; Msg: string; WaitForResult: Boolean; Timeout: Integer): string;

implementation

function SendMsg(Host: string; Port: Integer; Msg: string; WaitForResult: Boolean; Timeout: Integer): string;
var
  TestClient: TIdTCPClient;
  s: string;
begin
  result := '';

  if Host = '' then exit;
  if Port <= 0 then exit;
  if Timeout <= 10 then Timeout := 1000;
  try
    TestClient := TIdTCPClient.Create(nil);
    try
{$IFDEF VER150}
      TestClient.Host := Host;
      TestClient.Port := Port;
      TestClient.Connect(Timeout);
      if TestClient.Connected then
      begin
        TestClient.Write(#2 + Msg + #3);

        if WaitForResult then
        begin
          s := TestClient.ReadLn(#3, 2000) + #3;
          if (s <> '') and (Length(s) > 3) and (s[1] = #2) and (s[Length(s)] = #3) then
            result := Copy(s, 2, Length(s)-2);
        end;
        TestClient.Disconnect;
      end;
{$ELSE}
      TestClient.Host := Host;
      TestClient.Port := Port;
      TestClient.ConnectTimeout := Timeout;
      TestClient.Connect;
      if TestClient.Connected then
      begin
        TestClient.IOHandler.Write(#2 + Msg + #3);
        if WaitForResult then
        begin
          s := TestClient.IOHandler.ReadLn(#3, 2000) + #3;
          if (s <> '') and (Length(s) > 3) and (s[1] = #2) and (s[Length(s)] = #3) then
            result := Copy(s, 2, Length(s)-2);
        end;
        TestClient.Disconnect;
      end;
{$ENDIF}
    finally
      TestClient.Free;
    end;
  except
    on e: Exception do
    begin
      result := 'e';
    end;
  end;
end;

end.
 
