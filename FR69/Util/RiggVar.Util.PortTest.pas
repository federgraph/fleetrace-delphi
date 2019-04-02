unit RiggVar.Util.PortTest;

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
  SysUtils,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  RiggVar.Util.PortTestModule;

type
  TPortTester = class
  private
    FUseDLL: Boolean;
    PortTesterModule: TPortTesterModule;
    procedure SetUseDLL(const Value: Boolean);
    function TestConnectionWorker(Host: string; Port,
      Timeout: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function TestConnection(Host: string; Port: Integer; Timeout: Integer): Boolean;
    property UseDLL: Boolean read FUseDLL write SetUseDLL;
  end;

implementation

uses
  RiggVar.Util.PortTestCall;

constructor TPortTester.Create;
var
  fn: string;
begin
  fn := 'PT01.dll';
  if FileExists(fn) then
  begin
    PortTesterModule := TPortTesterModule.Create(fn);
    UseDLL := True;
  end;
end;

destructor TPortTester.Destroy;
begin
  PortTesterModule.Free;
  inherited;
end;

procedure TPortTester.SetUseDLL(const Value: Boolean);
begin
  FUseDLL := Value;
end;

function TPortTester.TestConnection(Host: string; Port: Integer; Timeout: Integer): Boolean;
begin
  if UseDLL then
    result := PortTesterModule.TestConnection(Host, Port, Timeout)
  else
    result := TestConnectionWorker(Host, Port, Timeout);
end;

function TPortTester.TestConnectionWorker(Host: string; Port: Integer; Timeout: Integer): Boolean;
var
  TestClient: TIdTCPClient;
begin
  result := false;

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
{$ELSE}
      TestClient.ConnectTimeout := Timeout;
      TestClient.Connect(Host, Port);
      TestClient.Disconnect;
{$ENDIF}
      result := true;
    finally
      TestClient.Free;
    end;
  except
  end;
end;

end.
