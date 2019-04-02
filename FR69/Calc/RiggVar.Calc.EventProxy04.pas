unit RiggVar.Calc.EventProxy04;

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
  SysUtils, Classes,
  RiggVar.Calc.EventProxy00,
  RiggVar.Calc.EventProxyCall,
  RiggVar.Calc.EV,
  RiggVar.Calc.EventProxy03,
  RiggVar.Col.Event,
  RiggVar.BO.Def,
  RiggVar.BO.IniImage,
  RiggVar.App.Config,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  { Proxy for scoring via Indy socket using Xml }
  TCalcEventProxy4 = class(TDLLCalcEventProxy)
  private
    procedure ScoreRegattaXml(p: TFRProxy);
  protected
    function ScoreRegattaRemote(xml: string): string; virtual;
  public
    Host: string;
    Port: Integer;
    Timeout: Integer;
    constructor Create;
    procedure Calc(aqn: TEventNode); override;
    function GetProxyXmlOutput(aqn: TEventNode): string; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Calc.EventProxyWriter,
  RiggVar.Calc.EventProxyReader;

{ TCalcEventProxy4 }

procedure TCalcEventProxy4.Calc(aqn: TEventNode);
begin
  try
    en := aqn;
    EventProps := BO.EventProps;

    if en.EventRowCollection.Count = 0 then exit;

    p := TFRProxy.Create;
    try
      LoadProxy;
      ScoreRegattaXml(p);
      if WithTest then
      begin
        WithTest := false;
        CheckResult(p);
      end;
      UnLoadProxy;
    finally
      p.Free;
      p := nil;
    end;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy4.Calc';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

procedure TCalcEventProxy4.ScoreRegattaXml(p: TFRProxy);
var
  xml, xml2: string;
  t: TFRProxyTestWriter;
  r: TFRProxyTestReader;
begin
  t := TFRProxyTestWriter.Create;
  xml := t.WriteXml1(p);

  xml2 := ScoreRegattaRemote(xml);

  r := TFRProxyTestReader.Create;
  r.ReadXMLString(xml2);
  p.Assign(r.Proxy);

  t.Free;
  r.Free;
end;

function TCalcEventProxy4.ScoreRegattaRemote(xml: string): string;
var
  TestClient: TIdTCPClient;
  s: string;
begin
  result := xml;

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
        TestClient.MaxLineLength := 128 * 1024;
        TestClient.Write(#2 + xml + #3);
        s := TestClient.ReadLn(#3, 2000) + #3;
        if (s <> '') and (Length(s) > 3) and (s[1] = #2) and (s[Length(s)] = #3) then
          result := Copy(s, 2, Length(s)-2);
        TestClient.Disconnect;
      end;
{$ELSE}
      TestClient.Host := Host;
      TestClient.Port := Port;
      TestClient.ConnectTimeout := Timeout;
      TestClient.Connect;
      if TestClient.Connected then
      begin
        TestClient.IOHandler.MaxLineLength := 128 * 1024;;
        TestClient.IOHandler.Write(#2 + xml + #3);
        s := TestClient.IOHandler.ReadLn(#3, 2000) + #3;
        if (s <> '') and (Length(s) > 3) and (s[1] = #2) and (s[Length(s)] = #3) then
          result := Copy(s, 2, Length(s)-2);
        TestClient.Disconnect;
      end;
{$ENDIF}
    finally
      TestClient.Free;
    end;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy4.ScoreRegattaRemote';
      _ScoringExceptionMessage := e.Message;
    end;
  end;

end;

constructor TCalcEventProxy4.Create;
begin
  inherited Create;
  if Assigned(Main.IniImage) then
  begin
    Host := Main.IniImage.CalcHost;
    Port := Main.IniImage.CalcPort;
    Timeout := 1000;
  end;
end;

function TCalcEventProxy4.GetProxyXmlOutput(aqn: TEventNode): string;
var
  t: TFRProxyTestWriter;
  xml: string;
begin
  result := 'ProxyXmlOutput';
  try
    en := aqn;
    EventProps := BO.EventProps;

    if en.EventRowCollection.Count = 0 then exit;

    t := nil;
    p := TFRProxy.Create;
    try
      LoadProxy;
      t := TFRProxyTestWriter.Create;
      xml := t.WriteXml1(p);
      result := ScoreRegattaRemote(xml);
    finally
      p.Free;
      p := nil;
      t.Free;
    end;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEventProxy4.GetProxyXmlOuput';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

end.
