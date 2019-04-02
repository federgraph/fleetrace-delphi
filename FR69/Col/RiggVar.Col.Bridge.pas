unit RiggVar.Col.Bridge;

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
  SysUtils, Classes;

type
  TBridgeProxyType = (
    ptMockBridge,
    ptASPNET,
    ptPHP,
    ptClient,
    ptServer,
    ptSynchron,
    ptREST,
    ptProxy,
    ptCombi
  );

  TBridgeField = (
    bfProxyType,
    bfHost,
    bfPort,
    bfPortHTTP,
    bfUrl,
    bfHomePage,
    bfTaktIn,
    bfTaktOut
  );

  TBridgeItem = class
  private
    function GetComboEntry: string;
  public
    ID: Integer;
    InfoName: string;
    ProxyType: Integer;
    Host: string;
    Port: Integer;
    PortHTTP: Integer;
    Url: string;
    HomePage: string;
    TaktIn: Integer;
    TaktOut: Integer;
    DisplayName: string;
    function Enabled(t: TBridgeProxyType; f: TBridgeField): Boolean;
    procedure Show(Memo: TStrings);
    property ComboEntry: string read GetComboEntry;
  end;

  TBridgeItems = class
  private
    FCurrent: TBridgeItem;
    FItems: array of TBridgeItem;
    function GetBridgeItem(index: Integer): TBridgeItem;
    procedure SetBridgeItem(index: Integer; const Value: TBridgeItem);
    function GetCount: Integer;
    function GetCurrent: TBridgeItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function Add: TBridgeItem;
    function FindItem(ComboEntry: string): TBridgeItem;
    procedure InitProxyType(Combo: TStrings);
    procedure InitBridgeInfo(Combo: TStrings);
    procedure InitBridgeInfoFiltered(Combo: TStrings; pt: TBridgeProxyType);
    property Count: Integer read GetCount;
    property BridgeItem [index: Integer]: TBridgeItem read GetBridgeItem write SetBridgeItem;
    property Current: TBridgeItem read GetCurrent;
  end;

implementation

{ TBridgeItems }

constructor TBridgeItems.Create;
begin
  FCurrent := TBridgeItem.Create;
  Init;
end;

destructor TBridgeItems.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    FItems[i].Free;
  end;
  FCurrent.Free;
end;

function TBridgeItems.FindItem(ComboEntry: string): TBridgeItem;
var
  p, i: Integer;
begin
  p := Pos('=', ComboEntry);
  i := StrToIntDef(Copy(ComboEntry, 1, p - 1), -1);
  result := BridgeItem[i];
end;

function TBridgeItems.GetCount: Integer;
begin
  result := Length(FItems);
end;

function TBridgeItems.GetCurrent: TBridgeItem;
begin
  result := FCurrent;
end;

function TBridgeItems.Add: TBridgeItem;
var
  i: Integer;
begin
  i := Count;
  SetLength(FItems, i + 1);
  result := TBridgeItem.Create;
  result.ID := i;
  BridgeItem[i] := result;
end;

procedure TBridgeItems.SetBridgeItem(index: Integer; const Value: TBridgeItem);
begin
  if (index >= 0) and (index < Count) then
    FItems[index] := Value;
end;

function TBridgeItems.GetBridgeItem(index: Integer): TBridgeItem;
begin
  result := nil;
  if (index >= 0) and (index < Count) then
    result := FItems[index];
end;

procedure TBridgeItems.Init;
var
  cr: TBridgeItem;
begin
  cr := Add;
  cr.InfoName := 'gsup3-asmx';
  cr.ProxyType := 1;
  cr.Host := 'gsup3';
  cr.Port := 0;
  cr.PortHTTP := 0;
  cr.Url := 'http://gsup3/FR88/';
  cr.HomePage := 'FRBridge.aspx';
  cr.TaktIn := 2;
  cr.TaktOut := 2;
  cr.DisplayName := 'gsup3 - asp.net 2.0';

  cr := Add;
  cr.InfoName := 'riggvar.net-asmx';
  cr.ProxyType := 1;
  cr.Host := '';
  cr.Port := 0;
  cr.PortHTTP := 0;
  cr.Url := 'http://riggvar.net/cgi-bin/RiggVar15/';
  cr.HomePage := 'FRBridge.aspx';
  cr.TaktIn := 30;
  cr.TaktOut := 30;
  cr.DisplayName := 'riggvar.net - asp.net 2.0';

  cr := Add;
  cr.InfoName := 'gsup3-php';
  cr.ProxyType := 2;
  cr.Host := '';
  cr.Port := 0;
  cr.PortHTTP := 0;
  cr.Url := 'http://gsup3:3569/FR88/';
  cr.HomePage := 'FR88_FRPage.php';
  cr.TaktIn := 10;
  cr.TaktOut := 10;
  cr.DisplayName := 'gsup3 - D4PHP';

  cr := Add;
  cr.InfoName := 'riggvar.net-php';
  cr.ProxyType := 2;
  cr.Host := '';
  cr.Port := 0;
  cr.PortHTTP := 0;
  cr.Url := 'http://riggvar.net/cgi-bin/FR88PHP/';
  cr.HomePage := 'FR88_FRPage.php';
  cr.TaktIn := 60;
  cr.TaktOut := 60;
  cr.DisplayName := 'riggvar.net - php';

  cr := Add;
  cr.InfoName := 'riggvar.de-php';
  cr.ProxyType := 2;
  cr.Host := '';
  cr.Port := 0;
  cr.PortHTTP := 0;
  cr.Url := 'http://riggvar.de/projectindex/FR88PHP/';
  cr.HomePage := 'FR88_FRPage.php';
  cr.TaktIn := 60;
  cr.TaktOut := 60;
  cr.DisplayName := 'riggvar.de - php';

  cr := Add;
  cr.InfoName := 'ClientBridge';
  cr.ProxyType := 3;
  cr.Host := 'gsup3';
  cr.Port := 4037;
  cr.PortHTTP := 8037;
  cr.Url := 'http://gsup3:8037';
  cr.HomePage := '';
  cr.TaktIn := 0;
  cr.TaktOut := 0;
  cr.DisplayName := 'ClientBridge';

  cr := Add;
  cr.InfoName := 'ServerBridge';
  cr.ProxyType := 4;
  cr.Host := 'gsup3';
  cr.Port := 4530;
  cr.PortHTTP := 8087;
  cr.Url := 'http://gsup3:8087';
  cr.HomePage := '';
  cr.TaktIn := 0;
  cr.TaktOut := 0;
  cr.DisplayName := 'ServerBridge';

  cr := Add;
  cr.InfoName := 'ProxyBridge';
  cr.ProxyType := 7;
  cr.Host := 'gsup3';
  cr.Port := 4030;
  cr.PortHTTP := 8087;
  cr.Url := 'http://gsup3:8087';
  cr.HomePage := '';
  cr.TaktIn := -1;
  cr.TaktOut := -1;
  cr.DisplayName := 'ProxyBridge';

  cr := Add;
  cr.InfoName := 'CombiBridge';
  cr.ProxyType := 8;
  cr.Host := 'gsup3';
  cr.Port := 4030;
  cr.PortHTTP := 8087;
  cr.Url := 'http://gsup3:8087';
  cr.HomePage := '';
  cr.TaktIn := 0;
  cr.TaktOut := 0;
  cr.DisplayName := 'CombiBridge';

  cr := Add;
  cr.InfoName := 'CombiBridge Service';
  cr.ProxyType := 8;
  cr.Host := 'gsup3';
  cr.Port := 4097;
  cr.PortHTTP := 8097;
  cr.Url := 'http://gsup3:8097';
  cr.HomePage := '';
  cr.TaktIn := 0;
  cr.TaktOut := 0;
  cr.DisplayName := 'CombiBridge';

  cr := Add;
  cr.InfoName := 'gsVista-REST.net';
  cr.ProxyType := 6;
  cr.Host := '';
  cr.Port := 0;
  cr.PortHTTP := 0;
  cr.Url := 'http://gsvista/FR88/';
  cr.HomePage := 'FRBridge.aspx';
  cr.TaktIn := 5;
  cr.TaktOut := 5;
  cr.DisplayName := 'gsVista - REST asp.net 3.5';
end;

procedure TBridgeItems.InitBridgeInfo(Combo: TStrings);
var
  cr: TBridgeItem;
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    cr := BridgeItem[i];
    if cr <> nil then
      Combo.Add(cr.ComboEntry);
  end;
end;

procedure TBridgeItems.InitBridgeInfoFiltered(Combo: TStrings;
  pt: TBridgeProxyType);
var
  cr: TBridgeItem;
  i: Integer;
begin
  Combo.Clear;
  for i := 0 to Count - 1 do
  begin
    cr := BridgeItem[i];
    if (cr <> nil) and (cr.ProxyType = Integer(pt)) then
      Combo.Add(cr.ComboEntry);
  end;
end;

procedure TBridgeItems.InitProxyType(Combo: TStrings);
begin
  Combo.Add('0=MockBridge');
  Combo.Add('1=ASP.NET Web Service');
  Combo.Add('2=PHP Web Service');
  Combo.Add('3=ClientBridge');
  Combo.Add('4=ServerBridge');
  Combo.Add('5=SynchronBridge');
  Combo.Add('6=RESTBridge');
  Combo.Add('7=ProxyBridge');
  Combo.Add('8=CombiBridge');
end;

{ TBridgeItem }

function TBridgeItem.Enabled(t: TBridgeProxyType; f: TBridgeField): Boolean;
begin
  result := false;

  case t of
    ptASPNET, ptPHP, ptREST:
      case f of
        bfUrl: result := true;
        bfHomePage: result := true;
        bfTaktIn: result := true;
        bfTaktOut: result := true;
      end;

    ptClient, ptServer, ptSynchron, ptProxy, ptCombi:
      case f of
        bfHost: result := true;
        bfPort: result := true;
        bfPortHTTP: result := true;
        bfUrl: result := true;
      end;
  end;
end;

function TBridgeItem.GetComboEntry: string;
begin
  result := IntToStr(ID) + '=' + DisplayName;
end;

procedure TBridgeItem.Show(Memo: TStrings);
begin
  Memo.Add('ProxyType=' + IntToStr(ProxyType));
  Memo.Add('Host=' + Host);
  Memo.Add('Port=' + IntToStr(Port));
  Memo.Add('PortHTTP=' + IntToStr(PortHTTP));
  Memo.Add('Url=' + Url);
  Memo.Add('HomePage=' + HomePage);
  Memo.Add('TaktIn=' + IntToStr(TaktIn));
  Memo.Add('TaktOut=' + IntToStr(TaktOut));
  Memo.Add('DisplayName=' + DisplayName)
end;

end.
