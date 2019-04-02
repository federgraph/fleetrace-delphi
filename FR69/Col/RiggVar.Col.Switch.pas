unit RiggVar.Col.Switch;

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
  RiggVar.Util.Classes;

type
  TSwitchField = (
    bfHost,
    bfPort,
    bfPortHTTP,
    bfRouterHost,
    bfUseRouterHost
  );

  TSwitchItem = class
  private
    function GetComboEntry: string;
  public
    ID: Integer;
    InfoName: string;
    Host: string;
    Port: Integer;
    PortHTTP: Integer;
    RouterHost: string;
    UseRouterHost: Boolean;
    DisplayName: string;
    procedure Show(Memo: TStrings);
    property ComboEntry: string read GetComboEntry;
  end;

  TSwitchItems = class
  private
    FCurrent: TSwitchItem;
    FItems: array of TSwitchItem;
    function GetSwitchItem(index: Integer): TSwitchItem;
    procedure SetSwitchItem(index: Integer; const Value: TSwitchItem);
    function GetCount: Integer;
    function GetCurrent: TSwitchItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function Add: TSwitchItem;
    function FindItem(ComboEntry: string): TSwitchItem;
    procedure InitProxyInfo(Combo: TStrings);
    property Count: Integer read GetCount;
    property SwitchItem [index: Integer]: TSwitchItem read GetSwitchItem write SetSwitchItem;
    property Current: TSwitchItem read GetCurrent;
  end;

implementation

{ TSwitchItems }

constructor TSwitchItems.Create;
begin
  FCurrent := TSwitchItem.Create;
  Init;
end;

destructor TSwitchItems.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    FItems[i].Free;
  end;
  FCurrent.Free;
end;

function TSwitchItems.FindItem(ComboEntry: string): TSwitchItem;
var
  p, i: Integer;
begin
  p := Pos('=', ComboEntry);
  i := StrToIntDef(Copy(ComboEntry, 1, p - 1), -1);
  result := SwitchItem[i];
end;

function TSwitchItems.GetCount: Integer;
begin
  result := Length(FItems);
end;

function TSwitchItems.GetCurrent: TSwitchItem;
begin
  result := FCurrent;
end;

function TSwitchItems.Add: TSwitchItem;
var
  i: Integer;
begin
  i := Count;
  SetLength(FItems, i + 1);
  result := TSwitchItem.Create;
  result.ID := i;
  SwitchItem[i] := result;
end;

procedure TSwitchItems.SetSwitchItem(index: Integer; const Value: TSwitchItem);
begin
  if (index >= 0) and (index < Count) then
    FItems[index] := Value;
end;

function TSwitchItems.GetSwitchItem(index: Integer): TSwitchItem;
begin
  result := nil;
  if (index >= 0) and (index < Count) then
    result := FItems[index];
end;

procedure TSwitchItems.Init;
var
  cr: TSwitchItem;
begin
{
[SwitchInfo-gsup3-intern]
SwitchHost=gsup3
SwitchPort=4029
SwitchPortHTTP=8085
RouterHost=
UseRouterHost=0
Description=intranet gui switch app
}
  cr := Add;
  cr.InfoName := 'gsup3-intern';
  cr.Host := 'gsup3';
  cr.Port := 4029;
  cr.PortHTTP := 8085;
  cr.RouterHost := '';
  cr.UseRouterHost := false;
  cr.DisplayName := 'intranet gui switch app';

{
[SwitchInfo-HE-extern]
SwitchHost=riggvar.de
SwitchPort=4029
SwitchPortHTTP=8085
RouterHost=rvinfo.dynalias.net
UseRouterHost=1
Description=extranet switch-service
}
  cr := Add;
  cr.InfoName := 'HE-extern';
  cr.Host := 'riggvar.de';
  cr.Port := 4029;
  cr.PortHTTP := 8085;
  cr.RouterHost := 'rvinfo.dynalias.net';
  cr.UseRouterHost := true;
  cr.DisplayName := 'extranet switch-service';
end;

procedure TSwitchItems.InitProxyInfo(Combo: TStrings);
var
  cr: TSwitchItem;
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    cr := SwitchItem[i];
    if cr <> nil then
      Combo.Add(cr.ComboEntry);
  end;
end;

{ TSwitchItem }

function TSwitchItem.GetComboEntry: string;
begin
  result := IntToStr(ID) + '=' + DisplayName;
end;

procedure TSwitchItem.Show(Memo: TStrings);
begin
  Memo.Add('SwitchHost=' + Host);
  Memo.Add('SwitchPort=' + IntToStr(Port));
  Memo.Add('SwitchPortHTTP=' + IntToStr(PortHTTP));
  Memo.Add('RouterHost=' + RouterHost);
  Memo.Add('UseRouterHost=' + BoolStr[UseRouterHost]);
  Memo.Add('DisplayName=' + DisplayName)
end;

end.
