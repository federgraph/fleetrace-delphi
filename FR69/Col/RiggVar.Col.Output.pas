unit RiggVar.Col.Output;

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
  TOutputField = (
    bfHost,
    bfPort
  );

  TOutputItem = class
  private
    function GetComboEntry: string;
  public
    ID: Integer;
    InfoName: string;
    Host: string;
    Port: Integer;
    DisplayName: string;
    procedure Show(Memo: TStrings);
    property ComboEntry: string read GetComboEntry;
  end;

  TOutputItems = class
  private
    FCurrent: TOutputItem;
    FItems: array of TOutputItem;
    function GeTOutputItem(index: Integer): TOutputItem;
    procedure SeTOutputItem(index: Integer; const Value: TOutputItem);
    function GetCount: Integer;
    function GetCurrent: TOutputItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function Add: TOutputItem;
    function FindItem(ComboEntry: string): TOutputItem;
    procedure InitProxyInfo(Combo: TStrings);
    property Count: Integer read GetCount;
    property OutputItem [index: Integer]: TOutputItem read GeTOutputItem write SeTOutputItem;
    property Current: TOutputItem read GetCurrent;
  end;

implementation

{ TOutputItems }

constructor TOutputItems.Create;
begin
  FCurrent := TOutputItem.Create;
  Init;
end;

destructor TOutputItems.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    FItems[i].Free;
  end;
  FCurrent.Free;
end;

function TOutputItems.FindItem(ComboEntry: string): TOutputItem;
var
  p, i: Integer;
begin
  p := Pos('=', ComboEntry);
  i := StrToIntDef(Copy(ComboEntry, 1, p - 1), -1);
  result := OutputItem[i];
end;

function TOutputItems.GetCount: Integer;
begin
  result := Length(FItems);
end;

function TOutputItems.GetCurrent: TOutputItem;
begin
  result := FCurrent;
end;

function TOutputItems.Add: TOutputItem;
var
  i: Integer;
begin
  i := Count;
  SetLength(FItems, i + 1);
  result := TOutputItem.Create;
  result.ID := i;
  OutputItem[i] := result;
end;

procedure TOutputItems.SetOutputItem(index: Integer; const Value: TOutputItem);
begin
  if (index >= 0) and (index < Count) then
    FItems[index] := Value;
end;

function TOutputItems.GetOutputItem(index: Integer): TOutputItem;
begin
  result := nil;
  if (index >= 0) and (index < Count) then
    result := FItems[index];
end;

procedure TOutputItems.Init;
var
  cr: TOutputItem;
begin
  cr := Add;
  cr.InfoName := 'gsup3-Delphi';
  cr.Host := 'gsup3';
  cr.Port := 3428;
  cr.DisplayName := 'gsup3 delphi';

  cr := Add;
  cr.InfoName := 'gsup3-VS2005';
  cr.Host := 'riggvar.de';
  cr.Port := 6228;
  cr.DisplayName := 'gsup3 VS2005';

  cr := Add;
  cr.InfoName := 'gsup3-Java';
  cr.Host := 'riggvar.de';
  cr.Port := 3028;
  cr.DisplayName := 'gsup3 Java';
end;

procedure TOutputItems.InitProxyInfo(Combo: TStrings);
var
  cr: TOutputItem;
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    cr := OutputItem[i];
    if cr <> nil then
      Combo.Add(cr.ComboEntry);
  end;
end;

{ TOutputItem }

function TOutputItem.GetComboEntry: string;
begin
  result := IntToStr(ID) + '=' + DisplayName;
end;

procedure TOutputItem.Show(Memo: TStrings);
begin
  Memo.Add('Host=' + Host);
  Memo.Add('Port=' + IntToStr(Port));
  Memo.Add('DisplayName=' + DisplayName)
end;

end.
