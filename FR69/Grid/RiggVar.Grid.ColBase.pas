unit RiggVar.Grid.ColBase;

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

{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  RiggVar.DAL.Redirector;

type
  TBaseObject = class;

  TBaseObject = class(TPersistent)
  private
    class var
      BaseObjectNextKey: Integer;
      BaseObjectCounter: Integer;
    var
      FKey: Integer;
      FKeyString: string;
  protected
    function GetKey: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function CompareTo(obj: TBaseObject): Integer; virtual;
    function Equals(obj: TObject): Boolean; override;
    function ToString: string; override;
    function GetHashCode: Integer; override;
    function IsBlank: Boolean; virtual;
    procedure Clear; virtual;

    property KeyString: string read GetKey;
  end;

  TBaseObjectCompare = function(const left, right: TBaseObject): Integer of object;

  TArrayList = class(TBaseObject)
  private
    FList: TObjectList;
    function GetItems(i: Integer): TBaseObject;
    procedure SetItems(i: Integer; const Value: TBaseObject);
    function GetCount: Integer;
    procedure SetOwnsObjects(const Value: Boolean);
    function GetOwnsObjects: Boolean;
  public
    constructor Create; overload;
    constructor Create(inOwnsObjects: Boolean); overload;

    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(obj: TBaseObject);
    procedure Remove(obj: TBaseObject);
    procedure AddAll(al: TArrayList);
    procedure RemoveAll(al: TArrayList);
    function Contains(obj: TBaseObject): Boolean;
    procedure Sort; overload;
    procedure Sort(f: TBaseObjectCompare); overload;
    property Items[i: Integer]: TBaseObject read GetItems write SetItems;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TBaseObjectType = class of TBaseObject;

  TBaseList = class(TArrayList)
  private
    FElementType: TBaseObjectType;
    function GetTypedItem(i: Integer): TBaseObject;
    procedure SetTypedItem(i: Integer; const Value: TBaseObject);
  public
    constructor Create(elementType: TBaseObjectType; aOwnsObjects: Boolean);

    function Clone: TBaseList;

    property TypedItem[i: Integer]: TBaseObject read GetTypedItem write SetTypedItem;
    property ElementType: TBaseObjectType read FElementType write FElementType;
  end;

  // The TBaseObjectSort class has some methods that help other
  // classes implement the TArrayList.Sort methods.
  TBaseObjectSort = class
  protected
    class procedure QuickSort(const List: TList; Lo, Hi: Integer; Compare: TBaseObjectCompare);
  public
    class function DefaultCompare(const A, B: TBaseObject): Integer;
    class procedure Sort(const List: TList; Compare: TBaseObjectCompare = nil);
  end;

  StringBuilder = class
  private
    SL: string;
  public
    constructor Create(); overload;
    constructor Create(s: string); overload;
    constructor Create(size: Integer); overload;
    procedure Append(s: string); overload;
    procedure Append(i: Integer); overload;
    procedure Insert(p: Integer; s: string);
    function ToString: string; override;
  end;

  TextWriter = class
  private
    fFileName: string;
    SL: TStringList;
    function GetStrings: TStrings;
  public
    constructor Create(fn: string);
    destructor Destroy; override;
    procedure Write(s: string);
    procedure WriteLine(s: string);
    procedure Flush;
    procedure Close;
    property Strings: TStrings read GetStrings;
  end;

implementation

uses
  RiggVar.App.Main;

{ TBaseObject }

constructor TBaseObject.Create;
begin
  inherited Create;
  Inc(BaseObjectCounter);
  Inc(BaseObjectNextKey);
  FKey := BaseObjectNextKey;
  FKeyString := IntToStr(FKey);
end;

destructor TBaseObject.Destroy;
begin
  Dec(BaseObjectCounter);
  inherited;
end;

function TBaseObject.CompareTo(obj: TBaseObject): Integer;
begin
  if obj = nil then
    result := -1
  else if obj = self then
    result := 0
  else
    result := 1;
end;

function TBaseObject.Equals(obj: TObject): Boolean;
begin
  result := self = obj;
end;

function TBaseObject.GetKey: string;
begin
  result := FKeyString; //IntToStr(FKey);
end;

function TBaseObject.GetHashCode: Integer;
begin
  result := Integer(self);
end;

function TBaseObject.IsBlank: Boolean;
begin
  result := False;
end;

function TBaseObject.ToString: string;
begin
  result := ClassName;
end;

procedure TBaseObject.Clear;
begin
  //virtual;
end;

{ TBaseList }

constructor TBaseList.Create(elementType: TBaseObjectType;
  aOwnsObjects: Boolean);
begin
  inherited Create(aOwnsObjects);
  FElementType := elementType;
end;

function TBaseList.GetTypedItem(i: Integer): TBaseObject;
begin
  result := Items[i] as FElementType;
end;

procedure TBaseList.SetTypedItem(i: Integer; const Value: TBaseObject);
begin
  Items[i] := Value;
end;

function TBaseList.Clone: TBaseList;
var
  i: Integer;
begin
  result := TBaseList.Create(ElementType, false);
  for i := 0 to Count-1 do
    result.Add(Items[i]);
end;

{ TArrayList }

constructor TArrayList.Create(inOwnsObjects: Boolean);
begin
  inherited Create;
  FList := TObjectList.Create(inOwnsObjects);
end;

constructor TArrayList.Create;
begin
  Create(false);
end;

destructor TArrayList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TArrayList.Add(obj: TBaseObject);
begin
  FList.Add(obj);
end;

procedure TArrayList.AddAll(al: TArrayList);
var
  i: Integer;
begin
  for i := 0 to al.Count-1 do
    FList.Add(al.Items[i]);
end;

procedure TArrayList.Clear;
begin
  FList.Clear;
end;

function TArrayList.Contains(obj: TBaseObject): Boolean;
begin
  result := FList.IndexOf(obj) > -1;
end;

function TArrayList.GetCount: Integer;
begin
  result := FList.Count;
end;

function TArrayList.GetItems(i: Integer): TBaseObject;
begin
  if (i >= 0) and (i < Count) then
    result := FList[i] as TBaseObject
  else
    result := nil;
end;

procedure TArrayList.Remove(obj: TBaseObject);
begin
  FList.Remove(obj);
end;

procedure TArrayList.RemoveAll(al: TArrayList);
var
  i: Integer;
begin
  for i := 0 to al.Count-1 do
    Remove(al.Items[i]);
end;

procedure TArrayList.SetItems(i: Integer; const Value: TBaseObject);
begin
  if (i >= 0) and (i < Count) then
    FList[i] := Value;
end;

procedure TArrayList.Sort;
begin
  TBaseObjectSort.Sort(FList);
end;

procedure TArrayList.Sort(f: TBaseObjectCompare);
begin
  TBaseObjectSort.Sort(FList, f);
end;

procedure TArrayList.SetOwnsObjects(const Value: Boolean);
begin
  FList.OwnsObjects := Value;
end;

function TArrayList.GetOwnsObjects: Boolean;
begin
  result := FList.OwnsObjects;
end;

{ TBaseObjectSort }

class function TBaseObjectSort.DefaultCompare(const A, B: TBaseObject): Integer;
begin
  Result := A.CompareTo(B);
end;

class procedure TBaseObjectSort.QuickSort(const List: TList; Lo,
  Hi: Integer; Compare: TBaseObjectCompare);
var
  I, J, Mid: Integer;
begin
  repeat
    I := Lo;
    J := Hi;
    Mid := (Lo + Hi) div 2;
    repeat
      while Compare(List[I], List[Mid]) < 0 do
        Inc(I);
      while Compare(List[J], List[Mid]) > 0 do
        Dec(J);

      if I <= J then
      begin
        List.Exchange(I, J);
        if Mid = I then
          Mid := J
        else if Mid = J then
          Mid := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if Lo < J then
      QuickSort(List, Lo, J, Compare);
    Lo := I;
  until I >= Hi;
end;

class procedure TBaseObjectSort.Sort(const List: TList; Compare: TBaseObjectCompare);
begin
  if List.Count > 1 then
  begin
    if not Assigned(Compare) then
      Compare := DefaultCompare;
    QuickSort(List, 0, List.Count-1, Compare);
  end;
end;

{ StringBuilder }

constructor StringBuilder.Create;
begin
  Create('');
end;

constructor StringBuilder.Create(size: Integer);
begin
  Create('');
end;

constructor StringBuilder.Create(s: string);
begin
  inherited Create;
  SL := s;
end;

function StringBuilder.ToString: string;
begin
  result := SL;
end;

procedure StringBuilder.Append(s: string);
begin
  SL := SL + s;
end;

procedure StringBuilder.Append(i: Integer);
begin
  Append( IntToStr(i));
end;

procedure StringBuilder.Insert(p: Integer; s: string);
begin
  SL := Copy(SL, 1, p-1) + s + Copy(SL, p, Length(s));
end;

{ TextWriter }

procedure TextWriter.Close;
begin
  if fFileName <> '' then
    Main.StoreAdapter.StringListSaveToFile(SL, fFileName);
end;

constructor TextWriter.Create(fn: string);
begin
  inherited Create;
  fFileName := fn;
  SL := TDBStringList.Create;
end;

destructor TextWriter.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TextWriter.Flush;
begin
  //
end;

function TextWriter.GetStrings: TStrings;
begin
  result := SL;
end;

procedure TextWriter.Write(s: string);
begin
  SL[SL.Count-1] := SL[SL.Count-1] + s;
end;

procedure TextWriter.WriteLine(s: string);
begin
  SL.Add(s);
end;

end.

