unit RiggVar.Col.NameField;

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
  Vcl.Controls,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Col.Stammdaten,
  RiggVar.DAL.Redirector;

type
  TNameFieldNode = class;

  TNameFieldRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FFieldName: string;
    FCaption: string;
    FMap: Integer;
    FSwap: Integer;
    function GetNameFieldNode: TNameFieldNode;
    procedure SetModified(const Value: Boolean);
  public
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TNameFieldNode read GetNameFieldNode;
  published
    property FieldName: string read FFieldName write FFieldName;
    property Caption: string read FCaption write FCaption;
    property Map: Integer read FMap write FMap;
    property Swap: Integer read FSwap write FSwap;
  end;

  TNameFieldRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TNameFieldRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TNameFieldRowCollectionItem);
  public
    function Add: TNameFieldRowCollectionItem;
    function GetFieldCaptions: string;
    function FindKey(Bib: Integer): TNameFieldRowCollectionItem;
    property Items[Index: Integer]: TNameFieldRowCollectionItem read GetItem
      write SetItem;
  end;

  TNameFieldColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TNameFieldBO = class;

  TNameFieldNode = class(TBaseNode)
  private
    function GetNameFieldRowCollection: TNameFieldRowCollection;
    function GetNameFieldBO: TNameFieldBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Init(sdn: TStammdatenNode);
    property NameFieldRowCollection: TNameFieldRowCollection read GetNameFieldRowCollection;
    property NameFieldBO: TNameFieldBO read GetNameFieldBO;
  end;

  TNameFieldBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure EditCaption(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditMap(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditSwap(crgs: TBaseRowCollectionItem; var Value: string);
    procedure InitColsActiveLayout(StringGrid: TColGrid;
      aLayout: Integer); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

  TNameFieldGrid = class
  private
    SL: TStringList;
    procedure CreateGrid(aParent: TWinControl);
    procedure DestroyGrid;
  protected
    SortColName: string;
  public
    GB: TGridBlock;
    ColBO: TNameFieldBO;
    Node: TNameFieldNode;
    constructor Create(aParent: TWinControl);
    destructor Destroy; override;
    function GetHTM: string;
    function IsOutputSorted: Boolean;
    procedure SaveHTM(FileName: string);
  end;

const
  JS_SkipperF = 1;
  JS_SkipperL = 2;
  JS_SkipperID = 3;
  JS_Club = 4;
  JS_CBYRA = 5;
  JS_USSA = 6;
  JS_Crew1F = 7;
  JS_Crew1L = 8;
  JS_Crew1ID = 9;
  JS_Crew2F = 10;
  JS_Crew2L = 11;
  JS_Crew2ID = 12;

  JS_NameFieldMax = 12;

type
  TNameFieldMapArray = array [1 .. JS_NameFieldMax] of Integer;

  TNameFieldMap = record
    FieldIndex: TNameFieldMapArray;
  public
    function FieldName(FieldID: Integer): string;
    function FieldValue(FieldID: Integer; cr: TStammdatenRowCollectionItem): string;
  end;

var
  NameFieldMap: TNameFieldMap;

implementation

uses
  RiggVar.App.Main;

{ TNameFieldMap }

function TNameFieldMap.FieldName(FieldID: Integer): string;
begin
  case FieldID of
    JS_SkipperF: result := 'Skipper First';
    JS_SkipperL: result := 'Skipper Last';
    JS_SkipperID: result := 'Skipper ID';

    JS_Crew1F: result := 'Crew 1 First';
    JS_Crew1L: result := 'Crew 1 Last';
    JS_Crew1ID: result := 'Crew 1 ID';

    JS_Crew2F: result := 'Crew 2 First';
    JS_Crew2L: result := 'Crew 2 Last';
    JS_Crew2ID: result := 'Crew 2 ID';

    JS_Club: result := 'Club';
    JS_CBYRA: result := 'CBYRA';
    JS_USSA: result := 'USSA';
    else
      result := '';
  end;
end;

function TNameFieldMap.FieldValue(FieldID: Integer; cr: TStammdatenRowCollectionItem): string;
var
  m: Integer;
begin
  if (FieldID > 0) and (FieldID <= JS_NameFieldMax) and Assigned(cr) then
  begin
    m := FieldIndex[FieldID];
    result := cr.FieldValue[m];
  end
  else
    result := '';
end;

{ TNameFieldRowCollectionItem }

constructor TNameFieldRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;

end;

destructor TNameFieldRowCollectionItem.Destroy;
begin

  inherited;
end;

procedure TNameFieldRowCollectionItem.Assign(Source: TPersistent);
var
  o: TNameFieldRowCollectionItem;
begin
  if Source is TNameFieldRowCollectionItem then
  begin
    o := TNameFieldRowCollectionItem(Source);
    FieldName := o.FieldName;
    Caption := o.Caption;
    Map := o.Map;
    Swap := o.Swap;
  end
end;

function TNameFieldRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TNameFieldRowCollection;
begin
  cl := Collection as TNameFieldRowCollection;
  result := cl.BaseNode;
end;

function TNameFieldRowCollectionItem.GetNameFieldNode: TNameFieldNode;
var
  cl: TNameFieldRowCollection;
begin
  result := nil;
  cl := Collection as TNameFieldRowCollection;
  if cl.BaseNode is TNameFieldNode then
    result := TNameFieldNode(cl.BaseNode);
end;

procedure TNameFieldRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

{ TNameFieldRowCollection }

function TNameFieldRowCollection.Add: TNameFieldRowCollectionItem;
begin
  Result := TNameFieldRowCollectionItem(inherited Add);
  Result.BaseID := Count;
  Result.FieldName := 'N' + IntToStr(Count);
  Result.Caption := Result.FieldName;
  Result.Map := Result.BaseID;
end;

function TNameFieldRowCollection.FindKey(
  Bib: Integer): TNameFieldRowCollectionItem;
var
  i: Integer;
  o: TNameFieldRowCollectionItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.BaseID = Bib) then
    begin
      result := o;
      break;
    end;
  end;
end;

function TNameFieldRowCollection.GetFieldCaptions: string;
var
  SL: TStringList;
  i: Integer;
  cr: TNameFieldRowCollectionItem;
begin
  SL := TStringList.Create;
  for i := 0 to Count - 1 do
  begin
    cr := Items[i];
    SL.Add(cr.Caption)
  end;
  result := SL.CommaText;
  SL.Free;
end;

function TNameFieldRowCollection.GetItem(
  Index: Integer): TNameFieldRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TNameFieldRowCollectionItem(inherited GetItem(Index));
end;

procedure TNameFieldRowCollection.SetItem(Index: Integer;
  const Value: TNameFieldRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TNameFieldColProp }

procedure TNameFieldColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TNameFieldRowCollectionItem;
begin
  Value := '';
  if crgs is TNameFieldRowCollectionItem then
    cr := TNameFieldRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_FieldName' then
    Value := cr.FieldName

  else if NameID = 'col_Caption' then
    Value := cr.Caption

  else if NameID = 'col_Swap' then
    Value := IntToStr(cr.Swap)

  else if NameID = 'col_Map' then
    Value := NameFieldMap.FieldName(cr.Map);
end;

procedure TNameFieldColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  { FieldName }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FieldName';
  cp.Caption := 'Name';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Caption }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Caption';
  cp.Caption := 'Caption';
  cp.Width := 100;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Swap }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Swap';
  cp.Caption := 'Swap';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Map }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Map';
  cp.Caption := 'JS FieldMap';
  cp.Width := 100;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
end;

{ TNameFieldNode }

constructor TNameFieldNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TNameFieldRowCollection.Create(Self,
    TNameFieldRowCollectionItem);
end;

destructor TNameFieldNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TNameFieldNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TNameFieldColProp;
end;

function TNameFieldNode.GetNameFieldBO: TNameFieldBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TNameFieldBO;
end;

function TNameFieldNode.GetNameFieldRowCollection: TNameFieldRowCollection;
begin
  result := BaseRowCollection as TNameFieldRowCollection;
end;

procedure TNameFieldNode.Init(sdn: TStammdatenNode);
var
  o: TNameFieldRowCollectionItem;
  i: Integer;
  cl: TStammdatenRowCollection;
begin
  BaseRowCollection.Clear;

  cl := sdn.StammdatenRowCollection;

  for i := 1 to cl.FieldCount do
  begin
    o := NameFieldRowCollection.Add;
    //o.FieldName := 'N' + IntToStr(i); //automatic see Add
    o.Caption := cl.GetFieldCaption(i);
    //o.Map := i; //automatic see Add
  end;
end;

{ TNameFieldBO }

function TNameFieldBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TNameFieldBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TNameFieldBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
var
  cp: TBaseColProp;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    AddColumn('col_FieldName');

    cp := AddColumn('col_Caption');
    cp.OnFinishEdit := EditCaption;
    cp.ReadOnly := False;

    cp := AddColumn('col_Swap');
    cp.OnFinishEdit := EditSwap;
    cp.ReadOnly := False;

    cp := AddColumn('col_Map');
    cp.OnFinishEdit := EditMap;
    cp.ReadOnly := False;
  end;
end;

procedure TNameFieldBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TNameFieldBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TNameFieldBO.EditCaption(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TNameFieldRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TNameFieldRowCollectionItem) then
  begin
    cr := TNameFieldRowCollectionItem(crgs);
    cr.Caption := Value;
    Value := cr.Caption;
  end;
end;

procedure TNameFieldBO.EditMap(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TNameFieldRowCollectionItem;
  t: Integer;
begin
  if (crgs <> nil) and (crgs is TNameFieldRowCollectionItem) then
  begin
    cr := TNameFieldRowCollectionItem(crgs);
    t := StrToIntDef(Value, -1);
    if t = 0 then
      cr.Map := 0;
    if (t > 0) and (t <= JS_NameFieldMax) then
    begin
      cr.Map := t;
      NameFieldMap.FieldIndex[t] := cr.BaseID;
    end;
    Value := NameFieldMap.FieldName(cr.Map);
  end;
end;

procedure TNameFieldBO.EditSwap(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TNameFieldRowCollectionItem;
  t: Integer;
begin
  if (crgs <> nil) and (crgs is TNameFieldRowCollectionItem) then
  begin
    cr := TNameFieldRowCollectionItem(crgs);
    t := StrToIntDef(Value, -1);
    if (t >= 0) and (t <= cr.GetNameFieldNode.NameFieldRowCollection.Count) then
      cr.Swap := StrToIntDef(Value, cr.Swap);
    Value := IntToStr(cr.Swap);
  end;
end;

{ TNameFieldGrid }

constructor TNameFieldGrid.Create(aParent: TWinControl);
begin
  inherited Create;
  SL := TDBStringList.Create;
  CreateGrid(aParent);
  SortColName := 'col_FieldName';
end;

destructor TNameFieldGrid.Destroy;
begin
  DestroyGrid;
  SL.Free;
  inherited;
end;

procedure TNameFieldGrid.CreateGrid(aParent: TWinControl);
begin
  ColBO := TNameFieldBO.Create;
  Node := TNameFieldNode.Create(ColBO);
  ColBO.CurrentNode := Node;

  GB := TGridBlock.Create;
  GB.ColBO := ColBO;
  GB.Node := Node;
  GB.Parent := aParent;
  GB.Name := 'NameFieldGrid';

  GB.BeginInitGrid;
  GB.ColGrid.ColorSchema := colorBlue;
  GB.EndInitGrid;
end;

procedure TNameFieldGrid.DestroyGrid;
begin
  GB.Free;
  GB := nil;
  Node.Free;
  Node := nil;
  ColBO.Free;
  ColBO := nil;
end;

function TNameFieldGrid.GetHTM: string;
begin
  SL.Clear;
  GB.ColGrid.UseHTML := True;
  try
    //Grid.ColsActive.SortColIndex := x;
    GB.ColGrid.UpdateAll;
    GB.ColGrid.Content(SL, '');
    result := SL.Text;
  finally
    GB.ColGrid.UseHTML := False;
  end;
end;

function TNameFieldGrid.IsOutputSorted: Boolean;
begin
  result := GB.ColGrid.ColsActive.SortColIndex > 0;
end;

procedure TNameFieldGrid.SaveHTM(FileName: string);
begin
  GetHTM;
  Main.StoreAdapter.StringListSaveToFile(SL, FileName);
  SL.Clear;
end;

end.
