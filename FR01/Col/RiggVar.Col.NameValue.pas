unit RiggVar.Col.NameValue;

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
  RiggVar.Util.Classes;

type
  TNameValueNode = class;

  TNameValueFieldType = (
    nvftInteger,
    nvftString,
    nvftBoolean
  );

  TNameValueRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FFieldName: string;
    FFieldValue: string;
    FFieldType: TNameValueFieldType;
    FCaption: string;
    FDescription: string;
    FCategory: string;
    function GetNameValueNode: TNameValueNode;
    procedure SetModified(const Value: Boolean);
  public
    Requesting: Boolean;
    constructor Create(ACollection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TNameValueNode read GetNameValueNode;
  published
    property FieldName: string read FFieldName write FFieldName;
    property FieldValue: string read FFieldValue write FFieldValue;
    property FieldType: TNameValueFieldType read FFieldType write FFieldType;
    property Caption: string read FCaption write FCaption;
    property Description: string read FDescription write FDescription;
    property Category: string read FCategory write FCategory;
  end;

  TNameValueRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TNameValueRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TNameValueRowCollectionItem);
  public
    function Add: TNameValueRowCollectionItem;
    function FindKey(Bib: Integer): TNameValueRowCollectionItem;
    property Items[Index: Integer]: TNameValueRowCollectionItem read GetItem
      write SetItem;
  end;

  TNameValueColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TNameValueBO = class;

  TNameValueNode = class(TBaseNode)
  private
    function GetNameValueRowCollection: TNameValueRowCollection;
    function GetNameValueBO: TNameValueBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    property NameValueRowCollection: TNameValueRowCollection read GetNameValueRowCollection;
    property NameValueBO: TNameValueBO read GetNameValueBO;
  end;

  TNameValueBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
    function CheckInteger(const OldValue: string; const Value: string): string;
    function CheckBoolean(const Value: string): string;
    function CheckString(const OldValue, Value: string): string;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure EditValue(crgs: TBaseRowCollectionItem; var Value: string);
    procedure InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

  TNameValueGrid = class
  private
    SL: TStringList;
    procedure CreateGrid(aParent: TWinControl);
    procedure DestroyGrid;
  protected
    SortColName: string;
  public
    GB: TGridBlock;
    ColBO: TNameValueBO;
    Node: TNameValueNode;
    constructor Create(aParent: TWinControl);
    destructor Destroy; override;
    function GetHTM: string;
    function IsOutputSorted: Boolean;
    procedure SaveHTM(FileName: string);
  end;

implementation

uses
  RiggVar.App.Main;

const
  FieldTypeStrings: array [TNameValueFieldType] of string = (
    'int',
    'string',
    'bool'
  );

{ TNameValueRowCollectionItem }

constructor TNameValueRowCollectionItem.Create(ACollection: TBaseRowCollection);
begin
  inherited;

end;

destructor TNameValueRowCollectionItem.Destroy;
begin

  inherited;
end;

procedure TNameValueRowCollectionItem.Assign(Source: TPersistent);
var
  o: TNameValueRowCollectionItem;
begin
  if Source is TNameValueRowCollectionItem then
  begin
    o := TNameValueRowCollectionItem(Source);
    FieldName := o.FieldName;
    FieldValue := o.FieldValue;
    FieldType := o.FieldType;
    Caption := o.Caption;
    Description := o.Description;
    Category := o.Category;
  end
end;

function TNameValueRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TNameValueRowCollection;
begin
  cl := Collection as TNameValueRowCollection;
  result := cl.BaseNode;
end;

function TNameValueRowCollectionItem.GetNameValueNode: TNameValueNode;
var
  cl: TNameValueRowCollection;
begin
  result := nil;
  cl := Collection as TNameValueRowCollection;
  if cl.BaseNode is TNameValueNode then
    result := TNameValueNode(cl.BaseNode);
end;

procedure TNameValueRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

{ TNameValueRowCollection }

function TNameValueRowCollection.Add: TNameValueRowCollectionItem;
begin
  Result := TNameValueRowCollectionItem(inherited Add);
  Result.BaseID := Count;
  Result.FieldName := 'N' + IntToStr(Count);
end;

function TNameValueRowCollection.FindKey(
  Bib: Integer): TNameValueRowCollectionItem;
var
  i: Integer;
  o: TNameValueRowCollectionItem;
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

function TNameValueRowCollection.GetItem(
  Index: Integer): TNameValueRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TNameValueRowCollectionItem(inherited GetItem(Index));
end;

procedure TNameValueRowCollection.SetItem(Index: Integer;
  const Value: TNameValueRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TNameValueColProp }

procedure TNameValueColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TNameValueRowCollectionItem;
begin
  Value := '';
  if crgs is TNameValueRowCollectionItem then
    cr := TNameValueRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_FieldName' then
    Value := cr.FieldName

  else if NameID = 'col_FieldValue' then
    Value := cr.FieldValue

  else if NameID = 'col_FieldType' then
    Value := FieldTypeStrings[cr.FieldType]

  else if NameID = 'col_Caption' then
    Value := cr.Caption

  else if NameID = 'col_Description' then
    Value := cr.Description

  else if NameID = 'col_Category' then
    Value := cr.Category;
end;

procedure TNameValueColProp.InitColsAvail;
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
  cp.Width := 100;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { FieldValue }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FieldValue';
  cp.Caption := 'Value';
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { FieldType }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FieldType';
  cp.Caption := 'Type';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Caption }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Caption';
  cp.Caption := 'Caption';
  cp.Width := 120;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Description }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Description';
  cp.Caption := 'Description';
  cp.Width := 260;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Category }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Category';
  cp.Caption := 'Category';
  cp.Width := 60;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
end;

{ TNameValueNode }

constructor TNameValueNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TNameValueRowCollection.Create(Self,
    TNameValueRowCollectionItem);
end;

destructor TNameValueNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TNameValueNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TNameValueColProp;
end;

function TNameValueNode.GetNameValueBO: TNameValueBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TNameValueBO;
end;

function TNameValueNode.GetNameValueRowCollection: TNameValueRowCollection;
begin
  result := BaseRowCollection as TNameValueRowCollection;
end;

{ TNameValueBO }

function TNameValueBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TNameValueBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TNameValueBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
var
  cp: TBaseColProp;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    //AddColumn('col_FieldName');
    AddColumn('col_FieldType');
    AddColumn('col_Caption');

    cp := AddColumn('col_FieldValue');
    cp.OnFinishEdit := EditValue;
    cp.ReadOnly := False;

    AddColumn('col_Description');
    AddColumn('col_Category');
  end;
end;

procedure TNameValueBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TNameValueBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TNameValueBO.EditValue(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TNameValueRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TNameValueRowCollectionItem) then
  begin
    cr := TNameValueRowCollectionItem(crgs);
    case cr.FieldType of
      nvftInteger: Value := CheckInteger(cr.FieldValue, Value);
      nvftBoolean: Value := CheckBoolean(Value);
      nvftString: Value := CheckString(cr.FieldValue, Value);
    end;
    cr.FieldValue := Value;
  end;
end;

function TNameValueBO.CheckInteger(const OldValue: string; const Value: string): string;
var
  i: Integer;
begin
  i := StrToIntDef(OldValue, 0);
  i := StrToIntDef(Value, i);
  result := IntToStr(i);
end;

function TNameValueBO.CheckBoolean(const Value: string): string;
var
  b: Boolean;
begin
  b := TUtils.IsTrue(Value);
  result := BoolStr[b];
end;

function TNameValueBO.CheckString(const OldValue, Value: string): string;
begin
  if Length(Value) < 200 then
    result := Value
  else
    result := OldValue;
end;

{ TNameValueGrid }

constructor TNameValueGrid.Create(aParent: TWinControl);
begin
  inherited Create;
  SL := TStringList.Create;
  CreateGrid(aParent);
  SortColName := 'col_FieldName';
end;

destructor TNameValueGrid.Destroy;
begin
  DestroyGrid;
  SL.Free;
  inherited;
end;

procedure TNameValueGrid.CreateGrid(aParent: TWinControl);
begin
  Assert(GB = nil);

  ColBO := TNameValueBO.Create;
  Node := TNameValueNode.Create(ColBO);
  ColBO.CurrentNode := Node;

  GB := TGridBlock.Create;
  GB.Parent := aParent;
  GB.Name := 'NameValueGrid';
  GB.Node := Node;
  GB.ColBO := ColBO;

  GB.BeginInitGrid;

  GB.ColGrid.AlwaysShowCurrent := True;
  GB.ColGrid.ColorSchema := colorBlue;
  GB.ColGrid.AutoMark := True;
  GB.ColGrid.ExcelStyle := true;

  GB.EndInitGrid;
end;

procedure TNameValueGrid.DestroyGrid;
begin
  GB.Free;
  GB := nil;
  Node.Free;
  Node := nil;
  ColBO.Free;
  ColBO := nil;
end;

function TNameValueGrid.GetHTM: string;
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

function TNameValueGrid.IsOutputSorted: Boolean;
begin
  result := GB.ColGrid.ColsActive.SortColIndex > 0;
end;

procedure TNameValueGrid.SaveHTM(FileName: string);
begin
  GetHTM;
  Main.StoreAdapter.StringListSaveToFile(SL, FileName);
  SL.Clear;
end;

end.
