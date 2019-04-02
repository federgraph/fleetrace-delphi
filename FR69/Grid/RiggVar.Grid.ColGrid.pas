unit RiggVar.Grid.ColGrid;

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
  System.UITypes,
  Vcl.Controls,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColBase;

type
  TCCC = (
		cccBlank,
		cccDefaultColor,
		cccAlternatingColor,
		cccFocusColor,
		cccEditableColor,
		cccAlternatingEditableColor,
		cccCurrentColor,
		cccTransColor,
		cccHeaderColor,
		cccCustomColor
  );

  TCellProp = class
  private
    function GetHasGroup: Boolean;
  public
    Alignment: TAlignment;
    Color: TColor;
    GroupColor: TColor;
    HTMLColor: string;
    ColorClass: TCCC;
    ShowGroup: Boolean;
    property HasGroup: Boolean read GetHasGroup;
  end;

  TCellProps = class
  private
    BlockSize: Integer;
    FCount: Integer;
    T: array of TCellProp;
    FColCount: Integer;
    procedure Grow(i: Integer);
    procedure SetColCount(const Value: Integer);
  protected
    function GetCellProp(ACol, ARow: Integer): TCellProp;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property CellProp[ACol, ARow: Integer]: TCellProp read GetCellProp;
    property ColCount: Integer read FColCount write SetColCount;
  end;

  IColGrid = interface
    procedure SetupGrid;
    procedure ShowData;
    procedure CancelEdit;
    procedure InvalidateGrid;

    function GetCells(c, r: Integer): string;
    function GetColCount: Integer;
    function GetEnabled: Boolean;
    function GetFixedRows: Integer;
    function GetRow: Integer;
    function GetRowCount: Integer;
    function GetHeaderRowIndex: Integer;

    procedure SetCells(c, r: Integer; const Value: string);
    procedure SetColCount(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFixedRows(const Value: Integer);
    procedure SetHeaderRowIndex(Value: Integer);
    procedure SetRow(const Value: Integer);
    procedure SetRowCount(const Value: Integer);

    property HeaderRowIndex: Integer read GetHeaderRowIndex write SetHeaderRowIndex;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property Cells[c, r: Integer]: string read GetCells write SetCells;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FixedRows: Integer read GetFixedRows write SetFixedRows;
    property Row: Integer read GetRow write SetRow;
  end;

  TBaseColProp = class;
  TBaseRowCollection = class;
  TColGrid = class;

  { row object, BaseID is the primary key }
  TBaseRowCollectionItem = class(TBaseObject)
  private
    FBaseID: Integer;
    FCollection: TBaseRowCollection;
    FID: Integer;
    function GetIndex: Integer;
  protected
    function GetOwner: TPersistent; override;
    procedure SetCollection(Value: TBaseRowCollection); virtual;
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(Collection: TBaseRowCollection); virtual;
    destructor Destroy; override;
    procedure ClearList; virtual;
    procedure ClearResult; virtual;
    function IsInFilter: Boolean; virtual;
    procedure UpdateCellProp(cp: TBaseColProp; cellProp: TCellProp); virtual;
    function ColumnToColorDef(cp: TBaseColProp; aColor: TColor): TColor; virtual;
    property Collection: TBaseRowCollection read FCollection write SetCollection;
    property ID: Integer read FID;
    property Index: Integer read GetIndex write SetIndex;
  published
    property BaseID: Integer read FBaseID write FBaseID;
  end;

  TBaseRowCollectionItemClass = class of TBaseRowCollectionItem;
  TBaseNode = class;

  TBaseRowCollectionEnumerator = class
  private
    FIndex: Integer;
    FCollection: TBaseRowCollection;
  public
    constructor Create(ACollection: TBaseRowCollection);
    function GetCurrent: TBaseRowCollectionItem;
    function MoveNext: Boolean;
    property Current: TBaseRowCollectionItem read GetCurrent;
  end;

  { table object, a collection of rows, owned by TBaseNode }
  TBaseRowCollection = class(TPersistent)
  private
    FOwner: TPersistent;
    FItemClass: TBaseRowCollectionItemClass;
    FItems: TList;
    FNextID: Integer;
    function GetBase(Index: Integer): TBaseRowCollectionItem;
    procedure SetBase(Index: Integer; const Value: TBaseRowCollectionItem);
    function GetBaseNode: TBaseNode;
    function GetCount: Integer;
  protected
    function GetOwner: TPersistent; override;
    procedure InsertItem(Item: TBaseRowCollectionItem);
    procedure RemoveItem(Item: TBaseRowCollectionItem);
    property NextID: Integer read FNextID;
    function GetItem(Index: Integer): TBaseRowCollectionItem;
    procedure SetItem(Index: Integer; Value: TBaseRowCollectionItem);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TBaseRowCollectionItemClass);
    destructor Destroy; override;
    function Add: TBaseRowCollectionItem;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function FindItemID(ID: Integer): TCollectionItem;
    function GetEnumerator: TBaseRowCollectionEnumerator;
    function Insert(Index: Integer): TBaseRowCollectionItem;
    property Count: Integer read GetCount;
    property ItemClass: TBaseRowCollectionItemClass read FItemClass;
    property Items[Index: Integer]: TBaseRowCollectionItem read GetItem write SetItem;

    function IndexOf(Item: TCollectionItem): Integer;
    function FindBase(const BaseID: Integer): TBaseRowCollectionItem; virtual;
    function FilteredCount: Integer; virtual;
    procedure ClearList; virtual;
    procedure ClearResult; virtual;
    property Base[Index: Integer]: TBaseRowCollectionItem read GetBase write SetBase; default;
    property BaseNode: TBaseNode read GetBaseNode;
  end;

  { Eventhandler definitions
    - the Grid will fire these on the business object.
    - these events pass the row object as parameter
    - no need to indicate the column as parameter because
      there will be a separate eventhandler for each editable field of a row.
      The eventhandler can be assigned to the column property object.
      The eventhandler will usually be implemented in the business object.
  }
  TBaseSetTextEvent = procedure(cr: TBaseRowCollectionItem; const Value: string) of object;
  TBaseGetTextEvent = procedure(cr: TBaseRowCollectionItem; var Value: string) of object;
  TBaseGetTextEvent2 = procedure(cr: TBaseRowCollectionItem; var Value: string; const ColName: string) of object;
  TFinishEditCREvent = procedure(cr: TBaseRowCollectionItem) of object;
  TCellSelectEvent = procedure (Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean) of object;
  TRowChangedEvent = procedure (Sender: TObject; ARow: Longint) of object;

  TColType = (
    colTypeInteger,
    colTypeString,
    colTypeRank,
    colTypeMemo
    );

  TColCaptionBag = class
  private
    FSL: TStringList;
    function GetCount: Integer;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    IsPersistent: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetCaption(const key: string): string;
    procedure SetCaption(const key: string; value: string);
    property Count: Integer read GetCount;
    property Text: string read GetText write SetText;
  end;

  { This class defines column properties,
    there will be one object for every field of the row object.
    This is an abstract class, you need to override at least
    - InitColsAvail
    - GetTextDefault
  }
  TBaseColProp = class(TCollectionItem)
  private
    FNameID: string;
    FNumID: Integer; //for better performance of method GetTextDefault
    FCaption: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FVisible: Boolean;
    FSortable: Boolean;
    FOnGetSortKey: TBaseGetTextEvent;
    FOnGetSortKey2: TBaseGetTextEvent2;
    FOnGetText: TBaseGetTextEvent;
    FOnSetText: TBaseSetTextEvent;
    FReadOnly: Boolean;
    FOnFinishEdit: TBaseGetTextEvent;
    FOnFinishEdit2: TBaseGetTextEvent2;
    FColType: TColType;
    FDescending: Boolean;
    function GetCaption: string;
    procedure SetNameID(const Value: string);
  public
    constructor Create(ACollection: TCollection); override;
    procedure InitColsAvail; virtual;
    procedure GetTextDefault(cr: TBaseRowCollectionItem; var Value: string); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure GetSortKey(cr: TBaseRowCollectionItem; var SortKey: string);
    function GetText(cr: TBaseRowCollectionItem): string;
  published
    property Width: Integer read FWidth write FWidth default 35;
    property Visible: Boolean read FVisible write FVisible default True;
    property Sortable: Boolean read FSortable write FSortable;
    property NameID: string read FNameID write SetNameID;
    property NumID: Integer read FNumID write FNumID;
    property Caption: string read GetCaption write FCaption;
    property Alignment: TAlignment read FAlignment write FAlignment default taRightJustify;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property OnGetSortKey: TBaseGetTextEvent read FOnGetSortKey write FOnGetSortKey;
    property OnGetSortKey2: TBaseGetTextEvent2 read FOnGetSortKey2 write FOnGetSortKey2;
    property OnGetText: TBaseGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TBaseSetTextEvent read FOnSetText write FOnSetText;
    property OnFinishEdit: TBaseGetTextEvent read FOnFinishEdit write FOnFinishEdit;
    property OnFinishEdit2: TBaseGetTextEvent2 read FOnFinishEdit2 write FOnFinishEdit2;
    property ColType: TColType read FColType write FColType;
    property Descending: Boolean read FDescending write FDescending;
  end;

  {  The client code must create and assign a concrete ColProp class:
       StringGrid.ColPropClass := TMyColProp;
       StringGrid.ColsAvail.Init;
     TBaseColProps.Init will call the virtual method TBaseColProps.InitColsAvail
     and this is how the StringGrid caches column info.
  }
  TBaseColPropClass = class of TBaseColProp;

  { container for column property objects
    the StringGrid will maintain two ColProps Collections
    - Available Columns
    - Active Columns
  }
  TBaseColProps = class(TOwnedCollection)
  private
    FSortColIndex: Integer;
    function GetVisibleCount: Integer;
    function GetByName(NameIndex: string): TBaseColProp;
    function GetItem(Index: Integer): TBaseColProp;
    procedure SetItem(Index: Integer; const Value: TBaseColProp);
    procedure SetSortColIndex(const Value: Integer);
    function GetSortColIndex: Integer;
    function GetGridName: string;
    function GetCaptionOverride(cp: TBaseColProp): string;
    function Grid: TColGrid;
  public
    UseCustomColCaptions: Boolean;
    function IsDuplicateNameID(s: string): Boolean;
    function Add: TBaseColProp;
    procedure Init;
    procedure InitCustomCaptions;
    procedure UpdateRow(AGrid: IColGrid; ARow: Integer; cr: TBaseRowCollectionItem);
    property VisibleCount: Integer read GetVisibleCount;
    property ByName[NameIndex: string]: TBaseColProp read GetByName;
    property Items[Index: Integer]: TBaseColProp read GetItem write SetItem; default;
    property SortColIndex: Integer read GetSortColIndex write SetSortColIndex;
    property GridName: string read GetGridName;
  end;

  { forward declaration, every node knows about the BO }
  TBaseColBO = class;

  { The application may build a tree or list of Nodes
    with each Node having a BaseRowCollection }
  TBaseNode = class(TBaseObject)
  private
    FBaseColBO: TBaseColBO;
    FCollection: TBaseRowCollection;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    procedure SetBaseRowCollection(const Value: TBaseRowCollection);
    procedure SetOnModified(const Value: TNotifyEvent);
  protected
    function GetBaseRowCollection: TBaseRowCollection; virtual;
    function GetBaseColPropClass: TBaseColPropClass; virtual;
    function GetBaseColBO: TBaseColBO; virtual;
    procedure SetModified(const Value: Boolean); virtual;
  public
    NameID: string;
    Layout: Integer;
    constructor Create(aOwner: TBaseColBO);
    function Owner: TPersistent;
    procedure ShowTabs(Tabs: TStrings); virtual;
    procedure Calc; virtual;
    property BaseRowCollection: TBaseRowCollection read GetBaseRowCollection write SetBaseRowCollection;
    property BaseColBO: TBaseColBO read GetBaseColBO;
    property ColPropClass: TBaseColPropClass read GetBaseColPropClass;
    property Modified: Boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write SetOnModified;
  end;

  { The StringGrid is able to highlight the current.
    Only the business object knows who is current,
    i.e. which Row in which node's RowCollection.
  }
  TBaseColBO = class(TBaseObject)
  protected
    function GetCurrentRow: TBaseRowCollectionItem; virtual;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); virtual;
    function GetCurrentNode: TBaseNode; virtual;
    procedure SetCurrentNode(const Value: TBaseNode); virtual;
  public
    procedure InitColsActive(StringGrid: TColGrid); virtual;
    procedure InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer); virtual;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

  { The StringGrid can paint itself, it only needs to know what
    RowCollection to pull the data from.
    You need to assign a function of the following type to the StringGrid.
    Note that each RowCollection is owned by a node.
  }
  TGetBaseNodeFunction = function: TBaseNode of object;

  { for debugging only }
  TTraceProcedure = procedure(s: string) of object;

  TCellClickEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;

  TDisplayOrderList = class(TStringList)
  private
    FDescending: Boolean;
    function GetDisplayIndex(j: Integer): Integer;
    procedure SetDescending(const Value: Boolean);
  public
    procedure Sort; override;
    property DisplayIndex [Pos: Integer]: Integer read GetDisplayIndex;
    property Descending: Boolean read FDescending write SetDescending;
  end;

  TColGrid = class(TPersistent)
  private
    FColPropClass: TBaseColPropClass;
    //
    FColBODefault: TBaseColBO;
    FColsActiveDefault: TBaseColProps;
    //
    FColBO: TBaseColBO; //reference, don't create
    FColsAvail: TBaseColProps; //use assign()
    FColsActive: TBaseColProps; //reference, don't create
    //
    FOnGetBaseNode: TGetBaseNodeFunction;
    FOnTrace: TTraceProcedure;
    FOnEdit: TNotifyEvent;
    FOnFinishEditCR: TFinishEditCREvent;
    FOnBaseClearContent: TNotifyEvent;
    FOnMarkRow: TNotifyEvent;
    //
    FAlwaysShowCurrent: Boolean;
    FOnKeyDown: TKeyEvent;
    //
    FColorSchema: TColGridColorScheme;
    //
    FAutoMark: Boolean;
    FAutoInsert: Boolean;
    FAutoDelete: Boolean;
    FHeatSize: Integer;
    FOnCellSelect: TCellSelectEvent;
    FOnCellClick: TCellClickEvent;
    FCellProps: TCellProps;
    FUseHTML: Boolean;
    FMenuMode: Boolean;
    FExcelStyle: Boolean;

    FDefaultColor: TColor;
    FAlternatingColor: TColor;
    FEditableColor: TColor;
    FAlternatingEditableColor: TColor;
    FCurrentColor: TColor;
    FTransColor: TColor;
    FFocusColor: TColor;

    FHeaderRowIndex: Integer;

    procedure SetColsAvail(const Value: TBaseColProps);
    procedure SetBaseColPropClass(const Value: TBaseColPropClass);

    function GetColorPaint: Boolean;
    procedure SetColorPaint(const Value: Boolean);
    procedure SetColorSchema(const Value: TColGridColorScheme);
    function InitCellProp2(rd: TBaseNode; cp: TBaseColProp;
      IsSorted: Boolean; ACol, ARow: Integer): TCellProp;
    procedure SetMenuMode(const Value: Boolean);
    function GetActiveColor: TColGridColorRec;
    procedure SetActiveColors(const Value: TColGridColorRec);
  protected
    procedure Trace(s: string);
    property ColBO: TBaseColBO read FColBO;
  public
    Name: string;
    GridModel: IColGrid;
    DisplayOrder: TDisplayOrderList;

    class function LeadingZeros(LengthRequired: Integer; const sIn: string): string;

    constructor Create;
    destructor Destroy; override;
    function InitCellProp(ACol, ARow: Integer): TCellProp;
    procedure InitCellProps;
    function GetBaseRowCollection: TBaseRowCollection;
    function GetBaseNode: TBaseNode;
    function AddColumn(aNameIndex: string): TBaseColProp;
    procedure SetupGrid;
    procedure ShowData;
    procedure UpdateAll;
    procedure InitDisplayOrder(col: Integer);
    procedure AddRowCollectionItem;
    procedure InsertRowCollectionItem;
    procedure DeleteRowCollectionItem;
    procedure MarkRowCollectionItem;
    procedure ToggleColorPaint;
    function ToString: string; override;
    procedure Content(SL: TStrings; aCaption: string);
    function GetRowCollectionItem(row: Integer): TBaseRowCollectionItem;
    procedure SetColsActiveReference(const Value: TBaseColProps);
    procedure SetColBOReference(const Value: TBaseColBO);
    procedure SetBaseColPropClass2(const Value: TBaseColPropClass);
    property ColPropClass: TBaseColPropClass read FColPropClass write SetBaseColPropClass;
    property ColsActive: TBaseColProps read FColsActive;
    property ColorPaint: Boolean read GetColorPaint write SetColorPaint;
    property CellProps: TCellProps read FCellProps;
  //published
    property ColsAvail: TBaseColProps read FColsAvail write SetColsAvail;
    property OnGetBaseNode: TGetBaseNodeFunction read FOnGetBaseNode write FOnGetBaseNode;
    property OnTrace: TTraceProcedure read FOnTrace write FOnTrace;
    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
    property OnFinishEditCR: TFinishEditCREvent read FOnFinishEditCR write FOnFinishEditCR;
    property OnClearContent: TNotifyEvent read FOnBaseClearContent write FOnBaseClearContent;
    property OnMarkRow: TNotifyEvent read FOnMarkRow write FOnMarkRow;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnCellSelect: TCellSelectEvent read FOnCellSelect write FOnCellSelect;
    property OnCellClick: TCellClickEvent read FOnCellClick write FOnCellClick;
    property AlwaysShowCurrent: Boolean read FAlwaysShowCurrent write FAlwaysShowCurrent;
    property ColorSchema: TColGridColorScheme read FColorSchema write SetColorSchema;
    property ActiveColors: TColGridColorRec read GetActiveColor write SetActiveColors;
    property AutoMark: Boolean read FAutoMark write FAutoMark;
    property AutoDelete: Boolean read FAutoDelete write FAutoDelete;
    property AutoInsert: Boolean read FAutoInsert write FAutoInsert;
    property HeatSize: Integer read FHeatSize write FHeatSize;
    property UseHTML: Boolean read FUseHTML write FUseHTML;
    property MenuMode: Boolean read FMenuMode write SetMenuMode;
    property ExcelStyle: Boolean read FExcelStyle write FExcelStyle;
    property FocusColor: TColor read FFocusColor;
  end;

var
  GetCellPropCounter: Integer;
  ColCaptionBag: TColCaptionBag;
  UseDoubleBufferForGrid: Boolean;

implementation

function MyStringListCompareText(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List[Index1], List[Index2]);
end;

{ TDisplayOrderList }

function TDisplayOrderList.GetDisplayIndex(j: Integer): Integer;
begin
  if (j >= 0) and (j < Count) then
  begin
    if Descending then
      result := Integer(Objects[Count-1-j])
    else
      result := Integer(Objects[j]);
  end
  else
    result := j;
end;

procedure TDisplayOrderList.SetDescending(const Value: Boolean);
begin
  FDescending := Value;
end;

procedure TDisplayOrderList.Sort;
begin
  CustomSort(MyStringListCompareText);
end;

{ TBaseStringGrid }

constructor TColGrid.Create;
begin
  inherited Create;

  FCellProps := TCellProps.Create;
  FHeatSize := 1;
  FAutoMark := False;
  FExcelStyle := False;
  ColorSchema := TColorService.GetColorScheme;
  FColPropClass := TBaseColProp;
  DisplayOrder := TDisplayOrderList.Create;
  FColsAvail := TBaseColProps.Create(Self, FColPropClass);
  ColsAvail.Init;

  FColsActiveDefault := TBaseColProps.Create(nil, FColPropClass);
  FColsActive := FColsActiveDefault;

  FColBODefault := TBaseColBO.Create;
  FColBO := FColBODefault;

  AddColumn('col_BaseID');
end;

destructor TColGrid.Destroy;
begin
  Trace('TBaseStringGrid.Destroy - ' + self.Name);
  DisplayOrder.Free;
  FColsAvail.Free;
  FColsActiveDefault.Free;
  FColBODefault.Free;
  FCellProps.Free;
  GridModel := nil;
  inherited;
end;

procedure TColGrid.InitCellProps;
var
  c, r: Integer;
  rd: TBaseNode;
  cp: TBaseColProp;
  IsSorted: Boolean;
begin
  CellProps.ColCount := GridModel.ColCount;
  for c := 0 to GridModel.ColCount-1 do
  begin
    rd := GetBaseNode;
    cp := ColsActive[c];
    IsSorted := ColsActive.SortColIndex <> -1;
    for r := 0 to GridModel.RowCount-1 do
    begin
      InitCellProp2(rd, cp, IsSorted, c, r);
    end;
  end;
end;

function TColGrid.InitCellProp(ACol, ARow: Integer): TCellProp;
var
  rd: TBaseNode;
  cp: TBaseColProp;
  IsSorted: Boolean;
begin
  result := nil;
  rd := GetBaseNode;
  if not Assigned(rd) then exit;
  try
    cp := ColsActive[ACol];
    if not Assigned(cp) then exit;
    IsSorted := ColsActive.SortColIndex <> -1;
    result := InitCellProp2(rd, cp, IsSorted, ACol, ARow);
  except
  end;
end;

function TColGrid.InitCellProp2(
  rd: TBaseNode;
  cp: TBaseColProp;
  IsSorted: Boolean;
  ACol, ARow: Integer): TCellProp;
var
  cpBaseID: TBaseColProp;
  BaseID: Integer;
  cr: TBaseRowCollectionItem;
  bc: TColor;
  TempColor: TColor;
  CellProp: TCellProp;
  IsNormalRow: Boolean;
  cc: TCCC;
begin
  result := nil;
  if not Assigned(rd) then exit;
  if not Assigned(cp) then exit;

  try
    CellProp := FCellProps.CellProp[ACol, ARow];
    CellProp.ShowGroup := false;
    CellProp.GroupColor := clFleetNone;

    bc := clBlank;
    cc := cccBlank;

    if (ARow > FHeaderRowIndex) and (AlwaysShowCurrent or (IsSorted = False) ) then
    begin
      IsNormalRow := Odd((ARow-1) div HeatSize);
      { alternating row color }
      if IsNormalRow then
      begin
        bc := FDefaultColor;
        cc := cccDefaultColor;
      end
      else
      begin
        bc := FAlternatingColor;
        cc := cccAlternatingColor;
      end;

      { editable columns color }
      if cp.ReadOnly = False then
      begin
        if IsNormalRow then
        begin
          bc := FEditableColor;
          cc := cccEditableColor;
        end
        else
        begin
          bc := FAlternatingEditableColor;
          cc := cccAlternatingEditableColor;
        end;
      end;

      { 'current row' color(s) }
      begin
        cpBaseID := ColsActive.ByName['col_BaseID'];
        if Assigned(cpBaseID) then
        begin
          BaseID := StrToIntDef(GridModel.Cells[cpBaseID.Index, aRow], -1);
          if BaseID >= 0 then
          begin
            if Assigned(rd.BaseRowCollection ) then
            begin
              cr := rd.BaseRowCollection.FindBase(BaseID);

              if (cr <> nil) and (ColBO.CurrentRow = cr) then
              begin
                if cp.ReadOnly then
                begin
                  bc := FCurrentColor;
                  cc := cccEditableColor;
                end
                else
                begin
                  bc := FTransColor;
                  cc := cccAlternatingEditableColor;
                end;
              end;
              TempColor := bc;
              if cr <> nil then
              begin
                CellProp.Color := bc;
                cr.UpdateCellProp(cp, CellProp);
                if CellProp.HasGroup then
                  CellProp.ShowGroup := true
                else
                  CellProp.GroupColor := CellProp.Color;
                bc := CellProp.Color;
              end;
  						if (bc <> TempColor) then
  							cc := cccCustomColor;
            end;
          end;
        end;
      end;

    end;

    if (ARow > FHeaderRowIndex) and ((AlwaysShowCurrent = False) and IsSorted) then
    begin
      IsNormalRow := Odd((ARow + 1 + FHeaderRowIndex) div HeatSize);
      { alternating row color }
      if IsNormalRow then
      begin
        bc := FDefaultColor;
        cc := cccDefaultColor;
      end
      else
      begin
        bc := FAlternatingColor;
        cc := cccAlternatingColor;
      end;

      { editable column color }
      if cp.ReadOnly = False then
      begin
        if IsNormalRow then
        begin
          bc := FEditableColor;
          cc := cccEditableColor;
        end
        else
        begin
          bc := FAlternatingEditableColor;
          cc := cccAlternatingEditableColor;
        end;
      end;
    end;

    if ARow = FHeaderRowIndex then
    begin
      bc := clHeader;
      cc := cccHeaderColor;
    end;

    CellProp.HTMLColor := TColorService.HTMLColor(bc);
    CellProp.Color := bc;
    CellProp.Alignment := cp.Alignment;
    CellProp.ColorClass := cc;
    result := CellProp;
  except
  end;
end;

procedure TColGrid.InitDisplayOrder(col: Integer);
var
  cl: TBaseRowCollection;
  cr: TBaseRowCollectionItem;
  i: integer;
  sortkey: string;
  cp: TBaseColProp;
begin
  sortkey := '';
  { only sortable columns,
    of cause nur bei den sortierbaren Spalten }
  cp := ColsActive[col];
  if not Assigned(cp) or not cp.Sortable then Exit;

  { remember the index of the column object in ColsAvail }
  ColsActive.SortColIndex := cp.Index;

  //----------------------------------------------------------------------------
  { get to the data }
  cl := GetBaseRowCollection;
  if not Assigned(cl) then Exit;

  { check RowCount and draw header
    020804
    InitDisplayOrder only called from UpdateAll
    SetupGrid will be called from UpdateAll prior to InitDisplayOrder
  }
  //SetupGrid; //called already in methode UpdateAll

  //----------------------------------------------------------------------------

  { update DisplayOrder }
  DisplayOrder.Clear;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl[i];
    if cr.IsInFilter then
    begin
      cp.GetSortKey(cr, sortkey);
      { no leading zero's for string columns,
        bei Strings zum Beispiel keine Leadingzeros }
      if (cp.ColType = colTypeString) then
        DisplayOrder.addobject(sortkey, TObject(i))
      else
        DisplayOrder.addobject(LeadingZeros(20, sortkey), TObject(i));
    end;
  end;
  DisplayOrder.Sort;
  DisplayOrder.Descending := cp.Descending;
end;

function TColGrid.AddColumn(aNameIndex: string): TBaseColProp;
var
  cp: TBaseColProp;
begin
  { find column in ColsAvail and add to ColsActive }
  cp := ColsAvail.ByName[aNameIndex];
  if Assigned(cp) then
  begin
    result := ColsActive.Add;
    result.Assign(cp);
  end
  else
    result := nil;
end;

procedure TColGrid.SetupGrid;
begin
  GridModel.SetupGrid;
end;

procedure TColGrid.ShowData;
begin
  GridModel.ShowData;
end;

function TColGrid.GetBaseNode: TBaseNode;
begin
  result := nil;
  if Assigned(OnGetBaseNode) then
    result := OnGetBaseNode;
end;

function TColGrid.GetBaseRowCollection: TBaseRowCollection;
var
  rd: TBaseNode;
begin
  result := nil;
  rd := GetBaseNode;
  if Assigned(rd) then
    result := rd.BaseRowCollection;
end;

function TColGrid.GetRowCollectionItem(row: Integer): TBaseRowCollectionItem;
var
  cl: TBaseRowCollection;
  cp: TBaseColProp;
begin
  result := nil;
  if ColsActive.Count = 0 then Exit;
  cl := GetBaseRowCollection;
  if not Assigned(cl) then Exit;
  if (row > FHeaderRowIndex) and (row <= cl.Count) then
  begin
    cp := ColsActive.ByName['col_BaseID'];
    if Assigned(cp) then
      result := cl.FindBase(StrToIntDef(GridModel.Cells[cp.Index, row], -1));
  end;
end;

procedure TColGrid.Trace(s: string);
begin
  if Assigned(OnTrace) then
    OnTrace(s);
end;

class function TColGrid.LeadingZeros(LengthRequired: Integer; const sIn: string): string;
var
  s: string;
  i: Integer;
begin
  s := '';
  { get LengthRequired zeros / erstmal die required Anzahl Nullen holen }
  for i := 1 to LengthRequired do
    s := s + '0';
  { place zeros in front / davorsetzen }
  s := s + sIn;
  { and select LengthRequired characters from the right / rightaligned auslesen }
  Result := Copy(s, Length(s) - LengthRequired + 1, LengthRequired);
end;

procedure TColGrid.SetColsAvail(const Value: TBaseColProps);
begin
  if (FColsAvail <> Value) then
    if Assigned(FColsAvail) then
      FColsAvail.Assign(Value);
end;

procedure TColGrid.SetColsActiveReference(const Value: TBaseColProps);
begin
  GridModel.CancelEdit;
  if Assigned(Value) then
    FColsActive := Value
  else
    FColsActive := FColsActiveDefault;

  UpdateAll;
end;

procedure TColGrid.UpdateAll;
begin
  GridModel.CancelEdit;
  DisplayOrder.Clear;
  SetupGrid;
  InitDisplayOrder(ColsActive.SortColIndex);
  ShowData;
  if UseHTML then
  begin
    InitCellProps;
  end;
end;

procedure TColGrid.SetColBOReference(
  const Value: TBaseColBO);
begin
  if Assigned(Value) then
    FColBO := Value
  else
    FColBO := FColBODefault;
end;

procedure TColGrid.SetBaseColPropClass(const Value: TBaseColPropClass);
begin
  if Assigned(Self.ColBO) then
    SetBaseColPropClass2(Value)
  else
    SetBaseColPropClass2(Value);
end;

function TColGrid.GetColorPaint: Boolean;
begin
  result := ColsActive.SortColIndex = -1;
end;

{ This method toggles between sorting of first column and no sorting.

  This will effect the visibility of the row highlight.
  (ColorPaint = row highlight)

  Highlighting the current row only takes place if not sorted.

  The sortorder of the first column is the default sort order.
  The values in this column are usually sorted by default (autoincremented numbers),
  so that the Grid looks the same if sorted by first column or not sorted.

  Setting SortColIndex to 0 turns Sorting on and ColorPaint off.
  Setting SortColIndex to -1 turns ColorPaint on and sorting off.

  Sorting and ColorPainting are dependent on each other.
  There is one application where the highligting is done over a range of related rows
  which only makes sense if the rows are drawn next to each other
  as when sorted in default order, which is the same as no sorting.

  For some applications with only one current row it still makes sense
  to highlight the current row only when not sorted, otherwise the highlight
  will jump around if the 'current position' iterates over the rows.
}
procedure TColGrid.ToggleColorPaint;
begin
  if ColorPaint { ColsActive.SortColIndex = -1 } then
    ColsActive.SortColIndex := 0 //turn ColorPaint off
  else
    ColsActive.SortColIndex := -1; //turn ColorPaint on

  UpdateAll;
end;

procedure TColGrid.SetColorPaint(const Value: Boolean);
begin
  if Value then
    ColsActive.SortColIndex := -1
  else
    ColsActive.SortColIndex := 0;

  UpdateAll;
end;

procedure TColGrid.AddRowCollectionItem;
var
  cl: TBaseRowCollection;
  cr: TBaseRowCollectionItem;
begin
  cl := nil;
  if Assigned(ColBO) and Assigned(ColBO.CurrentNode) then
    cl := ColBO.CurrentNode.BaseRowCollection;
  cr := cl.Add as TBaseRowCollectionItem;
  cr.BaseID := cl.Count;
  UpdateAll;
  GridModel.Enabled := cl.FilteredCount > 0;
end;

procedure TColGrid.InsertRowCollectionItem;
var
  cl: TBaseRowCollection;
  cr: TBaseRowCollectionItem;
  cr1: TBaseRowCollectionItem;
  i: Integer;
begin
  cl := nil;
  if Assigned(ColBO) and Assigned(ColBO.CurrentNode) then
    cl := ColBO.CurrentNode.BaseRowCollection;
  cr1 := GetRowCollectionItem(GridModel.Row);
  if Assigned(cl) and Assigned(cr1) then
  begin
    cl.Insert(cr1.Index);
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i] as TBaseRowCollectionItem;
      cr.BaseID := i + 1;
    end;
    UpdateAll;
    GridModel.Enabled := True;
  end;
end;

{ Delete a single row }
procedure TColGrid.DeleteRowCollectionItem;
var
  cl: TBaseRowCollection;
  cr: TBaseRowCollectionItem;
  cr1: TBaseRowCollectionItem;
  i: Integer;
begin
  cl := nil;
  if Assigned(ColBO) and Assigned(ColBO.CurrentNode) then
    cl := ColBO.CurrentNode.BaseRowCollection;
  cr1 := GetRowCollectionItem(GridModel.Row);
  if Assigned(cr1) then
  begin
    if ColBO.CurrentRow = cr1 then
      ColBO.CurrentRow := nil;
    cl.Delete(cr1.Index);
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i] as TBaseRowCollectionItem;
      cr.BaseID := i + 1;
    end;
    UpdateAll;
    if cl.FilteredCount = 0 then
      GridModel.Enabled := False;
  end;
end;

procedure TColGrid.MarkRowCollectionItem;
var
  cr: TBaseRowCollectionItem;
begin
  { VK_F4 }
  if Assigned(ColBO) then
  begin
    cr := GetRowCollectionItem(GridModel.Row);
    if (cr = ColBO.CurrentRow) then
      ColBO.CurrentRow := nil
    else
      ColBO.CurrentRow := cr;

    ShowData;
    if Assigned(OnMarkRow) then
      OnMarkRow(Self);
  end;
end;

procedure TColGrid.SetColorSchema(const Value: TColGridColorScheme);
var
  t: TColGridColorRec;
begin
  t := TColorService.GetGridColors(Value);
  FColorSchema := Value;
  FDefaultColor := t.DefaultColor;
  FAlternatingColor := t.AlternatingColor;
  FFocusColor := t.FocusColor;
  FEditableColor := t.EditableColor;
  FAlternatingEditableColor := t.AlternatingEditableColor;
  FCurrentColor := t.CurrentColor;
  FTransColor := t.TransColor;
end;

function TColGrid.ToString: string;
var
  SL: TStrings;
begin
  try
    SL := TStringList.Create;
    try
      Content(SL, '');
      result := SL.Text;
    finally
      SL.Free;
    end;
  except
    result := '';
  end;
end;

procedure TColGrid.Content(SL: TStrings; aCaption: string);
var
  c, r: Integer;
  s: string;
  cp: TBaseColProp;
  sColor: string;
begin
  //SL.Add('<html><head><tiltle>StringGrid</title></head><body>');
  SL.Add('<table border="1" width="100%" cellspacing="0" cellpadding="1">');
  if aCaption <> '' then
    SL.Add('<caption>' + aCaption + '</caption>');
  for r := 0 to GridModel.RowCount-1 do
  begin
    SL.Add('<tr align="left">');
    for c := 0 to GridModel.ColCount-1 do
    begin
      cp := Self.FColsActive.Items[c];
      if not Assigned(cp) then continue;

      s := GridModel.Cells[c, r];
      sColor := FCellProps.CellProp[c, r].HTMLColor;
      if s = '' then
        s := '&nbsp;';
      if r = 0 then
        if cp.FAlignment = taRightJustify then
          SL.Add('<th align="right">' + s + '</th>')
        else
          SL.Add('<th>' + s + '</th>')
      else
      begin
        if cp.FAlignment = taRightJustify then
          SL.Add('<td bgcolor="' + sColor + '" align="right">' + s + '</td>')
        else
          SL.Add('<td bgcolor="' + sColor + '">' + s + '</td>')
      end;
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');
  //SL.Add('</body></html>');
end;

procedure TColGrid.SetMenuMode(const Value: Boolean);
begin
  FMenuMode := Value;
  if FMenuMode then
  begin
    GridModel.FixedRows := 0;
    FHeaderRowIndex := -1;
  end
  else
  begin
    GridModel.FixedRows := 1;
    FHeaderRowIndex := 0;
  end;
  GridModel.SetHeaderRowIndex(FHeaderRowIndex);
end;

function TColGrid.GetActiveColor: TColGridColorRec;
begin
  result.DefaultColor := FDefaultColor;
  result.AlternatingColor := FDefaultColor;
  result.FocusColor := FFocusColor;
  result.EditableColor := FEditableColor;
  result.AlternatingEditableColor := FAlternatingEditableColor;
  result.CurrentColor := FCurrentColor;
  result.TransColor := FTransColor;
end;

procedure TColGrid.SetActiveColors(const Value: TColGridColorRec);
begin
  FDefaultColor := Value.DefaultColor;
  FAlternatingColor := Value.DefaultColor;
  FFocusColor := Value.FocusColor;
  FEditableColor := Value.EditableColor;
  FAlternatingEditableColor := Value.AlternatingEditableColor;
  FCurrentColor := Value.CurrentColor;
  FTransColor := Value.TransColor;
  GridModel.InvalidateGrid;
end;

procedure TColGrid.SetBaseColPropClass2(const Value: TBaseColPropClass);
begin
  if (Value <> nil) and (FColPropClass <> Value) then
  begin
    FColPropClass := Value;
    { ColsActive }
    FColsActiveDefault.Free;
    FColsActiveDefault := TBaseColProps.Create(nil, FColPropClass);
    FColsActive := FColsActiveDefault;
    { ColsAvail }
    FColsAvail.Free;
    FColsAvail := TBaseColProps.Create(Self, FColPropClass);
    FColsAvail.Init;
    AddColumn('col_BaseID');
  end;
end;

{ TBaseColProp }

procedure TBaseColProp.Assign(Source: TPersistent);
var
  cp: TBaseColProp;
begin
  if Source is TBaseColProp then
  begin
    cp := Source as TBaseColProp;
    NameID := cp.NameID;
    NumID := cp.NumID;
    Caption := cp.Caption;
    Width := cp.Width;
    Alignment := cp.Alignment;
    Visible := cp.Visible;
    Sortable := cp.Sortable;
    ColType := cp.ColType;
    Descending := cp.Descending;
    //
    FOnGetSortKey := cp.FOnGetSortKey;
    FOnGetSortKey2 := cp.FOnGetSortKey2;
    FOnGetText := cp.FOnGetText;
    FOnSetText := cp.FOnSetText;
    FReadOnly := cp.FReadOnly;
    FOnFinishEdit := cp.FOnFinishEdit;
    FOnFinishEdit2 := cp.FOnFinishEdit2;
  end
  else
    inherited Assign(Source);
end;

constructor TBaseColProp.Create(ACollection: TCollection);
begin
  inherited;
  NameID := ''; //--> SetNameID
  FVisible := True;
  FAlignment := taRightJustify;
  FWidth := 35;
  FReadOnly := True;
  if not Assigned(ColCaptionBag) then
    ColCaptionBag := TColCaptionBag.Create;
end;

function TBaseColProp.GetCaption: string;
begin
    result := FCaption;
end;

function TBaseColProp.GetText(cr: TBaseRowCollectionItem): string;
begin
  result := '';
  if Assigned(FOnGetText) then
    FOnGetText(cr, result)
  else
    GetTextDefault(cr, result);
end;

procedure TBaseColProp.GetSortKey(cr: TBaseRowCollectionItem; var SortKey: string);
begin
  SortKey := GetText(cr);

  { move down zero's and blanks,
    0 bzw. Blank in einigen Spalten nach hinten schieben }
  if (ColType = colTypeRank) and ((SortKey = '0') or (SortKey = '')) then
    SortKey := IntToStr(999 + cr.BaseID)
  else if (ColType = ColTypeString) and (SortKey = '') then
    SortKey := 'ZZZ' + TColGrid.LeadingZeros(3, IntToStr(cr.BaseID));

  if Assigned(FOnGetSortKey) then
    FOnGetSortKey(cr, SortKey)
  else if Assigned(FOnGetSortKey2) then
    FOnGetSortKey2(cr, SortKey, NameID);
end;

{ override InitColsAvail like this:

  procedure TMyColProp.InitColsAvail;
  var
    cp: TBaseColProp;
    ColsAvail: TBaseColProps;
  begin
    if Collection is TBaseColProps then
      ColsAvail := TBaseColProps(Collection)
    else
      exit;

    inherited;

    cp := ColsAvail.Add;
    cp.NameID := 'col_Run';
    cp.Caption := 'Run';
    cp.Width := 35;
    cp.Sortable := True;
    cp.Alignment := taLeftJustify;
    ...
  end;
}
procedure TBaseColProp.InitColsAvail;
begin
end;

procedure TBaseColProp.GetTextDefault(cr: TBaseRowCollectionItem; var Value: string);
begin
  Value := '';
  if not Assigned(cr) then exit;

  if NumID = 0 then //if NameID = 'col_BaseID' then
  begin
    Value := IntToStr(cr.BaseID);
  end
end;

procedure TBaseColProp.SetNameID(const Value: string);
var
  o: TBaseColProps;
begin
  o := Collection as TBaseColProps;
  if (Value = '') or ((Value <> FNameID) and o.IsDuplicateNameID(Value)) then
  begin
    FNameID := 'col_'+ IntToStr(ID);
    if (FCaption = '') or (FCaption = FNameID) then
      FCaption := Value;
  end
  else
  begin
    if FCaption = FNameID then
      FCaption := Value;
    FNameID := Value;
  end;
end;

{ TBaseColProps }

function TBaseColProps.IsDuplicateNameID(s: string): Boolean;
var
  i: Integer;
  o: TBaseColProp;
begin
  result := False;
  for i := 0 to Count-1 do
  begin
    o := Items[i];
    if o.NameID = s then
    begin
      result := True;
      break;
    end;
  end;
end;

function TBaseColProps.Add: TBaseColProp;
begin
  Result := TBaseColProp(inherited Add);
end;

function TBaseColProps.GetByName(NameIndex: string): TBaseColProp;
var
  i: Integer;
  cp: TBaseColProp;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    cp := Items[i];
    if Assigned(cp) and (cp.NameID = NameIndex) then
    begin
      result := cp;
      break;
    end;
  end;
end;

function TBaseColProps.Grid: TColGrid;
begin
  result := nil;
  if (Owner <> nil) and (Owner is TColGrid) then
    result  := Owner as TColGrid;
end;

function TBaseColProps.GetGridName: string;
begin
  if Grid <> nil then
  begin
    result := Grid.Name
  end
  else
    result := '';
end;

function TBaseColProps.GetItem(Index: Integer): TBaseColProp;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TBaseColProp(inherited GetItem(Index))
  else
    result := nil;
end;

function TBaseColProps.GetVisibleCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if Items[i].Visible then
      Inc(result);
end;

procedure TBaseColProps.SetItem(Index: Integer; const Value: TBaseColProp);
begin
  inherited SetItem(Index, Value);
end;

procedure TBaseColProps.UpdateRow(AGrid: IColGrid; ARow: Integer;
  cr: TBaseRowCollectionItem);
var
  i: Integer;
  s: string;
begin
  for i := 0 to Count - 1 do
  begin
    s := Items[i].GetText(cr);
    AGrid.Cells[i, ARow] := s;
  end;
end;

procedure TBaseColProps.SetSortColIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < Count) and Items[Value].Sortable then
    FSortColIndex := Value
  else
    FSortColIndex := -1;
end;

function TBaseColProps.GetSortColIndex: Integer;
begin
  result := -1;
  if (FSortColIndex >= 0)
    and (FSortColIndex < Count)
    and Assigned(Items[FSortColIndex])
    and Items[FSortColIndex].Sortable then
      result := FSortColIndex
    else
      FSortColIndex := -1;
end;

procedure TBaseColProps.Init;
var
  cp: TBaseColProp;
begin
  Clear;

  { BaseID }
  cp := Add;
  cp.NameID := 'col_BaseID';
  cp.Caption := 'ID';
  cp.Width := 25;
  cp.Sortable := True;
  cp.NumID := 0; //default

  cp.InitColsAvail; //virtual

  //if Owner is not nil, memory for objects is managed
  //Owner is nil for ColsActive, not nil for ColsAvail
  if (Owner <> nil) and self.UseCustomColCaptions then
    InitCustomCaptions;
end;

procedure TBaseColProps.InitCustomCaptions;
var
  i: Integer;
  cp: TBaseColProp;
begin
  for i := 0 to Count - 1 do
  begin
    cp := self.Items[i];
    cp.Caption := self.GetCaptionOverride(cp);
  end;
end;

function TBaseColProps.GetCaptionOverride(cp: TBaseColProp): string;
var
  Node: TBaseNode;
  key: string;
begin
  result := '';

  //first try, Grid specific search
  if (GridName <> '') then
  begin
    key := GridName + '_' + cp.NameID;
    result := ColCaptionBag.GetCaption(key);
  end;

  //second try, Table specific search
  if result = '' then
  begin
    if Grid <> nil then
    begin
      Node := Grid.GetBaseNode;
      if Node <> nil then
      begin
        if Node.NameID <> '' then
        begin
          key := Node.NameID + '_' + cp.NameID;
          result := ColCaptionBag.GetCaption(key);
        end;
      end;
    end;
  end;

  //third try, cross table, column name based
  if result = '' then
  begin
    result := ColCaptionBag.GetCaption(cp.NameID);
  end;

  //else use default
  if result = '' then
    result := cp.Caption;
end;

{ TBaseNode }

function TBaseNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TBaseColProp;
end;

function TBaseNode.GetBaseColBO: TBaseColBO;
begin
  result := nil;
  if Owner is TBaseColBO then
    result := TBaseColBO(Owner);
end;

function TBaseNode.GetBaseRowCollection: TBaseRowCollection;
begin
  result := FCollection;
end;

function TBaseNode.Owner: TPersistent;
begin
  result := FBaseColBO;
end;

{ When there is a hierarchy of nodes, with each node having a RowCollection,
  and one StringGrid sitting on a TabControl (not PageControl),
  being able to display a collection at a time,
  call this function on the root node to initialize the TabControl's Tabs.
  This method is virtual, a concrete node will overide this method
  and add Tabs for its children...
}
procedure TBaseNode.ShowTabs(Tabs: TStrings);
begin
  Tabs.Clear;
  Tabs.AddObject('TBaseNode', Self);
end;

procedure TBaseNode.SetBaseRowCollection(const Value: TBaseRowCollection);
begin
  FCollection := Value;
end;

procedure TBaseNode.Calc;
begin
  //virtual
end;

procedure TBaseNode.SetModified(const Value: Boolean);
begin
  FModified := Value;
  if Value and Assigned(OnModified) then
    OnModified(Self);
end;

procedure TBaseNode.SetOnModified(const Value: TNotifyEvent);
begin
  FOnModified := Value;
end;

constructor TBaseNode.Create(aOwner: TBaseColBO);
begin
  inherited Create;
  FBaseColBO := aOwner;
end;

{ TBaseRowCollection }

constructor TBaseRowCollection.Create(AOwner: TPersistent;
  ItemClass: TBaseRowCollectionItemClass);
begin
  inherited Create;
  FOwner := aOwner;
  FItemClass := ItemClass;
  FItems := TList.Create;
  if AOwner is TBaseNode then
    TBaseNode(AOwner).BaseRowCollection := Self;
end;

destructor TBaseRowCollection.Destroy;
begin
  if FItems <> nil then
    Clear;
  FItems.Free;
  inherited Destroy;
end;

function TBaseRowCollection.Add: TBaseRowCollectionItem;
begin
  Result := FItemClass.Create(Self);
end;

procedure TBaseRowCollection.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TCollection then
  begin
    Clear;
    for i := 0 to TCollection(Source).Count - 1 do
      Add.Assign(TCollection(Source).Items[i]);
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TBaseRowCollection.Clear;
begin
  if FItems.Count > 0 then
  begin
    while FItems.Count > 0 do
      TCollectionItem(FItems.Last).Free;
  end;
end;

procedure TBaseRowCollection.Delete(Index: Integer);
begin
  TCollectionItem(FItems[Index]).Free;
end;

function TBaseRowCollection.GetItem(Index: Integer): TBaseRowCollectionItem;
begin
  Result := FItems[Index];
end;

procedure TBaseRowCollection.SetItem(Index: Integer; Value: TBaseRowCollectionItem);
begin
  TBaseRowCollectionItem(FItems[Index]).Assign(Value);
end;

procedure TBaseRowCollection.ClearList;
var
  i: Integer;
  cr: TBaseRowCollectionItem;
begin
  for i := 0 to Count-1 do
  begin
    cr := Self.Base[i];
    cr.ClearList;
  end;
end;

function TBaseRowCollection.FindItemID(ID: Integer): TCollectionItem;
var
  i: Integer;
begin
  for i := 0 to FItems.Count-1 do
  begin
    Result := TCollectionItem(FItems[i]);
    if Result.ID = ID then Exit;
  end;
  Result := nil;
end;

function TBaseRowCollection.GetEnumerator: TBaseRowCollectionEnumerator;
begin
  Result := TBaseRowCollectionEnumerator.Create(Self);
end;

function TBaseRowCollection.Insert(Index: Integer): TBaseRowCollectionItem;
begin
  Result := Add;
  Result.Index := Index;
end;

procedure TBaseRowCollection.InsertItem(Item: TBaseRowCollectionItem);
begin
  if not (Item is FItemClass) then exit;
    //TList.Error(@SInvalidProperty, 0);
  FItems.Add(Item);
  Item.FCollection := Self;
  Item.FID := FNextID;
  Inc(FNextID);
end;

procedure TBaseRowCollection.RemoveItem(Item: TBaseRowCollectionItem);
begin
  if Item = FItems.Last then
    FItems.Delete(FItems.Count - 1)
  else
    FItems.Remove(Item);
  Item.FCollection := nil;
end;

procedure TBaseRowCollection.ClearResult;
var
  i: Integer;
  cr: TBaseRowCollectionItem;
begin
  for i := 0 to Count-1 do
  begin
    cr := Self.Base[i];
    cr.ClearResult;
  end;
end;

function TBaseRowCollection.FilteredCount: Integer;
begin
  result := Count;
end;

function TBaseRowCollection.FindBase(const BaseID: Integer): TBaseRowCollectionItem;
var
  i: Integer;
begin
  { search for BaseID, return nil if not found }
  Result := nil;
  for i := 0 to Count - 1 do
    if BaseID = Base[i].BaseID then
    begin
      Result := Base[i];
      Break;
    end
end;

function TBaseRowCollection.GetBase(Index: Integer): TBaseRowCollectionItem;
begin
  Assert((Index >= 0) and (Index < Count), 'invalid index in GetBase');
  if (Index < 0) or (Index >= Count) then
    result := nil
  else
    Result := TBaseRowCollectionItem(Items[Index]);
end;

function TBaseRowCollection.GetBaseNode: TBaseNode;
begin
  result := nil;
  if GetOwner is TBaseNode then
    result := TBaseNode(GetOwner);
end;

function TBaseRowCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBaseRowCollection.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TBaseRowCollection.IndexOf(Item: TCollectionItem): Integer;
begin
  result := Item.Index;
end;

procedure TBaseRowCollection.SetBase(Index: Integer;
  const Value: TBaseRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
  begin
    TBaseRowCollectionItem(Items[Index]).Free;
    Items[Index] := Value;
  end;
end;

{ TBaseColBO }

function TBaseColBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := nil;
end;

function TBaseColBO.GetCurrentNode: TBaseNode;
begin
  result := nil;
end;

procedure TBaseColBO.SetCurrentNode(const Value: TBaseNode);
begin
  //virtual
end;

procedure TBaseColBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  //virtual
  { virtual method allows the StringGrid UI to reset the Current }
end;

procedure TBaseColBO.InitColsActive(StringGrid: TColGrid);
begin
  //virtual
  InitColsActiveLayout(StringGrid, 0);
end;

procedure TBaseColBO.InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer);
begin
  //virtual;
end;

{ TBaseRowCollectionItem }

procedure TBaseRowCollectionItem.ClearList;
begin
  //virtual
end;

procedure TBaseRowCollectionItem.ClearResult;
begin
  //virtual
end;

function TBaseRowCollectionItem.ColumnToColorDef(cp: TBaseColProp;
  aColor: TColor): TColor;
begin
  result := aColor;
end;

constructor TBaseRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited Create;
  SetCollection(Collection);
end;

destructor TBaseRowCollectionItem.Destroy;
begin
  SetCollection(nil);
  inherited Destroy;
end;

function TBaseRowCollectionItem.GetIndex: Integer;
begin
  if FCollection <> nil then
    Result := FCollection.FItems.IndexOf(Self)
  else
    Result := -1;
end;

function TBaseRowCollectionItem.GetOwner: TPersistent;
begin
  Result := FCollection;
end;

function TBaseRowCollectionItem.IsInFilter: Boolean;
begin
  result := True;
end;

procedure TBaseRowCollectionItem.SetCollection(Value: TBaseRowCollection);
begin
  if FCollection <> Value then
  begin
    if FCollection <> nil then FCollection.RemoveItem(Self);
    if Value <> nil then Value.InsertItem(Self);
  end;
end;

procedure TBaseRowCollectionItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
  begin
    FCollection.FItems.Move(CurIndex, Value);
  end;
end;

procedure TBaseRowCollectionItem.UpdateCellProp(cp: TBaseColProp;
  cellProp: TCellProp);
begin
  cellProp.Color := ColumnToColorDef(cp, cellProp.Color);
end;

{ TCellProps }

constructor TCellProps.Create;
begin
  BlockSize := 16;
end;

destructor TCellProps.Destroy;
begin
  Clear;
  T := nil;
  inherited;
end;

procedure TCellProps.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount-1 do
  begin
    T[i].Free;
    T[i] := nil;;
    FCount := 0;
    FColCount := 0;
  end;
  inherited;
end;

function TCellProps.GetCellProp(ACol, ARow: Integer): TCellProp;
var
  i: Integer;
begin
  //FColCount := Max(FColCount, ACol);
  i := FColCount * ARow + ACol;
  if i > FCount-1 then
    Grow(i);

  result := T[i];
  if result = nil then
  begin
    result := TCellProp.Create;
    T[i] := result;
  end;
end;

procedure TCellProps.Grow(i: Integer);
var
  m: Integer;
//  j: integer;
begin
  if i > Length(T)-1  then
  begin
    m := ((i div BlockSize) + 1) * BlockSize;
    SetLength(T, m);
//    for j := FCount to m - 1 do
//      T[j] := TCellProp.Create;
    FCount := m;
  end;
end;

procedure TCellProps.SetColCount(const Value: Integer);
begin
  FColCount := Value;
end;

{ TBaseRowCollectionEnumerator }

constructor TBaseRowCollectionEnumerator.Create(
  ACollection: TBaseRowCollection);
begin
  inherited Create;
  FIndex := -1;
  FCollection := ACollection;
end;

function TBaseRowCollectionEnumerator.GetCurrent: TBaseRowCollectionItem;
begin
  Result := FCollection.Items[FIndex];
end;

function TBaseRowCollectionEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TColCaptionBag }

procedure TColCaptionBag.Clear;
begin
  FSL.Clear;
end;

constructor TColCaptionBag.Create;
begin
  inherited Create;
  FSL := TStringList.Create;
  FSL.Duplicates := dupError;
  IsPersistent := False;
end;

destructor TColCaptionBag.Destroy;
begin
  FSL.Free;
  inherited Destroy;
end;

function TColCaptionBag.GetCaption(const key: string): string;
begin
  result := FSL.Values[key];
end;

procedure TColCaptionBag.SetCaption(const key: string; value: string);
begin
  FSL.Values[key] := Value;
  IsPersistent := True;
end;

function TColCaptionBag.GetCount: Integer;
begin
  result := FSL.Count;
end;

function TColCaptionBag.GetText: string;
begin
  result := FSL.Text;
end;

procedure TColCaptionBag.SetText(const Value: string);
begin
  FSL.Text := Value;
end;

{ TCellProp }

function TCellProp.GetHasGroup: Boolean;
begin
  result := GroupColor <> clFleetNone;
end;

initialization
  ColCaptionBag := TColCaptionBag.Create;

finalization
  ColCaptionBag.Free;

end.
