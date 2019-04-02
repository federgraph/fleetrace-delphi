unit RiggVar.Col.JsonCache;

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
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleBlock;

type
  TCacheNode = class;

  TCacheRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FCaption: string;
    FUrl: string;
    FDataType: Integer;
    FDataFormat: Integer;
    FAge: Integer;
    FUpdates: Integer;
    FHits: Integer;
    FTimeStamp: TDateTime;
    FMillies: Integer;
    //
    FData: string;
    FEventName: string;
    function GetCacheNode: TCacheNode;
    procedure SetModified(const Value: Boolean);
    function GetModus: string;
    procedure SetData(const Value: string);
    function GetAge: Integer;
    function GetData: string;
  public
    Requesting: Boolean;
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure StoreData(aData: string; aName: string; aMillies: Integer);
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TCacheNode read GetCacheNode;
    property Modus: string read GetModus;
    property Data: string read GetData write SetData;
    property EventName: string read FEventName write FEventName;
  published
    property Caption: string read FCaption write FCaption;
    property Url: string read FUrl write FUrl;
    property DataType: Integer read FDataType write FDataType;
    property DataFormat: Integer read FDataFormat write FDataFormat;
    property Age: Integer read GetAge write FAge;
    property Updates: Integer read FUpdates write FUpdates;
    property Hits: Integer read FHits write FHits;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Millies: Integer read FMillies write FMillies;
  end;

  TCacheRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TCacheRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TCacheRowCollectionItem);
  public
    function Add: TCacheRowCollectionItem;
    function FindKey(Bib: Integer): TCacheRowCollectionItem;
    property Items[Index: Integer]: TCacheRowCollectionItem read GetItem write SetItem;
  end;

  TCacheColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string); override;
  end;

  TCacheBO = class;

  TCacheNode = class(TBaseNode)
  private
    function GetCacheRowCollection: TCacheRowCollection;
    function GeCacheBO: TCacheBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    Age: Integer;
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    property CacheRowCollection: TCacheRowCollection read GetCacheRowCollection;
    property CacheBO: TCacheBO read GeCacheBO;
  end;

  TCacheBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

implementation

uses
  RiggVar.App.Main;

{ TCacheRowCollectionItem }

procedure TCacheRowCollectionItem.Assign(Source: TPersistent);
var
  o: TCacheRowCollectionItem;
begin
  if Source is TCacheRowCollectionItem then
  begin
    o := TCacheRowCollectionItem(Source);
    Caption := o.Caption;
    Url := o.Url;
    EventName := o.EventName;
    DataFormat := o.DataFormat;
    Age := o.Age;
    Updates := o.Updates;
    Hits := o.Hits;
    Millies := o.Millies;
    TimeStamp := o.TimeStamp;
  end
end;

constructor TCacheRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;
end;

destructor TCacheRowCollectionItem.Destroy;
begin
  inherited;
end;

function TCacheRowCollectionItem.GetAge: Integer;
begin
  Result := ru.Age - FAge;
end;

function TCacheRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TCacheRowCollection;
begin
  cl := Collection as TCacheRowCollection;
  result := cl.BaseNode;
end;

function TCacheRowCollectionItem.GetData: string;
begin
  result := FData;
end;

function TCacheRowCollectionItem.GetModus: string;
begin
  case DataFormat of
    1: result := 'Txt';
    2: result := 'Xml';
    3: result := 'Html';
    else
      result := '';
  end;
end;

function TCacheRowCollectionItem.GetCacheNode: TCacheNode;
var
  cl: TCacheRowCollection;
begin
  result := nil;
  cl := Collection as TCacheRowCollection;
  if cl.BaseNode is TCacheNode then
    result := TCacheNode(cl.BaseNode);
end;

procedure TCacheRowCollectionItem.SetData(const Value: string);
begin
  FData := Value;
  Updates := FUpdates + 1;
  FAge := ru.Age;
end;

procedure TCacheRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

procedure TCacheRowCollectionItem.StoreData(aData: string; aName: string; aMillies: Integer);
begin
  Data := aData;
  EventName := aName;
  TimeStamp := Now;
  Millies := aMillies;
end;

{ TCacheRowCollection }

function TCacheRowCollection.Add: TCacheRowCollectionItem;
begin
  Result := TCacheRowCollectionItem(inherited Add);
  Result.BaseID := Count;
  Result.TimeStamp := Now;
end;

function TCacheRowCollection.FindKey(
  Bib: Integer): TCacheRowCollectionItem;
var
  i: Integer;
  o: TCacheRowCollectionItem;
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

function TCacheRowCollection.GetItem(
  Index: Integer): TCacheRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TCacheRowCollectionItem(inherited GetItem(Index));
end;

procedure TCacheRowCollection.SetItem(Index: Integer;
  const Value: TCacheRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TCacheColProp }

procedure TCacheColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TCacheRowCollectionItem;
begin
  Value := '';
  if crgs is TCacheRowCollectionItem then
    cr := TCacheRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_Caption' then
    Value := cr.Caption

  else if NameID = 'col_Url' then
    Value := cr.Url

  else if NameID = 'col_EventName' then
    Value := cr.EventName

  else if NameID = 'col_DataType' then
  begin
    case cr.DataType of
      1: Value := 'EventDataText';
      2: Value := 'EventDataJson';
      3: Value := 'RaceDataJson';
      else
        Value := '';
    end;
  end

  else if NameID = 'col_DataFormat' then
  begin
    case cr.DataFormat of
      1: Value := 'TXT';
      2: Value := 'XML';
      3: Value := 'HTM';
      else
        Value := '';
    end;
  end

  else if NameID = 'col_Age' then
    Value := IntToStr(cr.Age)

  else if NameID = 'col_Updates' then
    Value := IntToStr(cr.Updates)

  else if NameID = 'col_Hits' then
    Value := IntToStr(cr.Hits)

  else if NameID = 'col_Millies' then
    Value := IntToStr(cr.Millies)

  else if NameID = 'col_TimeStamp' then
    Value := DateTimeToStr(cr.TimeStamp)
end;

procedure TCacheColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  { Caption }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Caption';
  cp.Caption := 'Caption';
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Url }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Url';
  cp.Caption := 'Url';
  cp.Width := 100;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { EventName }
  cp := ColsAvail.Add;
  cp.NameID := 'col_EventName';
  cp.Caption := 'EventName';
  cp.Width := 120;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { DT }
  cp := ColsAvail.Add;
  cp.NameID := 'col_DataType';
  cp.Caption := 'DT';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { DF }
  cp := ColsAvail.Add;
  cp.NameID := 'col_DataFormat';
  cp.Caption := 'DF';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Age }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Age';
  cp.Caption := 'Age';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Updates }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Updates';
  cp.Caption := 'Updates';
  cp.Width := 70;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Hits }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Hits';
  cp.Caption := 'Hits';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.Descending := True;

  { Millies }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Millies';
  cp.Caption := 'Millies';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.Descending := True;

  { Timestamp }
  cp := ColsAvail.Add;
  cp.NameID := 'col_TimeStamp';
  cp.Caption := 'TimeStamp';
  cp.Width := 120;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
end;

{ TCacheNode }

procedure TCacheNode.Calc;
begin
  inherited;
end;

constructor TCacheNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TCacheRowCollection.Create(Self,
    TCacheRowCollectionItem);
  Age := 1;
end;

destructor TCacheNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TCacheNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TCacheColProp;
end;

function TCacheNode.GeCacheBO: TCacheBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TCacheBO;
end;

function TCacheNode.GetCacheRowCollection: TCacheRowCollection;
begin
  result := BaseRowCollection as TCacheRowCollection;
end;

procedure TCacheNode.Init(RowCount: Integer);
var
  o: TCacheRowCollectionItem;
  i: Integer;
begin
  BaseRowCollection.Clear;
  for i := 0 to RowCount - 1 do
  begin
    o := CacheRowCollection.Add;
    o.TimeStamp := Now;
  end;
end;

procedure TCacheNode.Load;
begin
  CacheRowCollection.Clear;
end;

{ TCacheBO }

function TCacheBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TCacheBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TCacheBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');
    AddColumn('col_Caption');
    AddColumn('col_Url');
    AddColumn('col_EventName');
    AddColumn('col_DataType');
    AddColumn('col_DataFormat');
    AddColumn('col_Sort');
    AddColumn('col_Age');
    AddColumn('col_Updates');
    AddColumn('col_Hits');
    AddColumn('col_Millies');
    AddColumn('col_TimeStamp');
  end;
end;

procedure TCacheBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TCacheBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

end.
