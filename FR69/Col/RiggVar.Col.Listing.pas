unit RiggVar.Col.Listing;

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
  TListingNode = class;

  TListingCollectionItem = class(TBaseRowCollectionItem)
  private
    function GetListingNode: TListingNode;
    procedure SetModified(const Value: Boolean);
  public
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TListingNode read GetListingNode;
  end;

  TListingCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TListingCollectionItem;
    procedure SetItem(Index: Integer; const Value: TListingCollectionItem);
  public
    function Add: TListingCollectionItem;
    function FindKey(Bib: Integer): TListingCollectionItem;
    property Items[Index: Integer]: TListingCollectionItem read GetItem
      write SetItem;
  end;

  TListingColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TListingBO = class;

  TListingNode = class(TBaseNode)
  private
    function GetListingCollection: TListingCollection;
    function GeListingBO: TListingBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    property ListingCollection: TListingCollection read
      GetListingCollection;
    property ListingBO: TListingBO read GeListingBO;
  end;

  TListingBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure InitColsActiveLayout(StringGrid: TColGrid;
      aLayout: Integer); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Col.Event,
  RiggVar.Col.Race,
  RiggVar.Col.Stammdaten,
  RiggVar.Util.Classes;

{ TListingCollectionItem }

procedure TListingCollectionItem.Assign(Source: TPersistent);
begin
end;

constructor TListingCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;
end;

destructor TListingCollectionItem.Destroy;
begin
  inherited;
end;

function TListingCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TListingCollection;
begin
  cl := Collection as TListingCollection;
  result := cl.BaseNode;
end;

function TListingCollectionItem.GetListingNode: TListingNode;
var
  cl: TListingCollection;
begin
  result := nil;
  cl := Collection as TListingCollection;
  if cl.BaseNode is TListingNode then
    result := TListingNode(cl.BaseNode);
end;

procedure TListingCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

{ TListingCollection }

function TListingCollection.Add: TListingCollectionItem;
begin
  Result := TListingCollectionItem(inherited Add);
  Result.BaseID := Count;
end;

function TListingCollection.FindKey(
  Bib: Integer): TListingCollectionItem;
var
  i: Integer;
  o: TListingCollectionItem;
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

function TListingCollection.GetItem(
  Index: Integer): TListingCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TListingCollectionItem(inherited GetItem(Index));
end;

procedure TListingCollection.SetItem(Index: Integer;
  const Value: TListingCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TListingColProp }

procedure TListingColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TListingCollectionItem;
  cre: TEventRowCollectionItem;
  crs: TStammdatenRowCollectionItem;
  mr: Integer;
  mb: Integer;

  snr: Integer;
  bib: Integer;
  r: Integer;
  ere: TEventRaceEntry;
begin
  Value := '';
  if crgs is TListingCollectionItem then
    cr := TListingCollectionItem(crgs)
  else
    exit;

  inherited;

  mr := cr.Index mod 3;
  mb := cr.Index div 3;

  cre := BO.EventNode.EventRowCollection.Items[mb];

  snr := cre.SNR;
  bib := cre.Bib;

  crs := BO.StammdatenNode.StammdatenRowCollection.FindKey(snr);

  if NameID = 'col_EntryNames' then
  begin
    if crs <> nil then
    begin
      case mr of
         0: Value := crs.FN;
         1: Value := crs.LN;
         2: Value := '';
      end;
    end
    else
    begin
      case mr of
         0: Value := 'EN1';
         1: Value := 'EN2';
         2: Value := '';
      end;
    end;
  end

  else if NameID = 'col_Bib' then
  begin
    case mr of
       0: Value := IntToStr(bib);
       1: Value := '';
       2: Value := '';
    end;
  end

  else if NameID = 'col_Team' then
  begin
    case mr of
       0: Value := cre.SN;
       1: Value := '';
       2: Value := '';
    end;
  end

  else if TUtils.StartsWith(NameID, 'col_W') then
  begin
    r := StrToIntDef(Copy(NameID, 6), -1);
    ere := cre.Race[r];
    case mr of
       0: Value := IntToStr(ere.OTime);
       1: Value := ere.DecimalPoints;
       2: Value := ere.Penalty.ToString;
    end;
  end

  else if NameID = 'col_Result' then
  begin
    case mr of
       0: Value := cre.SN;
       1: Value := cre.GPoints;
       2: Value := IntToStr(cre.GPosR);
    end;
  end

end;

procedure TListingColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
  rc: Integer;
  i: Integer;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  { Bib }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Bib';
  cp.Caption := 'Bib';
  cp.Width := 40;
  cp.Sortable := True;

  { EntryNames }
  cp := ColsAvail.Add;
  cp.NameID := 'col_EntryNames';
  cp.Caption := 'EntryNames';
  cp.Width := 160;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Team }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Team';
  cp.Caption := 'Team';
  cp.Width := 40;
  cp.Sortable := True;

  rc := BO.BOParams.RaceCount;
  for i := 1 to rc do
  begin
  { RX }
  cp := ColsAvail.Add;
  cp.NameID := 'col_W' + IntToStr(i);;
  cp.Caption := 'R' + IntToStr(i);
  cp.Width := 50;
  end;

  { Result }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Result';
  cp.Caption := 'Event';
  cp.Width := 60;
  cp.Sortable := True;
end;

{ TListingNode }

procedure TListingNode.Calc;
begin
  inherited;
end;

constructor TListingNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TListingCollection.Create(Self,
    TListingCollectionItem);
end;

destructor TListingNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TListingNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TListingColProp;
end;

function TListingNode.GeListingBO: TListingBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TListingBO;
end;

function TListingNode.GetListingCollection: TListingCollection;
begin
  result := BaseRowCollection as TListingCollection;
end;

procedure TListingNode.Init(RowCount: Integer);
var
  i: Integer;
begin
  BaseRowCollection.Clear;
  for i := 0 to RowCount - 1 do
    ListingCollection.Add;
end;

procedure TListingNode.Load;
begin
  ListingCollection.Clear;
end;

{ TListingBO }

function TListingBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TListingBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TListingBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
var
  rc: Integer;
  i: Integer;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');
    AddColumn('col_Bib');
    AddColumn('col_EntryNames');
    AddColumn('col_Team');
    rc := BO.BOParams.RaceCount;
    for i := 1 to rc do
      AddColumn('col_W' + IntToStr(i));
    AddColumn('col_Result');
  end;
end;

procedure TListingBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TListingBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

end.
