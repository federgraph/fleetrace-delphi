unit RiggVar.Col.Roundings;

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
  Vcl.Graphics,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleBlock;

type
  TRoundingsNode = class;

  TRoundingsCollectionItem = class(TBaseRowCollectionItem)
  private
    function GetRoundingsNode: TRoundingsNode;
    procedure SetModified(const Value: Boolean);
  public
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    procedure UpdateCellProp(cp: TBaseColProp; cellProp: TCellProp); override;
    function ColumnToColorDef(cp: TBaseColProp; aColor: TColor): TColor; override;
    property Modified: Boolean write SetModified;
    property ru: TRoundingsNode read GetRoundingsNode;
  end;

  TRoundingsCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TRoundingsCollectionItem;
    procedure SetItem(Index: Integer; const Value: TRoundingsCollectionItem);
  public
    function Add: TRoundingsCollectionItem;
    function FindKey(Bib: Integer): TRoundingsCollectionItem;
    property Items[Index: Integer]: TRoundingsCollectionItem read GetItem
      write SetItem;
  end;

  TRoundingsColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TRoundingsBO = class;

  TRoundingsNode = class(TBaseNode)
  private
    function GetRoundingsCollection: TRoundingsCollection;
    function GeRoundingsBO: TRoundingsBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    property RoundingsCollection: TRoundingsCollection read
      GetRoundingsCollection;
    property RoundingsBO: TRoundingsBO read GeRoundingsBO;
  end;

  TRoundingsBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    TableLayout: Integer;
    constructor Create;
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

{ TRoundingsCollectionItem }

procedure TRoundingsCollectionItem.Assign(Source: TPersistent);
begin
end;

constructor TRoundingsCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;
end;

destructor TRoundingsCollectionItem.Destroy;
begin
  inherited;
end;

function TRoundingsCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TRoundingsCollection;
begin
  cl := Collection as TRoundingsCollection;
  result := cl.BaseNode;
end;

function TRoundingsCollectionItem.GetRoundingsNode: TRoundingsNode;
var
  cl: TRoundingsCollection;
begin
  result := nil;
  cl := Collection as TRoundingsCollection;
  if cl.BaseNode is TRoundingsNode then
    result := TRoundingsNode(cl.BaseNode);
end;

procedure TRoundingsCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

procedure TRoundingsCollectionItem.UpdateCellProp(cp: TBaseColProp;
  cellProp: TCellProp);
begin
  cellProp.Color := ColumnToColorDef(cp, cellProp.Color);
end;

function TRoundingsCollectionItem.ColumnToColorDef(cp: TBaseColProp;
  aColor: TColor): TColor;
var
  c: Integer;
  offset: Integer;
  it: Integer;
  cit: Integer;
  cft: Integer;
  isOdd: Boolean;
  mb: Integer;
  clIT: TColor;
  clFT: TColor;
begin
  c := cp.Index;
  offset := 1;
  it := Main.GuiManager.IT;
  cit := it + offset;
  cft := Main.GuiManager.MarkCount + offset;

  mb := Index div 3;
  isOdd := Odd(mb);

  if isOdd then
  begin
    clIT := clTransRot;
    clFT := clTransBlau;
  end
  else
  begin
    clIT := clHellRot;
    clFT := clHellBlau;
  end;

  if (it = 0) and (c = cft) then
    result := clFT
  else if (it > 0) and (c = cit) then
    result := clIT
  else
    result := aColor
end;

{ TRoundingsCollection }

function TRoundingsCollection.Add: TRoundingsCollectionItem;
begin
  Result := TRoundingsCollectionItem(inherited Add);
  Result.BaseID := Count;
end;

function TRoundingsCollection.FindKey(
  Bib: Integer): TRoundingsCollectionItem;
var
  i: Integer;
  o: TRoundingsCollectionItem;
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

function TRoundingsCollection.GetItem(
  Index: Integer): TRoundingsCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TRoundingsCollectionItem(inherited GetItem(Index));
end;

procedure TRoundingsCollection.SetItem(Index: Integer;
  const Value: TRoundingsCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TRoundingsColProp }

procedure TRoundingsColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cle: TEventRowCollection;
  cre1: TEventRowCollectionItem;
  cre2: TEventRowCollectionItem;
  plze: Integer;

  cr: TRoundingsCollectionItem;
  mr: Integer;
  mb: Integer;

  mark: Integer;
  idx: Integer;
  plz: Integer;
  cl: TRaceRowCollection;
  cr1: TRaceRowCollectionItem;
  cr2: TRaceRowCollectionItem;
  mc: Integer;
  tpCurrent: TTimePoint;
  tpPrevious: TTimePoint;
  rankdiff: Integer;
begin
  Value := '';
  if crgs is TRoundingsCollectionItem then
    cr := TRoundingsCollectionItem(crgs)
  else
    exit;

  inherited;

  mr := cr.Index mod 3;
  mb := cr.Index div 3;

  idx := mb + 1;
  if NameID = 'col_Idx' then
  begin
    case mr of
       0: Value := IntToStr(idx);
       1: Value := '';
       2: Value := '';
    end;
  end

  else if NameID = 'col_Standing' then
  begin
    Value := '';
    cle := BO.EventNode.EventRowCollection;
    cre1 := cle.Items[mb];
    plze := cre1.PLZ;
    cre2 := cle.Items[plze];
    if cre2 <> nil then
    begin
      case mr of
         0: Value := 'Bib ' + IntToStr(cre2.Bib);
         1: Value := cre2.GPoints;
         2: Value := IntToStr(cre2.GPosR);
      end;
    end;
  end

  else if TUtils.StartsWith(NameID, 'col_M') then
  begin
    Value := '';
    cl := BO.RaceNode.RaceRowCollection;
    mark := StrToIntDef(Copy(NameID, 6), -1);
    if cl.HasTimpointData(mark) then
    begin
      cr1 := cl.Items[mb];
      plz := cr1.IT[mark].PLZ;
      cr2 := cl.Items[plz];
      if cr2 <> nil then
      begin
        tpCurrent := cr2.IT[mark];
        case mr of
           0: Value := 'Bib' + IntToStr(cr2.Bib);
           1: Value := tpCurrent.Behind.ToString;
           2:
           begin
             mc := Main.GuiManager.MarkCount;

             if mark = 1 then
               tpPrevious := nil
             else if mark = 0 then
               tpPrevious := cr2.IT[mc-1]
             else if mark < mc then
               tpPrevious := cr2.IT[mark-1]
             else
               tpPrevious := nil;

             if tpPrevious <> nil then
               rankdiff := tpCurrent.Rank-tpPrevious.Rank
             else
               rankdiff := 0;

             if rankdiff = 0 then
               Value := ''
             else if rankdiff < 0 then
               Value := IntToStr(rankdiff)
             else
               Value := '+' + IntToStr(rankdiff);
           end;
        end;
      end;
    end;
  end;

end;

procedure TRoundingsColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
  mc: Integer;
  i: Integer;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  { Idx }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Idx';
  cp.Caption := 'Idx';
  cp.Width := 40;

  mc := Main.GuiManager.MarkCount-1;
  for i := 1 to mc do
  begin
  { MX }
  cp := ColsAvail.Add;
  cp.NameID := 'col_M' + IntToStr(i);;
  cp.Caption := 'M' + IntToStr(i);
  cp.Width := 60;
  end;

  { Finish }
  cp := ColsAvail.Add;
  cp.NameID := 'col_M0';
  cp.Caption := 'Finish';
  cp.Width := 60;

  { Standing }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Standing';
  cp.Caption := 'Standing';
  cp.Width := 60;
end;

{ TRoundingsNode }

procedure TRoundingsNode.Calc;
begin
  inherited;
end;

constructor TRoundingsNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TRoundingsCollection.Create(Self,
    TRoundingsCollectionItem);
end;

destructor TRoundingsNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TRoundingsNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TRoundingsColProp;
end;

function TRoundingsNode.GeRoundingsBO: TRoundingsBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TRoundingsBO;
end;

function TRoundingsNode.GetRoundingsCollection: TRoundingsCollection;
begin
  result := BaseRowCollection as TRoundingsCollection;
end;

procedure TRoundingsNode.Init(RowCount: Integer);
var
  i: Integer;
begin
  BaseRowCollection.Clear;
  for i := 0 to RowCount - 1 do
  begin
    RoundingsCollection.Add;
  end;
end;

procedure TRoundingsNode.Load;
begin
  RoundingsCollection.Clear;
end;

{ TRoundingsBO }

constructor TRoundingsBO.Create;
begin
  inherited;
  TableLayout := 1;
end;

function TRoundingsBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TRoundingsBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TRoundingsBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
var
  i: Integer;
  mc: Integer;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');
    AddColumn('col_Idx');
    mc := Main.GuiManager.MarkCount-1;
    for i := 1 to mc do
      AddColumn('col_M' + IntToStr(i));
    AddColumn('col_M0');
    if TableLayout = 2 then
      AddColumn('col_Standing');
  end;
end;

procedure TRoundingsBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TRoundingsBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

end.
