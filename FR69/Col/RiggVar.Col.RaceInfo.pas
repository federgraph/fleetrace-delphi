unit RiggVar.Col.RaceInfo;

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
  RiggVar.Grid.ColGrid;

type
  TRaceInfoEntry = class(TPersistent)
  public
    Bib: Integer;
    Time: string;
    Behind: string;
    Rank: Integer;
    PosR: Integer;
    QU: string;
  end;

  TRaceInfoNode = class;

  TRaceInfoRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FBib: Integer;
    FTime: string;
    FBehind: string;
    FRank: Integer;
    FPosR: Integer;
    FQU: string;
    function GetRaceInfoNode: TRaceInfoNode;
    procedure SetModified(const Value: Boolean);
  public
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TRaceInfoNode read GetRaceInfoNode;
  published
    property Bib: Integer read FBib write FBib;
    property Time: string read FTime write FTime;
    property Behind: string read FBehind write FBehind;
    property Rank: Integer read FRank write FRank;
    property PosR: Integer read FPosR write FPosR;
    property QU: string read FQU write FQU;
  end;

  TRaceInfoRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TRaceInfoRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TRaceInfoRowCollectionItem);
  public
    function Add: TRaceInfoRowCollectionItem;
    procedure UpdateItem(e: TRaceInfoEntry);
    function FindKey(Bib: Integer): TRaceInfoRowCollectionItem;
    property Items[Index: Integer]: TRaceInfoRowCollectionItem read GetItem
      write SetItem;
  end;

  TRaceInfoColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
      {
    procedure GetSortKeyRace(crgs: TBaseRowCollectionItem;
      var Value: string; const ColName: string);
      }
  end;

  TRaceInfoBO = class;

  TRaceInfoNode = class(TBaseNode)
  private
    function GetRaceInfoRowCollection: TRaceInfoRowCollection;
    function GetRaceInfoBO: TRaceInfoBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    property RaceInfoRowCollection: TRaceInfoRowCollection read
      GetRaceInfoRowCollection;
    property RaceInfoBO: TRaceInfoBO read
      GetRaceInfoBO;
  end;

  TRaceInfoBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure InitColsActive(StringGrid: TColGrid); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

implementation

uses
  RiggVar.BO.Def;

{ TRaceInfoRowCollectionItem }

constructor TRaceInfoRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;
end;

destructor TRaceInfoRowCollectionItem.Destroy;
begin
  inherited;
end;

procedure TRaceInfoRowCollectionItem.Assign(Source: TPersistent);
var
  o: TRaceInfoRowCollectionItem;
  e: TRaceInfoEntry;
begin
  if Source is TRaceInfoRowCollectionItem then
  begin
    o := TRaceInfoRowCollectionItem(Source);
    Bib := o.Bib;
    Time := o.Time;
    Behind := o.Behind;
    Rank := o.Rank;
    PosR := o.PosR;
    QU := o.QU;
  end
  else if Source is TRaceInfoEntry then
  begin
    e := TRaceInfoEntry(Source);
    Bib := e.Bib;
    Time := e.Time;
    Behind := e.Behind;
    Rank := e.Rank;
    PosR := e.PosR;
    QU := e.QU;
  end
  else
    inherited Assign(Source);
end;

procedure TRaceInfoRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

function TRaceInfoRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TRaceInfoRowCollection;
begin
  cl := Collection as TRaceInfoRowCollection;
  result := cl.BaseNode;
end;

function TRaceInfoRowCollectionItem.GetRaceInfoNode: TRaceInfoNode;
var
  cl: TRaceInfoRowCollection;
begin
  result := nil;
  cl := Collection as TRaceInfoRowCollection;
  if cl.BaseNode is TRaceInfoNode then
    result := TRaceInfoNode(cl.BaseNode);
end;

{ TRaceInfoRowCollection }

function TRaceInfoRowCollection.GetItem(Index: Integer):
  TRaceInfoRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TRaceInfoRowCollectionItem(inherited GetItem(Index));
end;

procedure TRaceInfoRowCollection.SetItem(Index: Integer; const Value:
  TRaceInfoRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

function TRaceInfoRowCollection.Add: TRaceInfoRowCollectionItem;
begin
  Result := TRaceInfoRowCollectionItem(inherited Add);
  Result.BaseID := Count;
end;

procedure TRaceInfoRowCollection.UpdateItem(e: TRaceInfoEntry);
var
  o: TRaceInfoRowCollectionItem;
begin
  o := FindKey(e.Bib);
  if not Assigned(o) then
    o := Add;
  if Assigned(o) then
    o.Assign(e);
end;

function TRaceInfoRowCollection.FindKey(Bib: Integer):
  TRaceInfoRowCollectionItem;
var
  i: Integer;
  o: TRaceInfoRowCollectionItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.Bib = Bib) then
    begin
      result := o;
      break;
    end;
  end;
end;

{ TRaceInfoColProp }

procedure TRaceInfoColProp.GetTextDefault(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceInfoRowCollectionItem;
begin
  Value := '';
  if crgs is TRaceInfoRowCollectionItem then
    cr := TRaceInfoRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_Bib' then
    Value := IntToStr(cr.Bib)

  else if NameID = 'col_Time' then
    Value := cr.Time

  else if NameID = 'col_Behind' then
    Value := cr.Behind

  else if NameID = 'col_Rank' then
    Value := IntToStr(cr.Rank)

  else if NameID = 'col_PosR' then
    Value := IntToStr(cr.PosR)

  else if NameID = 'col_QU' then
    Value := cr.QU
end;

procedure TRaceInfoColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
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
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Time }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Time';
  cp.Caption := 'Time';
  cp.Width := 60;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeString;

  { Behind }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Behind';
  cp.Caption := 'Behind';
  cp.Width := 60;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeString;

  { Rank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Rank';
  cp.Caption := 'Rank';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;

  { PosR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PosR';
  cp.Caption := 'PosR';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;

  { QU }
  cp := ColsAvail.Add;
  cp.NameID := 'col_QU';
  cp.Caption := 'QU';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeString;
end;

{ TRaceInfoNode }

constructor TRaceInfoNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TRaceInfoRowCollection.Create(Self,
    TRaceInfoRowCollectionItem);
end;

destructor TRaceInfoNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TRaceInfoNode.GetRaceInfoRowCollection: TRaceInfoRowCollection;
begin
  result := BaseRowCollection as TRaceInfoRowCollection;
end;

function TRaceInfoNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TRaceInfoColProp;
end;

procedure TRaceInfoNode.Save;
begin
end;

procedure TRaceInfoNode.Load;
var
  o: TRaceInfoRowCollectionItem;
begin
  RaceInfoRowCollection.Clear;

  o := RaceInfoRowCollection.Add;
  o.Bib := 1;
  o.Time := '1.00';
  o.Behind := '0.00';
  o.Rank := 1;
  o.PosR := 1;

  o := RaceInfoRowCollection.Add;
  o.Bib := 2;
  o.Time := '2.00';
  o.Behind := '1.00';
  o.Rank := 2;
  o.PosR := 2;

  o := RaceInfoRowCollection.Add;
  o.Bib := 3;
  o.Time := '3.00';
  o.Behind := '2.00';
  o.Rank := 3;
  o.PosR := 3;
end;

procedure TRaceInfoNode.Init(RowCount: Integer);
var
  o: TRaceInfoRowCollectionItem;
  i: Integer;
begin
  BaseRowCollection.Clear;

  for i := 0 to RowCount - 1 do
  begin
    o := RaceInfoRowCollection.Add;
    o.Bib := i + 1;
    o.Rank := i + 1;
    o.PosR := i + 1;
    o.Time := IntToStr(i + 1) + '.00';
    o.Behind := IntToStr(i) + '.00';
  end;
end;

procedure TRaceInfoNode.Calc;
begin
end;

function TRaceInfoNode.GetRaceInfoBO: TRaceInfoBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TRaceInfoBO;
end;

{ TRaceInfoBO }

procedure TRaceInfoBO.InitColsActive(StringGrid: TColGrid);
begin
  //Race-Layout
  if self.GetCurrentNode.Layout = 0 then
  begin
    with StringGrid do
    begin
      ColsActive.Clear;
      AddColumn('col_Bib');
      AddColumn('col_QU');
      AddColumn('col_Time');
      AddColumn('col_Behind');
      AddColumn('col_Rank');
      AddColumn('col_PosR');
    end;
  end

  //IT-Layout
  else
  begin
    with StringGrid do
    begin
      ColsActive.Clear;
      AddColumn('col_Bib');
      //AddColumn('col_QU');
      AddColumn('col_Time');
      AddColumn('col_Behind');
      AddColumn('col_Rank');
      //AddColumn('col_PosR');
    end;
  end;

end;

function TRaceInfoBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

function TRaceInfoBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

procedure TRaceInfoBO.SetCurrentRow(const Value:
  TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TRaceInfoBO.SetCurrentNode(const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

end.
