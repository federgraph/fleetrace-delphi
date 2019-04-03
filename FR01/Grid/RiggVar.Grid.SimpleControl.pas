unit RiggVar.Grid.SimpleControl;

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
  System.Classes,
  RiggVar.Grid.ColGrid;

type
  TSimpleGridControl = class
  private
    FHeaderRowIndex: Integer;
    SL: TStringList;
    FRowCount: Integer;
    FRow: Integer;
    FColCount: Integer;
    function GetCells(c, r: Integer): string;
    procedure SetCells(c, r: Integer; const Value: string);
    procedure SetColCount(const Value: Integer);
    procedure SetRow(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure CheckSpace;
    function GetCellCount: Integer;
    property CellCount: Integer read GetCellCount;
  public
    ColGrid: TColGrid;
    constructor Create;
    destructor Destroy; override;

    procedure SetupGrid;
    procedure ShowData;
    procedure ShowHeader;

    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
    property Cells[c, r: Integer]: string read GetCells write SetCells;
    property Row: Integer read FRow write SetRow;
  end;

implementation

{ TSimpleGridControl }

constructor TSimpleGridControl.Create;
begin
  SL := TStringList.Create;
  ColCount := 1;
  RowCount := 1;
  Row := 1;
end;

destructor TSimpleGridControl.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TSimpleGridControl.CheckSpace;
var
  a: Integer;
begin
  a := CellCount;
  while SL.Count > a do
      SL.Delete(a);
  while SL.Count < a do
    SL.Add('');
  Assert(a = SL.Count, 'SL must contain CellCount strings.');
end;

function TSimpleGridControl.GetCellCount: Integer;
begin
  result := (FRowCount + 1) * FColCount;
end;

function TSimpleGridControl.GetCells(c, r: Integer): string;
var
  a: Integer;
begin
  a := r * FColCount + c;
  if a < SL.Count then
    result := SL[a]
  else
    result := '';
end;

procedure TSimpleGridControl.SetCells(c, r: Integer; const Value: string);
var
  a: Integer;
begin
  a := r * FColCount + c;
  if a < SL.Count then
    SL[r * FColCount + c] := Value;
end;

procedure TSimpleGridControl.SetColCount(const Value: Integer);
begin
  FColCount := Value;
  CheckSpace;
end;

procedure TSimpleGridControl.SetRow(const Value: Integer);
begin
  if FRow < FRowCount then
    FRow := Value;
end;

procedure TSimpleGridControl.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
  CheckSpace;
end;

procedure TSimpleGridControl.SetupGrid;
var
  cl: TBaseRowCollection;
  i: Integer;
begin
  cl := ColGrid.GetBaseRowCollection;

  { init RowCount, clear visible cells }
  if Assigned(cl) and (cl.Count > 0) then
    RowCount := cl.FilteredCount + 1 + FHeaderRowIndex
  else
    RowCount := 2 + FHeaderRowIndex;

  for i := 0 to CellCount - 1 do
    SL[i] := '';

  { init width of columns, show captions }
  ShowHeader;
end;

procedure TSimpleGridControl.ShowHeader;
var
  i: Integer;
  cp: TBaseColProp;
begin
  ColCount := ColGrid.ColsActive.VisibleCount; // ColCount always >= 1
  for i := 0 to ColGrid.ColsActive.Count - 1 do
  begin
    cp := ColGrid.ColsActive[i];
    if Assigned(cp) and cp.Visible then
    begin
      //ColWidths[i] := cp.Width;
      if not ColGrid.MenuMode then
        Cells[i, 0] := cp.Caption;
    end;
  end;
end;

procedure TSimpleGridControl.ShowData;
var
  i, j: Integer;
  cr: TBaseRowCollectionItem;
  cl: TBaseRowCollection;
  r: Integer;
begin
  //if EditorMode then exit;
  cl := ColGrid.GetBaseRowCollection;
  if Assigned(cl) then
  begin
    { check RowCount }
    if RowCount <> cl.Count + 1 + FHeaderRowIndex then
    begin
      if cl.Count > 0 then
        RowCount := cl.FilteredCount + 1 + FHeaderRowIndex
      else
        RowCount := 2 + FHeaderRowIndex;
    end;
    { update all rows }
    r := 0 + FHeaderRowIndex;
    for j := 0 to cl.Count - 1 do
    begin
      i := j;
      if (ColGrid.DisplayOrder.Count = cl.FilteredCount) then
        i := ColGrid.DisplayOrder.DisplayIndex[j];
      if (i < 0) or (i > cl.Count-1) then Continue;
      cr := cl[i];
      if cr.IsInFilter then
      begin
        Inc(r);
        if Assigned(ColGrid.ColsActive) then
          ColGrid.ColsActive.UpdateRow(ColGrid.GridModel, r, cr);
      end;
    end;
  end;
end;

end.
