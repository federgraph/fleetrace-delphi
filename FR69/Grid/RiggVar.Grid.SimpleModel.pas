unit RiggVar.Grid.SimpleModel;

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
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleControl;

type
  TSimpleGridModel = class(TInterfacedObject, IColGrid)
  private
    Grid: TSimpleGridControl;
    FEnabled: Boolean;
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
  public
    constructor Create(aGrid: TSimpleGridControl);

    procedure CancelEdit;
    procedure InvalidateGrid;
    procedure SetupGrid;
    procedure ShowData;
    procedure ShowHeader;

    property HeaderRowIndex: Integer read GetHeaderRowIndex write SetHeaderRowIndex;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property Cells[c, r: Integer]: string read GetCells write SetCells;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FixedRows: Integer read GetFixedRows write SetFixedRows;
    property Row: Integer read GetRow write SetRow;
  end;

implementation

{ TSimpleGridModel }

procedure TSimpleGridModel.CancelEdit;
begin
  //Grid.CancelEdit;
end;

constructor TSimpleGridModel.Create(aGrid: TSimpleGridControl);
begin
  Grid := aGrid;
  FEnabled := true;
end;

function TSimpleGridModel.GetCells(c, r: Integer): string;
begin
  result := Grid.Cells[c, r];
end;

function TSimpleGridModel.GetColCount: Integer;
begin
  result := Grid.ColCount;
end;

function TSimpleGridModel.GetEnabled: Boolean;
begin
  result := FEnabled;
end;

function TSimpleGridModel.GetFixedRows: Integer;
begin
  result := 1;
end;

function TSimpleGridModel.GetHeaderRowIndex: Integer;
begin
  result := 1;
end;

function TSimpleGridModel.GetRow: Integer;
begin
  result := Grid.Row;
end;

function TSimpleGridModel.GetRowCount: Integer;
begin
  result := Grid.RowCount;
end;

procedure TSimpleGridModel.InvalidateGrid;
begin
  //Grid.InvalidateGrid;
end;

procedure TSimpleGridModel.SetCells(c, r: Integer; const Value: string);
begin
  Grid.Cells[c, r] := Value;
end;

procedure TSimpleGridModel.SetColCount(const Value: Integer);
begin
  Grid.ColCount := Value;
end;

procedure TSimpleGridModel.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSimpleGridModel.SetFixedRows(const Value: Integer);
begin
  //Grid.FixedRows := Value;
end;

procedure TSimpleGridModel.SetHeaderRowIndex(Value: Integer);
begin
  //Grid.HeaderRowIndex := Value;
end;

procedure TSimpleGridModel.SetRow(const Value: Integer);
begin
  Grid.Row := Value;
end;

procedure TSimpleGridModel.SetRowCount(const Value: Integer);
begin
  Grid.RowCount := Value;
end;

procedure TSimpleGridModel.SetupGrid;
begin
  Grid.SetupGrid;
end;

procedure TSimpleGridModel.ShowData;
begin
  Grid.ShowData;
end;

procedure TSimpleGridModel.ShowHeader;
begin
  Grid.ShowHeader;
end;

end.
