unit RiggVar.Grid.Model;

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
  RiggVar.Grid.Control;

type
  TGridModel = class(TInterfacedObject, IColGrid)
  private
    Grid: TDisplayGrid;

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
    constructor Create(aGrid: TDisplayGrid);

    procedure CancelEdit;
    procedure InvalidateGrid;
    procedure SetupGrid;
    procedure ShowData;

    property HeaderRowIndex: Integer read GetHeaderRowIndex write SetHeaderRowIndex;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property Cells[c, r: Integer]: string read GetCells write SetCells;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FixedRows: Integer read GetFixedRows write SetFixedRows;
    property Row: Integer read GetRow write SetRow;
  end;

implementation

{ TGridModel }

procedure TGridModel.CancelEdit;
begin
  Grid.CancelEdit;
end;

constructor TGridModel.Create(aGrid: TDisplayGrid);
begin
  Grid := aGrid;
end;

function TGridModel.GetCells(c, r: Integer): string;
begin
  result := Grid.Cells[c, r];
end;

function TGridModel.GetColCount: Integer;
begin
  result := Grid.ColCount;
end;

function TGridModel.GetEnabled: Boolean;
begin
  result := Grid.Enabled;
end;

function TGridModel.GetFixedRows: Integer;
begin
  result := Grid.FixedRows;
end;

function TGridModel.GetHeaderRowIndex: Integer;
begin
  result := Grid.HeaderRowIndex;
end;

function TGridModel.GetRow: Integer;
begin
  result := Grid.Row;
end;

function TGridModel.GetRowCount: Integer;
begin
  result := Grid.RowCount;
end;

procedure TGridModel.InvalidateGrid;
begin
  Grid.InvalidateGrid;
end;

procedure TGridModel.SetCells(c, r: Integer; const Value: string);
begin
  Grid.Cells[c, r] := Value;
end;

procedure TGridModel.SetColCount(const Value: Integer);
begin
  Grid.ColCount := Value;
end;

procedure TGridModel.SetEnabled(const Value: Boolean);
begin
  Grid.Enabled := Value;
end;

procedure TGridModel.SetFixedRows(const Value: Integer);
begin
  Grid.FixedRows := Value;
end;

procedure TGridModel.SetHeaderRowIndex(Value: Integer);
begin
  Grid.HeaderRowIndex := Value;
end;

procedure TGridModel.SetRow(const Value: Integer);
begin
  Grid.Row := Value;
end;

procedure TGridModel.SetRowCount(const Value: Integer);
begin
  Grid.RowCount := Value;
end;

procedure TGridModel.SetupGrid;
begin
  Grid.SetupGrid;
end;

procedure TGridModel.ShowData;
begin
  Grid.ShowData;
end;

end.
