unit RiggVar.Grid.Block;

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
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Control,
  RiggVar.Grid.Model,
  RiggVar.Grid.Update;

type
  TGridBlock = class
  private
    FOnIndexChanging: TRowChangedEvent;
    function GetBaseNode: TBaseNode;
    procedure OnEdit(Sender: TObject);
    procedure CellSelect(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnUpdateView(Sender: TObject);
    procedure SetOnIndexChanging(const Value: TRowChangedEvent);
  public
    Layout: Integer;
    Name: string;
    Parent: TWinControl;
    GridUpdate: TGridUpdate;
    ColBO: TBaseColBO;
    Node: TBaseNode;
    Grid: TDisplayGrid;
    ColGrid: TColGrid;
    constructor Create;
    destructor Destroy; override;
    procedure BeginInitGrid;
    procedure EndInitGrid;
    procedure InitGrid;
    procedure DisposeGrid;
    property OnIndexChanging: TRowChangedEvent read FOnIndexChanging write SetOnIndexChanging;
  end;

implementation

constructor TGridBlock.Create;
begin
  inherited Create;
  GridUpdate := TGridUpdate.Create;
end;

destructor TGridBlock.Destroy;
begin
  DisposeGrid;
  GridUpdate.Free;
  inherited Destroy;
end;

procedure TGridBlock.BeginInitGrid;
begin
  Assert(ColGrid = nil);
  Assert(Parent <> nil);
  Assert(ColBO <> nil);
  Assert(Node <> nil);

  if (ColGrid = nil) then
  begin
    ColGrid := TColGrid.Create;
    ColGrid.Name := Name;
    ColGrid.OnGetBaseNode := GetBaseNode;
    //ColGrid.UseHTML := True;

    Grid := TDisplayGrid.Create(Parent);
    Grid.Name := Name;
    Grid.Parent := Parent;
    Grid.Align := alClient;

    ColGrid.GridModel := TGridModel.Create(Grid);
    Grid.ColGrid := ColGrid;
  end;

  ColGrid.SetColBOReference(ColBO);
  ColGrid.ColPropClass := Node.ColPropClass;
  ColGrid.ColsAvail.Init;

  ColBO.InitColsActiveLayout(ColGrid, Layout);

  ColGrid.AlwaysShowCurrent := True;
  ColGrid.ExcelStyle := True;
  ColGrid.AutoMark := True;

  ColGrid.OnEdit := OnEdit;
  ColGrid.OnCellSelect := CellSelect;
end;

procedure TGridBlock.EndInitGrid;
begin
  if (ColGrid <> nil) then
  begin
    ColGrid.UpdateAll;
    GridUpdate.OnUpdateView := OnUpdateView;
    GridUpdate.ViewEnabled := True;
  end;
end;

procedure TGridBlock.InitGrid;
begin
  Assert(ColGrid = nil);
  BeginInitGrid;
  EndInitGrid;
end;

procedure TGridBlock.DisposeGrid;
begin
  if GridUpdate <> nil then
  begin
    GridUpdate.ViewEnabled := False;
    GridUpdate.OnUpdateView := nil;
  end;

  ColBO := nil;
  Node := nil;

  if( ColGrid <> nil) then
  begin
    ColGrid.GridModel := nil;
  end;

  FreeAndNil(Grid);

  ColGrid.Free;
  ColGrid := nil;

  Grid.Free;
  Grid := nil;
end;

function TGridBlock.GetBaseNode: TBaseNode;
begin
  result := Node;
end;

procedure TGridBlock.CellSelect(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  GridUpdate.DelayUpdate;
  if ARow <> Grid.Row then
    if Assigned(OnIndexChanging) then
      OnIndexChanging(Sender, ARow-1);
end;

procedure TGridBlock.OnEdit(Sender: TObject);
begin
  GridUpdate.InvalidateView;
end;

procedure TGridBlock.OnUpdateView(Sender: TObject);
begin
  if not Grid.EditorMode then
    Grid.ShowData;
end;

procedure TGridBlock.SetOnIndexChanging(const Value: TRowChangedEvent);
begin
  FOnIndexChanging := Value;
end;

end.
