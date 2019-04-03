unit RiggVar.Grid.SimpleBlock;

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
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleControl,
  RiggVar.Grid.SimpleModel;

type
  TSimpleGridBlock = class
  protected
    function GetBaseNode: TBaseNode;
  public
    Name: string;
    Layout: Integer;
    ColBO: TBaseColBO;
    Node: TBaseNode;
    Grid: TSimpleGridControl;
    ColGrid: TColGrid;
    constructor Create;
    destructor Destroy; override;
    procedure DisposeGrid; virtual;
    procedure InitGrid; virtual;
  end;

implementation

constructor TSimpleGridBlock.Create;
begin
  inherited Create;
end;

destructor TSimpleGridBlock.Destroy;
begin
  DisposeGrid;
  inherited Destroy;
end;

procedure TSimpleGridBlock.InitGrid;
begin
  Assert(ColBO <> nil);
  Assert(Node <> nil);

  if (ColGrid = nil) then
  begin
    ColGrid := TColGrid.Create;
    Grid := TSimpleGridControl.Create;
    ColGrid.GridModel := TSimpleGridModel.Create(Grid);
    Grid.ColGrid := ColGrid;
  end;

  ColGrid.Name := Name;
  ColGrid.OnGetBaseNode := GetBaseNode;
  ColGrid.SetColBOReference(ColBO);
  ColGrid.ColPropClass := Node.ColPropClass;
  ColGrid.ColsAvail.Init;

  //ColBO.InitColsActive(ColGrid);
  ColBO.InitColsActiveLayout(ColGrid, Layout);

  ColGrid.AlwaysShowCurrent := True;
  ColGrid.ExcelStyle := True;
  ColGrid.AutoMark := True;
end;

procedure TSimpleGridBlock.DisposeGrid;
begin
  ColBO := nil;
  Node := nil;

  if( ColGrid <> nil) then
  begin
    ColGrid.GridModel := nil;
  end;

  Grid.Free;
  Grid := nil;

  ColGrid.Free;
  ColGrid := nil;
end;

function TSimpleGridBlock.GetBaseNode: TBaseNode;
begin
  result := Node;
end;

end.
