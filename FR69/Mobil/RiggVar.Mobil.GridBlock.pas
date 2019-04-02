unit RiggVar.Mobil.GridBlock;

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
  Vcl.Controls,
  RiggVar.Mobil.Facade,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Col.Event;

type
  TMobilColGrid = class(TMobilFacade)
  private
    procedure HandleModifiedNotification(Sender: TObject);
    procedure UpdateView(Sender: TObject);
    procedure OnIndexChanging(Sender: TObject; NewIndex: Integer);
  protected
    procedure RaceChanged; override;
    procedure UpdateUI; override;
  public
    Parent: TWinControl;
    ColBO: TEventBO;
    Node: TEventNode;
    GB: TGridBlock;

    constructor Create;
    destructor Destroy; override;

    procedure InitGrid;
    procedure DisposeGrid;
  end;

implementation

uses
  RiggVar.BO.Def,
  RiggVar.Util.Classes;

constructor TMobilColGrid.Create;
begin
  inherited;
  GB := TGridBlock.Create;
  GB.Parent := Parent;
  GB.Name := 'MobilGrid';
  GB.Layout := 1;
  GB.OnIndexChanging := OnIndexChanging;
end;

destructor TMobilColGrid.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TMobilColGrid.InitGrid;
begin
  Assert(GB.ColGrid = nil);

  ColBO := BO.EventBO;
  Node := BO.EventNode;
  GB.ColBO := ColBO;
  GB.Node := Node;

  GB.InitGrid;

  BO.EventBO.RelaxedInputMode := false;
  Node.OnModified := HandleModifiedNotification;

  UpdateView(nil);
end;

procedure TMobilColGrid.DisposeGrid;
begin
  GB.DisposeGrid;
  ColBO := nil;
  Node := nil;
end;

procedure TMobilColGrid.RaceChanged;
begin
  ColBO.SetMobilRaceColumn(GB.ColGrid, CurrentRace);
end;

procedure TMobilColGrid.UpdateUI;
begin
  inherited;
  GB.Grid.ShowHeader;
  GB.Grid.ShowData;
end;

procedure TMobilColGrid.UpdateView(Sender: TObject);
begin
  if not GB.Grid.EditorMode then
  begin
    GB.ColGrid.ShowData;
  end;
end;

procedure TMobilColGrid.HandleModifiedNotification(Sender: TObject);
begin
  //Es kann nur einer benachrichtigt werden
  //Node.OnModified is a single cast event
  //it is now no longer attached to EventTab
  UpdateDetails;
//  if Sender is TEventNode then
//  begin
//    if Assigned(Main.GuiManager.CacheMotor) then
//      Main.GuiManager.CacheMotor.Synchronize;
//  end;
end;

procedure TMobilColGrid.OnIndexChanging(Sender: TObject; NewIndex: Integer);
begin
  CurrentIndex := NewIndex;
  inherited UpdateUI; //do not draw grid, only details
end;

end.
