unit FmListing;

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
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  RiggVar.Grid.Block,
  RiggVar.Col.Listing;

type
  TListingTab = class(TFrame)
    ToolBar: TToolBar;
    UpdateBtn: TSpeedButton;
    procedure UpdateBtnClick(Sender: TObject);
  private
  public
    ColBO: TListingBO;
    Node: TListingNode;
    GB: TGridBlock;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitGrid;
    procedure DisposeGrid;
    procedure UpdateGrid;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TListingTab }

constructor TListingTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'ListingGrid';
end;

destructor TListingTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TListingTab.UpdateBtnClick(Sender: TObject);
begin
  UpdateGrid;
end;

procedure TListingTab.InitGrid;
begin
  ColBO := BO.ListingBO;
  Node := BO.ListingNode;
  GB.Node := Node;
  GB.ColBO := ColBO;

  GB.InitGrid;
  GB.ColGrid.HeatSize := 3;
end;

procedure TListingTab.DisposeGrid;
begin
  GB.DisposeGrid;
  ColBO := nil;
  Node := nil;
end;

procedure TListingTab.UpdateGrid;
begin
  GB.ColGrid.UpdateAll;
end;

end.
