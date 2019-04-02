unit FmRoundings;

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
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  RiggVar.Col.Roundings,
  RiggVar.Grid.Block;

type
  TRoundingsTab = class(TFrame)
    ToolBar: TToolBar;
    UpdateBtn: TSpeedButton;
    RaceBtn: TSpeedButton;
    LayoutBtn: TSpeedButton;
    procedure UpdateBtnClick(Sender: TObject);
    procedure LayoutBtnClick(Sender: TObject);
  private
  public
    ColBO: TRoundingsBO;
    Node: TRoundingsNode;
    GB: TGridBlock;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitTimePoint;
    procedure InitGrid;
    procedure DisposeGrid;
    procedure UpdateGrid;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TRoundingsTab }

constructor TRoundingsTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'RoundingsGrid';
end;

destructor TRoundingsTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TRoundingsTab.UpdateBtnClick(Sender: TObject);
begin
  UpdateGrid;
end;

procedure TRoundingsTab.InitGrid;
begin
  ColBO := BO.RoundingsBO;
  Node := BO.RoundingsNode;
  GB.Node := Node;
  GB.ColBO := ColBO;

  GB.InitGrid;
  GB.ColGrid.HeatSize := 3;

  RaceBtn.Caption := 'R' + IntToStr(BO.Race);
end;

procedure TRoundingsTab.DisposeGrid;
begin
  GB.DisposeGrid;
  ColBO := nil;
  Node := nil;
end;

procedure TRoundingsTab.InitTimePoint;
begin
  if ColBO <> nil then
  begin
    DisposeGrid;
    InitGrid;
    RaceBtn.Caption := 'R' + IntToStr(BO.Race);
  end;
end;

procedure TRoundingsTab.LayoutBtnClick(Sender: TObject);
var
  l: Integer;
begin
  l := BO.RoundingsBO.TableLayout;

  //toggle layout
  if l = 1 then
    l := 2
  else
    l := 1;

  if ColBO <> nil then
  begin
    ColBO.TableLayout := l;
    DisposeGrid;
    InitGrid;
  end;
end;

procedure TRoundingsTab.UpdateGrid;
begin
  GB.ColGrid.UpdateAll;
  RaceBtn.Caption := 'R' + IntToStr(BO.Race);
end;

end.
