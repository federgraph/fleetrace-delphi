unit FmRace;

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
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.App.GuiManager,
  RiggVar.Col.Race,
  RiggVar.Grid.Block,
  RiggVar.Grid.ColGrid;

type
  TRaceTab = class(TFrame)
    ToolBar: TToolBar;
    RaceDownBtn: TSpeedButton;
    RaceBtn: TSpeedButton;
    RaceUpBtn: TSpeedButton;
    ITDownBtn: TSpeedButton;
    ITBtn: TSpeedButton;
    ITUpBtn: TSpeedButton;
    ToMRankBtn: TSpeedButton;
    FromMRankBtn: TSpeedButton;
    UpdateEventBtn: TSpeedButton;
    LayoutBtn: TSpeedButton;
    procedure ITDownBtnClick(Sender: TObject);
    procedure ITUpBtnClick(Sender: TObject);
    procedure RaceDownBtnClick(Sender: TObject);
    procedure RaceUpBtnClick(Sender: TObject);
    procedure FromMRankBtnClick(Sender: TObject);
    procedure ToMRankBtnClick(Sender: TObject);
    procedure UpdateEventBtnClick(Sender: TObject);
    procedure LayoutBtnClick(Sender: TObject);
  private
    procedure SetIT(const Value: Integer);
    procedure SetRace(const Value: Integer);
    function GetRace: Integer;
    function GetIT: Integer;
    function GetGuiManager: TGuiManager;
  protected
    property IT: Integer read GetIT write SetIT;
    property Race: Integer read GetRace write SetRace;
    property GM: TGuiManager read GetGuiManager;
  public
    ColBO: TRaceBO;
    Node: TRaceNode;
    GB: TGridBlock;
    OnUpdateIT: TNotifyEvent;
    OnUpdateRace: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitTimePoint;

    procedure InitGrid;
    procedure DisposeGrid;
    procedure UpdateGrid;

    procedure UpdateIT;
    procedure UpdateRace;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BO.MsgToken;

{ TRaceTab }

constructor TRaceTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'RaceGrid';
end;

destructor TRaceTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TRaceTab.InitGrid;
begin
  Assert(GB.ColGrid = nil);

  ColBO := BO.RaceBO;
  Node := BO.RNode[1];
  GB.ColBO := ColBO;
  GB.Node := Node;

  GB.InitGrid;

  UpdateIT;
  UpdateRace;
end;

procedure TRaceTab.DisposeGrid;
begin
  GB.DisposeGrid;
  ColBO := nil;
  Node := nil;
end;

procedure TRaceTab.UpdateGrid;
begin
  GB.ColGrid.UpdateAll;
end;

procedure TRaceTab.SetIT(const Value: Integer);
begin
  if Value <> GM.IT then
  begin
    GM.IT := Value;
    //UpdateIT;
  end;
end;

procedure TRaceTab.UpdateIT;
var
  temp: Integer;
begin
  temp := IT;

  ITBtn.Caption := 'IT' + IntToStr(temp);
  ITUpBtn.Enabled := temp < BO.BOParams.ITCount;
  ITDownBtn.Enabled := temp > 0;

  //if (temp >= 0) and (temp < BO.BOParams.ITCount) then
  begin
    ColBO.InitColsActiveLayout(GB.ColGrid, temp);
    GB.ColGrid.UpdateAll;
  end;

  if Assigned(OnUpdateIT) then
    OnUpdateIT(self);
end;

procedure TRaceTab.SetRace(const Value: Integer);
begin
  if Value <> GM.Race then
  begin
    GM.Race := Value;
    //UpdateRace;
  end;
end;

procedure TRaceTab.UpdateRace;
var
  temp: Integer;
begin
  temp := Race;
  Node := BO.RNode[temp];
  GB.Node := Node;
  RaceBtn.Caption := cTokenRace + IntToStr(temp);
  RaceUpBtn.Enabled := temp < BO.BOParams.RaceCount;
  RaceDownBtn.Enabled := temp > 1;

  //if (temp >= 0) and (temp < BO.BOParams.RaceCount) then
  begin
    ColBO.InitColsActiveLayout(GB.ColGrid, IT);
    GB.ColGrid.UpdateAll;
  end;

  if Assigned(OnUpdateRace) then
    OnUpdateRace(self);
end;

procedure TRaceTab.FromMRankBtnClick(Sender: TObject);
begin
  Node.CopyFromMRank;
end;

procedure TRaceTab.ToMRankBtnClick(Sender: TObject);
begin
  Node.CopyToMRank;
end;

procedure TRaceTab.UpdateEventBtnClick(Sender: TObject);
begin
  if Main.FormAdapter.ConfirmOperation('CopyFromRaceNode') then
    BO.CopyFromRaceNode(Node, False);
end;

procedure TRaceTab.ITDownBtnClick(Sender: TObject);
begin
  IT := IT - 1;
end;

procedure TRaceTab.ITUpBtnClick(Sender: TObject);
begin
  IT := IT + 1;
end;

procedure TRaceTab.LayoutBtnClick(Sender: TObject);
var
  l: Integer;
begin
  l := BO.RaceBO.TableLayout;

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

procedure TRaceTab.RaceDownBtnClick(Sender: TObject);
begin
  Race := Race - 1;
end;

procedure TRaceTab.RaceUpBtnClick(Sender: TObject);
begin
  Race := Race + 1;
end;

function TRaceTab.GetIT: Integer;
begin
  result := GM.IT;
end;

function TRaceTab.GetRace: Integer;
begin
  result := GM.Race;
end;

function TRaceTab.GetGuiManager: TGuiManager;
begin
  result := Main.GuiManager;
end;

procedure TRaceTab.InitTimePoint;
begin
  ColBO.InitColsActiveLayout(GB.ColGrid, Main.GuiManager.IT);
  UpdateGrid;
end;

end.
