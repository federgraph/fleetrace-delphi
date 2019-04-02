unit FmCourse;

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
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  RiggVar.EM.SpeedBtn,
  RiggVar.EM.SpeedBtn03,
  RiggVar.RM.RaceBar,
  RiggVar.RM.RaceCombo;

type
  TCourseTab = class(TFrame)
    RacePanel: TPanel;
    ITPanel: TPanel;
    ButtonPanel: TPanel;
  private
    procedure CreateRaceBar;
    procedure CreateTimepointBar;
    procedure CreateButtonBar;

    procedure RaceBtnClick(Sender: TObject);
    procedure TimepointBtnClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);

    procedure CreateBars;
    procedure InitBars;
    procedure InitItems;
    procedure UpdateItems;

    function GetIT: Integer;
    function GetRace: Integer;
    procedure SetIT(const Value: Integer);
    procedure SetRace(const Value: Integer);
  protected
    property IT: Integer read GetIT write SetIT;
    property Race: Integer read GetRace write SetRace;
  public
    RaceBar: TButtonBar;
    TimepointBar: TButtonBar;
    ButtonBar: TButtonBar;

    RaceLayout: TSpeedPanelLayout;
    TimepointLayout: TSpeedPanelLayout;
    ButtonLayout: TSpeedPanelLayout;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoNextR;
    procedure DoNextE;
    procedure DoUpdateCourse;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Def;

constructor TCourseTab.Create(AOwner: TComponent);
begin
  inherited;

  RacePanel.BevelOuter := bvNone;
  ITPanel.BevelOuter := bvNone;
  ButtonPanel.BevelOuter := bvNone;

  RacePanel.Height := 28 + 2 * 3;
  ITPanel.Height := RacePanel.Height;
  ButtonPanel.Height := RacePanel.Height;

  CreateBars;
  InitBars;
  InitItems;
  UpdateItems;
end;

destructor TCourseTab.Destroy;
begin
  RaceLayout.Free;
  RaceBar.Free;

  TimepointLayout.Free;
  TimepointBar.Free;

  ButtonLayout.Free;
  ButtonBar.Free;

  inherited;
end;

function TCourseTab.GetIT: Integer;
begin
  result := Main.GuiManager.IT;
end;

procedure TCourseTab.SetIT(const Value: Integer);
begin
  if Value <> Main.GuiManager.IT then
  begin
    Main.GuiManager.IT := Value;
  end;
end;

function TCourseTab.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

procedure TCourseTab.SetRace(const Value: Integer);
begin
  if Value <> Main.GuiManager.Race then
  begin
    Main.GuiManager.Race := Value;
  end;
end;

procedure TCourseTab.CreateRaceBar;
begin
  RaceBar := TButtonBar.Create;
  RaceBar.InitRaces;

  RaceLayout := TSpeedPanelLayout.Create;
  RaceLayout.BtnCount := BO.BOParams.RaceCount;
  RaceLayout.BtnClick := RaceBtnClick;
  RaceLayout.EventCombo := RaceBar;
  RaceLayout.CreateBar(RacePanel);
  RaceLayout.StyleID := 1;
end;

procedure TCourseTab.CreateTimepointBar;
begin
  TimepointBar := TButtonBar.Create;
  TimepointBar.InitTimepoints;

  TimepointLayout := TSpeedPanelLayout.Create;
  TimepointLayout.BtnCount := BO.BOParams.ITCount + 1;
  TimepointLayout.BtnClick := TimepointBtnClick;
  TimepointLayout.EventCombo := TimepointBar;
  TimepointLayout.CreateBar(ITPanel);
  TimepointLayout.StyleID := 1;
end;

procedure TCourseTab.CreateButtonBar;
begin
  ButtonBar := TButtonBar.Create;
  ButtonBar.InitClearButtons;

  ButtonLayout := TSpeedPanelLayout.Create;
  ButtonLayout.BtnCount := Integer(High(TClearButtons)) + 1;
  ButtonLayout.BtnClick := ButtonClick;
  ButtonLayout.EventCombo := ButtonBar;
  ButtonLayout.CreateBar(ButtonPanel);
  ButtonLayout.StyleID := 1;
end;

procedure TCourseTab.CreateBars;
begin
  CreateRaceBar;
  CreateTimepointBar;
  CreateButtonBar;
end;

procedure TCourseTab.InitBars;
begin
  RaceLayout.InitBar;
  TimepointLayout.InitBar;
  ButtonLayout.InitBar;
end;

procedure TCourseTab.InitItems;
begin
  RaceLayout.InitItems;
  TimepointLayout.InitItems;
  ButtonLayout.InitItems;
end;

procedure TCourseTab.UpdateItems;
begin
  RaceLayout.UpdateItems;
  TimepointLayout.UpdateItems;
  ButtonLayout.UpdateItems;
end;

procedure TCourseTab.RaceBtnClick(Sender: TObject);
var
  r: Integer;
begin
  r := RaceLayout.GetItemTag(Sender);
  Main.GuiManager.Race := r;
  //BO.Inform(TimePointChanged);
end;

procedure TCourseTab.TimepointBtnClick(Sender: TObject);
var
  tp: Integer;
begin
  tp := TimepointLayout.GetItemTag(Sender);
  Main.GuiManager.IT := tp - 1;
  //BO.Inform(TimePointChanged);
end;

procedure TCourseTab.ButtonClick(Sender: TObject);
var
  i: Integer;
  b: TClearButtons;
begin
  i := ButtonLayout.GetItemTag(Sender);
  b := TClearButtons(i-1);
  case b of
    ClearEvent: BO.ClearCommand;
    ClearRace:   BO.ClearRaceCommand(Race);
    ClearTimepoint:   BO.ClearTimepointCommand(Race, IT);
    ResetRace: BO.ResetRaceCommand(Race);
    GoBackToRace: BO.GoBackToRaceCommand(Race);

    NextR: DoNextR;
    NextE: DoNextE;
    UpdateCourse: DoUpdateCourse;
  end;
end;

procedure TCourseTab.DoNextR;
var
  cn: TCurrentNumbers;
begin
  cn := BO.FindCurrentInEvent(BO.CurrentNumbers);

  Main.GuiManager.Race := cn.race;
  Main.GuiManager.IT := cn.tp;
  DoUpdateCourse;
end;

procedure TCourseTab.DoNextE;
var
  cn: TCurrentNumbers;
begin
  cn := BO.FindCurrentInRace(BO.CurrentNumbers);
  Main.GuiManager.Race := cn.race;
  Main.GuiManager.IT := cn.tp;
  DoUpdateCourse;
end;

procedure TCourseTab.DoUpdateCourse;
begin
  if BO.BOParams.RaceCount <> RaceLayout.BtnCount then
  begin
    RaceLayout.BtnCount := BO.BOParams.RaceCount;
    RaceBar.InitRaces;
    RaceLayout.InitItems;
    RaceLayout.UpdateItems;
  end;
  if BO.BOParams.ITCount +1 <> TimepointLayout.BtnCount then
  begin
    TimepointLayout.BtnCount := BO.BOParams.ITCount + 1;
    TimepointBar.InitTimepoints;
    TimepointLayout.InitItems;
    TimepointLayout.UpdateItems;
  end;
end;

end.

