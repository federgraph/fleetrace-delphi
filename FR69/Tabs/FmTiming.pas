unit FmTiming;

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
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.Conn.Intern,
  RiggVar.Util.InputMatrix;

type
  TTimingTab = class(TFrame)
    ToolBarTiming: TToolBar;
    TestBtn: TSpeedButton;
    RandomBtn: TSpeedButton;
    AgeBtn: TSpeedButton;
    SendBtn: TSpeedButton;
    TimingLog: TMemo;
    TimingMemo: TMemo;
    TimingGrid: TStringGrid;
    AutoSendBtn: TSpeedButton;
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure TimingGridDblClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure RandomBtnClick(Sender: TObject);
    procedure AgeBtnClick(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure AutoSendBtnClick(Sender: TObject);
  private
    TimingConnection: TConnection;
    UpdateCounter: Integer;
    procedure SetRace(const Value: Integer);
    function GetRace: Integer;
    procedure SetIT(const Value: Integer);
    function GetIT: Integer;
    procedure HandleTransponderEvent(bibIndex: Integer);
    procedure SendTimingMsg(s: string);
    procedure SendTimingMsg2(s: string);
    procedure SetupTimingGrid;
    procedure InitDimensions;
  protected
    property IT: Integer read GetIT write SetIT;
    property Race: Integer read GetRace write SetRace;
  public
    Model: TMatrix;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitTiming;
    procedure DisposeTiming;
    procedure UpdateGrid;
    procedure UpdateRaceCaption;
    procedure ResetAge;
    procedure UpdateAuto;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.App.GuiInterface,
  RiggVar.App.GuiManager,
  RiggVar.BO.MsgToken;

{ TTimingTab }

constructor TTimingTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBarTiming);
  Model := TMatrix.Create;
  Randomize;
end;

destructor TTimingTab.Destroy;
begin
  Model.Free;
  inherited;
end;

procedure TTimingTab.InitTiming;
begin
  InitDimensions;

  SetupTimingGrid;
  TimingGrid.Align := alClient;
  TimingConnection := BO.InputServer.Server.Connect('Timing.Input');

  //UpdateRaceCaption;
end;

procedure TTimingTab.DisposeTiming;
begin
  TimingConnection.Free;
  TimingConnection := nil;
end;

function TTimingTab.GetIT: Integer;
begin
  result := Main.GuiManager.IT;
end;

procedure TTimingTab.SetIT(const Value: Integer);
begin
  if Value <> Main.GuiManager.IT then
  begin
    Main.GuiManager.IT := Value;
  end;
end;

function TTimingTab.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

procedure TTimingTab.SetRace(const Value: Integer);
begin
  if Value <> Main.GuiManager.Race then
  begin
    Main.GuiManager.Race := Value;
  end;
end;

procedure TTimingTab.SetupTimingGrid;
var
  c, r: Integer;
begin
  TimingGrid.ColCount := Main.Params.TimingGridColCount + 1;
  TimingGrid.RowCount := Main.Params.TimingGridRowCount + 1;

  for r := 1 to Main.Params.TimingGridRowCount do
    TimingGrid.Cells[0, r] := IntToStr(r);
  for c := 1 to Main.Params.TimingGridColCount do
    TimingGrid.Cells[c, 0] := IntToStr(c);

  TimingGrid.Cells[0, 0] := 'Bib';

  for c := 0 to Main.Params.TimingGridColCount-1 do
    for r := 0 to Main.Params.TimingGridRowCount - 1 do
      TimingGrid.Cells[c + 1, r + 1] := Model.Cells[c, r];
end;

procedure TTimingTab.HandleTransponderEvent(bibIndex: Integer);
var
  col, row: Integer;
  s: string;
  bibValue: string;
  t: string;
begin
  { send Msg }
  t := FormatDateTime('hh:mm:ss.zz', Now);

  bibValue := IntToStr(bibIndex + 1);
  s := cTokenA + '.' + cTokenB + '.' + cTokenRace +
    IntToStr(Race) + '.Bib' + bibValue + '.IT' + IntToStr(IT) + ' = ' + t;
  if (bibIndex < Main.Params.TimingGridBibCount) then
    SendTimingMsg2(s);

  { paint cell }
  if (bibIndex < 0) or (bibIndex > 255) then
    Exit;

  col := bibIndex mod Main.Params.TimingGridColCount;
  row := bibIndex div Main.Params.TimingGridColCount;
  Inc(UpdateCounter);
  Model.Age[col, row] := UpdateCounter;
  Model.TimerTick;
  TimingGrid.Repaint;
end;

procedure TTimingTab.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Space) or (Key = VK_Return) then
    HandleTransponderEvent(TimingGrid.Col-1 + (TimingGrid.Row - 1) * Main.Params.TimingGridColCount);
end;

procedure TTimingTab.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ARow = 0) or (ACol = 0) then
    Exit;

  { set the background color }
  if (gdSelected in State) and
    (not (gdFocused in State) or
    ([goDrawFocusSelected, goRowSelect] * TimingGrid.Options <> [])) then
  begin
    TimingGrid.Canvas.Brush.Color := clAqua
  end
  else
  begin
  TimingGrid.Canvas.Brush.Color := Model.Colors[ACol-1, ARow-1];
  end;

  { and paint the text }
  Inc(Rect.Left, -4);
  TimingGrid.Canvas.TextRect(
    Rect, Rect.Left + 2, Rect.Top + 2, //add padding
    TimingGrid.Cells[ACol, ARow]
    );
end;

procedure TTimingTab.TimingGridDblClick(Sender: TObject);
begin
  HandleTransponderEvent(TimingGrid.Col-1 + (TimingGrid.Row - 1) * Main.Params.TimingGridColCount);
end;

procedure TTimingTab.TestBtnClick(Sender: TObject);
var
  s: string;
begin
  s := cTokenA + '.' + BO.BOParams.DivisionName + '.' + cTokenRace +
    IntToStr(Race) + '.Bib1.XX = Test';

  SendTimingMsg2(s);
end;

procedure TTimingTab.RandomBtnClick(Sender: TObject);
var
  bib: Integer;
begin
  bib := Random(Main.Params.TimingGridBibCount);
  HandleTransponderEvent(bib);
end;

procedure TTimingTab.AgeBtnClick(Sender: TObject);
begin
  ResetAge;
end;

procedure TTimingTab.SendBtnClick(Sender: TObject);
begin
  SendTimingMsg(TimingMemo.Text);
end;

procedure TTimingTab.SendTimingMsg(s: string);
begin
  if Assigned(TimingConnection) then
  begin
    TimingLog.Text := '';
    TimingConnection.HandleMsg(s);
    TimingLog.Text := s;
    BO.Calc;
    BO.Inform(ScheduleEventUpdate);
  end
  else
    TimingLog.Text := '';
end;

procedure TTimingTab.SendTimingMsg2(s: string);
begin
  if AutoSendBtn.Down then
  begin
    SendTimingMsg(s);
  end
  else
    TimingMemo.Text := s;
end;

procedure TTimingTab.UpdateRaceCaption;
begin
  //RaceBtn.Caption := 'R' + IntToStr(Race);
end;

procedure TTimingTab.ResetAge;
begin
  Model.ResetAge;
  TimingGrid.RePaint;
  UpdateCounter := 0;
end;

procedure TTimingTab.UpdateGrid;
begin
  InitDimensions;
  SetupTimingGrid;
  Model.ResetAge;
end;

procedure TTimingTab.InitDimensions;
var
  n: Integer;
  c: Integer;
  r: Integer;
begin
  n := BO.BOParams.StartlistCount;

  { number of columns }
  if n <= 40 then
    c := 10
  else if n <= 80 then
    c := 20
  else
    c := 25;

  { number of rows }
  r := n div c;
  if n mod c > 0 then
    Inc(r);

  if c * r > Model.Size - 1 then
  begin
    c := 25;
    r := 6;
  end;

  Main.Params.TimingGridColCount := c;
  Main.Params.TimingGridRowCount := r;
end;

procedure TTimingTab.AutoSendBtnClick(Sender: TObject);
begin
  Main.GuiManager.AutoSend := AutoSendBtn.Down;
end;

procedure TTimingTab.UpdateAuto;
begin
  AutoSendBtn.Down := Main.GuiManager.AutoSend;
end;

end.
