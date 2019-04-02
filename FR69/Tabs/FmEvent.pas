unit FmEvent;

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
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Col.Event;

type
  TEventTab = class(TFrame)
    ToolBar: TToolBar;
    PointsBtn: TSpeedButton;
    FinishBtn: TSpeedButton;
    StrictBtn: TSpeedButton;
    DollarBtn: TSpeedButton;
    ColorBtn: TSpeedButton;
    SelectRaceBtn: TSpeedButton;
    ThrowoutPlusBtn: TSpeedButton;
    ThrowoutBtn: TSpeedButton;
    ThrowoutMinusBtn: TSpeedButton;
    UndoBtn: TSpeedButton;
    RedoBtn: TSpeedButton;
    UpdateBtn: TSpeedButton;
    CalcBtn: TSpeedButton;
    procedure LayoutBtnClick(Sender: TObject);
    procedure StrictBtnClick(Sender: TObject);
    procedure DollarBtnClick(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure SelectRaceBtnClick(Sender: TObject);
    procedure ThrowoutMinusBtnClick(Sender: TObject);
    procedure ThrowoutBtnClick(Sender: TObject);
    procedure ThrowoutPlusBtnClick(Sender: TObject);
  private
    procedure HandleModifiedNotification(Sender: TObject);
    procedure UpdateThrowouts(NewValue: Integer);
    procedure InitDefaultBtnText;
    procedure InitAlternativeBtnText;
    procedure InitPrimaryBtnText;
  public
    ColBO: TEventBO;
    Node: TEventNode;
    GB: TGridBlock;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExportBtnText;
    procedure InitBtnText;
    procedure InitGrid;
    procedure DisposeGrid;
    procedure UpdateGrid;
    procedure UpdateButtonCaptions;
    function GetSelectedRaceIndex: Integer;
    procedure UpdateStrictRelaxed;
    procedure UpdateColorMode;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.App.Translation,
  RiggVar.BO.Def,
  RiggVar.App.GuiManager,
  RiggVar.App.GuiInterface;

constructor TEventTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'EventGrid';
  InitBtnText;
end;

destructor TEventTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TEventTab.InitGrid;
begin
  Assert(GB.ColGrid = nil);

  ColBO := BO.EventBO;
  Node := BO.EventNode;
  GB.ColBO := ColBO;
  GB.Node := Node;

  GB.InitGrid;

  ThrowoutBtn.Caption := IntToStr(BO.EventProps.Throwouts);
  BO.EventBO.RelaxedInputMode := false;
  Node.OnModified := HandleModifiedNotification;

  UpdateButtonCaptions;
end;

procedure TEventTab.DisposeGrid;
begin
  GB.DisposeGrid;
  ColBO := nil;
  Node := nil;
end;

procedure TEventTab.UpdateButtonCaptions;
begin
  if BO.EventBO.RelaxedInputMode then
    StrictBtn.Caption := 'Relaxed'
  else
    StrictBtn.Caption := 'Strict';

  if BO.EventNode.ColorMode = ColorMode_Error then
    ColorBtn.Caption := 'Color E'
  else if BO.EventNode.ColorMode = ColorMode_Fleet then
    ColorBtn.Caption := 'Color F'
  else
    ColorBtn.Caption := 'Color N';

  UndoBtn.Caption := 'Undo (' + IntToStr(BO.UndoManager.UndoCount) +')';
  RedoBtn.Caption := 'Redo (' + IntToStr(BO.UndoManager.RedoCount) +')';
end;

procedure TEventTab.UpdateBtnClick(Sender: TObject);
begin
  if BO <> nil then
  begin
    UpdateBtn.Enabled := False;
    try
      BO.EventNode.ErrorList.CheckAll(BO.EventNode);
      GB.ColGrid.UpdateAll;
    finally
      UpdateBtn.Enabled := True;
    end;
  end;
end;

procedure TEventTab.UndoBtnClick(Sender: TObject);
begin
  Main.GuiManager.UndoExecute(Sender);
end;

procedure TEventTab.RedoBtnClick(Sender: TObject);
begin
  Main.GuiManager.RedoExecute(Sender);
end;

procedure TEventTab.DollarBtnClick(Sender: TObject);
var
  r: Integer;
  cr: TEventRowCollectionItem;
  sValue: string;
begin
  r := GetSelectedRaceIndex;
  if r <> -1 then
  begin
    cr := BO.EventNode.EventRowCollection.Items[0];
    if cr <> nil then
    begin
      sValue := '$';
      BO.EventBO.EditRaceValue(cr, sValue, 'colR_' + IntToStr(r));
      BO.EventNode.Modified := True;
      GB.GridUpdate.InvalidateView;
    end;
  end;
end;

procedure TEventTab.SelectRaceBtnClick(Sender: TObject);
begin
  Main.GuiManager.Race := GetSelectedRaceIndex;
end;

procedure TEventTab.StrictBtnClick(Sender: TObject);
begin
  BO.EventBO.RelaxedInputMode := not BO.EventBO.RelaxedInputMode;
  UpdateStrictRelaxed;
end;

function TEventTab.GetSelectedRaceIndex: Integer;
var
  c: Integer;
  sColCaption: string;
  sRaceIndex: string;
  RaceIndex: Integer;
  sPrefix: string;
begin
  c := GB.Grid.Selection.Left;
  sColCaption := GB.Grid.Cells[c, 0];
  sPrefix := 'R';
  RaceIndex := -1;
  if (Copy(sColCaption, 1, Length(sPrefix)) = sPrefix) then
  begin
    sRaceIndex := Copy(sColCaption, Length(sPrefix)+1, Length(sColCaption));
    RaceIndex := StrToIntDef(sRaceIndex, -1);
  end;
  result := RaceIndex;
end;

procedure TEventTab.CalcBtnClick(Sender: TObject);
begin
  BO.EventNode.Modified := True;
  BO.Calc;
end;

procedure TEventTab.ColorBtnClick(Sender: TObject);
var
  ColorMode: TColorMode;
begin
  ColorMode := BO.EventNode.ColorMode;
  if ColorMode < High(TColorMode) then
    Inc(ColorMode)
  else
    ColorMode := Low(TColorMode);
  case ColorMode of
    ColorMode_None: ColorBtn.Caption := 'Color N';
    ColorMode_Error: ColorBtn.Caption := 'Color E';
    ColorMode_Fleet: ColorBtn.Caption := 'Color F';
  end;
  BO.EventNode.ColorMode := ColorMode;
  GB.GridUpdate.InvalidateView;
end;

procedure TEventTab.HandleModifiedNotification(Sender: TObject);
begin
  if Sender is TEventNode then
  begin
    if Assigned(Main.GuiManager.CacheMotor) then
      Main.GuiManager.CacheMotor.Synchronize;
  end;
end;

procedure TEventTab.UpdateStrictRelaxed;
begin
  if BO.EventBO.RelaxedInputMode then
    StrictBtn.Caption := 'Relaxed'
  else
    StrictBtn.Caption := 'Strict';
end;

procedure TEventTab.UpdateColorMode;
begin
  case BO.EventNode.ColorMode of
    ColorMode_None: ColorBtn.Caption := 'Color N';
    ColorMode_Error: ColorBtn.Caption := 'Color E';
    ColorMode_Fleet: ColorBtn.Caption := 'Color F';
  end;
end;

procedure TEventTab.LayoutBtnClick(Sender: TObject);
begin
  BO.EventNode.ShowPoints := (Sender as TComponent).Tag;
  GB.GridUpdate.InvalidateView;
end;

procedure TEventTab.ThrowoutMinusBtnClick(Sender: TObject);
begin
  UpdateThrowouts(BO.EventProps.Throwouts - 1);
end;

procedure TEventTab.ThrowoutBtnClick(Sender: TObject);
begin
  ThrowoutBtn.Caption := IntToStr(BO.EventProps.Throwouts);
end;

procedure TEventTab.ThrowoutPlusBtnClick(Sender: TObject);
begin
  UpdateThrowouts(BO.EventProps.Throwouts + 1);
end;

procedure TEventTab.UpdateThrowouts(NewValue: Integer);
begin
  if NewValue < 0 then
    //do nothing
  else if NewValue >= BO.BOParams.RaceCount then
    //do nothing
  else
  begin
    if BO.EventProps.Throwouts <> NewValue then
    begin
      Main.GuiManager.Throwouts := NewValue;
      ThrowoutBtn.Caption := IntToStr(Main.GuiManager.Throwouts);

      //BO.EventProps.Throwouts := NewValue;
      //BO.EventNode.Modified := True;
      //ThrowoutBtn.Caption := IntToStr(BO.EventProps.Throwouts);

      GB.GridUpdate.InvalidateView;
    end;
  end;
end;

procedure TEventTab.InitBtnText;
begin
  if Main.Params.WantLocalText then
  begin
    PointsBtn.Caption := GetText(EV_PointsBtn_Caption);
    FinishBtn.Caption := GetText(EV_FinishBtn_Caption);
    SelectRaceBtn.Caption := GetText(EV_SelectRaceBtn_Caption);

    PointsBtn.Hint := GetText(EV_PointsBtn_Hint);
    FinishBtn.Hint := GetText(EV_FinishBtn_Hint);
    StrictBtn.Hint := GetText(EV_StrictBtn_Hint);
    ThrowoutMinusBtn.Hint := GetText(EV_ThrowoutMinusBtn_Hint);
    ThrowoutBtn.Hint := GetText(EV_ThrowoutBtn_Hint);
    ThrowoutPlusBtn.Hint := GetText(EV_ThrowoutPlusBtn_Hint);
    DollarBtn.Hint := GetText(EV_DollarBtn_Hint);
    ColorBtn.Hint := GetText(EV_ColorBtn_Hint);
    SelectRaceBtn.Hint := GetText(EV_SelectRaceBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TEventTab.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TEventTab.InitPrimaryBtnText;
begin
  PointsBtn.Caption := 'Points';
  FinishBtn.Caption := 'Finish';
  SelectRaceBtn.Caption := 'Select Race';

  PointsBtn.Hint := 'show calculated points';
  FinishBtn.Hint := 'show finish position input';
  StrictBtn.Hint := 'enforces contiguous range of finish positions';
  ThrowoutMinusBtn.Hint := 'throwouts down';
  ThrowoutBtn.Hint := 'display for number of throwouts';
  ThrowoutPlusBtn.Hint := 'throwouts up';
  DollarBtn.Hint := 'swap race enabled';
  ColorBtn.Hint := 'color mode: F(leet), E(rror), N(None)';
  SelectRaceBtn.Hint := 'select race (set current)';
end;

procedure TEventTab.InitAlternativeBtnText;
begin
  PointsBtn.Caption := 'Punkte';
  FinishBtn.Caption := 'Platz';
  SelectRaceBtn.Caption := 'WF wählen';

  PointsBtn.Hint := 'berechnete Punkte anzeigen';
  FinishBtn.Hint := 'Zielpositionen anzeigen';
  StrictBtn.Hint := 'fortlaufende Zielpositionen erzwingen';
  ThrowoutMinusBtn.Hint := 'Anzahl Streicher verringern';
  ThrowoutBtn.Hint := 'Anzeige für die Azahl der Streicher (Throwouts)';
  ThrowoutPlusBtn.Hint := 'Anzahl Streicher erhöhen';
  DollarBtn.Hint := 'Wettfahrt aktivieren/deaktivieren';
  ColorBtn.Hint := 'Farbmodus: F(leet), E(rror), N(None)';
  SelectRaceBtn.Hint := 'die Wettahrt auswählen (aktuell setzen)';
end;

procedure TEventTab.ExportBtnText;
begin
  InitDefaultBtnText;

  SetText(EV_PointsBtn_Caption, PointsBtn.Caption);
  SetText(EV_FinishBtn_Caption, FinishBtn.Caption);
  SetText(EV_SelectRaceBtn_Caption, SelectRaceBtn.Caption);

  SetText(EV_PointsBtn_Hint, PointsBtn.Hint);
  SetText(EV_FinishBtn_Hint, FinishBtn.Hint);
  SetText(EV_StrictBtn_Hint, StrictBtn.Hint);
  SetText(EV_ThrowoutMinusBtn_Hint, ThrowoutMinusBtn.Hint);
  SetText(EV_ThrowoutBtn_Hint, ThrowoutBtn.Hint);
  SetText(EV_ThrowoutPlusBtn_Hint, ThrowoutPlusBtn.Hint);
  SetText(EV_DollarBtn_Hint, DollarBtn.Hint);
  SetText(EV_ColorBtn_Hint, ColorBtn.Hint);
  SetText(EV_SelectRaceBtn_Hint, SelectRaceBtn.Hint);
end;

procedure TEventTab.UpdateGrid;
begin
  GB.ColGrid.UpdateAll;
end;

end.
