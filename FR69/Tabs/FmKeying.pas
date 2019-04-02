unit FmKeying;

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
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  RiggVar.Conn.Intern;

type
  TKeyTab = class(TFrame)
    ToolBar: TToolBar;
    RaceDownBtn: TSpeedButton;
    RaceBtn: TSpeedButton;
    RaceUpBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    SendBtn: TSpeedButton;
    AutoSendBtn: TSpeedButton;
    edKey: TEdit;
    HelpLabel: TLabel;
    MsgLabel: TLabel;
    cbWantMessageBeep: TCheckBox;
    ResultLabel: TLabel;
    NameLabel: TLabel;
    SeriesLabel: TLabel;
    procedure RaceDownBtnClick(Sender: TObject);
    procedure RaceBtnClick(Sender: TObject);
    procedure RaceUpBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure edKeyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edKeyKeyPress(Sender: TObject; var Key: Char);
    procedure AutoSendBtnClick(Sender: TObject);
  private
    TimingConnection: TConnection;
    function GetRace: Integer;
    procedure SetRace(const Value: Integer);
    procedure HandleTransponderEvent(Bib: Integer);
    procedure SendTimingMsg(s: string);
    procedure SendTimingMsg2(s: string);
    procedure ShowResult(Race, Bib: Integer);
    procedure InitDefaultBtnText;
    procedure InitAlternativeBtnText;
    procedure InitPrimaryBtnText;
  protected
    property Race: Integer read GetRace write SetRace;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExportBtnText;
    procedure InitBtnText;
    procedure InitTiming;
    procedure DisposeTiming;
    procedure UpdateRaceCaption;
    procedure UpdateAuto;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.App.Translation,
  RiggVar.BO.Def,
  RiggVar.App.GuiInterface,
  RiggVar.App.GuiManager,
  RiggVar.BO.MsgToken,
  RiggVar.Col.Event;

procedure TKeyTab.InitTiming;
begin
  TimingConnection := BO.InputServer.Server.Connect('Timing.Keys');

  UpdateRaceCaption;
  edKey.Text := '';
  ResultLabel.Caption := '';
  NameLabel.Caption := '';
  SeriesLabel.Caption := '';
end;

constructor TKeyTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  InitBtnText;
end;

procedure TKeyTab.DisposeTiming;
begin
  TimingConnection.Free;
  TimingConnection := nil;
end;

function TKeyTab.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

procedure TKeyTab.SetRace(const Value: Integer);
begin
  if Value <> Main.GuiManager.Race then
  begin
    Main.GuiManager.Race := Value;
  end;
end;

procedure TKeyTab.RaceBtnClick(Sender: TObject);
begin
  UpdateRaceCaption;
end;

procedure TKeyTab.UpdateRaceCaption;
var
  fs: string;
  s: string;
begin
  RaceBtn.Caption := 'R' + IntToStr(Race);
  fs := 'Enter Bow number to create Finish message for boat in Race %d';
  s := Format(fs, [Race]);
  HelpLabel.Caption := s;
end;

procedure TKeyTab.RaceDownBtnClick(Sender: TObject);
begin
  Race := Race - 1;
end;

procedure TKeyTab.RaceUpBtnClick(Sender: TObject);
begin
  Race := Race + 1;
end;

procedure TKeyTab.AutoSendBtnClick(Sender: TObject);
begin
  Main.GuiManager.AutoUpdateEvent := AutoSendBtn.Down;
end;

procedure TKeyTab.ClearBtnClick(Sender: TObject);
begin
  BO.EventNode.EventRowCollection.ResetRace(Race);
  BO.Inform(ScheduleEventUpdate);
end;

procedure TKeyTab.SendBtnClick(Sender: TObject);
begin
  SendTimingMsg(MsgLabel.Caption);
end;

procedure TKeyTab.SendTimingMsg(s: string);
begin
  if Assigned(TimingConnection) then
  begin
    TimingConnection.HandleMsg(s);
    BO.Inform(ScheduleEventUpdate);
  end;
end;

procedure TKeyTab.SendTimingMsg2(s: string);
begin
  if (AutoSendBtn.Down) then
  begin
    MsgLabel.Caption := s;
    SendTimingMsg(s)
  end
  else
    MsgLabel.Caption := s;
end;

procedure TKeyTab.HandleTransponderEvent(Bib: Integer);
var
  s: string;
begin
  s := cTokenA + '.' + cTokenB + '.' + cTokenRace +
    IntToStr(Race) + '.Bib' + IntToStr(Bib) + '.RV' + ' = 500';
  SendTimingMsg2(s);
end;

procedure TKeyTab.edKeyKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then // #13 = Return
  begin
    key := #0;
    if cbWantMessageBeep.Checked then
      Main.GuiManager.PlaySound(3);
  end;
end;

procedure TKeyTab.edKeyKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Bib: Integer;
begin
  if Key = VK_Return then
  begin
    MsgLabel.Caption := '';

    { message label }
    Bib := StrToIntDef(edkey.Text, -1);
    edKey.Text := '';
    if (Bib > 0)
      //and (Bib <= BO.BOParams.StartlistCount)
    then
      HandleTransponderEvent(Bib);

    { error label }
    if Bib = -1 then
      MsgLabel.Caption := 'invalid input (must be a number)'
    else if Bib = 0 then
      MsgLabel.Caption := 'bow number must be > 0';
//    else if Bib > BO.BOParams.StartlistCount then
//      MsgLabel.Caption := 'bow number must be < ' + IntToStr(BO.BOParams.StartlistCount);

    ShowResult(Race, Bib);
  end;
end;

procedure TKeyTab.ShowResult(Race, Bib: Integer);
var
  cr: TEventRowCollectionitem;
  s: string;
  n: string;
  p: Integer;

  i: Integer;
  rc: Integer;
begin
  cr := BO.EventNode.FindBib(Bib);

  if Assigned(cr) then
  begin
    { Name }
    n := cr.DN;
    if n = '' then
      n := 'DN';
    NameLabel.Caption := n;

    { Position }
    p := cr.Race[Race].OTime;
    s := Format('Race %d | Bib %d = Pos %d', [Race, Bib, p]);
    ResultLabel.Caption := s;

    rc := BO.BOParams.RaceCount;
    s := '';
    for i := 1 to rc do
    begin
      if i > 1 then
      begin
        if (i - Race = 0) or (i - Race = 1) then
          s := s + ' - '
        else
          s := s + ' ; ';
      end;
      s := s + cr.Race[i].RaceValue;
    end;
    SeriesLabel.Caption := s;
  end
  else
  begin
    ResultLabel.Caption := '';
    NameLabel.Caption := '';
    SeriesLabel.Caption := '';
  end;
end;

procedure TKeyTab.InitBtnText;
begin
  if Main.Params.WantLocalText then
  begin
    RaceDownBtn.Hint := GetText(KT_RaceDownBtn_Hint);
    RaceBtn.Hint := GetText(KT_RaceBtn_Hint);
    RaceUpBtn.Hint := GetText(KT_RaceUpBtn_Hint);
    ClearBtn.Hint := GetText(KT_ClearBtn_Hint);
    SendBtn.Hint := GetText(KT_SendBtn_Hint);
    AutoSendBtn.Hint := GetText(KT_AutoSendBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TKeyTab.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TKeyTab.InitPrimaryBtnText;
begin
  RaceDownBtn.Hint := 'make previous race current, if any';
  RaceBtn.Hint := 'display for current (selected) race, click will update the display again';
  RaceUpBtn.Hint := 'make next race current, if any';
  ClearBtn.Hint := 'Attention: the button will clear out data all data for the current race';
  SendBtn.Hint := 'send the generated timing message (what you see lower text box)';
  AutoSendBtn.Hint := 'toggle button: send generated messages automatically if in down position';
end;

procedure TKeyTab.InitAlternativeBtnText;
begin
  RaceDownBtn.Hint := 'Vorangehendes Race selektieren, wenn noch nicht am Anfang.';
  RaceBtn.Hint := 'Anzeige für aktuelles Race, Klick aktualisiert nochmal.';
  RaceUpBtn.Hint := 'Nächstes Race selektieren, wenn noch nicht am Ende.';
  ClearBtn.Hint := 'Achtung: löscht die Daten für das ausgewählte Race!';
  SendBtn.Hint := 'Sende das generierte Telegramm, siehe untere Textbox.';
  AutoSendBtn.Hint := 'ToggleButton: Sende automatisch wenn gedrückt.';
end;

procedure TKeyTab.ExportBtnText;
begin
  InitDefaultBtnText;
  SetText(KT_RaceDownBtn_Hint, RaceDownBtn.Hint);
  SetText(KT_RaceBtn_Hint, RaceBtn.Hint);
  SetText(KT_RaceUpBtn_Hint, RaceUpBtn.Hint);
  SetText(KT_ClearBtn_Hint, ClearBtn.Hint);
  SetText(KT_SendBtn_Hint, SendBtn.Hint);
  SetText(KT_AutoSendBtn_Hint, AutoSendBtn.Hint);
end;

procedure TKeyTab.UpdateAuto;
begin
  AutoSendBtn.Down := Main.GuiManager.AutoUpdateEvent;
end;

end.
