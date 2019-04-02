unit FmMark;

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
  Vcl.ExtCtrls,
  Vcl.Buttons,
  RiggVar.Conn.Intern,
  RiggVar.RM.TimingButtons;

type
  TMarkTab = class(TFrame)
    LBA: TListBox;
    LBB: TListBox;
    TimingLog: TEdit;
    TimePointLabel: TLabel;
    LabelSpace: TLabel;
    SendBtn: TSpeedButton;
    AutoBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    OptionGroup: TRadioGroup;
    procedure LBAKeyPress(Sender: TObject; var Key: Char);
    procedure LBBKeyPress(Sender: TObject; var Key: Char);
    procedure SendBtnClick(Sender: TObject);
    procedure AutoBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    FAuto: Boolean;
    FRace: Integer;
    FIT: Integer;
    TimingConnection: TConnection;
    IndexA: Integer;
    IndexB: Integer;
    TimingButtonHelper: TTimingButtonHelper;
    procedure InitOptions(ML: TStrings);
    procedure MoveItem(A, B: TListBox; Idx: Integer);
    procedure SendRaceTime(bib: Integer);
    procedure SendRacePosition(bib: Integer);
    procedure ClearRaceTime(bib: Integer);
    procedure ClearRacePosition(bib: Integer);
    procedure SendTimingMsg(s: string);
    function GetIT: Integer;
    function GetRace: Integer;
    procedure InitEntries;
    procedure Clear(bib: Integer);
    procedure Send(bib: Integer);
    function GetOption: Integer;
    procedure SetOption(const Value: Integer);
  protected
    Mode: Integer;
    procedure Send1(bib: Integer);
    procedure Send2(bib: Integer);
    procedure Clear1(bib: Integer);
    procedure Clear2(bib: Integer);
    procedure InitEntries1;
    procedure InitEntries2;
    procedure InitEntries3;
    procedure UpdateEventFinishPositions1;
    procedure UpdateEventFinishPositions2;
    property IT: Integer read GetIT;
    property Race: Integer read GetRace;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitTiming;
    procedure DisposeTiming;
    procedure InitTimePoint;
    procedure UpdateAuto;
    property Option: Integer read GetOption write SetOption;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.App.GuiManager,
  RiggVar.BO.Def,
  RiggVar.BO.MsgToken,
  RiggVar.Col.Event,
  RiggVar.Col.Race;

const
  OptionTime = 0;
  OptionFinish = 0;
  OptionDNS = 1;
  OptionDNF = 2;
  OptionDSQ = 3;
  OptionOK = 4;
  OptionErase = 5;

{ TMarkTab }

constructor TMarkTab.Create(AOwner: TComponent);
begin
  inherited;
  Mode := 3;

  LabelSpace.Caption := 'SpaceBar!';
  LabelSpace.Hint := 'use space-key to move the selected item between Listbox A and B.';

  InitOptions(OptionGroup.Items);
  TimingButtonHelper := TTimingButtonHelper.Create;

  if Mode = 3 then
  begin
    TimingLog.Visible := False;
    AutoBtn.Visible := False;
    SendBtn.Visible := False;
    ClearBtn.Visible := False;
  end;
end;

destructor TMarkTab.Destroy;
begin
  TimingButtonHelper.Free;
  inherited;
end;

procedure TMarkTab.InitOptions(ML: TStrings);
begin
  ML.Clear;
  ML.Add('time/finish');
  ML.Add('dns');
  ML.Add('dnf');
  ML.Add('dsq');
//  ML.Add('ok');
//  ML.Add('erase');
end;

procedure TMarkTab.InitTiming;
begin
  TimingConnection := BO.InputServer.Server.Connect('MarkTiming.Input');
end;

procedure TMarkTab.DisposeTiming;
begin
  TimingConnection.Free;
  TimingConnection := nil;
end;

procedure TMarkTab.InitTimePoint;
begin
  FRace := Race;
  FIT := IT;
  LBA.Clear;
  LBB.Clear;
  InitEntries;
  IndexA := 0;
  IndexB := 0;
  TimePointLabel.Caption := Format('Race %d - IT %d', [FRace, FIT]);
  TimingLog.Text := 'Timing Message';
end;

procedure TMarkTab.LBAKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ' ' then
    MoveItem(LBA, LBB, LBA.ItemIndex);
end;

procedure TMarkTab.LBBKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ' ' then
    MoveItem(LBB, LBA, LBB.ItemIndex);
end;

procedure TMarkTab.MoveItem(A: TListBox; B: TListBox; Idx: Integer);
var
  s: string;
  bib: Integer;
begin
  if (Idx >= 0) and (Idx < A.Count) then
  begin
    s := A.Items[Idx];
    B.Items.Add(s);
    A.Items.Delete(Idx);
    s := Copy(s, 5);
    bib := StrToIntDef(s, -1);
    if bib > -1 then
    begin
      if A = LBA then
        Send(bib)
      else
        Clear(bib);
    end;
    IndexA := LBA.ItemIndex;
  end;
end;

procedure TMarkTab.Send(bib: Integer);
begin
  case Mode of
    1: Send1(bib);
    2: Send1(bib);
    3: Send2(bib);
  end;
end;

procedure TMarkTab.Clear(bib: Integer);
begin
  case Mode of
    1: Clear1(bib);
    2: Clear2(bib);
    3: Clear2(bib);
  end;
end;

procedure TMarkTab.Send1(bib: Integer);
begin
  SendRaceTime(bib);
  if FAuto then
    SendRacePosition(bib)
end;

procedure TMarkTab.Clear1(bib: Integer);
begin
  ClearRaceTime(bib);
  if FAuto then
    ClearRacePosition(bib)
end;

procedure TMarkTab.SendRaceTime(bib: Integer);
var
  s: string;
  bibValue: string;
  t: string;
begin
  t := FormatDateTime('hh:mm:ss.zz', Now);
  bibValue := IntToStr(bib);
  s := cTokenA + '.' + cTokenB + '.' + cTokenRace +
    IntToStr(FRace) + '.Bib' + bibValue + '.IT' + IntToStr(FIT) + ' = ' + t;
  SendTimingMsg(s);
end;

procedure TMarkTab.SendRacePosition(Bib: Integer);
var
  s: string;
begin
  s := cTokenA + '.' + cTokenB + '.' + cTokenRace +
    IntToStr(Race) + '.Bib' + IntToStr(Bib) + '.RV' + ' = 500';
  SendTimingMsg(s);
end;

procedure TMarkTab.ClearRaceTime(bib: Integer);
var
  s: string;
  bibValue: string;
  t: string;
begin
  t := '-1';
  bibValue := IntToStr(bib);
  s := cTokenA + '.' + cTokenB + '.' + cTokenRace +
    IntToStr(FRace) + '.Bib' + bibValue + '.IT' + IntToStr(FIT) + ' = ' + t;
  SendTimingMsg(s);
end;

procedure TMarkTab.ClearRacePosition(Bib: Integer);
var
  s: string;
begin
  s := cTokenA + '.' + cTokenB + '.' + cTokenRace +
    IntToStr(Race) + '.Bib' + IntToStr(Bib) + '.RV' + ' = 0';
  SendTimingMsg(s);
end;

procedure TMarkTab.SendTimingMsg(s: string);
begin
  if Assigned(TimingConnection) then
  begin
    TimingLog.Text := '';
    TimingConnection.HandleMsg(s);
    TimingLog.Text := s;
    BO.Inform(TimingDataChanged);
  end
  else
    TimingLog.Text := '';
end;

procedure TMarkTab.SetOption(const Value: Integer);
begin
  if (Value >= 0) and (Value < OptionGroup.Items.Count) then
    OptionGroup.ItemIndex := Value;
end;

function TMarkTab.GetIT: Integer;
begin
  result := Main.GuiManager.IT;
end;

function TMarkTab.GetOption: Integer;
begin
  result := OptionGroup.ItemIndex;
  if result = -1 then
    result := 0;
end;

function TMarkTab.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

procedure TMarkTab.InitEntries;
begin
  case Mode of
    1: InitEntries1;
    2: InitEntries2;
    3: InitEntries3;
  end;
end;

procedure TMarkTab.InitEntries1;
var
  i: Integer;
  s: string;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  cl := BO.EventNode.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    s := IntToStr(cr.Bib);
    LBA.Items.Add('Bib ' + s);
  end;
end;

procedure TMarkTab.InitEntries2;
var
  i: Integer;
  s: string;
  cl: TRaceRowCollection;
  cr1: TRaceRowCollectionItem;
  cr2: TRaceRowCollectionItem;
  tp: TTimePoint;
  plz: Integer;
begin
  cl := BO.RaceNode.RaceRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr1 := cl.Items[i];
    s := 'Bib ' + IntToStr(cr1.Bib);
    tp := cr1.IT[FIT];
    if not tp.OTime.TimePresent then
      LBA.Items.Add(s)
  end;
  for i := 0 to cl.Count-1 do
  begin
    cr1 := cl.Items[i];
    plz := cr1.IT[FIT].PLZ;
    cr2 := cl.Items[plz];
    if cr2 <> nil then
    begin
      s := 'Bib ' + IntToStr(cr2.Bib);
      tp := cr2.IT[FIT];
      if tp.OTime.TimePresent then
        LBB.Items.Add(s)
    end;
  end;
end;

procedure TMarkTab.InitEntries3;
var
  i: Integer;
  ML: TStrings;
begin
  TimingButtonHelper.Update;

  ML := TimingButtonHelper.Bibs;
  for i := 0 to ML.Count-1 do
    LBA.Items.Add('Bow ' + ML[i]);

  ML := TimingButtonHelper.Bows;
  for i := 0 to ML.Count-1 do
    LBB.Items.Add('Bow ' + ML[i])
end;

procedure TMarkTab.ClearBtnClick(Sender: TObject);
begin
  //BO.ClearTimepointCommand(BO.Race, BO.IT);

  BO.RaceNode.RaceRowCollection.ResetIT(IT);
  InitTimePoint;
  BO.Inform(TimingDataChanged);
end;

procedure TMarkTab.AutoBtnClick(Sender: TObject);
begin
  FAuto := AutoBtn.Down;
  Main.GuiManager.AutoUpdateEvent := FAuto;
end;

procedure TMarkTab.SendBtnClick(Sender: TObject);
begin
  UpdateEventFinishPositions1;
end;

procedure TMarkTab.UpdateAuto;
begin
  FAuto := Main.GuiManager.AutoUpdateEvent;
  AutoBtn.Down := FAuto;
end;

procedure TMarkTab.UpdateEventFinishPositions1;
var
  cle: TEventRowCollection;
  cre: TEventRowCollectionItem;
  clr: TRaceRowCollection;
  crr: TRaceRowCollectionItem;
  i: Integer;
  r: Integer;
begin
  r := Race;
  cle := BO.EventNode.EventRowCollection;
  clr := BO.RaceNode.RaceRowCollection;
  for i := 0 to cle.Count - 1 do
  begin
    cre := cle.Items[i];
    crr := clr.Items[i];
    cre.Race[r].OTime := crr.IT[IT].ORank;
  end;
  cle.BaseNode.Modified := True;
end;

procedure TMarkTab.UpdateEventFinishPositions2;
var
  s: string;
  bib: Integer;
  i: Integer;
  cr: TEventRowCollectionItem;
begin
  for i := 0 to LBB.Count-1 do
  begin
    s := LBB.Items[i];
    s := Copy(s, 5);
    bib := StrToIntDef(s, -1);
    if bib > -1 then
    begin
      cr := BO.EventNode.FindBib(bib);
      if cr <> nil then
      begin
        cr.Race[Race].OTime := i+1;
      end;
    end;
  end;
  BO.EventNode.EventRowCollection.BaseNode.Modified := True;
end;

procedure TMarkTab.Send2(bib: Integer);
begin
  BO.DoTimingEvent(BO.Race, BO.IT, bib, Option);
  Option := OptionTime;
  BO.Inform(TimepointChanged);
end;

procedure TMarkTab.Clear2(bib: Integer);
begin
  BO.DoTimingEvent(BO.Race, BO.IT, bib, OptionOK);
  BO.DoTimingEvent(BO.Race, BO.IT, bib, OptionErase);
  BO.Inform(TimepointChanged);
end;

end.
