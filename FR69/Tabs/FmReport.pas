unit FmReport;

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
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.BO.CacheMotor;

type
  TReportTab = class(TFrame)
    ToolBar: TToolBar;
    StatusBtn: TSpeedButton;
    DataBtn: TSpeedButton;
    XmlBtn: TSpeedButton;
    HtmlBtn: TSpeedButton;
    JsonBtn: TSpeedButton;
    IniBtn: TSpeedButton;
    HashBtn: TSpeedButton;
    TafelBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    SwapBtn: TSpeedButton;
    SemiBtn: TSpeedButton;
    TabBtn: TSpeedButton;
    TestMemoBtn: TSpeedButton;
    CheckBtn: TSpeedButton;
    TransBtn: TSpeedButton;
    NormalTextBtn: TSpeedButton;
    CompactTextBtn: TSpeedButton;
    StatusMemo: TMemo;
    TestMemo: TMemo;
    procedure StatusMemoKeyPress(Sender: TObject; var Key: Char);
    procedure StatusBtnClick(Sender: TObject);
    procedure IniBtnClick(Sender: TObject);
    procedure SourceBtnClick(Sender: TObject);
    procedure NormalTextBtnClick(Sender: TObject);
    procedure CompactTextBtnClick(Sender: TObject);
    procedure DataBtnClick(Sender: TObject);
    procedure XmlBtnClick(Sender: TObject);
    procedure HtmlBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);
    procedure HashBtnClick(Sender: TObject);
    procedure TafelBtnClick(Sender: TObject);
    procedure SemiBtnClick(Sender: TObject);
    procedure TabBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SwapBtnClick(Sender: TObject);
    procedure TestMemoBtnClick(Sender: TObject);
    procedure CheckBtnClick(Sender: TObject);
    procedure TransBtnClick(Sender: TObject);
    procedure ToolBarDblClick(Sender: TObject);
    procedure ToolBarStatusClick(Sender: TObject);
  private
    function GetRace: Integer;
    procedure TafelReport(SL: TStrings);
    procedure SwapSep(old, new: string);
    procedure EnsureSemicolon;
    procedure EnsureTab;
    procedure SendMessages;
    procedure ProcessBlock;
    procedure Import(SLD: TStrings);
    function GetBlockText: string;
    procedure ShowHelp;
    procedure ShowTranslation;
    procedure ToggleToolBarBtn;
    function GetTestMemoVisible: Boolean;
    procedure SetTestMemoVisible(const Value: Boolean);
  protected
    procedure DoIni;
    procedure DoStatus;
    procedure DoSource;
    procedure DoNormalText;
    procedure DoCompactText;
    procedure DoText;
    procedure DoXml;
    procedure DoJson;
    procedure DoHtml;
    procedure DoHash;
    procedure DoCheck;
    procedure DoFinishReport;
    procedure DoTafelReport;
    procedure DoTimeTable;
    procedure DoTrans;
    procedure DoHelp;
    procedure DoPrepareSwap;
    procedure DoShortcutTest;
    procedure DoSwap;
    function CurrentLine: string;
    procedure SaveTranslation;
  public
    constructor Create(AOwner: TComponent); override;
    property Race: Integer read GetRace;
    property TestMemoVisible: Boolean read GetTestMemoVisible write SetTestMemoVisible;
  end;

implementation

{$R *.dfm}

uses
  Winapi.Messages,
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.Col.Event,
  RiggVar.BO.Def,
  RiggVar.BO.Msg,
  RiggVar.BO.ExcelImport,
  RiggVar.Conn.Intern,
  RiggVar.BO.ResultHash;

const
  SelectAll_Hint = '^A - select all';
  BlockBtn_Hint = '^B - isolate block';
  ImportBtn_Hint = '^I - import block';
  ProcessBtn_Hint = '^P - process block';
  MsgBtn_Hint = '^N - send list of messages';
  SwapBtn_Hint = '^M - swap event data from status memo';
  SemiBtn_Hint = '^S - replace tab by semicolon';
  TabBtn_Hint = '^T - replace semicolon by tab';
  HelpBtn_Hint = '^U - show help';

constructor TReportTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
end;

procedure TReportTab.StatusBtnClick(Sender: TObject);
begin
  DoStatus;
end;

procedure TReportTab.CompactTextBtnClick(Sender: TObject);
begin
  DoCompactText;
end;

procedure TReportTab.DataBtnClick(Sender: TObject);
begin
  DoText;
end;

procedure TReportTab.XmlBtnClick(Sender: TObject);
begin
  DoXml;
end;

procedure TReportTab.HashBtnClick(Sender: TObject);
begin
  DoHash;
end;

procedure TReportTab.HtmlBtnClick(Sender: TObject);
begin
  DoHtml;
end;

procedure TReportTab.IniBtnClick(Sender: TObject);
begin
  DoIni;
end;

procedure TReportTab.JsonBtnClick(Sender: TObject);
begin
  DoJson;
end;

procedure TReportTab.SourceBtnClick(Sender: TObject);
begin
  DoSource;
end;

procedure TReportTab.SemiBtnClick(Sender: TObject);
begin
  TestMemo.Text := 'Swap Separator (EnsureSemicolon)';
  EnsureSemicolon;
end;

procedure TReportTab.EnsureSemicolon;
var
  tb: string;
  sc: string;
begin
  sc := ';';
  tb := #9;
  SwapSep(tb, sc);
end;

procedure TReportTab.TabBtnClick(Sender: TObject);
begin
  TestMemo.Text := 'Swap Separator (EnsureTab)';
  EnsureTab;
end;

procedure TReportTab.EnsureTab;
var
  tb: string;
  sc: string;
begin
  sc := ';';
  tb := #9;
  SwapSep(sc, tb);
end;

procedure TReportTab.SwapSep(old, new: string);
var
  i: Integer;
  s: string;
begin
  StatusMemo.Lines.BeginUpdate;
  try
    for i := 0 to StatusMemo.Lines.Count - 1 do
    begin
      s := StatusMemo.Lines[i];
      if Pos(old, s) > 0 then
      begin
        s := StringReplace(s, old, new, [rfReplaceAll, rfIgnoreCase]);
        StatusMemo.Lines[i] := s;
      end;
    end;
  finally
    StatusMemo.Lines.EndUpdate;
  end;
end;

function TReportTab.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

function TReportTab.GetTestMemoVisible: Boolean;
begin
  result := Self.TestMemo.Visible;
end;

procedure TReportTab.TafelBtnClick(Sender: TObject);
begin
  DoTafelReport;
end;

procedure TReportTab.TafelReport(SL: TStrings);
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  Series: string;

  SNR: Integer;
  Bib: Integer;
  DN: string;
  Points: string;
  Rank: Integer;

  i: Integer;
  r: Integer;

  NameLabel: string;
  IdentLabel: string;
  ResultLabel: string;
  SeriesLabel: string;
begin
  cl := BO.EventNode.EventRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];

    SNR := cr.SNR;
    Bib := cr.Bib;
    DN := cr.DN;
    Points := cr.GPoints;
    Rank := cr.GRank;

    Series := '';
    for r := 1 to BO.BOParams.RaceCount do
    begin
      if r > 1 then
        Series := Series + ' - ';
      Series := Series + cr.Race[r].RaceValue;
    end;

    IdentLabel  := Format('Bib  %3d | SNR %4d', [Bib, SNR]);
    ResultLabel := Format('Rank %3d | Total %s', [Rank, Points]);
    NameLabel   := DN;
    SeriesLabel := Series;

    SL.Add(IdentLabel);
    SL.Add(ResultLabel);
    if NameLabel <> '' then
      SL.Add(NameLabel);
    SL.Add(SeriesLabel);
    SL.Add('');
  end;
end;

procedure TReportTab.TestMemoBtnClick(Sender: TObject);
begin
  TestMemo.Visible := not TestMemo.Visible;
end;

procedure TReportTab.SendMessages;
var
  c: TConnection;
  s: string;
begin
  c := BO.InputServer.Server.Connect('StatusMemo.Import');
  s := 'RiggVar.Request.'#13#10 + StatusMemo.Text;
  c.InjectMsg(s);
  c.Free
end;

procedure TReportTab.SetTestMemoVisible(const Value: Boolean);
begin
  Self.TestMemo.Visible := Value;
end;

procedure TReportTab.NormalTextBtnClick(Sender: TObject);
begin
  DoNormalText;
end;

procedure TReportTab.ProcessBlock;
var
  m: TStringList;
  s: string;
begin
  s := StatusMemo.Text;
  if StatusMemo.SelLength > 0 then
  begin
    s := Copy(s, StatusMemo.SelStart+1, StatusMemo.SelLength);
  end;
  m := TStringList.Create;
  try
    BO.ExcelImporter.RunImportFilter(s, m);
    StatusMemo.Text := m.Text;
  finally
    m.Free;
  end;
end;

procedure TReportTab.Import(SLD: TStrings);
var
  i: Integer;
  s: string;
  msg: TBOMsg;
begin
  msg := TBOMsg.Create;
  try
    BO.EventBO.RelaxedInputMode := true;
    for i := 0 to SLD.Count - 1 do
    begin
      s := StatusMemo.Lines[i];
      msg.prot := s;
      if not msg.DispatchProt then
        Main.Logger.Info('MessageError: ' + s);
    end;
  finally
    BO.EventBO.RelaxedInputMode := false;
    msg.Free;
  end;
  BO.EventNode.Modified := True;
  BO.EventNode.Calc;
  BO.Inform(ScheduleEventUpdate);
end;

function TReportTab.GetBlockText: string;
var
  m, n: TStrings;
  s: string;
  l, l1, l2: Integer;
  i: Integer;
begin
  result := '';

  l := StatusMemo.Perform(EM_LINEFROMCHAR, StatusMemo.SelStart, 0);

  if (l > 0) and (l < StatusMemo.Lines.Count) then
  begin
    m := StatusMemo.Lines;

    l1 := -1;
    l2 := -1;
    for i := l downto 0 do
    begin
      s := m[i];
      if Pos('.Begin', s) > 0 then
      begin
        l1 := i;
        break;
      end;
    end;
    for i := l to m.Count-1 do
    begin
      s := m[i];
      if Pos('.End', s) > 0 then
      begin
        l2 := i;
        break;
      end;
    end;

    if (l1 > -1) and (l2 > -1) then
    begin
      n := TStringList.Create;
      for i := l1 to l2 do
        n.Add(m[i]);
      result := n.Text;
      n.Free;
    end;
  end;
end;

procedure TReportTab.StatusMemoKeyPress(Sender: TObject; var Key: Char);
var
  s: string;
begin
  if Key = ^A then
  begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
  if Key = ^B then
  begin
    s := GetBlockText;
    if s <> '' then
      StatusMemo.Text := s;
    Key := #0;
  end;
  if Key = ^I then
  begin
    Import(StatusMemo.Lines);
    Key := #0;
  end;
  if Key = ^J then
  begin
    DoShortcutTest;
    Key := #0;
  end;
  if Key = ^N then
  begin
    SendMessages;
    Key := #0;
  end;
  if Key = ^P then
  begin
    ProcessBlock;
    Key := #0;
  end;
  if Key = ^Q then //^M does not work
  begin
    DoSwap;
    Key := #0;
  end;
  if Key = ^S then
  begin
    EnsureSemicolon;
    Key := #0;
  end;
  if Key = ^T then
  begin
    EnsureTab;
    Key := #0;
  end;
  if Key = ^U then
  begin
    ShowHelp;
    Key := #0;
  end;
end;

function TReportTab.CurrentLine: string;
var
  i: Integer;
begin
  i := StatusMemo.Perform(EM_LINEFROMCHAR, StatusMemo.SelStart, 0);
  if (i >= 0) and (i < StatusMemo.Lines.Count) then
    result := StatusMemo.Lines[i];
end;

procedure TReportTab.HelpBtnClick(Sender: TObject);
begin
  DoHelp;
end;

procedure TReportTab.ShowHelp;
var
  SL: TStrings;
begin
  SL := StatusMemo.Lines;
  SL.Clear;
  SL.Add(SelectAll_Hint);
  SL.Add(SemiBtn_Hint);
  SL.Add(TabBtn_Hint);
  SL.Add(BlockBtn_Hint);
  SL.Add(ProcessBtn_Hint);
  SL.Add(MsgBtn_Hint);
  SL.Add(SwapBtn_Hint);
end;

procedure TReportTab.SwapBtnClick(Sender: TObject);
begin
  TestMemoVisible := True;
  DoPrepareSwap;
end;

procedure TReportTab.CheckBtnClick(Sender: TObject);
begin
  DoCheck;
end;

procedure TReportTab.TransBtnClick(Sender: TObject);
begin
  DoTrans;
end;

procedure TReportTab.DoTrans;
begin
  TestMemo.Text := 'Toggle and Show Translation';
  Main.Params.WantAlternativeText := not Main.Params.WantAlternativeText;
  BO.Inform(ExportTranslation);
  ShowTranslation;
end;

procedure TReportTab.ShowTranslation;
var
  SL: TStrings;
begin
  SL := StatusMemo.Lines;
  SL.BeginUpdate;
  SL.Clear;
  SL.Add('Template for ''fr01-translation.txt'' below the line:');
  SL.Add('place the file in either the Application-Directory');
  SL.Add('or into AppData\Local\RiggVar\FR\');
  SL.Add('---');
  Main.AppTranslator.GetTemplateStrings(SL);
  SL.EndUpdate;
end;

procedure TReportTab.SaveTranslation;
begin
  Main.Params.WantAlternativeText := False;
  BO.Inform(ExportTranslation);
  Main.AppTranslator.SaveTemplate;
end;

procedure TReportTab.ToolBarStatusClick(Sender: TObject);
begin
  ToggleToolBarBtn;
end;

procedure TReportTab.ToolBarDblClick(Sender: TObject);
begin
  ToggleToolBarBtn;
end;

procedure TReportTab.ToggleToolBarBtn;
var
  b: Boolean;
begin
  b := not SemiBtn.Visible;

  HashBtn.Visible := b;
  TafelBtn.Visible := b;
  HelpBtn.Visible := b;

  SwapBtn.Visible := b;
  IniBtn.Visible := b;
  CheckBtn.Visible := b;

  TransBtn.Visible := b;
  SemiBtn.Visible := b;
  TabBtn.Visible := b;
end;

procedure TReportTab.DoStatus;
begin
  TestMemo.Text := 'Main.GetStatusString';
  StatusMemo.Text := Main.GetStatusString;
end;

procedure TReportTab.DoIni;
begin
  TestMemo.Text := 'Main.IniImage.ToString';
  StatusMemo.Text := Main.IniImage.ToString;
end;

procedure TReportTab.DoSource;
begin
  TestMemo.Text := 'BO.ConvertedData';
  StatusMemo.Text := BO.ConvertedData;
  StatusMemo.Lines.Insert(0, BO.EventHash);
end;

procedure TReportTab.DoNormalText;
begin
  TestMemo.Text := 'Normal Text';
  StatusMemo.Text := BO.ToNormalTXT;
end;

procedure TReportTab.DoCompactText;
begin
  TestMemo.Text := 'Compact Text';
  StatusMemo.Text := BO.ToCompactTXT;
end;

procedure TReportTab.DoText;
var
  data: string;
begin
  TestMemo.Text := 'BO.ToText';
  data := BO.ToTXT;
  StatusMemo.Text := data;
  StatusMemo.Lines.Insert(0, BO.ResultHash.GetTextHash(data));
end;

procedure TReportTab.DoXml;
var
  data: string;
begin
  TestMemo.Text := 'BO.ToXML';
  data := BO.ToXML;
  StatusMemo.Text := data;
  StatusMemo.Lines.Insert(0, BO.ResultHash.GetTextHash(data));
end;

procedure TReportTab.DoJson;
begin
  TestMemo.Text := 'BO.ToJson';
  StatusMemo.Text := BO.ToJson;
end;

procedure TReportTab.DoHtml;
var
  s: string;
begin
  TestMemo.Text := 'BO.ToHtml';
  s := BO.ToHtml;
  StatusMemo.Text := s;
end;

procedure TReportTab.DoHash;
begin
  TestMemo.Text := 'BO.ResultHash.MemoString';
  StatusMemo.Text := BO.ResultHash.MemoString;
end;

procedure TReportTab.DoCheck;
begin
  TestMemo.Text := 'BO.CalcEV.Proxy.CheckList.Text';
  StatusMemo.Text := BO.CalcEV.Proxy.CheckList.Text;
end;

procedure TReportTab.DoFinishReport;
var
  s: string;
begin
  TestMemo.Text := 'Main.GuiManager.CacheMotor.FinishReport';
  s := Main.GuiManager.CacheMotor.FinishReport;
  StatusMemo.Text := s;
end;

procedure TReportTab.DoTafelReport;
begin
  TestMemo.Text := 'Tafel Report';
  StatusMemo.Clear;
  StatusMemo.Lines.BeginUpdate;
  try
    TafelReport(StatusMemo.Lines);
  finally
    StatusMemo.Lines.EndUpdate;
  end;
end;

procedure TReportTab.DoHelp;
begin
  TestMemo.Text := 'Help Text for using the Status Memo below';
  ShowHelp;
end;

procedure TReportTab.DoTimeTable;
begin
{$ifdef RoundingsOnly}
  TestMemo.Text := Format('BO.TimeTable[%d]', [BO.Race]);
  StatusMemo.Text := BO.TimeTable[BO.Race];
{$endif}
end;

procedure TReportTab.DoPrepareSwap;
var
  ML: TStrings;
begin
  TestMemo.Clear;
  ML := TestMemo.Lines;
  ML.Add('Preparing for swap of EventData:');
  ML.Add('  Make sure StatusMemo contains EventData text (normal or compact).');
  ML.Add('  Make sure StatusMemo has focus.');
  ML.Add('( Then press control q to begin the swap. )');
end;

procedure TReportTab.DoShortcutTest;
begin
  TestMemo.Text := 'Testing Shortcut: ok';
end;

procedure TReportTab.DoSwap;
var
  ed: string;
begin
  TestMemo.Text := 'Main.GuiManager.SwapEvent(StatusMemo.Text)';
  try
    ed := StatusMemo.Text;
    Main.GuiManager.SwapEvent(ed);
  except
    on e: Exception do
    begin
      TestMemo.Text := e.Message;
    end;
  end;
end;

end.
