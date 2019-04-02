unit FrmMsgParser2;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  RiggVar.BO.MsgParser2, Vcl.Buttons, Vcl.ToolWin, Vcl.ComCtrls;

type
  TFormMsgParser2 = class(TForm)
    Memo: TMemo;
    ListBox: TListBox;
    ToolBar: TToolBar;
    TestBtn: TSpeedButton;
    LiveBtn: TSpeedButton;
    SimpleBtn: TSpeedButton;
    JsonBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TestBtnClick(Sender: TObject);
    procedure LiveBtnClick(Sender: TObject);
    procedure SimpleBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);
  private
    MP: TMsgParser2;
    ML: TStrings;
    LI: TStrings;
    procedure InitList;
    procedure ReportGroups;
    procedure ReportVars;
    procedure Test(const s: string);
  end;

var
  FormMsgParser2: TFormMsgParser2;

procedure ShowMsgParser2;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Util.Classes;

procedure ShowMsgParser2;
begin
  if not Assigned(FormMsgParser2) then
    FormMsgParser2 := TFormMsgParser2.Create(Application);

  FormMsgParser2.Show;
end;

procedure TFormMsgParser2.FormCreate(Sender: TObject);
begin
  ClientHeight := 800;
  Main.ForceToolbarProps(ToolBar);

  Memo.Clear;

  Listbox.Align := alLeft;
  Memo.Align := alClient;

  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 10;
  Memo.ScrollBars := System.UITypes.TScrollStyle.ssBoth;

  Listbox.Font.Name := 'Courier New';
  Listbox.Font.Size := 10;

  MP := TMsgParser2.Create;
  ML := Memo.Lines;
  LI := Listbox.Items;

  InitList;
end;

procedure TFormMsgParser2.FormDestroy(Sender: TObject);
begin
  MP.Free;
end;

procedure TFormMsgParser2.ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  s: string;
begin
  if (Key = VK_Space) or (Key = VK_Return) then
  begin
    i := Listbox.ItemIndex;
    if (i > -1) and (i < LI.Count) then
    begin
      s := LI[i];
      Test(s);
    end;
  end;
end;

procedure TFormMsgParser2.Test(const s: string);
var
  b: Boolean;
begin
  b := MP.ParseLine(s);

  ML.Clear;

  ML.Add('result := ' + BoolStr[b]);

  if not MP.ErrorMsg.IsEmpty then
  begin
    ML.Add('---ErrorMsg---');
    ML.Add(MP.ErrorMsg);
    ML.Add('');
  end;

  ML.Add('---Groups---');
  ReportGroups;

  ML.Add('');
  ML.Add('---Vars---');
  ReportVars;

  ML.Add('');
  ML.Add('---GenMsg---');
  ML.Add(MP.GenMsg);

//  ML.Add('');
//  ML.Add('---GenAll---');
//  MP.GenAll(ML);
end;

procedure TFormMsgParser2.ReportGroups;
begin
  ML.Add('gc = ' + IntToStr(MP.gc));
  ML.Add('g1 = ' + MP.g1);
  ML.Add('g2 = ' + MP.g2);
  ML.Add('g3 = ' + MP.g3);
  ML.Add('g4 = ' + MP.g4);
end;

procedure TFormMsgParser2.ReportVars;
begin
  ML.Add('RaceCount = ' + IntToStr(MP.RaceCount));
  ML.Add('ITCount = ' + IntToStr(MP.ITCount));
  ML.Add('StartlistCount = ' + IntToStr(MP.StartlistCount));
  ML.Add('');
  ML.Add('EventName = ' + MP.EventName);
  ML.Add('InputMode = ' + BoolStr[MP.InputMode]);
  ML.Add('');
  ML.Add('Race = ' + MP.sRace);
  ML.Add('IT = ' + MP.sIT);
  ML.Add('Bib = ' + MP.sBib);
  ML.Add('');
  ML.Add('Time = ' + MP.sTime);
  ML.Add('Rank = ' + MP.sRank);
  ML.Add('QU = ' + MP.sQU);
  ML.Add('');
  ML.Add('Value = ' + MP.sValue);
end;

procedure TFormMsgParser2.InitList;
begin
  MP.InitTestData(LI);
end;

procedure TFormMsgParser2.TestBtnClick(Sender: TObject);
begin
  InitList;
end;

procedure TFormMsgParser2.LiveBtnClick(Sender: TObject);
begin
  LI.Text := BO.ToNormalTXT;
end;

procedure TFormMsgParser2.SimpleBtnClick(Sender: TObject);
begin
  LI.Text := BO.ToSimpleTXT;
end;

procedure TFormMsgParser2.JsonBtnClick(Sender: TObject);
begin
  ML.BeginUpdate;
  BO.BackupToSimpleJson(ML);
  ML.Text := TUtils.PrettyFormat(ML.Text);
  ML.EndUpdate;
end;

end.
