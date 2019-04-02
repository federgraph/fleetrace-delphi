unit FrmTest;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  System.UITypes,
  RiggVar.App.TestDef, Vcl.ExtCtrls;

type
  TFormTest = class(TForm)
    ListBox: TListBox;
    Memo: TMemo;
    ToolBar: TToolBar;
    AllBtn: TSpeedButton;
    ReportBtn: TSpeedButton;
    TestBtn: TSpeedButton;
    DataBtn: TSpeedButton;
    DefaultBtn: TSpeedButton;
    StatusBtn: TSpeedButton;
    InitBtn: TSpeedButton;
    Panel: TPanel;
    TestMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AllBtnClick(Sender: TObject);
    procedure ReportBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure DataBtnClick(Sender: TObject);
    procedure DefaultBtnClick(Sender: TObject);
    procedure StatusBtnClick(Sender: TObject);
    procedure InitBtnClick(Sender: TObject);
  private
    TestDef: TTestDef;
    TestList: TTestList;
  end;

var
  FormTest: TFormTest;

procedure ShowTest;

implementation

{$R *.dfm}

procedure ShowTest;
begin
  if FormTest = nil then
  begin
    FormTest := TFormTest.Create(Application);
  end;
  FormTest.Show;
end;

{ TFormTest }

procedure TFormTest.FormCreate(Sender: TObject);
begin
  Memo.Clear;

  Listbox.Align := alLeft;
  Panel.Align := alClient;
  TestMemo.Align := alTop;
  Memo.Align := alClient;

  TestMemo.Height := 100;
  TestMemo.Font.Name := 'Courier New';
  TestMemo.Font.Size := 10;
  TestMemo.ScrollBars := System.UITypes.TScrollStyle.ssBoth;

  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 10;
  Memo.ScrollBars := System.UITypes.TScrollStyle.ssBoth;

  TestDef := TTestDef.Create;
  TestDef.ML := Memo.Lines;
  TestDef.TL := TestMemo.Lines;

  TestList := TTestList.Create;
  TestList.TestDef := TestDef;
  TestList.ML := Memo.Lines;
  TestList.TL := TestMemo.Lines;
  TestList.LI := Listbox.Items;

  FormTest := self;
  FormTest.StatusBtnClick(nil);
end;

procedure TFormTest.FormDestroy(Sender: TObject);
begin
  TestList.Free;
  TestDef.Free;
end;

procedure TFormTest.InitBtnClick(Sender: TObject);
begin
  TestList.InitIni;
end;

procedure TFormTest.TestBtnClick(Sender: TObject);
begin
  TestList.InitTest;
end;

procedure TFormTest.AllBtnClick(Sender: TObject);
begin
  TestList.InitAll;
end;

procedure TFormTest.DataBtnClick(Sender: TObject);
begin
  TestList.InitData;
end;

procedure TFormTest.DefaultBtnClick(Sender: TObject);
begin
  TestList.InitDefault;
end;

procedure TFormTest.ReportBtnClick(Sender: TObject);
begin
  TestList.InitReport;
end;

procedure TFormTest.StatusBtnClick(Sender: TObject);
begin
  TestList.InitStatus;
end;

procedure TFormTest.ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  h: Integer;
  op: TestOp;
begin
  if Key = VK_Space then
  begin
    i := Listbox.ItemIndex;
    h := Integer(High(TestOp));
    if (i >= 0) and (i <= h) and (i < TestList.OpCount) then
    begin
      op := TestList.OpList[i];
      TestList.DoTest(op);
    end;
  end
  else if Key = VK_Return then
  begin
    i := Listbox.ItemIndex;
    h := Integer(High(TestOp));
    if (i >= 0) and (i <= h) and (i < TestList.OpCount) then
    begin
      op := TestList.OpList[i];
      TestList.DoOp(op);
    end;
  end;
end;

//procedure TFormTest.AddOp(op: TestOp);
//var
//  s: string;
//begin
//  s := TestDef.GetItemCaption(op);
//  Inc(OpCount);
//  if (OpCount > 0) and (OpCount < Length(OpList)) then
//  begin
//    OpList[OpCount-1] := op;
//    Listbox.Items.Add(s);
//  end;
//end;
//
//procedure TFormTest.ClearList;
//begin
//  Memo.Clear;
//  Listbox.Clear;
//  OpCount := 0;
//end;
//
//procedure TFormTest.InitAll;
//var
//  op: TestOp;
//begin
//  ClearList;
//  for op := Low(TestOp) to High(TestOp) do
//    AddOp(op);
//end;
//
//procedure TFormTest.DoOp(op: TestOp);
//begin
//  TestDef.DoOp(op);
//  Memo.Text := TestDef.ML.Text;
//end;

end.
