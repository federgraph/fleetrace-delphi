unit FrmExcelImport;

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
  Vcl.ExtCtrls,
  RiggVar.BO.ExcelImport;

type
  TFormExcelImport = class(TForm)
    ToolBar: TToolBar;
    PasteBtn: TSpeedButton;
    ConvertBtn: TSpeedButton;
    Memo: TMemo;
    TestdataBtn: TSpeedButton;
    ShowTabsBtn: TSpeedButton;
    DelimiterCombo: TComboBox;
    edRecordLineCount: TEdit;
    ShuffleBtn: TSpeedButton;
    SendBtn: TSpeedButton;
    ListCombo: TComboBox;
    ResultMemo: TMemo;
    Splitter: TSplitter;
    SepBtn: TToolButton;
    CloseBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
    procedure TestdataBtnClick(Sender: TObject);
    procedure ShowTabsBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShuffleBtnClick(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    ExcelImporter: TExcelImporter;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormExcelImport: TFormExcelImport;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Conn.Intern;

{$R *.dfm}

procedure TFormExcelImport.FormCreate(Sender: TObject);
begin
  Main.ForceToolbarProps(ToolBar);
  ResultMemo.Align := alClient;

  if Main.IniImage.UseDoubleBuffer then
  begin
    ToolBar.DoubleBuffered := True;
    Memo.DoubleBuffered := True;
    ResultMemo.DoubleBuffered := True;
  end;

  ExcelImporter := TExcelImporter.Create;

  DelimiterCombo.Clear;
  DelimiterCombo.Items.Add('Tab');
  DelimiterCombo.Items.Add(';');
  DelimiterCombo.Items.Add(',');
  DelimiterCombo.Items.Add('Space');
  DelimiterCombo.ItemIndex := 1;

  ListCombo.Clear;
  ListCombo.Items.Add('NameList');
  ListCombo.Items.Add('StartList');
  ListCombo.Items.Add('FleetList');
  ListCombo.Items.Add('TimeList');
  ListCombo.Items.Add('FinishList');
  ListCombo.Items.Add('ResultList');
  ListCombo.ItemIndex := 1;
end;

procedure TFormExcelImport.FormDestroy(Sender: TObject);
begin
  ExcelImporter.Free;
end;

procedure TFormExcelImport.PasteBtnClick(Sender: TObject);
begin
  Memo.SelectAll;
  Memo.PasteFromClipboard;
end;

procedure TFormExcelImport.TestdataBtnClick(Sender: TObject);
begin
  ExcelImporter.GetTestData(Memo.Lines);
end;

procedure TFormExcelImport.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFormExcelImport.ConvertBtnClick(Sender: TObject);
var
  c: char;
  s: string;
  TID: Integer;
begin
  if DelimiterCombo.ItemIndex > -1  then
  begin
    c := ExcelImporter.Delimiter;
    s := DelimiterCombo.Items[DelimiterCombo.ItemIndex];
    if s = 'Tab' then
      c := #9
    else if s = ';' then
      c := ';'
    else if s = ',' then
      c := ','
    else if s = 'Space' then
      c := ' ';
    ExcelImporter.Delimiter := c;
  end;

  TID := TableID_ResultList;
  if ListCombo.ItemIndex > -1  then
  begin
    s := ListCombo.Text;
    if Pos('Name', s) > 0 then
      TID := TableID_NameList
    else if Pos('Finish', s) > 0 then
      TID := TableID_FinishList
    else if Pos('Start', s) > 0 then
      TID := TableID_StartList
    else if Pos('Result', s) > 0 then
      TID := TableID_ResultList
    else if Pos('Time', s) > 0 then
      TID := TableID_TimeList
    else if Pos('Fleet', s) > 0 then
      TID := TableID_FleetList
  end;

  ResultMemo.Text := ExcelImporter.ConvertString(Memo.Text, TID);
end;

procedure TFormExcelImport.ShowTabsBtnClick(Sender: TObject);
begin
  ExcelImporter.ShowTabs(Memo.Lines);
end;

procedure TFormExcelImport.ShuffleBtnClick(Sender: TObject);
var
  i: Integer;
  s: string;
  rlc: Integer;
  SL: TStringList;
  c: Integer;
begin
  rlc := StrToIntDef(edRecordLineCount.Text, 1);
  if rlc > 1 then
  begin
    SL := TStringList.Create;

    for i := 0 to Memo.Lines.Count - 1 do
    begin
      c := i mod rlc;
      if c = 0 then
        s := Memo.Lines[i]
      else
        s := s + ';' + Memo.Lines[i];

      if c = rlc - 1 then
        SL.Add(s);
    end;

    Memo.Lines.Text := SL.Text;
    SL.Free;
    edRecordLineCount.Text := '1';
  end;
end;

procedure TFormExcelImport.SendBtnClick(Sender: TObject);
var
  c: TConnection;
  s: string;
begin
  c := BO.InputServer.Server.Connect('ExcelImport.Input.LocalVar');
  s := 'RiggVar.Request.'#13#10 + ResultMemo.Text;
  c.InjectMsg(s);
  c.Free;
end;

end.
