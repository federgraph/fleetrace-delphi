unit FrmUnicodeScanner;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
  Vcl.Grids,
  RiggVar.Util.UnicodeScanner;

type
  TFormUnicodeScanner = class(TForm)
    PageControl: TPageControl;
    tsGrid: TTabSheet;
    tsListView: TTabSheet;
    StringGrid: TStringGrid;
    GridSplitter: TSplitter;
    Memo: TMemo;
    ListView: TListView;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ListBtn: TToolButton;
    ShowBtn: TToolButton;
    ListAllBtn: TButton;
    ListFRBtn: TButton;
    ListGroupedBtn: TButton;
    SaveBtn: TToolButton;
    RemovePreambleBtn: TToolButton;
    ShowDefaultBtn: TToolButton;
    ShowAsciiBtn: TToolButton;
    FillBtn: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
    procedure ListAllBtnClick(Sender: TObject);
    procedure ListFRBtnClick(Sender: TObject);
    procedure ListGroupedBtnClick(Sender: TObject);
    procedure StringGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SaveBtnClick(Sender: TObject);
    procedure RemovePreambleBtnClick(Sender: TObject);
    procedure ShowDefaultBtnClick(Sender: TObject);
    procedure ShowAsciiBtnClick(Sender: TObject);
    procedure FillBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    CurrentFileName: string;
    US: TUnicodeScanner;
    SL: TStringList;
    procedure ShowDetail(Encoding: TEncoding);
    procedure FillGrid;
    procedure SaveDetail;
  public
    { Public-Deklarationen }
  end;

var
  FormUnicodeScanner: TFormUnicodeScanner;

function IsScannerEnabled: Boolean;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.DAL.WorkspaceIntf;

function IsScannerEnabled: Boolean;
var
  wt: Integer;
begin
  result := false;
  wt := Main.StoreAdapter.GetWorkspaceType;
  case wt of
    WorkspaceType_SharedFS: result := true;
  end;
end;

procedure TFormUnicodeScanner.FormCreate(Sender: TObject);
begin
  Main.ForceToolbarProps(ToolBar);
  //ReportMemoryLeaksOnShutdown := True;

  PageControl.Align := alClient;
  Memo.Align := alClient;
  US := TUnicodeScanner.Create;
  US.LV := ListView;
  SL := TStringList.Create;
  StringGrid.Cells[0,0] := 'Size';
  StringGrid.Cells[1,0] := 'Name';

  SaveBtn.Enabled := False;
  //FillGrid;
  PageControl.ActivePageIndex := 0;
end;

procedure TFormUnicodeScanner.FormDestroy(Sender: TObject);
begin
  US.Free;
  SL.Free;
end;

procedure TFormUnicodeScanner.ListBtnClick(Sender: TObject);
var
  s: string;
  r: Integer;
begin
  SL.Clear;
  US.ListNames(SL);
  StringGrid.RowCount := SL.Count + 1;
  r := 1;
  for s in SL do
  begin
    StringGrid.Cells[0, r] := s;
    StringGrid.Cells[1, r] := '';
    Inc(r);
  end;
end;

procedure TFormUnicodeScanner.RemovePreambleBtnClick(Sender: TObject);
var
  s: string;
begin
  if Memo.Lines.Count > 0 then
  begin
    s := Memo.Lines[0];
    s := US.RemovePreamble(s);
    Memo.Lines[0] := s;
  end;
end;

procedure TFormUnicodeScanner.StringGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ShowDetail(nil);
end;

procedure TFormUnicodeScanner.SaveBtnClick(Sender: TObject);
begin
  SaveDetail;
end;

procedure TFormUnicodeScanner.ShowDefaultBtnClick(Sender: TObject);
begin
  ShowDetail(TEncoding.Default);
end;

procedure TFormUnicodeScanner.ShowAsciiBtnClick(Sender: TObject);
begin
  ShowDetail(TEncoding.ASCII);
end;

procedure TFormUnicodeScanner.ShowBtnClick(Sender: TObject);
begin
  ShowDetail(nil);
end;

procedure TFormUnicodeScanner.ShowDetail(Encoding: TEncoding);
var
  fn: string;
  ext: string;
begin
  CurrentFileName := '';
  SaveBtn.Enabled := false;
  if StringGrid.Row > -1 then
  begin
    fn := StringGrid.Rows[StringGrid.Row][0];
    if fn <> '' then
    begin
      ext := ExtractFileExt(fn);
      if US.IsDetailFile(ext) then
      begin
        if Encoding = nil then
          US.ShowFile(fn, Memo.Lines)
        else
          US.ShowDetail(fn, Memo.Lines, Encoding);
        CurrentFileName := fn;
        SaveBtn.Enabled := true;
      end
      else
      begin
        Memo.Text := Format('%s: IsDetailFile(''%s'') = false', [TimeToStr(Now), ext]);
      end;
    end
    else
    begin
      Memo.Text := 'fn: ' + fn;
    end;
  end;
end;

procedure TFormUnicodeScanner.SaveDetail;
begin
  if CurrentFileName <> '' then
  begin
    US.SaveFile(CurrentFileName, Memo.Lines);
  end;
end;

procedure TFormUnicodeScanner.FillBtnClick(Sender: TObject);
begin
  FillGrid;
end;

procedure TFormUnicodeScanner.FillGrid;
var
  r: Integer;
  i: Integer;
begin
  SL.Clear;
  US.FillGrid(SL);
  if SL.Count > 0 then
  begin
    StringGrid.RowCount := SL.Count + 1;
    r := 1;
    for i := 0 to SL.Count-1 do
    begin
      StringGrid.Cells[0, r] := SL[i];
      StringGrid.Cells[1, r] := IntToStr(Integer(SL.Objects[i]));
      Inc(r);
    end;
  end;
end;

procedure TFormUnicodeScanner.ListAllBtnClick(Sender: TObject);
begin
  US.ListAll;
end;

procedure TFormUnicodeScanner.ListFRBtnClick(Sender: TObject);
begin
  US.ListFR;
end;

procedure TFormUnicodeScanner.ListGroupedBtnClick(Sender: TObject);
begin
  US.ListGrouped;
end;

end.
