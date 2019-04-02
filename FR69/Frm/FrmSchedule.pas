unit FrmSchedule;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ToolWin, ComCtrls, ExtCtrls, StdCtrls,
  RiggVar.Col.Schedule,
  RiggVar.Out.JS00;

type
  TFormSchedule = class(TForm)
    ToolBar: TToolBar;
    OKBtn: TSpeedButton;
    Memo: TMemo;
    Splitter: TSplitter;
    PreviewBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    GridPanel: TPanel;
    CloseBtn: TButton;
    procedure OKBtnClick(Sender: TObject);
    procedure PreviewBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    Model: IJavaScoreXMLWriter;
    ScheduleGrid: TScheduleGrid;
    procedure LoadModel(aModel: IJavaScoreXMLWriter);
    procedure Unload;
    procedure OnSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
  end;

function EditSchedule(Model: IJavaScoreXMLWriter): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

var
  FormSchedule: TFormSchedule;

function EditSchedule(Model: IJavaScoreXMLWriter): Boolean;
begin
  result := False;

  if not Assigned(FormSchedule) then
    FormSchedule := TFormSchedule.Create(Application);

  FormSchedule.LoadModel(Model);

  if FormSchedule.ShowModal = mrOK then
  begin
    //FormSchedule.SaveModel(Model);
    result := True;
  end;
  FormSchedule.Unload;

  //FormSchedule := nil;
end;

procedure TFormSchedule.FormCreate(Sender: TObject);
begin
  Main.ForceToolbarProps(ToolBar);
  Memo.Align := alClient;
  if Main.IniImage.UseDoubleBuffer then
  begin
    Memo.DoubleBuffered := True;
    ToolBar.DoubleBuffered := True;
  end;
end;

procedure TFormSchedule.LoadModel(aModel: IJavaScoreXMLWriter);
begin
  Model := aModel;
  ScheduleGrid := TScheduleGrid.Create(GridPanel);
  ScheduleGrid.Node.Init(Model.DivisionInfo.StartInfoList);
  ScheduleGrid.GB.ColGrid.UpdateAll;
  ScheduleGrid.GB.ColGrid.OnCellSelect := OnSelectCell;
end;

procedure TFormSchedule.Unload;
begin
  FormSchedule.ScheduleGrid.Free;
  FormSchedule.ScheduleGrid := nil;
  Memo.Clear;
end;

procedure TFormSchedule.OnSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if ScheduleGrid.Node.ScheduleRowCollection.Count > 2 then
  begin
    ScheduleGrid.ColBO.CurrentRow := ScheduleGrid.GB.ColGrid.GetRowCollectionItem(ARow);
    ScheduleGrid.GB.ColGrid.ShowData;
  end;
end;

procedure TFormSchedule.PreviewBtnClick(Sender: TObject);
begin
  Model.GetXML(Memo.Lines);
end;

procedure TFormSchedule.SaveBtnClick(Sender: TObject);
var
  dn: string;
  fn: string;
begin
  dn := IncludeTrailingPathDelimiter(Model.Dir);
  fn := Model.DivisionInfo.EventName;

  if DirectoryExists(dn) then
  begin
    OpenDialog.InitialDir := dn;
    OpenDialog.FileName := fn;
    OpenDialog.Filter := 'Regatta files|*.regatta';
    OpenDialog.FilterIndex := 0;
  end
  else
  begin
    fn := Main.FolderInfo.TracePath + '_' + fn + '.regatta';
    dn := ExtractFilePath(fn);
    fn := ExtractFileName(fn);
    OpenDialog.InitialDir := dn;
    OpenDialog.FileName := fn;
    OpenDialog.Filter := 'Regatta files|*.regatta';
    OpenDialog.FilterIndex := 0;
  end;

  if OpenDialog.Execute then
  begin
    fn := OpenDialog.FileName;
    fn := ChangeFileExt(fn, Model.Extension);
    Memo.Lines.SaveToFile(fn);
  end;
end;

procedure TFormSchedule.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormSchedule.CloseBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
