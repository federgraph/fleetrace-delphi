unit FrmUndoManager;

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
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  RiggVar.BO.Def,
  RiggVar.BO.UndoManager;

type
  TFormUndoManager = class(TForm)
    ToolBar: TToolBar;
    BaseBtn: TSpeedButton;
    CurrentBtn: TSpeedButton;
    LogBtn: TSpeedButton;
    UndoBtn: TSpeedButton;
    RedoBtn: TSpeedButton;
    Memo: TMemo;
    UndoRedoBtn: TSpeedButton;
    ProxyXMLBtn: TSpeedButton;
    CloseBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BaseBtnClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
    procedure CurrentBtnClick(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure UndoRedoBtnClick(Sender: TObject);
    procedure ProxyXMLBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    UndoManager: TUndoManager;
  end;

var
  FormUndoManager: TFormUndoManager;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormUndoManager.FormCreate(Sender: TObject);
begin
  Main.ForceToolbarProps(ToolBar);
  Memo.Align := alClient;
  if Main.IniImage.UseDoubleBuffer then
  begin
    ToolBar.DoubleBuffered := True;
    Memo.DoubleBuffered := True;
  end;
end;

procedure TFormUndoManager.BaseBtnClick(Sender: TObject);
begin
  Memo.Text := UndoManager.GetBase;
end;

procedure TFormUndoManager.LogBtnClick(Sender: TObject);
begin
  Memo.Text := UndoManager.GetLog;
end;

procedure TFormUndoManager.CurrentBtnClick(Sender: TObject);
begin
  //Memo.Text := UndoManager.GetCurrent;
  Memo.Clear;
  Memo.Lines.BeginUpdate;
  try
    BO.BackupToText(Memo.Lines);
  finally
    Memo.Lines.EndUpdate;
  end;
end;

procedure TFormUndoManager.UndoBtnClick(Sender: TObject);
begin
  Memo.Text := UndoManager.GetUndo;
end;

procedure TFormUndoManager.RedoBtnClick(Sender: TObject);
begin
  Memo.Text := UndoManager.GetRedo;
end;

procedure TFormUndoManager.UndoRedoBtnClick(Sender: TObject);
begin
  Memo.Text := UndoManager.GetUndoRedo;
end;

procedure TFormUndoManager.ProxyXMLBtnClick(Sender: TObject);
begin
  Memo.Lines.Assign(UndoManager.ProxyXML);
end;

procedure TFormUndoManager.CloseBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
