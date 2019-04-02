unit FrmWorkspaceFiles;

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
  System.Classes,
  System.SysUtils,
  Data.DB,
  Data.Win.ADODB,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.DBCtrls,
  Vcl.DBGrids,
  Vcl.Grids,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  RiggVar.DAL.WorkspaceIntf;

type
  TFormWorkspaceFiles = class(TForm)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    ToolBar: TToolBar;
    DBNavigator: TDBNavigator;
    ClearBtn: TSpeedButton;
    RequeryBtn: TSpeedButton;
    ShowBtn: TSpeedButton;
    Memo: TMemo;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClearBtnClick(Sender: TObject);
    procedure RequeryBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
  private
    function GetDBWorkspace: IDBWorkspace;
    property DBWorkspace: IDBWorkspace read GetDBWorkspace;
  public
  end;

var
  FormWorkspaceFiles: TFormWorkspaceFiles;

implementation

{$R *.DFM}

uses
  RiggVar.App.Main;

procedure TFormWorkspaceFiles.FormCreate(Sender: TObject);
begin
  Main.ForceToolbarProps(ToolBar);
  Memo.Align := alClient;
  DataSource.DataSet := DBWorkspace.GetDataSet;
end;

procedure TFormWorkspaceFiles.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormWorkspaceFiles.FormDestroy(Sender: TObject);
begin
  DataSource.DataSet := nil;
  FormWorkspaceFiles := nil;
end;

function TFormWorkspaceFiles.GetDBWorkspace: IDBWorkspace;
begin
  result := Main.StoreAdapter.GetDBWorkspace;
end;

procedure TFormWorkspaceFiles.ClearBtnClick(Sender: TObject);
begin
  DBWorkspace.DBDeleteWorkspace;
  if Assigned(DataSource.DataSet) then
    DataSource.DataSet.Refresh;
end;

procedure TFormWorkspaceFiles.RequeryBtnClick(Sender: TObject);
var
  qa: TADOQuery;
begin
  if DataSource.DataSet is TADOQuery then
  begin
    qa := DBWorkspace.GetDataSet as TADOQuery;
    qa.Requery;
  end
  else if Assigned(DataSource.DataSet) then
  begin
    DataSource.DataSet.Refresh;
  end;
end;

procedure TFormWorkspaceFiles.ShowBtnClick(Sender: TObject);
var
  fn: string;
begin
  if Assigned(DataSource.DataSet) then
  try
    fn := DataSource.DataSet.FieldByName('ItemKey').AsString;
    if fn <> '' then
    begin
      DBWorkspace.DBLoadFromFile(fn, Memo.Lines);
      StatusBar.SimpleText := fn;
    end
    else
      Memo.Lines.Text := 'ItemKey is empty';
  except
    on e: Exception do
      Memo.Lines.Text := e.Message;
  end;
end;

end.
