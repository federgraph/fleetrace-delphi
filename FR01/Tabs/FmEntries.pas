unit FmEntries;

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
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  RiggVar.BO.Def,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.BO.MsgTree,
  RiggVar.Col.Stammdaten,
  RiggVar.DAL.Redirector;

type
  TEntriesTab = class(TFrame)
    ToolBar: TToolBar;
    LoadBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    AddBtn: TSpeedButton;
    UpdateBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    procedure AddBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
  public
    ColBO: TStammdatenBO;
    Node: TStammdatenNode;

    GB: TGridBlock;
    ColGrid: TColGrid;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitGrid;
    procedure DisposeGrid;
    procedure UpdateGrid;

    procedure DisableLocalAccess;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

constructor TEntriesTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Name := 'EntriesGrid';
  GB.Parent := self;
end;

destructor TEntriesTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TEntriesTab.InitGrid;
begin
  ColBO := BO.StammdatenBO;
  Node := BO.StammdatenNode;
  GB.Node := Node;
  GB.ColBO := ColBO;

  GB.BeginInitGrid;

  ColGrid := GB.ColGrid;
  ColGrid.AutoDelete := True;
  ColGrid.AutoInsert := True;
  ColGrid.ExcelStyle := True;

  GB.EndInitGrid;
end;

procedure TEntriesTab.DisposeGrid;
begin
  GB.DisposeGrid;
  ColBO := nil;
  Node := nil;
  ColGrid := nil;
end;

procedure TEntriesTab.DisableLocalAccess;
begin
  LoadBtn.Enabled := false;
  SaveBtn.Enabled := false;
end;

procedure TEntriesTab.LoadBtnClick(Sender: TObject);
var
  fn: string;
  SL: TStringList;
begin
  fn := Main.FolderInfo.TracePath + '_Athletes.txt';
  if DBFileExists(fn) then
  begin
    ColBO.CurrentRow := nil;
    SL := TDBStringList.Create;
    SL.LoadFromFile(fn);
    BO.LoadPartial(SL);
    SL.Free;
    ColGrid.ShowData;
  end;
end;

procedure TEntriesTab.SaveBtnClick(Sender: TObject);
var
  b: TBackup;
  fn: string;
begin
  fn := Main.FolderInfo.TracePath + '_Athletes.txt';
  b := TBackup.Create;
  b.OnBackup := BO.BackupAthletes;
  try
    b.Backup(fn);
  finally
    b.Free;
  end;
end;

procedure TEntriesTab.AddBtnClick(Sender: TObject);
var
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
begin
  ColGrid.AddRowCollectionItem;
  cl := BO.StammdatenNode.StammdatenRowCollection;
  cr := cl.Items[cl.Count-1];
  cr.SNR := 1001 + cr.Index;
  UpdateGrid;
end;

procedure TEntriesTab.UpdateBtnClick(Sender: TObject);
begin
  ColGrid.UpdateAll;
end;

procedure TEntriesTab.ClearBtnClick(Sender: TObject);
begin
  if Main.FormAdapter.ConfirmOperation('Clear all Athlete Data') then
  begin
    BO.StammdatenNode.StammdatenRowCollection.Clear;
    ColGrid.UpdateAll;
  end;
end;

procedure TEntriesTab.UpdateGrid;
begin
  GB.ColGrid.UpdateAll;
end;

end.
