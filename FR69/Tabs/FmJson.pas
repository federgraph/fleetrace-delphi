unit FmJson;

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
  System.IniFiles,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.Col.CategoryCache,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Web4.JsonCache;

type
  TJsonTab = class(TFrame)
    ToolBar: TToolBar;
    UpdateBtn: TSpeedButton;
    BackupBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    ExportBtn: TSpeedButton;
    procedure UpdateBtnClick(Sender: TObject);
    procedure BackupBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure BatchBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
  private
    function GetCache: TJsonCache;
    procedure InitDefaultBtnText;
    procedure InitAlternativeBtnText;
    procedure InitPrimaryBtnText;
  public
    GB: TGridBlock;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExportBtnText;
    procedure InitBtnText;
    procedure InitGrid;
    procedure DisposeGrid;
    property Cache: TJsonCache read GetCache;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Translation,
  RiggVar.App.GuiInterface,
  RiggVar.App.Main;

constructor TJsonTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'JsonGrid';
  InitBtnText;
end;

destructor TJsonTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TJsonTab.InitGrid;
begin
  Assert(GB.ColGrid = nil);
  GB.ColBO := Cache.ColBO;
  GB.Node := Cache.Node;
  GB.BeginInitGrid;
  GB.ColGrid.ColorSchema := colorMoneyGreen;
  GB.EndInitGrid;
end;

procedure TJsonTab.DisposeGrid;
begin
  GB.DisposeGrid;
end;

function TJsonTab.GetCache: TJsonCache;
begin
  result := Main.JsonCache;
end;

procedure TJsonTab.UpdateBtnClick(Sender: TObject);
begin
  GB.ColGrid.ShowData;
end;

procedure TJsonTab.BackupBtnClick(Sender: TObject);
begin
  Cache.Backup;
end;

procedure TJsonTab.SaveBtnClick(Sender: TObject);
begin
  Cache.Save;
end;

procedure TJsonTab.BatchBtnClick(Sender: TObject);
begin
  Main.GuiManager.GuiInterface.HandleInform(StartBatch);
end;

procedure TJsonTab.ExportBtnClick(Sender: TObject);
begin
  Main.Params.WantEntryList := False;
  Main.GuiManager.GuiInterface.HandleInform(StartBatch);
  Main.Params.WantEntryList := True;
end;

procedure TJsonTab.InitBtnText;
begin
  if Main.Params.WantLocalText then
  begin
    BackupBtn.Hint := GetText(CA_BackupBtn_Hint);
    SaveBtn.Hint := GetText(CA_SaveBtn_Hint);
    ExportBtn.Hint := GetText(CA_ExportBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TJsonTab.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TJsonTab.InitPrimaryBtnText;
begin
  BackupBtn.Hint := 'write content of cache to ED*.html files';
  SaveBtn.Hint := 'same as Backup, but uses file names based on EP.EventName';
  ExportBtn.Hint := 'same as Batch, but clear out entry names';
end;

procedure TJsonTab.InitAlternativeBtnText;
begin
  BackupBtn.Hint := 'schreibe gemerkten Inhalt in ED*.html Dateien';
  SaveBtn.Hint := 'wie Backup, aber mit Dateinamen auf Grundlage von EP.EventName';
  ExportBtn.Hint := 'wie Batch, aber ohne Stammdaten-Namen';
end;

procedure TJsonTab.ExportBtnText;
begin
  InitDefaultBtnText;
  SetText(CA_BackupBtn_Hint, BackupBtn.Hint);
  SetText(CA_SaveBtn_Hint, SaveBtn.Hint);
  SetText(CA_ExportBtn_Hint, ExportBtn.Hint);
end;

end.
