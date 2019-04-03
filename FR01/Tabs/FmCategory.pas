unit FmCategory;

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
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Col.CategoryCache,
  RiggVar.EM.CategoryCache,
  RiggVar.BO.CacheMotor;

type
  TCategoryTab = class(TFrame)
    ToolBar: TToolBar;
    UpdateBtn: TSpeedButton;
    BackupBtn: TSpeedButton;
    HtmlFragmentBtn: TSpeedButton;
    XmlBtn: TSpeedButton;
    HtmlDocBtn: TSpeedButton;
    TextBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    BatchBtn: TSpeedButton;
    ExportBtn: TSpeedButton;
    procedure UpdateBtnClick(Sender: TObject);
    procedure BackupBtnClick(Sender: TObject);
    procedure HtmlFragmentBtnClick(Sender: TObject);
    procedure HtmlDocBtnClick(Sender: TObject);
    procedure XmlBtnClick(Sender: TObject);
    procedure TextBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure BatchBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
  private
    function GetCache: TCategoryCache;
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
    property Cache: TCategoryCache read GetCache;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Translation,
  RiggVar.App.GuiInterface,
  RiggVar.App.Main;

constructor TCategoryTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'CacheGrid';
  InitBtnText;
end;

destructor TCategoryTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TCategoryTab.InitGrid;
begin
  Assert(GB.ColGrid = nil);
  GB.ColBO := Cache.ColBO;
  GB.Node := Cache.Node;
  GB.BeginInitGrid;
  GB.ColGrid.ColorSchema := colorMoneyGreen;
  GB.EndInitGrid;
  Cache.Enabled := True;
end;

procedure TCategoryTab.DisposeGrid;
begin
  GB.DisposeGrid;
end;

function TCategoryTab.GetCache: TCategoryCache;
begin
  result := Main.CategoryCache;
end;

procedure TCategoryTab.UpdateBtnClick(Sender: TObject);
begin
  GB.ColGrid.ShowData;
end;

procedure TCategoryTab.BackupBtnClick(Sender: TObject);
begin
  Cache.Backup;
end;

procedure TCategoryTab.HtmlFragmentBtnClick(Sender: TObject);
begin
  Cache.CacheMode := cmHtmlFragment;
end;

procedure TCategoryTab.HtmlDocBtnClick(Sender: TObject);
begin
  Cache.CacheMode := cmHtmlDoc;
end;

procedure TCategoryTab.XmlBtnClick(Sender: TObject);
begin
  Cache.CacheMode := cmXml;
end;

procedure TCategoryTab.TextBtnClick(Sender: TObject);
begin
  Cache.CacheMode := cmTxt;
end;

procedure TCategoryTab.SaveBtnClick(Sender: TObject);
begin
  Cache.Save;
end;

procedure TCategoryTab.BatchBtnClick(Sender: TObject);
begin
  Main.GuiManager.GuiInterface.HandleInform(StartBatch);
end;

procedure TCategoryTab.ExportBtnClick(Sender: TObject);
begin
  Main.Params.WantEntryList := False;
  Main.GuiManager.GuiInterface.HandleInform(StartBatch);
  Main.Params.WantEntryList := True;
end;

procedure TCategoryTab.InitBtnText;
begin
  if Main.Params.WantLocalText then
  begin
    HtmlFragmentBtn.Hint := GetText(CA_HtmlFragmentBtn_Hint);
    HtmlDocBtn.Hint := GetText(CA_HtmlDocBtn_Hint);
    XmlBtn.Hint := GetText(CA_XmlBtn_Hint);
    TextBtn.Hint := GetText(CA_TextBtn_Hint);

    BackupBtn.Hint := GetText(CA_BackupBtn_Hint);
    SaveBtn.Hint := GetText(CA_SaveBtn_Hint);
    BatchBtn.Hint := GetText(CA_BatchBtn_Hint);
    ExportBtn.Hint := GetText(CA_ExportBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TCategoryTab.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TCategoryTab.InitPrimaryBtnText;
begin
  HtmlFragmentBtn.Hint := 'cache html fragments, good for use wiht Ajax';
  HtmlDocBtn.Hint := 'wrap html fragments into valid html documents';
  XmlBtn.Hint := 'cache xml files in FR-XML format';
  TextBtn.Hint := 'cache text output in compact text format';

  BackupBtn.Hint := 'write content of cache to ED*.html files';
  SaveBtn.Hint := 'same as Backup, but uses file names based on EP.EventName';
  BatchBtn.Hint := 'update cache for every event in the workspace, then save';
  ExportBtn.Hint := 'same as Batch, but clear out entry names';
end;

procedure TCategoryTab.InitAlternativeBtnText;
begin
  HtmlFragmentBtn.Hint := 'merke Html-Fragmente, für Verwendung mit Ajax';
  HtmlDocBtn.Hint := 'bindet Html-Fragmente zusätzlich in gültiges Html-Dokument ein';
  XmlBtn.Hint := 'merke Xml-Ausgabe im FR-XML Format';
  TextBtn.Hint := 'merke Text-Ausgabe im kompakten Format';

  BackupBtn.Hint := 'schreibe gemerkten Inhalt in ED*.html Dateien';
  SaveBtn.Hint := 'wie Backup, aber mit Dateinamen auf Grundlage von EP.EventName';
  BatchBtn.Hint := 'aktualisiere Cache für jeden Event im Workspace und speichere Alles';
  ExportBtn.Hint := 'wie Batch, aber ohne Stammdaten-Namen';
end;

procedure TCategoryTab.ExportBtnText;
begin
  InitDefaultBtnText;

  SetText(CA_HtmlFragmentBtn_Hint, HtmlFragmentBtn.Hint);
  SetText(CA_HtmlDocBtn_Hint, HtmlDocBtn.Hint);
  SetText(CA_XmlBtn_Hint, XmlBtn.Hint);
  SetText(CA_TextBtn_Hint, TextBtn.Hint);

  SetText(CA_BackupBtn_Hint, BackupBtn.Hint);
  SetText(CA_SaveBtn_Hint, SaveBtn.Hint);
  SetText(CA_BatchBtn_Hint, BatchBtn.Hint);
  SetText(CA_ExportBtn_Hint, ExportBtn.Hint);
end;

end.
