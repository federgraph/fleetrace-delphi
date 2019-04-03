unit FrmMain;

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
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Actions,
  System.DateUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  Vcl.AppEvnts,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Themes,
  RiggVar.App.Config,
  RiggVar.App.GuiInterface,
  RiggVar.App.GuiManager,
  RiggVar.BO.Def,
  RiggVar.BO.ExcelImport,
  RiggVar.BO.ExcelExport,
  RiggVar.BO.Params,
  RiggVar.Col.Event,
  RiggVar.Col.Race,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.DAL.Manager,
  RiggVar.Grid.Color,
  RiggVar.Grid.Control,
  RiggVar.Grid.ColGrid,
  FmCategory,
  FmEntries,
  FmEvent,
  FmKeying,
  FmMenu,
  FmMobil,
  FmRacing,
  FmReport,
  FmWorkspace,
  Vcl.Grids;

const
  wm_UpdateGrid = wm_User + 300;

type
  TFormFR62 = class(TForm, IGuiInterface)
    ApplicationEvents: TApplicationEvents;
    IdleTimer: TTimer;
    NorthContainer: TPageControl;
    PageControl: TPageControl;
    SouthContainer: TPageControl;
    ToolBar: TToolBar;
    ClearBtn: TSpeedButton;
    NorthBtn: TSpeedButton;
    SouthBtn: TSpeedButton;
    InfoBtn: TSpeedButton;
    StyleBtn: TSpeedButton;
    RowHeightBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    LanguageBtn: TSpeedButton;
    PlusBtn: TSpeedButton;
    MinusBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure IdleTimerTimer(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure NorthBtnClick(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure SouthBtnClick(Sender: TObject);
    procedure StyleBtnClick(Sender: TObject);
    procedure RowHeightBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure LanguageBtnClick(Sender: TObject);
    procedure PlusBtnClick(Sender: TObject);
    procedure MinusBtnClick(Sender: TObject);
    procedure PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FTestMemo: TStrings;
    FStatusMemo: TStrings;
    DraggedTabSheet: TTabsheet;
    CurrentStyleIndex: Integer;
    InfoClicks: Integer;

    function GetDocManager: TDocManager;
    function GetGuiManager: TGuiManager;
    procedure InitViews;
    procedure DisposeViews;
    procedure UpdateCaption;
    procedure UpdateWorkspaceStatus;

    procedure WMUpdateGrid(var Msg: TMessage); message wm_UpdateGrid;

    procedure DoOnIdle;
    procedure HandleInform(Action: TGuiAction);
    procedure SetupDoubleBufferControls;

    procedure ToggleEventMenu;
    procedure ToggleTimingPanel;

    function GetStatusMemo: TStrings;
    function GetTestMemo: TStrings;

    procedure InitPages;
    procedure DisposePages;

    procedure InitStatic;
    procedure InitForm;
    function InitTabSheet(pc: TPageControl; ts: TTabSheet; ACaption: string): TTabSheet;

    procedure ExportBtnText;
    procedure InitDefaultBtnText;
    procedure InitAlternativeBtnText;
    procedure InitPrimaryBtnText;
    procedure InitBtnText;

    property GM: TGuiManager read GetGuiManager;
    property DM: TDocManager read GetDocManager;
  protected
    WP: TWantPages;

    WantProviderMenu: Boolean;
    WantPublishMenu: Boolean;

    tsMenu: TTabSheet;
    tsEntries: TTabSheet;
    tsEvent: TTabSheet;
    tsReport: TTabSheet;
    tsMobil: TTabSheet;
    tsCategory: TTabSheet;
    tsRacing: TTabSheet;
    tsKeying: TTabSheet;
    tsWorkspace: TTabSheet;

    { Tabsheet-Want-Guards }
    FWantBrowser: Boolean;
    FWantCache: Boolean;
    FWantEntries: Boolean;
    FWantRace: Boolean;
    FWantTiming: Boolean;
    FWantRacing: Boolean;
    FWantKeying: Boolean;
    FWantEvent: Boolean;
    FWantWorkspace: Boolean;
    FWantReport: Boolean;
    FWantMenu: Boolean;
    FWantCategory: Boolean;
    FWantMobil: Boolean;
    FWantProfile: Boolean;
    FWantWeb: Boolean;
    FWantListing: Boolean;
    FWantRoundings: Boolean;

    { Test that access to all of these Tabs is properly guarded by a WantXX }
    TabMenu: TMenuTab;
    TabEntries: TEntriesTab;
    TabEvent: TEventTab;
    TabReport: TReportTab;
    TabRacing: TRacingTab;
    TabKeying: TKeyTab;
    TabWorkspace: TWorkspaceTab;
    TabCategory: TCategoryTab;
    TabMobil: TMobilTab;

    { protected to avoid warning H2219, unused method}
    procedure RecreatePages;
    procedure LoadWP;
    procedure UnloadWP;
    procedure InitWant;
    procedure InitWantHard;
    procedure UpdateWant;
  public
    procedure ShowMemo;

    property TestMemo: TStrings read GetTestMemo;
    property StatusMemo: TStrings read GetStatusMemo;

    property WantEntries: Boolean read FWantEntries;
    property WantRace: Boolean read FWantRace;
    property WantEvent: Boolean read FWantEvent;
    property WantReport: Boolean read FWantReport;
    property WantTiming: Boolean read FWantTiming;
    property WantRacing: Boolean read FWantRacing;
    property WantKeying: Boolean read FWantKeying;
    property WantWorkspace: Boolean read FWantWorkspace;
    property WantMenu: Boolean read FWantMenu;
    property WantCategory: Boolean read FWantCategory;
    property WantMobil: Boolean read FWantMobil;
    property WantCache: Boolean read FWantCache;
    property WantBrowser: Boolean read FWantBrowser;
    property WantProfile: Boolean read FWantProfile;
    property WantRoundings: Boolean read FWantRoundings;
    property WantListing: Boolean read FWantListing;
    property WantCourse: Boolean read FWantRoundings;
    property WantMark: Boolean read FWantRoundings;
    property WantWeb: Boolean read FWantWeb;
  end;

var
  FormFR62: TFormFR62;

implementation

uses
  RiggVar.App.Main,
  RiggVar.App.Translation,
  RiggVar.Util.InfoMemo;

{$R *.dfm}

{ TFormFR62 }

procedure TFormFR62.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  GM.GuiInterface := self;

  CurrentStyleIndex := 1;

  Width := 1000;
  Height := 700;

  NorthContainer.Align := alTop;
  NorthContainer.Height := 200;

  SouthContainer.Align := alBottom;
  SouthContainer.Height := 240;

  PageControl.Align := alClient;

  InitStatic;
  InitWant;
  InitPages;

  SetupDoubleBufferControls;

  ClearBtn.Enabled := Main.HaveAppPermission;
  StyleBtn.Enabled := Main.HaveAppPermission;
  RowHeightBtn.Enabled := Main.HaveAppPermission;
end;

procedure TFormFR62.RecreatePages;
begin
  if WP <> nil then
  begin
    DisposePages;
    UnloadWP;
    InitPages;
  end;
end;

procedure TFormFR62.InitPages;
begin
  NorthContainer.Visible := True;
  SouthContainer.Visible := True;

  InitForm;

  if WantMenu then
  begin
    TabMenu.TestMemo := TestMemo;
    TabMenu.Memo := StatusMemo;
  end;
  Main.IniImage.NorthContainerVisible := WantMenu;

  if WantMenu then
    NorthContainer.ActivePage := tsMenu;

  if WantEvent then
    PageControl.ActivePage := tsEvent
  else if PageControl.PageCount > 0 then
    PageControl.ActivePage := PageControl.Pages[0];

  if WantRacing then
    SouthContainer.ActivePage := tsRacing
  else if SouthContainer.PageCount > 0 then
   SouthContainer.ActivePage := SouthContainer.Pages[0];

  Main.IniImage.SouthContainerVisible := false;
  SouthContainer.Visible := Main.IniImage.SouthContainerVisible;

  InitBtnText;
  ExportBtnText;
  Main.AppTranslator.Load;
end;

procedure TFormFR62.LoadWP;
begin
  WP.WantBrowser := FWantBrowser;
  WP.WantCache := FWantCache;
  WP.WantRace := FWantRace;
  WP.WantTiming := FWantTiming;
  WP.WantEntries := FWantEntries;
  WP.WantEvent := FWantEvent;
  WP.WantReport := FWantReport;
  WP.WantRacing := FWantRacing;
  WP.WantKeying := FWantKeying;
  WP.WantWorkspace := FWantWorkspace;
  WP.WantMenu := FWantMenu;
  WP.WantCategory := FWantCategory;
  WP.WantMobil := FWantMobil;
  WP.WantWeb := FWantWeb;
  WP.WantListing := FWantListing;
  WP.WantRoundings := FWantRoundings;
end;

procedure TFormFR62.UnloadWP;
begin
  FWantBrowser := WP.WantBrowser;
  FWantCache := WP.WantCache;
  FWantRace := WP.WantRace;
  FWantTiming := WP.WantTiming;
  FWantEntries := WP.WantEntries;
  FWantEvent := WP.WantEvent;
  FWantReport := WP.WantReport;
  FWantRacing := WP.WantRacing;
  FWantKeying := WP.WantKeying;
  FWantWorkspace := WP.WantWorkspace;
  FWantMenu := WP.WantMenu;
  FWantCategory := WP.WantCategory;
  FWantMobil := WP.WantMobil;
  FWantWeb := WP.WantWeb;
  FWantListing := WP.WantListing;
  FWantRoundings := WP.WantRoundings;
end;

procedure TFormFR62.InitStatic;
begin
  TInfoMemo.WantLogo := True;
  TDisplayGrid.WantSmallRows := true;
  TDisplayGrid.WantCustomDrawCell := true;

  WantProviderMenu := False;
  WantPublishMenu := False;
end;

procedure TFormFR62.InitWantHard;
begin
  { Tabs-NorthContainer }
  FWantMenu := True;

  { Tabs-PageControl }
  FWantEntries := True;
  FWantRace := False;
  FWantEvent := True;
  FWantReport := True;
  FWantMobil := False;
  FWantCache := False;
  FWantCategory := False;
  FWantBrowser := False;
  FWantWeb := False;

  { Tabs-SourthContainer }
  FWantTiming := True;
  FWantRacing := True;
  FWantKeying := True;
  FWantWorkspace := True;

  FWantListing := False;
  FWantRoundings := False;
end;

procedure TFormFR62.InitWant;
var
  ii: TIniImage;
begin
  { Tabs }
  ii := Main.IniImage;

  { Tabs-NorthContainer }
  FWantMenu := ii.WantMenu;

  { Tabs-PageControl }
  FWantEntries := ii.WantEntries;
  FWantRace := ii.WantRace;
  FWantEvent := ii.WantEvent;
  FWantReport := ii.WantReport;
  FWantMobil := Main.HaveAppPermission and ii.WantMobil;
  FWantCache := ii.WantCache;
  FWantCategory := Main.HaveAppPermission and ii.WantCategory;
  FWantBrowser := ii.WantBrowser;
  FWantWeb := ii.WantWeb;

  { Tabs-SourthContainer }
  FWantTiming := ii.WantTiming;
  FWantRacing := ii.WantRacing;
  FWantKeying := ii.WantKeying;
  FWantWorkspace := ii.WantWorkspace;

  FWantListing := ii.WantListing;
  FWantRoundings := ii.WantRoundings;
end;

procedure TFormFR62.UpdateWant;
var
  ii: TIniImage;
begin
  ii := Main.IniImage;

  ii.WantMenu := FWantMenu;

  ii.WantEntries := FWantEntries;
  ii.WantRace := FWantRace;
  ii.WantEvent := FWantEvent;
  ii.WantReport := FWantReport;
  ii.WantMobil := FWantMobil;
  ii.WantCache := FWantCache;
  ii.WantCategory := FWantCategory;
  ii.WantBrowser := FWantBrowser;
  ii.WantWeb := FWantWeb;

  ii.WantTiming := FWantTiming;
  ii.WantRacing := FWantRacing;
  ii.WantKeying := FWantKeying;
  ii.WantWorkspace := FWantWorkspace;

  ii.WantListing := FWantListing;
  ii.WantRoundings := FWantRoundings;
end;

procedure TFormFR62.InitForm;
begin
  { Entries }
  if WantEntries then
  begin
    TabEntries := TEntriesTab.Create(self);
    tsEntries := InitTabSheet(PageControl, tsEntries, 'Entries');
    TabEntries.Parent := tsEntries;
    TabEntries.InitGrid;
  end
  else
    FreeAndNil(tsEntries);

  { Event }
  if WantEvent then
  begin
    TabEvent := TEventTab.Create(self);
    tsEvent := InitTabSheet(PageControl, tsEvent, 'Event');
    TabEvent.Parent := tsEvent;
    TabEvent.InitGrid;
  end
  else
    FreeAndNil(tsEvent);

  { Report }
  if WantReport then
  begin
    TabReport := TReportTab.Create(self);
    tsReport := InitTabSheet(PageControl, tsReport, 'Report');
    TabReport.Parent := tsReport;
  end
  else
    FreeAndNil(tsReport);

  { Menu }
  if WantMenu then
  begin
    TabMenu := TMenuTab.Create(self);
    tsMenu := InitTabSheet(NorthContainer, tsMenu, 'Menu');
    TabMenu.Parent := tsMenu;
    TabMenu.Align := alClient;
    TabMenu.DebugMode := false;
  end
  else
    FreeAndNil(tsMenu);

  { Category }
  if WantCategory then
  begin
    TabCategory := TCategoryTab.Create(self);
    tsCategory := InitTabSheet(PageControl, tsCategory, 'Category');
    TabCategory.Parent := tsCategory;
    TabCategory.Align := alClient;
    TabCategory.InitGrid;
  end
  else
    FreeAndNil(tsCategory);

  { Racing }
  if WantRacing then
  begin
    TabRacing := TRacingTab.Create(self);
    tsRacing := InitTabSheet(SouthContainer, tsRacing, 'Racing');
    TabRacing.Parent := tsRacing;
    TabRacing.InitTiming;
  end
  else
    FreeAndNil(tsRacing);

  { Keying }
  if WantKeying then
  begin
    TabKeying := TKeyTab.Create(self);
    tsKeying := InitTabSheet(SouthContainer, tsKeying, 'Keying');
    TabKeying.Parent := tsKeying;
    TabKeying.InitTiming;
  end
  else
    FreeAndNil(tsKeying);

  { Workspace }
  if WantWorkspace then
  begin
    TabWorkspace := TWorkspaceTab.Create(self);
    tsWorkspace := InitTabSheet(SouthContainer, tsWorkspace, 'Workspace');
    TabWorkspace.Parent := tsWorkspace;
    TabWorkspace.Align := alClient;
  end
  else
    FreeAndNil(tsWorkspace);

  { Mobil }
  if WantMobil then
  begin
    TabMobil := TMobilTab.Create(self);
    tsMobil := InitTabSheet(PageControl, tsMobil, 'Mobil');
    TabMobil.Parent := tsMobil;
    TabMobil.InitGrid;
  end
  else
    FreeAndNil(tsMobil);
end;

procedure TFormFR62.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeViews;
  GM.GuiInterface := nil;
end;

procedure TFormFR62.FormDestroy(Sender: TObject);
begin
  Main.GuiManager.Free;
  Main.GuiManager := nil;

  if WantCategory then
    TabCategory.DisposeGrid;

  DisposePages;

  FTestMemo.Free;
  FStatusMemo.Free;
  WP.Free;
end;

procedure TFormFR62.DisposePages;
begin
  TabEntries.Free;
  TabEntries := nil;

  TabEvent.Free;
  TabEvent := nil;

  TabReport.Free;
  TabReport := nil;

  TabCategory.Free;
  TabCategory := nil;

  TabRacing.Free;
  TabRacing := nil;

  TabKeying.Free;
  TabKeying := nil;

  TabMenu.Free;
  TabMenu := nil;

  TabWorkspace.Free;
  TabWorkspace := nil;

  TabMobil.Free;
  TabMobil := nil;
end;

procedure TFormFR62.SetupDoubleBufferControls;
begin
  UseDoubleBufferForGrid := True;
  ToolBar.DoubleBuffered := True;

  NorthContainer.DoubleBuffered := True;
  PageControl.DoubleBuffered := True;
  SouthContainer.DoubleBuffered := True;
end;

procedure TFormFR62.InitViews;
begin
  { interface method }
  if WantEvent then
    TabEvent.InitGrid;
  if WantEntries then
    TabEntries.InitGrid;
  if WantRacing then
    TabRacing.InitTiming;
  if WantKeying then
    TabKeying.InitTiming;
  if WantMobil then
  begin
    TabMobil.InitGrid;
    TabMobil.InitMobil;
  end;

  GM.InitCache;

  Update; //to update display properly when using styles
end;

procedure TFormFR62.DisposeViews;
begin
  { interface method }
  if WantEvent then
    TabEvent.GB.GridUpdate.OnUpdateView := nil;
  if WantEntries then
    TabEntries.GB.GridUpdate.OnUpdateView := nil;
  if WantMobil then
    TabMobil.GB.GridUpdate.OnUpdateView := nil;

  GM.DisposeCache;

  if WantKeying then
    TabKeying.DisposeTiming;
  if WantRacing then
    TabRacing.DisposeTiming;
  if WantEntries then
    TabEntries.DisposeGrid;
  if WantEvent then
    TabEvent.DisposeGrid;
  if WantMobil then
    TabMobil.DisposeGrid;
end;

procedure TFormFR62.UpdateCaption;
begin
  { interface method }
  Caption := GM.AppName + ' - ' + DM.EventName;
end;

procedure TFormFR62.UpdateWorkspaceStatus;
begin
  { interface method }
end;

procedure TFormFR62.WMUpdateGrid(var Msg: TMessage);
begin
  //do nothing, just trigger OnIdle
end;

procedure TFormFR62.DoOnIdle;
begin
  GM.DoOnIdle;
  if WantEvent and (PageControl.ActivePage = tsEvent) then
  begin
    TabEvent.GB.GridUpdate.DoOnIdle;
    TabEvent.UpdateButtonCaptions;
  end
  else if WantEntries and (PageControl.ActivePage = tsEntries) then
    TabEntries.GB.GridUpdate.DoOnIdle
  else if WantMobil and (PageControl.ActivePage = tsMobil) then
    TabMobil.DoOnIdle;
end;

function TFormFR62.GetGuiManager: TGuiManager;
begin
  result := Main.GuiManager;
end;

function TFormFR62.GetStatusMemo: TStrings;
begin
  if WantReport then
    result := TabReport.StatusMemo.Lines
  else
  begin
    if FStatusMemo = nil then
      FStatusMemo := TStringList.Create;
    result := FStatusMemo;
  end;
end;

function TFormFR62.GetTestMemo: TStrings;
begin
  if WantReport then
    result := TabReport.TestMemo.Lines
  else
  begin
    if FTestMemo = nil then
      FTestMemo := TStringList.Create;
    result := FTestMemo;
  end;
end;

function TFormFR62.GetDocManager: TDocManager;
begin
  result := Main.DocManager;
end;

procedure TFormFR62.HandleInform(Action: TGuiAction);
begin
  case Action of
    acUndo, acRedo:
    begin
      if WantEvent then
        TabEvent.GB.GridUpdate.InvalidateView;
      if WantMobil then
        TabMobil.GB.GridUpdate.InvalidateView;
    end;

    acColor:
    begin
      if WantEvent then
      begin
        TabEvent.UpdateColorMode;
        TabEvent.GB.GridUpdate.InvalidateView;
      end;
    end;

    acStrict, acRelaxed:
    begin
      if WantEvent then
        TabEvent.UpdateStrictRelaxed;
    end;

    RaceChanged:
    begin
      if WantRacing then
      begin
        TabRacing.UpdateRaceCaption;
        TabRacing.ResetAge;
      end;
      if WantKeying then
        TabKeying.UpdateRaceCaption;
    end;

    EventChanged:
    begin
      BO.Calc;
      if WantMobil then
        TabMobil.InitMobil;
    end;

    ThrowoutNumberChanged:
    begin
      if WantMobil then
        TabMobil.MF.UpdateCore;
    end;

    WorkspaceListChanged:
    begin
      if WantMenu then
        TabMenu.InitUrlCombo;
    end;

    CategoryGridChanged:
    begin
      if WantCategory and (TabCategory <> nil) then
        TabCategory.UpdateBtnClick(nil);
    end;

    StartBatch:
    begin
      if WantMenu then
        TabMenu.Batch;
    end;

    ExportTranslation:
    begin
      ExportBtnText;
    end;

    else
    begin
      if WantEvent then
        TabEvent.GB.GridUpdate.HandleInform;
      if WantEntries then
        TabEntries.GB.GridUpdate.HandleInform;
    end;
  end;
end;

procedure TFormFR62.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  DoOnIdle;
end;

procedure TFormFR62.IdleTimerTimer(Sender: TObject);
begin
  PostMessage(Handle, wm_UpdateGrid, 0, 0);
  if WantRacing and (PageControl.ActivePage = tsRacing) then
  begin
    TabRacing.Model.TimerTick;
    TabRacing.TimingGrid.RePaint;
  end;
end;

procedure TFormFR62.ApplicationEventsException(Sender: TObject;
  E: Exception);
begin
  GM.DoOnException(Sender, E);
end;

procedure TFormFR62.InitBtnText;
begin
  if WantCategory then
    TabCategory.InitBtnText;
  if WantWorkspace then
    TabWorkspace.InitBtnText;
  if WantKeying then
    TabKeying.InitBtnText;
  if WantEvent then
    TabEvent.InitBtnText;
  if WantMenu then
    TabMenu.InitBtnText;

  if Main.Params.WantLocalText then
  begin
    ClearBtn.Caption := GetText(MF_ClearBtn_Caption);
    NorthBtn.Caption := GetText(MF_NorthBtn_Caption);
    SouthBtn.Caption := GetText(MF_SouthBtn_Caption);
    InfoBtn.Caption := GetText(MF_InfoBtn_Caption);
    StyleBtn.Caption := GetText(MF_StyleBtn_Caption);
    RowHeightBtn.Caption := GetText(MF_RowHeightBtn_Caption);
    LanguageBtn.Caption := GetText(MF_LanguageBtn_Caption);

    ClearBtn.Hint := GetText(MF_ClearBtn_Hint);
    NorthBtn.Hint := GetText(MF_NorthBtn_Hint);
    SouthBtn.Hint := GetText(MF_SouthBtn_Hint);
    InfoBtn.Hint := GetText(MF_InfoBtn_Hint);
    StyleBtn.Hint := GetText(MF_StyleBtn_Hint);
    RowHeightBtn.Hint := GetText(MF_RowHeightBtn_Hint);
    LanguageBtn.Hint := GetText(MF_LanguageBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TFormFR62.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TFormFR62.InitPrimaryBtnText;
begin
  ClearBtn.Caption := 'Clear';
  NorthBtn.Caption := 'Data';
  SouthBtn.Caption := 'Timing';
  InfoBtn.Caption := 'Info';
  StyleBtn.Caption := 'Style';
  RowHeightBtn.Caption := 'Height';
  LanguageBtn.Caption := 'Lang';

  ClearBtn.Hint := 'reset by loading builtin test data';
  NorthBtn.Hint := 'toggle visibility of event menu controls';
  SouthBtn.Hint := 'toggle visibility of finish position key pad';
  InfoBtn.Hint := 'show info message in Memo';
  StyleBtn.Hint := 'toggle betweeen default and custom style';
  RowHeightBtn.Hint := 'toggle between smaller and bigger height of rows in the grid';
  LanguageBtn.Hint := 'toggle language for some elements';
end;

procedure TFormFR62.InitAlternativeBtnText;
begin
  ClearBtn.Caption := 'Clear';
  NorthBtn.Caption := 'Daten';
  SouthBtn.Caption := 'Timing';
  InfoBtn.Caption := 'Info';
  StyleBtn.Caption := 'Stil';
  RowHeightBtn.Caption := 'Höhe';
  LanguageBtn.Caption := 'Sprache';

  ClearBtn.Hint := 'Testdaten laden (alles auf Anfangszustand zurücksetzen)';
  NorthBtn.Hint := 'EventMenu für Download umschalten (PageControl oben)';
  SouthBtn.Hint := 'TimingGrid für Zielpositionen umschalten (PageControl unten)';
  InfoBtn.Hint := 'Info anzeigen (im Memo)';
  StyleBtn.Hint := 'Stil umschalten (Windows und Custom)';
  RowHeightBtn.Hint := 'Zeilenhöhe umschalten (im Grid)';
  LanguageBtn.Hint := 'Sprache umschalten';
end;

procedure TFormFR62.ExportBtnText;
begin
  if WantCategory then
    TabCategory.ExportBtnText;
  if WantWorkspace then
    TabWorkspace.ExportBtnText;
  if WantKeying then
    TabKeying.ExportBtnText;
  if WantEvent then
    TabEvent.ExportBtnText;
  if WantMenu then
    TabMenu.ExportBtnText;

  InitDefaultBtnText;

  SetText(MF_TestDataBtn_Caption, ClearBtn.Caption);
  SetText(MF_NorthBtn_Caption, NorthBtn.Caption);
  SetText(MF_SouthBtn_Caption, SouthBtn.Caption);
  SetText(MF_InfoBtn_Caption, InfoBtn.Caption);
  SetText(MF_StyleBtn_Caption, StyleBtn.Caption);
  SetText(MF_RowHeightBtn_Caption, RowHeightBtn.Caption);
  SetText(MF_LanguageBtn_Caption, LanguageBtn.Caption);

  SetText(MF_TestDataBtn_Hint, ClearBtn.Hint);
  SetText(MF_NorthBtn_Hint, NorthBtn.Hint);
  SetText(MF_SouthBtn_Hint, SouthBtn.Hint);
  SetText(MF_InfoBtn_Hint, InfoBtn.Hint);
  SetText(MF_StyleBtn_Hint, StyleBtn.Hint);
  SetText(MF_RowHeightBtn_Hint, RowHeightBtn.Hint);
  SetText(MF_LanguageBtn_Hint, LanguageBtn.Hint);
end;

procedure TFormFR62.ToggleEventMenu;
begin
  NorthContainer.Visible := not NorthContainer.Visible;
end;

procedure TFormFR62.ToggleTimingPanel;
begin
  SouthContainer.Visible := not SouthContainer.Visible;
end;

procedure TFormFR62.ClearBtnClick(Sender: TObject);
begin
  GM.SwapEvent(Main.TestData);
end;

procedure TFormFR62.NorthBtnClick(Sender: TObject);
begin
  ToggleEventMenu;
end;

procedure TFormFR62.SouthBtnClick(Sender: TObject);
begin
  ToggleTimingPanel;
end;

procedure TFormFR62.InfoBtnClick(Sender: TObject);
begin
  if WantReport then
  begin
    TabReport.StatusMemo.Lines.BeginUpdate;
    try
      Inc(InfoClicks);
      PageControl.ActivePage := tsReport;
      TInfoMemo.Fill(TabReport.StatusMemo.Lines);

      TabReport.StatusMemo.Lines.Add('---');
      TabReport.StatusMemo.Lines.Add('info button click counter: ' + IntToStr(InfoClicks));
    finally
      TabReport.StatusMemo.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormFR62.StyleBtnClick(Sender: TObject);
var
 Name: string;
 c: Integer;
 s: string;
 i: Integer;
begin
  c := -1;
  for Name in TStyleManager.StyleNames do
    Inc(c);
  i := CurrentStyleIndex;
  Inc(i);
  if i > c then
    i := 0;
  s := TStyleManager.StyleNames[i];
  TColorService.StyleName := s;
  TStyleManager.SetStyle(s);
  CurrentStyleIndex := i;
  UpdateCaption;
  Caption := Format('%s - [ Style = %s ]', [Caption, s]);
end;

procedure TFormFR62.RowHeightBtnClick(Sender: TObject);
begin
  TDisplayGrid.WantSmallRows := not TDisplayGrid.WantSmallRows;
  DisposeViews;
  InitViews;
  if WantCategory then
  begin
    TabCategory.DisposeGrid;
    TabCategory.InitGrid;
  end;
end;

function GetWindowsLanguage: string;
var
  WinLanguage: array [0..50] of char;
begin
  VerLanguageName(GetSystemDefaultLangID, WinLanguage, 50);
  Result := StrPas(WinLanguage);
end;

function IsControlKeyDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State) ;
  result := ((State[vk_Control] And 128) <> 0) ;
end;

procedure TFormFR62.HelpBtnClick(Sender: TObject);
var
  s: string;
  fn: string;
  fn1: string;
  fn2: string;
  fn3: string;
begin
  //HtmlHelp(hWndCaller: HWND; pszFile: PWideChar; uCommand: UINT; dwData: DWORD_PTR): HWND;

  fn1 := 'FR01-en.chm';
  fn2 := 'FR01-de.chm';
  fn3 := 'FR01.chm';
  //load appropriate file if it exists
  if IsControlKeyDown then
    fn := fn1
  else
  begin
    s := GetWindowsLanguage;
    if Pos('eutsch', s) > 0 then
      fn := fn2
    else if Pos('eutsch', s) > 0 then
      fn := fn2
    else
      fn := fn1;
  end;

  //english is default
  if not FileExists(fn) then
  begin
    fn := fn2;
  end;

  if not FileExists(fn) then
  begin
    fn := fn3;
  end;

  if FileExists(fn) then
    HtmlHelp(
      Application.Handle,
      PChar(fn),
      HH_DISPLAY_TOPIC,
      0);
end;

procedure TFormFR62.LanguageBtnClick(Sender: TObject);
begin
  if Main.AppTranslator.HasLocalText then
  begin
    //toggle between builtin english text and external localized text
    Main.Params.WantAlternativeText := False;
    Main.Params.WantLocalText := not Main.Params.WantLocalText;
  end
  else
  begin
    //toggle between builtin english and german text
    Main.Params.WantLocalText := False;
    Main.Params.WantAlternativeText := not Main.Params.WantAlternativeText;
  end;
  InitBtnText;
end;

procedure TFormFR62.PlusBtnClick(Sender: TObject);
begin
  BO.UpdateStartlistCount('', BO.BOParams.StartlistCount + 1);
  if WantEvent then
    TabEvent.UpdateGrid;
  if WantRacing then
    TabRacing.UpdateGrid;
end;

procedure TFormFR62.MinusBtnClick(Sender: TObject);
begin
  if BO.BOParams.StartlistCount > 2 then
    BO.UpdateStartlistCount('', BO.BOParams.StartlistCount - 1);
  if WantEvent then
    TabEvent.UpdateGrid;
  if WantRacing then
    TabRacing.UpdateGrid;
end;

procedure TFormFR62.PageControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer) ;
var
  pc: TPageControl;
begin
  if Sender is TPageControl then
  begin
    pc := Sender as TPageControl;
    if pc.PageCount > 1 then
    begin
      DraggedTabSheet := pc.ActivePage;
      pc.BeginDrag(False);
    end;
  end;
end;

procedure TFormFR62.PageControlDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean) ;
begin
  if (Sender is TPageControl) then
    Accept := True;
end;

procedure TFormFR62.PageControlDragDrop(Sender, Source: TObject; X, Y: Integer) ;
const
  TCM_GETITEMRECT = $130A;
var
  TabRect: TRect;
  j: Integer;
  pc: TPageControl;
begin
  if (Sender is TPageControl) then
  begin
    pc := Sender as TPageControl;

    if Sender = Source then
    begin
      //move tab within the same pagecontrol
      for j := 0 to pc.PageCount - 1 do
      begin
        pc.Perform(TCM_GETITEMRECT, j, LParam(@TabRect)) ;
        if PtInRect(TabRect, Point(X, Y)) then
        begin
          if pc.ActivePage.PageIndex <> j then
            pc.ActivePage.PageIndex := j;
          Exit;
        end;
      end;
    end
    else
    begin
      DraggedTabSheet.PageControl := pc;
    end;

  end;

end;

function TFormFR62.InitTabSheet(pc: TPageControl; ts: TTabSheet; ACaption: string): TTabSheet;
begin
  if ts = nil then
  begin
    ts := TTabSheet.Create(pc);
    ts.Caption := ACaption;
    ts.PageControl := pc;
  end;
  result := ts;
end;

procedure TFormFR62.ShowMemo;
begin
  if WantReport and (tsReport <> nil) then
    PageControl.ActivePage := tsReport;
end;

end.
