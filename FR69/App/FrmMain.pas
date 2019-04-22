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
  RiggVar.BO.CacheMotor,
  RiggVar.BO.Def,
  RiggVar.BO.ExcelImport,
  RiggVar.BO.ExcelExport,
  RiggVar.BO.Params,
  RiggVar.Col.Cache,
  RiggVar.Col.Event,
  RiggVar.Col.Race,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.DAL.Manager,
  RiggVar.DAL.Redirector,
  RiggVar.Grid.Color,
  RiggVar.Grid.Control,
  RiggVar.Grid.ColGrid,
  RiggVar.Util.InputMatrix,
  RiggVar.Util.Sound,
  RiggVar.Web1.MotorBase,
  RiggVar.Web1.MotorRemote,
  RiggVar.Web1.MotorHome,
  FmBrowser,
  FmCache,
  FmCategory,
  FmCourse,
  FmEntries,
  FmEvent,
  FmJson,
  FmKeying,
  FmListing,
  FmMark,
  FmMenu,
  FmMobil,
  FmRace,
  FmRacing,
  FmReport,
  FmRoundings,
  FrmTest,
  FmTiming,
  FmWeb,
  FmWorkspace,
  Vcl.Grids;

const
  wm_UpdateGrid = wm_User + 300;

type
  TFormFR62 = class(TForm, IGuiInterface)
    ApplicationEvents: TApplicationEvents;
    DocActionList: TActionList;
    acBackup: TAction;
    acRestore: TAction;
    acClear: TAction;
    acRecreate: TAction;
    IdleTimer: TTimer;
    Panel: TPanel;
    NorthContainer: TPageControl;
    PageControl: TPageControl;
    SouthContainer: TPageControl;
    WestContainer: TPageControl;
    ToolBar: TToolBar;
    ClearBtn: TSpeedButton;
    WestBtn: TSpeedButton;
    NorthBtn: TSpeedButton;
    SouthBtn: TSpeedButton;
    InfoBtn: TSpeedButton;
    TestBtn: TSpeedButton;
    FeatureBtn: TSpeedButton;
    LanguageBtn: TSpeedButton;
    PlusBtn: TSpeedButton;
    MinusBtn: TSpeedButton;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileOpenItem: TMenuItem;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    FileDeleteItem: TMenuItem;
    NF1: TMenuItem;
    ExitItem: TMenuItem;
    ActionsMenu: TMenuItem;
    ConnectItem: TMenuItem;
    DisconnectItem: TMenuItem;
    SpacerBackup: TMenuItem;
    BackupItem: TMenuItem;
    RestoreItem: TMenuItem;
    ClearItem: TMenuItem;
    SpacerPlugin: TMenuItem;
    PlugInItem: TMenuItem;
    PlugOutItem: TMenuItem;
    SynchronizeItem: TMenuItem;
    UploadItem: TMenuItem;
    DownloadItem: TMenuItem;
    OptionsMenu: TMenuItem;
    EventParamsItem: TMenuItem;
    EventPropsItem: TMenuItem;
    UniquaPropsItem: TMenuItem;
    ScoringModuleItem: TMenuItem;
    DBInterfaceItem: TMenuItem;
    SwitchPropsItem: TMenuItem;
    TestMenu: TMenuItem;
    LoadTestDataItem: TMenuItem;
    HelpMenu: TMenuItem;
    ContentItem: TMenuItem;
    InfoItem: TMenuItem;
    CopyRankItem: TMenuItem;
    WriteJSXmlItem: TMenuItem;
    WriteProxyXmlItem: TMenuItem;
    ExportItem: TMenuItem;
    ShowUndoListsItem: TMenuItem;
    ShowWatchesItem: TMenuItem;
    TestPenaltyItem: TMenuItem;
    OpenByNameItem: TMenuItem;
    RecreateItem: TMenuItem;
    LanguageItem: TMenuItem;
    BatchTestItem: TMenuItem;
    BatchDownloadItem: TMenuItem;
    BatchReportItem: TMenuItem;
    CalcItem: TMenuItem;
    NProviderStart: TMenuItem;
    NProviderEnd: TMenuItem;
    BridgeProviderItem: TMenuItem;
    FleetPropsItem: TMenuItem;
    ToolsMenu: TMenuItem;
    ToolsN3: TMenuItem;
    ImportItem: TMenuItem;
    ToolsN2: TMenuItem;
    InitFleetItem: TMenuItem;
    CopyFleetItem: TMenuItem;
    InitFleetFromFinishItem: TMenuItem;
    ToolsN1: TMenuItem;
    PartialCalcItem: TMenuItem;
    IniPropsItem: TMenuItem;
    RegattaPropsItem: TMenuItem;
    NamePropsItem: TMenuItem;
    DisableFleetItem: TMenuItem;
    AllOptionsItem: TMenuItem;
    ProviderMenu: TMenuItem;
    BatchProcessItem: TMenuItem;
    WriteRDXmlItem: TMenuItem;
    ShowWorkspaceFilesItem: TMenuItem;
    WorkspaceLocationItem: TMenuItem;
    PublishMenu: TMenuItem;
    PublishAllItem: TMenuItem;
    SoundItem: TMenuItem;
    WriteResourcesItem: TMenuItem;
    WorkspaceInfoItem: TMenuItem;
    PublishPolicyServerItem: TMenuItem;
    UnicodeScannerItem: TMenuItem;
    PublishSocketsItem: TMenuItem;
    PublishHomeWebItem: TMenuItem;
    PublishRemoteWebItem: TMenuItem;
    PublishSilverlightWebItem: TMenuItem;
    EventMenuItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    NorthContainerItem: TMenuItem;
    SouthContainerItem: TMenuItem;
    LanguageToggle: TMenuItem;
    ClearRaceItem: TMenuItem;
    SelectRaceItem: TMenuItem;
    GoBackToRaceItem: TMenuItem;
    StyleItem: TMenuItem;
    MainParamsItem: TMenuItem;
    InspectReportsItem: TMenuItem;
    ShowScheduleItem: TMenuItem;
    AboutItem: TMenuItem;
    AccessToggle: TMenuItem;
    TestJsonItem: TMenuItem;
    PagesItem: TMenuItem;
    TestFormItem: TMenuItem;
    TestMessageParserItem: TMenuItem;
    EnableLogItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure IdleTimerTimer(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure LanguageItemClick(Sender: TObject);
    procedure FileOpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure FileDeleteItemClick(Sender: TObject);
    procedure UniquaPropsItemClick(Sender: TObject);
    procedure EventParamsItemClick(Sender: TObject);
    procedure EventPropsItemClick(Sender: TObject);
    procedure ScoringModuleItemClick(Sender: TObject);
    procedure ConnectItemClick(Sender: TObject);
    procedure DisconnectItemClick(Sender: TObject);
    procedure acBackupExecute(Sender: TObject);
    procedure acRecreateExecute(Sender: TObject);
    procedure acRestoreExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure ContentItemClick(Sender: TObject);
    procedure InfoItemClick(Sender: TObject);
    procedure DBInterfaceItemClick(Sender: TObject);
    procedure PlugInItemClick(Sender: TObject);
    procedure PlugOutItemClick(Sender: TObject);
    procedure SynchronizeItemClick(Sender: TObject);
    procedure UploadItemClick(Sender: TObject);
    procedure DownloadItemClick(Sender: TObject);
    procedure SwitchPropsItemClick(Sender: TObject);
    procedure CopyRankItemClick(Sender: TObject);
    procedure LoadTestDataItemClick(Sender: TObject);
    procedure ExportItemClick(Sender: TObject);
    procedure ShowUndoListsItemClick(Sender: TObject);
    procedure TestMenuClick(Sender: TObject);
    procedure ShowWatchesItemClick(Sender: TObject);
    procedure TestPenaltyItemClick(Sender: TObject);
    procedure OpenByNameItemClick(Sender: TObject);
    procedure BatchTestItemClick(Sender: TObject);
    procedure BatchDownloadItemClick(Sender: TObject);
    procedure BatchReportItemClick(Sender: TObject);
    procedure CalcItemClick(Sender: TObject);
    procedure ActionsMenuClick(Sender: TObject);
    procedure BridgeProviderItemClick(Sender: TObject);
    procedure OptionsMenuClick(Sender: TObject);
    procedure InitFleetItemClick(Sender: TObject);
    procedure CopyFleetItemClick(Sender: TObject);
    procedure PartialCalcItemClick(Sender: TObject);
    procedure FleetPropsItemClick(Sender: TObject);
    procedure InitFleetFromFinishItemClick(Sender: TObject);
    procedure ImportItemClick(Sender: TObject);
    procedure IniPropsItemClick(Sender: TObject);
    procedure ToolsMenuClick(Sender: TObject);
    procedure RegattaPropsItemClick(Sender: TObject);
    procedure NamePropsItemClick(Sender: TObject);
    procedure DisableFleetItemClick(Sender: TObject);
    procedure AllOptionsItemClick(Sender: TObject);
    procedure ProviderMenuClick(Sender: TObject);
    procedure SoundItemClick(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure WriteResourcesItemClick(Sender: TObject);
    procedure PublishSocketsItemClick(Sender: TObject);
    procedure NorthContainerItemClick(Sender: TObject);
    procedure SouthContainerItemClick(Sender: TObject);
    procedure ClearRaceItemClick(Sender: TObject);
    procedure SelectRaceItemClick(Sender: TObject);
    procedure GoBackToRaceItemClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure WestBtnClick(Sender: TObject);
    procedure NorthBtnClick(Sender: TObject);
    procedure SouthBtnClick(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure ToggleStyleBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure LanguageBtnClick(Sender: TObject);
    procedure FeatureBtnClick(Sender: TObject);
    procedure PlusBtnClick(Sender: TObject);
    procedure MinusBtnClick(Sender: TObject);
    procedure LanguageToggleClick(Sender: TObject);
    procedure AccessToggleClick(Sender: TObject);
    procedure MainParamsItemClick(Sender: TObject);
    procedure WriteJSXmlItemClick(Sender: TObject);
    procedure WriteProxyXmlItemClick(Sender: TObject);
    procedure WriteRDXmlItemClick(Sender: TObject);
    procedure ShowWorkspaceFilesItemClick(Sender: TObject);
    procedure WorkspaceLocationItemClick(Sender: TObject);
    procedure WorkspaceInfoItemClick(Sender: TObject);
    procedure PublishAllItemClick(Sender: TObject);
    procedure PublishPolicyServerItemClick(Sender: TObject);
    procedure UnicodeScannerItemClick(Sender: TObject);
    procedure PublishHomeWebItemClick(Sender: TObject);
    procedure PublishSilverlightWebItemClick(Sender: TObject);
    procedure PublishRemoteWebItemClick(Sender: TObject);
    procedure EventMenuItemClick(Sender: TObject);
    procedure InspectReportsItemClick(Sender: TObject);
    procedure ShowScheduleItemClick(Sender: TObject);
    procedure PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure PageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TestJsonItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
    procedure SaveConfigItemClick(Sender: TObject);
    procedure PagesItemClick(Sender: TObject);
    procedure TestFormItemClick(Sender: TObject);
    procedure TestMessageParserItemClick(Sender: TObject);
    procedure EnableLogItemClick(Sender: TObject);
    procedure PublishMenuClick(Sender: TObject);
  private
    FTestMemo: TStrings;
    FStatusMemo: TStrings;
    DraggedTabSheet: TTabsheet;
    CurrentStyleIndex: Integer;
    InfoClicks: Integer;

    function GetDocManager: TDocManager;
    function GetGuiManager: TGuiManager;
    function GetCacheMotor: TCacheMotor;
    function GetRace: Integer;
    function GetIT: Integer;

    procedure InitViews;
    procedure DisposeViews;

    procedure UpdateIT(Sender: TObject);
    procedure UpdateRace(Sender: TObject);
    procedure UpdateCaption;
    procedure UpdateWorkspaceStatus;

    procedure WMUpdateGrid(var Msg: TMessage); message wm_UpdateGrid;
    procedure WMCopyData(var M: TMessage); message WM_COPYDATA;

    procedure PaintLED;
    procedure CopyRank;
    procedure CheckFileParam(fn: string);
    procedure ProcessBatch(BatchID: Integer);

    procedure DoOnIdle;
    procedure HandleInform(Action: TGuiAction);
    procedure HandleNewStartlistCount;
    procedure SetupDoubleBufferControls;

    procedure InitNewBO;
    procedure InitBridge;
    procedure DisposeBridge;
    procedure PanelOK(Sender: TObject);
    procedure EndEdit(ep: TEditPage; newParams: TBOParams; refreshFlag: Boolean = false);
    procedure UpdateConnectionItems;
    procedure EnableConnectionItems;
    procedure DisableConnectionItems;
    procedure DisableLocalAccess;
    procedure DisableWebAccess;
    procedure DisableBridgeItems;
    procedure DisablePublishItems;
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
    procedure ShowHelp;

    property GM: TGuiManager read GetGuiManager;
    property DM: TDocManager read GetDocManager;
    property IT: Integer read GetIT;
    property Race: Integer read GetRace;
    property CacheMotor: TCacheMotor read GetCacheMotor;
  protected
    WP: TWantPages;

    WantProviderMenu: Boolean;
    WantPublishMenu: Boolean;

    tsMenu: TTabSheet;
    tsEntries: TTabSheet;
    tsRace: TTabSheet;
    tsEvent: TTabSheet;
    tsReport: TTabSheet;
    tsMobil: TTabSheet;
    tsCache: TTabSheet;
    tsCategory: TTabSheet;
    tsJson: TTabSheet;
    tsBrowser: TTabSheet;
    tsTiming: TTabSheet;
    tsRacing: TTabSheet;
    tsKeying: TTabSheet;
    tsWorkspace: TTabSheet;
    tsWeb: TTabSheet;
    tsCourse: TTabSheet;
    tsMark: TTabSheet;
    tsRoundings: TTabSheet;
    tsListing: TTabSheet;

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
    FWantJson: Boolean;
    FWantMobil: Boolean;
    FWantProfile: Boolean;
    FWantWeb: Boolean;
    FWantListing: Boolean;
    FWantRoundings: Boolean;
    FWantCourse: Boolean;
    FWantMark: Boolean;

    { Test that access to all of these Tabs is properly guarded by a WantXX }
    TabMenu: TMenuTab;
    TabEntries: TEntriesTab;
    TabRace: TRaceTab;
    TabEvent: TEventTab;
    TabReport: TReportTab;
    TabTiming: TTimingTab;
    TabRacing: TRacingTab;
    TabKeying: TKeyTab;
    TabWorkspace: TWorkspaceTab;
    TabCategory: TCategoryTab;
    TabJson: TJsonTab;
    TabMobil: TMobilTab;
    TabCache: TCacheTab;
    TabBrowser: TBrowserTab;
    TabWeb: TWebTab;
    TabCourse: TCourseTab;
    TabMark: TMarkTab;
    TabRoundings: TRoundingsTab;
    TabListing: TListingTab;

    { protected to avoid warning H2219, unused method}
    procedure RecreatePages;
    procedure LoadWP;
    procedure UnloadWP;
    procedure InitWant;
    procedure InitWantHard;
    procedure UpdateWant;
    procedure InitButtons;
    procedure InitMenuItems;
    procedure DisableEditPages;
    procedure HandleBackup(Sender: TObject; EventData: string);
    procedure ToggleFeatures;
    procedure ToggleRowHeight;
    procedure ToggleStyle;
  public
    procedure ShowMemo;
    procedure Trace(Sender: TObject; s: string);

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
    property WantJson: Boolean read FWantJson;
    property WantMobil: Boolean read FWantMobil;
    property WantCache: Boolean read FWantCache;
    property WantBrowser: Boolean read FWantBrowser;
    property WantProfile: Boolean read FWantProfile;
    property WantRoundings: Boolean read FWantRoundings;
    property WantListing: Boolean read FWantListing;
    property WantCourse: Boolean read FWantCourse;
    property WantMark: Boolean read FWantMark;
    property WantWeb: Boolean read FWantWeb;
  end;

var
  FormFR62: TFormFR62;

implementation

uses
  RiggVar.App.Main,
  RiggVar.App.Translation,
  RiggVar.BO.Time,
  RiggVar.BR.BridgeAbstract,
  RiggVar.BR.BridgeController,
  RiggVar.BR.OutputController,
  RiggVar.BR.PeerController,
  RiggVar.BR.SwitchController,
  RiggVar.Conn.ClientMsg,
  RiggVar.Util.InfoMemo,
  RiggVar.Util.MultInst,
  FrmBatchProcess,
  FrmBridgeManager,
  FrmCacheOptions,
  FrmContainer,
  FrmExcelImport,
  FrmHelp,
  FrmInfo,
  FrmInspector,
  FrmJson,
  FrmMsgParser2,
  FrmNameFields,
  FrmOptions,
  FrmPages,
  FrmPenalty,
  FrmReportNav,
  FrmSwitchProps,
  FrmUndoManager,
  FrmUnicodeScanner,
  FrmWorkspaceFiles,
  FrmWorkspaceInfo,
  FrmWorkspace;

const
  StatusPanelPortIn = 0;
  StatusPanelPortOut = 1;
  StatusPanelRemoteSitePort = 2;
  StatusPanelHomeSitePort = 3;
  StatusPanelRace = 4;
  StatusPanelIT = 5;
  StatusPanelPlugStatus = 6;
  StatusPanelConnectStatus = 7;
  StatusPanelDAL = 8;
  StatusPanelWorkspaceType = 9;
  StatusPanelWorkspaceID = 10;
  StatusPanelWorkspaceUrl = 11;

{$R *.dfm}

{ TFormFR62 }

procedure TFormFR62.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  GM.GuiInterface := self;

  CurrentStyleIndex := 1;

  Width := 1300;
  Height := 720;

  WestContainer.Align := alLeft;
  WestContainer.Width := 272;

  Panel.Align := alClient;

  NorthContainer.Parent := Panel;
  NorthContainer.Align := alTop;
  NorthContainer.Height := 200;

  SouthContainer.Parent := Panel;
  SouthContainer.Align := alBottom;
  SouthContainer.Height := 240;

  PageControl.Parent := Panel;
  PageControl.Align := alClient;

  InitStatic;
  InitWant;
  InitPages;

  SetupDoubleBufferControls;

  DisableEditPages;
  InitMenuItems;

  SaveItem.Enabled := DM.SaveEnabled;
  SaveAsItem.Enabled := DM.SaveAsEnabled;
  FileDeleteItem.Enabled := DM.DeleteEnabled;

  ClearBtn.Enabled := Main.HaveAppPermission;
  TestBtn.Enabled := Main.HaveAppPermission;
  FeatureBtn.Enabled := Main.Params.WantFeatures;

  InitBridge;

  Main.LanguageManager.Localize(self);

  DisableConnectionItems;
  DisableBridgeItems;
  UpdateConnectionItems;

  StatusBar.Panels[StatusPanelDAL].Text := Main.DocManager.DBInterface;
  UpdateWorkspaceStatus;

  if not Main.HaveFilePermission then
    DisableLocalAccess;
  if not Main.HaveSocketPermission then
    DisableConnectionItems;
  if not Main.HaveWebPermission then
    DisableWebAccess;
  DisablePublishItems;

  if ParamCount > 0 then
  begin
    CheckFileParam(ParamStr(1));
  end;
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
  NorthContainer.Visible := WantRoundings;
  SouthContainer.Visible := False;
  WestContainer.Visible := WantMark;

  InitForm;

  if WantMenu then
  begin
    TabMenu.TestMemo := TestMemo;
    TabMenu.Memo := StatusMemo;
  end;
  Main.IniImage.NorthContainerVisible := WantMenu;

  if WantRoundings then
    NorthContainer.ActivePage := tsCourse
  else if NorthContainer.PageCount > 0 then
    NorthContainer.ActivePage := NorthContainer.Pages[0];

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
  WP.WantCourse := FWantCourse;
  WP.WantMark := FWantMark;
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
  FWantCourse := WP.WantCourse;
  FWantMark := WP.WantMark;
end;

procedure TFormFR62.InitStatic;
begin
  TInfoMemo.WantLogo := True;
  TDisplayGrid.WantSmallRows := true;
  TDisplayGrid.WantCustomDrawCell := true;

  WantProviderMenu := True;
  WantPublishMenu := True;
end;

procedure TFormFR62.InitWantHard;
begin
  { Tabs-NorthContainer }
  FWantMenu := True;
  FWantCourse := True;

  { Tabs-WestContainer }
  FWantMark := True;

  { Tabs-PageControl }
  FWantEntries := True;
  FWantRace := True;
  FWantEvent := True;
  FWantReport := True;
  FWantMobil := False;
  FWantCache := False;
  FWantCategory := False;
  FWantJson := False;
  FWantBrowser := False;
  FWantWeb := True;

  { Tabs-SourthContainer }
  FWantTiming := True;
  FWantRacing := True;
  FWantKeying := True;
  FWantWorkspace := False;

  FWantListing := True;
  FWantRoundings := True;
end;

procedure TFormFR62.InitWant;
var
  ii: TIniImage;
begin
  { Tabs }
  ii := Main.IniImage;

  { Tabs-NorthContainer }
  FWantMenu := ii.WantMenu;
  FWantCourse := ii.WantCourse;

  { Tabs-WestContainer }
  FWantMark := ii.WantMark;

  { Tabs-PageControl }
  FWantEntries := ii.WantEntries;
  FWantRace := ii.WantRace;
  FWantEvent := ii.WantEvent;
  FWantReport := ii.WantReport;
  FWantMobil := Main.HaveAppPermission and ii.WantMobil;
  FWantCache := ii.WantCache;
  FWantCategory := Main.HaveAppPermission and ii.WantCategory;
  FWantJson := Main.HaveAppPermission and ii.WantJson;
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
  ii.WantCourse := FWantCourse;

  ii.WantMark := FWantMark;

  ii.WantEntries := FWantEntries;
  ii.WantRace := FWantRace;
  ii.WantEvent := FWantEvent;
  ii.WantReport := FWantReport;
  ii.WantMobil := FWantMobil;
  ii.WantCache := FWantCache;
  ii.WantCategory := FWantCategory;
  ii.WantJson := FWantJson;
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

  { Race }
  if WantRace then
  begin
    TabRace := TRaceTab.Create(self);
    tsRace := InitTabSheet(PageControl, tsRace, 'Race');
    TabRace.Parent := tsRace;
    TabRace.InitGrid;
    TabRace.OnUpdateIT := UpdateIT;
    TabRace.OnUpdateRace := UpdateRace;
  end
  else
    FreeAndNil(tsRace);

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

  { Timing }
  if WantTiming then
  begin
    TabTiming := TTimingTab.Create(self);
    tsTiming := InitTabSheet(SouthContainer, tsTiming, 'Timing');
    TabTiming.Parent := tsTiming;
    TabTiming.InitTiming;
  end
  else
    FreeAndNil(tsTiming);

  { Cache }
  if WantCache then
  begin
    TabCache := TCacheTab.Create(self);
    tsCache := InitTabSheet(PageControl, tsCache, 'Cache');
    TabCache.Parent := tsCache;
    TabCache.DisplayTurbo := False;
    GM.InitCache;
  end
  else
    FreeAndNil(tsCache);

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

  { Json }
  if WantJson then
  begin
    TabJson := TJsonTab.Create(self);
    tsJson := InitTabSheet(PageControl, tsJson, 'Json');
    TabJson.Parent := tsJson;
    TabJson.Align := alClient;
    TabJson.InitGrid;
  end
  else
    FreeAndNil(tsJson);

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

  { Browser }
  if WantBrowser then
  begin
    TabBrowser := TBrowserTab.Create(self);
    tsBrowser := InitTabSheet(PageControl, tsBrowser, 'Browser');
    TabBrowser.Parent := tsBrowser;
    TabBrowser.UpdateBrowserOptions;
  end
  else
    FreeAndNil(tsBrowser);

  { Web }
  if WantWeb then
  begin
    TabWeb := TWebTab.Create(self);
    tsWeb := InitTabSheet(PageControl, tsWeb, 'Web');
    TabWeb.Parent := tsWeb;
    TabWeb.Align := alClient;
    TabWeb.Init;
  end
  else
    FreeAndNil(tsWeb);

  {  Course }
  if WantCourse then
  begin
    TabCourse := TCourseTab.Create(self);
    tsCourse := InitTabSheet(NorthContainer, tsCourse, 'Course');
    TabCourse.Parent := tsCourse;
  end
  else
    FreeAndNil(tsCourse);

  { Mark }
  if WantMark then
  begin
    TabMark := TMarkTab.Create(self);
    tsMark := InitTabSheet(WestContainer, tsMark, 'Mark');
    TabMark.Parent := tsMark;
    TabMark.InitTiming;
    TabMark.InitTimepoint;
  end
  else
    FreeAndNil(tsMark);

  { Roundings }
  if WantRoundings then
  begin
    TabRoundings := TRoundingsTab.Create(self);
    tsRoundings := InitTabSheet(PageControl, tsRoundings, 'Roundings');
    TabRoundings.Parent := tsRoundings;
    TabRoundings.Align := alClient;
    TabRoundings.InitGrid;
  end
  else
    FreeAndNil(tsRoundings);

  { Listing }
  if WantListing then
  begin
    TabListing := TListingTab.Create(self);
    tsListing := InitTabSheet(PageControl, tsListing, 'Listing');
    TabListing.Parent := tsListing;
    TabListing.Align := alClient;
    TabListing.InitGrid;
  end
  else
    FreeAndNil(tsListing);
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
  if WantJson then
    TabJson.DisposeGrid;

  DisposePages;

  FTestMemo.Free;
  FStatusMemo.Free;
  WP.Free;
end;

procedure TFormFR62.DisposePages;
begin
  TabEntries.Free;
  TabEntries := nil;

  TabRace.Free;
  TabRace := nil;

  TabEvent.Free;
  TabEvent := nil;

  TabReport.Free;
  TabReport := nil;

  TabCategory.Free;
  TabCategory := nil;

  TabJson.Free;
  TabJson := nil;

  TabTiming.Free;
  TabTiming := nil;

  TabRacing.Free;
  TabRacing := nil;

  TabKeying.Free;
  TabKeying := nil;

  TabCache.Free;
  TabCache := nil;

  TabBrowser.Free;
  TabBrowser := nil;

  TabMenu.Free;
  TabMenu := nil;

  TabWorkspace.Free;
  TabWorkspace := nil;

  TabMobil.Free;
  TabMobil := nil;

  TabWeb.Free;
  TabWeb := nil;

  TabCourse.Free;
  TabCourse := nil;

  TabMark.Free;
  TabMark := nil;

  TabRoundings.Free;
  TabRoundings := nil;

  TabListing.Free;
  TabListing := nil;
end;

procedure TFormFR62.SetupDoubleBufferControls;
begin
  UseDoubleBufferForGrid := True;
  ToolBar.DoubleBuffered := True;

  NorthContainer.DoubleBuffered := True;
  PageControl.DoubleBuffered := True;
  SouthContainer.DoubleBuffered := True;
end;

procedure TFormFR62.DisableConnectionItems;
begin
  ConnectItem.Enabled := false;
  DisconnectItem.Enabled := false;
end;

procedure TFormFR62.EnableConnectionItems;
begin
  ConnectItem.Enabled := true;
  DisconnectItem.Enabled := true;
end;

procedure TFormFR62.EnableLogItemClick(Sender: TObject);
begin
  if Assigned(Main.Logger.OnTrace) then
  begin
    Main.Logger.OnTrace := nil;
    if WantReport then
    begin
       StatusMemo.Clear;
    end;
  end
  else
  begin
    Main.Logger.OnTrace := Trace;
    if WantReport then
    begin
      PageControl.ActivePage := tsReport;
      TabReport.TestMemo.Visible := False;
      StatusMemo.Clear;
    end;
  end;
end;

procedure TFormFR62.DisableBridgeItems;
begin
  PlugInItem.Enabled := false;
  PlugOutItem.Enabled := false;
  SynchronizeItem.Enabled := false;
  UploadItem.Enabled := false;
end;

procedure TFormFR62.DisableLocalAccess;
begin
  FileOpenItem.Enabled := false;
  OpenByNameItem.Enabled := false;
  SaveItem.Enabled := false;
  SaveAsItem.Enabled := false;
  FileDeleteItem.Enabled := false;
  BackupItem.Enabled := false;
  RestoreItem.Enabled := false;
  RecreateItem.Enabled := false;

  ContentItem.Enabled := false;
  WriteJSXMLItem.Enabled := false;
  WriteProxyXMLItem.Enabled := false;
  WriteResourcesItem.Enabled := false;

  BatchTestItem.Enabled := false;
  BatchDownloadItem.Enabled := false;
  BatchReportItem.Enabled := false;

  PublishAllItem.Enabled := false;

  if WantEntries then
    TabEntries.DisableLocalAccess;

  UnicodeScannerItem.Enabled := FrmUnicodeScanner.IsScannerEnabled;
end;

procedure TFormFR62.DisableWebAccess;
begin
  DownloadItem.Enabled := false;
end;

procedure TFormFR62.DisablePublishItems;
begin
  if Assigned(Main.AdapterBO) then
    PublishSocketsItem.Enabled := false;
  if Main.PolicyProvider.IsActive then
    PublishPolicyServerItem.Enabled := false;

  if GM.UseWeb then
  begin
  PublishHomeWebItem.Checked := GM.WebMotorHome.IsOnline;
  PublishRemoteWebItem.Checked := GM.WebMotorRemote.IsOnline;
  PublishSilverlightWebItem.Checked := GM.SilverlightWeb.IsOnline;
  end
  else
  begin
    PublishHomeWebItem.Enabled := false;
    PublishRemoteWebItem.Enabled := false;
    PublishSilverlightWebItem.Enabled := false;
  end;
  DownloadItem.Enabled := false;
end;

procedure TFormFR62.UpdateConnectionItems;
begin
  if Main.AdapterBO <> nil then
  begin
    EnableConnectionItems;

    //StatusPanel cannot be disabled and does not have Tooltips...
    if not Main.AdapterBO.InputServer.Server.IsDummy then
      StatusBar.Panels[StatusPanelPortIn].Text := IntToStr(Main.IniImage.PortIn)
    else
    begin
      DisableConnectionItems;
      StatusBar.Panels[StatusPanelPortIn].Text := '';
    end;

    if not Main.AdapterBO.OutputServer.Server.IsDummy then
      StatusBar.Panels[StatusPanelPortOut].Text := IntToStr(Main.IniImage.PortOut)
    else
    begin
      DisableConnectionItems;
      StatusBar.Panels[StatusPanelPortOut].Text := '';
    end;

    if Assigned(Main.GuiManager.HomeRouter) then
    begin
      StatusBar.Panels[StatusPanelHomeSitePort].Text := IntToStr(Main.IniImage.WebServerHomePort);
    end;

    if Assigned(Main.GuiManager.RemoteRouter) then
    begin
      StatusBar.Panels[StatusPanelRemoteSitePort].Text := IntToStr(Main.IniImage.WebServerRemotePort);
    end;

  end;
end;

procedure TFormFR62.InitBridge;
begin
  Main.PeerController.Connect;
  Main.PeerController.OnBackup := HandleBackup;
end;

procedure TFormFR62.DisposeBridge;
begin
  Main.PeerController.OnBackup := nil;
  Main.PeerController.Disconnect;
end;

procedure TFormFR62.InitViews;
begin
  { interface method }
  if WantEvent then
    TabEvent.InitGrid;
  if WantRace then
    TabRace.InitGrid;
  if WantEntries then
    TabEntries.InitGrid;
  if WantTiming then
    TabTiming.InitTiming;
  if WantRacing then
    TabRacing.InitTiming;
  if WantKeying then
    TabKeying.InitTiming;
  if WantMobil then
  begin
    TabMobil.InitGrid;
    TabMobil.InitMobil;
  end;
  if WantMark then
    TabMark.InitTiming;;
  if WantRoundings then
    TabRoundings.InitGrid;
  if WantListing then
    TabListing.InitGrid;

  GM.InitCache;
  InitBridge;

  CopyRankItem.Enabled := Main.IniImage.CopyRankEnabled;

  Update; //to update display properly when using styles
end;

procedure TFormFR62.DisposeViews;
begin
   { interface method }
  if WantEvent then
    TabEvent.GB.GridUpdate.OnUpdateView := nil;
  if WantEntries then
    TabEntries.GB.GridUpdate.OnUpdateView := nil;
  if WantRace then
    TabRace.GB.GridUpdate.OnUpdateView := nil;
  if WantCache then
    TabCache.GB.GridUpdate.OnUpdateView := nil;
  if WantMobil then
    TabMobil.GB.GridUpdate.OnUpdateView := nil;

  DisposeBridge;
  GM.DisposeCache;

  if WantKeying then
    TabKeying.DisposeTiming;
  if WantRacing then
    TabRacing.DisposeTiming;
  if WantTiming then
    TabTiming.DisposeTiming;
  if WantEntries then
    TabEntries.DisposeGrid;
  if WantRace then
    TabRace.DisposeGrid;
  if WantEvent then
    TabEvent.DisposeGrid;
  if WantMobil then
    TabMobil.DisposeGrid;
  if WantMark then
    TabMark.DisposeTiming;
  if WantRoundings then
    TabRoundings.DisposeGrid;
  if WantListing then
    TabListing.DisposeGrid;

  CopyRankItem.Enabled := False;
end;

procedure TFormFR62.UpdateCaption;
begin
  { interface method }
  Caption := GM.AppName + ' - ' + DM.EventName;
end;

procedure TFormFR62.UpdateWorkspaceStatus;
begin
  { interface method }
  StatusBar.Panels[StatusPanelWorkspaceType].Text := Main.WorkspaceInfo.WorkspaceTypeName;
  //StatusBar.Panels[StatusPanelWorkspaceType].Text := IntToStr(Main.WorkspaceInfo.WorkspaceType);
  StatusBar.Panels[StatusPanelWorkspaceID].Text := IntToStr(Main.WorkspaceInfo.WorkspaceID);

  if Main.WorkspaceInfo.WorkspaceType = 5 then
    StatusBar.Panels[StatusPanelWorkspaceUrl].Text := Main.WorkspaceInfo.WorkspaceUrl
  else
    StatusBar.Panels[StatusPanelWorkspaceUrl].Text := '';
end;

function TFormFR62.GetCacheMotor: TCacheMotor;
begin
  result := GM.CacheMotor;
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
  else if WantRace and (PageControl.ActivePage = tsRace) then
    TabRace.GB.GridUpdate.DoOnIdle
  else if WantEntries and (PageControl.ActivePage = tsEntries) then
    TabEntries.GB.GridUpdate.DoOnIdle
  else if WantMobil and (PageControl.ActivePage = tsMobil) then
    TabMobil.DoOnIdle
  else if WantCache and (PageControl.ActivePage = tsCache) then
  begin
    if CacheMotor.CacheEnabled then
    begin
      if TabCache.DisplayTurbo then
      begin
        TabCache.GB.ColGrid.ShowData;
        //CacheMotor.Cache.Grid.ColGrid.ShowData; //-->100%, if Grid is visible
      end
      else
      begin
        TabCache.GB.GridUpdate.DoOnIdle;
      end;
    end;
  end;
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

function TFormFR62.GetIT: Integer;
begin
  result := GM.IT;
end;

function TFormFR62.GetRace: Integer;
begin
  result := GM.Race;
end;

procedure TFormFR62.UpdateIT(Sender: TObject);
var
  temp: Integer;
begin
  temp := IT;
  StatusBar.Panels[StatusPanelIT].Text :=  'IT' + IntToStr(temp);
end;

procedure TFormFR62.UpdateRace(Sender: TObject);
var
  temp: Integer;
begin
  temp := Race;
  StatusBar.Panels[StatusPanelRace].Text :=  'Race' + IntToStr(temp);
end;

procedure TFormFR62.HandleBackup(Sender: TObject; EventData: string);
begin
  GM.SwapEvent(EventData);
end;

procedure TFormFR62.InitNewBO;
begin
  { if BO is recreated, Views must immediately be reinitialized,
    because object references become invalid;
    in MDI all Views are destroyed, in SDI this is not possible }
  DisposeViews;
  InitViews;
end;

procedure TFormFR62.WMCopyData(var M: TMessage);
var
  fn: string;
begin
  if PCopyDataStruct(M.LParam)^.dwData = DWORD(MI_MsgID) then
  begin
    fn := PChar(PCopyDataStruct(M.LParam)^.lpData);
    CheckFileParam(fn);
  end;
end;

procedure TFormFR62.PaintLED;
begin
  if BO.Connected then
  begin
    //LED.Brush.Color := clBtnFace;
    StatusBar.Panels[StatusPanelConnectStatus].Text := 'Connected';
  end
  else
  begin
    //LED.Brush.Color := clGray;
    StatusBar.Panels[StatusPanelConnectStatus].Text := 'Disconnected';
  end;

  if Main.PeerController.Connected then
  begin
    //PlugLED.Brush.Color := clHellGruen;
    StatusBar.Panels[StatusPanelPlugStatus].Text := 'Plugged';
  end
  else
  begin
    //PlugLED.Brush.Color := clHellRot;
    StatusBar.Panels[StatusPanelPlugStatus].Text := 'Unplugged';
  end;

  if WantCache then
    TabCache.UpdateLED;
end;

procedure TFormFR62.FileOpenItemClick(Sender: TObject);
var
  s: string;
begin
  s := DM.DocDownload;
  if (s <> '') then
    GM.SwapEvent(s);
end;

procedure TFormFR62.OpenByNameItemClick(Sender: TObject);
var
  s: string;
  en: string;
begin
  en := InputBox('Download Event Data', 'please enter Event Name', '');
  s := DM.DocDownloadByName(en);
  if (s <> '') then
    GM.SwapEvent(s);
end;

procedure TFormFR62.SaveItemClick(Sender: TObject);
begin
  DM.DocSave;
  BO.UndoManager.Clear;
end;

procedure TFormFR62.SaveAsItemClick(Sender: TObject);
begin
  DM.DocSaveAs;
  UpdateCaption;
  BO.UndoManager.Clear;
end;

procedure TFormFR62.FileDeleteItemClick(Sender: TObject);
begin
  DM.DocDelete;
end;

procedure TFormFR62.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFR62.ActionsMenuClick(Sender: TObject);
begin
  PluginItem.Enabled := Main.PeerController.IsEnabled(SwitchOp_Plugin);
  PlugoutItem.Enabled := Main.PeerController.IsEnabled(SwitchOp_Plugout);
  SynchronizeItem.Enabled := Main.PeerController.IsEnabled(SwitchOp_Synchronize);
  UploadItem.Enabled := Main.PeerController.IsEnabled(SwitchOp_Upload);
  DownloadItem.Enabled := Main.PeerController.IsEnabled(SwitchOp_Download);
end;

procedure TFormFR62.ConnectItemClick(Sender: TObject);
begin
  BO.Connect;
end;

procedure TFormFR62.DisconnectItemClick(Sender: TObject);
begin
  BO.Disconnect;
end;

procedure TFormFR62.AboutItemClick(Sender: TObject);
begin
  TestMemo.Clear;
  TestMemo.Add('Main.StartupLogger.Text');
  Inc(InfoClicks);

  StatusMemo.Text := Main.StartupLogger.Text;
  PageControl.ActivePage := tsReport;
end;

procedure TFormFR62.acBackupExecute(Sender: TObject);
begin
  GM.BackupExecute(Sender);
end;

procedure TFormFR62.AccessToggleClick(Sender: TObject);
begin
  if Main.Params.UserLevel = 1 then
    Main.Params.UserLevel := 0
  else
    Main.Params.UserLevel := 1;
end;

procedure TFormFR62.acRecreateExecute(Sender: TObject);
begin
  GM.RecreateExecute(Sender);
end;

procedure TFormFR62.acRestoreExecute(Sender: TObject);
begin
  GM.RestoreExecute(Sender);
end;

procedure TFormFR62.acClearExecute(Sender: TObject);
begin
  GM.ClearExecute(Sender);
end;

procedure TFormFR62.PlugInItemClick(Sender: TObject);
begin
  Main.PeerController.Plugin;
end;

procedure TFormFR62.PlugOutItemClick(Sender: TObject);
begin
  Main.PeerController.Plugout;
end;

procedure TFormFR62.SynchronizeItemClick(Sender: TObject);
begin
  Main.PeerController.Synchronize;
end;

procedure TFormFR62.UploadItemClick(Sender: TObject);
begin
  Main.PeerController.Upload(BO.Save);
end;

procedure TFormFR62.DownloadItemClick(Sender: TObject);
var
  s: string;
begin
  s := Main.PeerController.Download;
  GM.SwapEvent(s);
end;

procedure TFormFR62.ProviderMenuClick(Sender: TObject);
begin
  SwitchPropsItem.Enabled := Main.IniImage.BridgeProvider > 0;

  if Main.IsClientBridge then
  begin
    SwitchPropsItem.Enabled := true;
    SwitchPropsItem.Caption := 'Client-Bridge Properties...'
  end
  else if Main.IsServerBridge then
  begin
    SwitchPropsItem.Enabled := false;
    SwitchPropsItem.Caption := 'Server-Bridge Properties...'
  end
  else if Main.PeerController is TSwitchController then
    SwitchPropsItem.Caption := 'Switch Properties...'
  else if Main.PeerController is TBridgeController then
    SwitchPropsItem.Caption := 'Bridge Properties...'
  else if Main.PeerController is TOutputController then
    SwitchPropsItem.Caption := 'Output Properties...'
  else
    SwitchPropsItem.Caption := 'Mock Properties...';
end;

procedure TFormFR62.ScoringModuleItemClick(Sender: TObject);
begin
  if BO.CalcEV.EditScoringModule then
  begin
    EndEdit(epScoring, nil);
  end;
end;

procedure TFormFR62.DBInterfaceItemClick(Sender: TObject);
begin
  if DM.EditDBEvent then
  begin
    EndEdit(epDB, nil);
    StatusBar.Panels[StatusPanelDAL].Text := Main.DocManager.DBInterface;
  end;
end;

procedure TFormFR62.BridgeProviderItemClick(Sender: TObject);
begin
  DisposeBridge;
  Main.PeerManager.EditBridgeProvider;
  InitBridge;
end;

procedure TFormFR62.SwitchPropsItemClick(Sender: TObject);
begin
  Main.PeerController.EditProps;
end;

procedure TFormFR62.MainParamsItemClick(Sender: TObject);
begin
  InspectProperties(
    Main.Params.InspectorOnLoad,
    Main.Params.InspectorOnSave);

  //EndEdit(epIni, nil);
end;

procedure TFormFR62.IniPropsItemClick(Sender: TObject);
begin
  InspectProperties(
    Main.IniImage.InspectorOnLoad,
    Main.IniImage.InspectorOnSave);

  EndEdit(epIni, nil);
end;

procedure TFormFR62.WorkspaceInfoItemClick(Sender: TObject);
var
  f: TFormWorkspaceInfo;
begin
  f := TFormWorkspaceInfo.Create(nil);
  try
    f.WorkspaceInfo := Main.WorkspaceInfo;
    if f.ShowModal = mrOK then
    begin
      Main.WorkspaceInfo.Assign(f.WorkspaceInfo);
      Main.StoreAdapter.SwapWorkspace(Main.WorkspaceInfo);
    end;
  finally
    f.Free;
  end;
end;

procedure TFormFR62.WorkspaceLocationItemClick(Sender: TObject);
var
  f: TFormWorkspace;
begin
  f := TFormWorkspace.Create(nil);
  try
    f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure TFormFR62.OptionsMenuClick(Sender: TObject);
begin
  //
end;

procedure TFormFR62.EventParamsItemClick(Sender: TObject);
var
  BOParams: TBOParams;
begin
  BOParams := TBOParams.Create;
  BOParams.Assign(BO.BOParams);
  if EditEventParams(BOParams) then
  begin
    EndEdit(epParams, BOParams);
  end;
  BOParams.Free;
end;

procedure TFormFR62.RegattaPropsItemClick(Sender: TObject);
begin
  if BO.EventProps.EditRegattaProps then
  begin
    EndEdit(epRegatta, nil);
  end;
end;

procedure TFormFR62.UnicodeScannerItemClick(Sender: TObject);
var
  f: TFormUnicodeScanner;
begin
  if Main.WorkspaceInfo.WorkspaceType = 1 then
  begin
    f := TFormUnicodeScanner.Create(nil);
    try
      f.ShowModal;
    finally
      f.Free;
    end;
  end;
end;

procedure TFormFR62.UniquaPropsItemClick(Sender: TObject);
var
  oldShowUniquaColumn: Boolean;
  refreshFlag: Boolean;
begin
  oldShowUniquaColumn := BO.EventProps.ShowCupColumn;
  if BO.EventProps.EditUniquaProps then
  begin
    refreshFlag := false;
    if oldShowUniquaColumn <> BO.EventProps.ShowCupColumn then
    begin
      refreshFlag := true;
    end;
    EndEdit(epUniqua, nil, refreshFlag);
  end;
end;

procedure TFormFR62.FleetPropsItemClick(Sender: TObject);
begin
  if BO.EventProps.EditFleetProps then
  begin
    EndEdit(epFleet, nil);
  end;
end;

procedure TFormFR62.EventPropsItemClick(Sender: TObject);
var
  oldWantDiffColumns: Boolean;
  oldShowPLZColumn: Boolean;
  oldShowPosRColumn: Boolean;
  oldNameFieldCount: Integer;
  oldNameFieldOrder: string;
  refreshFlag: Boolean;
begin
  oldWantDiffColumns := BO.EventBO.WantDiffCols;
  oldShowPLZColumn := BO.EventProps.ShowPLZColumn;
  oldShowPosRColumn := BO.EventProps.ShowPosRColumn;
  oldNameFieldCount := StrToInt(BO.EventProps.NameFieldCount);
  oldNameFieldOrder := BO.EventProps.NameFieldOrder;

  InspectProperties(
    BO.EventProps.InspectorOnLoad,
    BO.EventProps.InspectorOnSave);

  refreshFlag := false;
  if (oldShowPLZColumn <> BO.EventProps.ShowPLZColumn)
    or (oldShowPosRColumn <> BO.EventProps.ShowPosRColumn)
    or (oldNameFieldCount <> StrToInt(BO.EventProps.NameFieldCount))
    or (oldNameFieldOrder <> BO.EventProps.NameFieldOrder)
    or (oldWantDiffColumns <> BO.EventBO.WantDiffCols)
    then
  begin
    refreshFlag := true;
  end;

  EndEdit(epEvent, nil, refreshFlag);
end;

procedure TFormFR62.NamePropsItemClick(Sender: TObject);
begin
  if EditNameFields(BO.StammdatenNode) then
  begin
    EndEdit(epNames, nil);
  end;
end;

procedure TFormFR62.AllOptionsItemClick(Sender: TObject);
begin
  if not Assigned(FormContainer) then
  begin
    FormContainer := TFormContainer.Create(Application);
    FormContainer.OnOK := PanelOK;
  end;
  FormContainer.ShowModal;
end;

procedure TFormFR62.ToolsMenuClick(Sender: TObject);
var
  b: Boolean;
begin
  CopyRankItem.Enabled := (BO.EventNode <> nil) and Main.IniImage.CopyRankEnabled;
  b := (BO.EventNode <> nil) and (BO.EventNode.UseFleets);
  InitFleetItem.Enabled := b;
  InitFleetFromFinishItem.Enabled := b;
  CopyFleetItem.Enabled := b;
  DisableFleetItem.Enabled := b;
end;

procedure TFormFR62.CalcItemClick(Sender: TObject);
begin
  BO.EventNode.Modified := True;
  BO.Calc;
end;

procedure TFormFR62.PartialCalcItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    if r <> -1 then
    begin
      BO.EventNode.PartialCalc(r);
    end;
  end;
end;

procedure TFormFR62.ExportItemClick(Sender: TObject);
var
  o: TExcelExporter;
  Memo: TMemo;
begin
  if WantReport then
  begin
    o := TExcelExporter.Create;
    o.Delimiter := #9;
    try
      try
        PageControl.ActivePage := tsReport;
        Memo := TabReport.StatusMemo;
        Memo.Text := o.GetString(TableID_ResultList);
        Memo.SelectAll;
        Memo.CopyToClipboard;
        Memo.PasteFromClipboard;
      except
      end;
    finally
      o.Free;
    end;
  end;
end;

procedure TFormFR62.ImportItemClick(Sender: TObject);
begin
  if not Assigned(FormExcelImport) then
    FormExcelImport := TFormExcelImport.Create(Application);
  FormExcelImport.ShowModal;
end;

procedure TFormFR62.CopyRankItemClick(Sender: TObject);
begin
  CopyRank;
end;

procedure TFormFR62.SelectRaceItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    Main.GuiManager.Race := r;
  end;
end;

procedure TFormFR62.ClearRaceItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    BO.ClearRaceCommand(r);
  end;
end;

procedure TFormFR62.GoBackToRaceItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex + 1;
    Main.GuiManager.Race := r;
    BO.GoBackToRaceCommand(r);
  end;
end;

procedure TFormFR62.InitFleetFromFinishItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    if r <> -1 then
    begin
      BO.EventNode.InitFleetByFinishHack(r);
    end;
  end;
end;

procedure TFormFR62.InitFleetItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    if r <> -1 then
    begin
      BO.EventNode.InitFleet(r);
    end;
  end;
end;

procedure TFormFR62.CopyFleetItemClick(Sender: TObject);
var
  r: Integer;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    if r <> -1 then
    begin
      BO.EventNode.CopyFleet(r);
    end;
  end;
end;

procedure TFormFR62.DisableFleetItemClick(Sender: TObject);
var
  r: Integer;
  cb: TBaseRowCollectionItem;
  cr: TEventRowCollectionItem;
  ere: TEventRaceEntry;
begin
  if WantEvent then
  begin
    r := TabEvent.GetSelectedRaceIndex;
    if r <> -1 then
    begin
      cb := TabEvent.GB.ColGrid.GetRowCollectionItem(TabEvent.GB.Grid.Selection.Top);
      if Assigned(cb) and (cb is TEventRowCollectionItem) then
      begin
        cr := TEventRowCollectionItem(cb);
        ere := cr.Race[r];
        if Assigned(ere) then
          BO.EventNode.DisableFleet(r, ere.Fleet, not ere.IsRacing);
      end;
    end;
  end;
end;

procedure TFormFR62.TestMenuClick(Sender: TObject);
begin
  if Assigned (WriteProxyXmlItem.OnClick) then
  WriteProxyXmlItem.Enabled := BO.CalcEV.UsesProxy;
end;

procedure TFormFR62.LoadTestDataItemClick(Sender: TObject);
begin
  GM.SwapEvent(Main.TestData);
end;

procedure TFormFR62.SaveConfigItemClick(Sender: TObject);
begin
  UpdateWant;
  Main.IniImage.ForceWrite;
end;

procedure TFormFR62.WriteJSXmlItemClick(Sender: TObject);
begin
  if WantReport then
  begin
    if Main.HaveFilePermission then
      BO.JavaScoreXML.WriteXML;
    BO.JavaScoreXML.GetXML(TabReport.StatusMemo.Lines);
    PageControl.ActivePage := tsReport;
  end;
end;

procedure TFormFR62.WriteRDXmlItemClick(Sender: TObject);
begin
  if WantReport then
  begin
    if Main.HaveFilePermission then
      BO.RaceDataXML.WriteXML;
    BO.RaceDataXML.GetXML(TabReport.StatusMemo.Lines);
    PageControl.ActivePage := tsReport;
  end;
end;

procedure TFormFR62.WriteResourcesItemClick(Sender: TObject);
begin
  Main.ResourceManager.ExportAll;
end;

procedure TFormFR62.WriteProxyXmlItemClick(Sender: TObject);
begin
  BO.CalcEV.Proxy.WithTest := True;
  BO.EventNode.Calc;
  PageControl.ActivePage := tsReport;
end;

procedure TFormFR62.InspectReportsItemClick(Sender: TObject);
begin
  if not Assigned(FormReportNav) then
  begin
    FormReportNav := TFormReportNav.Create(Application);
  end;
  FormReportNav.ShowModal;
  FormReportNav.Free;
  FormReportNav := nil;
end;

procedure TFormFR62.ShowScheduleItemClick(Sender: TObject);
begin
  BO.JavaScoreXML.ShowSchedule;
end;

procedure TFormFR62.ShowUndoListsItemClick(Sender: TObject);
begin
  if not Assigned(FormUndoManager) then
  begin
    FormUndoManager := TFormUndoManager.Create(Application);
    FormUndoManager.UndoManager := BO.UndoManager;
  end;
  FormUndoManager.ShowModal;
  FormUndoManager.Free;
  FormUndoManager := nil;
end;

procedure TFormFR62.ShowWatchesItemClick(Sender: TObject);
begin
  BO.LocalWatches.SetWatchGUI(Main.FormAdapter.GetWatchGUI);
  BO.LocalWatches.Show(BO.EventProps.EventName);
end;

procedure TFormFR62.ShowWorkspaceFilesItemClick(Sender: TObject);
begin
  if not Assigned(FormWorkspaceFiles) then
  begin
    FormWorkspaceFiles := TFormWorkspaceFiles.Create(nil);
    FormWorkspaceFiles.Show;
    //FormWorkspaceFiles.Free; //keep in memory
  end
  else
    FormWorkspaceFiles.BringToFront;
end;

procedure TFormFR62.TestPenaltyItemClick(Sender: TObject);
begin
  EditPenalty(nil);
end;

procedure TFormFR62.PublishAllItemClick(Sender: TObject);
begin
  Main.Publisher.PublishAll;
end;

procedure TFormFR62.PublishHomeWebItemClick(Sender: TObject);
begin
  if Assigned(GM.WebMotorHome) then
  begin
    GM.WebMotorHome.IsOffline := not GM.WebMotorHome.IsOffline;;
    PublishHomeWebItem.Checked := GM.WebMotorHome.IsOnline;
//    if WantWeb and (TabWeb <> nil) then
//      TabWeb.UpdateFormStatus;
  end;
end;

procedure TFormFR62.PublishMenuClick(Sender: TObject);
begin
  PublishHomeWebItem.Checked := GM.WebMotorHome.IsOnline;
  PublishRemoteWebItem.Checked := GM.WebMotorRemote.IsOnline;
  PublishSilverlightWebItem.Checked := GM.SilverlightWeb.IsOnline;
end;

procedure TFormFR62.PublishRemoteWebItemClick(Sender: TObject);
begin
  if Assigned(GM.WebMotorRemote) then
  begin
    GM.WebMotorRemote.IsOffline := not GM.WebMotorRemote.IsOffline;
    PublishRemoteWebItem.Checked := GM.WebMotorRemote.IsOnline;
    if WantWeb and (TabWeb <> nil) then
      TabWeb.UpdateFormStatus;
  end;
end;

procedure TFormFR62.PublishSilverlightWebItemClick(Sender: TObject);
begin
  if Assigned(GM.SilverlightWeb) then
  begin
    GM.SilverlightWeb.IsOffline := not GM.SilverlightWeb.IsOffline;
    PublishSilverlightWebItem.Checked := GM.SilverlightWeb.IsOnline;
    if WantWeb and (TabWeb <> nil) then
      TabWeb.UpdateFormStatus;
  end;
end;

procedure TFormFR62.PublishPolicyServerItemClick(Sender: TObject);
begin
  Main.InitPolicyProvider;
  PublishPolicyServerItem.Enabled := false;
end;

procedure TFormFR62.PublishSocketsItemClick(Sender: TObject);
begin
  if not Assigned(Main.AdapterBO) then
  begin
    Main.IniImage.WantSockets := True;
    if C_WantAdapter and Main.IniImage.WantSockets then
    begin
      Main.BOManager.CreateAdapterBO;
      UpdateConnectionItems;
    end;
    PublishSocketsItem.Enabled := false;
  end;
end;

procedure TFormFR62.BatchDownloadItemClick(Sender: TObject);
begin
  ProcessBatch(BatchID_Download);
end;

procedure TFormFR62.BatchTestItemClick(Sender: TObject);
begin
  ProcessBatch(BatchID_Test);
end;

procedure TFormFR62.BatchReportItemClick(Sender: TObject);
begin
  ProcessBatch(BatchID_Report);
end;

procedure TFormFR62.HelpMenuClick(Sender: TObject);
begin
  EnableLogItem.Checked := Assigned(Main.Logger.OnTrace);
  SoundItem.Checked := Main.SoundManager.Enabled;
  EventMenuItem.Checked := tsMenu.Visible;
  AccessToggle.Checked := Main.Params.UserLevel = 1;
  LanguageToggle.Checked := Main.LanguageManager.Language = 'de';
end;

procedure TFormFR62.ContentItemClick(Sender: TObject);
begin
  if not Assigned(FormHelp) then
    FormHelp := TFormHelp.Create(Application);
  FormHelp.Show;
end;

procedure TFormFR62.InfoItemClick(Sender: TObject);
begin
  FrmInfo.ShowInfo;
end;

procedure TFormFR62.LanguageItemClick(Sender: TObject);
begin
  if Main.LanguageManager.SelectLanguage then
    Main.LanguageManager.Localize(self);
end;

procedure TFormFR62.LanguageToggleClick(Sender: TObject);
begin
  Main.LanguageManager.ToggleLanguage;
  Main.LanguageManager.Localize(self);
end;

procedure TFormFR62.SoundItemClick(Sender: TObject);
var
  snd: TSoundManager;
begin
  snd := Main.SoundManager;
  snd.Enabled := not snd.Enabled;
  if snd.Enabled then
    Main.GuiManager.PlaySound(Sound_Welcome);
end;

procedure TFormFR62.EventMenuItemClick(Sender: TObject);
begin
  if tsMenu <> nil then
    tsMenu.Visible := not tsMenu.Visible;
end;

procedure TFormFR62.NorthContainerItemClick(Sender: TObject);
begin
  Main.IniImage.NorthContainerVisible := not Main.IniImage.NorthContainerVisible;
  ToggleEventMenu;
end;

procedure TFormFR62.SouthContainerItemClick(Sender: TObject);
begin
  Main.IniImage.SouthContainerVisible := not Main.IniImage.SouthContainerVisible;
  ToggleTimingPanel;
end;

procedure TFormFR62.PanelOK(Sender: TObject);
var
  f: TFormContainer;
begin
  f := Sender as TFormContainer;
  if f <> nil then
  begin
    EndEdit(f.ActivePage, f.BOParams, f.RefreshFlag);
  end;
end;

procedure TFormFR62.EndEdit(ep: TEditPage; newParams: TBOParams; refreshFlag: Boolean = false);
begin
  case ep of
    epNone: ;

    epParams: GM.UpdateEventParams(
      newParams.RaceCount, newParams.ITCount, newParams.StartlistCount);

    epRegatta:
    begin
      BO.EventNode.Modified := True;
    end;

    epUniqua:
    begin
      BO.EventNode.Modified := True;
      if refreshFlag then
      begin
        if WantEvent then
        begin
          BO.EventBO.InitColsActive(TabEvent.GB.ColGrid);
          TabEvent.GB.ColGrid.UpdateAll;
        end;
      end;
    end;

    epFleet:
    begin
      BO.EventNode.Modified := True;
    end;

    epEvent:
    begin
      if refreshFlag then
      begin
        if WantEvent then
        begin
          BO.EventBO.InitColsActive(TabEvent.GB.ColGrid);
          TabEvent.GB.ColGrid.UpdateAll;
        end;
      end;
    end;

    epIni: ;

    epNames:
    begin;
      if WantEntries then
      begin
        TabEntries.DisposeGrid;
        TabEntries.InitGrid;
      end;
    end;

    epScoring:
    begin
      if BO.CalcEV.ScoringResult = true then
      begin
        //probe the calculation...
        BO.EventNode.Modified := True;
        BO.OnIdle;
        DoOnIdle;
        //check result, roll back if there is a problem
        if BO.CalcEV.ScoringResult = false then
        begin
          ShowMessage('Operation canceled, ScoringProvider returned error.' + #13#10 +
          BO.CalcEV.ScoringExceptionMessage + #13#10 +
          'next action after ok: InitModule(Inline_ScoringProvider)');
          BO.CalcEV.InitModule(2);
          BO.EventNode.Modified := True;
        end;
      end;
    end;

    epDB:
    begin
      if Main.HaveFilePermission then
      begin
        SaveItem.Enabled := DM.SaveEnabled;
        SaveAsItem.Enabled := DM.SaveAsEnabled;
        FileDeleteItem.Enabled := DM.DeleteEnabled;
      end;
      UnicodeScannerItem.Enabled := FrmUnicodeScanner.IsScannerEnabled;
    end;

  end;

end;

procedure TFormFR62.CheckFileParam(fn: string);
var
  s: string;
  en: string;
begin
  if (fn) <> '' then
  begin
    if ExtractFileExt(fn) = '.fr62' then
    begin
      if FileExists(fn) then
      begin
        en := ExtractFileName(fn);
        en := ChangeFileExt(en, '');
        Main.DocManager.InitDBInterface('LNK');
        //DBEventLNK extracts Dir and Ext from brutto FileName
        s := DM.DocDownloadByName(fn);
        //reset DM.EventName to netto FileName
        DM.EventName := en;
        if (s <> '') then
        begin
          GM.SwapEvent(s);
        end;
      end;
    end;
  end;
end;

procedure TFormFR62.ProcessBatch(BatchID: Integer);
var
  wasConnected: Boolean;
  needBackup: Boolean;
begin
  if not Main.HaveFilePermission then
    exit;

  wasConnected := False;

  //determine if Backup and Restore is needed
  case BatchID of
    BatchID_Download: needBackup := False;
    else
      needBackup := True;
  end;

  //Backup
  if needBackup then
  begin
    BO.Backup;
    BO.UndoManager.Clear;
    wasConnected := BO.Connected;
    DisposeViews;
  end;

  //Process Batch
  If FormBatchProcess = nil then
  begin
    FormBatchProcess := TFormBatchProcess.Create(self);
  end;
  FormBatchProcess.BatchID := BatchID;
  FormBatchProcess.ShowModal;

  //Restore
  if needBackup then
  begin
    Main.RecreateBOFromBackup;
    InitNewBO;
    if (wasConnected) then
      BO.Connect;
  end;
end;

procedure TFormFR62.CopyRank;
begin
  if Application.MessageBox(
    'no return, please confirm operation',
    'Copy Rank from RaceCollection',
    MB_OKCANCEL) = IDOK then
      BO.UpdateEventNode;
end;

procedure TFormFR62.DisableEditPages;
begin
//  Include(Main.Params.DisabledEditPages, epIni);
//  Include(Main.Params.DisabledEditPages, epScoring);
//  Include(Main.Params.DisabledEditPages, epDB);
end;

procedure TFormFR62.InitMenuItems;
var
  SaveEnabled: Boolean;
  FormEnabled: Boolean;
begin
  SaveEnabled := DM.SaveEnabled;
  FormEnabled := true;

  acBackup.Enabled :=  SaveEnabled;
  acRecreate.Enabled :=  SaveEnabled;
  acRestore.Enabled :=  SaveEnabled;
  acClear.Enabled :=  SaveEnabled;

  FileMenu.Enabled := true;
  FileOpenItem.Enabled := SaveEnabled;
  OpenByNameItem.Enabled := SaveEnabled and FormEnabled;
  SaveItem.Enabled := SaveEnabled;
  SaveAsItem.Enabled := DM.SaveAsEnabled;
  FileDeleteItem.Enabled := DM.DeleteEnabled;
  ExitItem.Enabled := true;

  ActionsMenu.Enabled := true;
  ConnectItem.Enabled := false;
  DisconnectItem.Enabled := false;
  BackupItem.Enabled := SaveEnabled;
  RestoreItem.Enabled := SaveEnabled;
  RecreateItem.Enabled := SaveEnabled;
  ClearItem.Enabled := SaveEnabled;
  PlugInItem.Enabled := false;
  PlugOutItem.Enabled := false;
  SynchronizeItem.Enabled := false;
  UploadItem.Enabled := false;
  DownloadItem.Enabled := false;

  if WantProviderMenu then
  begin
  ProviderMenu.Enabled := true;
  ScoringModuleItem.Enabled := true;
  DBInterfaceItem.Enabled := true;
  BridgeProviderItem.Enabled := true;
  WorkspaceLocationItem.Enabled := true;
  SwitchPropsItem.Enabled := true;
  IniPropsItem.Enabled := true;
  WorkspaceInfoItem.Enabled := true;
  end
  else
    ProviderMenu.Visible := False;

  OptionsMenu.Enabled := true;
  EventParamsItem.Enabled := true;
  RegattaPropsItem.Enabled := FormEnabled;
  FleetPropsItem.Enabled := true;
  UniquaPropsItem.Enabled := FormEnabled;
  NamePropsItem.Enabled := FormEnabled;
  EventPropsItem.Enabled := FormEnabled;
  AllOptionsItem.Enabled := FormEnabled;

  ToolsMenu.Enabled := true;
  CalcItem.Enabled := true;
  PartialCalcItem.Enabled := true;
  ExportItem.Enabled := true;
  ImportItem.Enabled := true;
  InitFleetItem.Enabled := true;
  InitFleetFromFinishItem.Enabled := true;
  CopyFleetItem.Enabled := true;
  DisableFleetItem.Enabled := true;
  CopyRankItem.Enabled := true;

  TestMenu.Enabled := true;
  LoadTestDataItem.Enabled := true;
  WriteJSXmlItem.Enabled := false;
  WriteRDXmlItem.Enabled := false;
  WriteProxyXmlItem.Enabled := false;
  ShowUndoListsItem.Enabled := true;
  ShowWatchesItem.Enabled := false;
  TestPenaltyItem.Enabled := true;
  TestJsonItem.Enabled := true;
  BatchProcessItem.Enabled := false;
  BatchTestItem.Enabled := false;
  BatchDownloadItem.Enabled := false;
  BatchReportItem.Enabled := false;
  ShowWorkspaceFilesItem.Enabled := false;
  WriteResourcesItem.Enabled := false;
  UnicodeScannerItem.Enabled := false;

  if WantPublishMenu then
  begin
  PublishMenu.Enabled := true;
  PublishAllItem.Enabled := true;
  PublishSocketsItem.Enabled := true;
  PublishPolicyServerItem.Enabled := true;
  PublishHomeWebItem.Enabled := true;
  PublishRemoteWebItem.Enabled := true;
  PublishSilverlightWebItem.Enabled := true;
  end
  else
    PublishMenu.Visible := False;

  HelpMenu.Enabled := true;
  ContentItem.Enabled := true;
  InfoItem.Enabled := true;
  LanguageItem.Enabled := true;
  SoundItem.Enabled := true;
  EventMenuItem.Enabled := true;
  NorthContainerItem.Enabled := true;
  SouthContainerItem.Enabled := true;
  StyleItem.Enabled := true;
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
      if WantMark then
        TabMark.InitTimePoint;
      if WantRoundings then
        TabRoundings.UpdateGrid;
    end;

    EventChanged:
    begin
      BO.Calc;
      if WantMobil then
        TabMobil.InitMobil;
      if WantMark then
        TabMark.InitTimePoint;
      if WantCourse then
        TabCourse.DoUpdateCourse;
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

    JsonGridChanged:
    begin
      if WantJson and (TabJson <> nil) then
        TabJson.UpdateBtnClick(nil);
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

    InitCacheGui:
    begin
      if WantCache then
        TabCache.InitGrid;
    end;

    DisposeCacheGui:
    begin
      if WantCache then
        TabCache.DisposeGrid;
    end;

    ClearAge:
    begin
      if WantTiming then
        TabTiming.AgeBtnClick(nil);
      if WantRacing then
        TabRacing.AgeBtnClick(nil);
    end;

    MarkChanged:
    begin
      if WantMark then
        TabMark.InitTimePoint;
    end;

    MarkCountChanged:
    begin
      if WantRoundings then
        TabRoundings.InitTimePoint;
    end;

    TimePointChanged:
    begin
      if WantRace then
        TabRace.InitTimePoint;
      if WantEvent then
        TabEvent.UpdateGrid;
      if WantMark then
        TabMark.InitTimePoint;
      if WantRoundings then
        TabRoundings.UpdateGrid;
    end;

    TimingDataChanged:
    begin
      if WantRace then
        TabRace.UpdateGrid;
      if WantRoundings then
      begin
        BO.RaceNode.Calc;
        TabRoundings.UpdateGrid;
      end;
    end;

    UpdateCurrent:
    begin
      if WantCourse then
        TabCourse.DoUpdateCourse;
      if WantRace then
      begin
        TabRace.UpdateRace;
        TabRace.UpdateIT;
      end;
    end;

    AutoSendChanged:
    begin
      if WantTiming then
        TabTiming.UpdateAuto;
    end;

    AutoUpdateEventChanged:
    begin
      if WantRacing then
        TabRacing.UpdateAuto;
      if WantKeying then
        TabKeying.UpdateAuto;
    end

    else
    begin
      if WantEvent then
        TabEvent.GB.GridUpdate.HandleInform;
      if WantEntries then
        TabEntries.GB.GridUpdate.HandleInform;
      if WantRace then 
        TabRace.GB.GridUpdate.HandleInform;
      if WantCache then
        TabCache.GB.GridUpdate.HandleInform;
    end;
  end;
end;

procedure TFormFR62.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  PaintLED;
  DoOnIdle;
end;

procedure TFormFR62.IdleTimerTimer(Sender: TObject);
begin
  PostMessage(Handle, wm_UpdateGrid, 0, 0);
  if WantTiming and (PageControl.ActivePage = tsTiming) then
  begin
    TabTiming.Model.TimerTick;
    TabTiming.TimingGrid.RePaint;
  end;
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
  if WantJson then
    TabJson.InitBtnText;
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
    WestBtn.Caption := GetText(MF_WestBtn_Caption);
    InfoBtn.Caption := GetText(MF_InfoBtn_Caption);
    TestBtn.Caption := GetText(MF_TestBtn_Caption);
    LanguageBtn.Caption := GetText(MF_LanguageBtn_Caption);
    FeatureBtn.Caption := GetText(MF_FeatureBtn_Caption);

    ClearBtn.Hint := GetText(MF_TestDataBtn_Hint);
    NorthBtn.Hint := GetText(MF_NorthBtn_Hint);
    SouthBtn.Hint := GetText(MF_SouthBtn_Hint);
    WestBtn.Hint := GetText(MF_WestBtn_Hint);
    InfoBtn.Hint := GetText(MF_InfoBtn_Hint);
    TestBtn.Hint := GetText(MF_TestBtn_Hint);
    LanguageBtn.Hint := GetText(MF_LanguageBtn_Hint);
    FeatureBtn.Hint := (MF_FeatureBtn_Hint);
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
  NorthBtn.Caption := 'North';
  SouthBtn.Caption := 'South';
  WestBtn.Caption := 'West';
  InfoBtn.Caption := 'Info';
  TestBtn.Caption := 'Test';
  LanguageBtn.Caption := 'Lang';
  FeatureBtn.Caption := 'Feature';

  ClearBtn.Hint := 'clear event data';
  NorthBtn.Hint := 'toggle visibility of north container';
  SouthBtn.Hint := 'toggle visibility of south container';
  WestBtn.Hint := 'toggle visibility of west container';
  InfoBtn.Hint := 'show info message in Memo';
  TestBtn.Hint := 'toggle betweeen default and custom style';
  LanguageBtn.Hint := 'toggle language for some elements';
  FeatureBtn.Hint := 'toggle additional features on/off';
end;

procedure TFormFR62.InitAlternativeBtnText;
begin
  ClearBtn.Caption := 'Clear';
  NorthBtn.Caption := 'Nord';
  SouthBtn.Caption := 'Süd';
  WestBtn.Caption := 'West';
  InfoBtn.Caption := 'Info';
  TestBtn.Caption := 'Test';
  LanguageBtn.Caption := 'Sprache';
  FeatureBtn.Caption := 'Feature';

  ClearBtn.Hint := 'Event-Daten löschen';
  NorthBtn.Hint := 'Nord-Contaier umschalten (PageControl oben)';
  SouthBtn.Hint := 'Süd-Container umschalten (PageControl unten)';
  WestBtn.Hint := 'West-Container umschalten (PageControl links)';
  InfoBtn.Hint := 'Info anzeigen (im Memo)';
  TestBtn.Hint := 'Test-Form anzeigen';
  LanguageBtn.Hint := 'Sprache umschalten';
  FeatureBtn.Hint := 'Funktionsumfang umschalten';
end;

procedure TFormFR62.ExportBtnText;
begin
  if WantCategory then
    TabCategory.ExportBtnText;
  if WantJson then
    TabJson.ExportBtnText;
  if WantWorkspace then
    TabWorkspace.ExportBtnText;
  if WantKeying then
    TabKeying.ExportBtnText;
  if WantEvent then
    TabEvent.ExportBtnText;
  if WantMenu then
    TabMenu.ExportBtnText;

  InitDefaultBtnText;

  SetText(MF_ClearBtn_Caption, ClearBtn.Caption);
  SetText(MF_NorthBtn_Caption, NorthBtn.Caption);
  SetText(MF_SouthBtn_Caption, SouthBtn.Caption);
  SetText(MF_WestBtn_Caption, WestBtn.Caption);
  SetText(MF_InfoBtn_Caption, InfoBtn.Caption);
  SetText(MF_TestBtn_Caption, TestBtn.Caption);
  SetText(MF_LanguageBtn_Caption, LanguageBtn.Caption);
  SetText(MF_FeatureBtn_Caption, FeatureBtn.Caption);

  SetText(MF_ClearBtn_Hint, ClearBtn.Hint);
  SetText(MF_NorthBtn_Hint, NorthBtn.Hint);
  SetText(MF_SouthBtn_Hint, SouthBtn.Hint);
  SetText(MF_WestBtn_Hint, WestBtn.Hint);
  SetText(MF_InfoBtn_Hint, InfoBtn.Hint);
  SetText(MF_TestBtn_Hint, TestBtn.Hint);
  SetText(MF_LanguageBtn_Hint, LanguageBtn.Hint);
  SetText(MF_FeatureBtn_Hint, FeatureBtn.Hint);
end;

procedure TFormFR62.ToggleEventMenu;
begin
  NorthContainer.Visible := not NorthContainer.Visible;
  NorthContainerItem.Checked := NorthContainer.Visible;
end;

procedure TFormFR62.ToggleTimingPanel;
begin
  SouthContainer.Visible := not SouthContainer.Visible;
  SouthContainerItem.Checked := SouthContainer.Visible;
end;

procedure TFormFR62.TestFormItemClick(Sender: TObject);
begin
  ShowTest;
end;

procedure TFormFR62.ClearBtnClick(Sender: TObject);
begin
  BO.ClearCommand;
  //GM.SwapEvent(Main.TestData);
end;

procedure TFormFR62.TestJsonItemClick(Sender: TObject);
begin
  EditJson;
end;

procedure TFormFR62.TestMessageParserItemClick(Sender: TObject);
begin
  ShowMsgParser2;
end;

procedure TFormFR62.NorthBtnClick(Sender: TObject);
begin
  ToggleEventMenu;
end;

procedure TFormFR62.SouthBtnClick(Sender: TObject);
begin
  ToggleTimingPanel;
end;

procedure TFormFR62.WestBtnClick(Sender: TObject);
begin
  WestContainer.Visible := not WestContainer.Visible;
end;

procedure TFormFR62.InfoBtnClick(Sender: TObject);
begin
  if WantReport then
  begin
    TabReport.TestMemoVisible := False;
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

procedure TFormFR62.TestBtnClick(Sender: TObject);
begin
  ShowTest;
end;

procedure TFormFR62.ToggleStyleBtnClick(Sender: TObject);
begin
  ToggleStyle;
end;

procedure TFormFR62.ToggleStyle;
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

procedure TFormFR62.ToggleRowHeight;
begin
  TDisplayGrid.WantSmallRows := not TDisplayGrid.WantSmallRows;
  DisposeViews;
  InitViews;
  if WantCategory then
  begin
    TabCategory.DisposeGrid;
    TabCategory.InitGrid;
  end;
  if WantJson then
  begin
    TabJson.DisposeGrid;
    TabJson.InitGrid;
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
begin
  ShowHelp;
end;

procedure TFormFR62.ShowHelp;
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

procedure TFormFR62.FeatureBtnClick(Sender: TObject);
begin
  ToggleFeatures;
end;

procedure TFormFR62.ToggleFeatures;
var
  b: Boolean;
begin
  if WantCategory then
    TabCategory.DisposeGrid;

  DisposePages;

  Main.Params.WantFeatures := not Main.Params.WantFeatures;
  b := Main.Params.WantFeatures;
  //b := Main.HaveAppPermission;

  InitWant;

  //toggle a group of feature-tabs
  FWantCategory := b;
  FWantMobil := b;
  FWantMark := b;
  FWantListing := b;
  FWantRoundings := b;

  InitPages;
end;

procedure TFormFR62.PlusBtnClick(Sender: TObject);
begin
  BO.UpdateStartlistCount('', BO.BOParams.StartlistCount + 1);
  HandleNewStartlistCount;
end;

procedure TFormFR62.MinusBtnClick(Sender: TObject);
begin
  if BO.BOParams.StartlistCount > 2 then
  begin
    BO.UpdateStartlistCount('', BO.BOParams.StartlistCount - 1);
    HandleNewStartlistCount;
  end;
end;

procedure TFormFR62.HandleNewStartlistCount;
begin
  BO.RNode[Main.GuiManager.Race].Modified := True;
  BO.Calc;
  if WantEvent then
    TabEvent.UpdateGrid;
  if WantRace then
    TabRace.UpdateGrid;
  if WantRacing then
    TabRacing.UpdateGrid;
  if WantMark then
    TabMark.InitTimePoint;
  if WantRoundings then
    TabRoundings.UpdateGrid;
  if WantListing then
    TabListing.UpdateGrid;
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

procedure TFormFR62.InitButtons;
begin
  TabEntries.LoadBtn.Enabled := false;
  TabEntries.SaveBtn.Enabled := false;

  TabReport.IniBtn.Enabled := false;
  TabReport.XmlBtn.Enabled := false;
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

procedure TFormFR62.PagesItemClick(Sender: TObject);
begin
  if WP = nil then
    WP := TWantPages.Create;

  LoadWP;
  if EditPages(WP) then
  begin
    RecreatePages;
  end;
end;

procedure TFormFR62.Trace(Sender: TObject; s: string);
var
  t: string;
begin
  if WantReport then
  begin
    //t := FormatDateTime('hh:nn:ss.zzz', Now);
    t := FormatDateTime('nn:ss.zzz', Now);
    StatusMemo.Add(t + ' ' + s);
    while StatusMemo.Count > 12 do
    begin
      StatusMemo.Delete(0);
    end;
  end;

end;

end.
