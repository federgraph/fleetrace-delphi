unit FrmMainRes;

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
  System.IniFiles,
  Vcl.Forms,
  RiggVar.BO.Localizer,
  FrmMain;

type
  TFR62Res = class(TBaseLocalizer)
  protected
    procedure InitDefault; override;
    procedure Init_de; override;
    procedure Init_en; override;
    procedure LocalizeFormData(af: TFormFR62);
    procedure ExtractFormData(af: TFormFR62);
  public
    TsAthletesCaption: string;
    TbAthletesCaption: string;
    LoadAthletesBtnCaption: string;
    SaveAthletesBtnCaption: string;
    AddAthleteBtnCaption: string;
    UpdateAthletesBtnCaption: string;
    ClearAthletesBtnCaption: string;

    TsTimingCaption: string;
    TbTimingCaption: string;
    TimingConfigBtnCaption: string;
    TimingTestBtnHint: string;
    TimingTestBtnCaption: string;
    TimingRandomBtnHint: string;
    TimingRandomBtnCaption: string;
    TimingResetAgeBtnHint: string;
    TimingResetAgeBtnCaption: string;
    TimingSendBtnCaption: string;
    AutoSendBtnCaption: string;

    TsRaceCaption: string;
    TbRaceCaption: string;
    RaceDownBtnCaption: string;
    RaceUpBtnCaption: string;
    ITDownBtnCaption: string;
    ITBtnCaption: string;
    ITUpBtnCaption: string;
    ToMRankBtnCaption: string;
    FromMRankBtnCaption: string;
    UpdateEventBtnCaption: string;

    TsEventCaption: string;
    TbEventCaption: string;
    PointsBtnHint: string;
    PointsBtnCaption: string;
    FinishBtnHint: string;
    FinishBtnCaption: string;
    StrictBtnHint: string;
    RelaxedBtnHint: string;
    StrictBtnCaption: string;
    RelaxedBtnCaption: string;
    EdThrowoutsHint: string;
    DollarBtnCaption: string;

    TsCacheCaption: string;
    TbCacheCaption: string;
    SynchronizeBtnCaption: string;
    CacheOptionsBtnCaption: string;
    CacheGridUpdateBtnCaption: string;
    StartBtnCaption: string;
    StopBtnCaption: string;
    TurboBtnCaption: string;

    TsBrowserCaption: string;
    TbBrowserCaption: string;
    HomeBtnCaption: string;
    BackBtnCaption: string;
    RefreshBtnCaption: string;

    TsStatusCaption: string;
    TbStatusCaption: string;
    DataBtnCaption: string;
    ReportBtnCaption: string;

    FileMenuCaption: string;
    FileNewItemCaption: string;
    FileOpenItemCaption: string;
    FileOpenByNameItemCaption: string;
    FileSaveItemCaption: string;
    FileSaveAsItemCaption: string;
    FileDeleteItemCaption: string;
    FileExitItemCaption: string;

    ActionsMenuCaption: string;
    ActionsConnectItemCaption: string;
    ActionsDisconnectItemCaption: string;
    ActionsBackupItemCaption: string;
    ActionsRestoreItemCaption: string;
    ActionsRecreateItemCaption: string;
    ActionsClearItemCaption: string;
    ActionsPlugInItemCaption: string;
    ActionsPlugOutItemCaption: string;
    ActionsSynchronizeItemCaption: string;
    ActionsUploadItemCaption: string;
    ActionsDownloadItemCaption: string;
    CopyRankItemCaption: string;

    ProviderMenuCaption: string;
    ScoringModuleItemCaption: string;
    DBInterfaceItemCaption: string;
    SwitchPropsItemCaption: string;

    OptionsMenuCaption: string;
    EventParamsItemCaption: string;
    RegattaPropsItemCaption: string;
    FleetPropsItemCaption: string;
    UniquaPropsItemCaption: string;
    NamePropsItemCaption: string;
    EventPropsItemCaption: string;
    AllOptionsItemCaption: string;

    ToolsMenuCaption: string;
    ImportItemCaption: string;
    ExportItemCaption: string;

    TestMenuCaption: string;
    LoadTestDataItemCaption: string;
    WriteJSXmlItemCaption: string;
    WriteProxyXmlItemCaption: string;
    ShowUndoListsItemCaption: string;
    ShowWatchesItemCaption: string;
    TestPenaltyItemCaption: string;
    BatchDownloadItemCaption: string;
    BatchReportItemCaption: string;
    BatchTestItemCaption: string;

    HelpMenuCaption: string;
    ContentItemCaption: string;
    InfoItemCaption: string;
    LanguageItemCaption: string;

    procedure LoadFromFile(ini: TCustomIniFile);
    procedure SaveToFile(ini: TCustomIniFile);
    procedure Localize(f: TForm); override;
    procedure Extract(f: TForm); override;
  end;

implementation

type
  { this is so that references to Tabs can remain private or protected,
    and we can verify the rest of the program does not access these. }
  TFormFR62Access = class(TFormFR62);

{ TFR62Res }

procedure TFR62Res.InitDefault;
begin
  TsAthletesCaption := 'Entries';
  TbAthletesCaption := 'ToolBar Athletes';
  LoadAthletesBtnCaption := 'Load';
  SaveAthletesBtnCaption := 'Save';
  AddAthleteBtnCaption := 'Add';
  UpdateAthletesBtnCaption := 'Update';
  ClearAthletesBtnCaption := 'Clear';

  TsTimingCaption := 'Timing';
  TbTimingCaption := 'ToolBar Timing';
  TimingConfigBtnCaption := 'Config...';
  TimingTestBtnHint := 'send test message';
  TimingTestBtnCaption := 'Test';
  TimingRandomBtnHint := 'send time for random bib';
  TimingRandomBtnCaption := 'Random';
  TimingResetAgeBtnHint := 'reset color of recently uses cells';
  TimingResetAgeBtnCaption := 'Reset Age';
  TimingSendBtnCaption := 'Send';
  AutoSendBtnCaption := 'AutoSend';

  TsRaceCaption := 'Race';
  TbRaceCaption := 'ToolBar Race';
  RaceDownBtnCaption := '<';
  RaceUpBtnCaption := '>';
  ITDownBtnCaption := '<';
  ITBtnCaption := 'IT%d';
  ITUpBtnCaption := '>';
  ToMRankBtnCaption := 'MRank <-- FT';
  FromMRankBtnCaption := 'MRank --> FT';
  UpdateEventBtnCaption := 'Update Event';

  TsEventCaption := 'Event';
  TbEventCaption := 'ToolBar Event';
  PointsBtnHint := 'show calculated points';
  PointsBtnCaption := 'Points';
  FinishBtnHint := 'show finish position input';
  FinishBtnCaption := 'Finish';
  StrictBtnHint := 'enforces contiguous range of finish positions';
  RelaxedBtnHint := 'accept duplicate numeric input';
  StrictBtnCaption := 'Strict';
  RelaxedBtnCaption := 'Relaxed';
  EdThrowoutsHint := 'number of Throwouts';
  DollarBtnCaption := 'X';

  TsCacheCaption := 'Cache';
  TbCacheCaption := 'ToolBar Cache';
  SynchronizeBtnCaption := 'Synchronize';
  CacheOptionsBtnCaption := 'Options...';
  CacheGridUpdateBtnCaption := 'Update Display';
  StartBtnCaption := 'Start';
  StopBtnCaption := 'Stop';
  TurboBtnCaption := 'Turbo';

  TsBrowserCaption := 'Browser';
  TbBrowserCaption := 'ToolBar Browser';
  HomeBtnCaption := 'Home';
  BackBtnCaption := 'Back';
  RefreshBtnCaption := 'Refresh';

  TsStatusCaption := 'Report';
  TbStatusCaption := 'ToolBar Status';
  DataBtnCaption := 'TXT';
  ReportBtnCaption := 'Report';

  FileMenuCaption := 'Document';
  FileNewItemCaption := 'New';
  FileOpenItemCaption := 'Open';
  FileOpenByNameItemCaption := 'Open by Name';
  FileSaveItemCaption := 'Save';
  FileSaveAsItemCaption := 'Save as';
  FileDeleteItemCaption := 'Delete';
  FileExitItemCaption := 'Exit';

  ActionsMenuCaption := 'Actions';
  ActionsConnectItemCaption := 'Connect';
  ActionsDisconnectItemCaption := 'Disconnect';
  ActionsBackupItemCaption := 'Backup';
  ActionsRestoreItemCaption := 'Restore (shallow)';
  ActionsRecreateItemCaption := 'Restore (full)';
  ActionsClearItemCaption := 'Clear';
  ActionsPlugInItemCaption := 'Plugin';
  ActionsPlugOutItemCaption := 'Plugout';
  ActionsSynchronizeItemCaption := 'Synchronize';
  ActionsUploadItemCaption := 'Upload';
  ActionsDownloadItemCaption := 'Download';
  CopyRankItemCaption := 'Copy Rank';

  ProviderMenuCaption := 'Provider';
  ScoringModuleItemCaption := 'Scoring Module...';
  DBInterfaceItemCaption := 'DB Interface...';
  SwitchPropsItemCaption := 'Switch Properties...';

  OptionsMenuCaption := 'Options';
  EventParamsItemCaption := 'Event Params...';
  RegattaPropsItemCaption := 'Regatta Properties...';
  FleetPropsItemCaption := 'Fleet Properties...';
  UniquaPropsItemCaption := 'Uniqua Properties...';
  NamePropsItemCaption := 'Name Properties...';
  EventPropsItemCaption := 'Event Properties...';

  ToolsMenuCaption := 'Tools';
  ExportItemCaption := 'Export to clipboard';
  ImportItemCaption := 'Import...';

  TestMenuCaption := 'Test';
  LoadTestDataItemCaption := 'Load Test Data';
  WriteJSXmlItemCaption := 'Write JS Xml';
  WriteProxyXmlItemCaption := 'Write Proxy Xml';
  ShowUndoListsItemCaption := 'Show Undo Lists...';
  ShowWatchesItemCaption := 'Show Watches...';
  TestPenaltyItemCaption := 'Test Penalty...';
  BatchDownloadItemCaption := 'Batch Download...'; //'Batch Download';
  BatchReportItemCaption := 'Batch Report...'; //'Batch Report';
  BatchTestItemCaption := 'Batch Test...'; //'Batch Test';

  HelpMenuCaption := 'Help';
  ContentItemCaption := 'Content';
  InfoItemCaption := 'Info';
  LanguageItemCaption := 'Language...';
end;

procedure TFR62Res.Init_de;
begin
end;

procedure TFR62Res.Init_en;
begin
end;

procedure TFR62Res.Localize(f: TForm);
begin
  if f is TFormFR62 then
    LocalizeFormData(f as TFormFR62);
end;

procedure TFR62Res.LocalizeFormData(af: TFormFR62);
begin
end;

procedure TFR62Res.Extract(f: TForm);
begin
end;

procedure TFR62Res.ExtractFormData(af: TFormFR62);
begin
end;

procedure TFR62Res.SaveToFile(ini: TCustomIniFile);
begin
end;

procedure TFR62Res.LoadFromFile(ini: TCustomIniFile);
begin
end;

end.
