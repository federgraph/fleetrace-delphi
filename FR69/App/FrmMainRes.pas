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
    LoadAthletesBtnCaption: string;
    SaveAthletesBtnCaption: string;
    AddAthleteBtnCaption: string;
    UpdateAthletesBtnCaption: string;
    ClearAthletesBtnCaption: string;

    TsTimingCaption: string;
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
    RaceDownBtnCaption: string;
    RaceUpBtnCaption: string;
    ITDownBtnCaption: string;
    ITBtnCaption: string;
    ITUpBtnCaption: string;
    ToMRankBtnCaption: string;
    FromMRankBtnCaption: string;
    UpdateEventBtnCaption: string;

    TsEventCaption: string;
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
    SynchronizeBtnCaption: string;
    CacheOptionsBtnCaption: string;
    CacheGridUpdateBtnCaption: string;
    StartBtnCaption: string;
    StopBtnCaption: string;
    TurboBtnCaption: string;

    TsBrowserCaption: string;
    HomeBtnCaption: string;
    BackBtnCaption: string;
    RefreshBtnCaption: string;

    TsStatusCaption: string;
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
  LoadAthletesBtnCaption := 'Load';
  SaveAthletesBtnCaption := 'Save';
  AddAthleteBtnCaption := 'Add';
  UpdateAthletesBtnCaption := 'Update';
  ClearAthletesBtnCaption := 'Clear';

  TsTimingCaption := 'Timing';
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
  RaceDownBtnCaption := '<';
  RaceUpBtnCaption := '>';
  ITDownBtnCaption := '<';
  ITBtnCaption := 'IT%d';
  ITUpBtnCaption := '>';
  ToMRankBtnCaption := 'MRank <-- FT';
  FromMRankBtnCaption := 'MRank --> FT';
  UpdateEventBtnCaption := 'Update Event';

  TsEventCaption := 'Event';
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
  SynchronizeBtnCaption := 'Synchronize';
  CacheOptionsBtnCaption := 'Options...';
  CacheGridUpdateBtnCaption := 'Update Display';
  StartBtnCaption := 'Start';
  StopBtnCaption := 'Stop';
  TurboBtnCaption := 'Turbo';

  TsBrowserCaption := 'Browser';
  HomeBtnCaption := 'Root';
  BackBtnCaption := 'Back';
  RefreshBtnCaption := 'Refresh';

  TsStatusCaption := 'Report';
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
  TsAthletesCaption := 'Stammdaten'; //'Entries';
  LoadAthletesBtnCaption := 'Laden'; //'Load';
  SaveAthletesBtnCaption := 'Speichern'; //'Save';
  AddAthleteBtnCaption := 'Hinzufügen'; //'Add';
  UpdateAthletesBtnCaption := 'Aktualsieren'; //'Update';
  ClearAthletesBtnCaption := 'Leeren'; //'Clear';

  TsTimingCaption := 'Zeitnahme'; //'Timing';
  TimingConfigBtnCaption := 'Konfiguration'; //'Config';
  TimingTestBtnHint := 'Test-Nachricht senden'; //'send test message';
  TimingTestBtnCaption := 'Test';
  TimingRandomBtnHint := 'sende Zeit für zufällig ausgewählte Startnummer'; //'send time for random bib';
  TimingRandomBtnCaption := 'Zufalls-Auslösung'; //'Random';
  TimingResetAgeBtnHint := 'Farbe zurücksetzen'; //'reset color of recently uses cells';
  TimingResetAgeBtnCaption := 'Farbe löschen'; //'Age';
  TimingSendBtnCaption := 'Senden'; //'Send';
  AutoSendBtnCaption := 'Automatisch Senden'; //'AutoSend';

  TsRaceCaption := 'Wettfahrten'; //'Race';
  RaceDownBtnCaption := '<';
  RaceUpBtnCaption := '>';
  ITDownBtnCaption := '<';
  ITUpBtnCaption := '>';
  ToMRankBtnCaption := 'MRank <-- FT';
  FromMRankBtnCaption := 'MRank --> FT';
  UpdateEventBtnCaption := 'Aktualisiere Event'; //'Update Event';

  TsEventCaption := 'Regatta';
  PointsBtnHint := 'berechnete Punkte anzeigen'; //'show calculated points';
  PointsBtnCaption := 'Punkte'; //'Points';
  FinishBtnHint := 'Zielpositionen anzeigen'; //'show finish position input';
  FinishBtnCaption := 'Zielpos.'; //'Finish';
  EdThrowoutsHint := 'Anzahl der Streicher'; //'number of Throwouts';
  DollarBtnCaption := 'X'; //'X';

  TsCacheCaption := 'Zwischenspeicher'; //'Cache';
  SynchronizeBtnCaption := 'Synchronisieren'; //'Synchronize';
  CacheOptionsBtnCaption := 'Optionen'; //'Options';
  CacheGridUpdateBtnCaption := 'Tabelle aktualisieren'; //'Update Display';
  StartBtnCaption := 'Start';
  StopBtnCaption := 'Stop';
  TurboBtnCaption := 'Turbo';

  TsBrowserCaption := 'Browser';
  HomeBtnCaption := 'Startseite'; //'Home';
  BackBtnCaption := 'Zurück'; //'Back';
  RefreshBtnCaption := 'Aktualisieren'; //'Refresh';

  TsStatusCaption := 'Report';
  DataBtnCaption := 'TXT'; //'TXT';
  ReportBtnCaption := 'Bericht'; //'Report';

  FileMenuCaption := 'Datei'; //File
  FileNewItemCaption := 'Neu'; //New
  FileOpenItemCaption := 'Öffnen'; //Open
  FileOpenByNameItemCaption := 'Öffne über Namen'; //Open by Name
  FileSaveItemCaption := 'Speichern'; //Save
  FileSaveAsItemCaption := 'Speichern unter'; //Save as
  FileDeleteItemCaption := 'Löschen'; //Delete
  FileExitItemCaption := 'Beenden'; //Exit

  ActionsMenuCaption := 'Aktionen'; //'Actions';
  ActionsConnectItemCaption := 'Verbinden'; //Connect
  ActionsDisconnectItemCaption := 'Trennen'; //Disconnect
  ActionsBackupItemCaption := 'Sicherungsdatei schreiben'; //'Backup';
  ActionsRestoreItemCaption := 'Wiederherstellen (einfach)'; //'Restore (shallow)';
  ActionsRecreateItemCaption := 'Wiederherstellen (vollständig)'; //'Restore (full)';
  ActionsClearItemCaption := 'Resultate verwerfen'; //'Clear';
  ActionsPlugInItemCaption := 'Einklinken'; //Plugin
  ActionsPlugOutItemCaption := 'Ausklinken'; //Plugout
  ActionsSynchronizeItemCaption := 'Synchronisieren'; //Synchronize
  ActionsUploadItemCaption := 'Hochladen'; //Upload
  ActionsDownloadItemCaption := 'Herunterladen'; //Download
  CopyRankItemCaption := 'Spalte Rank kopieren'; //Copy Rank

  ProviderMenuCaption := 'Provider';
  ScoringModuleItemCaption := 'Berechnungs Modul...'; //'Scoring Module..';
  DBInterfaceItemCaption := 'Daten Schnittstelle...'; //'DB Interface...';
  SwitchPropsItemCaption := 'Switch Eigenschaften...'; //'Switch Properties...';

  OptionsMenuCaption := 'Optionen'; //'Options';
  EventParamsItemCaption := 'Event Parameter...'; //'Event Params...';
  RegattaPropsItemCaption := 'Regatta Eigenschaften...'; //'Regatta Properties...'
  FleetPropsItemCaption := 'Flotten Eigenenschaften...'; //'Fleet Properties...'
  UniquaPropsItemCaption := 'Uniqua Eigenschaften...'; //'Uniqua Properties...';
  NamePropsItemCaption := 'Optionen für Namen...'; //'Name Properties...'
  EventPropsItemCaption := 'Event Eigenschaften...'; //'Event Properties...';

  ToolsMenuCaption := 'Tools';
  ExportItemCaption := 'Export in die Zwischenablage';
  ImportItemCaption := 'Import...';

  TestMenuCaption := 'Test';
  LoadTestDataItemCaption := 'Testdaten laden'; //'Load Test Data';
  WriteJSXmlItemCaption := 'JS Xml schreiben'; //'Write JS Xml';
  WriteProxyXmlItemCaption := 'Proxy Xml schreiben'; //'Write Proxy Xml';
  ShowUndoListsItemCaption := 'Undo-Listen anzeigen...'; //'Show Undo Lists';
  ShowWatchesItemCaption := 'Überwachte Werte anzeigen...'; //'Show Watches';
  TestPenaltyItemCaption := 'Penalty Testform anzeigen...'; //'Test Penalty';
  BatchDownloadItemCaption := 'Batch Download..'; //'Batch Download';
  BatchReportItemCaption := 'Batch Report...'; //'Batch Report...';
  BatchTestItemCaption := 'Batch Test...'; //'Batch Test...';

  HelpMenuCaption := 'Hilfe'; //'Help';
  ContentItemCaption := 'Inhalt'; //'Content';
  InfoItemCaption := 'Info';
  LanguageItemCaption := 'Sprache...';
end;

procedure TFR62Res.Init_en;
begin
  TsAthletesCaption := 'Entries';
  LoadAthletesBtnCaption := 'Load';
  SaveAthletesBtnCaption := 'Save';
  AddAthleteBtnCaption := 'Add';
  UpdateAthletesBtnCaption := 'Update';
  ClearAthletesBtnCaption := 'Clear';

  tsTimingCaption := 'Timing';
  TimingConfigBtnCaption := 'Config';
  TimingTestBtnHint := 'send test message';
  TimingTestBtnCaption := 'Test';
  TimingRandomBtnHint := 'send time for random bib';
  TimingRandomBtnCaption := 'Random';
  TimingResetAgeBtnHint := 'reset color of recently uses cells';
  TimingResetAgeBtnCaption := 'Age';
  TimingSendBtnCaption := 'Send';
  AutoSendBtnCaption := 'AutoSend';

  TsRaceCaption := 'Race';
  RaceDownBtnCaption := '<';
  RaceUpBtnCaption := '>';
  ITDownBtnCaption := '<';
  ITUpBtnCaption := '>';
  ToMRankBtnCaption := 'MRank <-- FT';
  FromMRankBtnCaption := 'MRank --> FT';
  UpdateEventBtnCaption := 'Update Event';

  TsEventCaption := 'Event';
  PointsBtnHint := 'show calculated points';
  PointsBtnCaption := 'Points';
  FinishBtnHint := 'show finish position input';
  FinishBtnCaption := 'Finish';
  edThrowoutsHint := 'number of Throwouts';
  DollarBtnCaption := 'X';

  TsCacheCaption := 'Cache';
  SynchronizeBtnCaption := 'Synchronize';
  CacheOptionsBtnCaption := 'Options';
  CacheGridUpdateBtnCaption := 'Update Display';
  StartBtnCaption := 'Start';
  StopBtnCaption := 'Stop';
  TurboBtnCaption := 'Turbo';

  TsBrowserCaption := 'Browser';
  HomeBtnCaption := 'Root';
  BackBtnCaption := 'Back';
  RefreshBtnCaption := 'Refresh';

  TsStatusCaption := 'Status';
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
  ScoringModuleItemCaption := 'Scoring Module';
  DBInterfaceItemCaption := 'DB Interface';
  SwitchPropsItemCaption := 'Switch Properties';

  OptionsMenuCaption := 'Options';
  EventParamsItemCaption := 'Event Params...';
  RegattaPropsItemCaption := 'Regatta Properties...';
  FleetPropsItemCaption := 'Fleet Properties...';
  UniquaPropsItemCaption := 'Uniqua Properties...';
  NamePropsItemCaption := 'Name Properties...';
  EventPropsItemCaption := 'Event Properties...';
  AllOptionsItemCaption := 'All Options...';

  ToolsMenuCaption := 'Tools';
  ExportItemCaption := 'Export to clipboard';
  ImportItemCaption := 'Import...';

  TestMenuCaption := 'Test';
  LoadTestDataItemCaption := 'Load Test Data';
  WriteJSXmlItemCaption := 'Write JS Xml';
  WriteProxyXmlItemCaption := 'Write Proxy Xml';
  ShowUndoListsItemCaption := 'Show Undo Lists';
  ShowWatchesItemCaption := 'Show Watches';
  TestPenaltyItemCaption := 'Test Penalty';
  BatchDownloadItemCaption := 'Batch Download';
  BatchReportItemCaption := 'Batch Report';
  BatchTestItemCaption := 'Batch Test';

  HelpMenuCaption := 'Help';
  ContentItemCaption := 'Content';
  InfoItemCaption := 'Info';
  LanguageItemCaption := 'Language';
end;

procedure TFR62Res.Localize(f: TForm);
begin
  if f is TFormFR62 then
    LocalizeFormData(f as TFormFR62);
end;

procedure TFR62Res.LocalizeFormData(af: TFormFR62);
var
  f: TFormFR62Access;
begin
  f := TFormFR62Access(af);

  if f.WantEntries then
  begin
  f.tsEntries.Caption := TsAthletesCaption;
  f.TabEntries.LoadBtn.Caption := LoadAthletesBtnCaption;
  f.TabEntries.SaveBtn.Caption := SaveAthletesBtnCaption;
  f.TabEntries.AddBtn.Caption := AddAthleteBtnCaption;
  f.TabEntries.UpdateBtn.Caption := UpdateAthletesBtnCaption;
  f.TabEntries.ClearBtn.Caption := ClearAthletesBtnCaption;
  end;

  if f.WantTiming then
  begin
  f.tsTiming.Caption := TsTimingCaption;
  f.TabTiming.TestBtn.Hint := TimingTestBtnHint;
  f.TabTiming.TestBtn.Caption := TimingTestBtnCaption;
  f.TabTiming.RandomBtn.Hint := TimingRandomBtnHint;
  f.TabTiming.RandomBtn.Caption := TimingRandomBtnCaption;
  f.TabTiming.AgeBtn.Hint := TimingResetAgeBtnHint;
  f.TabTiming.AgeBtn.Caption := TimingResetAgeBtnCaption;
  f.TabTiming.SendBtn.Caption := TimingSendBtnCaption;
  f.TabTiming.AutoSendBtn.Caption := AutoSendBtnCaption;
  end;

  if f.WantRace then
  begin
  f.tsRace.Caption := TsRaceCaption;
  f.TabRace.RaceDownBtn.Caption := RaceDownBtnCaption;
  f.TabRace.RaceUpBtn.Caption := RaceUpBtnCaption;
  f.TabRace.ITDownBtn.Caption := ITDownBtnCaption;
  f.TabRace.ITUpBtn.Caption := ItUpBtnCaption;
  f.TabRace.ToMRankBtn.Caption := ToMRankBtnCaption;
  f.TabRace.FromMRankBtn.Caption := FromMRankBtnCaption;
  f.TabRace.UpdateEventBtn.Caption := UpdateEventBtnCaption;
  end;

  if f.WantEvent then
  begin
  f.tsEvent.Caption := TsEventCaption;
  f.TabEvent.PointsBtn.Hint := PointsBtnHint;
  f.TabEvent.PointsBtn.Caption := PointsBtnCaption;
  f.TabEvent.FinishBtn.Hint := FinishBtnHint;
  f.TabEvent.FinishBtn.Caption := FinishBtnCaption;
  f.TabEvent.StrictBtn.Hint := StrictBtnHint;
  f.TabEvent.ThrowoutBtn.Hint := EdThrowoutsHint;
  f.TabEvent.DollarBtn.Caption := DollarBtnCaption;
  end;

  if f.WantCache then
  begin
  f.tsCache.Caption := TsCacheCaption;
  f.TabCache.SynchronizeBtn.Caption := SynchronizeBtnCaption;
  f.TabCache.OptionsBtn.Caption := CacheOptionsBtnCaption;
  f.TabCache.UpdateBtn.Caption := CacheGridUpdateBtnCaption;
  f.TabCache.StartBtn.Caption := StartBtnCaption;
  f.TabCache.StopBtn.Caption := StopBtnCaption;
  f.TabCache.TurboBtn.Caption := TurboBtnCaption;
  end;

  if f.WantBrowser then
  begin
  f.tsBrowser.Caption := TsBrowserCaption;
  f.TabBrowser.HomeBtn.Caption := HomeBtnCaption;
  f.TabBrowser.BackBtn.Caption := BackBtnCaption;
  f.TabBrowser.RefreshBtn.Caption := RefreshBtnCaption;
  end;

  if f.WantReport then
  begin
  f.tsReport.Caption := TsStatusCaption;
  f.TabReport.DataBtn.Caption := DataBtnCaption;
  end;

  f.FileMenu.Caption := FileMenuCaption;
  f.FileOpenItem.Caption := FileOpenItemCaption;
  f.OpenByNameItem.Caption := FileOpenByNameItemCaption;
  f.SaveItem.Caption := FileSaveItemCaption;
  f.SaveAsItem.Caption := FileSaveAsItemCaption;
  f.FileDeleteItem.Caption := FileDeleteItemCaption;
  f.ExitItem.Caption := FileExitItemCaption;

  f.ActionsMenu.Caption := ActionsMenuCaption;
  f.ConnectItem.Caption := ActionsConnectItemCaption;
  f.DisconnectItem.Caption := ActionsDisconnectItemCaption;
  f.acBackup.Caption := ActionsBackupItemCaption;
  f.acRestore.Caption := ActionsRestoreItemCaption;
  f.acRecreate.Caption := ActionsRecreateItemCaption;
  f.acClear.Caption := ActionsClearItemCaption;
  f.PlugInItem.Caption := ActionsPlugInItemCaption;
  f.PlugOutItem.Caption := ActionsPlugOutItemCaption;
  f.SynchronizeItem.Caption := ActionsSynchronizeItemCaption;
  f.UploadItem.Caption := ActionsUploadItemCaption;
  f.DownloadItem.Caption := ActionsDownloadItemCaption;
  f.CopyRankItem.Caption := CopyRankItemCaption;

  f.ProviderMenu.Caption := ProviderMenuCaption;
  f.ScoringModuleItem.Caption := ScoringModuleItemCaption;
  f.DBInterfaceItem.Caption := DBInterfaceItemCaption;
  f.SwitchPropsItem.Caption := SwitchPropsItemCaption;

  f.OptionsMenu.Caption := OptionsMenuCaption;
  f.EventParamsItem.Caption := EventParamsItemCaption;
  f.RegattaPropsItem.Caption := RegattaPropsItemCaption;
  f.FleetPropsItem.Caption := FleetPropsItemCaption;
  f.UniquaPropsItem.Caption := UniquaPropsItemCaption;
  f.NamePropsItem.Caption := NamePropsItemCaption;
  f.EventPropsItem.Caption := EventPropsItemCaption;

  f.ToolsMenu.Caption := ToolsMenuCaption;
  f.ExportItem.Caption := ExportItemCaption;
  f.ImportItem.Caption := ImportItemCaption;

  f.TestMenu.Caption := TestMenuCaption;
  f.LoadTestDataItem.Caption := LoadTestDataItemCaption;
  f.WriteJSXmlItem.Caption := WriteJSXmlItemCaption;
  f.WriteProxyXmlItem.Caption := WriteProxyXmlItemCaption;
  f.ShowUndoListsItem.Caption := ShowUndoListsItemCaption;
  f.ShowWatchesItem.Caption := ShowWatchesItemCaption;
  f.TestPenaltyItem.Caption := TestPenaltyItemCaption;
  f.BatchDownloadItem.Caption := BatchDownloadItemCaption;
  f.BatchReportItem.Caption := BatchReportItemCaption;
  f.BatchTestItem.Caption := BatchTestItemCaption;

  f.HelpMenu.Caption := HelpMenuCaption;
  f.ContentItem.Caption := ContentItemCaption;
  f.InfoItem.Caption := InfoItemCaption;
  f.LanguageItem.Caption := LanguageItemCaption;
end;

procedure TFR62Res.Extract(f: TForm);
begin
  if f is TFormFR62 then
    ExtractFormData(f as TFormFR62);
end;

procedure TFR62Res.ExtractFormData(af: TFormFR62);
var
  f: TFormFR62Access;
begin
  f := TFormFR62Access(af);

  if f.WantEntries then
  begin
  TsAthletesCaption := f.tsEntries.Caption;
  LoadAthletesBtnCaption := f.TabEntries.LoadBtn.Caption;
  SaveAthletesBtnCaption := f.TabEntries.SaveBtn.Caption;
  AddAthleteBtnCaption := f.TabEntries.AddBtn.Caption;
  UpdateAthletesBtnCaption := f.TabEntries.UpdateBtn.Caption;
  ClearAthletesBtnCaption := f.TabEntries.ClearBtn.Caption;
  end;

  if f.WantTiming then
  begin
  TsTimingCaption := f.tsTiming.Caption;
  TimingTestBtnHint := f.TabTiming.TestBtn.Hint;
  TimingTestBtnCaption := f.TabTiming.TestBtn.Caption;
  TimingRandomBtnHint := f.TabTiming.RandomBtn.Hint;
  TimingRandomBtnCaption := f.TabTiming.RandomBtn.Caption;
  TimingResetAgeBtnHint := f.TabTiming.AgeBtn.Hint;
  TimingResetAgeBtnCaption := f.TabTiming.AgeBtn.Caption;
  TimingSendBtnCaption := f.TabTiming.SendBtn.Caption;
  AutoSendBtnCaption := f.TabTiming.AutoSendBtn.Caption;
  end;

  if f.WantRace then
  begin
  TsRaceCaption := f.tsRace.Caption;
  RaceDownBtnCaption := f.TabRace.RaceDownBtn.Caption;
  RaceUpBtnCaption := f.TabRace.RaceUpBtn.Caption;
  ITDownBtnCaption := f.TabRace.ITDownBtn.Caption;
  ItUpBtnCaption := f.TabRace.ITUpBtn.Caption;
  ToMRankBtnCaption := f.TabRace.ToMRankBtn.Caption;
  FromMRankBtnCaption := f.TabRace.FromMRankBtn.Caption;
  UpdateEventBtnCaption := f.TabRace.UpdateEventBtn.Caption;
  end;

  if f.WantEvent then
  begin
  TsEventCaption := f.tsEvent.Caption;
  PointsBtnHint := f.TabEvent.PointsBtn.Hint;
  PointsBtnCaption := f.TabEvent.PointsBtn.Caption;
  FinishBtnHint := f.TabEvent.FinishBtn.Hint;
  FinishBtnCaption := f.TabEvent.FinishBtn.Caption;
  StrictBtnHint := f.TabEvent.StrictBtn.Hint;
  EdThrowoutsHint := f.TabEvent.ThrowoutBtn.Hint;
  DollarBtnCaption := f.TabEvent.DollarBtn.Caption;
  end;

  if f.WantCache then
  begin
  TsCacheCaption := f.tsCache.Caption;
  SynchronizeBtnCaption := f.TabCache.SynchronizeBtn.Caption;
  CacheOptionsBtnCaption := f.TabCache.OptionsBtn.Caption;
  CacheGridUpdateBtnCaption := f.TabCache.UpdateBtn.Caption;
  StartBtnCaption := f.TabCache.StartBtn.Caption;
  StopBtnCaption := f.TabCache.StopBtn.Caption;
  TurboBtnCaption := f.TabCache.TurboBtn.Caption;
  end;

  if f.WantBrowser then
  begin
  TsBrowserCaption := f.tsBrowser.Caption;
  HomeBtnCaption := f.TabBrowser.HomeBtn.Caption;
  BackBtnCaption := f.TabBrowser.BackBtn.Caption;
  RefreshBtnCaption := f.TabBrowser.RefreshBtn.Caption;
  end;

  if f.WantReport then
  begin
  TsStatusCaption := f.tsReport.Caption;
  DataBtnCaption := f.TabReport.DataBtn.Caption;
  end;

  FileMenuCaption := f.FileMenu.Caption;
  FileOpenItemCaption := f.FileOpenItem.Caption;
  FileOpenByNameItemCaption := f.OpenByNameItem.Caption;
  FileSaveItemCaption := f.SaveItem.Caption;
  FileSaveAsItemCaption := f.SaveAsItem.Caption;
  FileDeleteItemCaption  := f.FileDeleteItem.Caption;
  FileExitItemCaption := f.ExitItem.Caption;

  ActionsMenuCaption := f.ActionsMenu.Caption;
  ActionsConnectItemCaption := f.ConnectItem.Caption;
  ActionsDisconnectItemCaption := f.DisconnectItem.Caption;
  ActionsBackupItemCaption := f.acBackup.Caption;
  ActionsRestoreItemCaption := f.acRestore.Caption;
  ActionsRecreateItemCaption := f.acRecreate.Caption;
  ActionsClearItemCaption := f.acClear.Caption;
  ActionsPlugInItemCaption := f.PlugInItem.Caption;
  ActionsPlugOutItemCaption := f.PlugOutItem.Caption;
  ActionsSynchronizeItemCaption := f.SynchronizeItem.Caption;
  ActionsUploadItemCaption := f.UploadItem.Caption;
  ActionsDownloadItemCaption := f.DownloadItem.Caption;
  CopyRankItemCaption := f.CopyRankItem.Caption;

  ProviderMenuCaption := f.ProviderMenu.Caption;
  ScoringModuleItemCaption := f.ScoringModuleItem.Caption;
  DBInterfaceItemCaption := f.DBInterfaceItem.Caption;
  SwitchPropsItemCaption := f.SwitchPropsItem.Caption;

  OptionsMenuCaption := f.OptionsMenu.Caption;
  EventParamsItemCaption := f.EventParamsItem.Caption;
  RegattaPropsItemCaption := f.RegattaPropsItem.Caption;
  FleetPropsItemCaption := f.FleetPropsItem.Caption;
  UniquaPropsItemCaption := f.UniquaPropsItem.Caption;
  NamePropsItemCaption := f.NamePropsItem.Caption;
  EventPropsItemCaption := f.EventPropsItem.Caption;
  AllOptionsItemCaption := f.AllOptionsItem.Caption;

  ToolsMenuCaption := f.ToolsMenu.Caption;
  ExportItemCaption := f.ExportItem.Caption;
  ImportItemCaption := f.ImportItem.Caption;

  TestMenuCaption := f.TestMenu.Caption;
  LoadTestDataItemCaption := f.LoadTestDataItem.Caption;
  WriteJSXmlItemCaption := f.WriteJSXmlItem.Caption;
  WriteProxyXmlItemCaption := f.WriteProxyXmlItem.Caption;
  ShowUndoListsItemCaption := f.ShowUndoListsItem.Caption;
  ShowWatchesItemCaption := f.ShowWatchesItem.Caption;
  TestPenaltyItemCaption := f.TestPenaltyItem.Caption;
  BatchDownloadItemCaption := f.BatchDownloadItem.Caption;
  BatchReportItemCaption := f.BatchReportItem.Caption;
  BatchTestItemCaption := f.BatchTestItem.Caption;

  HelpMenuCaption := f.HelpMenu.Caption;
  ContentItemCaption := f.ContentItem.Caption;
  InfoItemCaption := f.InfoItem.Caption;
  LanguageItemCaption := f.LanguageItem.Caption;
end;

procedure TFR62Res.SaveToFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := 'PanelAthletes';
  ini.WriteString(n, 'TsAthletesCaption', TsAthletesCaption);
  ini.WriteString(n, 'LoadAthletesBtnCaption', LoadAthletesBtnCaption);
  ini.WriteString(n, 'SaveAthletesBtnCaption', SaveAthletesBtnCaption);
  ini.WriteString(n, 'AddAthleteBtnCaption', AddAthleteBtnCaption);
  ini.WriteString(n, 'UpdateAthletesBtnCaption', UpdateAthletesBtnCaption);
  ini.WriteString(n, 'ClearAthletesBtnCaption', ClearAthletesBtnCaption);

  n := 'PanelTiming';
  ini.WriteString(n, 'TsTimingCaption', TsTimingCaption);
  ini.WriteString(n, 'TimingConfigBtnCaption', TimingConfigBtnCaption);
  ini.WriteString(n, 'TimingTestBtnHint', TimingTestBtnHint);
  ini.WriteString(n, 'TimingTestBtnCaption', TimingTestBtnCaption);
  ini.WriteString(n, 'TimingRandomBtnHint', TimingRandomBtnHint);
  ini.WriteString(n, 'TimingRandomBtnCaption', TimingRandomBtnCaption);
  ini.WriteString(n, 'TimingResetAgeBtnHint', TimingResetAgeBtnHint);
  ini.WriteString(n, 'TimingResetAgeBtnCaption', TimingResetAgeBtnCaption);
  ini.WriteString(n, 'TimingSendBtnCaption', TimingSendBtnCaption);
  ini.WriteString(n, 'AutoSendBtnCaption', AutoSendBtnCaption);

  n := 'PanelRace';
  ini.WriteString(n, 'TsRaceCaption', TsRaceCaption);
  ini.WriteString(n, 'RaceDownBtnCaption', RaceDownBtnCaption);
  ini.WriteString(n, 'RaceUpBtnCaption', RaceUpBtnCaption);
  ini.WriteString(n, 'ITDownBtnCaption', ITDownBtnCaption);
  ini.WriteString(n, 'ITBtnCaption', ITBtnCaption);
  ini.WriteString(n, 'ITUpBtnCaption', ITUpBtnCaption);
  ini.WriteString(n, 'ToMRankBtnCaption', ToMRankBtnCaption);
  ini.WriteString(n, 'FromMRankBtnCaption', FromMRankBtnCaption);
  ini.WriteString(n, 'UpdateEventBtnCaption', UpdateEventBtnCaption);

  n := 'PanelEvent';
  ini.WriteString(n, 'TsEventCaption', TsEventCaption);
  ini.WriteString(n, 'PointsBtnHint', PointsBtnHint);
  ini.WriteString(n, 'PointsBtnCaption', PointsBtnCaption);
  ini.WriteString(n, 'FinishBtnHint', FinishBtnHint);
  ini.WriteString(n, 'FinishBtnCaption', FinishBtnCaption);
  ini.WriteString(n, 'StrictBtnHint', StrictBtnHint);
  ini.WriteString(n, 'RelaxedBtnHint', RelaxedBtnHint);
  ini.WriteString(n, 'StrictBtnCaption', StrictBtnCaption);
  ini.WriteString(n, 'RelaxedBtnCaption', RelaxedBtnCaption);
  ini.WriteString(n, 'EdThrowoutsHint', EdThrowoutsHint);
  ini.WriteString(n, 'DollarBtnCaption', DollarBtnCaption);

  n := 'PanelCache';
  ini.WriteString(n, 'TsCacheCaption', TsCacheCaption);
  ini.WriteString(n, 'SynchronizeBtnCaption', SynchronizeBtnCaption);
  ini.WriteString(n, 'CacheOptionsBtnCaption', CacheOptionsBtnCaption);
  ini.WriteString(n, 'CacheGridUpdateBtnCaption', CacheGridUpdateBtnCaption);
  ini.WriteString(n, 'StartBtnCaption', StartBtnCaption);
  ini.WriteString(n, 'StopBtnCaption', StopBtnCaption);
  ini.WriteString(n, 'TurboBtnCaption', TurboBtnCaption);

  n := 'PanelBrowser';
  ini.WriteString(n, 'TsBrowserCaption', TsBrowserCaption);
  ini.WriteString(n, 'HomeBtnCaption', HomeBtnCaption);
  ini.WriteString(n, 'BackBtnCaption', BackBtnCaption);
  ini.WriteString(n, 'RefreshBtnCaption', RefreshBtnCaption);

  n := 'PanelStatus';
  ini.WriteString(n, 'TsStatusCaption', TsStatusCaption);
  ini.WriteString(n, 'DataBtnCaption', DataBtnCaption);
  ini.WriteString(n, 'ReportBtnCaption', ReportBtnCaption);

  n := 'FileMenu';
  ini.WriteString(n, 'FileMenuCaption', FileMenuCaption);
  ini.WriteString(n, 'FileNewItemCaption', FileNewItemCaption);
  ini.WriteString(n, 'FileOpenItemCaption', FileOpenItemCaption);
  ini.WriteString(n, 'FileOpenByNameItemCaption', FileOpenByNameItemCaption);
  ini.WriteString(n, 'FileSaveItemCaption', FileSaveItemCaption);
  ini.WriteString(n, 'FileSaveAsItemCaption', FileSaveAsItemCaption);
  ini.WriteString(n, 'FileDeleteItemCaption', FileDeleteItemCaption);
  ini.WriteString(n, 'FileExitItemCaption', FileExitItemCaption);

  n := 'ActionsMenu';
  ini.WriteString(n, 'ActionsMenuCaption', ActionsMenuCaption);
  ini.WriteString(n, 'ActionsConnectItemCaption', ActionsConnectItemCaption);
  ini.WriteString(n, 'ActionsDisconnectItemCaption', ActionsDisconnectItemCaption);
  ini.WriteString(n, 'ActionsBackupItemCaption', ActionsBackupItemCaption);
  ini.WriteString(n, 'ActionsRestoreItemCaption', ActionsRestoreItemCaption);
  ini.WriteString(n, 'ActionsRecreateItemCaption', ActionsRecreateItemCaption);
  ini.WriteString(n, 'ActionsClearItemCaption', ActionsClearItemCaption);
  ini.WriteString(n, 'ActionsPlugInItemCaption', ActionsPlugInItemCaption);
  ini.WriteString(n, 'ActionsPlugOutItemCaption', ActionsPlugOutItemCaption);
  ini.WriteString(n, 'ActionsSynchronizeItemCaption', ActionsSynchronizeItemCaption);
  ini.WriteString(n, 'ActionsUploadItemCaption', ActionsUploadItemCaption);
  ini.WriteString(n, 'ActionsDownloadItemCaption', ActionsDownloadItemCaption);
  ini.WriteString(n, 'CopyRankItemCaption', CopyRankItemCaption);

  n := 'ProviderMenu';
  ini.WriteString(n, 'ScoringModuleItemCaption', ScoringModuleItemCaption);
  ini.WriteString(n, 'DBInterfaceItemCaption', DBInterfaceItemCaption);
  ini.WriteString(n, 'SwitchPropsItemCaption', SwitchPropsItemCaption);

  n := 'OptionsMenu';
  ini.WriteString(n, 'OptionsMenuCaption', OptionsMenuCaption);
  ini.WriteString(n, 'EventParamsItemCaption', EventParamsItemCaption);
  ini.WriteString(n, 'RegattaPropsItemCaption', RegattaPropsItemCaption);
  ini.WriteString(n, 'FleetPropsItemCaption', FleetPropsItemCaption);
  ini.WriteString(n, 'UniquaPropsItemCaption', UniquaPropsItemCaption);
  ini.WriteString(n, 'NamePropsItemCaption', NamePropsItemCaption);
  ini.WriteString(n, 'EventPropsItemCaption', EventPropsItemCaption);
  ini.WriteString(n, 'AllOptionsItemCaption', AllOptionsItemCaption);

  n := 'ToolsMenu';
  ini.WriteString(n, 'ExportItemCaption', ExportItemCaption);
  ini.WriteString(n, 'ImportItemCaption', ImportItemCaption);

  n := 'TestMenu';
  ini.WriteString(n, 'TestMenuCaption', TestMenuCaption);
  ini.WriteString(n, 'LoadTestDataItemCaption', LoadTestDataItemCaption);
  ini.WriteString(n, 'WriteJSXmlItemCaption', WriteJSXmlItemCaption);
  ini.WriteString(n, 'WriteProxyXmlItemCaption', WriteProxyXmlItemCaption);
  ini.WriteString(n, 'ShowUndoListsItemCaption', ShowUndoListsItemCaption);
  ini.WriteString(n, 'ShowWatchesItemCaption', ShowWatchesItemCaption);
  ini.WriteString(n, 'TestPenaltyItemCaption', TestPenaltyItemCaption);
  ini.WriteString(n, 'BatchDownloadItemCaption', BatchDownloadItemCaption);
  ini.WriteString(n, 'BatchReportItemCaption', BatchReportItemCaption);
  ini.WriteString(n, 'BatchTestItemCaption', BatchTestItemCaption);

  n := 'HelpMenu';
  ini.WriteString(n, 'HelpMenuCaption', HelpMenuCaption);
  ini.WriteString(n, 'ContentItemCaption', ContentItemCaption);
  ini.WriteString(n, 'InfoItemCaption', InfoItemCaption);
  ini.WriteString(n, 'LanguageItemCaption', LanguageItemCaption);
end;

procedure TFR62Res.LoadFromFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := 'PanelAthletes';
  TsAthletesCaption := ini.ReadString(n, 'TsAthletesCaption', TsAthletesCaption);
  LoadAthletesBtnCaption := ini.ReadString(n, 'LoadAthletesBtnCaption', LoadAthletesBtnCaption);
  SaveAthletesBtnCaption := ini.ReadString(n, 'SaveAthletesBtnCaption', SaveAthletesBtnCaption);
  AddAthleteBtnCaption := ini.ReadString(n, 'AddAthleteBtnCaption', AddAthleteBtnCaption);
  UpdateAthletesBtnCaption := ini.ReadString(n, 'UpdateAthletesBtnCaption', UpdateAthletesBtnCaption);
  ClearAthletesBtnCaption := ini.ReadString(n, 'ClearAthletesBtnCaption', ClearAthletesBtnCaption);

  n := 'PanelTiming';
  TsTimingCaption := ini.ReadString(n, 'TsTimingCaption', TsTimingCaption);
  TimingConfigBtnCaption := ini.ReadString(n, 'TimingConfigBtnCaption', TimingConfigBtnCaption);
  TimingTestBtnHint := ini.ReadString(n, 'TimingTestBtnHint', TimingTestBtnHint);
  TimingTestBtnCaption := ini.ReadString(n, 'TimingTestBtnCaption', TimingTestBtnCaption);
  TimingRandomBtnHint := ini.ReadString(n, 'TimingRandomBtnHint', TimingRandomBtnHint);
  TimingRandomBtnCaption := ini.ReadString(n, 'TimingRandomBtnCaption', TimingRandomBtnCaption);
  TimingResetAgeBtnHint := ini.ReadString(n, 'TimingResetAgeBtnHint', TimingResetAgeBtnHint);
  TimingResetAgeBtnCaption := ini.ReadString(n, 'TimingResetAgeBtnCaption', TimingResetAgeBtnCaption);
  TimingSendBtnCaption := ini.ReadString(n, 'TimingSendBtnCaption', TimingSendBtnCaption);
  AutoSendBtnCaption := ini.ReadString(n, 'AutoSendBtnCaption', AutoSendBtnCaption);

  n := 'PanelRace';
  TsRaceCaption := ini.ReadString(n, 'TsRaceCaption', TsRaceCaption);
  RaceDownBtnCaption := ini.ReadString(n, 'RaceDownBtnCaption', RaceDownBtnCaption);
  RaceUpBtnCaption := ini.ReadString(n, 'RaceUpBtnCaption', RaceUpBtnCaption);
  ITDownBtnCaption := ini.ReadString(n, 'ITDownBtnCaption', ITDownBtnCaption);
  ITBtnCaption := ini.ReadString(n, 'ITBtnCaption', ITBtnCaption);
  ITUpBtnCaption := ini.ReadString(n, 'ITUpBtnCaption', ITUpBtnCaption);
  ToMRankBtnCaption := ini.ReadString(n, 'ToMRankBtnCaption', ToMRankBtnCaption);
  FromMRankBtnCaption := ini.ReadString(n, 'FromMRankBtnCaption', FromMRankBtnCaption);
  UpdateEventBtnCaption := ini.ReadString(n, 'UpdateEventBtnCaption', UpdateEventBtnCaption);

  n := 'PanelEvent';
  TsEventCaption := ini.ReadString(n, 'TsEventCaption', TsEventCaption);
  PointsBtnHint := ini.ReadString(n, 'PointsBtnHint', PointsBtnHint);
  PointsBtnCaption := ini.ReadString(n, 'PointsBtnCaption', PointsBtnCaption);
  FinishBtnHint := ini.ReadString(n, 'FinishBtnHint', FinishBtnHint);
  FinishBtnCaption := ini.ReadString(n, 'FinishBtnCaption', FinishBtnCaption);
  StrictBtnHint := ini.ReadString(n, 'StrictBtnHint', StrictBtnHint);
  RelaxedBtnHint := ini.ReadString(n, 'RelaxedBtnHint', RelaxedBtnHint);
  StrictBtnCaption := ini.ReadString(n, 'StrictBtnCaption', StrictBtnCaption);
  RelaxedBtnCaption := ini.ReadString(n, 'RelaxedBtnCaption', RelaxedBtnCaption);
  EdThrowoutsHint := ini.ReadString(n, 'EdThrowoutsHint', EdThrowoutsHint);
  DollarBtnCaption := ini.ReadString(n, 'DollarBtnCaption', DollarBtnCaption);

  n := 'PanelCache';
  TsCacheCaption := ini.ReadString(n, 'TsCacheCaption', TsCacheCaption);
  SynchronizeBtnCaption := ini.ReadString(n, 'SynchronizeBtnCaption', SynchronizeBtnCaption);
  CacheOptionsBtnCaption := ini.ReadString(n, 'CacheOptionsBtnCaption', CacheOptionsBtnCaption);
  CacheGridUpdateBtnCaption := ini.ReadString(n, 'CacheGridUpdateBtnCaption', CacheGridUpdateBtnCaption);
  StartBtnCaption := ini.ReadString(n, 'StartBtnCaption', StartBtnCaption);
  StopBtnCaption := ini.ReadString(n, 'StopBtnCaption', StopBtnCaption);
  TurboBtnCaption := ini.ReadString(n, 'TurboBtnCaption', TurboBtnCaption);

  n := 'PanelBrowser';
  TsBrowserCaption := ini.ReadString(n, 'TsBrowserCaption', TsBrowserCaption);
  HomeBtnCaption := ini.ReadString(n, 'HomeBtnCaption', HomeBtnCaption);
  BackBtnCaption := ini.ReadString(n, 'BackBtnCaption', BackBtnCaption);
  RefreshBtnCaption := ini.ReadString(n, 'RefreshBtnCaption', RefreshBtnCaption);

  n := 'PanelStatus';
  TsStatusCaption := ini.ReadString(n, 'TsStatusCaption', TsStatusCaption);
  DataBtnCaption := ini.ReadString(n, 'DataBtnCaption', DataBtnCaption);
  ReportBtnCaption := ini.ReadString(n, 'ReportBtnCaption', ReportBtnCaption);

  n := 'FileMenu';
  FileMenuCaption := ini.ReadString(n, 'FileMenuCaption', FileMenuCaption);
  FileNewItemCaption := ini.ReadString(n, 'FileNewItemCaption', FileNewItemCaption);
  FileOpenItemCaption := ini.ReadString(n, 'FileOpenItemCaption', FileOpenItemCaption);
  FileOpenByNameItemCaption := ini.ReadString(n, 'FileOpenByNameItemCaption', FileOpenByNameItemCaption);
  FileSaveItemCaption := ini.ReadString(n, 'FileSaveItemCaption', FileSaveItemCaption);
  FileSaveAsItemCaption := ini.ReadString(n, 'FileSaveAsItemCaption', FileSaveAsItemCaption);
  FileDeleteItemCaption := ini.ReadString(n, 'FileDeleteItemCaption', FileDeleteItemCaption);
  FileExitItemCaption := ini.ReadString(n, 'FileExitItemCaption', FileExitItemCaption);

  n := 'ActionsMenu';
  ActionsMenuCaption := ini.ReadString(n, 'ActionsMenuCaption', ActionsMenuCaption);
  ActionsConnectItemCaption := ini.ReadString(n, 'ActionsConnectItemCaption', ActionsConnectItemCaption);
  ActionsDisconnectItemCaption := ini.ReadString(n, 'ActionsDisconnectItemCaption', ActionsDisconnectItemCaption);
  ActionsBackupItemCaption := ini.ReadString(n, 'ActionsBackupItemCaption', ActionsBackupItemCaption);
  ActionsRestoreItemCaption := ini.ReadString(n, 'ActionsRestoreItemCaption', ActionsRestoreItemCaption);
  ActionsRecreateItemCaption := ini.ReadString(n, 'ActionsRecreateItemCaption', ActionsRecreateItemCaption);
  ActionsClearItemCaption := ini.ReadString(n, 'ActionsClearItemCaption', ActionsClearItemCaption);
  ActionsPlugInItemCaption := ini.ReadString(n, 'ActionsPlugInItemCaption', ActionsPlugInItemCaption);
  ActionsPlugOutItemCaption := ini.ReadString(n, 'ActionsPlugOutItemCaption', ActionsPlugOutItemCaption);
  ActionsSynchronizeItemCaption := ini.ReadString(n, 'ActionsSynchronizeItemCaption', ActionsSynchronizeItemCaption);
  ActionsUploadItemCaption := ini.ReadString(n, 'ActionsUploadItemCaption', ActionsUploadItemCaption);
  ActionsDownloadItemCaption := ini.ReadString(n, 'ActionsDownloadItemCaption', ActionsDownloadItemCaption);
  CopyRankItemCaption := ini.ReadString(n, 'CopyRankItemCaption', CopyRankItemCaption);

  n := 'OptionsMenu';
  OptionsMenuCaption := ini.ReadString(n, 'OptionsMenuCaption', OptionsMenuCaption);
  EventParamsItemCaption := ini.ReadString(n, 'EventParamsItemCaption', EventParamsItemCaption);
  RegattaPropsItemCaption := ini.ReadString(n, 'RegattaPropsItemCaption', RegattaPropsItemCaption);
  FleetPropsItemCaption := ini.ReadString(n, 'FleetPropsItemCaption', FleetPropsItemCaption);
  UniquaPropsItemCaption := ini.ReadString(n, 'UniquaPropsItemCaption', UniquaPropsItemCaption);
  EventPropsItemCaption := ini.ReadString(n, 'EventPropsItemCaption', EventPropsItemCaption);

  n := 'ProviderMenu';
  ScoringModuleItemCaption := ini.ReadString(n, 'ScoringModuleItemCaption', ScoringModuleItemCaption);
  DBInterfaceItemCaption := ini.ReadString(n, 'DBInterfaceItemCaption', DBInterfaceItemCaption);
  SwitchPropsItemCaption := ini.ReadString(n, 'SwitchPropsItemCaption', SwitchPropsItemCaption);

  n := 'ToolsMenu';
  ToolsMenuCaption := ini.ReadString(n, 'ToolsMenuCaption', ToolsMenuCaption);
  ExportItemCaption := ini.ReadString(n, 'ExportItemCaption', ExportItemCaption);
  ImportItemCaption := ini.ReadString(n, 'ImportItemCaption', ImportItemCaption);

  n := 'TestMenu';
  TestMenuCaption := ini.ReadString(n, 'TestMenuCaption', TestMenuCaption);
  LoadTestDataItemCaption := ini.ReadString(n, 'LoadTestDataItemCaption', LoadTestDataItemCaption);
  WriteJSXmlItemCaption := ini.ReadString(n, 'WriteJSXmlItemCaption', WriteJSXmlItemCaption);
  WriteProxyXmlItemCaption := ini.ReadString(n, 'WriteProxyXmlItemCaption', WriteProxyXmlItemCaption);
  ShowUndoListsItemCaption := ini.ReadString(n, 'ShowUndoListsItemCaption', ShowUndoListsItemCaption);
  ShowWatchesItemCaption := ini.ReadString(n, 'ShowWatchesItemCaption', ShowWatchesItemCaption);
  TestPenaltyItemCaption := ini.ReadString(n, 'TestPenaltyItemCaption', TestPenaltyItemCaption);
  BatchDownloadItemCaption := ini.ReadString(n, 'BatchDownloadItemCaption', BatchDownloadItemCaption);
  BatchReportItemCaption := ini.ReadString(n, 'BatchReportItemCaption', BatchReportItemCaption);
  BatchTestItemCaption := ini.ReadString(n, 'BatchTestItemCaption', BatchTestItemCaption);

  n := 'HelpMenu';
  HelpMenuCaption := ini.ReadString(n, 'HelpMenuCaption', HelpMenuCaption);
  ContentItemCaption := ini.ReadString(n, 'ContentItemCaption', ContentItemCaption);
  InfoItemCaption := ini.ReadString(n, 'InfoItemCaption', InfoItemCaption);
  LanguageItemCaption := ini.ReadString(n, 'LanguageItemCaption', LanguageItemCaption);

end;

end.
