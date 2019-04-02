object FormFR62: TFormFR62
  Left = 0
  Top = 77
  Caption = 'FR62'
  ClientHeight = 636
  ClientWidth = 973
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesigned
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object NorthContainer: TPageControl
    Left = 248
    Top = 64
    Width = 693
    Height = 159
    TabOrder = 1
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object PageControl: TPageControl
    Left = 248
    Top = 229
    Width = 693
    Height = 161
    TabOrder = 0
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 973
    Height = 29
    ButtonHeight = 23
    Caption = 'ToolBar'
    TabOrder = 2
    Transparent = True
    object ClearBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 89
      Height = 23
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
    object WestBtn: TSpeedButton
      Left = 89
      Top = 0
      Width = 80
      Height = 23
      Caption = 'West'
      OnClick = WestBtnClick
    end
    object NorthBtn: TSpeedButton
      Left = 169
      Top = 0
      Width = 89
      Height = 23
      Caption = 'North'
      OnClick = NorthBtnClick
    end
    object SouthBtn: TSpeedButton
      Left = 258
      Top = 0
      Width = 89
      Height = 23
      Caption = 'South'
      OnClick = SouthBtnClick
    end
    object InfoBtn: TSpeedButton
      Left = 347
      Top = 0
      Width = 89
      Height = 23
      Caption = 'Info'
      OnClick = InfoBtnClick
    end
    object TestBtn: TSpeedButton
      Left = 436
      Top = 0
      Width = 61
      Height = 23
      Caption = 'Test'
      OnClick = TestBtnClick
    end
    object LanguageBtn: TSpeedButton
      Left = 497
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Lang'
      OnClick = LanguageBtnClick
    end
    object FeatureBtn: TSpeedButton
      Left = 561
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Feature'
      OnClick = FeatureBtnClick
    end
    object MinusBtn: TSpeedButton
      Left = 625
      Top = 0
      Width = 40
      Height = 23
      Caption = 'S-'
      OnClick = MinusBtnClick
    end
    object PlusBtn: TSpeedButton
      Left = 665
      Top = 0
      Width = 40
      Height = 23
      Caption = 'S+'
      OnClick = PlusBtnClick
    end
  end
  object SouthContainer: TPageControl
    Left = 248
    Top = 396
    Width = 693
    Height = 161
    TabOrder = 3
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object WestContainer: TPageControl
    Left = 8
    Top = 64
    Width = 225
    Height = 257
    TabOrder = 4
  end
  object Panel: TPanel
    Left = 8
    Top = 344
    Width = 225
    Height = 213
    TabOrder = 5
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 617
    Width = 973
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    OnIdle = ApplicationEventsIdle
    Left = 264
    Top = 120
  end
  object IdleTimer: TTimer
    OnTimer = IdleTimerTimer
    Left = 392
    Top = 120
  end
  object DocActionList: TActionList
    Left = 128
    Top = 120
    object acBackup: TAction
      Category = 'Actions'
      Caption = 'Backup'
      OnExecute = acBackupExecute
    end
    object acRestore: TAction
      Category = 'Actions'
      Caption = 'Restore (shallow)'
      OnExecute = acRestoreExecute
    end
    object acClear: TAction
      Category = 'Actions'
      Caption = 'Clear'
      OnExecute = acClearExecute
    end
    object acRecreate: TAction
      Category = 'Actions'
      Caption = 'Restore (full)'
      OnExecute = acRecreateExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 40
    Top = 120
    object FileMenu: TMenuItem
      Caption = 'Document'
      GroupIndex = 1
      object FileOpenItem: TMenuItem
        Caption = 'Open...'
        OnClick = FileOpenItemClick
      end
      object OpenByNameItem: TMenuItem
        Caption = 'Open by Name...'
        OnClick = OpenByNameItemClick
      end
      object SaveItem: TMenuItem
        Caption = 'Save'
        OnClick = SaveItemClick
      end
      object SaveAsItem: TMenuItem
        Caption = 'Save as'
        OnClick = SaveAsItemClick
      end
      object SaveConfigItem: TMenuItem
        Caption = 'Save Config'
        OnClick = SaveConfigItemClick
      end
      object FileDeleteItem: TMenuItem
        Caption = 'Delete'
        OnClick = FileDeleteItemClick
      end
      object NF1: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'Exit'
        OnClick = ExitItemClick
      end
    end
    object ActionsMenu: TMenuItem
      Caption = 'Actions'
      GroupIndex = 4
      OnClick = ActionsMenuClick
      object ConnectItem: TMenuItem
        Caption = 'Connect'
        OnClick = ConnectItemClick
      end
      object DisconnectItem: TMenuItem
        Caption = 'Disconnect'
        OnClick = DisconnectItemClick
      end
      object SpacerBackup: TMenuItem
        Caption = '-'
      end
      object BackupItem: TMenuItem
        Action = acBackup
      end
      object RestoreItem: TMenuItem
        Action = acRestore
      end
      object RecreateItem: TMenuItem
        Action = acRecreate
      end
      object ClearItem: TMenuItem
        Action = acClear
      end
      object SpacerPlugin: TMenuItem
        Caption = '-'
      end
      object PlugInItem: TMenuItem
        Caption = 'Plugin'
        OnClick = PlugInItemClick
      end
      object PlugOutItem: TMenuItem
        Caption = 'Plugout'
        OnClick = PlugOutItemClick
      end
      object SynchronizeItem: TMenuItem
        Caption = 'Synchronize'
        OnClick = SynchronizeItemClick
      end
      object UploadItem: TMenuItem
        Caption = 'Upload'
        OnClick = UploadItemClick
      end
      object DownloadItem: TMenuItem
        Caption = 'Download'
        OnClick = DownloadItemClick
      end
    end
    object ProviderMenu: TMenuItem
      Caption = 'Provider'
      GroupIndex = 4
      OnClick = ProviderMenuClick
      object ScoringModuleItem: TMenuItem
        Caption = 'Scoring Module...'
        OnClick = ScoringModuleItemClick
      end
      object DBInterfaceItem: TMenuItem
        Caption = 'DB Interface...'
        OnClick = DBInterfaceItemClick
      end
      object BridgeProviderItem: TMenuItem
        Caption = 'Bridge Provider...'
        OnClick = BridgeProviderItemClick
      end
      object WorkspaceLocationItem: TMenuItem
        Caption = 'Workspace Location...'
        OnClick = WorkspaceLocationItemClick
      end
      object NProviderStart: TMenuItem
        Caption = '-'
      end
      object SwitchPropsItem: TMenuItem
        Caption = 'Switch Properties...'
        OnClick = SwitchPropsItemClick
      end
      object IniPropsItem: TMenuItem
        Caption = 'Ini Properties...'
        OnClick = IniPropsItemClick
      end
      object WorkspaceInfoItem: TMenuItem
        Caption = 'Workspace Info...'
        OnClick = WorkspaceInfoItemClick
      end
    end
    object OptionsMenu: TMenuItem
      Caption = 'Options'
      GroupIndex = 4
      OnClick = OptionsMenuClick
      object EventParamsItem: TMenuItem
        Caption = 'Event Params...'
        OnClick = EventParamsItemClick
      end
      object RegattaPropsItem: TMenuItem
        Caption = 'Regatta Properties...'
        OnClick = RegattaPropsItemClick
      end
      object FleetPropsItem: TMenuItem
        Caption = 'Fleet Properties...'
        OnClick = FleetPropsItemClick
      end
      object UniquaPropsItem: TMenuItem
        Caption = 'Uniqua Properties...'
        OnClick = UniquaPropsItemClick
      end
      object NamePropsItem: TMenuItem
        Caption = 'Name Properties...'
        OnClick = NamePropsItemClick
      end
      object EventPropsItem: TMenuItem
        Caption = 'Event Properties...'
        OnClick = EventPropsItemClick
      end
      object MainParamsItem: TMenuItem
        Caption = 'Main Params...'
        OnClick = MainParamsItemClick
      end
      object NProviderEnd: TMenuItem
        Caption = '-'
      end
      object AllOptionsItem: TMenuItem
        Caption = 'All Options...'
        OnClick = AllOptionsItemClick
      end
    end
    object ToolsMenu: TMenuItem
      Caption = 'Tools'
      GroupIndex = 4
      OnClick = ToolsMenuClick
      object CalcItem: TMenuItem
        Caption = 'Calc'
        OnClick = CalcItemClick
      end
      object PartialCalcItem: TMenuItem
        Caption = 'Calc (Partial)'
        OnClick = PartialCalcItemClick
      end
      object ToolsN1: TMenuItem
        Caption = '-'
      end
      object ExportItem: TMenuItem
        Caption = 'Export (Clipboard)'
        OnClick = ExportItemClick
      end
      object ImportItem: TMenuItem
        Caption = 'Import...'
        OnClick = ImportItemClick
      end
      object ToolsN2: TMenuItem
        Caption = '-'
      end
      object InitFleetItem: TMenuItem
        Caption = 'Init Fleet (Initial)'
        OnClick = InitFleetItemClick
      end
      object InitFleetFromFinishItem: TMenuItem
        Caption = 'Init Fleet (Finish)'
        OnClick = InitFleetFromFinishItemClick
      end
      object CopyFleetItem: TMenuItem
        Caption = 'Copy Fleet'
        OnClick = CopyFleetItemClick
      end
      object DisableFleetItem: TMenuItem
        Caption = 'Disable Fleet (Toggle)'
        OnClick = DisableFleetItemClick
      end
      object ToolsN3: TMenuItem
        Caption = '-'
      end
      object ClearRaceItem: TMenuItem
        Caption = 'Clear Race'
        OnClick = ClearRaceItemClick
      end
      object SelectRaceItem: TMenuItem
        Caption = 'Select Race'
        OnClick = SelectRaceItemClick
      end
      object GoBackToRaceItem: TMenuItem
        Caption = 'Go back to Race'
        OnClick = GoBackToRaceItemClick
      end
      object CopyRankItem: TMenuItem
        Caption = 'Copy Rank'
        OnClick = CopyRankItemClick
      end
    end
    object TestMenu: TMenuItem
      Caption = 'Test'
      GroupIndex = 4
      OnClick = TestMenuClick
      object LoadTestDataItem: TMenuItem
        Caption = 'Load Test Data'
        OnClick = LoadTestDataItemClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object WriteJSXmlItem: TMenuItem
        Caption = 'Write JS Xml'
        OnClick = WriteJSXmlItemClick
      end
      object WriteRDXmlItem: TMenuItem
        Caption = 'Write RD Xml'
        OnClick = WriteRDXmlItemClick
      end
      object WriteProxyXmlItem: TMenuItem
        Caption = 'Write Proxy Xml'
        OnClick = WriteProxyXmlItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ShowUndoListsItem: TMenuItem
        Caption = 'Show Undo Lists...'
        OnClick = ShowUndoListsItemClick
      end
      object ShowWatchesItem: TMenuItem
        Caption = 'Show Watches...'
        OnClick = ShowWatchesItemClick
      end
      object ShowScheduleItem: TMenuItem
        Caption = 'Show Schedule...'
        OnClick = ShowScheduleItemClick
      end
      object TestPenaltyItem: TMenuItem
        Caption = 'Test Penalty...'
        OnClick = TestPenaltyItemClick
      end
      object TestJsonItem: TMenuItem
        Caption = 'Test Json...'
        OnClick = TestJsonItemClick
      end
      object TestMessageParserItem: TMenuItem
        Caption = 'Test Message Parser...'
        OnClick = TestMessageParserItemClick
      end
      object TestFormItem: TMenuItem
        Caption = 'Test Form...'
        OnClick = TestFormItemClick
      end
      object InspectReportsItem: TMenuItem
        Caption = 'Inspect Reports...'
        OnClick = InspectReportsItemClick
      end
      object BatchProcessItem: TMenuItem
        Caption = 'Batch Process'
        object BatchTestItem: TMenuItem
          Caption = 'Batch Test'
          OnClick = BatchTestItemClick
        end
        object BatchDownloadItem: TMenuItem
          Caption = 'Batch Download'
          OnClick = BatchDownloadItemClick
        end
        object BatchReportItem: TMenuItem
          Caption = 'Batch Report'
          OnClick = BatchReportItemClick
        end
      end
      object ShowWorkspaceFilesItem: TMenuItem
        Caption = 'Show Workspace Files...'
        OnClick = ShowWorkspaceFilesItemClick
      end
      object WriteResourcesItem: TMenuItem
        Caption = 'Write Resources'
        OnClick = WriteResourcesItemClick
      end
      object UnicodeScannerItem: TMenuItem
        Caption = 'Unicode Scanner...'
        OnClick = UnicodeScannerItemClick
      end
    end
    object PublishMenu: TMenuItem
      Caption = 'Publish'
      GroupIndex = 4
      object PublishAllItem: TMenuItem
        Caption = 'Workspace Reports'
        OnClick = PublishAllItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object PublishSocketsItem: TMenuItem
        Caption = 'Tcp In/Out Sockets'
        OnClick = PublishSocketsItemClick
      end
      object PublishPolicyServerItem: TMenuItem
        Caption = 'Tcp Policy Server'
        OnClick = PublishPolicyServerItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PublishHomeWebItem: TMenuItem
        Caption = 'Home Web'
        OnClick = PublishHomeWebItemClick
      end
      object PublishRemoteWebItem: TMenuItem
        Caption = 'Remote Web'
        OnClick = PublishRemoteWebItemClick
      end
      object PublishSilverlightWebItem: TMenuItem
        Caption = 'Silverlight Web'
        OnClick = PublishSilverlightWebItemClick
      end
    end
    object HelpMenu: TMenuItem
      Caption = 'Help'
      GroupIndex = 4
      OnClick = HelpMenuClick
      object ContentItem: TMenuItem
        Caption = 'Content'
        OnClick = ContentItemClick
      end
      object InfoItem: TMenuItem
        Caption = 'Info'
        OnClick = InfoItemClick
      end
      object AboutItem: TMenuItem
        Caption = 'About'
        OnClick = AboutItemClick
      end
      object LanguageItem: TMenuItem
        Caption = 'Language...'
        OnClick = LanguageItemClick
      end
      object PagesItem: TMenuItem
        Caption = 'Pages...'
        OnClick = PagesItemClick
      end
      object SoundItem: TMenuItem
        Caption = 'Enable Sound'
        OnClick = SoundItemClick
      end
      object EnableLogItem: TMenuItem
        Caption = 'Enable Log'
        OnClick = EnableLogItemClick
      end
      object NorthContainerItem: TMenuItem
        Caption = 'North Container'
        OnClick = NorthContainerItemClick
      end
      object SouthContainerItem: TMenuItem
        Caption = 'South Container'
        OnClick = SouthContainerItemClick
      end
      object EventMenuItem: TMenuItem
        Caption = 'Event Menu'
        OnClick = EventMenuItemClick
      end
      object LanguageToggle: TMenuItem
        Caption = 'Language Toggle'
        ShortCut = 24698
        OnClick = LanguageToggleClick
      end
      object StyleItem: TMenuItem
        Caption = 'Toggle Style'
        OnClick = ToggleStyleBtnClick
      end
      object AccessToggle: TMenuItem
        Caption = 'Access Toggle'
        ShortCut = 24699
        OnClick = AccessToggleClick
      end
    end
  end
end
