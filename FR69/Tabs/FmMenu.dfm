object MenuTab: TMenuTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  object ComboPanel: TPanel
    Left = 0
    Top = 0
    Width = 930
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 451
    object GetBtn: TSpeedButton
      Left = 423
      Top = 10
      Width = 42
      Height = 25
      Caption = 'Get'
      OnClick = GetBtnClick
    end
    object DebugModeBtn: TSpeedButton
      Left = 639
      Top = 10
      Width = 49
      Height = 25
      Caption = 'Debug'
      OnClick = DebugModeBtnClick
    end
    object WriteBtn: TSpeedButton
      Left = 694
      Top = 11
      Width = 63
      Height = 24
      Caption = 'Write'
      OnClick = WriteBtnClick
    end
    object CategoryCombo: TComboBox
      Left = 471
      Top = 11
      Width = 162
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = CategoryComboChange
    end
    object UrlCombo: TComboBox
      Left = 8
      Top = 11
      Width = 409
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = UrlComboChange
    end
  end
  object TestBtnPanel: TPanel
    Left = 0
    Top = 108
    Width = 451
    Height = 35
    Align = alTop
    TabOrder = 1
    object TextBtn: TSpeedButton
      Left = 8
      Top = 6
      Width = 42
      Height = 22
      Hint = 'show Example of EventMenu.xml (transformed to text)'
      Caption = 'Text'
      ParentShowHint = False
      ShowHint = True
      OnClick = TextBtnClick
    end
    object XmlBtn: TSpeedButton
      Left = 56
      Top = 6
      Width = 41
      Height = 22
      Hint = 'show Example of EventMenu.xml in Memo'
      Caption = 'Xml'
      ParentShowHint = False
      ShowHint = True
      OnClick = XmlBtnClick
    end
    object TransformBtn: TSpeedButton
      Left = 191
      Top = 6
      Width = 82
      Height = 22
      Hint = 'transform EventMenu.xml (from Memo)'
      Caption = 'Transform'
      ParentShowHint = False
      ShowHint = True
      OnClick = TransformBtnClick
    end
    object DownloadBtn: TSpeedButton
      Left = 103
      Top = 6
      Width = 82
      Height = 22
      Hint = 'download EventMenu.xml using UrlCombo.Text'
      Caption = 'Download'
      ParentShowHint = False
      ShowHint = True
      OnClick = DownloadBtnClick
    end
    object SkipDownloadBtn: TSpeedButton
      Left = 464
      Top = 6
      Width = 105
      Height = 22
      Hint = 'when checked, show url for download in TestMemo only'
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'Skip Download'
    end
    object SkipImportBtn: TSpeedButton
      Left = 575
      Top = 6
      Width = 89
      Height = 22
      Hint = 
        'when checked, download EventData but do not load into applicatio' +
        'n'
      AllowAllUp = True
      GroupIndex = 2
      Down = True
      Caption = 'Skip Import'
    end
    object PostModeBtn: TSpeedButton
      Left = 774
      Top = 6
      Width = 65
      Height = 22
      Hint = 'toggle EventData-Button-Modus (between post and get)'
      AllowAllUp = True
      GroupIndex = 3
      Caption = 'Post'
      ParentShowHint = False
      ShowHint = True
      OnClick = PostModeBtnClick
    end
    object ConvertBtn: TSpeedButton
      Left = 296
      Top = 6
      Width = 74
      Height = 22
      Hint = 'convert EventData.* (from Memo)'
      Caption = 'Convert'
      OnClick = ConvertBtnClick
    end
    object UrlBtn: TSpeedButton
      Left = 681
      Top = 6
      Width = 76
      Height = 22
      Hint = 'show Info for selected Url and current Event'
      Caption = 'Url info'
      OnClick = UrlBtnClick
    end
    object ReadBtn: TSpeedButton
      Left = 376
      Top = 6
      Width = 65
      Height = 22
      Hint = 'import EventData form Memo'
      Caption = 'Read'
      OnClick = ReadBtnClick
    end
  end
  object BackgroundPanel: TPanel
    Left = 0
    Top = 56
    Width = 451
    Height = 52
    Align = alTop
    Color = clMenuBar
    TabOrder = 2
  end
  object CurrentWorkspacePanel: TPanel
    Left = 0
    Top = 143
    Width = 451
    Height = 15
    Align = alTop
    AutoSize = True
    Caption = 'CurrentWorkspacePanel'
    ShowCaption = False
    TabOrder = 3
    object CurrentWorkspaceLabel: TLabel
      Left = 22
      Top = 1
      Width = 93
      Height = 13
      Caption = 'Current Workspace'
    end
  end
  object SelectedWorkspacePanel: TPanel
    Left = 0
    Top = 41
    Width = 451
    Height = 15
    Align = alTop
    AutoSize = True
    TabOrder = 4
    object SelectedWorkspaceLabel: TLabel
      Left = 22
      Top = 1
      Width = 97
      Height = 13
      Caption = 'Selected Workspace'
    end
  end
end
