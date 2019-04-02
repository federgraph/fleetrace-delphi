object WebTab: TWebTab
  Left = 0
  Top = 0
  Width = 789
  Height = 516
  TabOrder = 0
  object Label1: TLabel
    Left = 20
    Top = 33
    Width = 53
    Height = 13
    Caption = 'select Host'
  end
  object HostCombo: TComboBox
    Left = 96
    Top = 30
    Width = 168
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object InitBtn: TButton
    Left = 278
    Top = 28
    Width = 227
    Height = 25
    Caption = 'Startup Indy Http Server component'
    TabOrder = 1
    OnClick = InitBtnClick
  end
  object GroupBoxUrl: TGroupBox
    Left = 20
    Top = 76
    Width = 705
    Height = 90
    Caption = 'Url'
    TabOrder = 2
    object HomeLabel: TLabel
      Left = 32
      Top = 34
      Width = 236
      Height = 31
      Caption = 'http://localhost:8086/'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -27
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
  end
  object PublishRemoteWebItem: TButton
    Left = 88
    Top = 182
    Width = 57
    Height = 25
    Caption = 'On/Off'
    TabOrder = 3
    OnClick = PublishRemoteWebItemClick
  end
  object PublishSilverlightWebItem: TButton
    Left = 88
    Top = 213
    Width = 57
    Height = 25
    Caption = 'On/Off'
    TabOrder = 4
    OnClick = PublishSilverlightWebItemClick
  end
  object cbRemote: TCheckBox
    Left = 167
    Top = 186
    Width = 97
    Height = 17
    Caption = 'Remote Web'
    Enabled = False
    TabOrder = 5
  end
  object cbSilverlight: TCheckBox
    Left = 167
    Top = 217
    Width = 97
    Height = 17
    Caption = 'Silverlight Web'
    Enabled = False
    TabOrder = 6
  end
  object CopyUrlBtn: TButton
    Left = 650
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Copy'
    TabOrder = 7
    OnClick = CopyUrlBtnClick
  end
  object edTemp: TEdit
    Left = 336
    Top = 184
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'Temp'
  end
  object GroupBoxData: TGroupBox
    Left = 20
    Top = 295
    Width = 705
    Height = 72
    Caption = 'Data'
    TabOrder = 9
    object SaveBtn: TButton
      Left = 120
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      OnClick = SaveBtnClick
    end
    object ClearBtn: TButton
      Left = 201
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = ClearBtnClick
    end
    object LoadBtn: TButton
      Left = 282
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 2
      OnClick = LoadBtnClick
    end
  end
  object CopyDataBtn: TButton
    Left = 650
    Top = 295
    Width = 75
    Height = 25
    Caption = 'Copy'
    TabOrder = 10
    OnClick = CopyDataBtnClick
  end
  object PublishAngularWebItem: TButton
    Left = 88
    Top = 244
    Width = 57
    Height = 25
    Caption = 'On/Off'
    TabOrder = 11
    OnClick = PublishAngularWebItemClick
  end
  object cbAngular: TCheckBox
    Left = 167
    Top = 248
    Width = 97
    Height = 17
    Caption = 'Angular Web'
    Enabled = False
    TabOrder = 12
  end
end
