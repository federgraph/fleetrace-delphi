object BrowserTab: TBrowserTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 451
    Height = 22
    Caption = 'ToolBar'
    TabOrder = 0
    object HomeIndexBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 32
      Height = 22
      Caption = 'H'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 8388863
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = HomeIndexBtnClick
    end
    object RemoteIndexBtn: TSpeedButton
      Left = 32
      Top = 0
      Width = 32
      Height = 22
      Caption = 'R'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = RemoteIndexBtnClick
    end
    object HomeBtn: TSpeedButton
      Left = 64
      Top = 0
      Width = 64
      Height = 22
      Hint = 'show index page'
      Caption = 'Root'
      OnClick = HomeBtnClick
    end
    object BackBtn: TSpeedButton
      Left = 128
      Top = 0
      Width = 64
      Height = 22
      Hint = 'go back'
      Caption = 'Back'
      Enabled = False
      OnClick = BackBtnClick
    end
    object RefreshBtn: TSpeedButton
      Left = 192
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Refresh'
      Enabled = False
      OnClick = RefreshBtnClick
    end
    object OptionsBtn: TSpeedButton
      Left = 264
      Top = 0
      Width = 73
      Height = 22
      Hint = 'toggle XML/XSL'
      Caption = 'Options'
      OnClick = OptionsBtnClick
    end
    object PlainBtn: TSpeedButton
      Left = 337
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Plain'
      OnClick = PlainBtnClick
    end
  end
end
