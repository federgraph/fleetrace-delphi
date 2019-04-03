object ReportTab: TReportTab
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
    TabOrder = 0
    OnDblClick = ToolBarDblClick
    object StatusBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Status'
      OnClick = StatusBtnClick
    end
    object NormalTextBtn: TSpeedButton
      Left = 50
      Top = 0
      Width = 61
      Height = 22
      Caption = 'Normal'
      OnClick = NormalTextBtnClick
    end
    object CompactTextBtn: TSpeedButton
      Left = 111
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Compact'
      OnClick = CompactTextBtnClick
    end
    object DataBtn: TSpeedButton
      Left = 167
      Top = 0
      Width = 50
      Height = 22
      Caption = 'TXT'
      OnClick = DataBtnClick
    end
    object XmlBtn: TSpeedButton
      Left = 217
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Xml'
      OnClick = XmlBtnClick
    end
    object HtmlBtn: TSpeedButton
      Left = 267
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Html'
      OnClick = HtmlBtnClick
    end
    object JsonBtn: TSpeedButton
      Left = 317
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Json'
      OnClick = JsonBtnClick
    end
    object TestMemoBtn: TSpeedButton
      Left = 381
      Top = 0
      Width = 28
      Height = 22
      Caption = 'tm'
      OnClick = TestMemoBtnClick
    end
    object HashBtn: TSpeedButton
      Left = 409
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Hash'
      OnClick = HashBtnClick
    end
    object TafelBtn: TSpeedButton
      Left = 459
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Board'
      OnClick = TafelBtnClick
    end
    object HelpBtn: TSpeedButton
      Left = 509
      Top = 0
      Width = 63
      Height = 22
      Caption = 'Help'
      OnClick = HelpBtnClick
    end
    object SwapBtn: TSpeedButton
      Left = 572
      Top = 0
      Width = 60
      Height = 22
      Caption = 'Swap'
      OnClick = SwapBtnClick
    end
    object IniBtn: TSpeedButton
      Left = 632
      Top = 0
      Width = 54
      Height = 22
      Caption = 'Ini'
      OnClick = IniBtnClick
    end
    object CheckBtn: TSpeedButton
      Left = 686
      Top = 0
      Width = 50
      Height = 22
      Caption = 'Check'
      OnClick = CheckBtnClick
    end
    object TransBtn: TSpeedButton
      Left = 736
      Top = 0
      Width = 72
      Height = 22
      Caption = 'l10n'
      OnClick = TransBtnClick
    end
    object SemiBtn: TSpeedButton
      Left = 808
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Replace Tabs by Semicolons'
      Caption = ';'
      OnClick = SemiBtnClick
    end
    object TabBtn: TSpeedButton
      Left = 831
      Top = 0
      Width = 23
      Height = 22
      Hint = 'replace semicolons by tabs'
      Caption = '\t'
      OnClick = TabBtnClick
    end
  end
  object StatusMemo: TMemo
    Left = 0
    Top = 121
    Width = 451
    Height = 184
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'StatusMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnKeyPress = StatusMemoKeyPress
  end
  object TestMemo: TMemo
    Left = 0
    Top = 22
    Width = 451
    Height = 99
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'TestMemo')
    ParentFont = False
    TabOrder = 2
  end
end
