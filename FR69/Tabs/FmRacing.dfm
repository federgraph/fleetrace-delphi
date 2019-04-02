object RacingTab: TRacingTab
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
    Height = 27
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 23
    Caption = 'ToolBar'
    TabOrder = 0
    Transparent = True
    object RaceDownBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 33
      Height = 23
      Caption = 'R-'
      OnClick = RaceDownBtnClick
    end
    object RaceBtn: TSpeedButton
      Left = 33
      Top = 0
      Width = 40
      Height = 23
      Hint = 'send test message'
      Caption = 'R0'
      ParentShowHint = False
      ShowHint = True
      OnClick = RaceBtnClick
    end
    object RaceUpBtn: TSpeedButton
      Left = 73
      Top = 0
      Width = 32
      Height = 23
      Caption = 'R+'
      OnClick = RaceUpBtnClick
    end
    object ClearBtn: TSpeedButton
      Left = 105
      Top = 0
      Width = 57
      Height = 23
      Caption = 'Reset'
      OnClick = ClearBtnClick
    end
    object TestBtn: TSpeedButton
      Left = 162
      Top = 0
      Width = 56
      Height = 23
      Hint = 'send test message'
      Caption = 'Test'
      ParentShowHint = False
      ShowHint = True
      OnClick = TestBtnClick
    end
    object RandomBtn: TSpeedButton
      Left = 218
      Top = 0
      Width = 79
      Height = 23
      Hint = 'send time for random bib'
      Caption = 'Random'
      OnClick = RandomBtnClick
    end
    object AgeBtn: TSpeedButton
      Left = 297
      Top = 0
      Width = 56
      Height = 23
      Hint = 'reset color of recently uses cells'
      Caption = 'Age'
      OnClick = AgeBtnClick
    end
    object SendBtn: TSpeedButton
      Left = 353
      Top = 0
      Width = 56
      Height = 23
      Caption = 'Send'
      OnClick = SendBtnClick
    end
    object AutoSendBtn: TSpeedButton
      Left = 409
      Top = 0
      Width = 81
      Height = 23
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'AutoSend'
      OnClick = AutoSendBtnClick
    end
  end
  object TimingLog: TMemo
    Left = 0
    Top = 27
    Width = 451
    Height = 20
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Log')
    ParentFont = False
    TabOrder = 1
  end
  object TimingMemo: TMemo
    Left = 0
    Top = 47
    Width = 451
    Height = 20
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    TabOrder = 2
  end
  object TimingGrid: TStringGrid
    Left = 33
    Top = 112
    Width = 345
    Height = 129
    ColCount = 9
    Ctl3D = True
    DefaultColWidth = 30
    DefaultRowHeight = 18
    DrawingStyle = gdsGradient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentCtl3D = False
    TabOrder = 3
    OnDblClick = TimingGridDblClick
    OnDrawCell = GridDrawCell
    OnKeyDown = GridKeyDown
  end
end
