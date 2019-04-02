object TimingTab: TTimingTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  object ToolBarTiming: TToolBar
    Left = 0
    Top = 0
    Width = 451
    Height = 27
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 23
    Caption = 'ToolBarTiming'
    TabOrder = 0
    object TestBtn: TSpeedButton
      Left = 0
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
      Left = 56
      Top = 0
      Width = 81
      Height = 23
      Hint = 'send time for random bib'
      Caption = 'Random'
      OnClick = RandomBtnClick
    end
    object AgeBtn: TSpeedButton
      Left = 137
      Top = 0
      Width = 64
      Height = 23
      Hint = 'reset color of recently uses cells'
      Caption = 'Age'
      OnClick = AgeBtnClick
    end
    object SendBtn: TSpeedButton
      Left = 201
      Top = 0
      Width = 56
      Height = 23
      Caption = 'Send'
      OnClick = SendBtnClick
    end
    object AutoSendBtn: TSpeedButton
      Left = 257
      Top = 0
      Width = 88
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
    Color = clSkyBlue
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
