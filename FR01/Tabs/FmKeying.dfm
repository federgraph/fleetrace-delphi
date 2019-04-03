object KeyTab: TKeyTab
  Left = 0
  Top = 0
  Width = 625
  Height = 240
  TabOrder = 0
  object HelpLabel: TLabel
    Left = 29
    Top = 28
    Width = 324
    Height = 13
    Caption = 
      'Enter Bow number to create Finish message for boat in Race N and' +
      ' '
  end
  object MsgLabel: TLabel
    Left = 115
    Top = 67
    Width = 44
    Height = 13
    Caption = 'MsgLabel'
  end
  object ResultLabel: TLabel
    Left = 80
    Top = 104
    Width = 100
    Height = 23
    Caption = 'ResultLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -21
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object NameLabel: TLabel
    Left = 80
    Top = 133
    Width = 409
    Height = 13
    AutoSize = False
    Caption = 'NameLabel'
    WordWrap = True
  end
  object SeriesLabel: TLabel
    Left = 29
    Top = 168
    Width = 96
    Height = 23
    Caption = 'SeriesLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clPurple
    Font.Height = -21
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 625
    Height = 22
    Caption = 'ToolBar'
    TabOrder = 0
    Transparent = True
    object RaceDownBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 23
      Height = 22
      Caption = 'R-'
      OnClick = RaceDownBtnClick
    end
    object RaceBtn: TSpeedButton
      Left = 23
      Top = 0
      Width = 34
      Height = 22
      Hint = 'send test message'
      Caption = 'R0'
      ParentShowHint = False
      ShowHint = True
      OnClick = RaceBtnClick
    end
    object RaceUpBtn: TSpeedButton
      Left = 57
      Top = 0
      Width = 23
      Height = 22
      Caption = 'R+'
      OnClick = RaceUpBtnClick
    end
    object ClearBtn: TSpeedButton
      Left = 80
      Top = 0
      Width = 88
      Height = 22
      Caption = 'Clear Race'
      OnClick = ClearBtnClick
    end
    object SendBtn: TSpeedButton
      Left = 168
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Send'
      OnClick = SendBtnClick
    end
    object AutoSendBtn: TSpeedButton
      Left = 224
      Top = 0
      Width = 81
      Height = 22
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'AutoSend'
    end
  end
  object edKey: TEdit
    Left = 53
    Top = 64
    Width = 56
    Height = 21
    TabOrder = 1
    Text = 'edKey'
    OnKeyPress = edKeyKeyPress
    OnKeyUp = edKeyKeyUp
  end
  object cbWantMessageBeep: TCheckBox
    Left = 296
    Top = 66
    Width = 86
    Height = 17
    Caption = 'play sound.'
    TabOrder = 2
  end
end
