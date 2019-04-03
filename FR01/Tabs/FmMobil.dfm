object MobilTab: TMobilTab
  Left = 0
  Top = 0
  Width = 640
  Height = 320
  TabOrder = 0
  object EventLabel: TLabel
    Left = 479
    Top = 259
    Width = 108
    Height = 13
    Caption = 'Event Rank and Points'
  end
  object RaceLabel: TLabel
    Left = 388
    Top = 106
    Width = 53
    Height = 13
    Caption = 'Race Value'
  end
  object RaceDownBtn: TButton
    Left = 16
    Top = 45
    Width = 39
    Height = 25
    Caption = 'R-'
    TabOrder = 0
    OnClick = RaceDownBtnClick
  end
  object RaceBtn: TButton
    Left = 61
    Top = 45
    Width = 76
    Height = 25
    Caption = 'Race'
    Enabled = False
    TabOrder = 1
  end
  object RaceUpBtn: TButton
    Left = 143
    Top = 45
    Width = 41
    Height = 25
    Caption = 'R+'
    TabOrder = 2
    OnClick = RaceUpBtnClick
  end
  object ThrowoutsDownBtn: TButton
    Left = 16
    Top = 14
    Width = 39
    Height = 25
    Caption = 'T-'
    TabOrder = 3
    OnClick = ThrowoutsDownBtnClick
  end
  object edPoints: TEdit
    Left = 414
    Top = 256
    Width = 59
    Height = 21
    Alignment = taRightJustify
    Enabled = False
    TabOrder = 6
    Text = 'Points'
  end
  object Memo: TMemo
    Left = 368
    Top = 152
    Width = 254
    Height = 98
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    TabOrder = 8
  end
  object edRank: TEdit
    Left = 368
    Top = 256
    Width = 40
    Height = 21
    Alignment = taRightJustify
    Enabled = False
    TabOrder = 7
    Text = 'Rank'
  end
  object ThrowoutsBtn: TButton
    Left = 61
    Top = 14
    Width = 76
    Height = 25
    Caption = '0'
    Enabled = False
    TabOrder = 4
  end
  object ThrowoutsUpBtn: TButton
    Left = 143
    Top = 14
    Width = 41
    Height = 25
    Caption = 'T+'
    TabOrder = 5
    OnClick = ThrowoutsUpBtnClick
  end
  object edCommand: TEdit
    Left = 443
    Top = 125
    Width = 98
    Height = 24
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    Text = 'Command'
  end
  object SubmitBtn: TButton
    Left = 547
    Top = 121
    Width = 75
    Height = 25
    Caption = 'Submit'
    TabOrder = 10
    OnClick = SubmitBtnClick
  end
  object GridContainer: TPanel
    Left = 16
    Top = 88
    Width = 337
    Height = 209
    TabOrder = 11
  end
end
