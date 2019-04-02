object MarkTab: TMarkTab
  Left = 0
  Top = 0
  Width = 259
  Height = 577
  TabOrder = 0
  DesignSize = (
    259
    577)
  object TimePointLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 253
    Height = 24
    Align = alTop
    Caption = 'TimePointLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clTeal
    Font.Height = -19
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 131
  end
  object LabelSpace: TLabel
    Left = 126
    Top = 44
    Width = 54
    Height = 13
    Caption = 'LabelSpace'
    ParentShowHint = False
    ShowHint = True
  end
  object SendBtn: TSpeedButton
    Left = 126
    Top = 424
    Width = 107
    Height = 22
    Caption = 'Send'
    OnClick = SendBtnClick
  end
  object AutoBtn: TSpeedButton
    Left = 126
    Top = 452
    Width = 107
    Height = 22
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'Auto'
    OnClick = AutoBtnClick
  end
  object ClearBtn: TSpeedButton
    Left = 126
    Top = 480
    Width = 107
    Height = 22
    Caption = 'Clear'
    OnClick = ClearBtnClick
  end
  object LBA: TListBox
    Left = 0
    Top = 30
    Width = 120
    Height = 372
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnKeyPress = LBAKeyPress
  end
  object LBB: TListBox
    Left = 126
    Top = 72
    Width = 120
    Height = 321
    ItemHeight = 13
    TabOrder = 1
    OnKeyPress = LBBKeyPress
  end
  object TimingLog: TEdit
    AlignWithMargins = True
    Left = 0
    Top = 556
    Width = 244
    Height = 21
    Margins.Left = 0
    Margins.Top = 2
    Margins.Right = 15
    Margins.Bottom = 0
    Align = alBottom
    TabOrder = 2
    Text = 'Timing Message'
  end
  object OptionGroup: TRadioGroup
    Left = 3
    Top = 408
    Width = 117
    Height = 137
    Caption = 'Option'
    Items.Strings = (
      '1'
      '2'
      '3'
      '4')
    TabOrder = 3
  end
end
