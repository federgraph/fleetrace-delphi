object FormCacheOptions: TFormCacheOptions
  Left = 221
  Top = 233
  BorderStyle = bsDialog
  Caption = 'Cache Options'
  ClientHeight = 139
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object LabelIdleDelay: TLabel
    Left = 288
    Top = 40
    Width = 47
    Height = 13
    Caption = 'Idle Delay'
  end
  object LabelTicks: TLabel
    Left = 16
    Top = 8
    Width = 126
    Height = 13
    Caption = '10 ... 4960 ms (logarithmic)'
  end
  object NewValue: TLabel
    Left = 48
    Top = 104
    Width = 49
    Height = 13
    Alignment = taRightJustify
    Caption = 'NewValue'
    Color = clWindow
    ParentColor = False
  end
  object OldLabel: TLabel
    Left = 8
    Top = 88
    Width = 17
    Height = 13
    Caption = 'old:'
  end
  object NewLabel: TLabel
    Left = 8
    Top = 104
    Width = 23
    Height = 13
    Caption = 'new:'
  end
  object OldValue: TLabel
    Left = 54
    Top = 88
    Width = 43
    Height = 13
    Alignment = taRightJustify
    Caption = 'OldValue'
    Color = clWindow
    ParentColor = False
  end
  object TrackBar: TTrackBar
    Left = 56
    Top = 32
    Width = 225
    Height = 33
    Max = 30
    Min = 10
    Position = 10
    TabOrder = 0
    OnChange = TrackBarChange
  end
  object OKBtn: TButton
    Left = 184
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cbActive: TCheckBox
    Left = 232
    Top = 8
    Width = 73
    Height = 17
    Caption = 'Active'
    TabOrder = 2
  end
end
