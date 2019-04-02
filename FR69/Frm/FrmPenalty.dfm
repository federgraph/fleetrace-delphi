object FormPenalty: TFormPenalty
  Left = 195
  Top = 106
  Caption = 'Penalty Testbed'
  ClientHeight = 485
  ClientWidth = 686
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPenaltyOther: TLabel
    Left = 432
    Top = 252
    Width = 164
    Height = 13
    Caption = 'Accumulated Other Penalty Values'
  end
  object RulerLabel: TLabel
    Left = 80
    Top = 40
    Width = 128
    Height = 16
    Caption = 'FEDCBA9876543210'
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object BitsLabel: TLabel
    Left = 80
    Top = 58
    Width = 128
    Height = 16
    Caption = '0000000000000000'
    Font.Charset = ANSI_CHARSET
    Font.Color = clPurple
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LabelPenaltyOtherValue: TLabel
    Left = 517
    Top = 103
    Width = 94
    Height = 13
    Caption = 'Other Penalty Value'
  end
  object StringLabel: TLabel
    Left = 81
    Top = 8
    Width = 8
    Height = 16
    Caption = '0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object MsgLabel: TLabel
    Left = 81
    Top = 24
    Width = 8
    Height = 16
    Caption = '0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 42
    Top = 24
    Width = 32
    Height = 16
    Caption = 'Msg:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 18
    Top = 8
    Width = 56
    Height = 16
    Caption = 'String:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 34
    Top = 58
    Width = 40
    Height = 16
    Caption = 'Bits:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 26
    Top = 40
    Width = 48
    Height = 16
    Caption = 'Ruler:'
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 257
    Top = 24
    Width = 416
    Height = 16
    Caption = 'Time Formats: ss | mm:ss | hh:mm:ss |  mmss | hhmmss'
    Font.Charset = ANSI_CHARSET
    Font.Color = clOlive
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 257
    Top = 40
    Width = 184
    Height = 16
    Caption = 'Percent Values: Integer'
    Font.Charset = ANSI_CHARSET
    Font.Color = clOlive
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 257
    Top = 58
    Width = 360
    Height = 16
    Caption = 'Point Format: use localized decimal separator'
    Font.Charset = ANSI_CHARSET
    Font.Color = clOlive
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 415
    Top = 8
    Width = 256
    Height = 16
    Caption = 'Format of "Other Penalty Values"'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object rgPenaltyDSQ: TRadioGroup
    Left = 8
    Top = 95
    Width = 131
    Height = 163
    Caption = 'DSQ Penalty'
    Columns = 2
    Items.Strings = (
      '-dsq'
      'DSQ'
      'DNE'
      'RAF'
      'OCS'
      'BFD'
      'DGM'
      'UFD')
    TabOrder = 0
    OnClick = rgPenaltyDSQClick
  end
  object rgPenaltyNoFinish: TRadioGroup
    Left = 145
    Top = 95
    Width = 105
    Height = 163
    Caption = 'NoFinish Penalty'
    Items.Strings = (
      '-f'
      'TLE'
      'DNF'
      'DNS'
      'DNC')
    TabOrder = 1
    OnClick = rgPenaltyNoFinishClick
  end
  object cbPenaltyOther: TCheckListBox
    Left = 256
    Top = 271
    Width = 416
    Height = 186
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Items.Strings = (
      'STP $0008 standard penalty'
      'TIM $0010 time limit'
      'ZFP $0020'
      'AVG $0040 average'
      'SCP $0080 scoring penalty, % of finish position'
      'RDG $0100 redress given, points'
      'MAN $0200 manual, points'
      'CNF $0400 check-in failure'
      'TMP $0800 scoring time penalty, % of time'
      'DPI $1000 descretionary penalty imposed')
    ParentFont = False
    TabOrder = 2
  end
  object rgPenaltyOther: TRadioGroup
    Left = 256
    Top = 95
    Width = 136
    Height = 163
    Caption = 'Other Penalty'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'STP'
      'TIM'
      'ZFP'
      'AVG'
      'SCP'
      'RDG'
      'MAN'
      'CNF'
      'TMP'
      'DPI')
    TabOrder = 3
  end
  object edPenaltyOther: TEdit
    Left = 398
    Top = 99
    Width = 113
    Height = 24
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = '20'
  end
  object AddPenaltyOtherBtn: TButton
    Left = 438
    Top = 129
    Width = 73
    Height = 25
    Caption = 'Add'
    TabOrder = 5
    OnClick = AddPenaltyOtherBtnClick
  end
  object RemovePenaltyOtherBtn: TButton
    Left = 438
    Top = 160
    Width = 73
    Height = 25
    Caption = 'Remove'
    TabOrder = 6
    OnClick = RemovePenaltyOtherBtnClick
  end
  object OKBtn: TButton
    Left = 485
    Top = 212
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 7
  end
  object CancelBtn: TButton
    Left = 566
    Top = 212
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object ClearBtn: TButton
    Left = 544
    Top = 129
    Width = 129
    Height = 25
    Caption = 'Clear Penalty Object'
    TabOrder = 9
    OnClick = ClearBtnClick
  end
  object Memo: TMemo
    Left = 8
    Top = 271
    Width = 242
    Height = 186
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ReadOnly = True
    TabOrder = 10
  end
  object ClearStaticsBtn: TButton
    Left = 544
    Top = 160
    Width = 129
    Height = 25
    Caption = 'Clear Sailtime Statics'
    TabOrder = 11
    OnClick = ClearStaticsBtnClick
  end
end
