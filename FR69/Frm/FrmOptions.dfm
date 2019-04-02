object FormOptions: TFormOptions
  Left = 88
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Event Params'
  ClientHeight = 176
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblStarlistCount: TLabel
    Left = 80
    Top = 40
    Width = 62
    Height = 13
    Caption = 'StartlistCount'
  end
  object lblTCount: TLabel
    Left = 80
    Top = 72
    Width = 38
    Height = 13
    Caption = 'ITCount'
  end
  object lblRaceCount: TLabel
    Left = 80
    Top = 104
    Width = 54
    Height = 13
    Caption = 'RaceCount'
  end
  object edStartlistCount: TEdit
    Left = 24
    Top = 32
    Width = 33
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = '0'
  end
  object udStartlistCount: TUpDown
    Left = 57
    Top = 32
    Width = 16
    Height = 21
    Associate = edStartlistCount
    TabOrder = 1
  end
  object edITCount: TEdit
    Left = 24
    Top = 64
    Width = 33
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    Text = '0'
  end
  object udITCount: TUpDown
    Left = 57
    Top = 64
    Width = 16
    Height = 21
    Associate = edITCount
    TabOrder = 3
  end
  object edRaceCount: TEdit
    Left = 24
    Top = 96
    Width = 33
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = '0'
  end
  object udRaceCount: TUpDown
    Left = 57
    Top = 96
    Width = 16
    Height = 21
    Associate = edRaceCount
    TabOrder = 5
  end
  object OKBtn: TButton
    Left = 160
    Top = 134
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 241
    Top = 134
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
end
