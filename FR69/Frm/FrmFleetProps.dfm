object FormFleetProps: TFormFleetProps
  Left = 280
  Top = 256
  BorderStyle = bsDialog
  Caption = 'Fleet Properties'
  ClientHeight = 152
  ClientWidth = 267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTargetFleetSize: TLabel
    Left = 71
    Top = 48
    Width = 122
    Height = 13
    Caption = 'TargetFleetSize (Integer)'
  end
  object LabelFirstFinalRace: TLabel
    Left = 71
    Top = 75
    Width = 114
    Height = 13
    Caption = 'FirstFinalRace (Integer)'
  end
  object cbUseFleets: TCheckBox
    Left = 16
    Top = 14
    Width = 97
    Height = 17
    Caption = 'Use Fleets'
    TabOrder = 0
  end
  object edTargetFleetSize: TEdit
    Left = 16
    Top = 45
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '30'
  end
  object OKBtn: TButton
    Left = 88
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 169
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object edFirstFinalRace: TEdit
    Left = 16
    Top = 72
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '6'
  end
end
