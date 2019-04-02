object FormLanguage: TFormLanguage
  Left = 353
  Top = 233
  BorderStyle = bsDialog
  Caption = 'Select Language'
  ClientHeight = 123
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ComboLabel: TLabel
    Left = 16
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Language'
  end
  object LanguageCombo: TComboBox
    Left = 16
    Top = 27
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'LanguageCombo'
  end
  object OKBtn: TButton
    Left = 104
    Top = 80
    Width = 65
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 175
    Top = 80
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ExportBtn: TButton
    Left = 151
    Top = 25
    Width = 73
    Height = 25
    Caption = 'Export'
    TabOrder = 3
    OnClick = ExportBtnClick
  end
  object ImportBtn: TButton
    Left = 230
    Top = 25
    Width = 73
    Height = 25
    Caption = 'Import'
    TabOrder = 4
    OnClick = ImportBtnClick
  end
end
