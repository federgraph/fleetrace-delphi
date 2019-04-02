object FormDocManager: TFormDocManager
  Left = 221
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Select Document Manager'
  ClientHeight = 297
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMdb: TLabel
    Left = 315
    Top = 137
    Width = 114
    Height = 13
    Caption = 'connection definition file'
  end
  object LabelTxt: TLabel
    Left = 315
    Top = 97
    Width = 62
    Height = 13
    Caption = 'text file folder'
  end
  object LabelWeb: TLabel
    Left = 314
    Top = 178
    Width = 94
    Height = 13
    Caption = 'Web Application Url'
  end
  object LabelREST: TLabel
    Left = 315
    Top = 217
    Width = 101
    Height = 13
    Caption = 'REST pages location'
  end
  object LabelLNK: TLabel
    Left = 315
    Top = 57
    Width = 102
    Height = 13
    Caption = 'mapped file extension'
  end
  object LabelXML: TLabel
    Left = 315
    Top = 17
    Width = 61
    Height = 13
    Caption = 'file extension'
  end
  object MdbBtn: TRadioButton
    Left = 24
    Top = 136
    Width = 49
    Height = 17
    Caption = 'MDB'
    TabOrder = 0
  end
  object TxtBtn: TRadioButton
    Left = 24
    Top = 96
    Width = 49
    Height = 17
    Caption = 'TXT'
    TabOrder = 1
  end
  object WebBtn: TRadioButton
    Left = 24
    Top = 176
    Width = 57
    Height = 17
    Caption = 'WEB'
    TabOrder = 2
  end
  object edTxt: TEdit
    Left = 96
    Top = 96
    Width = 213
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = '.\DBEvent\'
  end
  object edMdb: TEdit
    Left = 96
    Top = 136
    Width = 213
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = '.\FleetRace.udl'
  end
  object edWeb: TEdit
    Left = 96
    Top = 176
    Width = 212
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'http://<WebApplicationUrl>/'
  end
  object OKBtn: TButton
    Left = 190
    Top = 256
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 278
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object RESTBtn: TRadioButton
    Left = 24
    Top = 216
    Width = 57
    Height = 17
    Caption = 'REST'
    TabOrder = 8
  end
  object edREST: TEdit
    Left = 96
    Top = 216
    Width = 213
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
    Text = 'http://<WebApplicationUrl>/REST/'
  end
  object LNKBtn: TRadioButton
    Left = 24
    Top = 56
    Width = 49
    Height = 17
    Caption = 'LNK'
    TabOrder = 10
  end
  object edLNK: TEdit
    Left = 96
    Top = 56
    Width = 213
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 11
    Text = '.fr62'
  end
  object XMLBtn: TRadioButton
    Left = 24
    Top = 16
    Width = 49
    Height = 17
    Caption = 'XML'
    TabOrder = 12
  end
  object edXML: TEdit
    Left = 96
    Top = 16
    Width = 213
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 13
    Text = '.fr62x'
  end
end
