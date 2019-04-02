object FormPages: TFormPages
  Left = 0
  Top = 0
  Caption = 'Pages'
  ClientHeight = 291
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object CheckListBox: TCheckListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 273
    ItemHeight = 13
    TabOrder = 0
  end
  object OKBtn: TButton
    Left = 223
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 223
    Top = 39
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
