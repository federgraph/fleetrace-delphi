object FormSelectName: TFormSelectName
  Left = 177
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Select Name'
  ClientHeight = 424
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnKeyUp = FormKeyUp
  DesignSize = (
    318
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object PromptLabel: TLabel
    Left = 8
    Top = 16
    Width = 56
    Height = 13
    Caption = 'PrompLabel'
  end
  object ListBox: TListBox
    Left = 8
    Top = 32
    Width = 302
    Height = 337
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnKeyUp = ListBoxKeyUp
  end
  object OKBtn: TButton
    Left = 111
    Top = 375
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 192
    Top = 375
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = CancelBtnClick
  end
end
