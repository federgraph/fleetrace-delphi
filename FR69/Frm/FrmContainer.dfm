object FormContainer: TFormContainer
  Left = 88
  Top = 186
  BorderIcons = [biSystemMenu]
  Caption = 'Properties'
  ClientHeight = 356
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 0
    Top = 0
    Width = 105
    Height = 356
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBoxClick
    OnDblClick = ListBoxDblClick
    OnKeyUp = ListBoxKeyUp
  end
  object Panel: TPanel
    Left = 120
    Top = 56
    Width = 321
    Height = 241
    TabOrder = 1
    object CaptionPanel: TPanel
      Left = 1
      Top = 1
      Width = 319
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Titel'
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object DetailPanel: TPanel
      Left = 1
      Top = 33
      Width = 319
      Height = 207
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
end
