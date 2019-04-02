object FormSwitchProps: TFormSwitchProps
  Left = 177
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Switch Properties'
  ClientHeight = 441
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnShow = FormShow
  DesignSize = (
    495
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelSwitchHost: TLabel
    Left = 8
    Top = 73
    Width = 57
    Height = 13
    Caption = 'Switch Host'
  end
  object LabelSwitchPort: TLabel
    Left = 11
    Top = 98
    Width = 54
    Height = 13
    Caption = 'Switch Port'
  end
  object LabelSwitchPortHTTP: TLabel
    Left = 215
    Top = 98
    Width = 86
    Height = 13
    Caption = 'Switch Port HTTP'
  end
  object LabelRouterHost: TLabel
    Left = 8
    Top = 25
    Width = 57
    Height = 13
    Caption = 'Router Host'
  end
  object OKBtn: TButton
    Left = 272
    Top = 406
    Width = 64
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object edSwitchHost: TEdit
    Left = 71
    Top = 68
    Width = 222
    Height = 21
    MaxLength = 24
    TabOrder = 1
  end
  object edSwitchPort: TEdit
    Left = 71
    Top = 95
    Width = 66
    Height = 21
    MaxLength = 5
    TabOrder = 2
  end
  object Memo: TMemo
    Left = 8
    Top = 124
    Width = 479
    Height = 271
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object DownloadBtn: TButton
    Left = 130
    Top = 406
    Width = 136
    Height = 25
    Caption = 'Test HTTP Download'
    TabOrder = 4
    OnClick = DownloadBtnClick
  end
  object edSwitchPortHTTP: TEdit
    Left = 143
    Top = 95
    Width = 66
    Height = 21
    MaxLength = 5
    TabOrder = 5
  end
  object edRouterHost: TEdit
    Left = 71
    Top = 22
    Width = 222
    Height = 21
    MaxLength = 24
    TabOrder = 6
  end
  object cbUseRouterHost: TCheckBox
    Left = 299
    Top = 24
    Width = 97
    Height = 17
    Caption = 'use Router'
    TabOrder = 7
  end
  object CancelBtn: TButton
    Left = 342
    Top = 406
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
end
