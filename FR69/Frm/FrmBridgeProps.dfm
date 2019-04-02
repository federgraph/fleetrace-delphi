object FormBridgeProps: TFormBridgeProps
  Left = 132
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Bridge Properties'
  ClientHeight = 144
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object LabelBridgeUrl: TLabel
    Left = 16
    Top = 23
    Width = 46
    Height = 13
    Caption = 'Bridge Url'
  end
  object LabelTaktOut: TLabel
    Left = 16
    Top = 50
    Width = 92
    Height = 13
    Caption = 'Takt Out (seconds)'
  end
  object LabelTaktIn: TLabel
    Left = 16
    Top = 77
    Width = 84
    Height = 13
    Caption = 'Takt In (seconds)'
  end
  object edBridgeUrl: TEdit
    Left = 79
    Top = 20
    Width = 226
    Height = 21
    MaxLength = 48
    TabOrder = 0
  end
  object OKBtn: TButton
    Left = 160
    Top = 109
    Width = 64
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 230
    Top = 109
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object TestBtn: TButton
    Left = 79
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 3
    OnClick = TestBtnClick
  end
  object edTaktOut: TEdit
    Left = 124
    Top = 47
    Width = 30
    Height = 21
    MaxLength = 3
    TabOrder = 4
  end
  object edTaktIn: TEdit
    Left = 124
    Top = 74
    Width = 30
    Height = 21
    MaxLength = 3
    TabOrder = 5
  end
end
