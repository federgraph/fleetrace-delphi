object FormConnectionProps: TFormConnectionProps
  Left = 221
  Top = 233
  BorderStyle = bsDialog
  Caption = 'Connection Properties'
  ClientHeight = 87
  ClientWidth = 388
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
  object HostLabel: TLabel
    Left = 16
    Top = 21
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object PortLabel: TLabel
    Left = 207
    Top = 21
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object edHost: TEdit
    Left = 16
    Top = 40
    Width = 185
    Height = 21
    TabOrder = 0
    Text = 'edHost'
  end
  object edPort: TEdit
    Left = 207
    Top = 40
    Width = 58
    Height = 21
    TabOrder = 1
    Text = 'edPort'
  end
  object OKBtn: TButton
    Left = 297
    Top = 8
    Width = 66
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 297
    Top = 39
    Width = 66
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
