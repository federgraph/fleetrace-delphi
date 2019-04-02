object FormScoringModule: TFormScoringModule
  Left = 221
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Scoring Module'
  ClientHeight = 352
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 480
    Top = 24
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 480
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBoxScoringModule: TGroupBox
    Left = 8
    Top = 16
    Width = 457
    Height = 321
    Caption = 'Scoring Module Selection and Configuration'
    TabOrder = 2
    object DLLNameLabel: TLabel
      Left = 160
      Top = 128
      Width = 51
      Height = 13
      Caption = 'DLL Name'
    end
    object PortLabel: TLabel
      Left = 312
      Top = 192
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object HostLabel: TLabel
      Left = 192
      Top = 192
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object UrlLabel: TLabel
      Left = 424
      Top = 264
      Width = 13
      Height = 13
      Caption = 'Url'
    end
    object SimpleTestBtn: TRadioButton
      Left = 16
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Simple Test'
      TabOrder = 0
    end
    object InlineBtn: TRadioButton
      Left = 16
      Top = 72
      Width = 193
      Height = 17
      Caption = 'Inline (compiled into executable)'
      TabOrder = 1
    end
    object DLLBtn: TRadioButton
      Left = 16
      Top = 104
      Width = 169
      Height = 17
      Caption = 'DLL (pass data object)'
      TabOrder = 2
    end
    object RemoteBtn: TRadioButton
      Left = 16
      Top = 168
      Width = 113
      Height = 17
      Caption = 'Remote (send XML)'
      TabOrder = 3
    end
    object edDLLFileName: TEdit
      Left = 56
      Top = 128
      Width = 97
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'JS01.dll'
    end
    object edHost: TEdit
      Left = 56
      Top = 192
      Width = 129
      Height = 21
      TabOrder = 5
      Text = 'localhost'
    end
    object edPort: TEdit
      Left = 240
      Top = 192
      Width = 65
      Height = 21
      MaxLength = 8
      TabOrder = 6
      Text = '3037'
    end
    object WebServiceBtn: TRadioButton
      Left = 16
      Top = 240
      Width = 113
      Height = 17
      Caption = 'Web Service'
      TabOrder = 7
    end
    object edServiceUrl: TEdit
      Left = 56
      Top = 264
      Width = 361
      Height = 21
      TabOrder = 8
      Text = 'http://thinkpad/FR42/'
    end
    object cbFR81: TCheckBox
      Left = 114
      Top = 241
      Width = 97
      Height = 17
      Caption = 'FR81 (Java)'
      TabOrder = 9
    end
  end
end
