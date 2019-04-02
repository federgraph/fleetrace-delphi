object FormUniquaProps: TFormUniquaProps
  Left = 132
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Uniqua Properties'
  ClientHeight = 147
  ClientWidth = 468
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
    Left = 376
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
    Left = 376
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbUniqua: TGroupBox
    Left = 17
    Top = 16
    Width = 336
    Height = 113
    Caption = 'Rangliste'
    TabOrder = 2
    object lblGemeldet: TLabel
      Left = 24
      Top = 24
      Width = 45
      Height = 13
      Caption = 'Gemeldet'
    end
    object lblGezeitet: TLabel
      Left = 112
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Gezeitet'
    end
    object lblGesegelt: TLabel
      Left = 192
      Top = 24
      Width = 42
      Height = 13
      Caption = 'Gesegelt'
    end
    object lblFaktor: TLabel
      Left = 272
      Top = 24
      Width = 30
      Height = 13
      Caption = 'Faktor'
    end
    object edGemeldet: TEdit
      Left = 32
      Top = 40
      Width = 33
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object udGemeldet: TUpDown
      Left = 65
      Top = 40
      Width = 16
      Height = 21
      Associate = edGemeldet
      Max = 500
      TabOrder = 1
    end
    object edGezeitet: TEdit
      Left = 112
      Top = 40
      Width = 33
      Height = 21
      TabOrder = 2
      Text = '0'
    end
    object udGezeitet: TUpDown
      Left = 145
      Top = 40
      Width = 16
      Height = 21
      Associate = edGezeitet
      Max = 500
      TabOrder = 3
    end
    object edGesegelt: TEdit
      Left = 192
      Top = 40
      Width = 33
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object udGesegelt: TUpDown
      Left = 225
      Top = 40
      Width = 16
      Height = 21
      Associate = edGesegelt
      Max = 500
      TabOrder = 5
    end
    object cbLock: TCheckBox
      Left = 32
      Top = 80
      Width = 281
      Height = 17
      Caption = 'Override calculated Values'
      TabOrder = 6
      OnClick = cbLockClick
    end
    object edFaktor: TEdit
      Left = 272
      Top = 40
      Width = 41
      Height = 21
      TabOrder = 7
      Text = '110'
    end
    object cbShowCupColumn: TCheckBox
      Left = 205
      Top = 80
      Width = 108
      Height = 17
      Caption = 'Show cup column'
      TabOrder = 8
    end
  end
end
