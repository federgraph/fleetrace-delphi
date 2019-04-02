object FormInspector: TFormInspector
  Left = 132
  Top = 140
  Caption = 'Inspector'
  ClientHeight = 316
  ClientWidth = 646
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
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 646
    Height = 27
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 23
    TabOrder = 0
    object LoadBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 65
      Height = 23
      Caption = 'Load'
      OnClick = LoadBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 65
      Top = 0
      Width = 56
      Height = 23
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object OKBtn: TSpeedButton
      Left = 121
      Top = 0
      Width = 56
      Height = 23
      Caption = 'OK'
      OnClick = OKBtnClick
    end
    object CloseBtn: TButton
      Left = 177
      Top = 0
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 0
      OnClick = CloseBtnClick
    end
  end
end
