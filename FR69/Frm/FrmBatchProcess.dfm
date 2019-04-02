object FormBatchProcess: TFormBatchProcess
  Left = 221
  Top = 186
  ActiveControl = CloseBtn
  BorderIcons = [biSystemMenu]
  Caption = 'Batch Process'
  ClientHeight = 405
  ClientWidth = 445
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
  OnShow = FormShow
  DesignSize = (
    445
    405)
  PixelsPerInch = 96
  TextHeight = 13
  object InformationLabel: TLabel
    Left = 32
    Top = 96
    Width = 56
    Height = 13
    Caption = 'Information'
  end
  object ProcessTypeLabel: TLabel
    Left = 8
    Top = 8
    Width = 109
    Height = 23
    Caption = 'Process Type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object StartBtn: TButton
    Left = 96
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = StartBtnClick
  end
  object CancelBtn: TButton
    Left = 177
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelBtnClick
  end
  object gbProgress: TGroupBox
    Left = 32
    Top = 127
    Width = 345
    Height = 74
    Caption = 'Progress'
    TabOrder = 2
    object ProgressLabel: TLabel
      Left = 32
      Top = 32
      Width = 70
      Height = 13
      Caption = 'Progress Label'
    end
  end
  object CloseBtn: TButton
    Left = 258
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 3
    OnClick = CloseBtnClick
  end
  object ClearBtn: TButton
    Left = 15
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = ClearBtnClick
  end
  object RequestCombo: TComboBox
    Left = 32
    Top = 230
    Width = 401
    Height = 21
    TabOrder = 5
    Text = 'FR.*.Request.JavaScore.XML'
  end
  object cbCalc: TCheckBox
    Left = 216
    Top = 207
    Width = 97
    Height = 17
    Caption = 'Calc'
    TabOrder = 6
  end
  object EventNameList: TCheckListBox
    Left = 32
    Top = 257
    Width = 264
    Height = 135
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 7
  end
  object InvertAllBtn: TButton
    Left = 310
    Top = 257
    Width = 123
    Height = 25
    Caption = 'Invert All'
    TabOrder = 8
    OnClick = InvertAllBtnClick
  end
  object InvertSelectedBtn: TButton
    Left = 310
    Top = 288
    Width = 123
    Height = 25
    Caption = 'Invert Selected'
    TabOrder = 9
    OnClick = InvertSelectedBtnClick
  end
  object CheckAllBtn: TButton
    Left = 310
    Top = 319
    Width = 123
    Height = 25
    Caption = 'Check All'
    TabOrder = 10
    OnClick = CheckAllBtnClick
  end
  object cbXml: TCheckBox
    Left = 89
    Top = 207
    Width = 97
    Height = 17
    Caption = 'Xml'
    TabOrder = 11
  end
end
