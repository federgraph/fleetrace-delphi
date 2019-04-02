object FormBridgeManager: TFormBridgeManager
  Left = 88
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Bridge Manager'
  ClientHeight = 468
  ClientWidth = 610
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
  DesignSize = (
    610
    468)
  PixelsPerInch = 96
  TextHeight = 13
  object com: TGroupBox
    Left = 8
    Top = 8
    Width = 505
    Height = 193
    Caption = 'Bridge Provider'
    TabOrder = 0
    object rbMock: TRadioButton
      Left = 16
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Mock'
      TabOrder = 0
      OnClick = rbMockClick
    end
    object rbSwitch: TRadioButton
      Left = 16
      Top = 47
      Width = 81
      Height = 17
      Caption = 'Switch'
      TabOrder = 1
      OnClick = rbSwitchClick
    end
    object rbBridge: TRadioButton
      Left = 16
      Top = 94
      Width = 81
      Height = 17
      Caption = 'Bridge'
      TabOrder = 2
      OnClick = rbBridgeClick
    end
    object BridgeCombo: TComboBox
      Left = 103
      Top = 92
      Width = 298
      Height = 21
      Style = csDropDownList
      DropDownCount = 12
      TabOrder = 5
      OnClick = BridgeComboClick
    end
    object SwitchCombo: TComboBox
      Left = 103
      Top = 53
      Width = 298
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnClick = SwitchComboClick
    end
    object SwitchTemplateBtn: TButton
      Left = 407
      Top = 51
      Width = 75
      Height = 25
      Caption = 'Edit...'
      TabOrder = 4
      OnClick = SwitchTemplateBtnClick
    end
    object BridgeTempateBtn: TButton
      Left = 407
      Top = 90
      Width = 75
      Height = 25
      Caption = 'Edit...'
      TabOrder = 6
      OnClick = BridgeTempateBtnClick
    end
    object rbOutput: TRadioButton
      Left = 16
      Top = 134
      Width = 81
      Height = 17
      Caption = 'Output'
      TabOrder = 7
      OnClick = rbOutputClick
    end
    object OutputCombo: TComboBox
      Left = 103
      Top = 132
      Width = 298
      Height = 21
      Style = csDropDownList
      DropDownCount = 12
      TabOrder = 8
      OnClick = OutputComboClick
    end
  end
  object OKBtn: TButton
    Left = 525
    Top = 36
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 525
    Top = 67
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Memo: TMemo
    Left = 8
    Top = 224
    Width = 592
    Height = 234
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clSkyBlue
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
    WordWrap = False
  end
end
