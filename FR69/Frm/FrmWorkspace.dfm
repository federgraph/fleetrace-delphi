object FormWorkspace: TFormWorkspace
  Left = 0
  Top = 0
  Caption = 'Workspace'
  ClientHeight = 396
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelWorkspaceID: TLabel
    Left = 361
    Top = 59
    Width = 67
    Height = 13
    Caption = 'Workspace ID'
  end
  object WorkspaceTypePromt: TLabel
    Left = 8
    Top = 8
    Width = 124
    Height = 13
    Caption = 'Current Workspace Type:'
  end
  object LabelCurrentWorkspace: TLabel
    Left = 147
    Top = 8
    Width = 52
    Height = 13
    Caption = 'User Home'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object WorkspaceIDPrompt: TLabel
    Left = 8
    Top = 27
    Width = 111
    Height = 13
    Caption = 'Current Workspace ID:'
  end
  object LabelCurrentWorkspaceID: TLabel
    Left = 147
    Top = 27
    Width = 6
    Height = 13
    Caption = '1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object OKBtn: TButton
    Left = 209
    Top = 341
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 290
    Top = 341
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = CancelBtnClick
  end
  object edWorkspaceID: TEdit
    Left = 361
    Top = 116
    Width = 51
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object udWorkspaceID: TUpDown
    Left = 412
    Top = 116
    Width = 16
    Height = 21
    Associate = edWorkspaceID
    TabOrder = 3
  end
  object MemoLines: TMemo
    Left = 32
    Top = 199
    Width = 396
    Height = 122
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'MemoLines')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object gbWorkspaceType: TGroupBox
    Left = 32
    Top = 59
    Width = 314
    Height = 126
    Caption = 'Workspace Type'
    TabOrder = 5
    object rbUserHome: TRadioButton
      Left = 18
      Top = 28
      Width = 113
      Height = 17
      Caption = 'User Home'
      TabOrder = 0
      OnClick = rbClick
    end
    object rbAppLocal: TRadioButton
      Left = 18
      Top = 59
      Width = 113
      Height = 17
      Caption = 'App Local'
      TabOrder = 1
      OnClick = rbClick
    end
    object rbLocalDB: TRadioButton
      Left = 153
      Top = 28
      Width = 113
      Height = 17
      Caption = 'Local DB'
      TabOrder = 2
      OnClick = rbClick
    end
    object rbRemoteDB: TRadioButton
      Left = 153
      Top = 59
      Width = 113
      Height = 17
      Caption = 'Remote DB'
      TabOrder = 3
      OnClick = rbClick
    end
    object rbWebService: TRadioButton
      Left = 153
      Top = 92
      Width = 113
      Height = 17
      Caption = 'Web Service'
      TabOrder = 4
      OnClick = rbClick
    end
  end
end
