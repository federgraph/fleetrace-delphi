object FormWorkspaceInfo: TFormWorkspaceInfo
  Left = 0
  Top = 0
  Caption = 'WorkspaceInfo'
  ClientHeight = 319
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelRepository: TLabel
    Left = 16
    Top = 11
    Width = 52
    Height = 13
    Caption = 'Repository'
  end
  object OKBtn: TButton
    Left = 255
    Top = 272
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 336
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = CancelBtnClick
  end
  object RepoCombo: TComboBox
    Left = 104
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = RepoComboChange
  end
  object gbWorkspaceInfo: TGroupBox
    Left = 16
    Top = 47
    Width = 465
    Height = 210
    Caption = 'Workspace Info'
    TabOrder = 2
    object LabelWorkspaceType: TLabel
      Left = 32
      Top = 29
      Width = 80
      Height = 13
      Caption = 'Workspace Type'
    end
    object LabelWorkspaceID: TLabel
      Left = 224
      Top = 29
      Width = 67
      Height = 13
      Caption = 'Workspace ID'
    end
    object LabelWorkspaceUrl: TLabel
      Left = 32
      Top = 82
      Width = 69
      Height = 13
      Caption = 'Workspace Url'
    end
    object LabelWorkspaceRoot: TLabel
      Left = 32
      Top = 136
      Width = 79
      Height = 13
      Caption = 'Workspace Root'
    end
    object TypeCombo: TComboBox
      Left = 32
      Top = 48
      Width = 169
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object UrlCombo: TComboBox
      Left = 32
      Top = 101
      Width = 401
      Height = 21
      TabOrder = 3
      Text = 'UrlCombo'
    end
    object cbAutoSaveIni: TCheckBox
      Left = 320
      Top = 50
      Width = 97
      Height = 17
      Caption = 'AutoSaveIni'
      TabOrder = 2
    end
    object edID: TEdit
      Left = 224
      Top = 48
      Width = 64
      Height = 21
      TabOrder = 1
      Text = 'edID'
    end
    object RootCombo: TComboBox
      Left = 32
      Top = 155
      Width = 401
      Height = 21
      TabOrder = 4
      Text = 'RootCombo'
    end
  end
end
