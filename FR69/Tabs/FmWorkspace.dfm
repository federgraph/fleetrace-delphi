object WorkspaceTab: TWorkspaceTab
  Left = 0
  Top = 0
  Width = 558
  Height = 240
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 558
    Height = 22
    TabOrder = 0
    object ShowDefaultBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 71
      Height = 22
      Caption = 'Default'
      OnClick = ShowDefaultBtnClick
    end
    object ShowComboBtn: TSpeedButton
      Left = 71
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Combo'
      OnClick = ShowComboBtnClick
    end
    object LoadBtn: TSpeedButton
      Left = 135
      Top = 0
      Width = 65
      Height = 22
      Caption = 'Load'
      OnClick = LoadBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 200
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object InitMemoBtn: TSpeedButton
      Left = 264
      Top = 0
      Width = 105
      Height = 22
      Caption = 'Init from Memo'
      OnClick = InitMemoBtnClick
    end
    object InitFileBtn: TSpeedButton
      Left = 369
      Top = 0
      Width = 119
      Height = 22
      Caption = 'Init from File'
      OnClick = InitFileBtnClick
    end
  end
  object Memo: TMemo
    Left = 72
    Top = 80
    Width = 185
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
