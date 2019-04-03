object EntriesTab: TEntriesTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 451
    Height = 23
    Caption = 'ToolBar'
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
      Width = 64
      Height = 23
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object AddBtn: TSpeedButton
      Left = 129
      Top = 0
      Width = 80
      Height = 23
      Caption = 'Add'
      OnClick = AddBtnClick
    end
    object UpdateBtn: TSpeedButton
      Left = 209
      Top = 0
      Width = 71
      Height = 23
      Caption = 'Update'
      OnClick = UpdateBtnClick
    end
    object ClearBtn: TSpeedButton
      Left = 280
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
  end
end
