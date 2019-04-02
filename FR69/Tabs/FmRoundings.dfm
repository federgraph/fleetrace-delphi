object RoundingsTab: TRoundingsTab
  Left = 0
  Top = 0
  Width = 719
  Height = 240
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 719
    Height = 22
    Caption = 'ToolBar'
    TabOrder = 0
    object UpdateBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 71
      Height = 22
      Caption = 'Update'
      OnClick = UpdateBtnClick
    end
    object RaceBtn: TSpeedButton
      Left = 71
      Top = 0
      Width = 114
      Height = 22
      Caption = 'Race'
    end
    object LayoutBtn: TSpeedButton
      Left = 185
      Top = 0
      Width = 88
      Height = 22
      Caption = 'Layout'
      OnClick = LayoutBtnClick
    end
  end
end
