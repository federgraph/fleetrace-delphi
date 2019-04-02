object RaceTab: TRaceTab
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
    Height = 22
    AutoSize = True
    Caption = 'ToolBar'
    TabOrder = 0
    object RaceDownBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 23
      Height = 22
      Caption = '<'
      OnClick = RaceDownBtnClick
    end
    object RaceBtn: TSpeedButton
      Left = 23
      Top = 0
      Width = 58
      Height = 22
      Caption = 'Race1'
      Enabled = False
    end
    object RaceUpBtn: TSpeedButton
      Left = 81
      Top = 0
      Width = 23
      Height = 22
      Caption = '>'
      OnClick = RaceUpBtnClick
    end
    object ITDownBtn: TSpeedButton
      Left = 104
      Top = 0
      Width = 23
      Height = 22
      Caption = '<'
      OnClick = ITDownBtnClick
    end
    object ITBtn: TSpeedButton
      Left = 127
      Top = 0
      Width = 41
      Height = 22
      Caption = 'IT1'
      Enabled = False
    end
    object ITUpBtn: TSpeedButton
      Left = 168
      Top = 0
      Width = 23
      Height = 22
      Caption = '>'
      OnClick = ITUpBtnClick
    end
    object ToMRankBtn: TSpeedButton
      Left = 191
      Top = 0
      Width = 98
      Height = 22
      Caption = 'MRank <-- FT'
      OnClick = ToMRankBtnClick
    end
    object FromMRankBtn: TSpeedButton
      Left = 289
      Top = 0
      Width = 98
      Height = 22
      Caption = 'MRank --> FT'
      OnClick = FromMRankBtnClick
    end
    object UpdateEventBtn: TSpeedButton
      Left = 387
      Top = 0
      Width = 118
      Height = 22
      Caption = 'Update Event'
      OnClick = UpdateEventBtnClick
    end
    object LayoutBtn: TSpeedButton
      Left = 505
      Top = 0
      Width = 63
      Height = 22
      Caption = 'Layout'
      OnClick = LayoutBtnClick
    end
  end
end
