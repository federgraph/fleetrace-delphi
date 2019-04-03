object EventTab: TEventTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 721
    Height = 22
    Caption = 'ToolBar'
    TabOrder = 0
    ExplicitWidth = 451
    object PointsBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 65
      Height = 22
      Hint = 'show calculated points'
      Caption = 'Points'
      OnClick = LayoutBtnClick
    end
    object FinishBtn: TSpeedButton
      Tag = 1
      Left = 65
      Top = 0
      Width = 64
      Height = 22
      Hint = 'show finish position input'
      Caption = 'Finish'
      OnClick = LayoutBtnClick
    end
    object StrictBtn: TSpeedButton
      Left = 129
      Top = 0
      Width = 56
      Height = 22
      Hint = 'enforces contiguous range of finish positions'
      Caption = 'Strict'
      OnClick = StrictBtnClick
    end
    object ThrowoutMinusBtn: TSpeedButton
      Left = 185
      Top = 0
      Width = 23
      Height = 22
      Caption = 'T-'
      OnClick = ThrowoutMinusBtnClick
    end
    object ThrowoutBtn: TSpeedButton
      Left = 208
      Top = 0
      Width = 33
      Height = 22
      Caption = '0'
      OnClick = ThrowoutBtnClick
    end
    object ThrowoutPlusBtn: TSpeedButton
      Left = 241
      Top = 0
      Width = 23
      Height = 22
      Caption = 'T+'
      OnClick = ThrowoutPlusBtnClick
    end
    object DollarBtn: TSpeedButton
      Left = 264
      Top = 0
      Width = 23
      Height = 22
      Hint = 'swap race enabled'
      Caption = 'X'
      OnClick = DollarBtnClick
    end
    object ColorBtn: TSpeedButton
      Left = 287
      Top = 0
      Width = 58
      Height = 22
      Hint = 'color mode: F(leet), E(rror), N(None)'
      Caption = 'Color'
      OnClick = ColorBtnClick
    end
    object SelectRaceBtn: TSpeedButton
      Left = 345
      Top = 0
      Width = 95
      Height = 22
      Caption = 'Select Race'
      OnClick = SelectRaceBtnClick
    end
    object UpdateBtn: TSpeedButton
      Left = 440
      Top = 0
      Width = 62
      Height = 22
      Hint = 'check input and update error colors'
      Caption = 'Update'
      OnClick = UpdateBtnClick
    end
    object CalcBtn: TSpeedButton
      Left = 502
      Top = 0
      Width = 64
      Height = 22
      Hint = 'fleet assignment'
      Caption = 'Calc'
      OnClick = CalcBtnClick
    end
    object UndoBtn: TSpeedButton
      Left = 566
      Top = 0
      Width = 68
      Height = 22
      Caption = 'Undo'
      OnClick = UndoBtnClick
    end
    object RedoBtn: TSpeedButton
      Left = 634
      Top = 0
      Width = 69
      Height = 22
      Caption = 'Redo'
      OnClick = RedoBtnClick
    end
  end
end
