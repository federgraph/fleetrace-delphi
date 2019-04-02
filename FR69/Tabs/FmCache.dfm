object CacheTab: TCacheTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  AutoSize = True
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 451
    Height = 22
    AutoSize = True
    Caption = 'ToolBar'
    TabOrder = 0
    object SynchronizeBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 97
      Height = 22
      Caption = 'Synchronize'
      OnClick = SynchronizeBtnClick
    end
    object OptionsBtn: TSpeedButton
      Left = 97
      Top = 0
      Width = 88
      Height = 22
      Caption = 'Options...'
      OnClick = OptionsBtnClick
    end
    object UpdateBtn: TSpeedButton
      Left = 185
      Top = 0
      Width = 128
      Height = 22
      Caption = 'Update'
      OnClick = UpdateBtnClick
    end
    object CacheLED: TShape
      Left = 313
      Top = 0
      Width = 7
      Height = 22
    end
    object StartBtn: TSpeedButton
      Left = 320
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Start'
      OnClick = StartBtnClick
    end
    object StopBtn: TSpeedButton
      Left = 384
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Stop'
      OnClick = StopBtnClick
    end
    object TurboBtn: TSpeedButton
      Left = 440
      Top = 0
      Width = 57
      Height = 22
      Caption = 'Turbo'
      OnClick = TurboBtnClick
    end
  end
end
