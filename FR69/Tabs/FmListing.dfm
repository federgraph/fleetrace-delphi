object ListingTab: TListingTab
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 320
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
  end
end
