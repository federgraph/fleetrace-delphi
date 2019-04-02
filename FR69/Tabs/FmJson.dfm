object JsonTab: TJsonTab
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  AutoSize = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 451
    Height = 22
    Caption = 'ToolBar'
    TabOrder = 0
    object UpdateBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Update'
      OnClick = UpdateBtnClick
    end
    object BackupBtn: TSpeedButton
      Left = 72
      Top = 0
      Width = 73
      Height = 22
      Caption = 'Backup'
      OnClick = BackupBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 145
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object ExportBtn: TSpeedButton
      Left = 201
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Export'
      OnClick = ExportBtnClick
    end
  end
end
