object CategoryTab: TCategoryTab
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
    Caption = 'ToolBar'
    TabOrder = 0
    object HtmlFragmentBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 72
      Height = 22
      GroupIndex = 1
      Down = True
      Caption = 'Fragment'
      OnClick = HtmlFragmentBtnClick
    end
    object HtmlDocBtn: TSpeedButton
      Left = 72
      Top = 0
      Width = 64
      Height = 22
      GroupIndex = 1
      Caption = 'Doc'
      OnClick = HtmlDocBtnClick
    end
    object XmlBtn: TSpeedButton
      Left = 136
      Top = 0
      Width = 48
      Height = 22
      GroupIndex = 1
      Caption = 'Xml'
      OnClick = XmlBtnClick
    end
    object TextBtn: TSpeedButton
      Left = 184
      Top = 0
      Width = 48
      Height = 22
      GroupIndex = 1
      Caption = 'Txt'
      OnClick = TextBtnClick
    end
    object UpdateBtn: TSpeedButton
      Left = 232
      Top = 0
      Width = 72
      Height = 22
      OnClick = UpdateBtnClick
    end
    object BackupBtn: TSpeedButton
      Left = 304
      Top = 0
      Width = 73
      Height = 22
      Caption = 'Backup'
      OnClick = BackupBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 377
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object BatchBtn: TSpeedButton
      Left = 433
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Batch'
      OnClick = BatchBtnClick
    end
    object ExportBtn: TSpeedButton
      Left = 489
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Export'
      OnClick = ExportBtnClick
    end
  end
end
