object FormReportNav: TFormReportNav
  Left = 0
  Top = 0
  Caption = 'Inspect Reports'
  ClientHeight = 451
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 29
    Top = 96
    Width = 324
    Height = 153
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 681
    Height = 29
    BorderWidth = 1
    ButtonHeight = 21
    TabOrder = 1
    object edReportSpinner: TEdit
      Left = 0
      Top = 0
      Width = 76
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object udReportSpinner: TUpDown
      Left = 76
      Top = 0
      Width = 16
      Height = 21
      Associate = edReportSpinner
      Min = 1
      Max = 10
      Position = 1
      TabOrder = 1
      OnChangingEx = udReportSpinnerChangingEx
    end
    object ReportBtn: TSpeedButton
      Left = 108
      Top = 0
      Width = 70
      Height = 21
      Caption = 'Report'
      OnClick = ReportBtnClick
    end
  end
end
