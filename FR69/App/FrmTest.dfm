object FormTest: TFormTest
  Left = 0
  Top = 0
  Caption = 'FormTest'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 8
    Top = 64
    Width = 217
    Height = 329
    ItemHeight = 13
    TabOrder = 0
    OnKeyUp = ListBoxKeyUp
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 784
    Height = 29
    Caption = 'ToolBar'
    TabOrder = 1
    object StatusBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Status'
      OnClick = StatusBtnClick
    end
    object ReportBtn: TSpeedButton
      Left = 72
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Report'
      OnClick = ReportBtnClick
    end
    object DataBtn: TSpeedButton
      Left = 128
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Data'
      OnClick = DataBtnClick
    end
    object TestBtn: TSpeedButton
      Left = 192
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Test'
      OnClick = TestBtnClick
    end
    object DefaultBtn: TSpeedButton
      Left = 256
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Default'
      OnClick = DefaultBtnClick
    end
    object InitBtn: TSpeedButton
      Left = 328
      Top = 0
      Width = 64
      Height = 22
      Caption = 'Ini'
      OnClick = InitBtnClick
    end
    object AllBtn: TSpeedButton
      Left = 392
      Top = 0
      Width = 65
      Height = 22
      Caption = 'All'
      OnClick = AllBtnClick
    end
  end
  object Panel: TPanel
    Left = 240
    Top = 136
    Width = 457
    Height = 393
    TabOrder = 2
    object Memo: TMemo
      Left = 64
      Top = 187
      Width = 273
      Height = 153
      Lines.Strings = (
        'Memo')
      TabOrder = 0
    end
    object TestMemo: TMemo
      Left = 32
      Top = 24
      Width = 185
      Height = 89
      Lines.Strings = (
        'TestMemo')
      TabOrder = 1
    end
  end
end
