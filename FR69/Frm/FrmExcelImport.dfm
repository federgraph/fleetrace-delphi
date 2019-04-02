object FormExcelImport: TFormExcelImport
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'FR Excel Import'
  ClientHeight = 282
  ClientWidth = 693
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
  object Splitter: TSplitter
    Left = 337
    Top = 25
    Height = 257
    ExplicitLeft = 352
    ExplicitTop = 112
    ExplicitHeight = 100
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 693
    Height = 25
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 21
    Caption = 'spe'
    TabOrder = 0
    object PasteBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 53
      Height = 21
      Caption = 'Paste'
      OnClick = PasteBtnClick
    end
    object TestdataBtn: TSpeedButton
      Left = 53
      Top = 0
      Width = 52
      Height = 21
      Hint = 'Paste testdata'
      Caption = 'Test'
      OnClick = TestdataBtnClick
    end
    object ShowTabsBtn: TSpeedButton
      Left = 105
      Top = 0
      Width = 72
      Height = 21
      Caption = 'Show Tabs'
      OnClick = ShowTabsBtnClick
    end
    object edRecordLineCount: TEdit
      Left = 177
      Top = 0
      Width = 32
      Height = 21
      Hint = 'Record line count'
      TabOrder = 1
      Text = '1'
    end
    object ShuffleBtn: TSpeedButton
      Left = 209
      Top = 0
      Width = 60
      Height = 21
      Hint = 'Reduce record line count to one'
      Caption = 'Shuffle'
      OnClick = ShuffleBtnClick
    end
    object SepBtn: TToolButton
      Left = 269
      Top = 0
      Width = 16
      Style = tbsSeparator
    end
    object DelimiterCombo: TComboBox
      Left = 285
      Top = 0
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object ListCombo: TComboBox
      Left = 365
      Top = 0
      Width = 104
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object ConvertBtn: TSpeedButton
      Left = 469
      Top = 0
      Width = 72
      Height = 21
      Caption = 'Convert'
      OnClick = ConvertBtnClick
    end
    object SendBtn: TSpeedButton
      Left = 541
      Top = 0
      Width = 48
      Height = 21
      Caption = 'Send'
      OnClick = SendBtnClick
    end
    object CloseBtn: TButton
      Left = 589
      Top = 0
      Width = 56
      Height = 21
      Cancel = True
      Caption = 'Close'
      TabOrder = 3
      OnClick = CloseBtnClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 25
    Width = 337
    Height = 257
    Align = alLeft
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WantTabs = True
    WordWrap = False
  end
  object ResultMemo: TMemo
    Left = 416
    Top = 72
    Width = 185
    Height = 89
    Color = clSkyBlue
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'ResultMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WantTabs = True
    WordWrap = False
  end
end
