object FormSchedule: TFormSchedule
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Schedule'
  ClientHeight = 522
  ClientWidth = 600
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
  object Splitter: TSplitter
    Left = 0
    Top = 164
    Width = 600
    Height = 3
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    ExplicitLeft = 8
    ExplicitTop = 93
    ExplicitWidth = 575
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 600
    Height = 27
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 23
    TabOrder = 0
    object PreviewBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 82
      Height = 23
      Caption = 'Preview'
      OnClick = PreviewBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 82
      Top = 0
      Width = 78
      Height = 23
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object OKBtn: TSpeedButton
      Left = 160
      Top = 0
      Width = 73
      Height = 23
      Caption = 'OK'
      OnClick = OKBtnClick
    end
    object CloseBtn: TButton
      Left = 233
      Top = 0
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = CloseBtnClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 167
    Width = 600
    Height = 148
    Align = alTop
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
    WordWrap = False
  end
  object GridPanel: TPanel
    Left = 0
    Top = 27
    Width = 600
    Height = 137
    Align = alTop
    TabOrder = 2
  end
  object OpenDialog: TOpenDialog
    Left = 168
    Top = 200
  end
end
