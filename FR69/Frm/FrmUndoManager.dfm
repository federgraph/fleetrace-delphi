object FormUndoManager: TFormUndoManager
  Left = 177
  Top = 186
  BorderIcons = [biSystemMenu]
  Caption = 'Undo Manager'
  ClientHeight = 306
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 600
    Height = 27
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 23
    Caption = 'ToolBar'
    TabOrder = 0
    object ProxyXMLBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 72
      Height = 23
      Caption = 'Proxy XML'
      OnClick = ProxyXMLBtnClick
    end
    object BaseBtn: TSpeedButton
      Left = 72
      Top = 0
      Width = 65
      Height = 23
      Caption = 'Base'
      OnClick = BaseBtnClick
    end
    object LogBtn: TSpeedButton
      Left = 137
      Top = 0
      Width = 56
      Height = 23
      Caption = 'Log'
      OnClick = LogBtnClick
    end
    object CurrentBtn: TSpeedButton
      Left = 193
      Top = 0
      Width = 72
      Height = 23
      Caption = 'Current'
      OnClick = CurrentBtnClick
    end
    object UndoBtn: TSpeedButton
      Left = 265
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Undo'
      OnClick = UndoBtnClick
    end
    object RedoBtn: TSpeedButton
      Left = 329
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Redo'
      OnClick = RedoBtnClick
    end
    object UndoRedoBtn: TSpeedButton
      Left = 393
      Top = 0
      Width = 88
      Height = 23
      Caption = 'Undo | Redo'
      OnClick = UndoRedoBtnClick
    end
    object CloseBtn: TButton
      Left = 481
      Top = 0
      Width = 64
      Height = 23
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 0
      OnClick = CloseBtnClick
    end
  end
  object Memo: TMemo
    Left = 24
    Top = 48
    Width = 185
    Height = 89
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
end
