object FormMsgParser2: TFormMsgParser2
  Left = 0
  Top = 0
  Caption = 'MsgParser 2'
  ClientHeight = 535
  ClientWidth = 750
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
  object Memo: TMemo
    Left = 408
    Top = 64
    Width = 233
    Height = 249
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
  object ListBox: TListBox
    Left = 24
    Top = 64
    Width = 337
    Height = 329
    ItemHeight = 13
    TabOrder = 1
    OnKeyUp = ListBoxKeyUp
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 750
    Height = 29
    Caption = 'ToolBar'
    TabOrder = 2
    object TestBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 57
      Height = 22
      Caption = 'Test'
      OnClick = TestBtnClick
    end
    object LiveBtn: TSpeedButton
      Left = 57
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Live'
      OnClick = LiveBtnClick
    end
    object SimpleBtn: TSpeedButton
      Left = 113
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Simple'
      OnClick = SimpleBtnClick
    end
    object JsonBtn: TSpeedButton
      Left = 185
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Json'
      OnClick = JsonBtnClick
    end
  end
end
