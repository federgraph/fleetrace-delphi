object FormLogger: TFormLogger
  Left = 0
  Top = 0
  Caption = 'Logger'
  ClientHeight = 284
  ClientWidth = 418
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
    Left = 56
    Top = 72
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo')
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
