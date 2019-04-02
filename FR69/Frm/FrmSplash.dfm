object FormSplash: TFormSplash
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 5
  Caption = 'Splash'
  ClientHeight = 135
  ClientWidth = 366
  Color = 8454016
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 366
    Height = 135
    Align = alClient
    Alignment = taCenter
    BorderStyle = bsNone
    Color = 11126238
    Font.Charset = ANSI_CHARSET
    Font.Color = 13339444
    Font.Height = -16
    Font.Name = 'Comic Sans MS'
    Font.Style = []
    Lines.Strings = (
      ''
      'FRXX - loading...'
      'please wait ca. 6 seconds,'
      'while Application is checking ports.')
    ParentFont = False
    TabOrder = 0
  end
end
