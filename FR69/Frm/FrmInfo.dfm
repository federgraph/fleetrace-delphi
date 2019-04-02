object FormInfo: TFormInfo
  Left = 265
  Top = 233
  BorderIcons = [biSystemMenu]
  Caption = 'Info'
  ClientHeight = 367
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  Scaled = False
  DesignSize = (
    565
    367)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 40
    Top = 334
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 549
    Height = 320
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    WordWrap = False
    OnKeyDown = MemoKeyDown
  end
end
