object FormFR62: TFormFR62
  Left = 0
  Top = 77
  Caption = 'FR62'
  ClientHeight = 636
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object NorthContainer: TPageControl
    Left = 8
    Top = 64
    Width = 693
    Height = 159
    TabOrder = 1
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object PageControl: TPageControl
    Left = 8
    Top = 229
    Width = 697
    Height = 161
    TabOrder = 0
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 786
    Height = 29
    ButtonHeight = 23
    Caption = 'ToolBar'
    TabOrder = 2
    Transparent = True
    object ClearBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 89
      Height = 23
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
    object NorthBtn: TSpeedButton
      Left = 89
      Top = 0
      Width = 89
      Height = 23
      Caption = 'North'
      OnClick = NorthBtnClick
    end
    object SouthBtn: TSpeedButton
      Left = 178
      Top = 0
      Width = 89
      Height = 23
      Caption = 'South'
      OnClick = SouthBtnClick
    end
    object InfoBtn: TSpeedButton
      Left = 267
      Top = 0
      Width = 89
      Height = 23
      Caption = 'Info'
      OnClick = InfoBtnClick
    end
    object StyleBtn: TSpeedButton
      Left = 356
      Top = 0
      Width = 61
      Height = 23
      Caption = 'Style'
      OnClick = StyleBtnClick
    end
    object LanguageBtn: TSpeedButton
      Left = 417
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Lang'
      OnClick = LanguageBtnClick
    end
    object RowHeightBtn: TSpeedButton
      Left = 481
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Height'
      OnClick = RowHeightBtnClick
    end
    object HelpBtn: TSpeedButton
      Left = 545
      Top = 0
      Width = 64
      Height = 23
      Caption = 'Help'
      OnClick = HelpBtnClick
    end
    object MinusBtn: TSpeedButton
      Left = 609
      Top = 0
      Width = 40
      Height = 23
      Caption = 'S-'
      OnClick = MinusBtnClick
    end
    object PlusBtn: TSpeedButton
      Left = 649
      Top = 0
      Width = 40
      Height = 23
      Caption = 'S+'
      OnClick = PlusBtnClick
    end
  end
  object SouthContainer: TPageControl
    Left = 8
    Top = 404
    Width = 697
    Height = 161
    TabOrder = 3
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    OnIdle = ApplicationEventsIdle
    Left = 264
    Top = 120
  end
  object IdleTimer: TTimer
    OnTimer = IdleTimerTimer
    Left = 392
    Top = 120
  end
end
