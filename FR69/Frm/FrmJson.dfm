object FormJson: TFormJson
  Left = 0
  Top = 0
  Caption = 'Json'
  ClientHeight = 543
  ClientWidth = 700
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
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 700
    Height = 29
    TabOrder = 0
    object ParamsBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Params'
      OnClick = ParamsBtnClick
    end
    object PropsBtn: TSpeedButton
      Left = 56
      Top = 0
      Width = 59
      Height = 22
      Caption = 'Props'
      OnClick = PropsBtnClick
    end
    object StartlistBtn: TSpeedButton
      Left = 115
      Top = 0
      Width = 77
      Height = 22
      Caption = 'Startlist'
      OnClick = StartlistBtnClick
    end
    object NameTableBtn: TSpeedButton
      Left = 192
      Top = 0
      Width = 56
      Height = 22
      Caption = 'Names'
      OnClick = NameTableBtnClick
    end
    object FleetlistBtn: TSpeedButton
      Left = 248
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Fleet'
      OnClick = FleetlistBtnClick
    end
    object FinishInfoBtn: TSpeedButton
      Left = 320
      Top = 0
      Width = 69
      Height = 22
      Caption = 'Finish'
      OnClick = FinishInfoBtnClick
    end
    object TimingInfoBtn: TSpeedButton
      Left = 389
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Timing'
      OnClick = TimingInfoBtnClick
    end
    object PenaltyInfoBtn: TSpeedButton
      Left = 461
      Top = 0
      Width = 72
      Height = 22
      Caption = 'Penalty'
      OnClick = PenaltyInfoBtnClick
    end
  end
  object Memo: TMemo
    Left = 191
    Top = 46
    Width = 347
    Height = 169
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo'
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object RightPanel: TPanel
    Left = 583
    Top = 29
    Width = 117
    Height = 514
    Align = alRight
    TabOrder = 2
    object UnescapeBtn: TButton
      Left = 24
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Unescape'
      TabOrder = 0
      OnClick = UnescapeBtnClick
    end
    object UnframeBtn: TButton
      Left = 24
      Top = 47
      Width = 75
      Height = 25
      Caption = 'Unframe'
      TabOrder = 1
      OnClick = UnframeBtnClick
    end
    object TrimBtn: TButton
      Left = 24
      Top = 78
      Width = 75
      Height = 25
      Caption = 'Trim'
      TabOrder = 2
      OnClick = TrimBtnClick
    end
    object FilterBtn: TButton
      Left = 24
      Top = 140
      Width = 75
      Height = 25
      Caption = 'Filter'
      TabOrder = 4
      OnClick = FilterBtnClick
    end
    object FPBtn: TButton
      Left = 24
      Top = 192
      Width = 75
      Height = 25
      Caption = 'FP'
      TabOrder = 5
      OnClick = FPBtnClick
    end
    object QUBtn: TButton
      Left = 24
      Top = 223
      Width = 75
      Height = 25
      Caption = 'QU'
      TabOrder = 6
      OnClick = QUBtnClick
    end
    object RaceBtn: TButton
      Left = 24
      Top = 254
      Width = 75
      Height = 25
      Caption = 'Race'
      TabOrder = 7
      OnClick = RaceBtnClick
    end
    object TLBtn: TButton
      Left = 24
      Top = 285
      Width = 75
      Height = 25
      Caption = 'TL'
      TabOrder = 8
      OnClick = TLBtnClick
    end
    object ConvertBtn: TButton
      Left = 25
      Top = 109
      Width = 75
      Height = 25
      Caption = 'Convert'
      TabOrder = 3
      OnClick = ConvertBtnClick
    end
    object PrettyPrintBtn: TButton
      Left = 24
      Top = 350
      Width = 75
      Height = 25
      Caption = 'Pretty'
      TabOrder = 9
      OnClick = PrettyPrintBtnClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 105
    Height = 514
    Align = alLeft
    TabOrder = 3
    object ETestBtn: TButton
      Left = 17
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Init E ( test )'
      TabOrder = 0
      OnClick = ETestBtnClick
    end
    object RTestBtn: TButton
      Left = 17
      Top = 47
      Width = 75
      Height = 25
      Caption = 'Init R ( test )'
      TabOrder = 1
      OnClick = RTestBtnClick
    end
    object DataBtn: TButton
      Left = 17
      Top = 117
      Width = 75
      Height = 25
      Caption = 'Show Data'
      TabOrder = 2
      OnClick = DataBtnClick
    end
    object JsonBtn: TButton
      Left = 17
      Top = 78
      Width = 75
      Height = 25
      Caption = 'Init J ( live )'
      TabOrder = 3
      OnClick = JsonBtnClick
    end
    object InitTransBtn: TButton
      Left = 17
      Top = 187
      Width = 75
      Height = 25
      Caption = 'Init Trans'
      TabOrder = 4
      OnClick = InitTransBtnClick
    end
    object TransformNBtn: TButton
      Left = 17
      Top = 263
      Width = 75
      Height = 25
      Caption = 'Transform N'
      TabOrder = 6
      OnClick = TransformNBtnClick
    end
    object CopyMemoBtn: TButton
      Left = 17
      Top = 304
      Width = 75
      Height = 25
      Caption = 'Copy Memo'
      TabOrder = 7
      OnClick = CopyMemoBtnClick
    end
    object ClearRaceBtn: TButton
      Left = 17
      Top = 335
      Width = 75
      Height = 25
      Caption = 'Clear Race'
      TabOrder = 8
      OnClick = ClearRaceBtnClick
    end
    object SendBtn: TButton
      Left = 17
      Top = 366
      Width = 75
      Height = 25
      Caption = 'Inject Msg'
      TabOrder = 9
      OnClick = SendBtnClick
    end
    object TransformCBtn: TButton
      Left = 17
      Top = 232
      Width = 75
      Height = 25
      Caption = 'Transform C'
      TabOrder = 5
      OnClick = TransformCBtnClick
    end
    object SwapBtn: TButton
      Left = 17
      Top = 397
      Width = 75
      Height = 25
      Caption = 'Swap Event'
      TabOrder = 10
      OnClick = SwapBtnClick
    end
    object LoadPartialBtn: TButton
      Left = 17
      Top = 428
      Width = 75
      Height = 25
      Caption = 'Load Partial'
      TabOrder = 11
      OnClick = LoadPartialBtnClick
    end
    object ShowJsonTypeBtn: TButton
      Left = 17
      Top = 148
      Width = 75
      Height = 25
      Caption = 'Show Type'
      TabOrder = 12
      OnClick = ShowJsonTypeBtnClick
    end
  end
end
