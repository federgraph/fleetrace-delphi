object FormBridgeTemplate: TFormBridgeTemplate
  Left = 177
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Bridge Template'
  ClientHeight = 291
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelUrl: TLabel
    Left = 32
    Top = 129
    Width = 13
    Height = 13
    Caption = 'Url'
  end
  object LabelTaktOut: TLabel
    Left = 407
    Top = 83
    Width = 42
    Height = 13
    Caption = 'Takt Out'
  end
  object LabelTaktIn: TLabel
    Left = 357
    Top = 83
    Width = 34
    Height = 13
    Caption = 'Takt In'
  end
  object HostLabel: TLabel
    Left = 32
    Top = 83
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object PortLabel: TLabel
    Left = 223
    Top = 83
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object LabelHomePage: TLabel
    Left = 32
    Top = 175
    Width = 54
    Height = 13
    Caption = 'Home Page'
  end
  object LabelPortHTTP: TLabel
    Left = 287
    Top = 83
    Width = 48
    Height = 13
    Caption = 'Port HTTP'
  end
  object LabelProxyType: TLabel
    Left = 16
    Top = 13
    Width = 55
    Height = 13
    Caption = 'Proxy Type'
  end
  object Template: TLabel
    Left = 199
    Top = 13
    Width = 44
    Height = 13
    Caption = 'Template'
  end
  object LabelTestResult: TLabel
    Left = 125
    Top = 241
    Width = 54
    Height = 13
    Caption = 'Test Result'
  end
  object edUrl: TEdit
    Left = 32
    Top = 148
    Width = 403
    Height = 21
    MaxLength = 48
    TabOrder = 5
  end
  object OKBtn: TButton
    Left = 336
    Top = 236
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 417
    Top = 236
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object TestBtn: TButton
    Left = 185
    Top = 236
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 8
    OnClick = TestBtnClick
  end
  object edTaktOut: TEdit
    Left = 407
    Top = 102
    Width = 30
    Height = 21
    MaxLength = 3
    TabOrder = 4
  end
  object edTaktIn: TEdit
    Left = 357
    Top = 102
    Width = 30
    Height = 21
    MaxLength = 3
    TabOrder = 3
  end
  object edHost: TEdit
    Left = 32
    Top = 102
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object edPort: TEdit
    Left = 223
    Top = 102
    Width = 58
    Height = 21
    TabOrder = 1
  end
  object edHomePage: TEdit
    Left = 32
    Top = 194
    Width = 350
    Height = 21
    MaxLength = 48
    TabOrder = 6
  end
  object edPortHTTP: TEdit
    Left = 287
    Top = 102
    Width = 58
    Height = 21
    TabOrder = 2
  end
  object ProxyTypeCombo: TComboBox
    Left = 16
    Top = 32
    Width = 177
    Height = 21
    Style = csDropDownList
    TabOrder = 11
    OnChange = ProxyTypeComboChange
  end
  object TemplateCombo: TComboBox
    Left = 199
    Top = 32
    Width = 234
    Height = 21
    Style = csDropDownList
    TabOrder = 12
    OnChange = TemplateComboChange
  end
  object CurrentBtn: TButton
    Left = 16
    Top = 236
    Width = 75
    Height = 25
    Caption = 'Current'
    TabOrder = 7
    OnClick = CurrentBtnClick
  end
  object ShowAllBtn: TButton
    Left = 439
    Top = 30
    Width = 34
    Height = 25
    Caption = 'All'
    TabOrder = 13
    OnClick = ShowAllBtnClick
  end
end
