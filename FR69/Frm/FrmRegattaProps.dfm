object FormRegattaProps: TFormRegattaProps
  Left = 221
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Regatta Properties'
  ClientHeight = 395
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 544
    Top = 30
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 544
    Top = 61
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbScoringSystemProps: TGroupBox
    Left = 19
    Top = 219
    Width = 510
    Height = 150
    Caption = 'Scoring System'
    TabOrder = 2
    object lblScoringSystem: TLabel
      Left = 24
      Top = 33
      Width = 111
      Height = 13
      Caption = 'Internal Scoring System'
    end
    object lblScoringSystemName: TLabel
      Left = 187
      Top = 33
      Width = 128
      Height = 13
      Caption = 'External Scoring System ID'
    end
    object lblThrowoutScheme: TLabel
      Left = 24
      Top = 88
      Width = 87
      Height = 13
      Caption = 'Throwout Scheme'
    end
    object lblNumberOfThrowouts: TLabel
      Left = 151
      Top = 88
      Width = 102
      Height = 13
      Caption = 'Number of Throwouts'
    end
    object lblThrowoutsEditInfo: TLabel
      Left = 182
      Top = 107
      Width = 279
      Height = 13
      Caption = '(edit number of throwouts with control on main view toolbar)'
    end
    object cbScoringSystem: TComboBox
      Left = 24
      Top = 52
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object edScoringSystemName: TEdit
      Left = 176
      Top = 52
      Width = 57
      Height = 21
      MaxLength = 20
      TabOrder = 1
    end
    object cbThrowoutScheme: TComboBox
      Left = 24
      Top = 104
      Width = 105
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object edThrowouts: TEdit
      Left = 135
      Top = 104
      Width = 41
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
    end
  end
  object gbGeneralRegattaProps: TGroupBox
    Left = 19
    Top = 16
    Width = 510
    Height = 189
    Caption = 'General Regatta Properties'
    TabOrder = 3
    object lblEventName: TLabel
      Left = 24
      Top = 24
      Width = 59
      Height = 13
      Caption = 'Event Name'
    end
    object lblEventDate: TLabel
      Left = 248
      Top = 24
      Width = 65
      Height = 13
      Caption = 'Event Date(s)'
    end
    object lblOrganizer: TLabel
      Left = 24
      Top = 72
      Width = 101
      Height = 13
      Caption = 'Ausrichter / HostClub'
    end
    object lblPRO: TLabel
      Left = 24
      Top = 120
      Width = 100
      Height = 13
      Caption = 'Wettfahrtleiter / PRO'
    end
    object lblJuryHead: TLabel
      Left = 192
      Top = 120
      Width = 123
      Height = 13
      Caption = 'Schiedsrichter / JuryHead'
    end
    object lblDivision: TLabel
      Left = 360
      Top = 120
      Width = 105
      Height = 13
      Caption = 'Bootsklasse / Division'
    end
    object edEventName: TEdit
      Left = 24
      Top = 40
      Width = 217
      Height = 21
      TabOrder = 0
    end
    object edEventDate: TEdit
      Left = 248
      Top = 40
      Width = 169
      Height = 21
      TabOrder = 1
    end
    object edHostClub: TEdit
      Left = 24
      Top = 88
      Width = 393
      Height = 21
      TabOrder = 2
    end
    object edPRO: TEdit
      Left = 24
      Top = 136
      Width = 161
      Height = 21
      TabOrder = 3
    end
    object edJuryHead: TEdit
      Left = 192
      Top = 136
      Width = 161
      Height = 21
      TabOrder = 4
    end
    object cbDivision: TComboBox
      Left = 360
      Top = 136
      Width = 129
      Height = 21
      TabOrder = 5
    end
  end
end
