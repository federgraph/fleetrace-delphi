object FormNameFields: TFormNameFields
  Left = 221
  Top = 140
  BorderIcons = [biSystemMenu]
  Caption = 'Name Properties'
  ClientHeight = 386
  ClientWidth = 499
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
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 499
    Height = 139
    Align = alTop
    TabOrder = 0
    object LabelFieldMap: TLabel
      Left = 17
      Top = 20
      Width = 106
      Height = 13
      Caption = 'DisplayName FieldMap'
    end
    object NameSchemaLabel: TLabel
      Left = 17
      Top = 79
      Width = 64
      Height = 13
      Caption = 'NameSchema'
    end
    object edFieldMap: TEdit
      Left = 16
      Top = 39
      Width = 273
      Height = 21
      TabOrder = 0
      Text = 'LN'
    end
    object OKBtn: TButton
      Left = 360
      Top = 24
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 361
      Top = 56
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object NameSchemaCombo: TComboBox
      Left = 17
      Top = 98
      Width = 130
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object ApplyBtn: TButton
      Left = 153
      Top = 96
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 4
      OnClick = ApplyBtnClick
    end
  end
  object GridPanel: TPanel
    Left = 16
    Top = 160
    Width = 447
    Height = 185
    TabOrder = 1
    object ToolBar: TToolBar
      Left = 1
      Top = 1
      Width = 445
      Height = 27
      AutoSize = True
      BorderWidth = 1
      ButtonHeight = 23
      Caption = 'ToolBar'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object SwapBtn: TSpeedButton
        Left = 0
        Top = 0
        Width = 81
        Height = 23
        Caption = 'Swap'
        OnClick = SwapBtnClick
      end
      object AddBtn: TSpeedButton
        Left = 81
        Top = 0
        Width = 64
        Height = 23
        Caption = 'Add'
        OnClick = AddBtnClick
      end
      object RemoveBtn: TSpeedButton
        Left = 145
        Top = 0
        Width = 96
        Height = 23
        Caption = 'Remove'
        OnClick = RemoveBtnClick
      end
    end
  end
end
