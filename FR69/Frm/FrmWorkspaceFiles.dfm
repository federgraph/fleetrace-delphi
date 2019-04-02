object FormWorkspaceFiles: TFormWorkspaceFiles
  Left = 214
  Top = 227
  Caption = 'Workspace Files'
  ClientHeight = 389
  ClientWidth = 652
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid: TDBGrid
    Left = 0
    Top = 31
    Width = 652
    Height = 160
    Align = alTop
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'WorkspaceID'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ItemPath'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ItemName'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Ext'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ItemKey'
        Width = 220
        Visible = True
      end>
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 652
    Height = 31
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 25
    Caption = 'DBToolBar'
    Flat = False
    TabOrder = 1
    object DBNavigator: TDBNavigator
      Left = 0
      Top = 0
      Width = 240
      Height = 25
      DataSource = DataSource
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
    object ClearBtn: TSpeedButton
      Left = 240
      Top = 0
      Width = 57
      Height = 25
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
    object RequeryBtn: TSpeedButton
      Left = 297
      Top = 0
      Width = 96
      Height = 25
      Caption = 'Requery'
      OnClick = RequeryBtnClick
    end
    object ShowBtn: TSpeedButton
      Left = 393
      Top = 0
      Width = 72
      Height = 25
      Caption = 'Show'
      OnClick = ShowBtnClick
    end
  end
  object Memo: TMemo
    Left = 64
    Top = 224
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
    TabOrder = 2
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 370
    Width = 652
    Height = 19
    Panels = <>
  end
  object DataSource: TDataSource
    Left = 176
    Top = 96
  end
end
