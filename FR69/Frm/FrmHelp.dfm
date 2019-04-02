object FormHelp: TFormHelp
  Left = 256
  Top = 167
  Caption = 'Readme'
  ClientHeight = 426
  ClientWidth = 646
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 646
    Height = 426
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnKeyUp = MemoKeyUp
  end
  object MainMenu: TMainMenu
    Left = 144
    Top = 72
    object ActionsMenu: TMenuItem
      Caption = 'Actions'
      OnClick = ActionsMenuClick
      object LoadItem: TMenuItem
        Caption = 'Load'
        OnClick = LoadItemClick
      end
      object EditItem: TMenuItem
        Caption = 'Edit'
        OnClick = EditItemClick
      end
      object SaveItem: TMenuItem
        Caption = 'Save'
        OnClick = SaveItemClick
      end
    end
  end
end
