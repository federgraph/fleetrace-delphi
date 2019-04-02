object FormUnicodeScanner: TFormUnicodeScanner
  Left = 0
  Top = 0
  Caption = 'Unicode Scanner'
  ClientHeight = 442
  ClientWidth = 657
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
  object PageControl: TPageControl
    Left = 16
    Top = 48
    Width = 609
    Height = 329
    ActivePage = tsListView
    TabOrder = 0
    object tsGrid: TTabSheet
      Caption = 'Grid'
      object GridSplitter: TSplitter
        Left = 320
        Top = 25
        Height = 276
        ExplicitLeft = 440
        ExplicitTop = 3
        ExplicitHeight = 405
      end
      object StringGrid: TStringGrid
        Left = 0
        Top = 25
        Width = 320
        Height = 276
        Align = alLeft
        ColCount = 2
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        TabOrder = 0
        OnKeyUp = StringGridKeyUp
        ColWidths = (
          207
          68)
      end
      object Memo: TMemo
        Left = 344
        Top = 120
        Width = 185
        Height = 89
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'Memo')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object ToolBar: TToolBar
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 595
        Height = 19
        AutoSize = True
        ButtonHeight = 19
        ButtonWidth = 98
        List = True
        AllowTextButtons = True
        TabOrder = 2
        object ListBtn: TToolButton
          Left = 0
          Top = 0
          Caption = 'List'
          ImageIndex = 0
          Style = tbsTextButton
          OnClick = ListBtnClick
        end
        object FillBtn: TToolButton
          Left = 32
          Top = 0
          Caption = 'Fill'
          ImageIndex = 4
          Style = tbsTextButton
          OnClick = FillBtnClick
        end
        object ShowBtn: TToolButton
          Left = 60
          Top = 0
          Caption = 'Utf-8'
          ImageIndex = 2
          Style = tbsTextButton
          OnClick = ShowBtnClick
        end
        object ShowDefaultBtn: TToolButton
          Left = 101
          Top = 0
          Caption = 'Default'
          ImageIndex = 4
          Style = tbsTextButton
          OnClick = ShowDefaultBtnClick
        end
        object ShowAsciiBtn: TToolButton
          Left = 152
          Top = 0
          Caption = 'Ascii'
          ImageIndex = 4
          Style = tbsTextButton
          OnClick = ShowAsciiBtnClick
        end
        object RemovePreambleBtn: TToolButton
          Left = 189
          Top = 0
          Caption = 'Remove Preamble'
          ImageIndex = 4
          Style = tbsTextButton
          OnClick = RemovePreambleBtnClick
        end
        object SaveBtn: TToolButton
          Left = 291
          Top = 0
          Caption = 'Save'
          ImageIndex = 3
          Style = tbsTextButton
          OnClick = SaveBtnClick
        end
      end
    end
    object tsListView: TTabSheet
      Caption = 'ListView'
      ImageIndex = 1
      object ListView: TListView
        Left = 0
        Top = 0
        Width = 465
        Height = 301
        Align = alLeft
        Columns = <
          item
            Caption = 'Name'
            Width = 300
          end
          item
            Alignment = taRightJustify
            Caption = 'Size'
          end>
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
      object ListAllBtn: TButton
        Left = 480
        Top = 32
        Width = 105
        Height = 25
        Caption = 'List All'
        TabOrder = 1
        OnClick = ListAllBtnClick
      end
      object ListFRBtn: TButton
        Left = 480
        Top = 63
        Width = 105
        Height = 25
        Caption = 'List FR'
        TabOrder = 2
        OnClick = ListFRBtnClick
      end
      object ListGroupedBtn: TButton
        Left = 480
        Top = 94
        Width = 105
        Height = 25
        Caption = 'List Grouped'
        TabOrder = 3
        OnClick = ListGroupedBtnClick
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 423
    Width = 657
    Height = 19
    Panels = <>
  end
end
