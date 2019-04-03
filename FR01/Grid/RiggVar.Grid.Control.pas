unit RiggVar.Grid.Control;

(*
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
*)

interface

{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.GraphUtil,
  Vcl.Grids,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColBase,
  RiggVar.Grid.ColGrid;

type
  TDisplayGrid = class(TStringGrid)
  private
    TextPaddingX: Integer;
    TextPaddingY: Integer;

    GR: TRect; //rectangle for filling in the group incicator with group-color
    TR: TRect; //rectangle for drawing text into
    BC: TColor; //background-color for drawing a cell

    FHeaderRowIndex: Integer;
    FCellsBold: Boolean;
    FHeaderBold: Boolean;

    SavedCellString: string;
    CurrentCellString: string;

    GroupRight: Boolean;
    GroupWidth: Integer;

    procedure MyGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure MyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MySelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure MySetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure MyDrawCell(Sender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
  protected
    DisableInvalidate: Boolean;
    procedure Init1;
    procedure Init2;
    property HeaderBold: Boolean read FHeaderBold write FHeaderBold;
    property CellsBold: Boolean read FCellsBold write FCellsBold;
  public
    ColGrid: TColGrid;

    class var WantSmallRows: Boolean;
    class var WantCustomDrawCell: Boolean;

    constructor Create(AOwner: TComponent); override;

    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;

    procedure SetupGrid;
    procedure ShowHeader;
    procedure ShowData;

    procedure InvalidateGrid;
    procedure CancelEdit;
    procedure FinishEdit(ACol, ARow: Integer; var Value: string);
    property HeaderRowIndex: Integer read FHeaderRowIndex write FHeaderRowIndex;
  end;

implementation

constructor TDisplayGrid.Create(AOwner: TComponent);
begin
  inherited;
  if WantCustomDrawCell then
    Init2
  else
    Init1;
end;

procedure TDisplayGrid.Init1;
begin
  if WantSmallRows then
    DefaultRowHeight := 17
  else
    DefaultRowHeight := 24;

  DefaultDrawing := True;

  FixedCols := 0;
  FixedRows := 1;
  ColCount := 3;
  RowCount := 2;

  Options := [
    goFixedVertLine,
    goFixedHorzLine,
    goVertLine,
    goHorzLine,
    goRangeSelect,
    goColSizing,
    goTabs
    ];

  if not (csDesigning in ComponentState) then
  begin
    OnMouseDown := MyMouseDown;
    OnKeyDown := MyKeyDown;
    OnSetEditText := MySetEditText;
    OnGetEditText := MyGetEditText;
    OnSelectCell := MySelectCell;
  end;
end;

procedure TDisplayGrid.Init2;
begin
  TextPaddingX := 2;
  TextPaddingY := 2;

  GroupRight := true;
  GroupWidth := 2;

  FHeaderBold := False;
  CellsBold := False;

  DoubleBuffered := UseDoubleBufferForGrid;
  BorderStyle := bsSingle;

  if WantSmallRows then
    DefaultRowHeight := 17
  else
    DefaultRowHeight := 24;

  DefaultDrawing := False;

  FixedCols := 0;
  FixedRows := 1;
  ColCount := 3;
  RowCount := 2;

  Font.Charset := DEFAULT_CHARSET;
  Font.Style := [fsBold];
  Options := [
    goFixedVertLine,
    goFixedHorzLine,
    goVertLine,
    goHorzLine,
    goRangeSelect,
    goColSizing,
    goTabs
    ];
  ParentFont := False;

  if not (csDesigning in ComponentState) then
  begin
    OnMouseDown := MyMouseDown;
    OnKeyDown := MyKeyDown;
    OnSetEditText := MySetEditText;
    OnGetEditText := MyGetEditText;
    OnSelectCell := MySelectCell;
    OnDrawCell := MyDrawCell;
  end;
end;

procedure TDisplayGrid.WMLButtonDown(var Message: TWMLButtonDown);
begin
  Winapi.Windows.SetFocus(handle);
  inherited;
end;

procedure TDisplayGrid.WMRButtonDown(var Message: TWMRButtonDown);
begin
  Winapi.Windows.SetFocus(handle);
  inherited;
end;

procedure TDisplayGrid.InvalidateGrid;
begin
  if not DisableInvalidate then
    Invalidate;
end;

procedure TDisplayGrid.MyDrawCell(Sender: TObject; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  CellProp: TCellProp;
begin
  try
    CellProp := ColGrid.InitCellProp(ACol, ARow);
    if not Assigned(CellProp) then exit;

    { background color of cell }
    BC := CellProp.Color;
    if (ColGrid.MenuMode = False) and (gdFocused in AState) then
      BC := ColGrid.FocusColor;
    if FHeaderRowIndex = ARow then
    begin
      BC := clBtnFace;
      CellProp.GroupColor := clBtnFace;
    end;
    Canvas.Brush.Color := BC;

    { set font properties for cell text }
    Canvas.Font := Font;
    if FHeaderRowIndex = ARow then
    begin
      if HeaderBold then
        Canvas.Font.Style := [fsBold]
      else
        Canvas.Font.Style := [];
    end
    else
    begin
      if CellsBold then
        Canvas.Font.Style := [fsBold]
      else
        Canvas.Font.Style := [];
    end;

    { make room for group-indicator rectangle }
    if CellProp.ShowGroup then
    begin
      if GroupRight then
      begin
        GR.Left := ARect.Left + ARect.Width - GroupWidth;
        GR.Top := ARect.Top;
        GR.Width := GroupWidth;
        GR.Height := ARect.Height;
        Dec(TR.Right, GroupWidth);
        Dec(ARect.Right, GroupWidth);
      end
      else
      begin
        GR.Location := ARect.Location;
        GR.Width := GroupWidth;
        GR.Height := ARect.Height;
      end;
    end;

    { draw text of cell }
    if CellProp.Alignment = taRightJustify then
    begin
      SetTextAlign(Canvas.Handle, TA_RIGHT or TA_Top);
      TR := ARect;
      Inc(TR.Top, TextPaddingY);
      Inc(TR.Right, -TextPaddingX);
      Canvas.TextRect(ARect, TR.Right, TR.Top, Cells[aCol, aRow]);
    end
    else
    begin
      SetTextAlign(Canvas.Handle, TA_Left or TA_Top);
      TR := ARect;
      Inc(TR.Top, TextPaddingY);
      Inc(TR.Left, TextPaddingX);
      Canvas.TextRect(ARect, TR.Left, TR.Top, Cells[aCol, aRow]);
    end;

    { draw group incicator rectangle }
    if CellProp.ShowGroup then
    begin
      Canvas.Brush.Color := CellProp.GroupColor;
      Canvas.FillRect(GR);
    end;
  except
  end;
end;

procedure TDisplayGrid.MyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  cp: TBaseColProp;
begin
  if (Key = VK_Escape) and EditorMode then
  begin
    EditorMode := False;
    Cells[Col, Row] := SavedCellString;
  end;
  if (Key = VK_Return) and EditorMode then
  begin
    FinishEdit(Col, Row, CurrentCellString);
    if Assigned(ColGrid.OnEdit) then
      ColGrid.OnEdit(Self);
  end
  else if ColGrid.ExcelStyle and (Key = VK_Tab) and EditorMode then
  begin
    FinishEdit(Col, Row, CurrentCellString);
    if EditorMode then
      HideEditor;
    if Assigned(ColGrid.OnEdit) then
      ColGrid.OnEdit(Self);
  end
  else if ColGrid.ExcelStyle and (Key = VK_Down) and EditorMode then
  begin
    FinishEdit(Col, Row, CurrentCellString);
    if EditorMode then
      HideEditor;
    if Assigned(ColGrid.OnEdit) then
      ColGrid.OnEdit(Self);
  end
  else if ColGrid.ExcelStyle and (Key = VK_Up) and EditorMode then
  begin
    FinishEdit(Col, Row, CurrentCellString);
    if EditorMode then
      HideEditor;
    if Assigned(ColGrid.OnEdit) then
      ColGrid.OnEdit(Self);
  end

  else if (Key = VK_Return) then
  begin
    cp := ColGrid.ColsActive[Col];
    if Assigned(cp) and cp.ReadOnly and Assigned(ColGrid.OnEdit) then
      ColGrid.OnEdit(Self);
  end
  else if Key = VK_F8 then
  begin
    if (ssCtrl in Shift) and Assigned(ColGrid.OnClearContent) then
      ColGrid.OnClearContent(Self);
  end
  else if (Key = VK_F4) and ColGrid.AutoMark then
    ColGrid.MarkRowCollectionItem
  else if (Key = VK_F4) then
  begin
    if Assigned(ColGrid.OnMarkRow) then
      ColGrid.OnMarkRow(Self);
  end
  else if (ssCtrl in Shift) and (Key = VK_Delete) and ColGrid.AutoDelete then
    ColGrid.DeleteRowCollectionItem
  else if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_Insert) and ColGrid.AutoInsert then
    ColGrid.AddRowCollectionItem
  else if (ssCtrl in Shift) and (Key = VK_Insert) and ColGrid.AutoInsert then
    ColGrid.InsertRowCollectionItem

  else if ColGrid.ExcelStyle and (Key = VK_Delete) and not EditorMode then
  begin
    EditorMode := True;
    CurrentCellString := '';
    FinishEdit(Col, Row, CurrentCellString);
    if EditorMode then
      HideEditor;
  end

  else if Assigned(ColGrid.OnKeyDown) then
    ColGrid.OnKeyDown(Self, Key, Shift);
end;

procedure TDisplayGrid.MyMouseDown(Sender: TObject;
Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row, col: integer;
begin
  MouseToCell(x, y, col, row);

  if Assigned(ColGrid.OnCellClick) then
    ColGrid.OnCellClick(Self, col, row);

  { when not in MenuMode, exit if click is not on header }
  if ColGrid.MenuMode or (row <> 0) then
    Exit;

  ColGrid.InitDisplayOrder(col);

  { draw data rows }
  ColGrid.ShowData;
end;

procedure TDisplayGrid.MySelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  cp: TBaseColProp;
begin
  cp := ColGrid.ColsActive[ACol];
  if not Assigned(cp) then exit;
  if EditorMode then
  begin
    EditorMode := False;
    if cp.ReadOnly = False then
      Cells[Col, Row] := SavedCellString;
  end;
  DisableInvalidate := True;
  if Assigned(cp) and cp.ReadOnly then
    Options := Options - [goEditing]
  else
    Options := Options + [goEditing];
  DisableInvalidate := False;
  if Assigned(ColGrid.OnCellSelect) then
    ColGrid.OnCellSelect(cp, ACol, ARow, CanSelect);
end;

procedure TDisplayGrid.MySetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  CurrentCellString := Value;
end;

procedure TDisplayGrid.MyGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  SavedCellString := Cells[ACol, ARow];
  CurrentCellString := SavedCellString;
end;

procedure TDisplayGrid.CancelEdit;
begin
  if EditorMode then
  begin
    EditorMode := False;
    Cells[Col, Row] := SavedCellString;
  end;
end;

procedure TDisplayGrid.FinishEdit(ACol, ARow: Integer; var Value: string);
var
  cp: TBaseColProp;
  cr: TBaseRowCollectionItem;
begin
  cp := ColGrid.ColsActive[ACol];
  if Assigned(cp) and Assigned(cp.OnFinishEdit) then
  begin
    cr := ColGrid.GetRowCollectionItem(ARow);
    if Assigned(cr) then
      cp.OnFinishEdit(cr, Value);
  end
  else if Assigned(cp) and Assigned(cp.OnFinishEdit2) then
  begin
    cr := ColGrid.GetRowCollectionItem(ARow);
    if Assigned(cr) then
      cp.OnFinishEdit2(cr, Value, cp.NameID);
  end;

  if Assigned(InplaceEditor) then
    InplaceEditor.Text := Value;

  if EditorMode = False then
    EditorMode := True;

  if Assigned(ColGrid.OnFinishEditCR) then
  begin
    cr := ColGrid.GetRowCollectionItem(ARow);
    if Assigned(cr) then
      ColGrid.OnFinishEditCR(cr);
  end;
end;

procedure TDisplayGrid.SetupGrid;
var
  cl: TBaseRowCollection;
  i: Integer;
begin
  cl := ColGrid.GetBaseRowCollection;

  { init RowCount, clear visible cells }
  if Assigned(cl) and (cl.Count > 0) then
    RowCount := cl.FilteredCount + 1 + FHeaderRowIndex
  else
    RowCount := 2 + FHeaderRowIndex;
  for i := 1 + FHeaderRowIndex to RowCount - 1 + FHeaderRowIndex do
    Rows[i].Clear;

  { init width of columns, show captions }
  ShowHeader;
end;

procedure TDisplayGrid.ShowHeader;
var
  i: Integer;
  cp: TBaseColProp;
begin
  { ColCount always >= 1, see TCustomGrid.SetColCount }
  ColCount := ColGrid.ColsActive.VisibleCount;
  for i := 0 to ColGrid.ColsActive.Count - 1 do
  begin
    cp := ColGrid.ColsActive[i];
    if Assigned(cp) and cp.Visible then
    begin
      ColWidths[i] := cp.Width;
      if not ColGrid.MenuMode then
        Cells[i, 0] := cp.Caption;
    end;
  end;
end;

procedure TDisplayGrid.ShowData;
var
  i, j: Integer;
  cr: TBaseRowCollectionItem;
  cl: TBaseRowCollection;
  r: Integer;
begin
  if EditorMode then
    Exit;
  cl := ColGrid.GetBaseRowCollection;
  if Assigned(cl) then
  begin
    { check RowCount }
    if RowCount <> cl.Count + 1 + FHeaderRowIndex then
    begin
      if cl.Count > 0 then
        RowCount := cl.FilteredCount + 1 + FHeaderRowIndex
      else
        RowCount := 2 + FHeaderRowIndex;
    end;
    { update all rows }
    r := 0 + FHeaderRowIndex;
    for j := 0 to cl.Count - 1 do
    begin
      i := j;
      if (ColGrid.DisplayOrder.Count = cl.FilteredCount) then
        i := ColGrid.DisplayOrder.DisplayIndex[j];
      if (i < 0) or (i > cl.Count-1) then Continue;
      cr := cl[i];
      if cr.IsInFilter then
      begin
        Inc(r);
        if Assigned(ColGrid.ColsActive) then
          ColGrid.ColsActive.UpdateRow(ColGrid.GridModel, r, cr);
      end;
    end;
  end;
end;

end.
