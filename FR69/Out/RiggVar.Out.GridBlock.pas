unit RiggVar.Out.GridBlock;

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

//FRContent0  - Report without formatting
//FRContent2  - Report with bgcolor and SortIndex Header-Line
//FRContent3  - Normal Report with css class attributes
//FRContent5  - Report with bgcolor
//FRContent11 - Report with bgcolor, 1 line url sort-header

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleBlock;

type
  TReportGrid = class(TSimpleGridBlock)
  public
    function GetCSSClass(cellProp: TCellProp): string;
    function GetCSSValue(cellProp: TCellProp): string;
    procedure FRContent0(SL: TStrings; aCaption: string);
    procedure FRContent2(SL: TStrings; aCaption, Modus: string);
    procedure FRContent3(SL: TStrings; aCaption: string);
    procedure FRContent5(SL: TStrings; aCaption, Modus: string);
    procedure FRContent11(SL: TStrings; aCaption, Modus: string);
  end;

implementation

{ TReportGrid }

//Report without formatting
procedure TReportGrid.FRContent0(SL: TStrings; aCaption: string);
var
  r, c: Integer;
  s: string;
begin
  SL.Add('<table border=''1'' width=''100%'' cellspacing=''0'' cellpadding=''1''>');
  if (aCaption <> '') then
  begin
    SL.Add('<caption>' + aCaption + '</caption>');
  end;
  for r := 0 to Grid.RowCount - 1 do
  begin
    SL.Add('<tr>');
    for c := 0 to Grid.ColCount - 1 do
    begin
      s := Grid.Cells[c, r];
      if (s = '') then
        s := '&nbsp;';
      if (r = 0) then
      begin
        SL.Add('<th>' + s + '</th>');
      end
      else
      begin
        SL.Add('<td>' + s + '</td>');
      end;
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');
end;

//Report with bgcolor
procedure TReportGrid.FRContent5(SL: TStrings; aCaption, Modus: string);
var
  r, c: Integer;
  cp: TBaseColProp;
  s: string;
  sColor: string;
begin
  SL.Add('<table border=''1'' width=''100%'' cellspacing=''0'' cellpadding=''1''>');
  if (aCaption <> '') then
  begin
    SL.Add('<caption>' + aCaption + '</caption>');
  end;
  for r := 0 to Grid.RowCount - 1 do
  begin
    SL.Add('<tr align=''left''>');
    for c := 0 to Grid.ColCount - 1 do
    begin
      cp := ColGrid.ColsActive[c];
      if (cp = nil) then
        continue;

      s := Grid.Cells[c, r];
      sColor := ColGrid.CellProps.CellProp[c, r].HTMLColor;
      if (s = '') then
        s := '&nbsp;';
      if (r = 0) then
      begin
        if (cp.Alignment = taRightJustify) then
          SL.Add('<th bgcolor=''' + sColor + ''' align=''right''>' + s + '</th>')
        else
          SL.Add('<th bgcolor=''' + sColor + '''>' + s + '</th>');
      end
      else
      begin
        if (cp.Alignment = taRightJustify) then
          SL.Add('<td bgcolor=''' + sColor + ''' align=''right''>' + s + '</td>')
        else
          SL.Add('<td bgcolor=''' + sColor + '''>' + s + '</td>');
      end;
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');
end;

//Report with bgcolor, 1 line url sort-header
procedure TReportGrid.FRContent11(SL: TStrings; aCaption, Modus: string);
var
  r, c: Integer;
  cp: TBaseColProp;
  s: string;
  sColor: string;
  a: string;
begin
  SL.Add('<table border=''1'' width=''100%'' cellspacing=''0'' cellpadding=''1''>');
  if (aCaption <> '') then
  begin
    SL.Add('<caption>' + aCaption + '</caption>');
  end;
  for r := 0 to Grid.RowCount - 1 do
  begin
    SL.Add('<tr align=''left''>');
    for c := 0 to Grid.ColCount - 1 do
    begin
      cp := ColGrid.ColsActive[c];
      if (cp = nil) then
        continue;

      s := Grid.Cells[c, r];
      sColor := ColGrid.CellProps.CellProp[c, r].HTMLColor;
      if (s = '') then
        s := '&nbsp;';
      if (r = 0) then
      begin
        if Modus = 'Finish' then
          a := Format('<a href="finish?Sort=%d">%s</a>', [c, s])
        else
          a := Format('<a href="points?Sort=%d">%s</a>', [c, s]);
        if (cp.Alignment = taRightJustify) then
          SL.Add('<th bgcolor=''' + sColor + ''' align=''right''>' + a + '</th>')
        else
          SL.Add('<th bgcolor=''' + sColor + '''>' + a + '</th>');
      end
      else
      begin
        if (cp.Alignment = taRightJustify) then
          SL.Add('<td bgcolor=''' + sColor + ''' align=''right''>' + s + '</td>')
        else
          SL.Add('<td bgcolor=''' + sColor + '''>' + s + '</td>');
      end;
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');
end;

//Report with bgcolor and SortIndex Header-Line
procedure TReportGrid.FRContent2(SL: TStrings; aCaption, Modus: string);
var
  r, c: Integer;
  cp: TBaseColProp;
  s: string;
  sColor: string;
begin
  SL.Add('<table border=''1'' width=''100%'' cellspacing=''0'' cellpadding=''1''>');
  if (aCaption <> '') then
  begin
    SL.Add('<caption>' + aCaption + '</caption>');
  end;
  for r := -1 to Grid.RowCount - 1 do
  begin
    SL.Add('<tr align=''left''>');
    for c := 0 to Grid.ColCount - 1 do
    begin
      if r = -1 then
      begin
        SL.Add('<td bgcolor=''lightskyblue'' align=''right''>' + IntToStr(c) + '</td>');
        continue;
      end;

      cp := ColGrid.ColsActive[c];
      if (cp = nil) then
        continue;

      s := Grid.Cells[c, r];
      sColor := ColGrid.CellProps.CellProp[c, r].HTMLColor;
      if (s = '') then
        s := '&nbsp;';
      if (r = 0) then
      begin
        if (cp.Alignment = taRightJustify) then
          SL.Add('<th bgcolor=''' + sColor + ''' align=''right''>' + s + '</th>')
        else
          SL.Add('<th bgcolor=''' + sColor + '''>' + s + '</th>');
      end
      else
      begin
        if (cp.Alignment = taRightJustify) then
          SL.Add('<td bgcolor=''' + sColor + ''' align=''right''>' + s + '</td>')
        else
          SL.Add('<td bgcolor=''' + sColor + '''>' + s + '</td>');
      end;
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');
end;

//Normal Report with css class attributes
procedure TReportGrid.FRContent3(SL: TStrings; aCaption: string);
var
  s: string;
  sColor: string;
  cp: TBaseColProp;
  r, c: Integer;
begin
  SL.Add('<table border=''1'' width=''100%'' cellspacing=''0'' cellpadding=''1''>');
  if (aCaption <> '') then
  begin
    SL.Add('<caption>' + aCaption + '</caption>');
  end;
  for r := 0 to Grid.RowCount - 1 do
  begin
    SL.Add('<tr>');
    for c := 0 to Grid.ColCount - 1 do
    begin
      cp := ColGrid.ColsActive[c];
      if (cp = nil) then
        continue;

      s := Grid.Cells[c, r];
      if (s = '') then
        s := '&nbsp;';
      if (r = 0) then
      begin
        sColor := 'h';
        if (cp.Alignment = taLeftJustify) then
          sColor := sColor + 'l';
        sColor := Format(' class=''%s''', [sColor]);
        SL.Add(Format('<th%s>%s</th>', [sColor, s]));
      end
      else
      begin
        sColor := GetCSSClass(ColGrid.CellProps.CellProp[c, r]);
        SL.Add('<td' + sColor + '>' + s + '</td>');
      end;
    end;
    SL.Add('</tr>');
  end;
  SL.Add('</table>');
end;

function TReportGrid.GetCSSClass(cellProp: TCellProp): string;
var
  s: string;
  c: TColor;
begin
  result := '';
  c := cellProp.Color;
  s := '';
  case cellProp.ColorClass of
    cccAlternatingColor:
      s := 'a';
    cccAlternatingEditableColor:
      s := 'ae';
    cccBlank:
      result := '';
    cccCurrentColor:
      s := 'c';
    cccCustomColor, cccDefaultColor:
      s := 'n';
    cccEditableColor:
      s := 'e';
    cccFocusColor:
      result := '';
    cccHeaderColor:
      s := 'h';
  end;
  if (s <> '') then
  begin
    if (cellProp.Alignment = taLeftJustify) then
      s := s + 'l';
    result := Format(' class=''%s''', [s]);
  end
  else
  begin
    if (cellProp.Alignment = taLeftJustify) then
      result := Format(' bgcolor=''%s'' align=''left''', [TColorService.HTMLColor(c)])
    else
      result := Format(' bgcolor=''%s'' align=''right''', [TColorService.HTMLColor(c)]);
  end;
end;

function TReportGrid.GetCSSValue(cellProp: TCellProp): string;
var
  s: string;
//  c: TColor;
begin
  result := '';
//  c := cellProp.Color;
  s := '';
  case cellProp.ColorClass of
    cccAlternatingColor:
      s := 'a';
    cccAlternatingEditableColor:
      s := 'ae';
    cccBlank:
      result := '';
    cccCurrentColor:
      s := 'c';
    cccCustomColor, cccDefaultColor:
      s := 'n';
    cccEditableColor:
      s := 'e';
    cccFocusColor:
      result := '';
    cccHeaderColor:
      s := 'h';
  end;
  if (s <> '') then
  begin
    if (cellProp.Alignment = taLeftJustify) then
      result := s + 'l';
  end;
end;

end.
