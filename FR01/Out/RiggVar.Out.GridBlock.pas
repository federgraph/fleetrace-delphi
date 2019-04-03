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
  end;

implementation

{ TReportGrid }

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
