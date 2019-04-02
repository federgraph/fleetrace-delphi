unit RiggVar.Grid.Color;

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
  System.UITypes;

type
  TColorTranslator = class
  public
    function GetBValue(ARGB: TColor): Integer;
    function GetGValue(ARGB: TColor): Integer;
    function GetRGB(R, G, B: Integer): Integer;
    function GetRValue(ARGB: TColor): Integer;
    function TranslateColor(aColor: TColor): string;
  end;

  TColGridColorRec = record
    DefaultColor: TColor;
    AlternatingColor: TColor;
    FocusColor: TColor;
    EditableColor: TColor;
    AlternatingEditableColor: TColor;
    CurrentColor: TColor;
    TransColor: TColor;
  end;

  TColGridColorScheme = (
    color256,
    colorRed,
    colorBlue,
    colorMoneyGreen,
    colorRubyGraphite
    );

  TColorService = class
  public
    class var StyleName: string;
    class function GetColorScheme: TColGridColorScheme;
    class function GetGridColors(const Value: TColGridColorScheme): TColGridColorRec;
    class function HTMLColor(c: TColor): string;
  end;

const
  TColorRec_Cornflowerblue = $00ED9564;
  TColorRec_Orange = TColor($00A5FF);
  TColorRec_Darkorange = $00008CFF;
  TColorRec_Yellowgreen = $0032CD9A;
  TColorRec_Crimson = $003C14DC;
  TColorRec_Lightgray = $00D3D3D3;
  TColorRec_Darkgray = $00A9A9A9;
  TColorRec_Skyblue = $00FFBF00;

  clFleetNone = TColorRec_DarkOrange;
  clFleetYellow = $00CCFFFF;
  clFleetBlue = $00FFFFCC;
  clFleetRed = $00CCCCFF;
  clFleetGreen = $00CCFFCC;

  clEventBtn = TColorRec.Darkorange;

  clBlank = TColorRec.White;
  clHeader = TColorRec_Lightgray;

  clFocus = TColorRec.Yellow;
  clEditable = $FFFFE8;
  clAlternatingEditable = clEditable;

  clNormal = TColorRec.White;
  clAlternate = $E0FFFF;

  clHellRot = $008080FF;
  clTransRot = $0089AAF5;

  clHellBlau = $00FF8888;
  clTransBlau = $00ECAD93;

  clHellGruen = $0080FF80; //for LED

  clMG = TColorRec.MoneyGreen;
  clMGA = $00FFFBF0;

var
  ColorTranslator: TColorTranslator;

implementation

function TColorTranslator.GetBValue(ARGB: TColor): Integer;
var
  temp: Integer;
begin
  temp := (ARGB shl 8) shr 8;
  result := temp and 255;
end;

function TColorTranslator.GetGValue(ARGB: TColor): Integer;
var
  temp: Integer;
begin
  temp := (ARGB shl 8) shr 8;
  result := (temp and (255 shl 8)) shr 8;
end;

function TColorTranslator.GetRValue(ARGB: TColor): Integer;
var
  temp: Integer;
begin
  temp := (ARGB shl 8) shr 8;
  result := (temp and (255 shl 16)) shr 16;
end;

function TColorTranslator.GetRGB(R, G, B: Integer): Integer;
begin
  result := (R shl 16) + (G shl 8) + B;
  result  := result + (256 shl 24);
end;

function TColorTranslator.TranslateColor(aColor: TColor): string;
var
  ir, ig, ib: Integer;
begin
  ir := GetRValue(aColor);
  ig := GetGValue(aColor);
  ib := GetBValue(aColor);
  result := Format('#%2x%2x%2x', [ib, ig, ir]);
end;

class function TColorService.GetColorScheme: TColGridColorScheme;
begin
  result := colorRed;
  if Pos('Graphite', StyleName) > 0then
    result := colorRubyGraphite;
end;

class function TColorService.GetGridColors(const Value: TColGridColorScheme): TColGridColorRec;
begin
  case Value of
    colorRubyGraphite:
    begin
      result.DefaultColor := TColorRec.Gray;
      result.AlternatingColor := TColorRec.Gray;
      result.FocusColor := TColorRec.Yellow;
      result.EditableColor := TColorRec.Darkgray;
      result.AlternatingEditableColor := TColorRec.Darkgray;
      result.CurrentColor := clHellBlau;
      result.TransColor := clTransBlau;
    end;

    colorMoneyGreen:
    begin
      result.DefaultColor := clMG;
      result.AlternatingColor := clMGA;
      result.FocusColor := TColorRec.Yellow;
      result.EditableColor := clEditable;
      result.AlternatingEditableColor := clEditable;
      result.CurrentColor := clHellRot;
      result.TransColor := clTransRot;
    end;

    colorRed:
    begin
      result.DefaultColor := clAlternate;
      result.AlternatingColor := clNormal;
      result.FocusColor := clFocus;
      result.EditableColor := clEditable;
      result.AlternatingEditableColor := clAlternatingEditable;
      result.CurrentColor := clHellRot;
      result.TransColor := clTransRot;
    end;

    colorBlue:
    begin
      result.DefaultColor := clNormal;
      result.AlternatingColor := clAlternate;
      result.FocusColor := clFocus;
      result.EditableColor := clEditable;
      result.AlternatingEditableColor := clAlternatingEditable;
      result.CurrentColor := clHellBlau;
      result.TransColor := clTransBlau;
    end;

    color256:
    begin
      result.DefaultColor := TColorRec.White;
      result.AlternatingColor := TColorRec.Lightgray;
      result.FocusColor := TColorRec.Teal;
      result.EditableColor := TColorRec.Yellow;
      result.AlternatingEditableColor := TColorRec.Yellow;
      result.CurrentColor := TColorRec.Red;
      result.TransColor := TColorRec.Red;
    end;
  end;
end;

class function TColorService.HTMLColor(c: TColor): string;
begin
  case c of
    clEditable: result := '#F0F8FF';

    clNormal: result := 'white';
    clAlternate: result := '#FFFFE0';

    clMGA: result := '#C0DCC0'; //green
    clMG: result := '#F0FBFF'; //blue

    clHellBlau: result := '#8080FF';
    clTransBlau: result := '#89AAF5';

    clHellRot: result := '#FF8888';
    clTransRot: result := '#ECAD93';

    TColorRec.SkyBlue: result := '#87CEEB';
    $000080FF: result := '#FF8000';

    TColorRec.Lightgray: result := 'silver';
    //
    TColorRec.Red: result := 'red';
    TColorRec.Maroon: result := 'maroon';
    TColorRec.Black: result := 'black';
    TColorRec.Yellow: result := 'yellow';
    TColorRec.Olive: result := 'olive';
    TColorRec.Lime: result := 'lime';
    TColorRec.Green: result := 'green';
    TColorRec.Teal: result := 'teal';
    TColorRec.Gray: result := 'gray';
    TColorRec.Aqua: result := 'aqua';
    TColorRec.Blue: result := 'blue';
    TColorRec.Navy: result := 'navy';
    TColorRec.Silver: result := 'silver';
    TColorRec.Purple: result := 'purple';
    TColorRec.Fuchsia: result := 'fuchsia';
    //clMoneyGreen: result := '#C0DCC0';
    //clCream: result := '#F0FBFF';
    else
    begin
      if not Assigned(ColorTranslator) then
        ColorTranslator := TColorTranslator.Create;
      result := ColorTranslator.TranslateColor(c);
    end;
  end;
end;

initialization
  ColorTranslator := TColorTranslator.Create;

finalization
  ColorTranslator.Free;

end.
