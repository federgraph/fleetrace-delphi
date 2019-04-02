unit RiggVar.Util.InputMatrix;

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
  System.UITypes;

const
  OldAge = -256;
  minBands = 3;
  maxBands = 16;
  StopBand = maxBands div 4;

type
  TMatrix = class;
  TBlendColor = array[0..maxBands] of TColor;

  TMatrixItem = class
  private
    FValue: string;
    FAge: Integer;
    FOwner: TMatrix;
    FIsAmpelMode: Boolean;
    function GetColor: TColor;
    procedure SetValue(const Value: string);
    function GetValue: string;
    procedure SetAge(const Value: Integer);
  public
    constructor Create(aOwner: TMatrix);
    property Value: string read GetValue write SetValue;
    property Color: TColor read GetColor;
    property IsAmpelMode: Boolean read FIsAmpelMode write FIsAmpelMode;
    property Age: Integer write SetAge;
  end;

  TMatrix = class
  private
    FData: array of TMatrixItem;
    FAge: Integer;
    FSize: Integer;
    function GetCells(ACol, ARow: Integer): string;
    procedure SetAge(ACol, ARow: Integer; const Value: Integer);
    function GetColors(ACol, ARow: Integer): TColor;
    function GetRValue(ARGB: TColor): Integer;
    function GetGValue(ARGB: TColor): Integer;
    function GetBValue(ARGB: TColor): Integer;
    function GetRGB(R, G, B: Integer): Integer;
    function GetIndex(ACol, ARow: Integer): Integer;
    function GetCount: Integer;
    function GetSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PrepareColors(Col1, Col2: TColor; ColBands: Byte; var BlendColor: TBlendColor);
    procedure TimerTick;
    procedure ResetAge;
    property Cells[ACol, ARow: Integer]: string read GetCells;
    property Age[ACol, ARow: Integer]: Integer write SetAge;
    property Colors[ACol, ARow: Integer]: TColor read GetColors;
    property Count: Integer read GetCount;
    property Size: Integer read GetSize;
  end;

implementation

uses
  RiggVar.App.Main;

var
  BlendColorInitialized: Boolean;
  BlendColor: TBlendColor;

{ TMatrixItem }

constructor TMatrixItem.Create(aOwner: TMatrix);
begin
  inherited Create;
  FOwner := aOwner;
  FAge := OldAge;
end;

function TMatrixItem.GetColor: TColor;
var
  i: Integer;
begin
  i := FOwner.FAge - FAge;

  { limit to valid values }
  if FAge < 0 then
      result := TColorRec.White
  else if (i > maxBands-StopBand) then
    result := BlendColor[maxBands-StopBand]
  else if i < 0 then
    result := TColorRec.White

  { Ampel }
  else if IsAmpelMode then
  begin
    if i > 10 then
      result := TColorRec.White
    else if i > 5 then
      result := TColorRec.Yellow
    else if i >= 0 then
      result := TColorRec.Lime
    else
      result := TColorRec.White;
  end

  { oder Farbverlauf }
  else
    result := BlendColor[i];
end;

function TMatrixItem.GetValue: string;
begin
  result := FValue;
end;

procedure TMatrixItem.SetAge(const Value: Integer);
begin
  FAge := FOwner.FAge;
end;

procedure TMatrixItem.SetValue(const Value: string);
begin
  FValue := Value;
 end;

{ TMatrix }

function TMatrix.GetIndex(ACol: Integer; ARow: Integer): Integer;
var
  i: Integer;
  c: Integer;
begin
  i := ACol + ARow * Main.Params.TimingGridColCount;
  c := Main.Params.TimingGridBibCount;
  if (i < 0) then
	  result := 0
  else if (i > c - 1) then
    result := c
  else
  	result := i;
end;

procedure TMatrix.ResetAge;
var
  bib: Integer;
begin
  for bib := 0 to Count-1 do
    FData[bib].FAge := OldAge;
  FAge := 0;
end;

constructor TMatrix.Create;
var
  bib: Integer;
  mi: TMatrixItem;
begin
  inherited Create;
  FSize := 256;

  if not BlendColorInitialized then
  begin
    BlendColorInitialized := true;
{$IFDEF VER150}
  PrepareColors($00A5FF, clWhite, maxBands, BlendColor);
{$ELSE}
  PrepareColors(TColorRec.Orange, TColorRec.White, maxBands, BlendColor);
{$ENDIF}
  end;

  SetLength(FData, FSize);
  for bib := 0 to FSize-1 do
  begin
    mi := TMatrixItem.Create(Self);
    mi.Value := IntToStr(bib + 1);
    FData[bib] := mi;
  end;
end;

destructor TMatrix.Destroy;
var
  bib: Integer;
begin
  for bib := 0 to FSize-1 do
    FData[bib].Free;
  inherited;
end;

function TMatrix.GetColors(ACol, ARow: Integer): TColor;
var
  i: Integer;
begin
  result := TColorRec.White;
  if (ACol >= 0)
    and (ARow >= 0)
    and (ACol < Main.Params.TimingGridColCount)
    and (ARow < Main.Params.TimingGridRowCount)
  then
  begin
    i := GetIndex(ACol, ARow);
    if i > Length(FData)-1 then
      result := TColorRec.Yellow
    else if (i < Main.Params.TimingGridBibCount) then
  		result := FData[i].Color;
  end
end;

function TMatrix.GetCells(ACol, ARow: Integer): string;
var
  i: Integer;
begin
  result := '';
  if (ACol >= 0)
    and (ARow >= 0)
    and (ACol < Main.Params.TimingGridColCount)
    and (ARow < Main.Params.TimingGridRowCount)
  then
  begin
    i := GetIndex(ACol, ARow);
    if (i < Main.Params.TimingGridBibCount) then
    begin
      if i > Length(FData)-1 then
        result := IntToStr(i+1)
      else
   		result := FData[i].Value;
    end;
  end;
end;

procedure TMatrix.SetAge(ACol, ARow: Integer; const Value: Integer);
var
  i: Integer;
begin
  if (ACol >= 0)
    and (ARow >= 0)
    and (ACol < Main.Params.TimingGridColCount)
    and (ARow < Main.Params.TimingGridRowCount)
  then
  begin
    i := GetIndex(ACol, ARow);
    if i > Length(FData)-1 then
      //do not remember age for values out of range nothing
    else if (i < Main.Params.TimingGridBibCount) then
  		FData[i].Age := Value;
  end;
end;

procedure TMatrix.TimerTick;
begin
  Inc(FAge);
end;

function TMatrix.GetBValue(ARGB: TColor): Integer;
var
  temp: Integer;
begin
  temp := (ARGB shl 8) shr 8;
  result := temp and 255;
end;

function TMatrix.GetGValue(ARGB: TColor): Integer;
var
  temp: Integer;
begin
  temp := (ARGB shl 8) shr 8;
  result := (temp and (255 shl 8)) shr 8;
end;

function TMatrix.GetRValue(ARGB: TColor): Integer;
var
  temp: Integer;
begin
  temp := (ARGB shl 8) shr 8;
  result := (temp and (255 shl 16)) shr 16;
end;

function TMatrix.GetSize: Integer;
begin
  result := FSize;
end;

function TMatrix.GetRGB(R, G, B: Integer): Integer;
begin
  result := (R shl 16) + (G shl 8) + B;
  result  := result + (256 shl 24);
end;

procedure TMatrix.PrepareColors(Col1, Col2: TColor; ColBands: Byte; var BlendColor: TBlendColor);
var
  dRGB: array[1..3] of Integer;
  z: Integer;
  ir, ig, ib: Integer;
begin
  dRGB[1] := GetRValue(Col2) - GetRValue(Col1);
  dRGB[2] := GetGValue(Col2) - GetGValue(Col1);
  dRGB[3] := GetBValue(Col2) - GetBValue(Col1);
  BlendColor[0] := GetRGB(GetRValue(Col1), GetGValue(Col1), GetBValue(Col1));
  for z := 1 to ColBands do
  begin
    ir := Round(GetRValue(BlendColor[0]) + z * (dRGB[1]/ColBands));
    ig := Round(GetGValue(BlendColor[0]) + z * (dRGB[2]/ColBands));
    ib := Round(GetBValue(BlendColor[0]) + z * (dRGB[3]/ColBands));
    BlendColor[z] := GetRGB(ir, ig, ib);
  end;
end;

function TMatrix.GetCount: Integer;
begin
  result := Main.Params.TimingGridColCount * Main.Params.TimingGridRowCount;
end;

end.
