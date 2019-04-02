unit RiggVar.BO.ExcelImport;

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
  RiggVar.Grid.ColGrid;

const
  TableID_NameList = 1;
  TableID_FinishList = 2;
  TableID_StartList = 3;
  TableID_ResultList = 4;
  TableID_TimeList = 5;
  TableID_FleetList = 6;
  TableID_CaptionList = 7;
  TableID_CompareList = 8;

  NameListStartToken = 'NameList.Begin';
  NameListEndToken = 'NameList.End';

  StartListStartToken = 'StartList.Begin';
  StartListEndToken = 'StartList.End';

  FinishListStartToken = 'FinishList.Begin';
  FinishListEndToken = 'FinishList.End';

  TimeListStartToken = 'TimeList.Begin'; //with Param: TimeList.Begin.R1
  TimeListEndToken = 'TimeList.End';

  ResultListStartToken = 'ResultList.Begin';
  ResultListEndToken = 'ResultList.End';

  FleetListStartToken = 'FleetList.Begin';
  FleetListEndToken = 'FleetList.End';

  CaptionListStartToken = 'CaptionList.Begin';
  CaptionListEndToken = 'CaptionList.End';

  CompareListStartToken = 'CompareList.Begin';
  CompareListEndToken = 'CompareList.End';

type
  TJsonType = (
    EventDataJSON,
    RaceDataJSON
  );

  TExcelImporter = class
  private
    SL: TStringList;
    SLToken: TStringList;
    SLField: TStringList;
    SLFilter: TStringList;
    SNR: Integer;
    Bib: Integer;
    Division: string;
    PosID: Integer;
    N: Integer;
    W: Integer;
    IT: Integer;
    FDelimiter: char;
    TableID: Integer;
    sToken: string;
    sRest: string;
    procedure SetTableID(const Value: string);
    procedure SetValue(f, v: string);
    procedure SetDelimiter(const Value: char);
    procedure Transpose(ML: TStrings);
    procedure TransposePropList(ML: TStrings);
    procedure SetValue_StartList(f: string; v: string);
    procedure SetValue_FleetList(f, v: string);
    procedure SetValue_NameList(f, v: string);
    procedure SetValue_FinishList(f, v: string);
    procedure SetValue_TimeList(f, v: string);
    procedure SetValue_ResultList(f, v: string);
    procedure SetValue_CaptionList(const f: string; const v: string);
    procedure SetValue_CompareList(const f: string; const v: string);
    function ExtractRaceParam(const Value: string): Integer;
    procedure NextToken;
    function NextTokenX(TokenName: string): Integer;
    function TrimAroundEqual(const s: string): string;
  public
    CompareList: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure SetRace(Value: Integer);
    procedure RunImportFilter(const Data: string; m: TStrings);
    procedure GetTestData(ML: TStrings);
    procedure ShowTabs(ML: TStrings);
    procedure ShowTabs2(ML: TStrings);
    procedure Convert(ML: TStrings; aTableID: Integer);
    function ConvertString(MemoText: string; aTableID: Integer): string;
    property Delimiter: char read FDelimiter write SetDelimiter;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Util.Classes;

{ TExcelImporter }

constructor TExcelImporter.Create;
begin
  CompareList := TStringList.Create;
  FDelimiter := ';';
  SL := TStringList.Create;
  SLToken := TStringList.Create;
  SLToken.Delimiter := FDelimiter;
{$ifdef VER150}
  //in D7 StrictDelimiter property not available
{$else}
  SLToken.StrictDelimiter := True;
{$endif}
  SLField := TStringList.Create;
  SLFilter := TStringList.Create;
  Division := '*';
end;

destructor TExcelImporter.Destroy;
begin
  CompareList.Free;
  SLFilter.Free;
  SLField.Free;
  SLToken.Free;
  SL.Free;
  inherited;
end;

procedure TExcelImporter.SetDelimiter(const Value: char);
begin
  FDelimiter := Value;
end;

procedure TExcelImporter.SetRace(Value: Integer);
begin
  W := Value;
end;

procedure TExcelImporter.SetTableID(const Value: string);
begin
  if Pos('StartList', Value) >= 1 then
    TableID := TableID_StartList
  else if Pos('FleetList', Value) >= 1 then
    TableID := TableID_FleetList
  else if Pos('FinishList', Value) >= 1 then
    TableID := TableID_FinishList
  else if Pos('NameList', Value) >= 1 then
    TableID := TableID_NameList
  else if Pos('ResultList', Value) >= 1 then
    TableID := TableID_ResultList
  else if Pos('TimeList', Value) >= 1 then
  begin
    TableID := TableID_TimeList;
    W := ExtractRaceParam(Value);
  end
  else if Pos('CaptionList', Value) >= 1 then
    TableID := TableID_CaptionList
  else if Pos('CompareList', Value) >= 1 then
  begin
    TableID := TableID_CompareList;
    CompareList.Clear;
  end
  else
    TableID := 0;
end;

function TExcelImporter.ExtractRaceParam(const Value: string): Integer;
var
  X: Integer;
begin
  sRest := Value;
  NextToken; //TimeList
  NextToken; //Begin
  X := NextTokenX('R'); //RX
  result := X;
end;

procedure TExcelImporter.NextToken;
begin
  sRest := TUtils.Cut('.', sRest, sToken);
end;

function TExcelImporter.TrimAroundEqual(const s: string): string;
var
  i: Integer;
begin
  result := s;
  i := Pos('=', s);
  if i > 0 then
    result := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)))
end;

function TExcelImporter.NextTokenX(TokenName: string): Integer;
var
  l: Integer;
begin
  NextToken;
  result := -1;
  l := Length(TokenName);
  if Copy(sToken, 1, l) = TokenName then
  begin
    sToken := Copy(sToken, l+1, Length(sToken) - l);
    result := StrToIntDef(sToken, -1);
  end;
end;

procedure TExcelImporter.GetTestData(ML: TStrings);
begin
  ML.Clear;
  ML.Add('Bib;SNR;FN;LN;NOC;R1;R2');
  ML.Add('1;1000;A;a;FRA;1;2');
  ML.Add('2;1001;B;b;GER;2;3');
  ML.Add('3;1002;C;b;ITA;3;1');
end;

procedure TExcelImporter.ShowTabs(ML: TStrings);
var
  i: Integer;
  s: string;
  delim: string;
begin
  SL.Text := ML.Text;
  ML.Clear;
  delim := #9;
  for i := 0 to SL.Count - 1 do
  begin
    s := SL[i];
    s := StringReplace(s, delim, ';', [rfReplaceAll]);
    ML.Add(s);
  end;
end;

procedure TExcelImporter.ShowTabs2(ML: TStrings);
var
  i: Integer;
  j: Integer;
  s: string;
  delim: string;
begin
  SL.Text := ML.Text;
  ML.Clear;
  delim := ';';
  for i := 0 to SL.Count - 1 do
  begin
    s := SL[i];
    SLToken.DelimitedText := Trim(s);
    s := '';
    for j := 0 to SLToken.Count - 1 do
    begin
      if j > 0 then
        s := s + delim;
      s := s + SLToken[j];
    end;
    ML.Add(s);
  end;
end;

procedure TExcelImporter.Convert(ML: TStrings; aTableID: Integer);
begin
  SL.Clear;
  TableID := aTableID;
  Transpose(ML);
  ML.Text := SL.Text;
  TableID := 0;
  SL.Clear;
end;

function TExcelImporter.ConvertString(MemoText: string; aTableID: Integer): string;
var
  ML: TStringList;
  i: Integer;
  s: string;
begin
  ML := TStringList.Create;
  ML.Text := MemoText;
  for i := 0 to ML.Count- 1 do
  begin
    s := ML[i];
    s := StringReplace(s, '[', '', [rfReplaceAll]);
    s := StringReplace(s, ']', '', [rfReplaceAll]);
    s := StringReplace(s, '(', '', [rfReplaceAll]);
    s := StringReplace(s, ')', '', [rfReplaceAll]);
    ML[i] := s;
  end;
  Convert(ML, aTableID);
  result := ML.Text;
  SL.Clear;
  ML.Free;
end;

procedure TExcelImporter.SetValue_StartList(f: string; v: string);
var
  s: string;
begin
  if f = 'SNR' then
  begin
    SNR := StrToIntDef(v, 0);
    if SNR <> 0 then
    begin
      s := Format('FR.*.W1.STL.Pos%d.SNR=%d', [PosID, SNR]);
      SL.Add(s);
    end;
  end
  else if f = 'Bib' then
  begin
    Bib := StrToIntDef(v, 0);
    if (SNR <> 0) and (Bib <> 0) then
    begin
      s := Format('FR.*.W1.STL.Pos%d.Bib=%d', [PosID, Bib]);
      SL.Add(s);
    end;
  end;
end;

procedure TExcelImporter.SetValue_NameList(f: string; v: string);
var
  v1: string;
//  l: Integer;
  s: string;
begin
  if (f = 'FN')
  or (f = 'LN')
  or (f = 'SN')
  or (f = 'NC')
  or (f = 'GR')
  or (f = 'PB')
  then
  begin
    v1 := Trim(v);
// Import Hack, usually not needed
//    if (f = 'NOC') and (Length(v1) > 3) then
//    begin
//      l := Length(v1);
//      v1 := Copy(v1, l-2, MaxInt);
//    end;
    s := Format('FR.*.SNR%d.%s=%s', [SNR, f, v1]);
    SL.Add(s);
  end

  else if (Length(f) > 1) and (f[1] = 'N') then
  begin
    N := StrToIntDef(Copy(f, 2, MaxInt), 0);
    if (N > 0) and (SNR > 0) then
    begin
      s := Format('FR.*.SNR%d.N%d=%s', [SNR, N, v]);
      SL.Add(s);
    end;
  end
end;

procedure TExcelImporter.SetValue_FinishList(f: string; v: string);
var
  s: string;
begin
  if (Length(f) > 1) and (f[1] = 'R') then
  begin
    W := StrToIntDef(Copy(f, 2, MaxInt), 0);
    if (W > 0) and (Bib > 0) then
    begin
      if StrToIntDef(v, 0) > 0 then
      begin
        if TUtils.StartsWith(s, '-') then
          s := Copy(s, 2, MaxInt);
        s := Format('FR.*.W%d.Bib%d.Rank=%s', [W, Bib, v]);
        SL.Add(s);
      end;
    end;
  end
end;

procedure TExcelImporter.SetValue_FleetList(f: string; v: string);
var
  s: string;
begin
  if (Length(f) > 1) and (f[1] = 'R') then
  begin
    W := StrToIntDef(Copy(f, 2, MaxInt), 0);
    if (W > 0) and (Bib > 0) then
    begin
      if StrToIntDef(v, -1) > -1 then
      begin
        //s := Format('FR.*.W%d.Bib%d.RV=F%s', [W, Bib, v]); //alternative
        s := Format('FR.*.W%d.Bib%d.FM=%s', [W, Bib, v]);
        SL.Add(s);
      end;
    end;
  end
end;

procedure TExcelImporter.SetValue_TimeList(f: string; v: string);
var
  s: string;
begin
  if (Length(f) > 2) and (f[1] = 'I') then
  begin
    IT := StrToIntDef(Copy(f, 3, MaxInt), -1);
    if (IT > 0) and (Bib > 0) then
    begin
      //FR.Europe.W11.Bib14.IT1=2:28.00
      s := Format('FR.*.W%d.Bib%d.IT%d=%s', [W, Bib, IT, v]);
      SL.Add(s);
    end;
    if (IT = 0) and (Bib > 0) then
    begin
      s := Format('FR.*.W%d.Bib%d.FT=%s', [W, Bib, v]);
      SL.Add(s);
    end;
  end
  else if f = 'FT' then
  begin
    s := Format('FR.*.W%d.Bib%d.FT=%s', [W, Bib, v]);
    SL.Add(s);
  end
end;

procedure TExcelImporter.SetValue_ResultList(f: string; v: string);
var
  s: string;
  v1: string;
  l: Integer;
begin
  if f = 'SNR' then
  begin
    SNR := StrToIntDef(v, 0);
    if SNR <> 0 then
    begin
      s := Format('FR.*.W1.STL.Pos%d.SNR=%d', [PosID, SNR]);
      SL.Add(s);
    end;
  end
  else if f = 'Bib' then
  begin
    Bib := StrToIntDef(v, 0);
    if (SNR <> 0) and (Bib <> 0) then
    begin
      s := Format('FR.*.W1.STL.Pos%d.Bib=%d', [PosID, Bib]);
      SL.Add(s);
    end;
  end

  else if (f = 'FN')
  or (f = 'LN')
  or (f = 'SN')
  or (f = 'NC')
  or (f = 'GR')
  or (f = 'PB')
  then
  begin
    v1 := Trim(v);
    if (Length(v1) > 1) and (Pos(',', v1) > 1) then
    begin
      l := Pos(',', v1);
      v1 := Copy(v1, 1, l-1);
    end;
    s := Format('FR.*.SNR%d.%s=%s', [SNR, f, v1]);
    SL.Add(s);
  end

  else if (Length(f) > 1) and (f[1] = 'N') then
  begin
    v1 := Trim(v);
    if (Length(v1) > 1) and (Pos(',', v1) > 1) then
    begin
      l := Pos(',', v1);
      v1 := Copy(v1, 1, l-1);
    end;
    N := StrToIntDef(Copy(f, 2, MaxInt), 0);
    if (N > 0) and (SNR > 0) then
    begin
      s := Format('FR.*.SNR%d.N%d=%s', [SNR, N, v1]);
      SL.Add(s);
    end;
  end

  else if (Length(f) > 1) and (f[1] = 'R') then
  begin
    W := StrToIntDef(Copy(f, 2, MaxInt), 0);
    if (W > 0) and (Bib > 0) then
    begin
      if StrToIntDef(v, -1) > -1 then
        s := Format('FR.*.W%d.Bib%d.RV=%s', [W, Bib, v])
      else
        s := Format('FR.*.W%d.Bib%d.QU=%s', [W, Bib, v]);
      SL.Add(s);
    end;
  end
end;

procedure TExcelImporter.SetValue_CaptionList(const f: string; const v: string);
begin
  if ColCaptionBag <> nil then
    ColCaptionBag.SetCaption(f, v);
end;

procedure TExcelImporter.SetValue_CompareList(const f: string; const v: string);
var
  b, p: Integer;
begin
  b := StrToIntDef(f, -1);
  p := StrToIntDef(v, -1);
  CompareList.Add(Format('%.3d=%.5d', [b, p]));
  //CompareList.Add(Format('%s=%s', [f, v]));
end;

procedure TExcelImporter.SetValue(f: string; v: string);
begin
  if TableID = TableID_StartList then
  begin
    SetValue_StartList(f, v);
  end
  else if TableID = TableID_NameList then
  begin
    SetValue_NameList(f, v);
  end
  else if TableID = TableID_FinishList then
  begin
    SetValue_FinishList(f, v);
  end
  else if TableID = TableID_TimeList then
  begin
    SetValue_TimeList(f, v);
  end
  else if TableID = TableID_FleetList then
  begin
    SetValue_FleetList(f, v);
  end
  else if TableID = TableID_ResultList then
  begin
    SetValue_ResultList(f, v);
  end
  else if TableID = TableID_CaptionList then
  begin
    SetValue_CaptionList(f, v);
  end
  else if TableID = TableID_CompareList then
  begin
    SetValue_CompareList(f, v);
  end;
end;

procedure TExcelImporter.Transpose(ML: TStrings);
var
  i: Integer;
  j: Integer;
  s: string;
  f: string;
  v: string;
  snrIndex: Integer;
  snrString: string;
  bibIndex: Integer;
  bibString: string;
begin
  Division := '*';
  PosID := -1;
  SLField.Clear;
  snrIndex := -1;
  bibIndex := -1;
  for i := 0 to ML.Count - 1 do
  begin
    s := ML[i];
    if Trim(s) = '' then
      Continue;
    Inc(PosID);
    Bib := 0 + PosID;
    SLToken.Delimiter := Delimiter;
    SLToken.DelimitedText := s;
    SNR := 1000 + PosID;

    if (i > 0) then
    begin
      if SLToken.Count <> SLField.Count then
      begin
//        SL.Add('');
//        SL.Add('//line skipped - SLToken.Count <> SLField.Count');
//        SL.Add('');
        Continue;
      end;
    end;

    { get real SNR }
    if (i = 0) then
    begin
      snrIndex := SLToken.IndexOf('SNR');
    end;
    if (i > 0) then
    begin
      if snrIndex > -1 then
      begin
        snrString := Trim(SLToken[snrIndex]);
        SNR := StrToIntDef(snrString, SNR);
      end;
    end;

    { get real Bib }
    if (i = 0) then
    begin
      bibIndex := SLToken.IndexOf('Bib');
    end;
    if (i > 0) then
    begin
      if bibIndex > -1 then
      begin
        bibString := Trim(SLToken[bibIndex]);
        Bib := StrToIntDef(bibString, Bib);
      end;
    end;

    for j := 0 to SLToken.Count - 1 do
    begin
      v := SLToken[j];
      if i = 0 then
        SLField.Add(v)
      else
      begin
        if Trim(v) = '' then
          Continue;
        f := SLField[j];
        SetValue(f, v);
      end;
    end;
  end;
end;

procedure TExcelImporter.TransposePropList(ML: TStrings);
var
  i: Integer;
  j: Integer;
  s: string;

  sK: string;
  sV: string;
  temp: string;
begin
  for j := 0 to ML.Count - 1 do
  begin
    s := ML[j];
    if Trim(s) = '' then
      Continue;

    SLToken.Clear;
    i := Pos('=', s);
    if i > 0 then
      temp := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)))
    else
      temp := StringReplace(Trim(s), ' ', '_', [rfReplaceAll]);

    if Pos('=', temp) = 0 then
      temp := temp + '=';
    SLToken.Add(temp);
    sK := SLToken.Names[0];
    sV := SLToken.Values[sK];
    //StringReplace(sV, '_', ' ', [rfReplaceAll]);

    SetValue(sK, sV);
  end;
end;

procedure TExcelImporter.RunImportFilter(const Data: string; m: TStrings);
var
  i: Integer;
  s: string;
  FilterMode: Boolean;
begin
  SL.Clear;

  TableID := 0;
  FilterMode := False;
  m.Text := Data;
  for i := 0 to m.Count - 1 do
  begin
    s := m[i];

    { Comment Lines }
    if (s = '') or (Copy(s, 1, 2) = '//') or (Copy(s, 1, 1) = '#') then
      Continue;

    { TableEndToken for key=value property list }
    if (s = CaptionListEndToken)
    or (s = CompareListEndToken)
    then
    begin
      TransposePropList(SLFilter);
      SLFilter.Clear;
      FilterMode := False;
      TableID := 0;
    end

    { TableEndToken for delimited Tables }
    else if (s = NameListEndToken)
    or (s = StartListEndToken)
    or (s = FinishListEndToken)
    or (s = TimeListEndToken)
    or (s = FleetListEndToken)
    then
    begin
      Transpose(SLFilter);
      SLFilter.Clear;
      FilterMode := False;
      TableID := 0;
    end

    { TableStartToken, may include Parameters }
    else if (s = NameListStartToken)
    or (s = StartListStartToken)
    or (s = FinishListStartToken)
    or (s = FleetListStartToken)
    or (Pos(TimeListStartToken, s) > 0)
    or (s = CaptionListStartToken)
    or (s = CompareListStartToken)
    then
    begin
      SLFilter.Clear;
      FilterMode := True;
      SetTableID(s);
    end

    { DataLines, normal Message or delimited Line}
    else
    begin
      if FilterMode then
        SLFilter.Add(s)
      else
      begin
        s := TrimAroundEqual(s);
        SL.Add(s);
      end;
    end;
  end;
  if SLFilter.Count <> 0 then
  begin
    Main.Logger.Info('FilterError');
  end;
  m.Text := SL.Text;
end;

end.
