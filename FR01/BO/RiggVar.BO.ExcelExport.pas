unit RiggVar.BO.ExcelExport;

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
  RiggVar.Grid.ColGrid,
  RiggVar.Col.Stammdaten,
  RiggVar.BO.ExcelImport;

type
  TExcelExporter = class
  private
    SLToken: TStringList;
    FDelimiter: char;
    procedure SetDelimiter(const Value: char);
    //
    procedure FillTable(TableID: Integer);
    procedure CopyLines(Memo: TStrings);
  protected
    SL: TStringList;
    procedure GetNameList;
    procedure GetFinishList;
    procedure GetResultList;
    procedure GetStartList;
    procedure GetTimeList(r: Integer);
    procedure GetCaptionList;
    procedure GetFleetList;
  public
    constructor Create;
    destructor Destroy; override;
    //
    function GetString(TableID: Integer): string;
    procedure AddLines(TableID: Integer; Memo: TStrings);
    procedure AddSection(TableID: Integer; Memo: TStrings);
    procedure AddTimingSection(Memo: TStrings; r: Integer);
    property Delimiter: char read FDelimiter write SetDelimiter;
  end;

implementation

uses
  RiggVar.Col.Event,
  RiggVar.BO.Def;

{ TExcelExporter }

constructor TExcelExporter.Create;
begin
  inherited Create;
  SL := TStringList.Create;
  SLToken := TStringList.Create;
  Delimiter := ';';
end;

destructor TExcelExporter.Destroy;
begin
  SL.Free;
  SLToken.Free;
  inherited Destroy;
end;

procedure TExcelExporter.SetDelimiter(const Value: char);
begin
  FDelimiter := Value;
end;

procedure TExcelExporter.FillTable(TableID: Integer);
begin
  case TableID of
    TableID_NameList: GetNameList;
    TableID_StartList: GetStartList;
    TableID_FleetList: GetFleetList;
    TableID_FinishList: GetFinishList;
    TableID_ResultList: GetResultList;
    TableID_CaptionList: GetCaptionList;
  end;
end;

procedure TExcelExporter.CopyLines(Memo: TStrings);
var
  i: Integer;
begin
  for i := 0 to SL.Count - 1 do
  begin
    Memo.Add(SL[i]);
  end;
end;

procedure TExcelExporter.AddTimingSection(Memo: TStrings; r: Integer);
begin
  Memo.Add(TimeListStartToken + '.R' + IntToStr(r));
  GetTimeList(r);
  CopyLines(Memo);
  Memo.Add(TimeListEndToken);
  Memo.Add('');
end;

procedure TExcelExporter.AddSection(TableID: Integer; Memo: TStrings);
var
  r: Integer;
begin
  case TableID of
    TableID_NameList:
    begin
      Memo.Add(NameListStartToken);
      GetNameList;
      CopyLines(Memo);
      Memo.Add(NameListEndToken);
    end;
    TableID_StartList:
    begin
      Memo.Add(StartListStartToken);
      GetStartList;
      CopyLines(Memo);
      Memo.Add(StartListEndToken);
    end;
    TableID_FleetList:
    begin
      Memo.Add(FleetListStartToken);
      GetFleetList;
      CopyLines(Memo);
      Memo.Add(FleetListEndToken);
    end;
    TableID_FinishList:
    begin
      Memo.Add(FinishListStartToken);
      GetFinishList;
      CopyLines(Memo);
      Memo.Add(FinishListEndToken);
    end;
    TableID_ResultList:
    begin
      Memo.Add(ResultListStartToken);
      GetResultList;
      CopyLines(Memo);
      Memo.Add(ResultListEndToken);
    end;
    TableID_TimeList:
    begin
      for r := 1 to BO.BOParams.RaceCount do
      begin
        if r > 0 then
          SL.Add('');
        Memo.Add(TimeListStartToken + '.R' + IntToStr(r));
        GetTimeList(r);
        CopyLines(Memo);
        Memo.Add(TimeListEndToken);
        Memo.Add('');
      end;
    end;
    TableID_CaptionList:
    begin
      Memo.Add(CaptionListStartToken);
      GetCaptionList;
      CopyLines(Memo);
      Memo.Add(CaptionListEndToken);
    end;
  end;
end;

procedure TExcelExporter.AddLines(TableID: Integer; Memo: TStrings);
var
  i: Integer;
begin
  FillTable(TableID);
  for i := 0 to SL.Count - 1 do
  begin
    Memo.Add(SL[i]);
  end;
end;

function TExcelExporter.GetString(TableID: Integer): string;
begin
  FillTable(TableID);
  result := SL.Text;
end;

procedure TExcelExporter.GetNameList;
var
  i: Integer;
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  s: string;
  j: Integer;
  FieldUsed: array of Boolean;
begin
  SL.Clear;
  SLToken.Delimiter := FDelimiter;
  cl := BO.StammdatenNode.StammdatenRowCollection;
  if cl.Count < 1 then
    Exit;

  cr := cl.Items[0];

  //cache FieldUsed for speedy access
  SetLength(FieldUsed, cr.FieldCount+1);
  for j := 1 to cr.FieldCount do
  begin
    FieldUsed[j] := cr.FieldUsed[j];
  end;

  { HeaderLine }
  SLToken.Clear;
  SLToken.Add('SNR');
  for j := 1 to cr.FieldCount do
  begin
    if FieldUsed[j] then
      SLToken.Add('N' + IntToStr(j));
  end;
  s := SLToken.DelimitedText;
  SL.Add(s);

  { DataLines }
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SLToken.Clear;
    SLToken.Add(IntToStr(cr.SNR));
    for j := 1 to cr.FieldCount do
    begin
      if not FieldUsed[j] then
        Continue;
      SLToken.Add(cr.FieldValue[j]);
    end;
    s := SLToken.DelimitedText;
    SL.Add(s);
  end;
end;

procedure TExcelExporter.GetStartList;
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  s: string;
begin
  SL.Clear;
  SLToken.Delimiter := FDelimiter;
  cl := BO.EventNode.EventRowCollection;
  if cl.Count < 1 then
    Exit;

  { HeaderLine }
  SLToken.Clear;
  SLToken.Add('Pos');
  SLToken.Add('SNR');
  SLToken.Add('Bib');
  s := SLToken.DelimitedText;
  SL.Add(s);

  { DataLines }
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SLToken.Clear;
    SLToken.Add(IntToStr(cr.BaseID));
    SLToken.Add(IntToStr(cr.SNR));
    SLToken.Add(IntToStr(cr.Bib));
    s := SLToken.DelimitedText;
    SL.Add(s);
  end;
end;

procedure TExcelExporter.GetFinishList;
var
  i: Integer;
  r: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  ere: TEventRaceEntry;
  s: string;
begin
  SL.Clear;
  SLToken.Delimiter := FDelimiter;
  cl := BO.EventNode.EventRowCollection;
  if cl.Count < 1 then
    Exit;

  { HeaderLine }
  SLToken.Clear;
  SLToken.Add('SNR');
  SLToken.Add('Bib');
  cr := cl.Items[0];
  for r := 1 to cr.RCount - 1 do
  begin
    SLToken.Add('R' + IntToStr(r));
  end;
  s := SLToken.DelimitedText;
  SL.Add(s);

  { DataLines }
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SLToken.Clear;
    SLToken.Add(IntToStr(cr.SNR));
    SLToken.Add(IntToStr(cr.Bib));
    for r := 1 to cr.RCount - 1 do
    begin
      ere := cr.Race[r];
      SLToken.Add(IntToStr(ere.OTime));
    end;
    s := SLToken.DelimitedText;
    SL.Add(s);
  end;
end;

procedure TExcelExporter.GetFleetList;
var
  i: Integer;
  r: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  ere: TEventRaceEntry;
  s: string;
begin
  SL.Clear;
  SLToken.Delimiter := FDelimiter;
  cl := BO.EventNode.EventRowCollection;
  if cl.Count < 1 then
    Exit;

  { HeaderLine }
  SLToken.Clear;
  SLToken.Add('SNR');
  SLToken.Add('Bib');
  cr := cl.Items[0];
  for r := 1 to cr.RCount - 1 do
  begin
    SLToken.Add('R' + IntToStr(r));
  end;
  s := SLToken.DelimitedText;
  SL.Add(s);

  { DataLines }
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SLToken.Clear;
    SLToken.Add(IntToStr(cr.SNR));
    SLToken.Add(IntToStr(cr.Bib));
    for r := 1 to cr.RCount - 1 do
    begin
      ere := cr.Race[r];
      SLToken.Add(IntToStr(ere.Fleet));
    end;
    s := SLToken.DelimitedText;
    SL.Add(s);
  end;
end;

procedure TExcelExporter.GetResultList;
var
  i: Integer;
  r: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  ere: TEventRaceEntry;
  s: string;
begin
  SL.Clear;
  SLToken.Delimiter := FDelimiter;
  cl := BO.EventNode.EventRowCollection;
  if cl.Count < 1 then
    Exit;

  { HeaderLine }
  SLToken.Clear;
  SLToken.Add('SNR');
  SLToken.Add('Bib');
  SLToken.Add('N1');
  SLToken.Add('N2');
  SLToken.Add('N3');
  SLToken.Add('N4');
  SLToken.Add('N5');
  SLToken.Add('N6');
  cr := cl.Items[0];
  for r := 1 to cr.RCount - 1 do
  begin
    SLToken.Add('R' + IntToStr(r));
  end;
  s := SLToken.DelimitedText;
  SL.Add(s);

  { DataLines }
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    SLToken.Clear;
    SLToken.Add(IntToStr(cr.SNR));
    SLToken.Add(IntToStr(cr.Bib));
    SLToken.Add(cr.FN);
    SLToken.Add(cr.LN);
    SLToken.Add(cr.SN);
    SLToken.Add(cr.NC);
    SLToken.Add(cr.GR);
    SLToken.Add(cr.PB);
    for r := 1 to cr.RCount - 1 do
    begin
      ere := cr.Race[r];
      SLToken.Add(ere.RaceValue);
    end;
    s := SLToken.DelimitedText;
    SL.Add(s);
  end;
end;

procedure TExcelExporter.GetTimeList(r: Integer);
begin
end;

procedure TExcelExporter.GetCaptionList;
begin
  SL.Clear;
  SLToken.Clear;
  SLToken.Delimiter := FDelimiter;
  if ColCaptionBag <> nil then
    SL.Text := ColCaptionBag.Text;
end;

end.
