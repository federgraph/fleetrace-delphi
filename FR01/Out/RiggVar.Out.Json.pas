unit RiggVar.Out.Json;

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
  RiggVar.Util.Classes,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Col.Event,
  RiggVar.Out.GridBlock;

type
  TOutputJson = class
  private
    Grid: TReportGrid;
    procedure PropertyJson(SL: TStrings);
  public
    Node: TBaseNode;
    Layout: Integer;
    constructor Create;
    destructor Destroy; override;

    procedure IndexJson(ML: TStrings);
    procedure TableJson(ML: TStrings);
    procedure FinishJson(SL: TStrings);
    procedure PointsJson(SL: TStrings);
    procedure TableSortReport(SL: TStrings; Modus: Integer);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BO.EventProps;

{ TOutputJson }

constructor TOutputJson.Create;
begin
  inherited Create;
  Layout := 0;
  Grid := TReportGrid.Create;
  Grid.Name := 'JsonOutput';
end;

destructor TOutputJson.Destroy;
begin
  Grid.Free;
  inherited Destroy;
end;

procedure TOutputJson.IndexJson(ML: TStrings);
var
  c: Integer;
  r: Integer;
  k: Integer;
  sep: string;
  g: TColGrid;
  ev: TEventBO;
  cl: TBaseRowCollection;
  cr: TBaseRowCollectionItem;
  cp: TBaseColProp;
begin
  ML.Clear;
  sep := ',';
  ev := BO.EventBO;
  Node := ev.CurrentNode;
  cl := Node.BaseRowCollection;
  Grid.Node := Node;
  Grid.ColBO := ev;
  Grid.Name := 'IndexReport';
  Grid.InitGrid;
  g := Grid.ColGrid;
  g.ColorSchema := colorRed;
  g.UseHTML := True;
  g.UpdateAll;

  for r := 0 to cl.Count - 1 do
  begin
    ML.Add(IntToStr(r+1));
  end;

  for c := 1 to g.ColsActive.Count - 1 do
  begin
    cp := g.ColsActive.Items[c];
    if not cp.Sortable then
    begin
      for r := 0 to cl.Count - 1 do
      begin
        ML[r] := ML[r] + sep + IntToStr(r+1);
      end;
      Continue;
    end;
    g.InitDisplayOrder(c);
    for r := 0 to cl.Count - 1 do
    begin
      k := g.DisplayOrder.DisplayIndex[r];
      cr := cl[k];
      ML[r] := ML[r] + sep + IntToStr(cr.BaseID);
    end;
  end;

  for r := 0 to cl.Count - 2 do
  begin
    ML[r] := '[' + ML[r] + '],';
  end;
  r := cl.Count-1;
  ML[r] := '[' + ML[r] + ']';
end;

procedure TOutputJson.PropertyJson(SL: TStrings);
var
  fs: string;
  EP: TEventProps;
  procedure tr(n: string; v: string);
  begin
    SL.Add(Format(fs, [n, v]));
  end;
begin
  EP := BO.EventProps;
  fs := '"%s":"%s",';
  SL.Add('"props": {');
  tr('RH.MD5', BO.ResultHash.MD5);
  tr('EP.Name', EP.EventName);
  tr('EP.ScoringSystem', JavaScore_ScoringSystemStrings[EP.ScoringSystem]);
  tr('EP.Throwouts', IntToStr(EP.Throwouts));
  if EP.FirstIs75 then
    tr('EP.FirstIs75', BoolStr[EP.FirstIs75]);
  if EP.ReorderRAF = false then
    tr('EP.ReorderRAF', BoolStr[EP.ReorderRAF]);
  tr('EP.FieldCount', EP.FieldCount);
  tr('EP.NameFieldSchema', EP.NameSchema);
  tr('EP.NameFieldCount', EP.NameFieldCount);
  tr('EP.NameFieldOrder', EP.NameFieldOrder);
  tr('EP.FieldCaptions', EP.FieldCaptions);
  tr('EP.UseFleets', BoolStr[EP.UseFleets]);
  tr('EP.TargetFleetSize', IntToStr(EP.TargetFleetSize));
  fs := '"%s":"%s"';
  tr('EP.FirstFinalRace', IntToStr(EP.FirstFinalRace));
  SL.Add('},');
end;

procedure TOutputJson.TableJson(ML: TStrings);
var
  en: TEventNode;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  cp: TBaseColProp;
  g: TColGrid;

  s, css: string;
  r, c: Integer;

  p, race: Integer;
  srace: string;
  fleet: Integer;
  l, rl: Integer;
begin
  ML.Clear();
  en := BO.EventNode;
  cl := en.EventRowCollection;
  Node := en;
  Grid.ColBO := Node.BaseColBO;
  Grid.Node := Node;
  Grid.Layout := Layout;
  Grid.Name := 'CssReport';
  Grid.InitGrid;
  g := Grid.ColGrid;
  g.ColsActive.SortColIndex := 0;
  g.AlwaysShowCurrent := false;
  g.UseHTML := true;
  g.UpdateAll();

  ML.Add('"head":{"cols":[');
  l := Grid.Grid.ColCount - 1;
  for c := 0 to l do
  begin
    cp := g.ColsActive[c];
    if (cp = nil) then
      Continue;
    s := g.GridModel.Cells[c, 0];
    css := 'h';
    if (cp.Alignment = taLeftJustify) then
        css := css + 'l';
    if c = l then
      ML.Add(Format('{"c":"%s", "v":"%s"}', [css, s]))
    else
      ML.Add(Format('{"c":"%s", "v":"%s"},', [css, s]));
  end;
  ML.Add(']},');

  ML.Add('"body":[');

  rl := g.GridModel.RowCount - 1;
  for r := 1 to rl do
  begin
    cr := cl.Items[r-1];
    ML.Add('{"cols":[');
    l := g.GridModel.ColCount - 1;
    for c := 0 to l do
    begin
      cp := g.ColsActive[c];
      if (cp = nil) then
        Continue;
      s := g.GridModel.Cells[c, r];
      css := Grid.GetCSSValue(g.CellProps.CellProp[c, r]);
      p := Pos('col_R', cp.NameID);
      if (p > 0) and (Length(cp.NameID) > 5) then
      begin
        srace := Copy(cp.NameID, 6, MaxInt);
        race := StrToIntDef(srace, -1);
        if (race > 0) then
        begin
          fleet := cr.Race[race].Fleet;
          css := 'g' + IntToStr(fleet);
        end
      end
      else
      begin
        if Pos(css, 'bgcolor') > 0 then
        begin
          css := 'n';
        end;
      end;
      if c = l then
        ML.Add(Format('{"c":"%s", "v":"%s"}', [css, s]))
      else
        ML.Add(Format('{"c":"%s", "v":"%s"},', [css, s]))
    end;
    if r = rl then
      ML.Add(']}')
    else
      ML.Add(']},')
  end;

  ML.Add('],');
end;

procedure TOutputJson.TableSortReport(SL: TStrings; Modus: Integer);
var
  b: Boolean;
  ML: TStringList;
  i: Integer;
begin
  b := BO.EventBO.WantDiffCols;
  ML := TStringList.Create;
  try
    SL.Clear;
    SL.Add('{');

    SL.Add('"event": "' + BO.EventProps.EventName + '",');

    //Table
    if Modus = 1 then
      BO.EventNode.WebLayout := 1 //Finish
    else
      BO.EventNode.WebLayout := 2; //Points
    BO.EventBO.WantDiffCols := False;
    TableJson(ML);
    for i := 0 to ML.Count - 1 do
    begin
      SL.Add(ML[i]);
    end;

    PropertyJson(SL);

    //Index
    SL.Add('"index": [');
    IndexJson(ML);
    for i := 0 to ML.Count - 1 do
    begin
      SL.Add(ML[i]);
    end;
    SL.Add(']');

    SL.Add('}');
  finally
    ML.Free;
  end;
  BO.EventNode.WebLayout := 0;
  BO.EventBO.WantDiffCols := b;
end;

procedure TOutputJson.FinishJson(SL: TStrings);
begin
  TableSortReport(SL, 1);
end;

procedure TOutputJson.PointsJson(SL: TStrings);
begin
  TableSortReport(SL, 2);
end;

end.
