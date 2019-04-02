unit RiggVar.Out.FR06;

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
  TOutput6 = class
  private
    Grid: TReportGrid;
    procedure WritePropertyTable(SL: TStrings);
  public
    Node: TBaseNode;
    Layout: Integer;
    WantProsa: Boolean;
    constructor Create;
    destructor Destroy; override;

    procedure IndexReport(SL: TStrings);
    procedure CssReport(SL: TStrings);
    procedure ProsaReport(SL: TStrings);
    procedure FinishReport(SL: TStrings);
    procedure FinishTable(SL: TStrings);
    procedure PointsReport(SL: TStrings);
    procedure PointsTable(SL: TStrings);
    procedure TableSortReport(SL: TStrings; Modus: Integer);
    procedure TableSortData(SL: TStrings);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BO.EventProps;

{ TOutput6 }

constructor TOutput6.Create;
begin
  inherited Create;
  WantProsa := false;
  Layout := 0;
  Grid := TReportGrid.Create;
  Grid.Name := 'WebOutput';
end;

destructor TOutput6.Destroy;
begin
  Grid.Free;
  inherited Destroy;
end;

procedure TOutput6.IndexReport(SL: TStrings);
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
  SL.Clear;
  sep := ',';

  //Shortcuts
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

  //first column is ID-column, always counting from 1 upwards
  for r := 0 to cl.Count - 1 do
  begin
    SL.Add(IntToStr(r+1));
  end;

  //from second column to last column
  for c := 1 to g.ColsActive.Count - 1 do
  begin
    cp := g.ColsActive.Items[c];

    //retain default sort-order for unsortable columns
    if not cp.Sortable then
    begin
      for r := 0 to cl.Count - 1 do
      begin
        SL[r] := SL[r] + sep + IntToStr(r+1);
      end;
      Continue;
    end;

    g.InitDisplayOrder(c);
    for r := 0 to cl.Count - 1 do
    begin
      k := g.DisplayOrder.DisplayIndex[r];
      cr := cl[k];
      SL[r] := SL[r] + sep + IntToStr(cr.BaseID);
    end;
  end;

  for r := 0 to cl.Count - 2 do
  begin
    SL[r] := '[' + SL[r] + '],';
  end;
  r := cl.Count-1;
  SL[r] := '[' + SL[r] + ']';
  SL.Insert(0, '[');
  SL.Add(']');
end;

procedure TOutput6.WritePropertyTable(SL: TStrings);
var
  fs: string;
  EP: TEventProps;
  procedure tr(n: string; v: string);
  begin
    SL.Add(Format(fs, [n, v]));
  end;
begin
  EP := BO.EventProps;
  fs := '<tr><td>%s</td><td>%s</td></tr>';
  SL.Add('<table class="fr properties">');
  SL.Add('<thead><tr><th>Name</th><th>Value</th></tr></thead>');
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
  tr('EP.FirstFinalRace', IntToStr(EP.FirstFinalRace));
  SL.Add('</table>');
end;

procedure TOutput6.CssReport(SL: TStrings);
var
  crlf: string;
  en: TEventNode;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  cp: TBaseColProp;
  g: TColGrid;

  s, css, clss: string;
  r, c: Integer;

  p, race: Integer;
  srace: string;
  fleet: Integer;
begin
  SL.Clear();

  en := BO.EventNode;
  cl := en.EventRowCollection;

  Node := en; //instance variable

  //Grid Setup
  Grid.ColBO := Node.BaseColBO;
  Grid.Node := Node;
  Grid.Layout := Layout;
  Grid.Name := 'CssReport';
  Grid.InitGrid;
  g := Grid.ColGrid; //shortcut
  g.ColsActive.SortColIndex := 0;
  g.AlwaysShowCurrent := false; //true=with Error-Colors and Current-Colors
  g.UseHTML := true;
  g.UpdateAll();

  SL.Add('<div id="results">');
  SL.Add('<table class="sortable fr results">');
  SL.Add(crlf);

  //if (aCaption != "")
  //{
  //    SL.Add("<caption>" + aCaption + "</caption>");
  //    SL.Add(crlf);
  //}

  //header row
  SL.Add('<thead>');
  SL.Add('<tr>');
  for c := 0 to Grid.Grid.ColCount - 1 do
  begin
    cp := g.ColsActive[c];
    if (cp = nil) then Continue;

    //content of field
    s := g.GridModel.Cells[c, 0];
    if (s = '') then
        s := '&nbsp;';

    //css for field
    css := 'h';
    if (cp.Alignment = taLeftJustify) then
        css := css + 'l';
    clss := Format(' class="%s"', [css]);

    //add field
    SL.Add(Format('<th%s>%s</th>', [clss, s]));
  end;
  SL.Add('</tr>');
  SL.Add('</thead>');
  SL.Add(crlf);

  //normal rows
  for r := 1 to g.GridModel.RowCount - 1 do
  begin
    cr := cl.Items[r-1];

    SL.Add('<tr>');
    for c := 0 to g.GridModel.ColCount - 1 do
    begin
      cp := g.ColsActive[c];
      if (cp = nil) then Continue;

      //content of field
      s := g.GridModel.Cells[c, r];
      if (s = '') then
          s := '&nbsp;';

      //css for field
      clss := Grid.GetCSSClass(g.CellProps.CellProp[c, r]);

      //if column is a race-column
      p := Pos('col_R', cp.NameID);
      if (p > 0) and (Length(cp.NameID) > 5) then
      begin
        srace := Copy(cp.NameID, 6, MaxInt);
        race := StrToIntDef(srace, -1);
        if (race > 0) then
        begin
          fleet := cr.Race[race].Fleet;
          css := 'g' + IntToStr(fleet);
          clss := Format(' class="%s"', [css]);
        end
      end
      else
      begin
        if Pos(clss, 'bgcolor') > 0 then
        begin
          css := 'n';
          clss := Format(' class="%s"', [css]);
        end;
      end;

      //add field
      SL.Add(Format('<td%s>%s</td>', [clss, s]));
    end;
    SL.Add('</tr>');
    SL.Add(crlf);
  end;

  SL.Add('</table>');
  SL.Add('</div>');
end;

procedure TOutput6.ProsaReport(SL: TStrings);
var
  crlf: string;
  en: TEventNode;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  cp: TBaseColProp;
  g: TColGrid;

  s, css, clss: string;
  r, c: Integer;

  p, race: Integer;
  srace: string;
  fleet: Integer;
begin
  SL.Clear();

  en := BO.EventNode;
  cl := en.EventRowCollection;

  Node := en; //instance variable

  //Grid Setup
  Grid.ColBO := Node.BaseColBO;
  Grid.Node := Node;
  Grid.Layout := Layout;
  Grid.InitGrid;
  g := Grid.ColGrid; //shortcut
  g.ColsActive.SortColIndex := 0;
  g.AlwaysShowCurrent := false; //true=with Error-Colors and Current-Colors
  g.UseHTML := true;
  g.UpdateAll();

  SL.Add('<div id="results">');
  SL.Add(crlf);

  for r := 1 to g.GridModel.RowCount - 1 do
  begin
    cr := cl.Items[r-1];

    SL.Add('<p>');
    for c := 0 to g.GridModel.ColCount - 1 do
    begin
      cp := g.ColsActive[c];
      if (cp = nil) then Continue;

      //content of field
      s := g.GridModel.Cells[c, r];
      if (s = '') then
          s := '&nbsp;';

      //css for field
      clss := Grid.GetCSSClass(g.CellProps.CellProp[c, r]);

      //if column is a race-column
      p := Pos('col_R', cp.NameID);
      if (p > 0) and (Length(cp.NameID) > 5) then
      begin
        srace := Copy(cp.NameID, 6, MaxInt);
        race := StrToIntDef(srace, -1);
        if (race > 0) then
        begin
          fleet := cr.Race[race].Fleet;
          css := 'g' + IntToStr(fleet);
          clss := Format(' class="%s"', [css]);
        end
      end
      else
      begin
        if Pos(clss, 'bgcolor') > 0 then
        begin
          css := 'n';
          clss := Format(' class="%s"', [css]);
        end;
      end;

      //add field
      SL.Add(Format('<span%s>%s</span>', [clss, s]));
    end;
    SL.Add('</p>');
    SL.Add(crlf);
  end;

  SL.Add('</div>');
end;

procedure TOutput6.TableSortReport(SL: TStrings; Modus: Integer);
var
  b: Boolean;
begin
  SL.Clear;
//now see WebMotor.WrapReport for header
//(with ContextPath dependent stylesheet/javascript links)
//and footer

//  SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0');
//  SL.Add('  Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
//  SL.Add('<html xmlns="http://www.w3.org/1999/xhtml">');
//  SL.Add('<head>');
//  SL.Add(Format('<script type="text/javascript" src="%sjavascripts/core.js"></script>', [ContextPath]));
//  SL.Add(Format('<script type="text/javascript" src="%sjavascripts/rvts.js"></script>', [ContextPath]));
//  SL.Add(Format('<link type="text/css" href="%sstylesheets/fr42.css" rel="stylesheet" />', [ContextPath]));
//  SL.Add('</head>');
//  SL.Add('<body>');
  SL.Add('<h2>' + BO.EventProps.EventName + '</h2>');
  SL.Add('');

  b := BO.EventBO.WantDiffCols;

  if Modus = 1 then
    BO.EventNode.WebLayout := 1 //Finish
  else
    BO.EventNode.WebLayout := 2; //Points

  BO.EventBO.WantDiffCols := False;

  TableSortData(SL);
  BO.EventNode.WebLayout := 0;
  BO.EventBO.WantDiffCols := b;

  SL.Add('');
//  SL.Add('</body>');
//  SL.Add('</html>');
end;

procedure TOutput6.TableSortData(SL: TStrings);
var
  SL1: TStringList;
  i: Integer;
begin
  SL1 := TStringList.Create;
  try
    if WantProsa then
      ProsaReport(SL1)
    else
    CssReport(SL1);

    for i := 0 to SL1.Count - 1 do
    begin
      SL.Add(SL1[i]);
    end;
    SL.Add('');
    WritePropertyTable(SL);
    SL.Add('');
    SL.Add('<div id="index_table"><pre>');
    IndexReport(SL1);
    for i := 0 to SL1.Count - 1 do
    begin
      SL.Add(SL1[i]);
    end;
    SL.Add('</pre></div>');
  finally
    SL1.Free;
  end;
end;

procedure TOutput6.FinishTable(SL: TStrings);
var
  en: TEventNode;
begin
  SL.Clear();
  en := BO.EventNode;
  en.WebLayout := 1;
	TableSortData(SL);
	en.WebLayout := 0;
end;

procedure TOutput6.PointsTable(SL: TStrings);
var
  en: TEventNode;
begin
  SL.Clear();
  en := BO.EventNode;
  en.WebLayout := 2;
	TableSortData(SL);
	en.WebLayout := 0;
end;

procedure TOutput6.FinishReport(SL: TStrings);
begin
  TableSortReport(SL, 1);
end;

procedure TOutput6.PointsReport(SL: TStrings);
begin
  TableSortReport(SL, 2);
end;

end.
