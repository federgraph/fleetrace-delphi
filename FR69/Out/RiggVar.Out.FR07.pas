unit RiggVar.Out.FR07;

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
  TOutput7 = class
  private
    TokenParser: TTokenParser;
    FBO: TBaseColBO;
    Grid: TReportGrid;
  public
    Node: TBaseNode;
    SortColName: string;
    Layout: Integer;
    TableCaption: string;
    ReportID: Integer;
    SortColIndex: Integer;
    Modus: string;
    constructor Create;
    destructor Destroy; override;

    procedure EventReport(SL: TStrings; AReportID, ASort: Integer; AFinish: Boolean);
    procedure RaceReport(SL: TStrings; AReportID, ARace, AIT: Integer);

    procedure GetHTML(SL: TStrings);
    procedure GetMsg(SL: TStrings; PathInfo: string);
  end;

implementation

uses
  RiggVar.BO.Def;

{ TOutput7 }

constructor TOutput7.Create;
begin
  inherited Create;
  TokenParser := TTokenParser.Create;
  Layout := 1;
  Modus := 'Points';
  Grid := TReportGrid.Create;
  Grid.Name := 'WebOutput';
end;

destructor TOutput7.Destroy;
begin
  Grid.Free;
  TokenParser.Free;
  inherited;
end;

procedure TOutput7.GetHTML(SL: TStrings);
var
  i: Integer;
  cp: TBaseColProp;
  g: TColGrid;
begin
  if not Assigned(FBO) then exit;
  if not Assigned(Node) then exit;

  Grid.Node := Node;
  Grid.ColBO := FBO;
  Grid.Layout := Layout;
  Grid.InitGrid;
  g := Grid.ColGrid;
  g.ColorSchema := colorRed;
  g.UseHTML := True;
  g.UpdateAll;

	if (SortColIndex > -1) then
		i := SortColIndex
  else
  begin
    cp := g.ColsActive.ByName[SortColName];
    if Assigned(cp) then
      i := cp.Index
    else
      i := -1;
  end;
  g.ColsActive.SortColIndex := i;

  g.AlwaysShowCurrent := True; //true=with Error-Colors and Current-Colors
  g.UpdateAll;
  case ReportID of
    2: Grid.FRContent2(SL, '', Modus); //bgcolor + sort-index-header
    3: Grid.FRContent3(SL, ''); //css
    5: Grid.FRContent5(SL, '', Modus); //bgcolor
    11: Grid.FRContent11(SL, '', Modus); //bgcolor, 1 line url sort-header
  else
    Grid.FRContent0(SL, '') //no formatting
  end;
end;

procedure TOutput7.EventReport(SL: TStrings; AReportID: Integer;
  ASort: Integer; AFinish: Boolean);
var
  ev: TEventNode;
begin
  ReportID := AReportID;
  SortColIndex := ASort;
  Layout := 0;
  TableCaption := Format('EventReport%d.Sort%d', [AReportID, ASort]);

  ev := BO.EventNode;
  Node := ev;
  if (AFinish) then
    ev.WebLayout := 1 //Layout_Finish
  else
    ev.WebLayout := 2; //--> default Layout_Points = 0 will be used

  try
    FBO := Node.BaseColBO;
    GetHTML(SL);
  finally
    ev.WebLayout := 0;
  end;
end;

procedure TOutput7.RaceReport(SL: TStrings; AReportID: Integer;
  ARace: Integer; AIT: Integer);
begin
  ReportID := AReportID;
  SortColName := '';
  SortColIndex := -1;
  Layout := 0;
  TableCaption := Format('RaceReport%d.Race%d.IT.%d', [AReportID, ARace, AIT]);

  if (AIT > -1) then
    Layout := AIT;
  Node := BO.FindNode('W' + IntToStr(ARace));
  if not Assigned(Node) then
    Node := BO.RNode[0];

  if not Assigned(Node) then
    exit;
  FBO := Node.BaseColBO;
  GetHTML(SL);
end;

procedure TOutput7.GetMsg(SL: TStrings; PathInfo: string);
var
  Race: Integer;
  IT: Integer;
  ev: TEventNode;
begin
  ReportID := 0;
  Layout := 0;
  SortColName := '';
  SortColIndex := -1;
  TableCaption := PathInfo;

  {
  if (Pos('Event.Params', PathInfo) > 0) then
  begin
    GetParams(SL);
  end
  }

  //new Event Report
  if (Pos('Event.Report', PathInfo) > 0) then
  begin
    TokenParser.sRest := PathInfo;
    TokenParser.NextToken(); //Event.
    ReportID := TokenParser.NextTokenX('Report');
    SortColIndex := TokenParser.NextTokenX('Sort');
    TokenParser.NextToken();
    Modus := TokenParser.sToken;

    //EventReport(SL, ReportID, SortColIndex, Modus = 'Finish');

    ev := BO.EventNode;
    Node := ev;
    if (Modus = 'Finish') then
      ev.WebLayout := 1 //Layout_Finish
    else
      ev.WebLayout := 2; //--> default Layout_Points = 0 will be used

    try
      FBO := Node.BaseColBO;
      GetHTML(SL);
    finally
      ev.WebLayout := 0;
    end;

  end

  //new Race Report
  else if (Pos('Race.Report', PathInfo) > 0) then
  begin
    TokenParser.sRest := PathInfo;
    TokenParser.NextToken(); //Race.
    ReportID := TokenParser.NextTokenX('Report');
    Race := TokenParser.NextTokenX('R');
    IT := TokenParser.NextTokenX('IT');

    //RaceReport(SL, ReportID, Race, IT);

    if (IT > -1) then
      Layout := IT;
    Node := BO.FindNode('W' + IntToStr(Race));
    if not Assigned(Node) then
      Node := BO.RNode[0];

    if not Assigned(Node) then
      exit;
    FBO := Node.BaseColBO;
    GetHTML(SL);
  end

  //old Event Report
  else if (Pos('Event', PathInfo) > 0) or (Pos('R0.IT', PathInfo) > 0) then
  begin
    ev := BO.EventNode;
    Node := ev;
    if PathInfo = 'Event.Finish' then
      ev.WebLayout := 1
    else if PathInfo = 'Event.Points' then
      ev.WebLayout := 2
    else if PathInfo = 'R0.IT1' then
      ev.WebLayout := 1
    else if PathInfo = 'R0.IT2' then
      ev.WebLayout := 2;
    try
      FBO := Node.BaseColBO;
      GetHTML(SL);
    finally
      ev.WebLayout := 0;
    end;
  end

  //old Race Report
  else
  begin
    TokenParser.sRest := PathInfo;
    Race := TokenParser.NextTokenX('R');
    IT := TokenParser.NextTokenX('IT');
    if IT > -1 then
      Layout := IT;
    Node := BO.FindNode('W' + IntToStr(Race));
    if not Assigned(Node) then
      Node := BO.RNode[0];

    if not Assigned(Node) then
      exit;
    FBO := Node.BaseColBO;
    GetHTML(SL);
  end;
end;

end.
