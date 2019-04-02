unit RiggVar.Out.FR03;

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
  RiggVar.Grid.SimpleBlock,
  RiggVar.Col.Race,
  RiggVar.Col.RaceInfo,
  RiggVar.BO.Time,
  RiggVar.DAL.Redirector;

type
  TOutput3 = class
  private
    procedure CreateGrid;
    procedure DestroyGrid;
    procedure CopyRaceInfo(ql: TRaceRowCollection; IT: Integer);
  protected
    SL: TStringList;
    TokenParser: TTokenParser;
    ColBO: TRaceInfoBO;
    Node: TRaceInfoNode;
    GB: TSimpleGridBlock;
    SortColName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateOutput(Race: Integer; IT: Integer);
    function GetHTM: string;
    procedure GetMsg(aSL: TStrings; PathInfo: string);
    function GetContent(roID: string): string;
    function IsOutputSorted: Boolean;
    procedure SaveHTM(FileName: string);
    procedure GetTimingReport(Memo: TStrings);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TOutput3 }

constructor TOutput3.Create;
begin
  inherited Create;
  SL := TDBStringList.Create;
  TokenParser := TTokenParser.Create;
  CreateGrid;
  SortColName := 'col_PosR';
end;

destructor TOutput3.Destroy;
begin
  DestroyGrid;
  TokenParser.Free;
  SL.Free;
  inherited;
end;

procedure TOutput3.CreateGrid;
begin
  ColBO := TRaceInfoBO.Create;
  Node := TRaceInfoNode.Create(ColBO);
  ColBO.CurrentNode := Node;

  GB := TSimpleGridBlock.Create;
  GB.Name := 'WebOutput3';
  GB.ColBO := ColBO;
  GB.Node := Node;

  GB.InitGrid;

  GB.ColGrid.AlwaysShowCurrent := True;
  GB.ColGrid.ColorSchema := colorMoneyGreen;

  //Node.IsSorted := IsSorted;
  GB.ColGrid.UpdateAll;
end;

procedure TOutput3.DestroyGrid;
begin
  GB.Free;
  GB := nil;
  Node.Free;
  Node := nil;
  ColBO.Free;
  ColBO := nil;
end;

procedure TOutput3.UpdateOutput(Race: Integer; IT: Integer);
var
  oldLayout: Integer;
  rn: TRaceNode;
begin
  oldLayout := Node.Layout;
  //Node.Layout := 1;
  if (Race > 0) and (Race < Length(BO.RNode)) then
  begin
    rn := BO.RNode[Race];
    if rn <> nil then
      CopyRaceInfo(rn.RaceRowCollection, IT)
    else
      rn.RaceRowCollection.ClearResult;
  end;
  if (oldLayout <> Node.Layout) then
    ColBO.InitColsActive(GB.ColGrid);
  GB.ColGrid.UpdateAll;
end;

function TOutput3.GetContent(roID: string): string;
var
  i: Integer;
  cp: TBaseColProp;
  Race: Integer;
  IT: Integer;
begin
  SL.Clear;
  GB.ColGrid.UseHTML := True;
  try

    TokenParser.sRest := roID;
    Race := TokenParser.NextTokenX('R');
    IT := TokenParser.NextTokenX('IT');

    if Race = 0 then
      Race := 1;

    ColBO.InitColsActive(GB.ColGrid);

    cp := GB.ColGrid.ColsActive.ByName[SortColName];
    if Assigned(cp) then
      i := cp.Index
    else
      i := -1;
    GB.ColGrid.ColsActive.SortColIndex := i;

    UpdateOutput(Race, IT);

    GB.ColGrid.Content(SL, roID);
    result := SL.Text;
  finally
    GB.ColGrid.UseHTML := False;
  end;
end;

procedure TOutput3.GetMsg(aSL: TStrings; PathInfo: string);
var
  i: Integer;
begin
  SortColName := 'col_PosR';
  Node.Layout := 0;

  GetContent(PathInfo);
  for i := 0 to SL.Count-1 do
    aSL.Add(SL[i]);
end;

function TOutput3.GetHTM: string;
begin
  GB.ColGrid.UseHTML := True;
  try
    GB.ColGrid.UpdateAll;
    GB.ColGrid.Content(SL, '');
    result := SL.Text;
  finally
    GB.ColGrid.UseHTML := False;
  end;
end;

function TOutput3.IsOutputSorted: Boolean;
begin
  result := GB.ColGrid.ColsActive.SortColIndex > 0;
end;

procedure TOutput3.SaveHTM(FileName: string);
begin
  GetHTM;
  Main.StoreAdapter.StringListSaveToFile(SL, FileName);
  SL.Clear;
end;

procedure TOutput3.CopyRaceInfo(ql: TRaceRowCollection; IT: Integer);
var
  qr: TRaceRowCollectionItem;
  i: Integer;
  cl: TRaceInfoRowCollection;
  cr: TRaceInfoRowCollectionItem;
  TP: TTimePoint;
begin
  cl := Node.RaceInfoRowCollection;
  cl.Clear;
  for i := 0 to ql.Count-1 do
  begin
    qr := ql.Items[i];
    cr := cl.Add;
    cr.BaseID := cl.Count;

    TP := qr.IT[IT];

    if (TP <> nil) then
    begin
      cr.Bib := qr.Bib;
      cr.Time := TP.OTime.ToString;
      cr.Behind := TP.Behind.ToString;
      cr.Rank := TP.Rank;
      cr.PosR := TP.PosR;
      cr.QU := qr.QU.ToString;
    end
    else
      cr.ClearResult;
  end;
end;

procedure TOutput3.GetTimingReport(Memo: TStrings);
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  Race: Integer;
  IT: Integer;
  i, j: Integer;

  g: TColGrid;
  Bib: Integer;
  OTime: TPTime;
  Behind, LastBehind: TPTime;
  Diff: Integer;

  cp: TBaseColProp;
  infoCL: TRaceInfoRowCollection;
  fs: string;
  BibColIndex: Integer;
  t: Integer;
  s: string;
  dn: string;
begin
  try
    Memo.Clear;

    g := GB.ColGrid;

    fs := 'FR.%s.W%d.Bib%d.IT%d=%s';
    dn := BO.BOParams.DivisionName;

    cp := g.ColsActive.ByName['col_Bib'];
    if Assigned(cp) then
      BibColIndex := cp.Index
    else
      BibColIndex := 0;

    cp := g.ColsActive.ByName['col_Time'];
    if Assigned(cp) then
      i := cp.Index
    else
      i := -1;

    g.ColsActive.SortColIndex := i;
    for Race := 1 to BO.BOParams.RaceCount do
    begin
      for j := 1 to BO.BOParams.ITCount + 1 do
      begin
        IT := j;
        if IT = BO.BOParams.ITCount + 1 then
          IT := 0;
        UpdateOutput(Race, IT);
        Memo.Add('');
        s := Format('#W%d IT%d', [Race, IT]);
        Memo.Add(Format('%8d, %s', [0, s]));
        Memo.Add('');
        cl := BO.RNode[Race].RaceRowCollection;
        infoCL := Node.RaceInfoRowCollection;
        LastBehind := nil;
        for i := 0 to infoCL.Count - 1 do
        begin
          Bib := StrToIntDef(GB.Grid.Cells[BibColIndex, i+1], 1);
          cr := cl.FindBib(Bib);
          OTime := cr.IT[IT].OTime;
          Behind := cr.IT[IT].Behind;
          if LastBehind <> nil then
            Diff := Behind.AsInteger - LastBehind.AsInteger
          else
            Diff := 0;
          t := Diff div 10;
          s := Format(fs, [dn, Race, Bib, IT, OTime.ToString]);
          if not OTime.TimePresent then
            s := '//' + s;
          Memo.Add(Format('%8d, %s', [t, s]));
          LastBehind := Behind;
        end;
      end;
    end;
  except on e: Exception do
    Memo.Add(e.Message);
  end;
end;

end.
