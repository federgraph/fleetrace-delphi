unit RiggVar.Out.FR02;

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
  Vcl.Controls,
  RiggVar.Util.Classes,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleBlock,
  RiggVar.Col.BaseEntry,
  RiggVar.Col.Event,
  RiggVar.Col.Uniqua,
  RiggVar.Util.Props,
  RiggVar.DAL.Redirector;

type
  TOutput2 = class
  private
    TokenParser: TTokenParser;
    ColBO: TUniquaBO;
    Node: TUniquaNode;
    GB: TSimpleGridBlock;
    SL: TStringList;
    procedure CreateGrid;
    procedure DestroyGrid;
    procedure UpdateOutputE(ql: TEventRowCollection);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateOutput(roID: string);
    function GetHTM: string;
    procedure GetMsg(aSL: TStrings; roID: string);
    function GetContent(roID: string): string;
    function IsOutputSorted: Boolean;
    procedure SaveHTM(FileName: string);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TOutput2 }

constructor TOutput2.Create;
begin
  inherited Create;
  SL := TDBStringList.Create;
  TokenParser := TTokenParser.Create;
  CreateGrid;
end;

destructor TOutput2.Destroy;
begin
  DestroyGrid;
  TokenParser.Free;
  SL.Free;
  inherited;
end;

procedure TOutput2.CreateGrid;
begin
  Assert(GB = nil);

  ColBO := TUniquaBO.Create;
  Node := TUniquaNode.Create(ColBO);
  ColBO.CurrentNode := Node;

  GB := TSimpleGridBlock.Create;
  GB.Name := 'WebOutput2';
  GB.ColBO := ColBO;
  GB.Node := Node;

  GB.InitGrid;

  GB.ColGrid.ColorSchema := colorMoneyGreen;
  //Node.IsSorted := IsSorted;
  GB.ColGrid.UpdateAll;
end;

procedure TOutput2.DestroyGrid;
begin
  GB.Free;
  GB := nil;
  Node.Free;
  Node := nil;
  ColBO.Free;
  ColBO := nil;
end;

function TOutput2.GetContent(roID: string): string;
var
  i: Integer;
  cp: TBaseColProp;
  SortColName: string;
  Race: Integer;
  //IT: Integer;
begin
  SL.Clear;
  GB.ColGrid.UseHTML := True;
  try

    TokenParser.sRest := roID;
    Race := TokenParser.NextTokenX('R');
    //IT := TokenParser.NextTokenX('IT');

    if Race = 0 then
    begin
      SortColName := 'col_Ges';
      Self.Node.Layout := 2; //ohne Bedeutung bei Uniqua?
    end
    else if Race > 0 then
    begin
      SortColName := 'col_Wf' + IntToStr(Race);
      Self.Node.Layout := 2;
    end
    else
    begin
      SortColName := '';
      Self.Node.Layout := 1;
    end;

    ColBO.InitColsActive(GB.ColGrid);

    cp := GB.ColGrid.ColsActive.ByName[SortColName];
    if Assigned(cp) then
      i := cp.Index
    else
      i := -1;
    GB.ColGrid.ColsActive.SortColIndex := i;

    UpdateOutput(roID);

    GB.ColGrid.Content(SL, roID);
    result := SL.Text;
  finally
    GB.ColGrid.UseHTML := False;
  end;
end;

function TOutput2.GetHTM: string;
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

procedure TOutput2.GetMsg(aSL: TStrings; roID: string);
var
  i: Integer;
begin
  GetContent(roID);
  for i := 0 to SL.Count-1 do
    aSL.Add(SL[i]);
end;

function TOutput2.IsOutputSorted: Boolean;
begin
  result := GB.ColGrid.ColsActive.SortColIndex > 0;
end;

procedure TOutput2.SaveHTM(FileName: string);
begin
  GetHTM;
  Main.StoreAdapter.StringListSaveToFile(SL, FileName);
  SL.Clear;
end;

procedure TOutput2.UpdateOutput(roID: string);
var
  oldLayout: Integer;
begin
  oldLayout := Node.Layout;
  Node.Layout := 1;

  UpdateOutputE(BO.EventNode.EventRowCollection);

  if (oldLayout <> Node.Layout) then
    ColBO.InitColsActive(GB.ColGrid);
  GB.ColGrid.UpdateAll;
end;

procedure TOutput2.UpdateOutputE(ql: TEventRowCollection);
var
  qr: TEventRowCollectionItem;
  i: Integer;
  cl: TUniquaRowCollection;
  cr: TUniquaRowCollectionItem;
  j: Integer;
  props: TProps;
begin
  cl := Node.UniquaRowCollection;
  cl.Clear;
  for i := 0 to ql.Count-1 do
  begin
    qr := ql.Items[i];
    cr := cl.Add;
    cr.BaseID := cl.Count;

    cr.Plz := qr.GPosR;
    cr.SegNr := qr.SNR;
    for j := 0 to qr.RCount-1 do
      if j < cr.WfCount then
        cr.Wf[j] := qr.Race[j].RaceValue;

    cr.Ges := qr.GPoints;
    cr.Rangl := FormatFloat('0.00', qr.RA);

    props := qr.Props;
    if Assigned(props) then
    begin
      cr.Steuermann := props.Value['SteuermannName'];
      cr.ClubS := props.Value['SteuermannClub'];
      cr.JgS := StrToIntDef(props.Value['SteuermannJg'], -1);
      cr.Vorschoter := props.Value['VorschoterName'];
      cr.ClubV := props.Value['VorschoterClub'];
      cr.JgV := StrToIntDef(props.Value['VorschoterJg'], -1);
    end
    else
    begin
      cr.Steuermann := qr.FN;
      cr.ClubS := qr.LN;
      {
      cr.JgS := StrToIntDef(qr.SN, -1);
      cr.Vorschoter := qr.NC;
      cr.ClubV := qr.GR;
      cr.JgV := StrToIntDef(qr.PB, -1);
      }
    end;
  end;
end;

end.
