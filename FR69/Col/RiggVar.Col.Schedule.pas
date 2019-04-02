unit RiggVar.Col.Schedule;

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
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Grid.BlockHtml,
  RiggVar.Col.Stammdaten,
  RiggVar.Util.Classes,
  RiggVar.BO.StartInfo,
  RiggVar.DAL.Redirector;

type
  TScheduleNode = class;

  TScheduleRowCollectionItem = class(TBaseRowCollectionItem)
  private
    function GetScheduleNode: TScheduleNode;
    procedure SetModified(const Value: Boolean);
  public
    StartInfo: TStartInfo;
    Requesting: Boolean;
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TScheduleNode read GetScheduleNode;
  published
  end;

  TScheduleRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TScheduleRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TScheduleRowCollectionItem);
  public
    function Add: TScheduleRowCollectionItem;
    function FindKey(Bib: Integer): TScheduleRowCollectionItem;
    property Items[Index: Integer]: TScheduleRowCollectionItem read GetItem
      write SetItem;
  end;

  TScheduleColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TScheduleBO = class;

  TScheduleNode = class(TBaseNode)
  private
    function GetScheduleRowCollection: TScheduleRowCollection;
    function GetScheduleBO: TScheduleBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Init(SL: TStringList);
    property ScheduleRowCollection: TScheduleRowCollection read GetScheduleRowCollection;
    property ScheduleBO: TScheduleBO read GetScheduleBO;
  end;

  TScheduleBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure EditStartTime(crgs: TBaseRowCollectionItem; var Value: string);
    procedure InitColsActiveLayout(StringGrid: TColGrid;
      aLayout: Integer); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

  TScheduleGrid = class(THtmlGridBlock<TScheduleNode, TScheduleBO>)
  public
    constructor Create(aParent: TWinControl);
  public
    property GB;
  end;

implementation

{ TScheduleRowCollectionItem }

constructor TScheduleRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;

end;

destructor TScheduleRowCollectionItem.Destroy;
begin

  inherited;
end;

function TScheduleRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TScheduleRowCollection;
begin
  cl := Collection as TScheduleRowCollection;
  result := cl.BaseNode;
end;

function TScheduleRowCollectionItem.GetScheduleNode: TScheduleNode;
var
  cl: TScheduleRowCollection;
begin
  result := nil;
  cl := Collection as TScheduleRowCollection;
  if cl.BaseNode is TScheduleNode then
    result := TScheduleNode(cl.BaseNode);
end;

procedure TScheduleRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

{ TScheduleRowCollection }

function TScheduleRowCollection.Add: TScheduleRowCollectionItem;
begin
  Result := TScheduleRowCollectionItem(inherited Add);
  Result.BaseID := Count;
end;

function TScheduleRowCollection.FindKey(
  Bib: Integer): TScheduleRowCollectionItem;
var
  i: Integer;
  o: TScheduleRowCollectionItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.BaseID = Bib) then
    begin
      result := o;
      break;
    end;
  end;
end;

function TScheduleRowCollection.GetItem(
  Index: Integer): TScheduleRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TScheduleRowCollectionItem(inherited GetItem(Index));
end;

procedure TScheduleRowCollection.SetItem(Index: Integer;
  const Value: TScheduleRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TScheduleColProp }

procedure TScheduleColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TScheduleRowCollectionItem;
begin
  Value := '';
  if crgs is TScheduleRowCollectionItem then
    cr := TScheduleRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_Race' then
    Value := IntToStr(cr.StartInfo.Race)

  else if NameID = 'col_Fleet' then
    Value := IntToStr(cr.StartInfo.Fleet)

  else if NameID = 'col_FleetName' then
    Value := cr.StartInfo.FleetName

  else if NameID = 'col_StartTime' then
    Value := cr.StartInfo.StartTime

  else if NameID = 'col_Length' then
    Value := cr.StartInfo.Length

  else if NameID = 'col_IsRacing' then
    Value := BoolStr[cr.StartInfo.IsRacing]

  else if NameID = 'col_Weight' then
    Value := cr.StartInfo.Weight

  else if NameID = 'col_GroupRank' then
    Value := cr.StartInfo.GroupRank

  else if NameID = 'col_IsFinalRace' then
    Value := BoolStr[cr.StartInfo.RaceInfo.IsFinalRace]

  else if NameID = 'col_IsFinalFleet' then
    Value := BoolStr[cr.StartInfo.IsFinalFleet]

end;

procedure TScheduleColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  { Race }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Race';
  cp.Caption := 'Race';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Fleet }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Fleet';
  cp.Caption := 'Fleet';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { FleetName }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FleetName';
  cp.Caption := 'FN';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { StartTime }
  cp := ColsAvail.Add;
  cp.NameID := 'col_StartTime';
  cp.Caption := 'StartTime';
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Length }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Length';
  cp.Caption := 'Length';
  cp.Width := 100;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { IsRacing }
  cp := ColsAvail.Add;
  cp.NameID := 'col_IsRacing';
  cp.Caption := 'Racing';
  cp.Width := 60;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { Weight }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Weight';
  cp.Caption := 'Weight';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { GroupRank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GroupRank';
  cp.Caption := 'GR';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { IsFinalRace }
  cp := ColsAvail.Add;
  cp.NameID := 'col_IsFinalRace';
  cp.Caption := 'FR';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;

  { IsFinalFleet }
  cp := ColsAvail.Add;
  cp.NameID := 'col_IsFinalFleet';
  cp.Caption := 'FF';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
end;

{ TScheduleNode }

constructor TScheduleNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TScheduleRowCollection.Create(Self,
    TScheduleRowCollectionItem);
end;

destructor TScheduleNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TScheduleNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TScheduleColProp;
end;

function TScheduleNode.GetScheduleBO: TScheduleBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TScheduleBO;
end;

function TScheduleNode.GetScheduleRowCollection: TScheduleRowCollection;
begin
  result := BaseRowCollection as TScheduleRowCollection;
end;

procedure TScheduleNode.Init(SL: TStringList);
var
  i: Integer;
  si: TStartInfo;
  cr: TScheduleRowCollectionItem;
begin
  BaseRowCollection.Clear;
  for i := 0 to SL.Count - 1 do
  begin
    si := TStartInfo(SL.Objects[i]);
    cr := ScheduleRowCollection.Add;
    cr.StartInfo := si;
  end;
end;

{ TScheduleBO }

function TScheduleBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TScheduleBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TScheduleBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
var
  cp: TBaseColProp;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    AddColumn('col_Race');
    AddColumn('col_Fleet');

    AddColumn('col_FleetName');

    cp := AddColumn('col_StartTime');
    cp.OnFinishEdit := EditStartTime;
    cp.ReadOnly := False;

    AddColumn('col_Length');
    AddColumn('col_IsRacing');
    AddColumn('col_Weight');
    AddColumn('col_GroupRank');

    AddColumn('col_IsFinalRace');
    AddColumn('col_IsFinalFleet');
  end;
end;

procedure TScheduleBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TScheduleBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TScheduleBO.EditStartTime(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TScheduleRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TScheduleRowCollectionItem) then
  begin
    cr := TScheduleRowCollectionItem(crgs);
    cr.StartInfo.StartTime := Value;
  end;
end;

{ TScheduleGrid }

constructor TScheduleGrid.Create(aParent: TWinControl);
begin
  inherited Create;
  Name := 'ScheduleGrid';
  Parent := aParent;
  ColBO := TScheduleBO.Create;
  Node := TScheduleNode.Create(ColBO);
  ColBO.CurrentNode := Node;
  CreateGrid;
end;

end.
