unit RiggVar.Col.Cache;

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
  RiggVar.BO.TemplateIDs,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.SimpleBlock,
  RiggVar.DAL.Redirector;

type
  TCacheNode = class;

  TCacheRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FReport: Integer;
    FRace: Integer;
    FIT: Integer;
    FSort: Integer;
    FMode: Integer;
    FAge: Integer;
    FUpdates: Integer;
    FHits: Integer;
    FTimeStamp: TDateTime;
    FMillies: Integer;
    //
    FRequest: string;
    FAddXMLHeader: Boolean;
    FData: string;
    FIsXml: Boolean;
    function GetOutputCacheNode: TCacheNode;
    procedure SetModified(const Value: Boolean);
    function GetRequest: string;
    function GetModus: string;
    procedure SetData(const Value: string);
    function GetAge: Integer;
    function GetReportHeader: string;
    function GetData: string;
  public
    Requesting: Boolean;
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure StoreData(answer: string; aMillies: Integer);
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TCacheNode read GetOutputCacheNode;
    property Request: string read GetRequest write FRequest;
    property Modus: string read GetModus;
    property Data: string read GetData write SetData;
    property ReportHeader: string read GetReportHeader;
  published
    property Report: Integer read FReport write FReport;
    property Race: Integer read FRace write FRace;
    property IT: Integer read FIT write FIT;
    property Sort: Integer read FSort write FSort;
    property Mode: Integer read FMode write FMode;
    property Age: Integer read GetAge write FAge;
    property Updates: Integer read FUpdates write FUpdates;
    property Hits: Integer read FHits write FHits;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property IsXml: Boolean read FIsXml write FIsXml;
    property AddXMLHeader: Boolean read FAddXMLHeader write FAddXMLHeader;
    property Millies: Integer read FMillies write FMillies;
  end;

  TCacheRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TCacheRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TCacheRowCollectionItem);
  public
    function Add: TCacheRowCollectionItem;
    function FindKey(Bib: Integer): TCacheRowCollectionItem;
    property Items[Index: Integer]: TCacheRowCollectionItem read GetItem
      write SetItem;
  end;

  TCacheColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TCacheBO = class;

  TCacheNode = class(TBaseNode)
  private
    function GetOutputCacheRowCollection: TCacheRowCollection;
    function GetOutputCacheBO: TCacheBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    RaceCount: Integer;
    ITCount: Integer;
    Age: Integer;
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    property OutputCacheRowCollection: TCacheRowCollection read
      GetOutputCacheRowCollection;
    property OutputCacheBO: TCacheBO read
      GetOutputCacheBO;
  end;

  TCacheBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure InitColsActiveLayout(StringGrid: TColGrid;
      aLayout: Integer); override;
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

  TCacheGrid = class
  private
    SL: TStringList;
    procedure CreateGrid;
    procedure DestroyGrid;
  protected
    SortColName: string;
  public
    SG: TSimpleGridBlock;
    ColBO: TCacheBO;
    Node: TCacheNode;
    constructor Create;
    destructor Destroy; override;
    function GetHTM: string;
    function IsOutputSorted: Boolean;
    procedure SaveHTM(FileName: string);
  end;

  TOutputCache = class(TCacheGrid)
  private
    function NextRequest: string;
    function GetEventType: Integer;
  public
    Synchronized: Boolean;
    Status: Integer;
    RequestID: Integer;
    CurrentRequest: string;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure DoOnIdle;
    procedure StoreAnswer(Request, Answer: string; Millies: Integer);
    procedure UpdateParams(RaceCount, ITCount: Integer);
    procedure ProcessInput(s: string);
    property EventType: Integer read GetEventType;
  end;

const
  CacheStatus_Idle = 0;
  CacheStatus_HaveRequest = 1;
  CacheStatus_WaitingForAnswer = 2;
  EmptyXML = '<?xml version="1.0" ?></Empty></xml>';

var
  CacheRequestToken: string = 'FR.*.Request.';

implementation

uses
  RiggVar.App.Main;

{ TOutputCacheRowCollectionItem }

procedure TCacheRowCollectionItem.Assign(Source: TPersistent);
var
  o: TCacheRowCollectionItem;
begin
  if Source is TCacheRowCollectionItem then
  begin
    o := TCacheRowCollectionItem(Source);
    Report := o.Report;
    Race := o.Race;
    IT := o.IT;
    Sort := o.Sort;
    Mode := o.Mode;
    Age := o.Age;
    Updates := o.Updates;
    Hits := o.Hits;
    Millies := o.Millies;
    TimeStamp := o.TimeStamp;
  end
end;

constructor TCacheRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;

end;

destructor TCacheRowCollectionItem.Destroy;
begin

  inherited;
end;

function TCacheRowCollectionItem.GetAge: Integer;
begin
  Result := ru.Age - FAge;
end;

function TCacheRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TCacheRowCollection;
begin
  cl := Collection as TCacheRowCollection;
  result := cl.BaseNode;
end;

function TCacheRowCollectionItem.GetData: string;
begin
  if IsXml then
  begin
    if FData = 'no connection' then
      result := '<answer>no connection</answer>'
    else if FData = '' then
      result := '<answer>empty</answer>'
    else
      result := FData;
  end
  else
    result := FData;
end;

function TCacheRowCollectionItem.GetModus: string;
begin
  if Mode = 0 then
    result := 'Finish'
  else
    result := 'Points';
end;

function TCacheRowCollectionItem.GetOutputCacheNode: TCacheNode;
var
  cl: TCacheRowCollection;
begin
  result := nil;
  cl := Collection as TCacheRowCollection;
  if cl.BaseNode is TCacheNode then
    result := TCacheNode(cl.BaseNode);
end;

function TCacheRowCollectionItem.GetReportHeader: string;
begin
  result := Format('<pre>Age=%d Updates=%d Hits=%d TimeStamp=%s</pre>', [FAge, FUpdates, FHits, DateTimeToStr(FTimeStamp)])
end;

function TCacheRowCollectionItem.GetRequest: string;
begin
  result := '';
  if Report > 999 then
    result := CacheRequestToken + FRequest
  else if Report < 0 then
    result := CacheRequestToken + 'RiggVar.Params'
  else if Report < 100 then
    result := Format('%sHTM.Web.Event.Report%d.Sort%d.%s', [CacheRequestToken, Report, Sort, Modus])
  else
    result := Format('%sHTM.Web.Race.Report%d.R%d.IT%d', [CacheRequestToken, Report-100, Race, IT])
end;

procedure TCacheRowCollectionItem.SetData(const Value: string);
begin
  FData := Value;
  Updates := FUpdates + 1;
  FAge := ru.Age;
  {
  if AddXMLHeader then
  begin
    FData := <?xml version="1.0" ?> + Value;
  end;
  }
end;

procedure TCacheRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

procedure TCacheRowCollectionItem.StoreData(answer: string; aMillies: Integer);
begin
  Data := answer;
  TimeStamp := Now;
  Millies := aMillies;
end;

{ TOutputCacheRowCollection }

function TCacheRowCollection.Add: TCacheRowCollectionItem;
begin
  Result := TCacheRowCollectionItem(inherited Add);
  Result.BaseID := Count;
  Result.TimeStamp := Now;
end;

function TCacheRowCollection.FindKey(
  Bib: Integer): TCacheRowCollectionItem;
var
  i: Integer;
  o: TCacheRowCollectionItem;
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

function TCacheRowCollection.GetItem(
  Index: Integer): TCacheRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TCacheRowCollectionItem(inherited GetItem(Index));
end;

procedure TCacheRowCollection.SetItem(Index: Integer;
  const Value: TCacheRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

{ TOutputCacheColProp }

procedure TCacheColProp.GetTextDefault(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TCacheRowCollectionItem;
begin
  Value := '';
  if crgs is TCacheRowCollectionItem then
    cr := TCacheRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_Report' then
    Value := IntToStr(cr.Report)

  else if NameID = 'col_Race' then
    Value := IntToStr(cr.Race)

  else if NameID = 'col_IT' then
    Value := IntToStr(cr.IT)

  else if NameID = 'col_Sort' then
    Value := IntToStr(cr.Sort)

  else if NameID = 'col_Mode' then
  begin
    if cr.Mode = 0 then
      Value := 'F'
    else
      Value := 'P';
  end

  else if NameID = 'col_Age' then
    Value := IntToStr(cr.Age)

  else if NameID = 'col_Updates' then
    Value := IntToStr(cr.Updates)

  else if NameID = 'col_Hits' then
    Value := IntToStr(cr.Hits)

  else if NameID = 'col_Millies' then
    Value := IntToStr(cr.Millies)

  else if NameID = 'col_TimeStamp' then
    Value := DateTimeToStr(cr.TimeStamp)

  else if NameID = 'col_Request' then
    Value := cr.Request
end;

procedure TCacheColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  { Report }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Report';
  cp.Caption := 'Report';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Race }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Race';
  cp.Caption := 'Race';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { IT }
  cp := ColsAvail.Add;
  cp.NameID := 'col_IT';
  cp.Caption := 'IT';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Sort }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Sort';
  cp.Caption := 'Sort';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Mode }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Mode';
  cp.Caption := 'Mode';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Age }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Age';
  cp.Caption := 'Age';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Updates }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Updates';
  cp.Caption := 'Updates';
  cp.Width := 70;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Hits }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Hits';
  cp.Caption := 'Hits';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.Descending := True;

  { Millies }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Millies';
  cp.Caption := 'Millies';
  cp.Width := 40;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.Descending := True;

  { Timestamp }
  cp := ColsAvail.Add;
  cp.NameID := 'col_TimeStamp';
  cp.Caption := 'TimeStamp';
  cp.Width := 120;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Request }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Request';
  cp.Caption := 'Request';
  cp.Width := 300;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
end;

{ TOutputCacheNode }

procedure TCacheNode.Calc;
begin
  inherited;
end;

constructor TCacheNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TCacheRowCollection.Create(Self,
    TCacheRowCollectionItem);
  RaceCount := 3;
  ITCount := 2;
  Age := 1;
end;

destructor TCacheNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TCacheNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TCacheColProp;
end;

function TCacheNode.GetOutputCacheBO: TCacheBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TCacheBO;
end;

function TCacheNode.GetOutputCacheRowCollection: TCacheRowCollection;
begin
  result := BaseRowCollection as TCacheRowCollection;
end;

procedure TCacheNode.Init(RowCount: Integer);
var
  o: TCacheRowCollectionItem;
  i: Integer;
begin
  BaseRowCollection.Clear;

  for i := 0 to RowCount - 1 do
  begin
    o := OutputCacheRowCollection.Add;
    o.Report := RowCount - i;
    o.TimeStamp := Now;
  end;
end;

procedure TCacheNode.Load;
var
  o: TCacheRowCollectionItem;
  r, i, s: Integer;
begin
  OutputCacheRowCollection.Clear;

  //GetParams (must be first line!)
  o := OutputCacheRowCollection.Add;
  o.Report := -1;
  o.Race := 0;
  o.IT := 0;
  o.Sort := 0;
  o.Mode := 0;
  o.TimeStamp := Now;

  //xml-messages
  o := OutputCacheRowCollection.Add;
  o.Report := 1001;
  o.Request := 'XML.Data.A';
  o.AddXMLHeader := True;
  o.IsXml := True;

  o := OutputCacheRowCollection.Add;
  o.Report := 1002;
  o.Request := 'XML.Data.E';
  o.AddXMLHeader := True;
  o.IsXml := True;

  o := OutputCacheRowCollection.Add;
  o.Report := 1003;
  o.Request := 'JavaScore.ProxyXmlInput';
  o.AddXMLHeader := True;
  o.IsXml := True;

  o := OutputCacheRowCollection.Add;
  o.Report := 1004;
  o.Request := 'JavaScore.ProxyXmlOutput';
  o.AddXMLHeader := True;
  o.IsXml := True;

  o := OutputCacheRowCollection.Add;
  o.Report := 1005;
  o.Request := 'JavaScore.XML';
  o.AddXMLHeader := False;
  o.IsXml := True;

  o := OutputCacheRowCollection.Add;
  o.Report := 1006;
  o.Request := 'RiggVar.TXT';
  o.AddXMLHeader := True;
  o.IsXml := True;

  //EventReports
  for s := 0 to RaceCount + 8 do //SpaltenAnzahl
  begin
    o := OutputCacheRowCollection.Add;
    o.Report := 11;
    o.Race := 0;
    o.IT := 0;
    o.Sort := s;
    o.Mode := 0;
    o.TimeStamp := Now;

    o := OutputCacheRowCollection.Add;
    o.Report := 11;
    o.Race := 0;
    o.IT := 0;
    o.Sort := s;
    o.Mode := 1;
    o.TimeStamp := Now;
  end;

  //RaceReports
  for r := 1 to RaceCount do begin
    for i := 0 to ITCount do begin
      o := OutputCacheRowCollection.Add;
      o.Report := 105; //100 + X
      o.Race := r;
      o.IT := i;
      o.Sort := 0;
      o.Mode := 0;
      o.TimeStamp := Now;
    end;
  end;
end;

{ TOutputCacheBO }

function TCacheBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

function TCacheBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

procedure TCacheBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');
    AddColumn('col_Report');
    AddColumn('col_Race');
    AddColumn('col_IT');
    AddColumn('col_Mode');
    AddColumn('col_Sort');
    AddColumn('col_Age');
    AddColumn('col_Updates');
    AddColumn('col_Hits');
    AddColumn('col_Millies');
    AddColumn('col_TimeStamp');
    AddColumn('col_Request');
  end;
end;

procedure TCacheBO.SetCurrentNode(
  const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TCacheBO.SetCurrentRow(
  const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

{ TCacheGrid }

constructor TCacheGrid.Create;
begin
  inherited Create;
  SL := TDBStringList.Create;
  CreateGrid;
  SortColName := 'col_Report';
end;

destructor TCacheGrid.Destroy;
begin
  DestroyGrid;
  SL.Free;
  inherited;
end;

procedure TCacheGrid.CreateGrid;
begin
  if (SG = nil) then
  begin
    ColBO := TCacheBO.Create;
    Node := TCacheNode.Create(ColBO);
    ColBO.CurrentNode := Node;

    SG := TSimpleGridBlock.Create;
    SG.ColBO := ColBO;
    SG.Node := Node;
    SG.Name := 'CacheGrid';

    SG.InitGrid;

    SG.ColGrid.AlwaysShowCurrent := True;
    SG.ColGrid.ColorSchema := colorMoneyGreen;
    SG.ColGrid.AutoMark := True;

    SG.ColGrid.UpdateAll;
  end;
end;

procedure TCacheGrid.DestroyGrid;
begin
  SG.Free;
  SG := nil;
  Node.Free;
  Node := nil;
  ColBO.Free;
  ColBO := nil;
end;

function TCacheGrid.GetHTM: string;
begin
  SL.Clear;
  SG.ColGrid.UseHTML := True;
  try
    //Grid.ColsActive.SortColIndex := 8;
    SG.ColGrid.UpdateAll;
    SG.ColGrid.Content(SL, '');
    result := SL.Text;
  finally
    SG.ColGrid.UseHTML := False;
  end;
end;

function TCacheGrid.IsOutputSorted: Boolean;
begin
  result := SG.ColGrid.ColsActive.SortColIndex > 0;
end;

procedure TCacheGrid.SaveHTM(FileName: string);
begin
  GetHTM;
  Main.StoreAdapter.StringListSaveToFile(SL, FileName);
  SL.Clear;
end;

{ TOutputCache }

constructor TOutputCache.Create;
begin
  inherited Create;
  Node.Load;
  SG.ColGrid.UpdateAll;
end;

destructor TOutputCache.Destroy;
begin
  //
  inherited;
end;

procedure TOutputCache.StoreAnswer(Request, Answer: string; Millies: Integer);
var
  cr: TCacheRowCollectionItem;
begin
  if Request = CurrentRequest then
  begin
    cr := Node.OutputCacheRowCollection.FindKey(RequestID);
    if (cr <> nil) then
      cr.StoreData(Answer, Millies);
  end;
  Status := CacheStatus_Idle;
end;

procedure TOutputCache.DoOnIdle;
var
  s: string;
begin
  case Status of
    CacheStatus_Idle:
    begin
      s := NextRequest;
      if (s <> '') then
      begin
        CurrentRequest := s;
        Status := CacheStatus_HaveRequest;
      end;
      //Grid.ShowData;
    end;

    CacheStatus_HaveRequest:
    begin

    end;

    CacheStatus_WaitingForAnswer:
    begin

    end;
  end;
end;

function TOutputCache.NextRequest: string;
var
  i: Integer;
  cl: TCacheRowCollection;
  cr: TCacheRowCollectionItem;
begin
  result := '';
  cl := Node.OutputCacheRowCollection;
  for i := 0 to cl.Count -1 do
  begin
    cr := cl.Items[i];
    if cr.Age <= 0 then
      continue;
    if (i = 0) and Synchronized then
      continue;

    result := cr.Request;
    RequestID := cr.BaseID;
    CurrentRequest := result;
    ColBO.CurrentRow := cr;
    break;
  end;
end;

procedure TOutputCache.ProcessInput(s: string);
begin
  Node.Age := Node.Age + 1;
end;

procedure TOutputCache.UpdateParams(RaceCount: Integer; ITCount: Integer);
var
  cr: TCacheRowCollectionItem;
begin
  if (Node.RaceCount <> RaceCount) or (Node.ITCount <> ITCount) then
  begin
    Node.RaceCount := RaceCount;
    Node.ITCount := ITCount;
    Node.Load;
    SG.ColGrid.UpdateAll;
  end;
  cr := Node.OutputCacheRowCollection.Items[0];
  if Assigned(cr) then
  begin
    cr.Age := Node.Age;
    cr.TimeStamp := Now;
    cr.Data := Format('RaceCount=%d'#13#10'ITCount=%d'#13#10, [RaceCount, ITCount]);
  end;
  Synchronized := True;
  Status := CacheStatus_Idle;
end;

function TOutputCache.GetEventType: Integer;
begin
  result := TypFREvent;
end;

procedure TOutputCache.Reset;
begin
  Status := CacheStatus_Idle;
end;

end.
