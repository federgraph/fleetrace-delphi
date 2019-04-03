unit RiggVar.Col.Stammdaten;

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
  Vcl.Grids,
  RiggVar.Grid.ColGrid,
  RiggVar.Col.BaseEntry,
  RiggVar.Util.Props,
  RiggVar.BO.MsgToken,
  RiggVar.BO.UndoManager;

type
  TStammdatenBO = class;
  TStammdatenNode = class;

  TStammdatenEntry = class(TBaseEntry)
  public
    SNR: Integer;
    FN: string;
    LN: string;
    SN: string;
    NC: string;
    GR: string;
    PB: string;
  end;

  TStammdatenRemoteObject = class(TStammdatenEntry)
  protected
    procedure GetOutput; override;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function GetCommaText(SL: TStrings): string; override;
    procedure SetCommaText(SL: TStrings); override;
    function GetCSV_Header: string; override;
    //procedure GetFieldDefs(FD: TFieldDefs);
    //procedure UpdateDataSet(DS: TDataSet);
  end;

  TStammdatenRowCollection = class;

  TStammdatenRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FOwner: TStammdatenRowCollection;
    FSNR: Integer;
    FFN: string;
    FLN: string;
    FSN: string;
    FNC: string;
    FGR: string;
    FPB: string;
    FProps: TProps;
    FDN: string;
    procedure SetFN(const Value: string);
    procedure SetGR(const Value: string);
    procedure SetLN(const Value: string);
    procedure SetNC(const Value: string);
    procedure SetPB(const Value: string);
    procedure SetSN(const Value: string);
    procedure SetSNR(const Value: Integer);
    function GetFieldCount: Integer;
    function GetStammdatenNode: TStammdatenNode;
    function GetFieldValue(FieldIndex: Integer): string;
    procedure SetFieldValue(FieldIndex: Integer; const Value: string);
    function GetFieldUsed(FieldIndex: Integer): Boolean;
  public
    constructor Create(ACollection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CalcDisplayName; virtual;
    property ru: TStammdatenNode read GetStammdatenNode;
    property FieldValue [Index: Integer]: string read GetFieldValue write SetFieldValue;
    property FieldUsed [Index: Integer]: Boolean read GetFieldUsed;
  published
    property SNR: Integer read FSNR write SetSNR;
    property FN: string read FFN write SetFN;
    property LN: string read FLN write SetLN;
    property SN: string read FSN write SetSN;
    property NC: string read FNC write SetNC;
    property GR: string read FGR write SetGR;
    property PB: string read FPB write SetPB;
    property Props: TProps read FProps;
    property DN: string read FDN write FDN;
    property FieldCount: Integer read GetFieldCount;
  end;

  TStammdatenRowCollection = class(TBaseRowCollection)
  private
    MapList: TStringList;
    FieldCaptionList: TStringList;
    FFieldMap: string;
    FFieldCount: Integer;
    FSchemaCode: Integer;
    function GetItem(Index: Integer): TStammdatenRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TStammdatenRowCollectionItem);
    procedure SetFieldMap(const Value: string);
    procedure SetFieldCount(const Value: Integer);
    function GetStammdatenNode: TStammdatenNode;
    function GetFieldCaptions: string;
    procedure SetFieldCaptions(const Value: string);
    procedure SetSchemaCode(Value: Integer);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TBaseRowCollectionItemClass);
    destructor Destroy; override;
    procedure CalcDisplayName(cr: TStammdatenRowCollectionItem);
    function Add: TStammdatenRowCollectionItem;
    procedure Swap(f1, f2: Integer);
    procedure UpdateItem(e: TStammdatenEntry);
    procedure ShowTableInGrid(StringGrid: TStringGrid);
    function FindKey(SNR: Integer): TStammdatenRowCollectionItem;
    function GetFieldCaption(Index: Integer): string;
    function GetStandardFieldCaption(Index, NameSchema: Integer): string;
    property ru: TStammdatenNode read GetStammdatenNode;
    property Items[Index: Integer]: TStammdatenRowCollectionItem read GetItem write SetItem;
    property FieldMap: string read FFieldMap write SetFieldMap;
    property FieldCount: Integer read FFieldCount write SetFieldCount;
    property FieldCaptions: string read GetFieldCaptions write SetFieldCaptions;
    property SchemaCode: Integer read FSchemaCode write SetSchemaCode;
  end;

  TStammdatenColProp = class(TBaseColProp)
  public
    function GetFieldCount: Integer;
    function GetStammdatenNode: TStammdatenNode;
    function GetStammdatenRowCollection: TStammdatenRowCollection;
    function GetFieldCaptionDef(cl: TStammdatenRowCollection; Index: Integer; def: string): string; overload;
    function GetFieldCaptionDef(Index: Integer; def: string): string; overload;
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string); override;
  end;

  TStammdatenNode = class(TBaseNode)
  private
    FStammdatenRowCollection: TStammdatenRowCollection;
    function GetStammdatenBO: TStammdatenBO;
    function GetNameSchema: string;
    procedure SetNameSchema(const Value: string);
    function GetSchemaCode: Integer;
  protected
    function GetBaseRowCollection: TBaseRowCollection; override;
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Init(RowCount: Integer);
    property StammdatenRowCollection: TStammdatenRowCollection read FStammdatenRowCollection;
    property StammdatenBO: TStammdatenBO read GetStammdatenBO;
    property NameSchema: string read GetNameSchema write SetNameSchema;
    property SchemaCode: Integer read GetSchemaCode;
  end;

  TStammdatenBO = class(TBaseColBO)
  private
    UndoAgent: TUndoAgent;
    FCurrentNode: TStammdatenNode;
    FCurrentRow: TStammdatenRowCollectionItem;
    function GetFieldCount: Integer;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    constructor Create;
    procedure InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer); override;
    procedure EditSNR(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditFN(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditLN(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditSN(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditNC(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditGR(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditPB(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditNameColumn(crgs: TBaseRowCollectionItem; var Value: string; const ColName: string);
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
    property FieldCount: Integer read GetFieldCount;
  end;

implementation

uses
  RiggVar.BO.Def;

const
	NumID_SNR = -1;
	NumID_FN = 1;
	NumID_LN = 2;
	NumID_SN = 3;
	NumID_NC = 4;
	NumID_GR = 5;
	NumID_PB = 6;

{ TStammdatenRowCollectionItem }

constructor TStammdatenRowCollectionItem.Create(ACollection: TBaseRowCollection);
begin
  inherited;
  FOwner := Collection as TStammdatenRowCollection;
  FProps := TProps.Create;
end;

destructor TStammdatenRowCollectionItem.Destroy;
begin
  FProps.Free;
  inherited;
end;

procedure TStammdatenRowCollectionItem.Assign(Source: TPersistent);
var
  o: TStammdatenRowCollectionItem;
  e: TStammdatenEntry;
begin
  if Source is TStammdatenRowCollectionItem then
  begin
    o := TStammdatenRowCollectionItem(Source);
    SNR := o.SNR;
    FN := o.FN;
    LN := o.LN;
    SN := o.SN;
    NC := o.NC;
    GR := o.GR;
    PB := o.PB;
    Props.Assign(o.Props);
  end
  else if Source is TStammdatenEntry then
  begin
    e := TStammdatenEntry(Source);
    SNR := e.SNR;
    FN := e.FN;
    LN := e.LN;
    SN := e.SN;
    NC := e.NC;
    GR := e.GR;
    PB := e.PB;
  end
  else
    inherited Assign(Source);
end;

procedure TStammdatenRowCollectionItem.SetFN(const Value: string);
begin
  FFN := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.SetGR(const Value: string);
begin
  FGR := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.SetLN(const Value: string);
begin
  FLN := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.SetNC(const Value: string);
begin
  FNC := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.SetPB(
  const Value: string);
begin
  FPB := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.SetSN(const Value: string);
begin
  FSN := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.SetSNR(const Value: Integer);
begin
  FSNR := Value;
  CalcDisplayName;
end;

procedure TStammdatenRowCollectionItem.CalcDisplayName;
begin
  FOwner.CalcDisplayName(self);
end;

function TStammdatenRowCollectionItem.GetFieldCount: Integer;
begin
  result := ru.FStammdatenRowCollection.FieldCount;
end;

function TStammdatenRowCollectionItem.GetFieldUsed(FieldIndex: Integer): Boolean;
var
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  i: Integer;
begin
  result := False;
  cl := Collection as TStammdatenRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if cr.FieldValue[FieldIndex] <> '' then
    begin
      result := True;
      break;
    end;
  end;
end;

function TStammdatenRowCollectionItem.GetStammdatenNode: TStammdatenNode;
var
  cl: TStammdatenRowCollection;
begin
  result := nil;
  cl := Collection as TStammdatenRowCollection;
  if cl.BaseNode is TStammdatenNode then
    result := TStammdatenNode(cl.BaseNode);
end;

function TStammdatenRowCollectionItem.GetFieldValue(
  FieldIndex: Integer): string;
begin
  result := '';
  case FieldIndex of
    1: result := FFN;
    2: result := FLN;
    3: result := FSN;
    4: result := FNC;
    5: result := FGR;
    6: result := FPB;
  else
    begin
      if (FieldIndex > 0) and (FieldIndex <= FieldCount) then
        result := Props.Value['N' + IntToStr(FieldIndex)];
    end;
  end
end;

procedure TStammdatenRowCollectionItem.SetFieldValue(FieldIndex: Integer;
  const Value: string);
begin
  case FieldIndex of
    1: FFN := Value;
    2: FLN := Value;
    3: FSN := Value;
    4: FNC := Value;
    5: FGR := Value;
    6: FPB := Value;
  else
    begin
      if (FieldIndex > 0) and (FieldIndex <= FieldCount) then
        Props.Value['N' + IntToStr(FieldIndex)] := Value;
    end;
  end;
  CalcDisplayName;
end;

{ TStammdatenRowCollection }

function TStammdatenRowCollection.GetItem(Index: Integer): TStammdatenRowCollectionItem;
begin
  Result := TStammdatenRowCollectionItem(inherited GetItem(Index));
end;

procedure TStammdatenRowCollection.SetItem(Index: Integer; const Value: TStammdatenRowCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TStammdatenRowCollection.Add: TStammdatenRowCollectionItem;
begin
  Result := TStammdatenRowCollectionItem(inherited Add);
  result.BaseID := result.Index + 1;
end;

procedure TStammdatenRowCollection.UpdateItem(e: TStammdatenEntry);
var
  o: TStammdatenRowCollectionItem;
begin
  o := FindKey(e.SNR);
  if not Assigned(o) then
    o := Add;
  if Assigned(o) then
    o.Assign(e);
end;

function TStammdatenRowCollection.FindKey(SNR: Integer): TStammdatenRowCollectionItem;
var
  i: Integer;
  o: TStammdatenRowCollectionItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.SNR = SNR) then
    begin
      result := o;
      break;
    end;
  end;
end;

procedure TStammdatenRowCollection.ShowTableInGrid(StringGrid: TStringGrid);
var
  i: Integer;
  r: Integer;
  o: TStammdatenRowCollectionItem;
const
  col_Index: Integer = 0;
  col_SNR: Integer = 1;
  col_FN: Integer = 2;
  col_LN: Integer = 3;
  col_SN: Integer = 4;
  col_NC: Integer = 5;
  col_GR: Integer = 6;
  col_PB: Integer = 7;
begin
  StringGrid.ColCount := 8;
  if Count > 1 then
    StringGrid.RowCount := Count + 1
  else
  begin
    StringGrid.RowCount := 2;
    StringGrid.Rows[1].Clear;
  end;
  { Header }
  StringGrid.Cells[col_Index, 0] := 'Index';
  StringGrid.Cells[col_SNR, 0] := 'SNR';
  StringGrid.Cells[col_FN, 0] := N_FN;
  StringGrid.Cells[col_LN, 0] := N_LN;
  StringGrid.Cells[col_SN, 0] := N_SN;
  StringGrid.Cells[col_NC, 0] := N_NC;
  StringGrid.Cells[col_GR, 0] := N_GR;
  StringGrid.Cells[col_PB, 0] := N_PB;
  { Rows }
  for i := 0 to Count - 1 do
  begin
    r := i + 1;
    o := Items[i];
    StringGrid.Cells[col_Index, r] := IntToStr(r);
    StringGrid.Cells[col_SNR, r] := IntToStr(o.SNR);
    StringGrid.Cells[col_FN, r] := o.FN;
    StringGrid.Cells[col_LN, r] := o.LN;
    StringGrid.Cells[col_SN, r] := o.SN;
    StringGrid.Cells[col_NC, r] := o.NC;
    StringGrid.Cells[col_GR, r] := o.GR;
    StringGrid.Cells[col_PB, r] := o.PB;
  end;
end;

procedure TStammdatenRowCollection.Swap(f1, f2: Integer);
var
  cr: TStammdatenRowCollectionItem;
  i: Integer;
  s: string;
begin
  if (f1 <> f2) and (f1 > 0) and (f2 > 0)
    and (f1 <= FieldCount) and (f2 <= FieldCount)
  then
  begin
    for i := 0 to Count - 1 do
    begin
      cr := Items[i];
      s := cr.FieldValue[f1];
      cr.FieldValue[f1] := cr.FieldValue[f2];
      cr.FieldValue[f2] := s;
    end;
  end;
end;

constructor TStammdatenRowCollection.Create(AOwner: TPersistent;
  ItemClass: TBaseRowCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  MapList := TStringList.Create;
  FieldCaptionList := TStringList.Create;
  FieldMap := 'SN';
  FieldCount := 6;
end;

destructor TStammdatenRowCollection.Destroy;
begin
  MapList.Free;
  FieldCaptionList.Free;
  inherited;
end;

procedure TStammdatenRowCollection.CalcDisplayName(
  cr: TStammdatenRowCollectionItem);
var
  s, t: string;
  i: Integer;
begin
  t := '';
  for i := 0 to MapList.Count - 1 do
  begin
    s := MapList[i];
    if (s = N_LN) or (s = 'LN') then
      t := t + cr.LN
    else if (s = N_FN) or (s = 'FN') then
      t := t + cr.FN
    else if (s = N_SN) or (s = 'SN') then
      t := t + cr.SN
    else if (s = N_PB) or (s = 'PB') then
      t := t + cr.PB
    else if (s = N_GR) or (s = 'GR') then
      t := t + cr.GR
    else if (s = N_NC) or (s = 'NC') then
      t := t + cr.NC
    else if (s = '_') or (s = 'Space') then
      t := t + ' '
    else if s = '-' then
      t := t + ' - '
    else if s = 'Slash' then
      t := t + ' / '
    else if s = 'Komma' then
      t := t + ', '
    else
      t := t + s;
  end;
  cr.DN := t;
end;

procedure TStammdatenRowCollection.SetFieldMap(const Value: string);
var
  i: Integer;
begin
  FFieldMap := Value;
  MapList.CommaText := Value;
  for i := 0 to Count - 1 do
    CalcDisplayName(Items[i]);
end;

procedure TStammdatenRowCollection.SetSchemaCode(Value: Integer);
begin
  case Value of
    0:
    begin
      FSchemaCode := 0;
      N_FN := N0_FN;
      N_LN := N0_LN;
      N_SN := N0_SN;
      N_NC := N0_NC;
      N_GR := N0_GR;
      N_PB := N0_PB;
    end;
    1:
    begin
      FSchemaCode := 1;
      N_FN := N1_FN;
      N_LN := N1_LN;
      N_SN := N1_SN;
      N_NC := N1_NC;
      N_GR := N1_GR;
      N_PB := N1_PB;
    end;
    2:
    begin
      FSchemaCode := 2;
      N_FN := N2_FN;
      N_LN := N2_LN;
      N_SN := N2_SN;
      N_NC := N2_NC;
      N_GR := N2_GR;
      N_PB := N2_PB;
    end;
  end;
end;

procedure TStammdatenRowCollection.SetFieldCount(const Value: Integer);
begin
  FFieldCount := Value;
end;

function TStammdatenRowCollection.GetStammdatenNode: TStammdatenNode;
begin
  if (BaseNode is TStammdatenNode) then
    result := TStammdatenNode(BaseNode)
  else
    result := nil;
end;

function TStammdatenRowCollection.GetFieldCaption(Index: Integer): string;
begin
  result := '';
  if (Index > 0) and (Index <= FieldCaptionList.Count) then
    result := FieldCaptionList[Index-1];

  if (result = '') then
  begin
    case Index of
      0: result := 'SNR';
      1: result := N_FN;
      2: result := N_LN;
      3: result := N_SN;
      4: result := N_NC;
      5: result := N_GR;
      6: result := N_PB;
    end;
  end;

  if result = '' then
    result := 'N' + IntToStr(Index);
end;

function TStammdatenRowCollection.GetStandardFieldCaption(Index: Integer; NameSchema: Integer): string;
begin
  result := '';

  if Index = 0 then
    result := 'SNR';

  case NameSchema of
    0:
    begin
      case Index of
        1: result := N0_FN;
        2: result := N0_LN;
        3: result := N0_SN;
        4: result := N0_NC;
        5: result := N0_GR;
        6: result := N0_PB;
      end;
    end;

    1:
    begin
      case Index of
        1: result := N1_FN;
        2: result := N1_LN;
        3: result := N1_SN;
        4: result := N1_NC;
        5: result := N1_GR;
        6: result := N1_PB;
      end;
    end;

    2:
    begin
      case Index of
        1: result := N2_FN;
        2: result := N2_LN;
        3: result := N2_SN;
        4: result := N2_NC;
        5: result := N2_GR;
        6: result := N2_PB;
      end;
    end;
  end;

  if result = '' then
    result := 'N' + IntToStr(Index);
end;

procedure TStammdatenRowCollection.SetFieldCaptions(const Value: string);
begin
  FieldCaptionList.CommaText := Value;
  //Field 0 is not editable, should always be 'SNR'
//  if FieldCaptionList.Count > 0 then
//  begin
//    if FieldCaptionList[0] <> 'SNR' then
//    begin
//      if FieldCaptionList.Count >= self.FieldCount then
//        FieldCaptionList[0] := 'SNR'
//      else
//        FieldCaptionList.Insert(0, 'SNR');
//    end;
//  end;
end;

function TStammdatenRowCollection.GetFieldCaptions: string;
begin
  result := FieldCaptionList.CommaText;
end;

{ TStammdatenColProp }

function TStammdatenColProp.GetFieldCaptionDef(
  cl: TStammdatenRowCollection; Index: Integer; def: string): string;
begin
  result := def;
  if (cl <> nil) then
    result := cl.GetFieldCaption(Index);
end;

function TStammdatenColProp.GetFieldCaptionDef(Index: Integer; def: string): string;
var
  n: TStammdatenNode;
begin
  result := def;
  n := GetStammdatenNode;
  if (n <> nil) then
    result := n.StammdatenRowCollection.GetFieldCaption(Index);
end;

function TStammdatenColProp.GetFieldCount: Integer;
begin
  result := FixFieldCount;
  if BO <> nil then
  begin
    result := BO.StammdatenNode.StammdatenRowCollection.FieldCount;
    if result < FixFieldCount then
      result := FixFieldCount;
  end;
end;

function TStammdatenColProp.GetStammdatenNode: TStammdatenNode;
begin
  result := nil;
  if BO <> nil then
  begin
    result := BO.StammdatenNode;
  end;
end;

function TStammdatenColProp.GetStammdatenRowCollection: TStammdatenRowCollection;
var
  n: TStammdatenNode;
begin
  result := nil;
  n := GetStammdatenNode;
  if (n <> nil) then
    result := n.StammdatenRowCollection;
end;

procedure TStammdatenColProp.GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
begin
  Value := '';
  if crgs is TStammdatenRowCollectionItem then
    cr := TStammdatenRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NumID = NumID_SNR then
    Value := IntToStr(cr.SNR)

  else if NumID = NumID_FN then
    Value := cr.FN

  else if NumID = NumID_LN then
    Value := cr.LN

  else if NumID = NumID_SN then
    Value := cr.SN

  else if NumID = NumID_NC then
    Value := cr.NC

  else if NumID = NumID_GR then
    Value := cr.GR

  else if NumID = NumID_PB then
    Value := cr.PB

  else if (NumID > FixFieldCount) then
  begin
    Value := cr.FieldValue[NumID];
  end;
end;

procedure TStammdatenColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
  cl: TStammdatenRowCollection;
  fieldCount: Integer;
  i: Integer;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  cl := self.GetStammdatenRowCollection;

  { SNR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SNR';
  cp.Caption := 'SNR';
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.NumID := NumID_SNR;

  { FN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FN';
  cp.Caption := self.GetFieldCaptionDef(cl, 1, N_FN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_FN;

  { LN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_LN';
  cp.Caption := self.GetFieldCaptionDef(cl, 2, N_LN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_LN;

  { SN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SN';
  cp.Caption := self.GetFieldCaptionDef(cl, 3, N_SN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_SN;

  { NC }
  cp := ColsAvail.Add;
  cp.NameID := 'col_NC';
  cp.Caption := self.GetFieldCaptionDef(cl, 4, N_NC);
  cp.Width := 60;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.NumID := NumID_NC;

  { GR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GR';
  cp.Caption := self.GetFieldCaptionDef(cl, 5, N_GR);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.NumID := NumID_GR;

  { PB }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PB';
  cp.Caption := self.GetFieldCaptionDef(cl, 6, N_PB);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_PB;

  fieldCount := cl.FieldCount;
  if fieldCount > FixFieldCount then
  begin
    for i := FixFieldCount + 1 to fieldCount do
    begin
      cp := ColsAvail.Add;
      cp.NameID := 'col_N' + IntToStr(i);
      cp.Caption := self.GetFieldCaptionDef(cl, i, 'N' + IntToStr(i));
      cp.Width := 60;
      cp.Sortable := True;
      cp.Alignment := taLeftJustify;
      cp.ColType := colTypeString;
      cp.NumID := i;
    end;
  end;
end;

{ TStammdatenNode }

constructor TStammdatenNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  FStammdatenRowCollection := TStammdatenRowCollection.Create(self, TStammdatenRowCollectionItem);
end;

destructor TStammdatenNode.Destroy;
begin
  FStammdatenRowCollection.Free;
  inherited;
end;

function TStammdatenNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TStammdatenColProp;
end;

function TStammdatenNode.GetBaseRowCollection: TBaseRowCollection;
begin
  result := FStammdatenRowCollection;
end;

function TStammdatenNode.GetSchemaCode: Integer;
begin
  result := self.FStammdatenRowCollection.FSchemaCode;
end;

function TStammdatenNode.GetStammdatenBO: TStammdatenBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TStammdatenBO;
end;

procedure TStammdatenNode.Init(RowCount: Integer);
var
  o: TStammdatenRowCollectionItem;
  i: Integer;
begin
  FStammdatenRowCollection.Clear;
  for i := 0 to RowCount - 1 do
  begin
    o := FStammdatenRowCollection.Add();
    o.BaseID := 1 + i;
    o.SNR := 1001 + i;
  end;
end;

procedure TStammdatenNode.Load;
begin
  Self.FStammdatenRowCollection.Clear;
  {
  o := Self.FStammdatenRowCollection.Add;
  o.SNR := 1000;
  o.FN := 'FN1';
  o.LN := 'LN1';
  o.SN := 'SN1';
  o.NC := 'GER';
  o.GR := 'M';
  o.PB := '';
  o.BaseID := 1;
  }
end;

function TStammdatenNode.GetNameSchema: string;
begin
  if SchemaCode = 2 then
    result := 'NX'
  else if SchemaCode = 1 then
    result := 'LongNames'
  else
    result := '';
end;

procedure TStammdatenNode.SetNameSchema(const Value: string);
begin
  if Value = 'NX' then
    StammdatenRowCollection.SchemaCode := 2
  else if Value = 'LongNames' then
    StammdatenRowCollection.SchemaCode := 1
  else
    StammdatenRowCollection.SchemaCode := 0;
end;

{ TStammdatenBO }

function TStammdatenBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

function TStammdatenBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

procedure TStammdatenBO.SetCurrentRow(const Value: TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else if Value is TBaseRowCollectionItem then
    FCurrentRow := Value as TStammdatenRowCollectionItem;
end;

procedure TStammdatenBO.SetCurrentNode(const Value: TBaseNode);
begin
  FCurrentNode := Value as TStammdatenNode;
end;

procedure TStammdatenBO.InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer);
var
  cp: TBaseColProp;
  fc: Integer;
  i: Integer;
begin
  with StringGrid do
  begin
    cp := AddColumn('col_SNR');
    cp.OnFinishEdit := EditSNR;
    cp.ReadOnly := False;

    fc := FieldCount;

    if (fc > 0) then
    begin
    cp := AddColumn('col_FN');
    cp.OnFinishEdit := EditFN;
    cp.ReadOnly := False;
    end;

    if (fc > 1) then
    begin
    cp := AddColumn('col_LN');
    cp.OnFinishEdit := EditLN;
    cp.ReadOnly := False;
    end;

    if (fc > 2) then
    begin
    cp := AddColumn('col_SN');
    cp.OnFinishEdit := EditSN;
    cp.ReadOnly := False;
    end;

    if (fc > 3) then
    begin
    cp := AddColumn('col_NC');
    cp.OnFinishEdit := EditNC;
    cp.ReadOnly := False;
    end;

    if (fc > 4) then
    begin
    cp := AddColumn('col_GR');
    cp.OnFinishEdit := EditGR;
    cp.ReadOnly := False;
    end;

    if (fc > 5) then
    begin
    cp := AddColumn('col_PB');
    cp.OnFinishEdit := EditPB;
    cp.ReadOnly := False;
  end;

    if (fc > FixFieldCount) then
    begin
      for i := FixFieldCount + 1 to fc do
      begin
        cp := AddColumn('col_N' + IntToStr(i));
        cp.OnFinishEdit2 := EditNameColumn;
        cp.ReadOnly := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditSNR(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    cr.SNR := StrToIntDef(Value, cr.SNR);
    Value := IntToStr(cr.SNR);
  end;
end;

constructor TStammdatenBO.Create;
begin
  UndoAgent := BO.UndoAgent;
end;

procedure TStammdatenBO.EditFN(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
  oldValue: string;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    oldValue := cr.FN;
    cr.FN := Value;
    if (oldValue <> Value) then
    begin
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].FN(oldValue);
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].FN(Value);
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditLN(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
  oldValue: string;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    oldValue := cr.LN;
    cr.LN := Value;
    if (oldValue <> Value) then
    begin
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].LN(oldValue);
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].LN(Value);
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditSN(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
  oldValue: string;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    oldValue := cr.SN;
    cr.SN := Value;
    if (oldValue <> Value) then
    begin
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].SN(oldValue);
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].SN(Value);
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditNC(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
  oldValue: string;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    oldValue := cr.NC;
    cr.NC := Value;
    if (oldValue <> Value) then
    begin
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].NC(oldValue);
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].NC(Value);
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditGR(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
  oldValue: string;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    oldValue := cr.GR;
    cr.GR := Value;
    if (oldValue <> Value) then
    begin
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].GR(oldValue);
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].GR(Value);
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditPB(crgs: TBaseRowCollectionItem; var Value: string);
var
  cr: TStammdatenRowCollectionItem;
  oldValue: string;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    oldValue := cr.PB;
    cr.PB := Value;
    if (oldValue <> Value) then
    begin
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].PB(oldValue);
        UndoAgent.MsgTree.Division.Athlete[cr.SNR].PB(Value);
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TStammdatenBO.EditNameColumn(
  crgs: TBaseRowCollectionItem; var Value: string; const ColName: string);
var
  cr: TStammdatenRowCollectionItem;
  i: Integer;
begin
  if (crgs <> nil) and (crgs is TStammdatenRowCollectionItem) then
  begin
    cr := TStammdatenRowCollectionItem(crgs);
    i := StrToIntDef(Copy(ColName, 6, Length(ColName)), -1);
    if (i > -1) then
      cr.FieldValue[i] := Value;
  end;
end;

function TStammdatenBO.GetFieldCount: Integer;
begin
  result := BO.StammdatenNode.StammdatenRowCollection.FieldCount;
end;

{ TStammdatenRemoteObject }

constructor TStammdatenRemoteObject.Create;
begin
  inherited Create;
end;

procedure TStammdatenRemoteObject.Assign(Source: TPersistent);
var
  o: TStammdatenRowCollectionItem;
  e: TStammdatenRemoteObject;
begin
  if Source is TStammdatenRowCollectionItem then
  begin
    o := TStammdatenRowCollectionItem(Source);
    SNR := o.SNR;
    FN := o.FN;
    LN := o.LN;
    SN := o.SN;
    NC := o.NC;
    GR := o.GR;
    PB := o.PB;
  end
  else if Source is TStammdatenRemoteObject then
  begin
    e := TStammdatenRemoteObject(Source);
    SNR := e.SNR;
    FN := e.FN;
    LN := e.LN;
    SN := e.SN;
    NC := e.NC;
    GR := e.GR;
    PB := e.PB;
  end
  else
    inherited Assign(Source);
end;

function TStammdatenRemoteObject.GetCommaText(SL: TStrings): string;
begin
  if not Assigned(SL) then
  begin
    result := '';
    exit;
  end;

  SL.Clear;

  SL.Add(IntToStr(SNR));
  SL.Add(FN);
  SL.Add(LN);
  SL.Add(SN);
  SL.Add(NC);
  SL.Add(GR);
  SL.Add(PB);

  result := SL.CommaText;
end;

function TStammdatenRemoteObject.GetCSV_Header: string;
begin
  result :=
    'SNR' + sep +
    N2_FN + sep +
    N2_LN + sep +
    N2_SN + sep +
    N2_NC + sep +
    N2_GR + sep +
    N2_PB;
end;

procedure TStammdatenRemoteObject.SetCommaText(SL: TStrings);
var
  i, c: Integer;

  function NextI: Integer;
  begin
    if i < c then
      result := StrToIntDef(SL[i], -1)
    else
      result := -1;
    Inc(i);
  end;

  function NextS: string;
  begin
    if i < c then
      result := SL[i]
    else
      result := '';
    Inc(i);
  end;

begin
  if not Assigned(SL) then exit;
  i := 0;
  c := SL.Count;

  SNR := NextI;
  FN := NextS;
  LN := NextS;
  SN := NextS;
  NC := NextS;
  GR := NextS;
  PB := NextS;
end;

{
procedure TStammdatenRemoteObject.GetFieldDefs(FD: TFieldDefs);
begin
  //fixed Field Names in DB, not variable
  FD.Add('SNR', ftInteger);
  FD.Add('FN', ftString, 26);
  FD.Add('LN', ftString, 26);
  FD.Add('SN', ftString, 33);
  FD.Add('NC', ftString, 3);
  FD.Add('GR', ftString, 1);
  FD.Add('PB', ftString, 11);
end;

procedure TStammdatenRemoteObject.UpdateDataSet(DS: TDataSet);
begin
  if DS.Locate('SNR', SNR, [loCaseInsensitive]) then
    DS.Edit
  else
  begin
    DS.Append;
    DS.FieldByName('ID').AsInteger := NewID;
  end;

  try
    //fixed Field Names in DB, not variable
    DS.FieldByName('SNR').AsInteger := SNR;
    DS.FieldByName('FN').AsString := FN;
    DS.FieldByName('LN').AsString := LN;
    DS.FieldByName('SN').AsString := SN;
    DS.FieldByName('NC').AsString := NC;
    DS.FieldByName('GR').AsString := GR;
    DS.FieldByName('PB').AsString := PB;

    DS.Post;
  except
    DS.Cancel;
  end;
end;
}

procedure TStammdatenRemoteObject.GetOutput;
begin
  SLADD('SNR', IntToStr(SNR));
  SLADD(N2_FN, FN);
  SLADD(N2_LN, LN);
  SLADD(N2_SN, SN);
  SLADD(N2_NC, NC);
  SLADD(N2_GR, GR);
  SLADD(N2_PB, PB);
end;

end.
