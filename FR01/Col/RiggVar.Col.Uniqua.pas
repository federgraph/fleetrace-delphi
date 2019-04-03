unit RiggVar.Col.Uniqua;

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

type
  TUniquaEntry = class(TPersistent)
  public
    Plz: Integer;
    SegNr: Integer;
    Steuermann: string;
    ClubS: string;
    JgS: Integer;
    Vorschoter: string;
    ClubV: string;
    JgV: Integer;
    Wf: array of string;
    Ges: string;
    Rangl: string;
    constructor Create(aRaceCount: Integer);
  end;

  TUniquaNode = class;

  TUniquaRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FPlz: Integer;
    FSegNr: Integer;
    FSteuermann: string;
    FClubS: string;
    FJgS: Integer;
    FVorschoter: string;
    FClubV: string;
    FJgV: Integer;
    FWf: array of string;
    FGes: string;
    FRangl: string;
    procedure SetModified(const Value: Boolean);
    function GetUniquaNode: TUniquaNode;
    function GetWf(Index: Integer): string;
    procedure SetWf(Index: Integer; const Value: string);
    function GetWfCount: Integer;
  public
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TUniquaNode read GetUniquaNode;
    property Wf[Index: Integer]: string read GetWf write SetWf;
    property WfCount: Integer read GetWfCount;
  published
    property Plz: Integer read FPlz write FPlz;
    property SegNr: Integer read FSegNr write FSegNr;
    property Steuermann: string read FSteuermann write FSteuermann;
    property ClubS: string read FClubS write FClubS;
    property JgS: Integer read FJgS write FJgS;
    property Vorschoter: string read FVorschoter write FVorschoter;
    property ClubV: string read FClubV write FClubV;
    property JgV: Integer read FJgV write FJgV;
    property Ges: string read FGes write FGes;
    property Rangl: string read FRangl write FRangl;
  end;

  TUniquaRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TUniquaRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TUniquaRowCollectionItem);
  public
    procedure Load;
    procedure Save;
    procedure Dump(Memo: TStrings);
    function Add: TUniquaRowCollectionItem;
    procedure UpdateItem(e: TUniquaEntry);
    function FindKey(SegNr: Integer): TUniquaRowCollectionItem;
    property Items[Index: Integer]: TUniquaRowCollectionItem read GetItem
      write SetItem;
  end;

  TUniquaColProp = class(TBaseColProp)
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
    procedure GetSortKeyRace(crgs: TBaseRowCollectionItem;
      var Value: string; const ColName: string);
  end;

  TUniquaBO = class;

  TUniquaNode = class(TBaseNode)
  private
    function GetUniquaRowCollection: TUniquaRowCollection;
    function GetUniquaBO: TUniquaBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    property UniquaRowCollection: TUniquaRowCollection read
      GetUniquaRowCollection;
    property UniquaBO: TUniquaBO read
      GetUniquaBO;
  end;

  TUniquaBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    procedure InitColsActive(StringGrid: TColGrid); override;
    procedure EditPlz(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditSegNr(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditSteuermann(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditClubS(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditJgS(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditVorschoter(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditClubV(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditJgV(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditWf(crgs: TBaseRowCollectionItem; var Value: string; const ColName: string);
    procedure EditGes(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditRangl(crgs: TBaseRowCollectionItem; var Value: string);
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
  end;

implementation

uses
  RiggVar.BO.Def;

{
const
  BoolStr: array[Boolean] of string = ('False', 'True');

function IsTrue(Value: string): Boolean;
var
  s: string;
begin
  result := False;
  s := UpperCase(Value);
  if (s = 'TRUE') or (s = 'T') then
    result := True
end;
}

{ TUniquaRowCollectionItem }

constructor TUniquaRowCollectionItem.Create(Collection: TBaseRowCollection);
begin
  inherited;
  SetLength(FWf, BO.BOParams.RaceCount + 1);
end;

destructor TUniquaRowCollectionItem.Destroy;
begin
  inherited;
end;

procedure TUniquaRowCollectionItem.Assign(Source: TPersistent);
var
  o: TUniquaRowCollectionItem;
  e: TUniquaEntry;
  i: Integer;
begin
  if Source is TUniquaRowCollectionItem then
  begin
    o := TUniquaRowCollectionItem(Source);
    Plz := o.Plz;
    SegNr := o.SegNr;
    Steuermann := o.Steuermann;
    ClubS := o.ClubS;
    JgS := o.JgS;
    Vorschoter := o.Vorschoter;
    ClubV := o.ClubV;
    JgV := o.JgV;
    for i := 0 to Length(FWf)-1 do
      FWf[i] := o.FWf[i];
    Ges := o.Ges;
    Rangl := o.Rangl;
  end
  else if Source is TUniquaEntry then
  begin
    e := TUniquaEntry(Source);
    Plz := e.Plz;
    SegNr := e.SegNr;
    Steuermann := e.Steuermann;
    ClubS := e.ClubS;
    JgS := e.JgS;
    Vorschoter := e.Vorschoter;
    ClubV := e.ClubV;
    JgV := e.JgV;
    for i := 0 to Length(FWf)-1 do
      FWf[i] := e.Wf[i];
    Ges := e.Ges;
    Rangl := e.Rangl;
  end
  else
    inherited Assign(Source);
end;

procedure TUniquaRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

function TUniquaRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TUniquaRowCollection;
begin
  cl := Collection as TUniquaRowCollection;
  result := cl.BaseNode;
end;

function TUniquaRowCollectionItem.GetUniquaNode: TUniquaNode;
var
  cl: TUniquaRowCollection;
begin
  result := nil;
  cl := Collection as TUniquaRowCollection;
  if cl.BaseNode is TUniquaNode then
    result := TUniquaNode(cl.BaseNode);
end;

function TUniquaRowCollectionItem.GetWf(
  Index: Integer): string;
begin
  if (Index >= 0) and (Index < Length(FWf)) then
    result := FWf[Index]
  else
    result := '';
end;

procedure TUniquaRowCollectionItem.SetWf(Index: Integer;
  const Value: string);
begin
  if (Index >= 0) and (Index < Length(FWf)) then
    FWf[Index] := Value;
end;

function TUniquaRowCollectionItem.GetWfCount: Integer;
begin
  result := Length(FWf);
end;

{ TUniquaRowCollection }

function TUniquaRowCollection.GetItem(Index: Integer):
  TUniquaRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TUniquaRowCollectionItem(inherited GetItem(Index));
end;

procedure TUniquaRowCollection.SetItem(Index: Integer; const Value:
  TUniquaRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

function TUniquaRowCollection.Add: TUniquaRowCollectionItem;
begin
  Result := TUniquaRowCollectionItem(inherited Add);
end;

procedure TUniquaRowCollection.UpdateItem(e: TUniquaEntry);
var
  o: TUniquaRowCollectionItem;
begin
  o := FindKey(e.SegNr);
  if not Assigned(o) then
    o := Add;
  if Assigned(o) then
    o.Assign(e);
end;

function TUniquaRowCollection.FindKey(SegNr: Integer):
  TUniquaRowCollectionItem;
var
  i: Integer;
  o: TUniquaRowCollectionItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.SegNr = SegNr) then
    begin
      result := o;
      break;
    end;
  end;
end;

procedure TUniquaRowCollection.Load;
begin
  //
end;

procedure TUniquaRowCollection.Save;
begin
  //
end;

procedure TUniquaRowCollection.Dump(Memo: TStrings);
begin
  //
end;

{ TUniquaColProp }

procedure TUniquaColProp.GetSortKeyRace(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TUniquaRowCollectionItem;
  i: Integer;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    i := StrToIntDef(Copy(ColName, 7, Length(ColName)), -1);
    if i > 0 then
    begin
      if Pos(' (0)', cr.FWf[i]) > 0 then
        Value := IntToStr(999 + cr.BaseID) + ' (0)'
      else
         Value := cr.FWf[i];
    end;
  end;
end;

procedure TUniquaColProp.GetTextDefault(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
  i: Integer;
begin
  Value := '';
  if crgs is TUniquaRowCollectionItem then
    cr := TUniquaRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NameID = 'col_Plz' then
    Value := IntToStr(cr.Plz)

  else if NameID = 'col_SegNr' then
    Value := IntToStr(cr.SegNr)

  else if NameID = 'col_Steuermann' then
    Value := cr.Steuermann

  else if NameID = 'col_ClubS' then
    Value := cr.ClubS

  else if NameID = 'col_JgS' then
    Value := IntToStr(cr.JgS)

  else if NameID = 'col_Vorschoter' then
    Value := cr.Vorschoter

  else if NameID = 'col_ClubV' then
    Value := cr.ClubV

  else if NameID = 'col_JgV' then
    Value := IntToStr(cr.JgV)

  { Wf[0] wird nicht angezeigt }
  else if Copy(NameID, 1, 6) = 'col_Wf' then
  begin
    i := StrToIntDef(Copy(NameID, 7, Length(NameID)), -1);
    Value := cr.Wf[i];
  end

  else if NameID = 'col_Ges' then
    Value := cr.Ges

  else if NameID = 'col_Rangl' then
    Value := cr.Rangl;

end;

procedure TUniquaColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
  i: Integer;
  tempWfCount: Integer;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  tempWfCount := -1;
  if Assigned(BO) then
    tempWfCount := BO.BOParams.RaceCount;

  { Plz }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Plz';
  cp.Caption := 'Plz';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { SegNr }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SegNr';
  cp.Caption := 'SegNr';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Steuermann }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Steuermann';
  cp.Caption := 'Steuermann';
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;

  { Club }
  cp := ColsAvail.Add;
  cp.NameID := 'col_ClubS';
  cp.Caption := 'Club';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;

  { Jg }
  cp := ColsAvail.Add;
  cp.NameID := 'col_JgS';
  cp.Caption := 'Jg';
  cp.Width := 20;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Vorschoter }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Vorschoter';
  cp.Caption := 'Vorschoter';
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;

  { Club }
  cp := ColsAvail.Add;
  cp.NameID := 'col_ClubV';
  cp.Caption := 'Club';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;

  { Jg }
  cp := ColsAvail.Add;
  cp.NameID := 'col_JgV';
  cp.Caption := 'Jg';
  cp.Width := 20;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;

  { Wf[0] wird nicht angezeigt, nur Wf[1]..Wf[WfCount-1] }
  for i := 1 to tempWfCount do
  begin
    { Wfi }
    cp := ColsAvail.Add;
    cp.NameID := 'col_Wf' + IntToStr(i);
    cp.Caption := 'Wf' + IntToStr(i);
    cp.Width := 30;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.ColType := colTypeRank;
    cp.OnGetSortKey2 := GetSortKeyRace;
  end;

  { Ges }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Ges';
  cp.Caption := 'Ges';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;

  { Rangl }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Rangl';
  cp.Caption := 'Rangl';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeString;
end;

{ TUniquaNode }

constructor TUniquaNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  BaseRowCollection := TUniquaRowCollection.Create(Self,
    TUniquaRowCollectionItem);
end;

destructor TUniquaNode.Destroy;
begin
  BaseRowCollection.Free;
  inherited;
end;

function TUniquaNode.GetUniquaRowCollection: TUniquaRowCollection;
begin
  result := BaseRowCollection as TUniquaRowCollection;
end;

function TUniquaNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TUniquaColProp;
end;

procedure TUniquaNode.Save;
begin
end;

procedure TUniquaNode.Load;
var
  o: TUniquaRowCollectionItem;
begin
  UniquaRowCollection.Clear;

  o := UniquaRowCollection.Add;
  o.Steuermann := 'Steuermann';
  o.ClubS := 'ClubS';
  o.Vorschoter := 'Vorschoter';
  o.ClubV := 'ClubV';
  //o.Wf := Value;
  o.Rangl := 'Rangl';
  o.BaseID := 1;

  o := UniquaRowCollection.Add;
  o.Steuermann := 'Steuermann';
  o.ClubS := 'ClubS';
  o.Vorschoter := 'Vorschoter';
  o.ClubV := 'ClubV';
  //o.Wf := Value;
  o.Rangl := 'Rangl';
  o.BaseID := 2;

  o := UniquaRowCollection.Add;
  o.Steuermann := 'Steuermann';
  o.ClubS := 'ClubS';
  o.Vorschoter := 'Vorschoter';
  o.ClubV := 'ClubV';
  //o.Wf := Value;
  o.Rangl := 'Rangl';
  o.BaseID := 3;
end;

procedure TUniquaNode.Init(RowCount: Integer);
var
  o: TUniquaRowCollectionItem;
  i: Integer;
begin
  BaseRowCollection.Clear;

  for i := 0 to RowCount - 1 do
  begin
    o := UniquaRowCollection.Add;
    o.BaseID := i + 1;
    o.Steuermann := 'Steuermann';
    o.ClubS := 'ClubS';
    o.Vorschoter := 'Vorschoter';
    o.ClubV := 'ClubV';
    //o.Wf := Value;
    o.Rangl := 'Rangl';
  end;
end;

procedure TUniquaNode.Calc;
{
var
  cl: TUniquaRowCollection;
  cr: TUniquaRowCollectionItem;
  i: Integer;
}
begin
  {
    cl := Self.GetUniquaRowCollection;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      //cr.QRank := cr.QTime.AsInteger;
    end;
    Modified := False;
  }
end;

function TUniquaNode.GetUniquaBO: TUniquaBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TUniquaBO;
end;

{ TUniquaBO }

procedure TUniquaBO.InitColsActive(StringGrid: TColGrid);
var
  s: string;
  i: Integer;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    AddColumn('col_Plz');
    AddColumn('col_SegNr');
    AddColumn('col_Steuermann');
    AddColumn('col_ClubS');
    AddColumn('col_JgS');
    AddColumn('col_Vorschoter');
    AddColumn('col_ClubV');
    AddColumn('col_JgV');

    for i := 1 to BO.BOParams.RaceCount do
    begin
      s := 'col_Wf' + IntToStr(i);
      AddColumn(s);
    end;

    AddColumn('col_Ges');
    AddColumn('col_Rangl');
  end;
end;

{
procedure TUniquaBO.InitColsActive2(StringGrid: TBaseStringGrid);
var
  cp: TBaseColProp;
  s: string;
  i: Integer;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    cp := AddColumn('col_Plz');
    cp.OnFinishEdit := EditPlz;
    cp.ReadOnly := False;
    cp := AddColumn('col_SegNr');
    cp.OnFinishEdit := EditSegNr;
    cp.ReadOnly := False;
    cp := AddColumn('col_Steuermann');
    cp.OnFinishEdit := EditSteuermann;
    cp.ReadOnly := False;
    cp := AddColumn('col_ClubS');
    cp.OnFinishEdit := EditClubS;
    cp.ReadOnly := False;
    cp := AddColumn('col_JgS');
    cp.OnFinishEdit := EditJgS;
    cp.ReadOnly := False;
    cp := AddColumn('col_Vorschoter');
    cp.OnFinishEdit := EditVorschoter;
    cp.ReadOnly := False;
    cp := AddColumn('col_ClubV');
    cp.OnFinishEdit := EditClubV;
    cp.ReadOnly := False;
    cp := AddColumn('col_JgV');
    cp.OnFinishEdit := EditJgV;
    cp.ReadOnly := False;

    for i := 1 to BO.BOParams.RaceCount do
    begin
      s := 'col_Wf' + IntToStr(i);
      cp := AddColumn(s);
      cp.OnFinishEdit2 := EditWf;
      cp.ReadOnly := False;
    end;

    cp := AddColumn('col_Ges');
    cp.OnFinishEdit := EditGes;
    cp.ReadOnly := False;
    cp := AddColumn('col_Rangl');
    cp.OnFinishEdit := EditRangl;
    cp.ReadOnly := False;
  end;
end;
}

function TUniquaBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

function TUniquaBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

procedure TUniquaBO.SetCurrentRow(const Value:
  TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TUniquaBO.SetCurrentNode(const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TUniquaBO.EditPlz(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.Plz := StrToIntDef(Value, cr.Plz);
    Value := IntToStr(cr.Plz);
  end;
end;

procedure TUniquaBO.EditSegNr(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.SegNr := StrToIntDef(Value, cr.SegNr);
    Value := IntToStr(cr.SegNr);
  end;
end;

procedure TUniquaBO.EditSteuermann(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.Steuermann := Value;
  end;
end;

procedure TUniquaBO.EditClubS(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.ClubS := Value;
  end;
end;

procedure TUniquaBO.EditJgS(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.JgS := StrToIntDef(Value, cr.JgS);
    Value := IntToStr(cr.JgS);
  end;
end;

procedure TUniquaBO.EditVorschoter(crgs: TBaseRowCollectionItem;
  var Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.Vorschoter := Value;
  end;
end;

procedure TUniquaBO.EditClubV(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.ClubV := Value;
  end;
end;

procedure TUniquaBO.EditJgV(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.JgV := StrToIntDef(Value, cr.JgV);
    Value := IntToStr(cr.JgV);
  end;
end;

procedure TUniquaBO.EditWf(crgs: TBaseRowCollectionItem; var
  Value: string; const ColName: string);
var
  cr: TUniquaRowCollectionItem;
  i: Integer;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    i := StrToIntDef(Copy(ColName, 7, Length(ColName)), -1);
    cr.Wf[i] := Value;
    //cr.Modified := True;
  end;
end;

procedure TUniquaBO.EditGes(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.Ges := Value;
    Value := cr.Ges;
  end;
end;

procedure TUniquaBO.EditRangl(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TUniquaRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TUniquaRowCollectionItem) then
  begin
    cr := TUniquaRowCollectionItem(crgs);
    cr.Rangl := Value;
  end;
end;

{ TUniquaEntry }

constructor TUniquaEntry.Create(aRaceCount: Integer);
begin
  inherited Create;
  SetLength(Wf, aRaceCount + 1);
end;

end.

