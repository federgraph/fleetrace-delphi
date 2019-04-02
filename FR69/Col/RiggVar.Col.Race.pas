unit RiggVar.Col.Race;

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
  RiggVar.Col.BaseEntry,
  RiggVar.BO.Time,
  RiggVar.BO.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.BO.MsgToken,
  RiggVar.Col.Stammdaten,
  RiggVar.BO.Params;

const
  channel_FT = 0;

type
  TRaceData = record
    IsRacing: Boolean;
    Course: string;
    TimeTable: string;
  end;

  TTimePointEntry = class(TPersistent)
  public
    OTime: string;
    ORank: Integer;
    Rank: Integer;
    PosR: Integer;
    Behind: string;
    BFT: string;
    BPL: string;
    PLZ: Integer;
    procedure Assign(Source: TPersistent); override;
  end;

  TTimePoint = class(TPersistent)
  private
    FOTime: TPTime;
    FBehind: TPTime;
    FBFT: TQTime;
    FBPL: TQTime;
    procedure SetBehind(const Value: TPTime);
    procedure SetBPL(const Value: TQTime);
    procedure SetOTime(const Value: TPTime);
    procedure SetBFT(const Value: TQTime);
  public
    ORank: Integer;
    Rank: Integer;
    PosR: Integer;
    PLZ: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property OTime: TPTime read FOTime write SetOTime;
    property Behind: TPTime read FBehind write SetBehind;
    property BFT: TQTime read FBFT write SetBFT;
    property BPL: TQTime read FBPL write SetBPL;
  end;

  TRaceEntry = class(TBaseEntry)
  private
    function GetITCount: Integer;
  public
    Bib: Integer;
    //
    SNR: Integer;
    FN: string;
    LN: string;
    SN: string;
    NC: string;
    GR: string;
    PB: string;
    //
    QU: string;
    DG: Integer;
    ST: string;
    IT: array of TTimePointEntry;
    FT: TTimePointEntry;
    //
    MRank: Integer;
    //
    constructor Create(aITCount: Integer);
    destructor Destroy; override;
    function GetRunID: string; virtual;
    property ITCount: Integer read GetITCount;
  end;

  TRaceRemoteObject = class(TRaceEntry)
  private
    FRunID: string;
    procedure SetRunID(const Value: string);
  protected
    procedure GetOutput; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetRunID: string; override;
    function GetCommaText(SL: TStrings): string; override;
    procedure SetCommaText(SL: TStrings); override;
    property RunID: string read GetRunID write SetRunID;
    function GetCSV_Header: string; override;
    //
    //procedure GetFieldDefs(FD: TFieldDefs);
    //procedure UpdateDataSet(DS: TDataSet);
  end;

  TRaceNode = class;

  TRaceRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FBib: Integer;
    FSNR: Integer;
    FQU: TPenalty;
    FDG: Integer;
    FIT: array of TTimePoint;
    FFT: TTimePoint;
    FMRank: Integer;
    function GetFN: string;
    function GetLN: string;
    function GetSN: string;
    function GetNOC: string;
    function GetGender: string;
    function GetPB: string;
    procedure SetQU(const Value: TPenalty);
    procedure SetModified(const Value: Boolean);
    function GetRaceNode: TRaceNode;
    function GetSDItem: TStammdatenRowCollectionItem;
    function GetIT(Index: Integer): TTimePoint;
    function GetST: TPTime;
    procedure SetST(const Value: TPTime);
    function GetITCount: Integer;
    function GetDN: string;
  public
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBaseNode: TBaseNode;
//    procedure ClearList; override;
    procedure ClearResult; override;
    property Modified: Boolean write SetModified;
    property ru: TRaceNode read GetRaceNode;
    //
    property ST: TPTime read GetST write SetST;
    property IT[Index: Integer]: TTimePoint read GetIT;
    property FT: TTimePoint read FFT;
    property ITCount: Integer read GetITCount;
  published
    property Bib: Integer read FBib write FBib;
    property SNR: Integer read FSNR write FSNR;
    property FN: string read GetFN;
    property LN: string read GetLN;
    property SN: string read GetSN;
    property NC: string read GetNOC;
    property GR: string read GetGender;
    property PB: string read GetPB;
    property DN: string read GetDN;
    property QU: TPenalty read FQU write SetQU;
    property DG: Integer read FDG write FDG;
    property MRank: Integer read FMRank write FMRank;
  end;

  TRaceRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TRaceRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TRaceRowCollectionItem);
  public
    procedure Load;
    procedure Save;
    function HasTimpointData(mark: Integer): Boolean;
    procedure Dump(Memo: TStrings);
    function Add: TRaceRowCollectionItem;
    procedure UpdateItem(e: TRaceEntry);
    procedure ResetIT(mark: Integer);
    function FindKey(SNR: Integer): TRaceRowCollectionItem;
    function FindBib(Bib: Integer): TRaceRowCollectionItem;
    property Items[Index: Integer]: TRaceRowCollectionItem read GetItem
      write SetItem;
  end;

  TRaceColProp = class(TBaseColProp)
  private
    function GetFieldCaptionDef(cl: TStammdatenRowCollection; Index: Integer;
      def: string): string;
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TRaceBO = class;

  TRaceNode = class(TBaseNode)
  private
    FOnCalc: TNotifyEvent;
    function GetRaceRowCollection: TRaceRowCollection;
    function GetRaceBO: TRaceBO;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    BOParams: TBOParams;
    StammdatenRowCollection: TStammdatenRowCollection;
    Index: Integer;
    BestTime: array of Integer;
    BestIndex: array of Integer;
    ST: TPTime;
    IsRacing: Boolean;
    //IsTied: Boolean;
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure Init(RowCount: Integer);
    procedure Calc; override;
    procedure CopyFromMRank;
    procedure CopyToMRank;
    function FindBib(Bib: Integer): TRaceRowCollectionItem;
    function FindSNR(SNR: Integer): TRaceRowCollectionItem;
    property RaceRowCollection: TRaceRowCollection read GetRaceRowCollection;
    property RaceBO: TRaceBO read GetRaceBO;
    property OnCalc: TNotifyEvent read FOnCalc write FOnCalc;
  end;

  TRaceBO = class(TBaseColBO)
  private
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
    FOnChange: TNotifyEvent;
    FFormatSettings: TFormatSettings;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure Changed;
    function ValidateOTime(t: TPTime; Value: string): string;
    procedure InitColsActiveLayout1(StringGrid: TColGrid; aLayout: Integer);
    procedure InitColsActiveLayout2(StringGrid: TColGrid; aLayout: Integer);
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    TableLayout: Integer;
    constructor Create;
    procedure InitColsActive(StringGrid: TColGrid); override;
    procedure InitColsActiveLayout(StringGrid: TColGrid;
      aLayout: Integer); override;
    procedure EditSNR(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditBib(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditQU(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditDG(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditOTime(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditMRank(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditST(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditIT(crgs: TBaseRowCollectionItem; var Value: string; const ColName: string);
    procedure EditFT(crgs: TBaseRowCollectionItem; var Value: string);
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write
      SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

uses
  RiggVar.BO.Def;

const
	NumID_SNR = 1;
	NumID_Bib = 2;
	NumID_FN = 3;
	NumID_LN = 4;
	NumID_SN = 5;
	NumID_NOC = 6;
	NumID_Gender = 7;
	NumID_PB = 8;

	NumID_QU = 9;
	NumID_DG = 10;
	NumID_MRank = 11;
	NumID_ST = 12;
	NumID_FT = 13;

	NumID_ORank = 14;
	NumID_Rank = 15;
	NumID_PosR = 16;
	NumID_PLZ = 17;

  NumID_Space = 18;

function NumID_IT (index: Integer): Integer;
begin
  result := 10000 + index * 10;
end;
function ITIndex (numID: Integer): Integer;
begin
  result := (numID - 10000) div 10;
end;
function IsITNumID (numID: Integer): Boolean;
begin
  result := numID > 10000;
end;

{ TRaceRowCollectionItem }

constructor TRaceRowCollectionItem.Create(Collection: TBaseRowCollection);
var
  i: Integer;
begin
  inherited;
  FQU := TPenaltyISAF.Create;

  SetLength(FIT, ru.BOParams.ITCount + 1);
  for i := 0 to ITCount-1 do
    FIT[i] := TTimePoint.Create;

  FFT := FIT[0];
end;

destructor TRaceRowCollectionItem.Destroy;
var
  i: Integer;
begin
  FQU.Free;
  for i := 0 to ITCount-1 do
    FIT[i].Free;
  inherited;
end;

procedure TRaceRowCollectionItem.Assign(Source: TPersistent);
var
  o: TRaceRowCollectionItem;
  e: TRaceEntry;
  i: Integer;
begin
  if Source is TRaceRowCollectionItem then
  begin
    o := TRaceRowCollectionItem(Source);
    Bib := o.Bib;
    SNR := o.SNR;
    QU.Assign(o.QU);
    DG := o.DG;
    MRank := o.MRank;
    ST.Assign(o.ST);
    for i := 0 to ITCount-1 do
      IT[i].Assign(o.IT[i]);
  end
  else if Source is TRaceEntry then
  begin
    e := TRaceEntry(Source);
    Bib := e.Bib;
    SNR := e.SNR;
    QU.FromString(e.QU); //.Parse(e.QU);
    DG := e.DG;
    MRank := e.MRank;
    ST.Parse(e.ST);
    for i := 0 to ITCount-1 do
      IT[i].Assign(e.IT[i]);
  end
  else
    inherited Assign(Source);
end;

function TRaceRowCollectionItem.GetSDItem: TStammdatenRowCollectionItem;
begin
  result := ru.StammdatenRowCollection.FindKey(SNR);
end;

function TRaceRowCollectionItem.GetFN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.FN
  else
    result := '';
end;

function TRaceRowCollectionItem.GetLN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.LN
  else
    result := '';
end;

function TRaceRowCollectionItem.GetSN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.SN
  else
    result := '';
end;

function TRaceRowCollectionItem.GetNOC: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.NC
  else
    result := '';
end;

function TRaceRowCollectionItem.GetGender: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.GR
  else
    result := '';
end;

function TRaceRowCollectionItem.GetPB: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.PB
  else
    result := '';
end;

procedure TRaceRowCollectionItem.SetQU(const Value: TPenalty);
begin
  if Value is TPenalty then
    QU.Assign(Value);
end;

function TRaceRowCollectionItem.GetST: TPTime;
begin
  result := ru.ST;
end;

procedure TRaceRowCollectionItem.SetST(const Value: TPTime);
begin
  if Value is TPTime then
    ru.ST.Assign(Value);
end;

procedure TRaceRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

function TRaceRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TRaceRowCollection;
begin
  cl := Collection as TRaceRowCollection;
  result := cl.BaseNode;
end;

function TRaceRowCollectionItem.GetDN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.DN
  else
    result := '';
end;

function TRaceRowCollectionItem.GetRaceNode: TRaceNode;
var
  cl: TRaceRowCollection;
begin
  result := nil;
  cl := Collection as TRaceRowCollection;
  if cl.BaseNode is TRaceNode then
    result := TRaceNode(cl.BaseNode);
end;

//procedure TRaceRowCollectionItem.ClearList;
//begin
//  inherited;
//  FSNR := 0;
//  FBib := BaseID;
//end;

procedure TRaceRowCollectionItem.ClearResult;
var
  i: Integer;
begin
  inherited;
  ST.Clear;
  DG := 0;
  MRank := 0;
  QU.Clear;
  for i := 0 to ITCount-1 do
    IT[i].Clear;
end;

function TRaceRowCollectionItem.GetIT(Index: Integer): TTimePoint;
begin
  result := nil;
  if (Index >= 0) and (Index < ITCount) then
    result := FIT[Index];
end;

function TRaceRowCollectionItem.GetITCount: Integer;
begin
  //result := ru.BOParams.ITCount + 1; //alternative
  result := Length(FIT);
end;

{ TRaceRowCollection }

function TRaceRowCollection.GetItem(Index: Integer):
  TRaceRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TRaceRowCollectionItem(inherited GetItem(Index));
end;

function TRaceRowCollection.HasTimpointData(mark: Integer): Boolean;
begin
  if Count < 2 then
    result := false
  else if Items[0].IT[mark] = nil then
    result := false
  else
    result := Items[0].IT[mark].PLZ <> Items[1].IT[mark].PLZ;
end;

procedure TRaceRowCollection.SetItem(Index: Integer; const Value:
  TRaceRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

function TRaceRowCollection.Add: TRaceRowCollectionItem;
begin
  Result := TRaceRowCollectionItem(inherited Add);
  Result.BaseID := Result.Index + 1;
end;

procedure TRaceRowCollection.UpdateItem(e: TRaceEntry);
var
  o: TRaceRowCollectionItem;
begin
  o := FindKey(e.SNR);
  if not Assigned(o) then
    o := Add;
  if Assigned(o) then
    o.Assign(e);
end;

procedure TRaceRowCollection.ResetIT(mark: Integer);
var
  i: Integer;
  cr: TRaceRowCollectionItem;
begin
  if (mark >= 0) and (mark <= BO.BOParams.ITCount) then
  begin
    for i := 0 to Count - 1 do
    begin
      cr := self.Items[i];
      cr.IT[mark].Clear;
    end;
    BaseNode.Modified := true;
  end;
end;

function TRaceRowCollection.FindKey(SNR: Integer):
  TRaceRowCollectionItem;
var
  i: Integer;
  o: TRaceRowCollectionItem;
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

function TRaceRowCollection.FindBib(Bib: Integer):
  TRaceRowCollectionItem;
var
  i: Integer;
  o: TRaceRowCollectionItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Items[i];
    if (o.Bib = Bib) then
    begin
      result := o;
      break;
    end;
  end;
end;

procedure TRaceRowCollection.Load;
begin
  //
end;

procedure TRaceRowCollection.Save;
begin
  //
end;

procedure TRaceRowCollection.Dump(Memo: TStrings);
begin
  //
end;

{ TRaceColProp }

procedure TRaceColProp.GetTextDefault(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
  i: Integer;
  TP: TTimePoint;
  BaseNumID: Integer;
begin
  Value := '';
  if crgs is TRaceRowCollectionItem then
    cr := TRaceRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NumID = NumID_SNR then
    Value := IntToStr(cr.SNR)

  else if NumID = NumID_Bib then
    Value := IntToStr(cr.Bib)

  else if NumID = NumID_FN then
    Value := cr.FN

  else if NumID = NumID_LN then
    Value := cr.LN

  else if NumID = NumID_SN then
    Value := cr.SN

  else if NumID = NumID_NOC then
    Value := cr.NC

  else if NumID = NumID_Gender then
    Value := cr.GR

  else if NumID = NumID_PB then
    Value := cr.PB

  else if NumID = NumID_QU then
    Value := cr.QU.ToString

  else if NumID = NumID_DG then
    Value := IntToStr(cr.DG)

  else if NumID = NumID_MRank then
    Value := IntToStr(cr.MRank)

  else if NumID = NumID_ST then
    Value := cr.ST.ToString

  else if NumID = NumID_FT then
    Value := cr.FT.OTime.ToString

  else if NumID = NumID_ORank then
    Value := IntToStr(cr.FT.ORank)

  else if NumID = NumID_Rank then
    Value := IntToStr(cr.FT.Rank)

  else if NumID = NumID_PosR then
    Value := IntToStr(cr.FT.PosR)

  else if NumID = NumID_PLZ then
    Value := IntToStr(cr.FT.PLZ + 1)

  else if IsITNumID(NumID) then
  begin
    i := ITIndex(NumID);
    BaseNumID := NumID_IT(i);
    TP := cr.IT[i];
    if Assigned(TP) then
    begin
      if NumID = BaseNumID + 1 then //if NameID = s then
        Value := TP.OTime.ToString
      else if NumID = BaseNumID + 2 then //else if NameID = s + 'B' then
        Value := TP.Behind.ToString
      else if NumID = BaseNumID + 3 then //else if NameID = s + 'BFT' then
        Value := TP.BFT.ToString
      else if NumID = BaseNumID + 4 then //else if NameID = s + 'BPL' then
        Value := TP.BPL.ToString
      else if NumID = BaseNumID + 5 then //else if NameID = s + 'ORank' then
        Value := IntToStr(TP.ORank)
      else if NumID = BaseNumID + 6 then //else if NameID = s + 'Rank' then
        Value := IntToStr(TP.Rank)
      else if NumID = BaseNumID + 7 then //else if NameID = s + 'PosR' then
        Value := IntToStr(TP.PosR)
      else if NumID = BaseNumID + 8 then //else if NameID = s + 'PLZ' then
        Value := IntToStr(TP.PLZ + 1);
    end;
  end;

end;

function TRaceColProp.GetFieldCaptionDef(
  cl: TStammdatenRowCollection; Index: Integer; def: string): string;
begin
  result := def;
  if (cl <> nil) then
    result := cl.GetFieldCaption(Index);
end;

procedure TRaceColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
  i: Integer;
  s: string;
  sc: string;
  ITCount: Integer;
  NumIDBase: Integer;
  scl: TStammdatenRowCollection;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  ColsAvail.UseCustomColCaptions := True;
  scl := BO.StammdatenNode.StammdatenRowCollection;

  ITCount := -1;
  if Assigned(BO) then
    ITCount := BO.BOParams.ITCount;

  { Bib }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Bib';
  cp.Caption := 'Bib';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_Bib;

  { SNR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SNR';
  cp.Caption := 'SNR';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_SNR;

  { FN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FN';
  cp.Caption := self.GetFieldCaptionDef(scl, 1, N_FN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_FN;

  { LN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_LN';
  cp.Caption := self.GetFieldCaptionDef(scl, 2, N_LN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_LN;

  { SN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SN';
  cp.Caption := self.GetFieldCaptionDef(scl, 3, N_SN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_SN;

  { NC }
  cp := ColsAvail.Add;
  cp.NameID := 'col_NC';
  cp.Caption := self.GetFieldCaptionDef(scl, 4, N_NC);
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NOC;

  { GR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GR';
  cp.Caption := self.GetFieldCaptionDef(scl, 5, N_GR);
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_Gender;

  { PB }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PB';
  cp.Caption := self.GetFieldCaptionDef(scl, 6, N_PB);
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_PB;

  { QU }
  cp := ColsAvail.Add;
  cp.NameID := 'col_QU';
  cp.Caption := 'QU';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_QU;

  { DG }
  cp := ColsAvail.Add;
  cp.NameID := 'col_DG';
  cp.Caption := 'DG';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_DG;

  { MRank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_MRank';
  cp.Caption := 'MRank';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_MRank;

  { ST }
  cp := ColsAvail.Add;
  cp.NameID := 'col_ST';
  cp.Caption := 'ST';
  cp.Width := 70;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_ST;

  { IT }
  for i := 0 to ITCount do
  begin
    s := 'col_IT' + IntToStr(i);
    if i = channel_FT then
      sc := 'FT' + IntToStr(i)
    else
      sc := 'IT' + IntToStr(i);
    NumIDBase := NumID_IT(i); //1000 + i * 100;
    { OTime }
    cp := ColsAvail.Add;
    cp.NameID := s;
    cp.Caption := sc;
    cp.Width := 70;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 1;
    { Behind }
    cp := ColsAvail.Add;
    cp.NameID := s + 'B';
    cp.Caption := sc + 'B';
    cp.Width := 70;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 2;
    { BFT }
    cp := ColsAvail.Add;
    cp.NameID := s + 'BFT';
    cp.Caption := sc + 'BFT';
    cp.Width := 70;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 3;
    { BPL }
    cp := ColsAvail.Add;
    cp.NameID := s + 'BPL';
    cp.Caption := sc + 'BPL';
    cp.Width := 70;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 4;
    { ORank }
    cp := ColsAvail.Add;
    cp.NameID := s + 'ORank';
    cp.Caption := sc + 'ORank';
    cp.Width := 55;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 5;
    { Rank }
    cp := ColsAvail.Add;
    cp.NameID := s + 'Rank';
    cp.Caption := sc + 'Rank';
    cp.Width := 55;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 6;
    { PosR }
    cp := ColsAvail.Add;
    cp.NameID := s + 'PosR';
    cp.Caption := sc + 'PosR';
    cp.Width := 55;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 7;
    { PLZ }
    cp := ColsAvail.Add;
    cp.NameID := s + 'PLZ';
    cp.Caption := sc + 'PLZ';
    cp.Width := 50;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.NumID := NumIDBase + 8;
  end;

  { OTime }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FT';
  cp.Caption := 'FT';
  cp.Width := 70;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_FT;

  { ORank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_ORank';
  cp.Caption := 'ORank';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_ORank;

  { Rank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Rank';
  cp.Caption := 'Rank';
  cp.Width := 45;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_Rank;

  { PosR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PosR';
  cp.Caption := 'PosR';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_PosR;

  { PLZ }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PLZ';
  cp.Caption := 'PLZ';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_PLZ;

  { Space }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Space';
  cp.Caption := '';
  cp.Width := 10;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_Space;
end;

{ TRaceNode }

constructor TRaceNode.Create(AOwner: TBaseColBO);
var
  frbo: TRaceBO;
begin
  inherited;
  IsRacing := True;
  ST := TPTime.Create;
  BaseRowCollection := TRaceRowCollection.Create(Self,
    TRaceRowCollectionItem);
  frbo := RaceBO;
  SetLength(BestTime, 1);
  SetLength(BestIndex, 1);
  if Assigned(frbo) then
  begin
    if Assigned(BO) then
    begin
      BOParams := BO.BOParams; //nicht erzeugt, nur Referenz kopiert
      StammdatenRowCollection := BO.StammdatenNode.StammdatenRowCollection;
      //..einiges wird in TBO.Init gesetzt, wo der RaceNode erzeugt wird
      SetLength(BestTime, BOParams.ITCount+1);
      SetLength(BestIndex, BOParams.ITCount+1);
    end;
  end;
end;

destructor TRaceNode.Destroy;
begin
  BaseRowCollection.Free;
  ST.Free;
  inherited;
end;

function TRaceNode.GetRaceRowCollection: TRaceRowCollection;
begin
  result := BaseRowCollection as TRaceRowCollection;
end;

function TRaceNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TRaceColProp;
end;

procedure TRaceNode.Save;
begin
end;

procedure TRaceNode.Load;
var
  o: TRaceRowCollectionItem;
begin
  RaceRowCollection.Clear;

  o := RaceRowCollection.Add;
  o.SNR := 1001;
  o.Bib := 1;
  o.BaseID := 1;

  o := RaceRowCollection.Add;
  o.SNR := 1002;
  o.Bib := 2;
  o.BaseID := 2;

  o := RaceRowCollection.Add;
  o.SNR := 1003;
  o.Bib := 3;
  o.BaseID := 3;
end;

procedure TRaceNode.Init(RowCount: Integer);
var
  o: TRaceRowCollectionItem;
  i: Integer;
begin
  BaseRowCollection.Clear;

  for i := 0 to RowCount - 1 do
  begin
    o := RaceRowCollection.Add;
    o.BaseID := i + 1;
    o.SNR := 1000 + i + 1;
    o.Bib := i + 1;
  end;
end;

procedure TRaceNode.Calc;
begin
  BO.CalcTP.Calc(Self);
  Modified := False;
  if Assigned(OnCalc) then
    OnCalc(Self);
end;

function TRaceNode.GetRaceBO: TRaceBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TRaceBO;
end;

function TRaceNode.FindBib(Bib: Integer): TRaceRowCollectionItem;
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  result := nil;
  if Bib = 0 then
    exit;
  cl := RaceRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if cr.Bib = Bib then
    begin
      result := cr;
      break;
    end;
  end;
end;

function TRaceNode.FindSNR(SNR: Integer): TRaceRowCollectionItem;
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  result := nil;
  if SNR = 0 then
    exit;
  cl := RaceRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if cr.SNR = SNR then
    begin
      result := cr;
      break;
    end;
  end;
end;

procedure TRaceNode.CopyFromMRank;
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  cl := Self.RaceRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if cr.MRank = 0 then
      cr.FT.OTime.Clear
    else
      cr.FT.OTime.Parse(IntToStr(cr.MRank));
    cr.Modified := True;
  end;
end;

procedure TRaceNode.CopyToMRank;
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  cl := Self.RaceRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    cr.MRank := cr.FT.PosR;
  end;
end;

{ TRaceBO }

procedure TRaceBO.InitColsActive(StringGrid: TColGrid);
begin
  if Assigned(CurrentNode) then
    InitColsActiveLayout(StringGrid, CurrentNode.Layout);
end;

procedure TRaceBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
begin
  if TableLayout = 2 then
    InitColsActiveLayout2(StringGrid, aLayout)
  else
    InitColsActiveLayout1(StringGrid, aLayout)
end;

procedure TRaceBO.InitColsActiveLayout1(StringGrid: TColGrid;
  aLayout: Integer);
var
  cp: TBaseColProp;
  s: string;
  i: Integer;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');
    //AddColumn('col_GR');
    //AddColumn('col_PB');

    { funktioniert, soll aber nur im Main-View geändert werden }
    AddColumn('col_SNR');
    //cp.OnFinishEdit := EditSNR;
    //cp.ReadOnly := False;
    //cp.OnFinishEdit := EditBib;
    //cp.ReadOnly := False;

    //AddColumn('col_FN');
    //AddColumn('col_LN');
    //AddColumn('col_SN');
    AddColumn('col_Bib');
    AddColumn('col_NC');

    AddColumn('col_MRank');
    //cp.OnFinishEdit := EditMRank;
    //cp.ReadOnly := False;

    cp := AddColumn('col_QU');
    cp.OnFinishEdit := EditQU;
    cp.ReadOnly := False;

    cp := AddColumn('col_DG');
    cp.OnFinishEdit := EditDG;
    cp.ReadOnly := False;

    cp := AddColumn('col_ST');
    cp.OnFinishEdit := EditST;
    cp.ReadOnly := False;

    if (aLayout >= 0) and (aLayout <= BO.BOParams.ITCount) then
    begin
      i := aLayout;
      s := 'col_IT' + IntToStr(i);
      cp := AddColumn(s);
      if Assigned(cp) then
      begin
        cp.OnFinishEdit2 := EditIT;
        cp.ReadOnly := False;
      end;
      AddColumn(s + 'B');
      //AddColumn(s + 'BFT');
      AddColumn(s + 'BPL');
      AddColumn(s + 'Rank');
      AddColumn(s + 'PosR');
      AddColumn(s + 'PLZ');
    end;

    cp := AddColumn('col_FT');
    cp.OnFinishEdit := EditFT;
    cp.ReadOnly := False;
    AddColumn('col_ORank');
    AddColumn('col_Rank');
    AddColumn('col_PosR');
    AddColumn('col_PLZ');
  end;
end;

procedure TRaceBO.InitColsActiveLayout2(StringGrid: TColGrid;
  aLayout: Integer);
var
  cp: TBaseColProp;
  s: string;
  i: Integer;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');
    AddColumn('col_SNR');
    AddColumn('col_Bib');
    AddColumn('col_NC');

    cp := AddColumn('col_QU');
    cp.OnFinishEdit := EditQU;
    cp.ReadOnly := False;

    if (aLayout >= 0) and (aLayout <= BO.BOParams.ITCount) then
    begin
      i := aLayout;
      s := 'col_IT' + IntToStr(i);
      cp := AddColumn(s);
      if Assigned(cp) then
      begin
        cp.OnFinishEdit2 := EditIT;
        cp.ReadOnly := False;
      end;
      AddColumn(s + 'B');
      AddColumn(s + 'Rank');
    end;
    //AddColumn('col_PosR');
  end;
end;

function TRaceBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

function TRaceBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

procedure TRaceBO.SetCurrentRow(const Value:
  TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TRaceBO.SetCurrentNode(const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TRaceBO.EditSNR(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    cr.SNR := StrToIntDef(Value, cr.SNR);
    Value := IntToStr(cr.SNR);
    { horizontal kopieren }
    BO.SNR[cr.Index] := cr.SNR;
  end;
end;

procedure TRaceBO.EditBib(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    cr.Bib := StrToIntDef(Value, cr.Bib);
    Value := IntToStr(cr.Bib);
    { horizontal kopieren }
    BO.Bib[cr.Index] := cr.Bib;
  end;
end;

procedure TRaceBO.EditQU(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    if Pos(',',Value) > 0 then
      cr.QU.FromString(Value)
    else
      cr.QU.Parse(Value);
    Value := cr.QU.ToString;
    cr.Modified := True;
    //BO.QU[cr.ru.Index, cr.Index] := cr.QU.AsInteger;
    BO.Penalty[cr.ru.Index, cr.Index] := cr.QU;
  end;
end;

procedure TRaceBO.EditDG(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    cr.DG := StrToIntDef(Value, cr.DG);
    Value := IntToStr(cr.DG);
    cr.Modified := True;
    BO.DG[cr.ru.Index, cr.Index] := cr.DG;
  end;
end;

procedure TRaceBO.EditOTime(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    cr.MRank := StrToIntDef(Value, cr.MRank);
    Value := IntToStr(cr.MRank);
    //cr.Modified := True;
    BO.OT[cr.ru.Index, cr.Index] := cr.MRank;
  end;
end;

procedure TRaceBO.EditMRank(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
  cl: TRaceRowCollection;
  oldRank: Integer;
  newRank: Integer;
  maxRank: Integer;
  temp: Integer;
  i: Integer;
  cr1: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    cl := cr.ru.RaceRowCollection;

    oldRank := cr.MRank;
    newRank := StrToIntDef(Value, cr.MRank);
    maxRank := 0;
    for i := 0 to cl.Count-1 do
    begin
      cr1 := cl.Items[i];
      if cr = cr1 then
        continue
      else if cr1.MRank > 0 then
        Inc(maxRank);
    end;

    { limit new value }
    if newRank < 0 then
      newRank := 0;
    if newRank > maxRank + 1 then
      newRank := maxRank + 1;
    if newRank > cl.Count then
      newRank := cl.Count;

    if oldRank = newRank then
      Value := IntToStr(cr.MRank)
    else
    begin
      for i := 0 to cl.Count-1 do
      begin
        cr1 := cl.Items[i];
        if cr1 = cr then
          Continue;
        temp := cr1.MRank;
        { remove }
        if (oldRank > 0) and (oldRank < temp) then
          cr1.MRank := temp - 1;
        { insert }
        if (newRank > 0) and (newRank <= cr1.MRank) then
          cr1.MRank := cr1.MRank + 1;
      end;
      cr.MRank := newRank;
      Value := IntToStr(cr.MRank);
      Changed;
      cr.Modified := True;
    end;
  end;
end;

procedure TRaceBO.EditST(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    Value := ValidateOTime(cr.ST, Value);
    cr.Modified := True;
  end;
end;

procedure TRaceBO.EditIT(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TRaceRowCollectionItem;
  i: Integer;
  t: TTimePoint;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    i := StrToIntDef(Copy(ColName, 7, Length(ColName)), -1);
    t := cr.IT[i];
    if Assigned(t) then
    begin
      Value := ValidateOTime(t.OTime, Value);
      cr.Modified := True;
      BO.CalcTP.UpdateDynamicBehind(Self, cr, i);
    end;
  end;
end;

procedure TRaceBO.EditFT(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TRaceRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TRaceRowCollectionItem) then
  begin
    cr := TRaceRowCollectionItem(crgs);
    Value := ValidateOTime(cr.FT.OTime, Value);
    cr.Modified := True;
    BO.CalcTP.UpdateDynamicBehind(Self, cr, channel_FT);
  end;
end;

function TRaceBO.ValidateOTime(t: TPTime; Value: string): string;
begin
  if True {not Locked} then
  begin
    if Value = '-1' then
      t.Clear
    else if Value = 'n' then
      //t.Parse(TimeToStr(Now, FFormatSettings))
      t.Parse(TimeToStr(Now))
    else
      t.Parse(Value);
  end;
  result := t.ToString;
end;

procedure TRaceBO.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TRaceBO.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TRaceBO.Create;
begin
  inherited;
  TableLayout := 1;
  FFormatSettings.TimeSeparator := ':';
  FFormatSettings.DecimalSeparator := '.';
end;

{ TTimePoint }

procedure TTimePoint.Assign(Source: TPersistent);
var
  o: TTimePoint;
  e: TTimePointEntry;
begin
  if Source is TTimePoint then
  begin
    o := TTimePoint(Source);
    OTime.Assign(o.OTime);
    Behind.Assign(o.Behind);
    BFT.Assign(o.BFT);
    BPL.Assign(o.BPL);
    ORank := o.ORank;
    Rank := o.Rank;
    PosR := o.PosR;
    PLZ := o.PLZ;
  end
  else if Source is TTimePointEntry then
  begin
    e := TTimePointEntry(Source);
    OTime.Parse(e.OTime);
    Behind.Parse(e.Behind);
    BFT.Parse(e.BFT);
    BPL.Parse(e.BPL);
    ORank := e.ORank;
    Rank := e.Rank;
    PosR := e.PosR;
    PLZ := e.PLZ;
  end
  else
    inherited Assign(Source);
end;

procedure TTimePoint.Clear;
begin
  OTime.Clear;
  Behind.Clear;
  BFT.Clear;
  BPL.Clear;
  ORank := 0;
  Rank := 0;
  PosR := 0;
  PLZ := 0;
end;

constructor TTimePoint.Create;
begin
  inherited;
  FOTime := TPTime.Create;
  FBehind := TPTime.Create;
  FBFT := TQTime.Create;
  FBPL := TQTime.Create;
end;

destructor TTimePoint.Destroy;
begin
  FOTime.Free;
  FBehind.Free;
  FBFT.Free;
  FBPL.Free;
  inherited;
end;

procedure TTimePoint.SetBehind(const Value: TPTime);
begin
  if Value is TPTime then
    FBehind.Assign(Value);
end;

procedure TTimePoint.SetBFT(const Value: TQTime);
begin
  if Value is TQTime then
    FBFT.Assign(Value);
end;

procedure TTimePoint.SetBPL(const Value: TQTime);
begin
  if Value is TQTime then
    FBPL.Assign(Value);
end;

procedure TTimePoint.SetOTime(const Value: TPTime);
begin
  if Value is TPTime then
    FOTime.Assign(Value);
end;

{ TTimePointEntry }

procedure TTimePointEntry.Assign(Source: TPersistent);
var
  o: TTimePoint;
  e: TTimePointEntry;
begin
  if Source is TTimePointEntry then
  begin
    e := TTimePointEntry(Source);
    OTime := e.OTime;
    Behind := e.Behind;
    BFT := e.BFT;
    BPL := e.BPL;
    ORank := e.ORank;
    Rank := e.Rank;
    PosR := e.PosR;
    PLZ := e.PLZ;
  end
  else if Source is TTimePoint then
  begin
    o := TTimePoint(Source);
    OTime := o.OTime.ToString;
    Behind := o.Behind.ToString;
    BFT := o.BFT.ToString;
    BPL := o.BPL.ToString;
    ORank := o.ORank;
    Rank := o.Rank;
    PosR := o.PosR;
    PLZ := o.PosR;
  end
  else
    inherited Assign(Source);
end;

{ TRaceEntry }

constructor TRaceEntry.Create(aITCount: Integer);
var
  i: Integer;
begin
  inherited Create;
  SetLength(IT, aITCount + 1);
  for i := 0 to ITCount-1 do
    IT[i] := TTimePointEntry.Create;
  FT := IT[0];
end;

destructor TRaceEntry.Destroy;
var
  i: Integer;
begin
  for i := 0 to ITCount-1 do
    IT[i].Free;
  inherited;
end;

function TRaceEntry.GetITCount: Integer;
begin
  result := Length(IT);
end;

function TRaceEntry.GetRunID: string;
begin
  result := '';
end;

{ TRaceRemoteObject }

function TRaceRemoteObject.GetRunID: string;
begin
  result := FRunID;
end;

procedure TRaceRemoteObject.SetRunID(const Value: string);
begin
  FRunID := Value;
end;

procedure TRaceRemoteObject.Assign(Source: TPersistent);
var
  o: TRaceRowCollectionItem;
  e: TRaceRemoteObject;
  i: Integer;
begin
  if Source is TRaceRowCollectionItem then
  begin
    o := TRaceRowCollectionItem(Source);
    SO := o.BaseID;
    Bib := o.Bib;
    //
    SNR := o.SNR;
    FN := o.FN;
    LN := o.LN;
    SN := o.SN;
    NC := o.NC;
    GR := o.GR;
    PB := o.PB;
    //
    QU := o.QU.ToString;
    DG := o.DG;
    ST := o.ST.ToString;
    for i := 0 to ITCount-1 do
      if i < o.ITCount then
        IT[i].Assign(o.IT[i]);
    MRank := o.MRank;
  end
  else if Source is TRaceRemoteObject then
  begin
    e := TRaceRemoteObject(Source);
    //
    SO := e.SO;
    Bib := e.Bib;
    //
    SNR := e.SNR;
    FN := e.FN;
    LN := e.LN;
    SN := e.SN;
    NC := e.NC;
    GR := e.GR;
    PB := e.PB;
    //
    QU := e.QU;
    DG := e.DG;
    ST := e.ST;
    for i := 0 to ITCount-1 do
      if i < e.ITCount then
        IT[i].Assign(e.IT[i]);
    MRank := e.MRank;
  end
  else
    inherited Assign(Source);
end;

function TRaceRemoteObject.GetCommaText(SL: TStrings): string;
var
  i: Integer;
  tp: TTimePointEntry;
begin
  if not Assigned(SL) then exit;
  SL.Clear;

  SL.Add(RunID);
  SL.Add(IntToStr(SO));
  SL.Add(IntToStr(Bib));
  //
  SL.Add(IntToStr(SNR));
  SL.Add(FN);
  SL.Add(LN);
  SL.Add(SN);
  SL.Add(NC);
  SL.Add(GR);
  SL.Add(PB);
  //
  SL.Add(QU);
  SL.Add(IntToStr(DG));
  SL.Add(ST);
  { FT }
  for i := 0 to 0 do
  begin
    tp := IT[i];
    SL.Add(tp.OTime);
    SL.Add(tp.Behind);
    SL.Add(tp.BFT);
    SL.Add(tp.BPL);
    SL.Add(IntToStr(tp.ORank));
    SL.Add(IntToStr(tp.Rank));
    SL.Add(IntToStr(tp.PosR));
    SL.Add(IntToStr(tp.PLZ));
  end;
  SL.Add(IntToStr(MRank));
  { IT }
  SL.Add(IntToStr(ITCount));
  for i := 1 to ITCount-1 do
  begin
    tp := IT[i];
    SL.Add(tp.OTime);
    SL.Add(tp.Behind);
    SL.Add(tp.BFT);
    SL.Add(tp.BPL);
    SL.Add(IntToStr(tp.ORank));
    SL.Add(IntToStr(tp.Rank));
    SL.Add(IntToStr(tp.PosR));
    SL.Add(IntToStr(tp.PLZ));
  end;

  result := SL.CommaText;
end;

procedure TRaceRemoteObject.SetCommaText(SL: TStrings);
var
  i, c: Integer;
  tempITCount: Integer;
  tp: TTimePointEntry;

  function NextI: Integer;
  var
    s: string;
  begin
    if i < c then
    begin
      s := SL[i];
      result := StrToIntDef(s, -1);
    end
    else
      result := -1;
    Inc(i);
  end;

  function NextS: string;
  var
    s: string;
  begin
    if i < c then
    begin
      s := SL[i];
      result := s;
    end
    else
      result := '';
    Inc(i);
  end;

  function NextC: char;
  begin
    if (i < c) and (Length(SL[i]) = 1) then
      result := SL[i][1]
    else
      result := EmptyMark;
    Inc(i);
  end;

begin
  if not Assigned(SL) then exit;
  i := 0;
  c := SL.Count;

  RunID := NextS;
  SO := NextI;
  Bib := NextI;
  //
  SNR := NextI;
  FN := NextS;
  LN := NextS;
  SN := NextS;
  NC := NextS;
  GR := NextS;
  PB := NextS;
  //
  QU := NextS;
  DG := NextI;
  ST := NextS;
  { FT }
  i := 0;
  tp := IT[i];
  tp.OTime := NextS;
  tp.Behind := NextS;
  //tp.BFT := NextS;
  tp.BPL := NextS;
  tp.ORank := NextI;
  tp.Rank := NextI;
  tp.PosR := NextI;
  tp.PLZ := NextI;
  MRank := NextI;
  { IT }
  tempITCount := NextI;
  { limit }
  if tempITCount > ITCount then
    tempITCount := ITCount;
  for i := 1 to tempITCount-1 do
  begin
    tp := IT[i];
    tp.OTime := NextS;
    tp.Behind := NextS;
    tp.BFT := NextS;
    tp.BPL := NextS;
    tp.ORank := NextI;
    tp.Rank := NextI;
    tp.PosR := NextI;
    tp.PLZ := NextI;
  end;
end;

function TRaceRemoteObject.GetCSV_Header: string;
var
  i: Integer;
  s: string;
  sep: string;
begin
  sep := ',';
  result :=
    'RunID' + sep +
    'Pos' + sep +
    'Bib' + sep +
    //
    'SNR' + sep +
    N_FN + sep +
    N_LN + sep +
    N_SN + sep +
    N_NC + sep +
    N_GR + sep +
    N_PB + sep +
     //
    'QU' + sep +
    'DG' + sep +
    'ST' + sep +

    'FTTime' + sep +
    'FTBehind' + sep +
    //'BFT'
    'FTBPL' + sep +
    'FTRank' + sep +
    'FTPosR' + sep +
    'FTPLZ' + sep +

    'MRank' + sep +
    'ITCount';

  for i := 1 to ITCount-1 do
  begin
    s := 'IT' + IntToStr(i);
    result := result + sep +
    s + 'Time' + sep +
    s + 'Behind' + sep +
    s + 'BFT' + sep +
    s + 'BPL' + sep +
    s + 'Rank' + sep +
    s + 'PosR' + sep +
    s + 'PLZ';
  end;
end;

procedure TRaceRemoteObject.GetOutput;
var
  i: Integer;
  s: string;
  tp: TTimePointEntry;
begin
  //SLADD('Pos', IntToStr(SO));
  SLADD('Bib', IntToStr(Bib));
  //
  SLADD('SNR', IntToStr(SNR));
  SLADD(N_FN, FN);
  SLADD(N_LN, LN);
  SLADD(N_SN, SN);
  SLADD(N_NC, NC);
  SLADD(N_GR, GR);
  SLADD(N_PB, PB);
  //
  { Parameter }
  i := 0;
  if i = 0 then
    s := 'FT'
  else
    s := 'IT' + IntToStr(i);

  tp := IT[i];
  SLADD(s + 'Time', tp.OTime);
  SLADD(s + 'Behind', tp.Behind);
  if i > 0 then
    SLADD(s + 'BFT', tp.BFT);
  SLADD(s + 'BPL', tp.BPL);
  SLADD(s + 'ORank', IntToStr(tp.ORank));
  SLADD(s + 'Rank', IntToStr(tp.Rank));
  SLADD(s + 'PosR', IntToStr(tp.PosR));
  SLADD(s + 'PLZ', IntToStr(tp.PLZ));
end;

{
procedure TRaceRemoteObject.GetFieldDefs(FD: TFieldDefs);
begin
  FD.Add('RunID', ftInteger, 0, True);
  FD.Add('Pos', ftInteger, 0, True);
  FD.Add('Bib', ftInteger);

  FD.Add('SNR', ftInteger);

  FD.Add('QU', ftString, 3);
  FD.Add('DG', ftInteger);
  FD.Add('TP', ftInteger);
  FD.Add('ST', ftString, 11);
  FD.Add('OTime', ftString, 11);
  FD.Add('LTime', ftString, 11);
  FD.Add('Behind', ftString, 11);
  FD.Add('BFT', ftString, 11);
  FD.Add('BPL', ftString, 11);
  FD.Add('ORank', ftInteger);
  FD.Add('Rank', ftInteger);
  FD.Add('PosR', ftInteger);
  FD.Add('PLZ', ftInteger);
end;

procedure TRaceRemoteObject.UpdateDataSet(DS: TDataSet);
var
  i: Integer;
begin
  if DS = nil then exit;

  for i := 0 to ITCount-1 do
  begin

  if DS.Locate('RunID;Pos', VarArrayOf([RunID,SO]), [loCaseInsensitive]) then
    DS.Edit
  else
  begin
    DS.Append;
    DS.FieldByName('ID').AsInteger := NewID;
  end;

  try
    DS.FieldByName('RunID').AsString := RunID;
    DS.FieldByName('Pos').AsInteger := SO;
    DS.FieldByName('Bib').AsInteger := Bib;

    DS.FieldByName('SNR').AsInteger := SNR;

    DS.FieldByName('QU').AsString := QU;
    DS.FieldByName('DG').AsInteger := DG;
    DS.FieldByName('TP').AsInteger := i;
    DS.FieldByName('ST').AsString := ST;
    DS.FieldByName('OTime').AsString := IT[i].OTime;
    DS.FieldByName('LTime').AsString := IT[i].OTime;
    DS.FieldByName('Behind').AsString := IT[i].Behind;
    DS.FieldByName('BFT').AsString := IT[i].BFT;
    DS.FieldByName('BPL').AsString := IT[i].BPL;
    DS.FieldByName('ORank').AsInteger := IT[i].ORank;
    DS.FieldByName('Rank').AsInteger := IT[i].Rank;
    DS.FieldByName('PosR').AsInteger := IT[i].PosR;
    DS.FieldByName('PLZ').AsInteger := IT[i].PLZ;

    DS.Post;
  except
    DS.Cancel;
  end;

  end;
end;
}

end.

