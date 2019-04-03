unit RiggVar.Col.Event;

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

{
  RCount = BO.BOParams.RaceCount + 1;

  Race[0] --> series results
  Race[1] ... Race[RCount-1] --> race-results

  BO.RNode[0] --> series results
  BO.RNode[1] ... BO.RNode[RaceCount] --> race results
}

//{$define RacePart}

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColBase,
  RiggVar.Grid.ColGrid,
  RiggVar.BO.MsgToken,
  RiggVar.BO.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.BO.Time,
  RiggVar.BO.UndoManager,
  RiggVar.Col.BaseEntry,
  RiggVar.Col.Race,
  RiggVar.Col.Stammdaten,
  RiggVar.Util.Classes,
  RiggVar.Util.Props;

const
  Layout_Points = 0;
  Layout_Finish = 1;

  clFleetYellow = $00CCFFFF;
  clFleetBlue = $00FFFFCC;
  clFleetRed = $00CCCCFF;
  clFleetGreen = $00CCFFCC;

type
  TEventNode = class;
  TEventBO = class;

  TColorMode = (
    ColorMode_None,
    ColorMode_Error,
    ColorMode_Fleet
  );

  TFinishError = (
    error_OutOfRange_OTime_Min,
    error_OutOfRange_OTime_Max,
    error_Duplicate_OTime,
    error_Contiguous_OTime
    );

  TEntryError = (
    error_Duplicate_SNR,
    error_Duplicate_Bib,
    error_OutOfRange_Bib,
    error_OutOfRange_SNR
    );

  TEventRaceEntry = class(TBaseObject)
  private
    FNode: TEventNode;
    FPenalty: TPenaltyISAF;
    function GetRaceValue: string;
    procedure SetRaceValue(const Value: string);
    function GetQU: Integer;
    procedure SetQU(const Value: Integer);
    function GetCPoints: double;
    function GetPoints: string;
    procedure SetCPoints(const Value: double);
    procedure SetCTime1(const Value: Integer);
    function GetCTime1: Integer;
    function GetLayout: Integer;
    function GetDecimalPoints: string;
    function ParseFleet(const Value: string): Integer;
  protected
    FCTime: Integer; //Points
    FTiebreakIncrement: Integer;
  public
    IsRacing: Boolean;
    Fleet: Integer;
    Drop: Boolean;
    DG: Integer;
    OTime: Integer; //ORank
    Rank: Integer;
    PosR: Integer;
    PLZ: Integer;
    FinishErrors: set of TFinishError;
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    property RaceValue: string read GetRaceValue write SetRaceValue;
    property QU: Integer read GetQU write SetQU;
    property Penalty: TPenaltyISAF read FPenalty;
    property Points: string read GetPoints;
    property DecimalPoints: string read GetDecimalPoints;
    property CPoints: double read GetCPoints write SetCPoints;
    property CTime: Integer read FCTime;
    property CTime1: Integer read GetCTime1 write SetCTime1;
    property Layout: Integer read GetLayout;
    property CTimeAccess: Integer read FCTime;
    property TiebreakIncrement: Integer read FTiebreakIncrement;
  end;

  TEventEntry = class(TBaseEntry)
  private
    function GetRCount: Integer;
  public
    Bib: Integer;
    //
    SNR: Integer;
    DN: string;
    NOC: string;
    Race: array of TEventRaceEntry;
    GRace: TEventRaceEntry;
    Cup: string;
    constructor Create(aRaceCount: Integer);
    destructor Destroy; override;
    property RCount: Integer read GetRCount;
  end;

  TEventRemoteObject = class(TEventEntry)
  private
    FEventID: string;
    procedure SetEventID(const Value: string);
    function GetEventID: string;
  protected
    procedure GetOutput; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetCommaText(SL: TStrings): string; override;
    procedure SetCommaText(SL: TStrings); override;
    function GetCSV_Header: string; override;
    property EventID: string read GetEventID write SetEventID;
  end;

  TEventRowCollectionItem = class(TBaseRowCollectionItem)
  private
    FSNR: Integer;
    FBib: Integer;
    OPoints: Integer;
    ORank: Integer;
    procedure SetModified(const Value: Boolean);
    function GetEventNode: TEventNode;
    function GetSDItem: TStammdatenRowCollectionItem;
    function GetFN: string;
    function GetLN: string;
    function GetSN: string;
    function GetNOC: string;
    function GetGender: string;
    function GetPB: string;
    function GetDN: string;
    function GetRaceValue(Index: Integer): string;
    procedure SetRaceValue(Index: Integer; const Value: string);
    function GetGPosR: Integer;
    function GetGRank: Integer;
    function GetGTime1: Integer;
    function GetGPoints: string;
    function GetPLZ: Integer;
    function GetProps: TProps;
    function GetRaceCount: Integer;
    function GetRCount: Integer;
    function FleetColorBold(r: Integer; aColor: TColor): TColor;
    function FleetColor(r: Integer; aColor: TColor): TColor;
    function BibColor(aColor: TColor): TColor;
    function RaceColor(NumID: Integer; aColor: TColor): TColor;
    function SNRColor(aColor: TColor): TColor;
    function RaceErrorColor(r: Integer; aColor: TColor): TColor;
    function GetDPoints: string;
    function GetDRank: Integer;
  public
    Race: array of TEventRaceEntry;
    GRace: TEventRaceEntry;
    //Cup: Integer;
    RA: double; //Uniqua-Ranglistenpunkte
    QR: double; //WM Qualifikations-Punkte
    isTied: Boolean;
    isGezeitet: Boolean;
    EntryErrors: set of TEntryError;
    constructor Create(Collection: TBaseRowCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateCellProp(cp: TBaseColProp; cellProp: TCellProp); override;
    function ColumnToColorDef(cp: TBaseColProp; aColor: TColor): TColor; override;
    procedure ClearResult; override;
    function GetBaseNode: TBaseNode;
    property Modified: Boolean write SetModified;
    property ru: TEventNode read GetEventNode;
    property RaceValue[Index: Integer]: string read GetRaceValue write SetRaceValue;
    property RCount: Integer read GetRCount;
    property RaceCount: Integer read GetRaceCount;
  published
    property Bib: Integer read FBib write FBib;
    //
    property SNR: Integer read FSNR write FSNR;
    property FN: string read GetFN;
    property LN: string read GetLN;
    property SN: string read GetSN;
    property NC: string read GetNOC;
    property GR: string read GetGender;
    property PB: string read GetPB;
    property DN: string read GetDN;
    property Props: TProps read GetProps;
    //
    property GPoints: string read GetGPoints;
    property GTime1: Integer read GetGTime1;
    property GRank: Integer read  GetGRank;
    property GPosR: Integer read GetGPosR;
    property PLZ: Integer read GetPLZ;
    property DPoints: string read GetDPoints;
    property DRank: Integer read GetDRank;
  end;

  TEventRowCollection = class(TBaseRowCollection)
  private
    function GetItem(Index: Integer): TEventRowCollectionItem;
    procedure SetItem(Index: Integer; const Value: TEventRowCollectionItem);
    function GetRCount: Integer;
    function GetRaceCount: Integer;
  public
    procedure Load;
    procedure Save;

    function GetHashString: string;

    procedure GetXML(SL: TStrings);
    procedure GetXMLSchema(SL: TStrings);
    procedure GetXMLResult(SL: TStrings);
    procedure GetXMLBackup(SL: TStrings);

    procedure ResetRace(r: Integer);
    function FleetCount(r: Integer): Integer;
    procedure FillFleetList(FL: TBaseList; r: Integer; f: Integer);
    function Add: TEventRowCollectionItem;
    procedure UpdateItem(e: TEventEntry);
    function FindKey(SNR: Integer): TEventRowCollectionItem;
    property Items[Index: Integer]: TEventRowCollectionItem read GetItem
      write SetItem;
    property RCount: Integer read GetRCount;
    property RaceCount: Integer read GetRaceCount;
  end;

  TCRList = class(TBaseList)
  private
    function GetCR(i: Integer): TEventRowCollectionItem;
    procedure SetCR(i: Integer; const Value: TEventRowCollectionItem);
  public
    constructor Create(aOwnsObjects: Boolean);
    property CR[i: Integer]: TEventRowCollectionItem read GetCR write SetCR;
  end;

  TEventColProp = class(TBaseColProp)
  private
    procedure GetSortKeyGPosR(crgs: TBaseRowCollectionItem;
      var Value: string; const ColName: string);
    procedure GetSortKeyPoints(crgs: TBaseRowCollectionItem;
      var Value: string; const ColName: string);
    procedure GetSortKeyRace(crgs: TBaseRowCollectionItem;
      var Value: string; const ColName: string);
    function GetFieldCaptionDef(cl: TStammdatenRowCollection; Index: Integer;
      def: string): string;
    procedure GetSortKeyDRank(crgs: TBaseRowCollectionItem; var Value: string;
      const ColName: string);
    procedure GetSortKeyDPoints(crgs: TBaseRowCollectionItem; var Value: string;
      const ColName: string);
  protected
    function GetRaceCount: Integer;
  public
    procedure InitColsAvail; override;
    procedure GetTextDefault(crgs: TBaseRowCollectionItem; var Value: string);
      override;
  end;

  TEventErrorList = class
  public
    function IsPreconditionForStrictInputMode(ev: TEventNode): Boolean; virtual; abstract;
    function CheckAll(ev: TEventNode): Boolean; virtual; abstract;
    procedure GetMsg(Memo: TStrings); virtual; abstract;
    function HasErrors: Boolean; virtual; abstract;
  end;

  TEventNode = class(TBaseNode)
  private
    CalcCounter: Integer;
    FOnCalc: TNotifyEvent;
    FShowPoints: Integer;
    FColorMode: TColorMode;
    FFirstFinalRace: Integer;
    function GetEventRowCollection: TEventRowCollection;
    function GetEventBO: TEventBO;
    function GetShowPoints: Integer;
    procedure SetShowPoints(const Value: Integer);
    function GetRaceCount: Integer;
    function GetRCount: Integer;
    function GetFirstFinalRace: Integer;
    procedure SetFirstFinalRace(const Value: Integer);
    function GetIsInFinalPhase: Boolean;
    procedure UpdateDiff;
  protected
    function GetBaseColPropClass: TBaseColPropClass; override;
  public
    StammdatenRowCollection: TStammdatenRowCollection;
    ErrorList: TEventErrorList;
    WebLayout: Integer;
    ShowPLZColumn: Boolean;
    ShowPosRColumn: Boolean;
    UseFleets: Boolean;
    TargetFleetSize: Integer;
    PartialCalcLastRace: Integer;
    constructor Create(AOwner: TBaseColBO);
    destructor Destroy; override;
    procedure Load;
    procedure Init(RowCount: Integer);
    function FindBib(b: Integer): TEventRowCollectionItem;
    //
    procedure GoBackToRace(r: Integer);
    procedure InitFleet(r: Integer);
    procedure InitFleetByFinishHack(r: Integer);
    procedure CopyFleet(r: Integer);
    procedure DisableFleet(r, f: Integer; b: Boolean);
    function FleetMaxProposed(r: Integer): Integer;
    function FleetMin(r: Integer): Integer;
    function FleetMax(r: Integer): Integer;
    procedure FillFleetList(r: Integer; f: Integer; L: TArrayList);
    procedure FillFleetListAll(r: Integer; L: TArrayList);
    function IsFinalRace(r: Integer): Boolean;
    function IsRacing(r: Integer): Boolean;
    function CountRacesSailed: Integer;

    procedure Calc; override;
    procedure PartialCalc(r: Integer);
    property EventRowCollection: TEventRowCollection read GetEventRowCollection;
    property EventBO: TEventBO read GetEventBO;
    property OnCalc: TNotifyEvent read FOnCalc write FOnCalc;
    property ShowPoints: Integer read GetShowPoints write SetShowPoints;
    property RaceCount: Integer read GetRaceCount;
    property RCount: Integer read GetRCount;
    property ColorMode: TColorMode read FColorMode write FColorMode;
    property FirstFinalRace: Integer read GetFirstFinalRace write SetFirstFinalRace;
    property IsInFinalPhase: Boolean read GetIsInFinalPhase;
  end;

  TEventBO = class(TBaseColBO)
  private
    fl: TCRList;
    FCurrentNode: TBaseNode;
    FCurrentRow: TBaseRowCollectionItem;
    FRelaxedInputMode: Boolean;
    FNameFieldCount: Integer;
    FNameFieldOrder: string;
    UndoAgent: TUndoAgent;
    FMobilRaceColIndex: Integer;
    procedure SetRelaxedInputMode(const Value: Boolean);
    function GetRaceCount: Integer;
    procedure EditNC(crgs: TBaseRowCollectionItem; var Value: string);
    procedure CheckOTime(cl: TEventRowCollection;
      cr: TEventRowCollectionItem; r: Integer; var Value: string);
    procedure CheckOTimeForFleet( fl: TCRList;
      cr: TEventRowCollectionItem; r: Integer; var Value: string);
    procedure CheckOTimeForAll(cl: TEventRowCollection;
      cr: TEventRowCollectionItem; r: Integer; var Value: string);
    function GetNameFieldCount: Integer;
    procedure SetNameFieldCount(const Value: Integer);
    procedure SetNameFieldOrder(const Value: string);
    function GetNameFieldID(Index: Integer): string;
    procedure InitColsActiveFull(StringGrid: TColGrid);
    procedure InitColsActiveMobil(StringGrid: TColGrid);
  protected
    function GetCurrentRow: TBaseRowCollectionItem; override;
    procedure SetCurrentRow(const Value: TBaseRowCollectionItem); override;
    function GetCurrentNode: TBaseNode; override;
    procedure SetCurrentNode(const Value: TBaseNode); override;
  public
    WantDiffCols: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure InitColsActive(StringGrid: TColGrid); override;
    procedure InitColsActiveLayout(StringGrid: TColGrid; aLayout: Integer); override;
    procedure EditBib(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditSNR(crgs: TBaseRowCollectionItem; var Value: string);
    procedure EditRaceValue(crgs: TBaseRowCollectionItem; var Value: string; const ColName: string);
    procedure EditOTime(crgs: TBaseRowCollectionItem; var Value: string; RaceIndex: Integer);
    procedure SetMobilRaceColumn(StringGrid: TColGrid; r: Integer);
    property CurrentRow: TBaseRowCollectionItem read GetCurrentRow write SetCurrentRow;
    property CurrentNode: TBaseNode read GetCurrentNode write SetCurrentNode;
    property RelaxedInputMode: Boolean read FRelaxedInputMode write SetRelaxedInputMode;
    property RaceCount: Integer read GetRaceCount;
    property NameFieldCount: Integer read GetNameFieldCount write SetNameFieldCount;
    property NameFieldOrder: string read FNameFieldOrder write SetNameFieldOrder;
  end;

implementation

uses
  RiggVar.BO.Def,
  RiggVar.BO.Validation;

const
  NumID_SNR = 1;
  NumID_Bib = 2;
  NumID_GPoints = 3;
  NumID_GRank = 4;
  NumID_GPosR = 5;
  NumID_PLZ = 6;
  NumID_Cup = 7;
  NumID_DPoints = 8;
  NumID_DRank = 9;

  NumID_DN = 10;
  NumID_NF1 = 11;
  NumID_NF2 = 12;
  NumID_NF3 = 13;
  NumID_NF4 = 14;
  NumID_NF5 = 15;
  NumID_NF6 = 16;

  NumID_TieRes = 17;

function NumID_Race (index: Integer): Integer;
begin
  result := 10000 + index * 1000;
end;
function RaceIndex (numID: Integer): Integer;
begin
  result := (numID - 10000) div 1000;
end;
function IsRaceNumID (numID: Integer): Boolean;
begin
  result := numID > 10000;
end;

{ TEventEntry }

constructor TEventEntry.Create(aRaceCount: Integer);
var
  i: Integer;
begin
  inherited Create;
  SetLength(Race, aRaceCount + 1);
  for i := 0 to RCount-1 do
    Race[i] := TEventRaceEntry.Create;
  GRace := Race[0];
end;

destructor TEventEntry.Destroy;
var
  i: Integer;
begin
  for i := 0 to RCount-1 do
    Race[i].Free;
  inherited;
end;

function TEventEntry.GetRCount: Integer;
begin
  result := Length(Race);
end;

{ TEventRowCollectionItem }

constructor TEventRowCollectionItem.Create(Collection: TBaseRowCollection);
var
  i: Integer;
  re: TEventRaceEntry;
begin
  inherited;
  SetLength(Race, RaceCount + 1);
  for i := 0 to RCount-1 do
  begin
    re := TEventRaceEntry.Create;
    re.FNode := ru;
    Race[i] := re;
  end;
  GRace := Race[0];
end;

destructor TEventRowCollectionItem.Destroy;
var
  i: Integer;
begin
  for i := 0 to RCount-1 do
    Race[i].Free;
  inherited;
end;

procedure TEventRowCollectionItem.Assign(Source: TPersistent);
var
  o: TEventRowCollectionItem;
  e: TEventEntry;
  i: Integer;
begin
  if Source is TEventRowCollectionItem then
  begin
    o := TEventRowCollectionItem(Source);
    SNR := o.SNR;
    Bib := o.Bib;
    for i := 0 to Length(Race)-1 do
      Race[i].Assign(o.Race[i]);
  end
  else if Source is TEventEntry then
  begin
    e := TEventEntry(Source);
    SNR := e.SNR;
    Bib := e.Bib;
    for i := 0 to Length(Race)-1 do
      Race[i].Assign(e.Race[i]);
  end
  else
    inherited Assign(Source);
end;

procedure TEventRowCollectionItem.SetModified(const Value: Boolean);
var
  rd: TBaseNode;
begin
  rd := GetBaseNode;
  if Assigned(rd) then
    rd.Modified := True;
end;

function TEventRowCollectionItem.GetBaseNode: TBaseNode;
var
  cl: TEventRowCollection;
begin
  cl := Collection as TEventRowCollection;
  result := cl.BaseNode;
end;

function TEventRowCollectionItem.GetEventNode: TEventNode;
var
  cl: TEventRowCollection;
begin
  result := nil;
  cl := Collection as TEventRowCollection;
  if cl.BaseNode is TEventNode then
    result := TEventNode(cl.BaseNode);
end;

function TEventRowCollectionItem.GetDPoints: string;
var
  d: double;
begin
  d := (Race[0].CTime - OPoints) / 100;
  result := FormatFloat('0.00', d);
end;

function TEventRowCollectionItem.GetDRank: Integer;
begin
  result := Race[0].Rank - ORank;
end;

function TEventRowCollectionItem.GetSDItem: TStammdatenRowCollectionItem;
begin
  result := ru.StammdatenRowCollection.FindKey(SNR);
end;

function TEventRowCollectionItem.GetFN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.FN
  else
    result := '';
end;

function TEventRowCollectionItem.GetLN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.LN
  else
    result := '';
end;

function TEventRowCollectionItem.GetSN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.SN
  else
    result := '';
end;

function TEventRowCollectionItem.GetNOC: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.NC
  else
    result := '';
end;

function TEventRowCollectionItem.GetGender: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.GR
  else
    result := '';
end;

function TEventRowCollectionItem.GetPB: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.PB
  else
    result := '';
end;

function TEventRowCollectionItem.GetDN: string;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.DN
  else
    result := '';
end;

function TEventRowCollectionItem.GetProps: TProps;
var
  sd: TStammdatenRowCollectionItem;
begin
  sd := GetSDItem;
  if Assigned(sd) then
    result := sd.Props
  else
    result := nil;
end;

function TEventRowCollectionItem.GetRaceValue(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Length(Race)) then
    result := Race[Index].RaceValue
  else
    result := '';
end;

procedure TEventRowCollectionItem.SetRaceValue(Index: Integer;
  const Value: string);
begin
  if (Index >= 0) and (Index < Length(Race)) then
    Race[Index].RaceValue := Value;
end;

function TEventRowCollectionItem.GetGPosR: Integer;
begin
  result := Race[0].PosR;
end;

function TEventRowCollectionItem.GetGRank: Integer;
begin
  result := Race[0].Rank;
end;

function TEventRowCollectionItem.GetGTime1: Integer;
begin
  result := Race[0].CTime1;
end;

function TEventRowCollectionItem.GetPLZ: Integer;
begin
  result := Race[0].PLZ;
end;

procedure TEventRowCollectionItem.ClearResult;
var
  r: Integer;
  ere: TEventRaceEntry;
begin
  inherited;
  for r := 1 to RaceCount do
  begin
    ere := Race[r];
    ere.Clear;
  end;
end;

procedure TEventRowCollectionItem.UpdateCellProp(cp: TBaseColProp;
  cellProp: TCellProp);
var
  NumID: Integer;
  r: Integer;
begin
  cellProp.Color := ColumnToColorDef(cp, cellProp.Color);
  NumID := cp.NumID;
  if ru.UseFleets then
  begin
    if IsRaceNumID(NumID) then
    begin
      r := RaceIndex(NumID);
      if ru.ColorMode = ColorMode_Error then
        cellProp.GroupColor := FleetColorBold(r, clFleetNone);
    end;
  end;
end;

function TEventRowCollectionItem.ColumnToColorDef(cp: TBaseColProp;
  aColor: TColor): TColor;
var
  ColorMode: TColorMode;
  NumID: Integer;
begin
  result := aColor;

  ColorMode := ru.ColorMode;
  if ColorMode = ColorMode_None then
    result := TColorRec.White
  else
  begin
    NumID := cp.NumID;
    if NumID = NumID_Bib then
      result := BibColor(aColor)
    else if NumID = NumID_SNR then
      result := SNRColor(aColor)
    else if IsRaceNumID(NumID) then
      result := RaceColor(NumID, aColor)
  end;
end;

function TEventRowCollectionItem.FleetColor(r: Integer; aColor: TColor): TColor;
var
  i: Integer;
begin
  i := Race[r].Fleet;
  case i of
    0: result := TColorRec.White;
    1: result := clFleetYellow;
    2: result := clFleetBlue;
    3: result := clFleetRed;
    4: result := clFleetGreen;
    else
      result := aColor;
  end;
end;

function TEventRowCollectionItem.FleetColorBold(r: Integer; aColor: TColor): TColor;
var
  i: Integer;
begin
  i := Race[r].Fleet;
  case i of
    0: result := TColorRec.Lime;
    1: result := TColorRec.Orange;
    2: result := TColorRec.Cornflowerblue;
    3: result := TColorRec.Crimson;
    4: result := TColorRec.YellowGreen;
    else
      result := aColor;
  end;
end;

function TEventRowCollectionItem.BibColor(aColor: TColor): TColor;
begin
  result := aColor;
  if error_Duplicate_Bib in EntryErrors then
    result := TColorRec.Aqua
  else if error_OutOfRange_Bib in EntryErrors then
    result := TColorRec.Aqua;
end;

function TEventRowCollectionItem.SNRColor(aColor: TColor): TColor;
begin
  result := aColor;
  if error_Duplicate_SNR in EntryErrors then
    result := TColorRec.Aqua
  else if error_OutOfRange_SNR in EntryErrors then
    result := TColorRec.Aqua;
end;

function TEventRowCollectionItem.RaceErrorColor(r: Integer; aColor: TColor): TColor;
begin
  result := aColor;
  if error_Duplicate_OTime in Race[r].FinishErrors then
    result := clHellRot
  else if error_Contiguous_OTime in Race[r].FinishErrors then
    result := TColorRec.Aqua
  else if error_OutOfRange_OTime_Max in Race[r].FinishErrors then
    result := TColorRec.Olive
  else if error_OutOfRange_OTime_Min in Race[r].FinishErrors then
    result := TColorRec.Olive;
end;

function TEventRowCollectionItem.RaceColor(NumID: Integer; aColor: TColor): TColor;
var
  r: Integer;
begin
  result := aColor;
  r := RaceIndex(NumID);
  if r > 0 then
  begin
    if not BO.IsRacing[r] then
      result := TColorRec.Lightgray
    else if ru.ColorMode = ColorMode_Error then
      result := RaceErrorColor(r, aColor)
    else if ru.ColorMode = ColorMode_Fleet then
      result := FleetColor(r, aColor);
  end
end;

function TEventRowCollectionItem.GetRaceCount: Integer;
begin
  result := BO.BOParams.RaceCount;
end;

function TEventRowCollectionItem.GetRCount: Integer;
begin
  result := Length(Race);
end;

function TEventRowCollectionItem.GetGPoints: string;
begin
  result := Race[0].DecimalPoints;
end;

{ TEventRowCollection }

function TEventRowCollection.GetItem(Index: Integer):
  TEventRowCollectionItem;
begin
  result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TEventRowCollectionItem(inherited GetItem(Index));
end;

procedure TEventRowCollection.SetItem(Index: Integer; const Value:
  TEventRowCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited SetItem(Index, Value);
end;

function TEventRowCollection.Add: TEventRowCollectionItem;
begin
  Result := TEventRowCollectionItem(inherited Add);
  Result.BaseID := Count;
  //Result.Bib := Count; //bei SKK
  //Result.ClearResult; //bei SKK
end;

procedure TEventRowCollection.ResetRace(r: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
  f: Integer;
  ere: TEventRaceEntry;
begin
  if (r > 0) and (r < RCount) then
  begin
    for i := 0 to Count - 1 do
    begin
      cr := self.Items[i];
      ere := cr.Race[r];
      f := ere.Fleet;
      ere.Clear;
      ere.Fleet := f;
    end;
    BaseNode.Modified := true;
  end;
end;

procedure TEventRowCollection.UpdateItem(e: TEventEntry);
var
  o: TEventRowCollectionItem;
begin
  o := FindKey(e.SNR);
  if not Assigned(o) then
    o := Add;
  if Assigned(o) then
    o.Assign(e);
end;

procedure TEventRowCollection.FillFleetList(FL: TBaseList; r: Integer; f: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
begin
  FL.Clear;
  if (r > 0) and (r < RCount) then
    for i := 0 to Count - 1 do
    begin
      cr := self.Items[i];
      if cr.Race[r].Fleet = f then
        FL.Add(cr);
    end;
end;

function TEventRowCollection.FindKey(SNR: Integer):
  TEventRowCollectionItem;
var
  i: Integer;
  o: TEventRowCollectionItem;
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

function TEventRowCollection.FleetCount(r: Integer): Integer;
var
  i: Integer;
  fc: Integer;
  temp: Integer;
begin
  fc := 0;
  for i := 0 to Count - 1 do
  begin
    temp := Items[i].Race[r].Fleet;
    if temp > fc then
      fc := temp;
  end;
  result := fc;
end;

procedure TEventRowCollection.Load;
begin
  //
end;

procedure TEventRowCollection.Save;
begin
  //
end;

function TEventRowCollection.GetHashString: string;
var
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  result := '';
  for i := 0 to Count-1 do
  begin
    cr := Self.Items[i];
    if i = 0 then
      result := IntToStr(cr.GRank)
    else
      result := result + '-' + IntToStr(cr.GRank);
  end;
end;

procedure TEventRowCollection.GetXML(SL: TStrings);
begin
  SL.Add('<e xmlns="http://riggvar.net/FR11.xsd">');
  SL.Add('');
  GetXMLSchema(SL);
  SL.Add('');
  GetXMLResult(SL);
  SL.Add('');
  GetXMLBackup(SL);
  SL.Add('');
  SL.Add('</e>');
end;

procedure TEventRowCollection.GetXMLResult(SL: TStrings);
var
  cr: TEventRowCollectionItem;
  i: Integer;
  s: string;
  r: Integer;
begin
  for i := 0 to Count-1 do
  begin
    cr := Self.Items[i];
    s := '<r Id="' + IntToStr(cr.BaseID) + '" ';
    s := s + 'Pos="' + IntToStr(cr.GRank) + '" ';
    s := s + 'Bib="' + IntToStr(cr.Bib) + '" ';
    s := s + 'SNR="' + IntToStr(cr.SNR) + '" ';
    s := s + 'SN="' + cr.DN + '" ';

    for r := 1 to RCount-1 do
    begin
      s := s + 'W' + IntToStr(r) + '="' + cr.RaceValue[r] + '" ';
    end;
    s := s + 'Pts="' + cr.GPoints + '" ';
    s := s + '/>';
    SL.Add(s);
  end;
end;

procedure TEventRowCollection.GetXMLSchema(SL: TStrings);
var
  r: Integer;
begin
  SL.Add('<xs:schema id="e" targetNamespace="http://riggvar.net/FR11.xsd"');
  SL.Add('xmlns:mstns="http://riggvar.net/FR11.xsd"');
  SL.Add('xmlns="http://riggvar.net/FR11.xsd"');
  SL.Add('xmlns:xs="http://www.w3.org/2001/XMLSchema"');
  SL.Add('xmlns:msdata="urn:schemas-microsoft-com:xml-msdata"');
  SL.Add('attributeFormDefault="qualified"');
  SL.Add('elementFormDefault="qualified">');
  SL.Add('');
  SL.Add('<xs:element name="e" msdata:IsDataSet="true" msdata:Locale="de-DE" msdata:EnforceConstraints="False">');
  SL.Add('<xs:complexType>');
  SL.Add('<xs:choice maxOccurs="unbounded">');
  SL.Add('');
  SL.Add('<xs:element name="r">');
  SL.Add('<xs:complexType>');
  SL.Add('<xs:attribute name="Id" form="unqualified" type="xs:positiveInteger" />');
  SL.Add('<xs:attribute name="Pos" form="unqualified" type="xs:positiveInteger" />');
  SL.Add('<xs:attribute name="Bib" form="unqualified" type="xs:positiveInteger" />');
  SL.Add('<xs:attribute name="SNR" form="unqualified" type="xs:positiveInteger" />');
  SL.Add('<xs:attribute name="SN" form="unqualified" type="xs:string" />');
  for r := 1 to RCount-1 do
  begin
  SL.Add('<xs:attribute name="W' + IntToStr(r) + '" form="unqualified" type="xs:string" />');
  end;
  SL.Add('<xs:attribute name="Pts" form="unqualified" type="xs:double" />');
  SL.Add('</xs:complexType>');
  SL.Add('</xs:element>');
  SL.Add('');
	SL.Add('<xs:element name="b">');
	SL.Add('<xs:complexType>');
	SL.Add('<xs:attribute name="I" form="unqualified" type="xs:positiveInteger" />');
	SL.Add('<xs:attribute name="N" form="unqualified" type="xs:string" />');
	SL.Add('<xs:attribute name="V" form="unqualified" type="xs:string" />');
	SL.Add('</xs:complexType>');
	SL.Add('</xs:element>');
  SL.Add('');
  SL.Add('</xs:choice>');
  SL.Add('</xs:complexType>');
  SL.Add('</xs:element>');
  SL.Add('</xs:schema>');
end;

procedure TEventRowCollection.GetXMLBackup(SL: TStrings);
var
  tempSL: TStringList;
  i, j: Integer;
  s: string;
  sName, sValue: string;
begin
  tempSL := TStringList.Create;
  try
    BO.BackupToSL(tempSL);

    j := 0;
    for i := 0 to tempSL.Count-1 do
    begin
      s := tempSL[i];

      if (s = '') or (s[1] = '<') then
      begin
        sName := '';
        sValue := '';
      end
      else if (s[1] = '#') or (Copy(s, 1, 2) = '//') then
      begin
        sName := s;
        sValue := '';
      end
      else
      begin
        sName := Trim(tempSL.Names[i]);
        sValue := Trim(tempSL.ValueFromIndex[i]);
      end;

      Inc(j);
      s := '<b I="' + IntToStr(j) + '" N="' + sName + '" V="' + sValue + '" />';
      //s := StringReplace(s, cTokenFleetRace + '.' + cTokenDivision + '.', '', []);
      tempSL[i] := s;
    end;

    for i := 0 to tempSL.Count-1 do
    begin
      SL.Add(tempSL[i]);
    end;
  finally
    tempSL.Free;
  end;
end;

function TEventRowCollection.GetRaceCount: Integer;
begin
  if Count > 0 then
    result := Items[0].RaceCount
  else if BaseNode is TEventNode then
    result := TEventNode(BaseNode).RaceCount
  else
    result := -1;
end;

function TEventRowCollection.GetRCount: Integer;
begin
  if Count > 0 then
    result := Items[0].RCount
  else if BaseNode is TEventNode then
    result := TEventNode(BaseNode).RaceCount + 1
  else
    result := -1;
end;

{ TEventColProp }

function TEventColProp.GetRaceCount: Integer;
begin
  if Assigned(BO) then
    result := BO.BOParams.RaceCount
  else
    result := -1;
end;

procedure TEventColProp.GetSortKeyRace(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TEventRowCollectionItem;
  i: Integer;
  ere: TEventRaceEntry;
begin
  //zur Zeit nicht verwendet. Problem mit O Points (bei Bonus Point System)
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    i := StrToIntDef(Copy(ColName, 6, Length(ColName)), -1);
    if i > 0 then // and i <= RaceCount
    begin
      ere := cr.Race[i];
      if cr.Race[i].OTime > 0 then
        Value := IntToStr(ere.OTime + ere.Fleet * 2000) //.CTime
      else
        Value := IntToStr(999 + cr.BaseID + ere.Fleet * 2000);
    end;
  end;
end;

procedure TEventColProp.GetSortKeyPoints(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
  	if (cr.ru.ShowPoints = Layout_Finish) then
      GetSortKeyRace(crgs, Value, ColName)
    else //default: if cr.ru.ShowPoints = Layout_Points then
    begin
      i := StrToIntDef(Copy(ColName, 6, Length(ColName)), -1);
      if i > 0 then // and i <= RaceCount
      begin
        if cr.Race[i].FCTime >= 0 then
          Value := IntToStr(cr.Race[i].FCTime)
        else
          Value := IntToStr(99 + cr.BaseID);
      end;
    end
  end;
end;

procedure TEventColProp.GetSortKeyGPosR(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TEventRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    Value := IntToStr(cr.GPosR);
  end;
end;

procedure TEventColProp.GetSortKeyDRank(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TEventRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    Value := IntToStr(cr.DRank);
  end;
end;

procedure TEventColProp.GetSortKeyDPoints(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TEventRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    Value := IntToStr(cr.Race[0].CTime - cr.OPoints);
  end;
end;

procedure TEventColProp.GetTextDefault(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  Value := '';
  if crgs is TEventRowCollectionItem then
    cr := TEventRowCollectionItem(crgs)
  else
    exit;

  inherited;

  if NumID = NumID_SNR then
    Value := IntToStr(cr.SNR)

  else if NumID = NumID_Bib then
    Value := IntToStr(cr.Bib)

  else if NumID = NumID_DN then
    Value := cr.DN

  else if NumID = NumID_NF4 then
    Value := cr.NC

  else if NumID = NumID_GPoints then
    Value := cr.GPoints

  else if NumID = NumID_GRank then
    Value := IntToStr(cr.GRank)

  else if NumID = NumID_GPosR then
    Value := IntToStr(cr.GPosR)

  else if NumID = NumID_PLZ then
    Value := IntToStr(cr.PLZ + 1)

  else if NumID = NumID_Cup then
    Value := FormatFloat('0.00', cr.RA)

  else if NumID = NumID_DPoints then
    Value := cr.DPoints

  else if NumID = NumID_DRank then
    Value := IntToStr(cr.DRank)

  { Race[0] wird nicht angezeigt }
  else if IsRaceNumID(NumID) then
  begin
    i := RaceIndex(NumID);
    Value := cr.Race[i].RaceValue;
  end

  else if NumID = NumID_NF1 then
    Value := cr.FN

  else if NumID = NumID_NF2 then
    Value := cr.LN

  else if NumID = NumID_NF3 then
    Value := cr.SN

  else if NumID = NumID_NF5 then
    Value := cr.GR

  else if NumID = NumID_NF6 then
    Value := cr.PB

  else if NumID = NumID_TieRes then
    Value := IntToStr(cr.Race[0].TiebreakIncrement)
end;

function TEventColProp.GetFieldCaptionDef(
  cl: TStammdatenRowCollection; Index: Integer; def: string): string;
begin
  result := def;
  if (cl <> nil) then
    result := cl.GetFieldCaption(Index);
end;

procedure TEventColProp.InitColsAvail;
var
  cp: TBaseColProp;
  ColsAvail: TBaseColProps;
  i: Integer;
  rc: Integer;
  scl: TStammdatenRowCollection;
begin
  if Collection is TBaseColProps then
    ColsAvail := TBaseColProps(Collection)
  else
    exit;

  inherited;

  ColsAvail.UseCustomColCaptions := True;
  scl := BO.StammdatenNode.StammdatenRowCollection;

  { SNR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SNR';
  cp.Caption := 'SNR';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_SNR;

  { Bib }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Bib';
  cp.Caption := 'Bib';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.NumID := NumID_Bib;

  { FN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_FN';
  cp.Caption := self.GetFieldCaptionDef(scl, 1, N_FN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NF1;

  { LN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_LN';
  cp.Caption := self.GetFieldCaptionDef(scl, 2, N_LN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NF2;

  { SN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_SN';
  cp.Caption := self.GetFieldCaptionDef(scl, 3, N_SN);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NF3;

  { NC }
  cp := ColsAvail.Add;
  cp.NameID := 'col_NC';
  cp.Caption := self.GetFieldCaptionDef(scl, 4, N_NC);
  cp.Width := 70;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NF4;

  { GR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GR';
  cp.Caption := self.GetFieldCaptionDef(scl, 5, N_GR);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NF5;

  { PB }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PB';
  cp.Caption := self.GetFieldCaptionDef(scl, 6, N_PB);
  cp.Width := 80;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_NF6;

  { DN }
  cp := ColsAvail.Add;
  cp.NameID := 'col_DN';
  cp.Caption := 'DN';
  cp.Width := 180;
  cp.Sortable := True;
  cp.Alignment := taLeftJustify;
  cp.ColType := colTypeString;
  cp.NumID := NumID_DN;

  { Race[0] wird nicht angezeigt, nur Race[1]..Race[RCount-1]
    bzw. Race[1]..Race[RaceCount]}
  rc := GetRaceCount;
  for i := 1 to rc do
  begin
    { Ri }
    cp := ColsAvail.Add;
    cp.NameID := 'col_R' + IntToStr(i);
    cp.Caption := 'R' + IntToStr(i);
    cp.Width := 60;
    cp.Sortable := True;
    cp.Alignment := taRightJustify;
    cp.ColType := colTypeRank;
    cp.OnGetSortKey2 := GetSortKeyPoints;
    cp.NumID := NumID_Race(i) + 1; //10000 + i * 1000 + 1;
  end;

  { TieRes }
  cp := ColsAvail.Add;
  cp.NameID := 'col_TieRes';
  cp.Caption := 'TR';
  cp.Width := 35;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_TieRes;

  { GPoints }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GPoints';
  cp.Caption := 'GPoints';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.OnGetSortKey2 := GetSortKeyGPosR;
  cp.NumID := NumID_GPoints;

  { GRank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GRank';
  cp.Caption := 'GRank';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_GRank;

  { GPosR }
  cp := ColsAvail.Add;
  cp.NameID := 'col_GPosR';
  cp.Caption := 'GPosR';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_GPosR;

  { PLZ }
  cp := ColsAvail.Add;
  cp.NameID := 'col_PLZ';
  cp.Caption := 'PLZ';
  cp.Width := 30;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_PLZ;

  { Cup }
  cp := ColsAvail.Add;
  cp.NameID := 'col_Cup';
  cp.Caption := 'Cup';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.NumID := NumID_Cup;

  { DPoints }
  cp := ColsAvail.Add;
  cp.NameID := 'col_DPoints';
  cp.Caption := 'DP';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.OnGetSortKey2 := GetSortKeyDPoints;
  cp.NumID := NumID_DPoints;

  { DRank }
  cp := ColsAvail.Add;
  cp.NameID := 'col_DRank';
  cp.Caption := 'DR';
  cp.Width := 50;
  cp.Sortable := True;
  cp.Alignment := taRightJustify;
  cp.ColType := colTypeRank;
  cp.OnGetSortKey2 := GetSortKeyDRank;
  cp.NumID := NumID_DRank;

end;

{ TEventNode }

constructor TEventNode.Create(AOwner: TBaseColBO);
begin
  inherited Create(AOwner);
  FFirstFinalRace := 20;
  TargetFleetSize := 8;
  FShowPoints := Layout_Finish;
  BaseRowCollection := TEventRowCollection.Create(Self,
    TEventRowCollectionItem);
  ErrorList := TOTimeErrorList.Create;
  ColorMode := ColorMode_Error;
end;

destructor TEventNode.Destroy;
begin
  BaseRowCollection.Free;
  ErrorList.Free;
  inherited;
end;

function TEventNode.GetEventRowCollection: TEventRowCollection;
begin
  result := BaseRowCollection as TEventRowCollection;
end;

function TEventNode.GetFirstFinalRace: Integer;
begin
  if FFirstFinalRace = 0 then
    result := RCount
  else
    result := FFirstFinalRace;
end;

function TEventNode.GetIsInFinalPhase: Boolean;
begin
  result := RaceCount >= FirstFinalRace;
end;

procedure TEventNode.SetFirstFinalRace(const Value: Integer);
begin
  FFirstFinalRace := Value;
end;

function TEventNode.GetBaseColPropClass: TBaseColPropClass;
begin
  result := TEventColProp;
end;

procedure TEventNode.Load;
var
  o: TEventRowCollectionItem;
begin
  EventRowCollection.Clear;

  o := EventRowCollection.Add;
  o.SNR := 1001;
  o.Bib := 1;
  o.BaseID := 1;

  o := EventRowCollection.Add;
  o.SNR := 1002;
  o.Bib := 2;
  o.BaseID := 2;

  o := EventRowCollection.Add;
  o.SNR := 1003;
  o.Bib := 3;
  o.BaseID := 3;
end;

procedure TEventNode.Init(RowCount: Integer);
var
  o: TEventRowCollectionItem;
  i: Integer;
begin
  BaseRowCollection.Clear;

  for i := 0 to RowCount - 1 do
  begin
    o := EventRowCollection.Add;
    o.BaseID := i + 1;
    o.SNR := 1000 + i + 1;
    o.Bib := i + 1;
  end;
end;

procedure TEventNode.CopyFleet(r: Integer);
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  UseFleets := True;
  cl := EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    if (r > 1) and (r < RCount) then
    begin
      cr.Race[r].Fleet := cr.Race[r-1].Fleet;
    end;
  end;
end;

procedure TEventNode.DisableFleet(r, f: Integer; b: Boolean);
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  if (r > 0) and (r < RCount)  and UseFleets then
  begin
    cl := EventRowCollection;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      if cr.Race[r].Fleet = f then
        cr.Race[r].IsRacing := b;
    end;
  end;
end;

function TEventNode.CountRacesSailed: Integer;
var
  r: Integer;
begin
  { calculate the actual count of sailed races }
  result := 0;
  for r := 1 to RaceCount do
    if BO.IsRacing[r] then
      Inc(result);
end;

function TEventNode.IsFinalRace(r: Integer): Boolean;
begin
  result := (FirstFinalRace > 0) and (r >= FirstFinalRace);
end;

function TEventNode.IsRacing(r: Integer): Boolean;
begin
  result := BO.IsRacing[r];
end;

function TEventNode.FleetMaxProposed(r: Integer): Integer;
var
  cl: TEventRowCollection;
  fc: Integer;
begin
  fc := 0;
  if (r > 0) and (r < RCount)
    and (TargetFleetSize > 0)
  then
  begin
    cl := EventRowCollection;
    fc := Cl.Count div TargetFleetSize;
    if (TargetFleetSize > 0) and (cl.Count > 0) then
    begin
      while TargetFleetSize * fc < cl.Count do
        Inc(fc);
    end;
  end;
  result := fc;
end;

function TEventNode.FleetMax(r: Integer): Integer;
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  result := 0;
  if (r > 0) and (r < RCount) then
  begin
    cl := EventRowCollection;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      if cr.Race[r].Fleet > result then
        result := cr.Race[r].Fleet;
    end;
  end;
end;

function TEventNode.FleetMin(r: Integer): Integer;
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  result := 1;
  if (r > 0) and (r < RCount) then
  begin
    cl := EventRowCollection;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      if cr.Race[r].Fleet < result then
        result := cr.Race[r].Fleet;
    end;
  end;
end;

procedure TEventNode.FillFleetList(r: Integer; f: Integer; L: TArrayList);
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  if (r > 0) and (r < RCount) then
  begin
    cl := EventRowCollection;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      if cr.Race[r].Fleet = f then
        L.Add(cr);
    end;
  end;
end;

procedure TEventNode.FillFleetListAll(r: Integer; L: TArrayList);
var
  i: Integer;
  cl: TEventRowCollection;
begin
  if (r > 0) and (r < RCount) then
  begin
    cl := EventRowCollection;
    for i := 0 to cl.Count-1 do
      L.Add(cl.Items[i]);
  end;
end;

function TEventNode.FindBib(b: Integer): TEventRowCollectionItem;
var
  i: Integer;
  cr: TEventRowCollectionItem;
begin
  result := nil;
  for i := 0 to EventRowCollection.Count - 1 do
  begin
    cr := EventRowCollection.Items[i];
    if cr.Bib = b then
    begin
      result := cr;
      break;
    end;
  end;
end;

procedure TEventNode.PartialCalc(r: Integer);
begin
  PartialCalcLastRace := r;
  BO.CalcEV.Calc(Self);
  Modified := False;
  if Assigned(OnCalc) then
    OnCalc(Self);
  ErrorList.CheckAll(self);
  PartialCalcLastRace := 0;
end;

procedure TEventNode.InitFleet(r: Integer);
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  fc: Integer;
  f: Integer;
  c: Integer;
  upPhase: Boolean;
begin
  UseFleets := True;
  fc := FleetMaxProposed(r);

  cl := EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    c := i mod fc;
    upPhase := not Odd(i div fc);

    if upPhase then
      f := c + 1
    else
      f := fc - c;

    if r = 1 then
    begin
      cr.Race[r].Fleet := f;
    end
    else if (r > 1) and (r < RCount) then
    begin
      cr := cl.Items[cr.PLZ];
      cr.Race[r].Fleet := f;
    end;
  end;
end;

procedure TEventNode.InitFleetByFinishHack(r: Integer);
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  FinishPosition, j: Integer;
  fc, f: Integer;
  ere: TEventRaceEntry;
begin
  fc := FleetMaxProposed(r);
  if (r > 0) and (r < RCount)
    and (TargetFleetSize > 0)
    and (fc > 0)
  then
  begin
    UseFleets := True;
    cl := EventRowCollection;
    { clear fleet assignment }
    for j := 0 to cl.Count-1 do
    begin
      cr := cl.Items[j];
      ere := cr.Race[r];
      ere.Fleet := 0;
    end;
    { gererate new from existing finish position info }
    for FinishPosition := 1 to TargetFleetSize do
    begin
      f := 1;
      for j := 0 to cl.Count-1 do
      begin
        cr := cl.Items[j];
        ere := cr.Race[r];
        if (ere.OTime = FinishPosition) and (ere.Fleet = 0) then
        begin
          ere.Fleet := f;
          Inc(f);
        end;
        if f = fc + 2 then
          break;
      end;
    end;
  end;
end;

procedure TEventNode.Calc;
begin
  BO.CalcEV.Calc(Self);
  Modified := False;
  if Assigned(OnCalc) then
    OnCalc(Self);
  ErrorList.CheckAll(self);
  if CalcCounter = 0 then
    UpdateDiff;
  Inc(CalcCounter);
end;

procedure TEventNode.UpdateDiff;
var
  i: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  cl := EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    cr.OPoints := cr.Race[0].CTime;
    cr.ORank := cr.Race[0].Rank;
  end;
end;

function TEventNode.GetEventBO: TEventBO;
begin
  result := nil;
  if Assigned(BaseColBO) then
    result := BaseColBO as TEventBO;
end;

function TEventNode.GetShowPoints: Integer;
begin
  if WebLayout > 0 then
    result := WebLayout
  else
    result := FShowPoints;
end;

procedure TEventNode.GoBackToRace(r: Integer);
var
  i, j: Integer;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
begin
  cl := EventRowCollection;
  cl.ResetRace(r);
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    for j := r + 1 to RaceCount do
    begin
{$ifdef RacePart}
      BO.RNode[j].RaceRowCollection.ClearResult;
{$endif}
      cr.Race[j].Clear;
    end;
  end;
end;

procedure TEventNode.SetShowPoints(const Value: Integer);
begin
  FShowPoints := Value;
end;

function TEventNode.GetRaceCount: Integer;
begin
  if Assigned(BO) then
    result := BO.BOParams.RaceCount
  else
    result := -1;
end;

function TEventNode.GetRCount: Integer;
begin
  result := RaceCount + 1;
end;

{ TEventBO }

procedure TEventBO.InitColsActive(StringGrid: TColGrid);
begin
  InitColsActiveLayout(StringGrid, 0);
end;

procedure TEventBO.InitColsActiveLayout(StringGrid: TColGrid;
  aLayout: Integer);
begin
  if aLayout = 1 then
    InitColsActiveMobil(StringGrid)
  else
    InitColsActiveFull(StringGrid);
end;

procedure TEventBO.InitColsActiveFull(StringGrid: TColGrid);
var
  cp: TBaseColProp;
  i: Integer;
  rc: Integer;
  s: string;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    cp := AddColumn('col_SNR');
    cp.OnFinishEdit := EditSNR;
    cp.ReadOnly := False;

    cp := AddColumn('col_Bib');
    cp.OnFinishEdit := EditBib;
    cp.ReadOnly := False;

    if (WantDiffCols) then
    begin
      AddColumn('col_DPoints');
      AddColumn('col_DRank');
    end;

    for i := 1 to NameFieldCount do
    begin
      s := GetNameFieldID(i);
      if s <> '' then
      begin
        cp := AddColumn(s);
        if s = 'col_NC' then
        begin
          cp.OnFinishEdit := EditNC;
          cp.ReadOnly := False;
        end;
      end;
    end;

    rc := RaceCount;
    for i := 1 to rc do
    begin
      cp := AddColumn('col_R' + IntToStr(i));
      cp.OnFinishEdit2 := EditRaceValue;
      cp.ReadOnly := False;
    end;

    AddColumn('col_GPoints');
    AddColumn('col_GRank');
    if BO.EventProps.ShowPosRColumn then
    begin
      AddColumn('col_GPosR');
      AddColumn('col_TieRes');
    end;
    if BO.EventProps.ShowPLZColumn then
      AddColumn('col_PLZ');
    if BO.EventProps.ShowCupColumn then
      AddColumn('col_Cup');
  end;
end;

procedure TEventBO.InitColsActiveMobil(StringGrid: TColGrid);
var
  cp: TBaseColProp;
begin
  with StringGrid do
  begin
    ColsActive.Clear;
    AddColumn('col_BaseID');

    AddColumn('col_SNR');
    //cp.OnFinishEdit := EditSNR;
    //cp.ReadOnly := False;

    AddColumn('col_Bib');
    //cp.OnFinishEdit := EditBib;
    //cp.ReadOnly := False;

    cp := AddColumn('col_R1');
    if Assigned(cp) then
    begin
      cp.OnFinishEdit2 := EditRaceValue;
      cp.ReadOnly := False;
      FMobilRaceColIndex := cp.Index;
    end;

    AddColumn('col_GRank');
    AddColumn('col_GPoints');
  end;
end;

procedure TEventBO.SetMobilRaceColumn(StringGrid: TColGrid; r: Integer);
var
  cp: TBaseColProp;
  cp2: TBaseColProp;
begin
  cp := StringGrid.ColsActive[FMobilRaceColIndex];
  if Assigned(cp) then
  begin
    cp2 := StringGrid.ColsAvail.ByName['col_R' + IntToStr(r)];
    if Assigned(cp2) then
    begin
      cp.Assign(cp2);
      cp.ReadOnly := False;
      cp.OnFinishEdit2 := EditRaceValue;
    end;
  end;
end;

function TEventBO.GetNameFieldID(Index: Integer): string;
var
  c: char;
begin
  result := '';

  if (Index < 1) or (Index > 6) then
    Exit;

  if Index <= Length(NameFieldOrder) then
    c := NameFieldOrder[Index]
  else
    c := Chr(Ord('0') + Index);

  case c of
    '0': result := 'col_DN';
    '1': result := 'col_FN';
    '2': result := 'col_LN';
    '3': result := 'col_SN';
    '4': result := 'col_NC';
    '5': result := 'col_GR';
    '6': result := 'col_PB';
  end;
end;

function TEventBO.GetCurrentRow: TBaseRowCollectionItem;
begin
  result := FCurrentRow;
end;

function TEventBO.GetNameFieldCount: Integer;
begin
  result := FNameFieldCount;
end;

procedure TEventBO.SetNameFieldCount(const Value: Integer);
begin
  if (Value >=0) and (Value <= 7) then
    FNameFieldCount := Value;
end;

procedure TEventBO.SetNameFieldOrder(const Value: string);
begin
  FNameFieldOrder := Value;
end;

function TEventBO.GetCurrentNode: TBaseNode;
begin
  result := FCurrentNode;
end;

procedure TEventBO.SetCurrentRow(const Value:
  TBaseRowCollectionItem);
begin
  if Value = nil then
    FCurrentRow := nil
  else
    FCurrentRow := Value;
end;

procedure TEventBO.SetCurrentNode(const Value: TBaseNode);
begin
  FCurrentNode := Value;
end;

procedure TEventBO.EditSNR(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TEventRowCollectionItem;
  oldSNR: Integer;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);

    oldSNR := cr.SNR;
    cr.SNR := StrToIntDef(Value, cr.SNR);
    Value := IntToStr(cr.SNR);
    BO.SNR[cr.Index] := cr.SNR;

    if (oldSNR <> cr.SNR) then
    begin
      UndoAgent.IsUndo := True;
      UndoAgent.MsgTree.Division.Race1.Startlist.Pos[cr.BaseID].SNR(IntToStr(oldSNR));
      UndoAgent.MsgTree.Division.Race1.Startlist.Pos[cr.BaseID].SNR(IntToStr(cr.SNR));
    end;
  end;
end;

constructor TEventBO.Create;
begin
  inherited;
  fl := TCRList.Create(false);
  FNameFieldCount := 2;
  FNameFieldOrder := '041256';
  UndoAgent := BO.UndoAgent;
end;

destructor TEventBO.Destroy;
begin
  fl.Free;
  inherited;
end;

procedure TEventBO.EditBib(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TEventRowCollectionItem;
  oldBib: Integer;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);

    oldBib := cr.Bib;
    cr.Bib := StrToIntDef(Value, cr.Bib);
    Value := IntToStr(cr.Bib);
    BO.Bib[cr.Index] := cr.Bib;

    if (oldBib <> cr.Bib) then
    begin
      UndoAgent.IsUndo := True;
      UndoAgent.MsgTree.Division.Race1.Startlist.Pos[cr.BaseID].Bib(IntToStr(oldBib));
      UndoAgent.MsgTree.Division.Race1.Startlist.Pos[cr.BaseID].Bib(IntToStr(cr.Bib));
    end;
  end;
end;

procedure TEventBO.EditNC(crgs: TBaseRowCollectionItem; var
  Value: string);
var
  cr: TEventRowCollectionItem;
  cl: TStammdatenRowCollection;
  crs: TStammdatenRowCollectionItem;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    cl := BO.StammdatenNode.StammdatenRowCollection;
    crs := cl.FindKey(cr.SNR);
    BO.StammdatenBO.EditNC(crs, Value);
  end;
end;

procedure TEventBO.EditRaceValue(crgs: TBaseRowCollectionItem;
  var Value: string; const ColName: string);
var
  cr: TEventRowCollectionItem;
  i: Integer;
  oldQU, newQU: Integer;
  oldQUString, newQUString: string;
  undoValue, redoValue: string;
  oldIsRacing: Boolean;
  re: TEventRaceEntry;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    i := StrToIntDef(Copy(ColName, 6, Length(ColName)), -1);
    if (i < 1) or (i > RaceCount) then
      Exit;

    if Value = '$' then
    begin
      oldIsRacing := BO.IsRacing[i];
      BO.IsRacing[i] := not oldIsRacing;
      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Race[i].IsRacing(BoolStr[oldIsRacing]); //undoMsg
        UndoAgent.MsgTree.Division.Race[i].IsRacing(BoolStr[not oldIsRacing]); //redoMsg
        UndoAgent.UndoFlag := False;
      end;
    end

    else if StrToIntDef(Value, -1) > -1 then
    begin
      EditOTime(crgs, Value, i);
    end

    { Fleet Assignment, easy edit }
    else if (Length(Value) = 1)
      and CharInSet(Value[1], ['y', 'b', 'r', 'g', 'm'])
    then
    begin
      re := cr.Race[i];
      case Value[1] of
        'y': re.Fleet := 1; //yellow
        'b': re.Fleet := 2; //blue
        'r': re.Fleet := 3; //red
        'g': re.Fleet := 4; //green
        'm': re.Fleet := 0; //medal
      end;
    end

    { Fleet Assignment, general method }
    else if (Length(Value) > 1) and (Value[1] = 'F') then
    begin
      re := cr.Race[i];
      re.RaceValue := Value; //do not broadcast Fleet Assignments
//      cr.Modified := True;
//      Value := re.RaceValue;
    end

    else if Value = 'x' then
    begin
      cr.Race[i].IsRacing := False;
      //cr.Modified := True; //use CalcBtn
    end

    else if Value = '-x' then
    begin
      cr.Race[i].IsRacing := True;
      //cr.Modified := True; /use CalcBtn
    end

    else
    begin
      re := cr.Race[i];
      oldQU := re.QU;
      oldQUString := re.Penalty.ToString;
      undoValue := re.Penalty.Invert(Value);
      redoValue := Value;
      re.RaceValue := Value;

      Value := re.RaceValue;
      cr.Modified := True;

      newQU := re.QU;
      newQUString := re.Penalty.ToString;

      if (oldQU <> newQU) or (oldQUString <> newQUString) then
      begin
        //BO.QU[i, cr.Index] := newQU;
        BO.Penalty[i, cr.Index] := re.Penalty;

        if not UndoAgent.UndoFlag then
        begin
          UndoAgent.IsUndo := True;
          UndoAgent.MsgTree.Division.Race[i].Bib[cr.Bib].RV(undoValue);
          UndoAgent.MsgTree.Division.Race[i].Bib[cr.Bib].RV(redoValue);
          UndoAgent.UndoFlag := False;
        end;
      end;
    end;
  end;
end;

procedure TEventBO.EditOTime(crgs: TBaseRowCollectionItem; var
  Value: string; RaceIndex: Integer);
var
  cr: TEventRowCollectionItem;
  cl: TEventRowCollection;
  oldRank: Integer;
  newRank: Integer;
  re: TEventRaceEntry;
begin
  if (crgs <> nil) and (crgs is TEventRowCollectionItem) then
  begin
    cr := TEventRowCollectionItem(crgs);
    cl := cr.ru.EventRowCollection;
  end
  else
    exit;

  re := cr.Race[RaceIndex];

  { mode a: direkt input, minimal restriction }
  if RelaxedInputMode then
  begin
    oldRank := re.OTime;
    newRank := StrToIntDef(Value, oldRank);
    if (newRank >= 0) and (newRank <= cl.Count) and (newRank <> oldRank) then
    begin
      re.OTime := newRank;
{$ifdef RacePart}
      BO.RNode[RaceIndex].RaceRowCollection.Items[cr.Index].MRank := newRank;
{$endif}
      cr.Modified := True;

      if not UndoAgent.UndoFlag then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Race[RaceIndex].Bib[cr.Bib].RV(IntToStr(oldRank));
        UndoAgent.MsgTree.Division.Race[RaceIndex].Bib[cr.Bib].RV(IntToStr(newRank));
      end;
      UndoAgent.UndoFlag := False;
    end;
    Value := IntToStr(re.OTime);
  end

  { mode b: maintain contiguous range from 1 to maxrank }
  else
  begin
    oldRank := re.OTime;
    CheckOTime(cl, cr, RaceIndex, Value);
    newRank := re.OTime;
    if oldRank <> newRank then
    begin
{$ifdef RacePart}
      BO.CopyOTimes(RaceIndex);
{$endif}
      cr.Modified := True;

      if (not UndoAgent.UndoFlag) then
      begin
        UndoAgent.IsUndo := True;
        UndoAgent.MsgTree.Division.Race[RaceIndex].Bib[cr.Bib].RV(IntToStr(oldRank));
        UndoAgent.MsgTree.Division.Race[RaceIndex].Bib[cr.Bib].RV(IntToStr(newRank));
        UndoAgent.UndoFlag := False;
      end;
    end;
  end;
end;

procedure TEventBO.CheckOTime(
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  r: Integer;
  var Value: string);
var
  f: Integer;
begin
  if BO.EventNode.UseFleets then
  begin
    f := cr.Race[r].Fleet;
    cl.FillFleetList(fl, r, f);
    CheckOTimeForFleet(fl, cr, r, Value);
    fl.Clear;
  end
  else
    CheckOTimeForAll(cl, cr, r, Value);
end;

procedure TEventBO.CheckOTimeForAll(
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  r: Integer;
  var Value: string);
var
  oldRank: Integer;
  newRank: Integer;
  maxRank: Integer;
  temp: Integer;
  i: Integer;
  cr1: TEventRowCollectionItem;
  re: TEventRaceEntry;
  re1: TEventRaceEntry;
begin
  re := cr.Race[r];

  oldRank := re.OTime;
  newRank := StrToIntDef(Value, oldRank);
  maxRank := 0;
  for i := 0 to cl.Count-1 do
  begin
    cr1 := cl.Items[i];
    re1 := cr1.Race[r];
    if cr = cr1 then
      Continue
    else if re1.OTime > 0 then
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
    Value := IntToStr(re.OTime)
  else
  begin
    for i := 0 to cl.Count-1 do
    begin
      cr1 := cl.Items[i];
      re1 := cr1.Race[r];
      if cr1 = cr then
        Continue;
      temp := re1.OTime;
      { remove }
      if (oldRank > 0) and (oldRank < temp) then
        re1.OTime := temp - 1;
      { insert }
      if (newRank > 0) and (newRank <= re1.OTime) then
        re1.OTime := re1.OTime + 1;
    end;
    re.OTime := newRank;
    Value := IntToStr(re.OTime);
  end;
end;

procedure TEventBO.CheckOTimeForFleet(
  fl: TCRList;
  cr: TEventRowCollectionItem;
  r: Integer;
  var Value: string);
var
  oldRank: Integer;
  newRank: Integer;
  maxRank: Integer;
  temp: Integer;
  i: Integer;
  cr1: TEventRowCollectionItem;
  re: TEventRaceEntry;
  re1: TEventRaceEntry;
begin
  re := cr.Race[r];

  oldRank := re.OTime;
  newRank := StrToIntDef(Value, oldRank);
  maxRank := 0;
  for i := 0 to fl.Count-1 do
  begin
    cr1 := fl.CR[i];
    re1 := cr1.Race[r];
    if cr = cr1 then
      Continue
    else if re1.OTime > 0 then
      Inc(maxRank);
  end;

  { limit new value }
  if newRank < 0 then
    newRank := 0;
  if newRank > maxRank + 1 then
    newRank := maxRank + 1;
  if newRank > fl.Count then
    newRank := fl.Count;

  if oldRank = newRank then
    Value := IntToStr(re.OTime)
  else
  begin
    for i := 0 to fl.Count-1 do
    begin
      cr1 := fl.CR[i];
      re1 := cr1.Race[r];
      if cr1 = cr then
        Continue;
      temp := re1.OTime;
      { remove }
      if (oldRank > 0) and (oldRank < temp) then
        re1.OTime := temp - 1;
      { insert }
      if (newRank > 0) and (newRank <= re1.OTime) then
        re1.OTime := re1.OTime + 1;
    end;
    re.OTime := newRank;
    Value := IntToStr(re.OTime);
  end;
end;

procedure TEventBO.SetRelaxedInputMode(const Value: Boolean);
var
  ev: TEventNode;
  oldValue: Boolean;
begin
  oldValue := FRelaxedInputMode;
  if Value then
    FRelaxedInputMode := True
  else
  begin
    ev := BO.EventNode;
    if ev.ErrorList.IsPreconditionForStrictInputMode(ev) then
      FRelaxedInputMode := False; //strict mode on
  end;
  if (UndoAgent.UndoFlag = false) and (oldValue <> FRelaxedInputMode) then
  begin
    if oldValue then
    begin
      UndoAgent.undoMsg := 'EP.InputMode = Relaxed';
      UndoAgent.redoMsg := 'EP.InputMode = Strict';
    end
    else
    begin
      UndoAgent.undoMsg := 'EP.InputMode = Strict';
      UndoAgent.redoMsg := 'EP.InputMode = Relaxed';
    end;
    BO.UndoManager.AddMsg(UndoAgent.undoMsg, UndoAgent.redoMsg);
    UndoAgent.UndoFlag := false;
    UndoAgent.UndoFlag := false;
  end;
end;

function TEventBO.GetRaceCount: Integer;
begin
  result := BO.BOParams.RaceCount;
end;

{ TEventRemoteObject }

function TEventRemoteObject.GetEventID: string;
begin
  result := FEventID;
end;

procedure TEventRemoteObject.SetEventID(const Value: string);
begin
  FEventID := Value;
end;

procedure TEventRemoteObject.Assign(Source: TPersistent);
var
  o: TEventRowCollectionItem;
  e: TEventRemoteObject;
  i: Integer;
begin
  if Source is TEventRowCollectionItem then
  begin
    o := TEventRowCollectionItem(Source);
    //
    SO := o.BaseID;
    Bib := o.Bib;
    //
    SNR := o.SNR;
    DN := o.DN;
    NOC := o.NC;
    //
    for i := 0 to RCount-1 do
      if i < o.RCount then
        Race[i].Assign(o.Race[i]);

  end
  else if Source is TEventRemoteObject then
  begin
    e := TEventRemoteObject(Source);
    //
    SO := e.SO;
    Bib := e.Bib;
    //
    SNR := e.SNR;
    DN := e.DN;
    NOC := e.NOC;
    //
    for i := 0 to RCount-1 do
      if i < e.RCount then
        Race[i].Assign(e.Race[i]);
  end
  else
    inherited Assign(Source);
end;

function TEventRemoteObject.GetCommaText(SL: TStrings): string;
var
  r: Integer;
begin
  if not Assigned(SL) then exit;
  SL.Clear;
  //
  SL.Add(IntToStr(SO));
  SL.Add(IntToStr(Bib));
  //
  SL.Add(IntToStr(SNR));
  SL.Add(DN);
  SL.Add(NOC);
  //
  for r := 1 to RCount-1 do
  begin
    SL.Add(Race[r].RaceValue);
  end;

  SL.Add(GRace.DecimalPoints);
  SL.Add(IntToStr(GRace.Rank));
  SL.Add(IntToStr(GRace.PosR));

  result := SL.CommaText;
end;

procedure TEventRemoteObject.SetCommaText(SL: TStrings);
var
  i, c: Integer;
  r: Integer;

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

  function NextC: char;
  begin
    if (i < c) and (Length(SL[i]) = 1) then
      result := SL[i][1]
    else
      result := SpaceChar;
    Inc(i);
  end;

begin
  if not Assigned(SL) then exit;
  i := 0;
  c := SL.Count;
  //
  SO := NextI;
  Bib :=  NextI;
  //
  SNR := NextI;
  DN := NextS;
  NOC := NextS;

  for r := 1 to RCount-1 do
  begin
    Race[r].RaceValue := NextS;
  end;

  GRace.CPoints := StrToFloatDef(NextS, 0); //GRace.Points := NextS;
  GRace.Rank := NextI;
  GRace.PosR := NextI;
end;

function TEventRemoteObject.GetCSV_Header: string;
var
  r: Integer;
begin
  result :=
    'Pos' + sep +
    'Bib' + sep +
    'SNR' + sep +
    'DN' + sep +
     N_NC + sep;
  for r := 1 to RCount-1 do
  begin
    result := result + 'R' + IntToStr(r) + sep;
  end;
  result := result + 'GPoints' + sep + 'GRank' + sep + 'GPosR';
end;

procedure TEventRemoteObject.GetOutput;
var
  r: Integer;
begin
  SLADD('Pos', IntToStr(SO));
  SLADD('Bib', IntToStr(Bib));
  //
  SLADD('SNR', IntToStr(SNR));
  SLADD('DN', DN);
  SLADD(N_NC, NOC);
  for r := 1 to RCount-1 do
  begin
    SLADD('R' + IntToStr(r), Race[r].RaceValue);
  end;
  SLADD('GPoints', GRace.DecimalPoints);
  SLADD('GRank', IntToStr(GRace.Rank));
  SLADDLAST('GPosR', IntToStr(GRace.PosR));
end;

{ TEventRaceEntry }

procedure TEventRaceEntry.Assign(Source: TPersistent);
var
  e: TEventRaceEntry;
begin
  if Source is TEventRaceEntry then
  begin
    e := TEventRaceEntry(Source);
    //
    IsRacing := e.IsRacing;
    Fleet := e.Fleet;
    Drop := e.Drop;
    Penalty.Assign(e.Penalty); //QU := e.QU;
    DG := e.DG;
    OTime := e.OTime;
    FCTime := e.FCTime;
    Rank := e.Rank;
    PosR := e.PosR;
    PLZ := e.PLZ;
  end
  else
    inherited Assign(Source);
end;

procedure TEventRaceEntry.Clear;
begin
  IsRacing := True;
  Fleet := 0;
  Drop := False;
  DG := 0;
  FCTime := 0;
  OTime := 0;
  Rank := 0;
  PosR := 0;
  PLZ := 0;
  FPenalty.Clear;
end;

constructor TEventRaceEntry.Create;
begin
  inherited Create;
  FPenalty := TPenaltyISAF.Create;
  IsRacing := True;
  Fleet := 1;
end;

destructor TEventRaceEntry.Destroy;
begin
  FPenalty.Free;
  inherited;
end;

function TEventRaceEntry.GetQU: Integer;
begin
  result := FPenalty.AsInteger;
end;

procedure TEventRaceEntry.SetQU(const Value: Integer);
begin
  FPenalty.AsInteger := Value;
end;

function TEventRaceEntry.GetRaceValue: string;
var
  sQU: string;
  sPoints: string;
begin
  sQU := Penalty.ToString;

  case Layout of
    0: sPoints := DecimalPoints; //Default
    1: sPoints := IntToStr(OTime); //FinishPos
    2: sPoints := DecimalPoints; //Points
    {
    3: sPoints := IntToStr(QU);
    4: sPoints := IntToStr(Rank);
    5: sPoints := IntToStr(PosR);
    6: sPoints := IntToStr(PLZ);
    }
    else
      sPoints := IntToStr(FCTime);
  end;
  result := sPoints;

  if sQU <> '' then
    result := result + '/' + sQU;

  { Parenthesis, runde Klammer }
  {
  if (CTime <> OTime)  and (Ttime <> 0) then
    result := result + ' (' + IntToStr(OTime) + ')';
  }

  { Bracket,  eckige Klammer }
  if Drop then
    result := '[' + result +']';

  if not IsRacing then
    result := '(' + result + ')';
end;

procedure TEventRaceEntry.SetRaceValue(const Value: string);
begin
  { use special value 0 to delete a FinishPosition, instead of -1,
    so that the sum of points is not affected }
  if Value = '0' then
    OTime := 0

  else if StrToIntDef(Value, -1) > 0 then
    OTime := StrToInt(Value)

  else if (Length(Value) > 1) and (Value[1] = 'F') then
    Fleet := ParseFleet(Value)

  else if Value = 'x' then
    IsRacing := False
  else if Value = '-x' then
    IsRacing := True

  else
    Penalty.Parse(Value);
end;

function TEventRaceEntry.ParseFleet(const Value: string): Integer;
var
  s: string;
  c: char;
begin
  result := Fleet;
  if (Length(Value) >= 2) and (UpperCase(Value[1]) = 'F') then
  begin
    s := Copy(UpperCase(Value), 2, MaxInt);
    c := s[1];
    if c = 'Y' then
      result := 1 //Yellow
    else if s = 'B' then
      result := 2 //Blue
    else if s = 'R' then
      result := 3 //Red
    else if s = 'G' then
      result := 4 //Green
    else if s = 'M' then
      result := 0 //MedalRace
    else
    begin
      result := StrToIntDef(s, Fleet); //valid integer
      if result < 0 then //limit to positiv values
        result := Fleet;
    end;
  end;
end;

function TEventRaceEntry.GetPoints: string;
begin
  result := FloatToStr(CPoints);
end;

function TEventRaceEntry.GetDecimalPoints: string;
begin
  result := FormatFloat('0.0#', CPoints);
end;

procedure TEventRaceEntry.SetCPoints(const Value: double);
var
  t1, t2: Integer;
begin
  FCTime := Round(Value * 100);
  t1 := Round(Value * 10000);
  t2 := Round(Value) * 10000;
  FTiebreakIncrement := t1 - t2;
end;

function TEventRaceEntry.GetCPoints: double;
begin
  result := FCTime / 100;
end;

procedure TEventRaceEntry.SetCTime1(const Value: Integer);
begin
  FCTime := Value * 100;
end;

function TEventRaceEntry.GetCTime1: Integer;
begin
  result := FCTime div 100;
end;

function TEventRaceEntry.GetLayout: Integer;
begin
  if Assigned(FNode) then
  begin
    result := FNode.ShowPoints;
  end
  else
    result := 0;
end;

{ TCRList }

constructor TCRList.Create(aOwnsObjects: Boolean);
begin
  inherited Create(TEventRowCollectionItem, aOwnsObjects);
end;

function TCRList.GetCR(i: Integer): TEventRowCollectionItem;
begin
  result := TypedItem[i] as TEventRowCollectionItem;
end;

procedure TCRList.SetCR(i: Integer; const Value: TEventRowCollectionItem);
begin
  self.TypedItem[i] := Value;
end;

end.

