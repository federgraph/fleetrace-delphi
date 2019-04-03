unit RiggVar.BO.MsgBase;

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
  RiggVar.Util.Classes;

type
  TReplayMsg = class(TPersistent)
  protected
    FDivision: string;
    FRunID: string;
    FBib: Integer;
    FCmd: string;
    FMsgValue: string;
    function GetDiskMsg: string; virtual;
    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
  public
    DBID: Integer; //autoinc in DB
    LogTime: TDateTime;
    SeqNo: Integer;
    Delivered: Boolean;
    CheckInt: Integer;
    ReplayInterval: Integer;
    Hidden: Boolean;
    ReplayOrder: Integer;
    IsError: Boolean; //do not persist
    IsCheckSumError: Boolean; //do not persist
    constructor Create;
    procedure ClearResult; virtual;
    procedure Assign(Source: TPersistent); override;
    class function DiskMsgHeader: string; virtual;
    //
    property Division: string read FDivision write FDivision;
    property RunID: string read FRunID write FRunID;
    property Bib: Integer read FBib write FBib;
    property Cmd: string read FCmd write FCmd;
    property MsgValue: string read FMsgValue write FMsgValue;
    //
    property AsString: string read GetAsString write SetAsString;
    property DiskMsg: string read GetDiskMsg;
  end;

  TMsgDB = class(TStringList)
  private
  protected
    function GetMsgItem(Index: Integer): TReplayMsg;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Dump(Memo: TStrings);
    procedure Save(FileName: string);
    property MsgItems[Index: Integer]: TReplayMsg read GetMsgItem;
  end;

  TBaseMsg = class(TReplayMsg)
  public
    Prot: string;
    MsgResult: Integer;
    OutputRequestList: TStrings;
    constructor Create; //virtual;
    destructor Destroy; override;
    function DispatchProt: Boolean; virtual;
  end;

implementation

uses
  RiggVar.DAL.Redirector;

var
  MsgID: Integer = 0;

{ TReplayMsg }

constructor TReplayMsg.Create;
begin
  inherited;
  ClearResult;
end;

procedure TReplayMsg.Assign(Source: TPersistent);
var
  cr: TReplayMsg;
begin
  if Source is TReplayMsg then
  begin
    cr := TReplayMsg(Source);
    { Content }
    FDivision := cr.FDivision;
    FRunID := cr.FRunID;
    FBib := cr.FBib;
    FCmd := cr.FCmd;
    FMsgValue := cr.FMsgValue;
    { Management }
    DBID := cr.DBID;
    LogTime := cr.LogTime;
    SeqNo := cr.SeqNo;
    IsError := cr.IsError;
    IsCheckSumError := cr.IsCheckSumError;
    Delivered := cr.Delivered;
    CheckInt := cr.CheckInt;
    ReplayInterval := cr.ReplayInterval;
    Hidden := cr.Hidden;
    ReplayOrder := cr.ReplayOrder;
  end
  else
    inherited Assign(Source);
end;

procedure TReplayMsg.ClearResult;
begin
  { Content }
  FDivision := '*';
  FRunID := 'RunID';
  FBib := 0;
  FCmd := 'Cmd';
  FMsgValue := '00:00:00.000';
  { Management }
  DBID := -1;
  LogTime := Now;
  SeqNo := 1;
  IsError := False;
  IsCheckSumError := False;
  Delivered := False;
  CheckInt := 0;
  ReplayInterval := 1000;
  Hidden := False;
  ReplayOrder := 0;
end;

function TReplayMsg.GetAsString: string;
var
  sDBID: string;
begin
  if DBID < 0 then
    sDBID := 'DBID'
  else
    sDBID := IntToStr(DBID);
  result := Cmd + ',' + MsgValue + ',' + sDBID;
end;

procedure TReplayMsg.SetAsString(const Value: string);
var
  s: string;
  temp: string;
begin
  s := TUtils.Cut(',', Value, temp);
  Cmd := temp;
  s := TUtils.Cut(',', s, temp);
  MsgValue := temp;
  s := TUtils.Cut(',', s, temp);
  DBID := StrToIntDef(temp, -1);
end;

function TReplayMsg.GetDiskMsg: string;
var
  sep: string;
begin
  sep := ',';
  result :=
    Cmd + sep +
    MsgValue + sep +
    IntToStr(ReplayInterval) + sep;
end;

class function TReplayMsg.DiskMsgHeader: string;
begin
  result := 'Cmd,MsgValue,ReplayInterval';
end;

{ TMsgDB }

destructor TMsgDB.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMsgDB.Clear;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    Objects[i].Free;
  inherited;
end;

procedure TMsgDB.Dump(Memo: TStrings);
var
  i: Integer;
  cm: TReplayMsg;
begin
  for i := 0 to Count-1 do
  begin
    cm := MsgItems[i];
    if Assigned(cm) then
      Memo.Add(Self[i] + ' ' + cm.AsString);
  end;
end;

function TMsgDB.GetMsgItem(Index: Integer): TReplayMsg;
begin
  result := nil;
  if (Index >= 0) and (Index <= Count-1) then
    result := TReplayMsg(Objects[Index]);
end;

procedure TMsgDB.Save(FileName: string);
var
  SL: TStringList;
  i: Integer;
  cr: TReplayMsg;
  s: string;
begin
  SL := TDBStringList.Create;
  try
    SL.Add(TReplayMsg.DiskMsgHeader);
    for i := 0 to Count-1 do
    begin
      cr := MsgItems[i];
      s := cr.DiskMsg;
      SL.Add(s);
    end;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

{ TBaseMsg }

constructor TBaseMsg.Create;
begin
  inherited Create;
  OutputRequestList := TStringList.Create;
end;

destructor TBaseMsg.Destroy;
begin
  OutputRequestList.Free;
  inherited;
end;

function TBaseMsg.DispatchProt: Boolean;
begin
  //virtual
  result := False;
end;

end.

