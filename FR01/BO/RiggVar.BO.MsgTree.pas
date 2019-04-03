unit RiggVar.BO.MsgTree;

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
  RiggVar.BO.MsgToken,
  RiggVar.BO.Time,
  RiggVar.DAL.Redirector;

type
  TBaseToken = class
  private
    class var
      MsgID: Integer;
      UseLongNames: Boolean;
    var
      Owner: TBaseToken;
      FNameID: string;
      FActionID: Integer;
  public
    TokenID: Integer;
    class var NewActionID: Integer; //used only during construction of MsgTree
    constructor New;
    constructor Create(aOwner: TBaseToken; aNameID: string);
    function NamePath: string; virtual;
    function NameID: string;
    property ActionID: Integer read FActionID;
  end;

  TTokenList = class
  private
    FIndexedChildToken: TBaseToken;
    function GetToken(ID: Integer): TBaseToken;
  public
    constructor Create(aOwner: TBaseToken; aTokenName: string; aChildToken: TBaseToken);
    destructor Destroy; override;
    property Token[ID: Integer]: TBaseToken read GetToken;
  end;

  TInputActionEvent = procedure(Sender: TObject; s: string) of object;

  TInputAction = class
  private
    FOnSend: TInputActionEvent;
  public
    procedure Send(sKey, sValue: string); virtual;
    property OnSend: TInputActionEvent read FOnSend write FOnSend;
  end;

  //adds validation support and sendMsg method (needed for leaf-functions)
  TInputValue = class(TBaseToken)
  private
    function GetInputAction: TInputAction;
  protected
    function IsValidBoolean(s: string): Boolean;
    function IsPositiveInteger(s: string): Boolean;
    //
    function IsValidCount(s: string): Boolean;
    function IsValidProp(Key, Value: string): Boolean;
    //
    function IsValidTime(s: string): Boolean;
    function IsValidStatus(s: string): Boolean;
    function IsValidBib(s: string): Boolean;
    function IsValidSNR(s: string): Boolean;
    function IsValidNOC(s: string): Boolean;
    function IsValidName(s: string): Boolean;
    function IsValidDSQGate(s: string): Boolean;
    function IsValidGender(s: string): Boolean;
    function IsValidPersonalBest(s: string): Boolean;

    function IsValidRaceValue(s: string): Boolean;
    property InputAction: TInputAction read GetInputAction;
  public
    procedure SendMsg(IsValid: Boolean; aCommand, aValue: string);
  end;

  TAthlete = class(TInputValue)
  public
    procedure SNR(Value: string);
    procedure FN(Value: string);
    procedure LN(Value: string);
    procedure SN(Value: string);
    procedure NC(Value: string);
    procedure GR(Value: string);
    procedure PB(Value: string);
    procedure Prop(Key, Value: string);
    procedure FieldN(Index: Integer; Value: string);
  end;

  TBib = class(TInputValue)
  private
    procedure SetIT(channel: Integer; const Value: string);
  public
    procedure XX(Value: string);
    procedure QU(Value: string);
    procedure ST(Value: string);
    procedure FT(Value: string);
    procedure DG(Value: string);
    procedure DSQGate(Value: string);
    procedure Rank(Value: string);
    procedure RV(Value: string);
    procedure FM(Value: string);
    property IT[channel: Integer]: string write SetIT;
  end;

  TPos = class(TInputValue)
  public
    procedure Bib(Value: string);
    procedure SNR(Value: string);
    procedure AthleteID(Value: string);
  end;

  TStartList = class(TInputValue)
  private
    FPosStore: TTokenList;
    function GetPos(index: Integer): TPos;
  public
    constructor Create(aOwner: TBaseToken; aNameID: string);
    destructor Destroy; override;
    procedure Count(Value: string);
    property Pos[index: Integer]: TPos read GetPos;
  end;

  TRun = class(TInputValue)
  private
    FBibStore: TTokenList;
    function GetBib(index: Integer): TBib;
  public
    constructor New;
    constructor Create(aOwner: TBaseToken; aNameID: string);
    destructor Destroy; override;
    procedure IsRacing(Value: string);
    property Bib[index: Integer]: TBib read GetBib;
  end;

  TRun1 = class(TRun)
  public
    StartList: TStartList;
    constructor Create(aOwner: TBaseToken; aNameID: string);
    destructor Destroy; override;
  end;

  TDivision = class(TBaseToken)
  private
    FAthleteStore: TTokenList;
    FRaceStore: TTokenList;
    function GetAthlete(index: Integer): TAthlete;
    function GetRace(index: Integer): TRun;
  public
    Race1: TRun1;
    constructor Create(aOwner: TBaseToken; aNameID: string);
    destructor Destroy; override;
    property Race[index: Integer]: TRun read GetRace;
    property Athlete[index: Integer]: TAthlete read GetAthlete;
  end;

  TMsgTree = class(TBaseToken)
  private
    FDivision: TDivision;
    function GetLongNames: Boolean;
    procedure SetLongNames(const Value: Boolean);
  public
    UseMsgID: Boolean;
    UsePrefix: Boolean;
    constructor Create(aOwner: TBaseToken; aNameID: string; aActionID: Integer); //override;
    destructor Destroy; override;
    function NamePath: string; override;
    property LongNames: Boolean read GetLongNames write SetLongNames;
    property Division: TDivision read FDivision;
  end;

  TBackupEvent = procedure (Sender: TMsgTree) of object;

  TBackup = class
  private
    FSLBackup: TStringList;
    InputAction: TInputAction;
    FOnBackup: TBackupEvent;
    procedure SaveLine(Sender: TObject; s: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Backup(aFileName: string);
    property OnBackup: TBackupEvent read FOnBackup write FOnBackup;
  end;

  TInputActionManager = class
  public
    class var
      DynamicActionRef: TInputAction;
      UndoActionRef: TInputAction;
  end;

const
  DynamicActionID: Integer = 0;
  UndoActionID: Integer = 1;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

var
  PTime: TPTime = nil; //Singelton, for validation

{ TMsgTree }

constructor TMsgTree.Create(aOwner: TBaseToken; aNameID: string; aActionID: Integer);
begin
  NewActionID := aActionID;
  inherited Create(aOwner, aNameID);
  FDivision := TDivision.Create(Self, cTokenB);
  UsePrefix := True;
end;

destructor TMsgTree.Destroy;
begin
  FDivision.Free;
  inherited;
end;

function TMsgTree.GetLongNames: Boolean;
begin
  result := UseLongNames;
end;

function TMsgTree.NamePath: string;
begin
  if UseMsgiD then
  begin
    if Assigned(Owner) then
      result := Owner.NamePath + '.' + NameID
    else
      result := NameID + cTokenMsg + IntToStr(MsgID);
  end
  else if UsePrefix then
    result := inherited NamePath
  else
    result := ''
end;

procedure TMsgTree.SetLongNames(const Value: Boolean);
begin
  UseLongNames := Value;
end;

{ TInputValue }

procedure TInputValue.SendMsg(IsValid: Boolean; aCommand, aValue: string);
var
  sKey: string;
begin
  if IsValid then
  begin
    inc(MsgID);
    sKey := '';
  end
  else
    sKey := '//';
  sKey := sKey + NamePath + '.' + aCommand;
  if InputAction <> nil then
    InputAction.Send(sKey, aValue);
end;

function TInputValue.IsValidSNR(s: string): Boolean;
begin
  result := StrToIntDef(s, -1) >= 0;
end;

function TInputValue.GetInputAction: TInputAction;
begin
  if (ActionID = 0) then
    result := TInputActionManager.DynamicActionRef
  else
    result := TInputActionManager.UndoActionRef;
end;

function TInputValue.IsPositiveInteger(s: string): Boolean;
begin
  result := StrToIntDef(s, -1) >= 0;
end;

function TInputValue.IsValidBoolean(s: string): Boolean;
begin
  result := (s = 'True') or (s = 'False');
end;

function TInputValue.IsValidCount(s: string): Boolean;
var
  i: Integer;
begin
  i := StrToIntDef(s, -1);
  result := (i >= 0) and (i <= 50);
end;

function TInputValue.IsValidBib(s: string): Boolean;
begin
  result := StrToIntDef(s, -1) >= 0;
end;

function TInputValue.IsValidDSQGate(s: string): Boolean;
begin
  result := StrToIntDef(s, -1) >= 0;
end;

function TInputValue.IsValidName(s: string): Boolean;
begin
  result := True;
end;

function TInputValue.IsValidNOC(s: string): Boolean;
begin
  result := Length(s) < 40;
end;

function TInputValue.IsValidStatus(s: string): Boolean;
begin
  result := True;
  {
  result := (s = 'ok')
    or (s = 'dnf')
    or (s = 'dsq')
    or (s = 'dns')
    or (s = '*');
  }
end;

function TInputValue.IsValidTime(s: string): Boolean;
begin
  //result := False;
  result := PTime.IsValidTimeString(s);
end;

function TInputValue.IsValidGender(s: string): Boolean;
begin
  result := Length(s) < 20;
end;

function TInputValue.IsValidPersonalBest(s: string): Boolean;
begin
  result := Length(s) < 20;
end;

function TInputValue.IsValidRaceValue(s: string): Boolean;
begin
  result := Length(s) < 13;
end;

function TInputValue.IsValidProp(Key, Value: string): Boolean;
begin
  result := Length(Key) < 20;
  result := result and (Length(Value) > 0);
  result := result and (Length(Value) < 40);
end;

{ TBaseToken }

constructor TBaseToken.New;
begin
  inherited Create;
  FActionID := NewActionID;
end;

constructor TBaseToken.Create(aOwner: TBaseToken; aNameID: string);
begin
  inherited Create;
  FActionID := NewActionID;

  Owner := aOwner;
  FNameID := aNameID;
  TokenID := -1;
end;

function TBaseToken.NameID: string;
begin
  if (TokenID > -1) then
  begin
    if UseLongNames then
      result := LongToken(FNameID) + IntToStr(TokenID)
    else
      result := FNameID + IntToStr(TokenID);
  end
  else
  begin
    if UseLongNames then
      result := LongToken(FNameID)
    else
      result := FNameID
  end;
end;

function TBaseToken.NamePath: string;
var
  s: string;
begin
  result := NameID;
  if Assigned(Owner) then
  begin
    s := Owner.NamePath;
    if s <> '' then
      result := s + '.' + NameID;
  end;
end;

{ TInputAction }

procedure TInputAction.Send(sKey, sValue: string);
begin
  if Assigned(OnSend) then
    OnSend(Self, sKey + '=' + sValue);
end;

{ TTokenList }

constructor TTokenList.Create(aOwner: TBaseToken; aTokenName: string;
  aChildToken: TBaseToken);
begin
  inherited Create;
  FIndexedChildToken := aChildToken;
  FIndexedChildToken.Owner := aOwner;
  FIndexedChildToken.FNameID := aTokenName;
end;

destructor TTokenList.Destroy;
begin
  FIndexedChildToken.Free;
  inherited;
end;

function TTokenList.GetToken(ID: Integer): TBaseToken;
begin
  FIndexedChildToken.TokenID := ID;
  result := FIndexedChildToken;
end;

{ TBackup }

constructor TBackup.Create;
begin
  inherited Create;
  FSLBackup := TDBStringList.Create;
  InputAction := TInputAction.Create;
  InputAction.OnSend := SaveLine;
end;

destructor TBackup.Destroy;
begin
  InputAction.Free;
  FSLBackup.Free;
  FSLBackup := nil;
  inherited;
end;

procedure TBackup.Backup(aFileName: string);
begin
  TInputActionManager.DynamicActionRef := InputAction;
  try
    if Assigned(OnBackup) then
      OnBackup(BO.MsgTree);
    Main.StoreAdapter.StringListSaveToFile(FSLBackup, aFileName);
  finally
    TInputActionManager.DynamicActionRef := nil;
  end;
end;

procedure TBackup.SaveLine(Sender: TObject; s: string);
begin
  with FSLBackup do Add(s);
end;

{ TAthlete }

procedure TAthlete.SNR(Value: string);
begin
  Self.SendMsg(IsPositiveInteger(Value), cTokenID, Value);
end;

procedure TAthlete.FN(Value: string);
begin
  Self.SendMsg(IsValidName(Value), N_FN, Value);
end;

procedure TAthlete.LN(Value: string);
begin
  Self.SendMsg(IsValidName(Value), N_LN, Value);
end;

procedure TAthlete.SN(Value: string);
begin
  Self.SendMsg(IsValidName(Value), N_SN, Value);
end;

procedure TAthlete.NC(Value: string);
begin
  Self.SendMsg(IsValidNOC(Value), N_NC, Value);
end;

procedure TAthlete.GR(Value: string);
begin
  Self.SendMsg(IsValidGender(Value), N_GR, Value);
end;

procedure TAthlete.PB(Value: string);
begin
  Self.SendMsg(IsValidPersonalBest(Value), N_PB, Value);
end;

procedure TAthlete.FieldN(Index: Integer; Value: string);
begin
  SendMsg(IsValidName(Value), 'N' + IntToStr(Index), Value);
end;

procedure TAthlete.Prop(Key, Value: string);
begin
  Self.SendMsg(IsValidProp(Key, Value), 'Prop_' + Key, Value);
end;

{ TBib }

procedure TBib.DG(Value: string);
begin
  Self.SendMsg(IsValidDSQGate(Value), 'DG', Value);
end;

procedure TBib.DSQGate(Value: string);
begin
  DG(Value);
end;

procedure TBib.FT(Value: string);
begin
  Self.SendMsg(IsValidTime(Value), 'FT', Value);
end;

{
procedure TBib.IT(Value: string; channel: Integer);
begin
  Self.SendMsg(IsValidTime(Value), 'IT' + IntToStr(channel), Value);
end;
}

procedure TBib.QU(Value: string);
begin
  Self.SendMsg(IsValidStatus(Value), 'QU', Value);
end;

procedure TBib.Rank(Value: string);
begin
  Self.SendMsg(IsPositiveInteger(Value), 'Rank', Value);
end;

procedure TBib.RV(Value: string);
begin
  Self.SendMsg(IsValidRaceValue(Value), 'RV', Value);
end;

procedure TBib.SetIT(channel: Integer; const Value: string);
begin
  Self.SendMsg(IsValidTime(Value), 'IT' + IntToStr(channel), Value);
end;

procedure TBib.ST(Value: string);
begin
  Self.SendMsg(IsValidTime(Value), 'ST', Value);
end;

procedure TBib.XX(Value: string);
begin
  Self.SendMsg(True, 'XX', Value);
end;

procedure TBib.FM(Value: string);
begin
  Self.SendMsg(IsValidRaceValue(Value), 'FM', Value);
end;

{ TPos }

procedure TPos.Bib(Value: string);
begin
  Self.SendMsg(IsValidBib(Value), cTokenBib, Value);
end;

procedure TPos.SNR(Value: string);
begin
  Self.SendMsg(IsValidSNR(Value), cTokenID, Value);
end;

procedure TPos.AthleteID(Value: string);
begin
  SNR(Value);
end;

{ TStartList }

procedure TStartList.Count(Value: string);
begin
  Self.SendMsg(IsPositiveInteger(Value), cTokenCount, Value);
end;

constructor TStartList.Create(aOwner: TBaseToken; aNameID: string);
begin
  inherited;
  FPosStore := TTokenList.Create(Self, 'Pos', TPos.New);
end;

destructor TStartList.Destroy;
begin
  FPosStore.Free;
  inherited;
end;

function TStartList.GetPos(index: Integer): TPos;
begin
  result := FPosStore.Token[index] as TPos;
end;

{ TRun }

constructor TRun.New;
begin
  inherited;
  FBibStore := TTokenList.Create(Self, cTokenBib, TBib.New);
end;

constructor TRun.Create(aOwner: TBaseToken; aNameID: string);
begin
  inherited;
  FBibStore := TTokenList.Create(Self, cTokenBib, TBib.New);
end;

destructor TRun.Destroy;
begin
  FBibStore.Free;
  inherited;
end;

function TRun.GetBib(index: Integer): TBib;
begin
  result := FBibStore.Token[index] as TBib;
end;

procedure TRun.IsRacing(Value: string);
begin
  SendMsg(IsValidBoolean(Value), 'IsRacing', Value);
end;

{ TRun1 }

constructor TRun1.Create(aOwner: TBaseToken; aNameID: string);
begin
  inherited;
  StartList := TStartList.Create(Self, 'STL');
end;

destructor TRun1.Destroy;
begin
  StartList.Free;
  inherited;
end;

{ TGender }

constructor TDivision.Create(aOwner: TBaseToken; aNameID: string);
begin
  inherited;
  FAthleteStore := TTokenList.Create(Self, cTokenID, TAthlete.New);
  FRaceStore := TTokenList.Create(Self, cTokenRace, TRun.New);
  Race1 := TRun1.Create(Self, cTokenRace + '1');
end;

destructor TDivision.Destroy;
begin
  FRaceStore.Free;
  FAthleteStore.Free;
  Race1.Free;
  inherited;
end;

function TDivision.GetAthlete(index: Integer): TAthlete;
begin
  result := FAthleteStore.Token[index] as TAthlete;
end;

function TDivision.GetRace(index: Integer): TRun;
begin
  result := FRaceStore.Token[index] as TRun;
end;

initialization
  PTime := TPTime.Create;

finalization
  PTime.Free;

end.
