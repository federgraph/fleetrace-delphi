unit RiggVar.Calc.EV;

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
  RiggVar.Col.Event;

const
  channel_FT = 0;

  //ScoringProvider_UseDefault = 0;
  ScoringProvider_SimpleTest = 1;
  ScoringProvider_Inline = 2;
  ScoringProvider_ProxyDLL = 3;

type
  TIntArray = array of Integer;

  TCalcEventProxy = class
  private
    FHighestBibGoesFirst: Boolean;
  public
    CheckList: TStrings;
    Debug: Boolean;
    WithTest: Boolean;
    EventName: string;
    procedure Calc(aqn: TEventNode); virtual; abstract;
    constructor Create;
    destructor Destroy; override;
    procedure GetScoringNotes(SL: TStrings); virtual;
    property HighestBibGoesFirst: Boolean read FHighestBibGoesFirst write FHighestBibGoesFirst;
  end;

  TCalcEvent = class
  private
    FProxy: TCalcEventProxy;
    FProviderID: Integer;
    function GetModuleType: Integer;
    procedure SetModuleType(const Value: Integer);
    function GetUsesProxy: Boolean;
    procedure SetProviderID(const Value: Integer);
  public
    constructor Create(aProviderID: Integer);
    destructor Destroy; override;
    procedure InitModule(aProviderID: Integer);
    procedure Calc(aqn: TEventNode);
    property Proxy: TCalcEventProxy read FProxy;
    property ModuleType: Integer read GetModuleType write SetModuleType;
    property UsesProxy: Boolean read GetUsesProxy;
    property ProviderID: Integer read FProviderID write SetProviderID;
  end;

var
  _ScoringResult: Integer;
  _ScoringExceptionLocation: string;
  _ScoringExceptionMessage: string;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Calc.EventProxy01, //simple version
  RiggVar.Calc.EventProxy06, //direkt (kompakt)
  RiggVar.BO.UniquaPoints;

{ TCalcEventProxy }

constructor TCalcEventProxy.Create;
begin
  CheckList := TStringList.Create;
end;

destructor TCalcEventProxy.Destroy;
begin
  CheckList.Free;
  inherited;
end;

procedure TCalcEventProxy.GetScoringNotes(SL: TStrings);
begin
  SL.Add('TCalcEventProxy.GetScoringNotes');
end;

{ TCalcEvent }

constructor TCalcEvent.Create(aProviderID: Integer);
begin
  inherited Create;
  InitModule(aProviderID);
end;

destructor TCalcEvent.Destroy;
begin
  FProxy.Free;
  inherited;
end;

procedure TCalcEvent.InitModule(aProviderID: Integer);
begin
  if (FProxy = nil) or (ProviderID <> aProviderID) then
  try
    ProviderID := aProviderID;
    { free old module if exists }
    if FProxy <> nil then
    begin
      FProxy.Free;
      FProxy := nil;
    end;
    { create new module }
    case ProviderID of
      ScoringProvider_SimpleTest: FProxy := TSimpleCalcEventProxy.Create; //1
      ScoringProvider_Inline: FProxy := TInlineCalcEventProxy.Create; //2
    else
      begin
        ProviderID := ScoringProvider_SimpleTest;
        FProxy := TSimpleCalcEventProxy.Create;
      end;
    end;
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEvent.InitModule';
      _ScoringExceptionMessage := e.Message;
      ProviderID := ScoringProvider_SimpleTest;
      FProxy := TSimpleCalcEventProxy.Create;
    end;
  end;
end;

procedure TCalcEvent.Calc(aqn: TEventNode);
begin
  try
    _ScoringResult := 0;
    FProxy.Calc(aqn);
    TUniquaPoints.Calc(aqn);
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEvent.Calc';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

function TCalcEvent.GetModuleType: Integer;
begin
  result := ProviderID;
end;

procedure TCalcEvent.SetModuleType(const Value: Integer);
begin
  if Value <> ProviderID then
  begin
    self.InitModule(Value);
  end;
end;

function TCalcEvent.GetUsesProxy: Boolean;
begin
  case ModuleType of
    ScoringProvider_SimpleTest: result := false;
    ScoringProvider_Inline: result := false;
    ScoringProvider_ProxyDLL: result := true;
    else
      result := false;
  end;
end;

procedure TCalcEvent.SetProviderID(const Value: Integer);
begin
  FProviderID := Value;
  Main.IniImage.ScoringProvider := Value;
end;

end.
