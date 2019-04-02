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
  RiggVar.App.Config,
  RiggVar.Col.Event,
  RiggVar.BO.Time,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Scoring.Penalty;

const
  channel_FT = 0;

  //ScoringProvider_UseDefault = 0;
  ScoringProvider_SimpleTest = 1;
  ScoringProvider_Inline = 2;
  ScoringProvider_ProxyDLL = 3;
  ScoringProvider_ProxyXML = 4;
  ScoringProvider_WebService = 5;
  ScoringProvider_WebServiceFR81 = 6;

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
    function GetHighestBibGoesFirst: Boolean;
    procedure SetHighestBibGoesFirst(const Value: Boolean);
    function GetModuleType: Integer;
    procedure SetModuleType(const Value: Integer);
    function GetScoringExceptionMessage: string;
    function GetScoringResult: Boolean;
    function GetUsesProxy: Boolean;
    procedure SetProviderID(const Value: Integer);
  public
    constructor Create(aProviderID: Integer);
    destructor Destroy; override;
    function EditScoringModule: Boolean;
    procedure InitModule(aProviderID: Integer);
    procedure Calc(aqn: TEventNode);
    procedure GetScoringNotes(SL: TStrings);
    property HighestBibGoesFirst: Boolean read GetHighestBibGoesFirst write SetHighestBibGoesFirst;
    property Proxy: TCalcEventProxy read FProxy;
    property ModuleType: Integer read GetModuleType write SetModuleType;
    property ScoringResult: Boolean read GetScoringResult;
    property ScoringExceptionMessage: string read GetScoringExceptionMessage;
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
  //RiggVar.CalcEventProxy02, //direkt
  RiggVar.Calc.EventProxy03, //via FRProxy
  RiggVar.Calc.EventProxy04, //via FRProxy/Xml
  RiggVar.Calc.EventProxy05, //via FRProxy/Xml/WebService
  RiggVar.Calc.EventProxy06, //direkt (kompakt)
  RiggVar.Calc.EventProxy81, //via FRProxy/Xml/WebService (Java)
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
      ScoringProvider_ProxyDLL: FProxy := TDLLCalcEventProxy.Create; //3
      ScoringProvider_ProxyXML: FProxy := TCalcEventProxy4.Create; //4
      ScoringProvider_WebService: FProxy := TCalcEventProxy5.Create; //5
      ScoringProvider_WebServiceFR81: FProxy := TCalcEventProxyFR81.Create; //5
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

function TCalcEvent.GetHighestBibGoesFirst: Boolean;
begin
  result := FProxy.HighestBibGoesFirst;
end;

procedure TCalcEvent.SetHighestBibGoesFirst(const Value: Boolean);
begin
  FProxy.HighestBibGoesFirst := Value;
end;

procedure TCalcEvent.GetScoringNotes(SL: TStrings);
begin
  try
    FProxy.GetScoringNotes(SL);
  except
    on e: Exception do
    begin
      _ScoringResult := -1;
      _ScoringExceptionLocation := 'TCalcEvent.GetScoringNotes';
      _ScoringExceptionMessage := e.Message;
    end;
  end;
end;

function TCalcEvent.EditScoringModule: Boolean;
begin
  result := Main.FormAdapter.EditScoringModule(self);
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

function TCalcEvent.GetScoringExceptionMessage: string;
begin
  result := 'Exception_Location: ' + _ScoringExceptionLocation + #13#10 +
  'Exception_Message: ' + _ScoringExceptionMessage;
end;

function TCalcEvent.GetScoringResult: Boolean;
begin
  result := _ScoringResult <> -1;
end;

function TCalcEvent.GetUsesProxy: Boolean;
begin
  case ModuleType of
    ScoringProvider_SimpleTest: result := false;
    ScoringProvider_Inline: result := false;
    ScoringProvider_ProxyDLL: result := true;
    ScoringProvider_ProxyXML: result := true;
    ScoringProvider_WebService: result := true;
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
