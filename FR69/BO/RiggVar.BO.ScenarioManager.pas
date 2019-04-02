unit RiggVar.BO.ScenarioManager;

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
  Vcl.Forms;

type
  TConfigScenario = (
    Scenario_Default,
    Scenario_Disconnected,
    Scenario_Readonly,
    Scenario_BridgeServer_LH,
    Scenario_BridgeServer,
    Scenario_HomeServer,
    Scenario_EC2,
    Scenario_BridgeClient,
    Scenario_Webonly,
    Scenario_OutputBridge,
    Scenario_ShowSelector,
    Scenario_Unknown
  );

  TScenarioManager = class
  private
    SL: TStringList;
    function GetScenarioList: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function FromIndex(Index: Integer): TConfigScenario;
    property ScenarioList: TStrings read GetScenarioList;
  end;

  TScenarioInfo = class
  private
    FScenario: TConfigScenario;
    FPortSet: Integer;
    FWantPolicyServer: Integer;
    procedure InitFromAppTitle0;
    procedure InitFromAppTitle1;
    procedure ReadParams(SL: TStrings);
    function ReadCmdLine: Boolean;
    procedure ReadTxtFile(fn: string);
    function InitDir: string;
  public
    constructor Create;
    procedure Load; virtual;
    class function ParseInt(Value: Integer): TConfigScenario; static;
    function ToString: string; override;
    property Scenario: TConfigScenario read FScenario write FScenario;
    property PortSet: Integer read FPortSet;
    property WantPolicyServer: Integer read FWantPolicyServer;
  end;

implementation

uses
  RiggVar.App.Main,
  System.StrUtils;

{ TScenarioManager }

constructor TScenarioManager.Create;
begin
  SL := TStringList.Create;
  Init;
end;

destructor TScenarioManager.Destroy;
begin
  SL.Free;
  inherited;
end;

function TScenarioManager.GetScenarioList: TStrings;
begin
  result := SL;
end;

procedure TScenarioManager.Init;
begin
  SL.Add('Default');
  SL.Add('Disconnected');
  SL.Add('Readonly');
  SL.Add('BridgeServer LH');
  SL.Add('BridgeServer');
  SL.Add('HomeServer');
  SL.Add('EC2');
  SL.Add('BridgeClient');
  SL.Add('Webonly');
  SL.Add('OutputBridge');
end;

function TScenarioManager.FromIndex(Index: Integer): TConfigScenario;
begin
  case Index of
    0: result := Scenario_Default;
    1: result := Scenario_Disconnected;
    2: result := Scenario_Readonly;
    3: result := Scenario_BridgeServer_LH;
    4: result := Scenario_BridgeServer;
    5: result := Scenario_HomeServer;
    6: result := Scenario_EC2;
    7: result := Scenario_BridgeClient;
    8: result := Scenario_Webonly;
    9: result := Scenario_OutputBridge;
    else
      result := Scenario_Disconnected;
  end;
end;

{ TScenarioInfo }

constructor TScenarioInfo.Create;
begin
  if Main.Params.IsSpecialVersion then
    InitFromAppTitle1
  else
    InitFromAppTitle0;
  FWantPolicyServer := -1;
end;

procedure TScenarioInfo.InitFromAppTitle0;
var
  at: string;
begin
  at := UpperCase(Application.Title);
  if at = 'FR01' then
    FScenario := Scenario_Readonly
  else if at = 'FR02' then
    FScenario := Scenario_Webonly
  else if at = 'FR03' then
    FScenario := Scenario_BridgeClient
  else if at = 'FR04' then
    FScenario := Scenario_BridgeServer
  else if at = 'FR05' then
    FScenario := Scenario_HomeServer
  else if at = 'FR06' then
    FScenario := Scenario_EC2
  else if at = 'FR07' then
    FScenario := Scenario_Unknown
  else if at = 'FR08' then
    FScenario := Scenario_BridgeServer
  else if at = 'FR09' then
    FScenario := Scenario_OutputBridge
  else if at = 'FR62' then
    FScenario :=  Scenario_BridgeServer
  else
    FScenario := Scenario_Default;
end;

procedure TScenarioInfo.InitFromAppTitle1;
var
  at: string;
begin
  at := UpperCase(Application.Title);
  if at = 'FR11' then
    FScenario := Scenario_Readonly
  else if at = 'FR12' then
    FScenario := Scenario_Webonly
  else if at = 'FR13' then
    FScenario := Scenario_BridgeClient
  else if at = 'FR14' then
    FScenario := Scenario_BridgeServer
  else if at = 'FR15' then
    FScenario := Scenario_HomeServer
  else if at = 'FR16' then
    FScenario := Scenario_EC2
  else if at = 'FR17' then
    FScenario := Scenario_Unknown
  else if at = 'FR18' then
    FScenario := Scenario_BridgeServer
  else if at = 'FR19' then
    FScenario := Scenario_OutputBridge
  else
    FScenario := Scenario_Readonly;
end;

function TScenarioInfo.ToString: string;
begin
  case Scenario of
    Scenario_Default: result := 'Default';
    Scenario_Disconnected: result := 'Disconnected';
    Scenario_Readonly: result := 'Readonly';
    Scenario_BridgeServer_LH: result := 'BridgeServer-LH';
    Scenario_BridgeServer: result := 'BridgeServer';
    Scenario_HomeServer: result := 'HomeServer';
    Scenario_EC2: result := 'EC2';
    Scenario_BridgeClient: result := 'BridgeClient';
    Scenario_Webonly: result := 'Webonly';
    Scenario_OutputBridge: result := 'OutputBridge';
    else
      result := 'Unknown';
  end;
end;

class function TScenarioInfo.ParseInt(Value: Integer): TConfigScenario;
begin
  case Value of
    0: result := Scenario_Default;
    1: result := Scenario_Disconnected;
    2: result := Scenario_Readonly;
    3: result := Scenario_BridgeServer_LH;
    4: result := Scenario_BridgeServer;
    5: result := Scenario_HomeServer;
    6: result := Scenario_EC2;
    7: result := Scenario_BridgeClient;
    8: result := Scenario_Webonly;
    9: result := Scenario_OutputBridge;
    10: result := Scenario_ShowSelector;
    else
      result := Scenario_Disconnected;
  end;
end;

procedure TScenarioInfo.Load;
var
  fn: string;
begin
  if not ReadCmdLine then
  begin
    fn := 'Scenario.Config.txt';
    if FileExists(fn) then
    begin
      ReadTxtFile(fn)
    end
    else
    begin
      fn := InitDir + fn;
      if FileExists(fn) then
        ReadTxtFile(fn);
    end;
  end;
end;

function TScenarioInfo.InitDir: string;
var
  p: Integer;
  dn: string;
begin
  dn := ExtractFileDir(ParamStr(0));
  dn := IncludeTrailingPathDelimiter(dn);
  if DirectoryExists(dn) then
  begin
    p := Pos('Win32', dn);
    if p > 0 then
      result := LeftStr(dn, p - 1)
    else
      result := dn;
  end;
end;

function TScenarioInfo.ReadCmdLine: Boolean;
var
  i: Integer;
  SL: TStringList;
  s: string;
begin
  if ParamCount > 0 then
  begin
    //--Example CmdLine: FR62.exe Scenario=6 WorkspaceType=1 WorkspaceID=1
    SL := TStringList.Create;
    try
      for i := 1 to ParamCount do
      begin
        s := ParamStr(i);
        SL.Add(s);
      end;
      ReadParams(SL);
    finally
      SL.Free;
    end;
    result := Scenario <> Scenario_Default;
  end
  else
    result := false;
end;

procedure TScenarioInfo.ReadParams(SL: TStrings);
var
  s: string;
  i: Integer;
begin
  s := SL.Values['Scenario'];
  i := StrToIntDef(s, -1);
  if i > -1 then
  begin
    FScenario := ParseInt(i);
  end;

  s := SL.Values['PortSet'];
  FPortSet := StrToIntDef(s, 0);

  s := SL.Values['WantPolicyServer'];
  FWantPolicyServer := StrToIntDef(s, -1);
end;

procedure TScenarioInfo.ReadTxtFile(fn: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(fn);
  try
    ReadParams(SL);
  finally
    SL.Free;
  end;
end;

end.
