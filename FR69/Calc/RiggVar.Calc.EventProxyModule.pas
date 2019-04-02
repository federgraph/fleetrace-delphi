unit RiggVar.Calc.EventProxyModule;

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
  RiggVar.Calc.EventProxy00;

type
  TScoreRegattaProc = procedure(p: TFRProxy);

  TScoringModule = class
  private
    FLoadCounter: Integer;
    FDLLName: string;
    FIsReady: Boolean;
    FLibLoaded: Boolean;
    LibHandle: THandle;
    ScoreRegattaProc: TScoreRegattaProc;
    procedure LoadLib;
    function GetIsReady: Boolean;
    function GetDLLName: string;
    procedure UnLoadLib;
  public
    constructor Create(ScoringModuleName: string);
    destructor Destroy; override;
    procedure ScoreRegatta(p: TFRProxy);
    property IsReady: Boolean read GetIsReady;
    property DLLName: string read GetDLLName;
    property LoadCounter: Integer read FLoadCounter;
  end;

implementation

uses
  SysUtils,
  Windows;

{ TRegattaScorer }

constructor TScoringModule.Create(ScoringModuleName: string);
var
  fn: string;
begin
  inherited Create;

  fn := ExtractFilePath(ParamStr(0));
  FDLLName := IncludeTrailingPathDelimiter(fn) + 'JS01.dll'; //default

  if FileExists(ScoringModuleName)
    and (ExtractFileExt(ScoringModuleName) = '.dll')
  then
  begin
    FDLLName := ScoringModuleName;
  end
end;

destructor TScoringModule.Destroy;
begin
  UnloadLib;
  inherited Destroy;
end;

function TScoringModule.GetDLLName: string;
begin
  result := FDLLName;
end;

function TScoringModule.GetIsReady: Boolean;
begin
  result := FIsReady and FLibLoaded;
end;

procedure TScoringModule.LoadLib;
begin
  Inc(FLoadCounter);
  LibHandle := LoadLibrary(PChar(FDLLName));
  if LibHandle > 0 then
  begin
    FLibLoaded := True;
    @ScoreRegattaProc := GetProcAddress(LibHandle, 'ScoreRegatta');
    if @ScoreRegattaProc <> nil then
    begin
      FIsReady := True;
    end;
  end;
end;

procedure TScoringModule.UnLoadLib;
begin
  if LibHandle > 0 then
  begin
    FreeLibrary(LibHandle);
    FLibLoaded := False;
    FIsReady := False;
    LibHandle := 0;
    @ScoreRegattaProc := nil;
    //FLoadCounter := 0;
  end;
end;

procedure TScoringModule.ScoreRegatta(p: TFRProxy);
begin
  //try to load lib only once
  if FLoadCounter = 0 then
  begin
    LoadLib;
  end;

  if IsReady then
  begin
    ScoreRegattaProc(p);
  end;
end;

end.
