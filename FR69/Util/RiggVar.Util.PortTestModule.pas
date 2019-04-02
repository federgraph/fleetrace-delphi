unit RiggVar.Util.PortTestModule;

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

type
  TPortTestFunction = function(Host: string; Port: Integer; Timeout: Integer): Boolean;

  TPortTesterModule = class
  private
    FLoadCounter: Integer;
    FDLLName: string;
    FIsReady: Boolean;
    FLibLoaded: Boolean;
    LibHandle: THandle;
    PortTestFunction: TPortTestFunction;
    procedure LoadLib;
    function GetIsReady: Boolean;
    function GetDLLName: string;
    procedure UnLoadLib;
  public
    constructor Create(ModuleName: string);
    destructor Destroy; override;
    function TestConnection(Host: string; Port, Timeout: Integer): Boolean;
    property IsReady: Boolean read GetIsReady;
    property DLLName: string read GetDLLName;
    property LoadCounter: Integer read FLoadCounter;
  end;

implementation

uses
  SysUtils,
  Windows;

{ TPortTester }

constructor TPortTesterModule.Create(ModuleName: string);
begin
  inherited Create;

  FDLLName := 'PT01.dll'; //default

  if FileExists(ModuleName)
    and (ExtractFileExt(ModuleName) = '.dll')
  then
  begin
    FDLLName := ModuleName;
  end
end;

destructor TPortTesterModule.Destroy;
begin
  UnloadLib;
  inherited Destroy;
end;

function TPortTesterModule.GetDLLName: string;
begin
  result := FDLLName;
end;

function TPortTesterModule.GetIsReady: Boolean;
begin
  result := FIsReady and FLibLoaded;
end;

procedure TPortTesterModule.LoadLib;
begin
  Inc(FLoadCounter);
  LibHandle := LoadLibrary(PChar(FDLLName));
  if LibHandle > 0 then
  begin
    FLibLoaded := True;
    @PortTestFunction := GetProcAddress(LibHandle, 'TestConnection');
    if @PortTestFunction <> nil then
    begin
      FIsReady := True;
    end;
  end;
end;

procedure TPortTesterModule.UnLoadLib;
begin
  if LibHandle > 0 then
  begin
    FreeLibrary(LibHandle);
    FLibLoaded := False;
    FIsReady := False;
    LibHandle := 0;
    @PortTestFunction := nil;
    //FLoadCounter := 0;
  end;
end;

function TPortTesterModule.TestConnection(Host: string; Port: Integer; Timeout: Integer): Boolean;
begin
  result := false;
  //try to load lib only once
  if FLoadCounter = 0 then
  begin
    LoadLib;
  end;

  if IsReady then
  begin
    result := PortTestFunction(Host, Port, Timeout);
  end;
end;

end.
