unit RiggVar.BO.Manager;

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

//{$define UseBOManagerMock}

{$ifdef UseBOManagerMock}
uses
  System.SysUtils,
  System.Classes,
  RiggVar.BO.Def,
  RiggVar.BO.Params;
{$endif}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.BO.Base,
  RiggVar.BO.Def,
  RiggVar.BO.NetworkAdapter,
  RiggVar.BO.Params,
  RiggVar.Conn.Def,
  RiggVar.Conn.Sockets,
  RiggVar.Util.PortTestCall;

type
  TBOManagerMock = class
  protected
    FConnected: Boolean;
    function GetConnected: Boolean;
    function GetBOConnected: Boolean;
  public
    destructor Destroy; override;
    procedure CreateBO(boParams: TBOParams);
    procedure DeleteBO;
    procedure ConnectBO;
    procedure DisconnectBO;
    property Connected: Boolean read GetConnected;
    property BOConnected: Boolean read GetBOConnected;
  end;

  TBOManager = class
  private
    FAdapterBO: TAdapterBO;
    FConnected: Boolean;
    function GetConnected: Boolean;
    function GetAdapterBO: TAdapterBO;
    function FindUsablePort(DefaultPort: Integer): Integer;
    function GetBOConnected: Boolean;
  public
    destructor Destroy; override;
    procedure CreateAdapterBO;
    procedure CreateBO(boParams: TBOParams);
    procedure DeleteBO;
    procedure ConnectBO;
    procedure DisconnectBO;
    property Connected: Boolean read GetConnected;
    property BOConnected: Boolean read GetBOConnected;
    property AdapterBO: TAdapterBO read GetAdapterBO;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.SDI;

{ TBOManagerMock }

destructor TBOManagerMock.Destroy;
begin
  DeleteBO;
  inherited Destroy;
end;

procedure TBOManagerMock.CreateBO(boParams: TBOParams);
var
  params: TBOParams;
begin
  if not Assigned(BO) then
  begin
    params := TBOParams.Create;
    params.Assign(boParams);
    params.ForceWithinLimits;
    BO := TSDIBO.Create(params);
    BO.Init;
  end;
end;

procedure TBOManagerMock.DeleteBO;
begin
  DisconnectBO;
  BO.Free;
  BO := nil;
end;

procedure TBOManagerMock.ConnectBO;
begin
end;

procedure TBOManagerMock.DisconnectBO;
begin
end;

function TBOManagerMock.GetBOConnected: Boolean;
begin
  result := FConnected;
end;

function TBOManagerMock.GetConnected: Boolean;
begin
  result := False;
end;

{ TBOManager }

destructor TBOManager.Destroy;
begin
  DeleteBO;
  FAdapterBO.Free;
  FAdapterBO := nil;
  inherited Destroy;
end;

procedure TBOManager.CreateBO(boParams: TBOParams);
var
  params: TBOParams;
begin
  if not Assigned(BO) then
  begin
    params := TBOParams.Create;
    params.Assign(boParams);
    params.ForceWithinLimits;
    BO := TSDIBO.Create(params);
    BO.Init;
  end;
end;

procedure TBOManager.CreateAdapterBO;
begin
  if not Assigned(FAdapterBO) then
  begin
    if not Main.Params.SkipTestForListeningSocket
      and Main.IniImage.SearchForUsablePorts then
    begin
      Main.IniImage.PortIn := FindUsablePort(Main.IniImage.PortIn);
      Main.IniImage.PortOut := FindUsablePort(Main.IniImage.PortOut);
    end;

    FAdapterBO := TAdapterBO.Create;

    Main.Logger.Info('InputServer.Port := ' + IntToStr(AdapterBO.InputServer.Server.Port) + ';');
    Main.Logger.Info('OutputServer.Port := ' + IntToStr(AdapterBO.OutputServer.Server.Port) + ';');
  end;
end;

procedure TBOManager.DeleteBO;
begin
  DisconnectBO;
  BO.Free;
  BO := nil;
end;

procedure TBOManager.ConnectBO;
begin
  if Assigned(BO) and Assigned(AdapterBO) then
  begin
    AdapterBO.AdapterInputConnection := BO.InputServer.Server.Connect('Adapter.Input');
    AdapterBO.AdapterOutputConnection := BO.OutputServer.Server.Connect('Adapter.Output');
    AdapterBO.AdapterOutputConnection.OnSendMsg := AdapterBO.ReceiveMsg;
    FConnected := true;
  end
  else
    DisconnectBO;
end;

procedure TBOManager.DisconnectBO;
begin
  if Assigned(AdapterBO) then
  begin
    AdapterBO.AdapterInputConnection := nil;
    AdapterBO.AdapterOutputConnection := nil;
  end;
  FConnected := false;
end;

function TBOManager.GetAdapterBO: TAdapterBO;
begin
  result := FAdapterBO;
end;

function TBOManager.GetBOConnected: Boolean;
begin
  result := FConnected;
end;

function TBOManager.GetConnected: Boolean;
begin
  if Assigned(AdapterBO) then
    result := (AdapterBO.InputServer.Server.Status = ServerStatus_Active)
      and (AdapterBO.OutputServer.Server.Status = ServerStatus_Active)
  else
    result := False;
end;

function TBOManager.FindUsablePort(DefaultPort: Integer): Integer;
var
  i, p: Integer;
  host: string;
begin
  host := '127.0.0.1';
  if not TestConnection(host, DefaultPort, 1000) then
    result := DefaultPort
  else
  begin
    i := 1;
    p := DefaultPort + 2;
    if p < 1024 then
      p := 1024;
    while (i < 100) and TestConnection(host, p, 1000) do
    begin
      Inc(i);
      Inc(p);
    end;
    result := p;
  end;
end;

end.

