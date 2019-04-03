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

{$define UseBOManagerMock}

{$ifdef UseBOManagerMock}
uses
  System.SysUtils,
  System.Classes,
  RiggVar.BO.Def,
  RiggVar.BO.Params;
{$endif}

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

  TBOManager = TBOManagerMock;

implementation

uses
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

end.

