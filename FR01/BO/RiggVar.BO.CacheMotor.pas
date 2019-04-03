unit RiggVar.BO.CacheMotor;

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

{$define NoColCache}

{$ifdef NoColCache}
uses
  System.SysUtils,
  RiggVar.Conn.Intern;
{$endif}

type
  TCacheMotorMock = class
  protected
    function DoRequest3(RequestString: string): string; virtual;
    function GetFinishReport: string;
    function GetPointsReport: string;
  public
    InputConnection: TConnection;
{$ifdef NoColCache}
    CacheRequestToken: string;
{$endif}
    CacheEnabled: Boolean;
    constructor Create;
    procedure DoOnIdle; virtual;
    procedure Synchronize; virtual;
    procedure SwapEvent; virtual;
    property FinishReport: string read GetFinishReport;
    property PointsReport: string read GetPointsReport;
  end;

implementation

{ TCacheMotorMock }

constructor TCacheMotorMock.Create;
begin
  CacheRequestToken := 'FR.*.Request.';
end;

function TCacheMotorMock.GetFinishReport: string;
var
  s: string;
begin
  s := Format('%sReport.FinishReport', [CacheRequestToken]);
  result := DoRequest3(s);
end;

function TCacheMotorMock.GetPointsReport: string;
var
  s: string;
begin
  s := Format('%sReport.PointsReport', [CacheRequestToken]);
  result := DoRequest3(s);
end;

procedure TCacheMotorMock.DoOnIdle;
begin

end;

procedure TCacheMotorMock.Synchronize;
begin

end;

procedure TCacheMotorMock.SwapEvent;
begin

end;

function TCacheMotorMock.DoRequest3(RequestString: string): string;
var
  answer: string;
begin
  if InputConnection <> nil then
  begin
    answer := InputConnection.HandleMsg(RequestString);
  end
  else
  begin
    answer := '<DoRequest3>no connection</DoRequest3>';
  end;
  result := answer;
end;

end.
