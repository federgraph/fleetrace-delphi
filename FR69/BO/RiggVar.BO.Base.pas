unit RiggVar.BO.Base;

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
  RiggVar.Grid.ColBase,
  RiggVar.Conn.IO,
  RiggVar.BO.MsgBase,
  RiggVar.Out.Intf,
  RiggVar.BO.Watches;

type
  TBaseBO = class(TBaseObject)
  private
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  protected
    function GetWatches: TAdapterWatches; virtual; abstract;
  public
    CounterMsgHandled: Integer;
    InputServer: TInputNCP;
    OutputServer: TOutputNCP;
    Output: IOutput;
    destructor Destroy; override;

    function Calc: Boolean; virtual;
    function NewMsg: TBaseMsg; virtual;

    procedure Connect;
    procedure Disconnect;
    property Connected: Boolean read GetConnected write SetConnected;
    property Watches: TAdapterWatches read GetWatches;
  end;

implementation

uses
  RiggVar.App.Main;

{ TBaseBO }

function TBaseBO.Calc: Boolean;
begin
  result := false; //not implemented
end;

procedure TBaseBO.Connect;
begin
  Main.BOManager.ConnectBO;
end;

procedure TBaseBO.Disconnect;
begin
  Main.BOManager.DisconnectBO;
end;

procedure TBaseBO.SetConnected(const Value: Boolean);
begin
  if not Value then
    Disconnect
  else if not Connected then
    Connect;
end;

function TBaseBO.GetConnected: Boolean;
begin
  result := Main.BOManager.BOConnected;
end;

destructor TBaseBO.Destroy;
begin
  InputServer.Free;
  InputServer := nil;
  OutputServer.Free;
  OutputServer := nil;
  Output := nil;
  inherited;
end;

function TBaseBO.NewMsg: TBaseMsg;
begin
  Assert(false, 'TBaseMsg.NewMsg is abstract, should never be called.');
  result := nil;
end;

end.
