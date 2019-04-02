unit RiggVar.BO.NetworkAdapter;

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
  Vcl.Dialogs,
  RiggVar.Out.Intf,
  RiggVar.Out.Adapter,
  RiggVar.BO.Watches,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.Conn.IO,
  RiggVar.Conn.Sockets;

type
  TAdapterBO = class
  private
    procedure Init;
  protected
    function GetWatches: TAdapterWatches;
  public
    AdapterInputConnection: TConnection;
    AdapterOutputConnection: TConnection;
    InputServer: TAdapterInputNCP;
    OutputServer: TAdapterOutputNCP;
    Output: IOutput;
    constructor Create;
    destructor Destroy; override;
    procedure ReceiveMsg(Sender: TObject; cm: TContextMsg);
    property Watches: TAdapterWatches read GetWatches;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.TemplateIDs;

{ TAdapterBO }

constructor TAdapterBO.Create;
begin
  Init;
end;

destructor TAdapterBO.Destroy;
begin
  InputServer.Free;
  InputServer := nil;
  OutputServer.Free;
  OutputServer := nil;
  Output := nil;
  inherited;
end;

function TAdapterBO.GetWatches: TAdapterWatches;
begin
  result := GlobalWatches;
end;

procedure TAdapterBO.Init;
var
  ts: TBaseServer;
begin
  inherited;
  try
    ts := CreateServer(Main.IniImage.PortIn, Function_Input);
    InputServer := TAdapterInputNCP.Create(ts);

    ts := CreateServer(Main.IniImage.PortOut, Function_Output);
    OutputServer := TAdapterOutputNCP.Create(ts);
  except
    InputServer.Free;
    InputServer := nil;
    OutputServer.Free;
    OutputServer := nil;
    ShowMessage('cannot open Sockets');
  end;
  Output := TAdapterOutput.Create;
end;

procedure TAdapterBO.ReceiveMsg(Sender: TObject; cm: TContextMsg);
begin
  //if connected
  if Assigned(AdapterOutputConnection) then
  begin
    //while processing a switch msg,
    //when multicasting to internally connected clients,
    //block the msg from going out
    if not cm.IsSwitchMsg then
    begin
      //OutputServer.SendMsg(cm);

      GlobalWatches.MsgOut := cm.msg;
      cm.MsgType := MsgTypeInput;
      OutputServer.Server.SendMsg(cm);

    end;
  end;
end;

end.

