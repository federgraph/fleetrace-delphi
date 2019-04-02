unit RiggVar.BR.BridgeIndy;

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
  RiggVar.BR.BridgeSynchron;

type
  TIndyBridge = class(TSynchronBridge)
  protected
    procedure Post(msg: string); override;
    function Request(msg: string): string; override;
  public

  end;
implementation

uses
  RiggVar.App.Main,
  RiggVar.Conn.ClientMsg;

{ TIndyBridge }

procedure TIndyBridge.Post(msg: string);
begin
  RiggVar.Conn.ClientMsg.SendMsg(
    Main.IniImage.BridgeHost,
    Main.IniImage.BridgePort,
    msg,
    False,
    2000);
end;

function TIndyBridge.Request(msg: string): string;
begin
  result := RiggVar.Conn.ClientMsg.SendMsg(
    Main.IniImage.BridgeHost,
    Main.IniImage.BridgePort,
    msg,
    True,
    2000);
end;

end.
