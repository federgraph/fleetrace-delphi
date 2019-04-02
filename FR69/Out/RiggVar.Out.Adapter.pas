unit RiggVar.Out.Adapter;

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
  System.Classes,
  RiggVar.BO.Base,
  RiggVar.Out.Intf;

type
  TAdapterOutput = class(TInterfacedObject, IOutput)
  public
    function GetMsg(sRequest: string): string;
    function GetAll(OutputRequestList: TStringList): string;
  end;

implementation

uses
  RiggVar.App.Main;

{ TAdapterOutput }

function TAdapterOutput.GetAll(OutputRequestList: TStringList): string;
begin
  result := 'not implemented';
end;

function TAdapterOutput.GetMsg(sRequest: string): string;
begin
  if Assigned(Main)
  and Assigned(Main.AdapterBO)
  and Assigned(Main.AdapterBO.AdapterOutputConnection) then
    result := Main.AdapterBO.AdapterOutputConnection.HandleMsg(sRequest)
  else
    result := 'adapter not connected';
end;

end.
