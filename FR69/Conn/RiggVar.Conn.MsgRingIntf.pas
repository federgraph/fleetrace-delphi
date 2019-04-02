unit RiggVar.Conn.MsgRingIntf;

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
  System.Classes;

type
  TMsgRing = class
  private
    FUseMerge: Boolean;
    procedure SetUseMerge(const Value: Boolean);
  protected
    FCapacity: Integer;
  public
    MsgID: Integer;
    constructor Create(Capacity: Integer);
    procedure GetDelta(SwitchID: Integer; DiffLog: TStrings; StartMsgID: Integer); virtual; abstract;
    procedure AddMsg(SwitchID: Integer; s: string); virtual; abstract;
    function LogValid(): Boolean; virtual;
    procedure Clear; virtual; abstract;
    property UseMerge: Boolean read FUseMerge write SetUseMerge;
  end;

implementation

{ TMsgRing }

constructor TMsgRing.Create(Capacity: Integer);
begin
  if Capacity < 10 then
    FCapacity := 10
  else
    FCapacity := Capacity;
end;

function TMsgRing.LogValid(): Boolean;
begin
  result := false;
end;

procedure TMsgRing.SetUseMerge(const Value: Boolean);
begin
  FUseMerge := Value;
end;

end.
