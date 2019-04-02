unit RiggVar.Util.ItemCollection;

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
  TItem = class
  public
    function Key: string; virtual; abstract;
  end;

  TItemCollection<T: TItem> = class
  private
    SL: TStringList;
    function GetNode(NodeName: string): T;
  public
    CurrentIdx: Integer;
    OnCalc: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function First: T;
    function Next: T;
    procedure Add(aNode: T);
    function NodeAt(Idx: Integer): T;
    property Node[NodeName: string]: T read GetNode;
    function Count: Integer;
  end;

implementation

{ TList }

constructor TItemCollection<T>.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TItemCollection<T>.Destroy;
begin
  Clear;
  SL.Free;
  inherited;
end;

function TItemCollection<T>.GetNode(NodeName: string): T;
var
  i: Integer;
begin
  result := Default(T);
  i := SL.IndexOf(NodeName);
  if i > -1 then
    result := T(SL.Objects[i]);
end;

function TItemCollection<T>.First: T;
begin
  CurrentIdx := 0;
  result := NodeAt(CurrentIdx);
end;

function TItemCollection<T>.Next: T;
begin
  Inc(CurrentIdx);
  result := NodeAt(CurrentIdx);
end;

function TItemCollection<T>.NodeAt(Idx: Integer): T;
begin
  result := Default(T);
  if SL.Count > Idx then
    result := T(SL.Objects[Idx]);
end;

procedure TItemCollection<T>.Add(aNode: T);
begin
  if Assigned(aNode) then
  begin
    SL.AddObject(aNode.Key, aNode);
//    if aNode is T then
//      T(aNode).OnCalc := OnCalc;
  end;
end;

function TItemCollection<T>.Count: Integer;
begin
  result := SL.Count;
end;

procedure TItemCollection<T>.Clear;
var
  i: Integer;
begin
  for i := SL.Count - 1 downto 0 do
    NodeAt(i).Free;
  SL.Clear;
end;

end.
