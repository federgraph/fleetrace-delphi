unit RiggVar.EM.Collection;

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
  RiggVar.EM.Intf;

type
  TEventMenuCollection = class
  private
    SL: TStringList;
    function GetNode(NodeName: string): IEventMenu;
  public
    CurrentIdx: Integer;
    OnCalc: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function First: IEventMenu;
    function Next: IEventMenu;
    procedure Add(aNode: IEventMenu);
    function NodeAt(Idx: Integer): IEventMenu;
    property Node[NodeName: string]: IEventMenu read GetNode;
    function Count: Integer;
  end;

implementation

{ IEventMenuList }

constructor TEventMenuCollection.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TEventMenuCollection.Destroy;
begin
  Clear;
  SL.Free;
  inherited;
end;

function TEventMenuCollection.GetNode(NodeName: string): IEventMenu;
var
  i: Integer;
begin
  result := nil;
  i := SL.IndexOf(NodeName);
  if i > -1 then
    result := SL.Objects[i] as IEventMenu;
end;

function TEventMenuCollection.First: IEventMenu;
begin
  CurrentIdx := 0;
  result := NodeAt(CurrentIdx);
end;

function TEventMenuCollection.Next: IEventMenu;
begin
  Inc(CurrentIdx);
  result := NodeAt(CurrentIdx);
end;

function TEventMenuCollection.NodeAt(Idx: Integer): IEventMenu;
begin
  result := nil;
  if SL.Count > Idx then
    result := SL.Objects[Idx] as IEventMenu;
end;

procedure TEventMenuCollection.Add(aNode: IEventMenu);
begin
  if Assigned(aNode) then
  begin
    SL.AddObject(aNode.ComboCaption, aNode);
//    if aNode is IEventMenu then
//      IEventMenu(aNode).OnCalc := OnCalc;
  end;
end;

function TEventMenuCollection.Count: Integer;
begin
  result := SL.Count;
end;

procedure TEventMenuCollection.Clear;
var
  i: Integer;
begin
  for i := SL.Count - 1 downto 0 do
    NodeAt(i).Free;
  SL.Clear;
end;

end.
