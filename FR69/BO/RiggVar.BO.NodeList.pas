unit RiggVar.BO.NodeList;

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
  RiggVar.Grid.ColGrid;

type
  TBaseNodeList = class
  private
    SL: TStringList;
    function GetNode(NodeName: string): TBaseNode;
  public
    CurrentIdx: Integer;
    OnCalc: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    function First: TBaseNode;
    function Next: TBaseNode;
    procedure Add(aNode: TBaseNode);
    function NodeAt(Idx: Integer): TBaseNode;
    property Node[NodeName: string]: TBaseNode read GetNode;
    function Count: Integer;
  end;

implementation

uses
  RiggVar.Col.Race;

{ TBaseNodeList }

constructor TBaseNodeList.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TBaseNodeList.Destroy;
begin
  SL.Free;
  inherited;
end;

function TBaseNodeList.GetNode(NodeName: string): TBaseNode;
var
  i: Integer;
begin
  result := nil;
  i := SL.IndexOf(NodeName);
  if i > -1 then
    result := SL.Objects[i] as TBaseNode;
end;

function TBaseNodeList.First: TBaseNode;
begin
  CurrentIdx := 0;
  result := NodeAt(CurrentIdx);
end;

function TBaseNodeList.Next: TBaseNode;
begin
  Inc(CurrentIdx);
  result := NodeAt(CurrentIdx);
end;

function TBaseNodeList.NodeAt(Idx: Integer): TBaseNode;
begin
  result := nil;
  if SL.Count > Idx then
    result := SL.Objects[Idx] as TBaseNode;
end;

procedure TBaseNodeList.Add(aNode: TBaseNode);
begin
  if Assigned(aNode) then
  begin
    SL.AddObject(aNode.NameID, aNode);
    if aNode is TRaceNode then
      TRaceNode(aNode).OnCalc := OnCalc;
  end;
end;

function TBaseNodeList.Count: Integer;
begin
  result := SL.Count;
end;

end.
