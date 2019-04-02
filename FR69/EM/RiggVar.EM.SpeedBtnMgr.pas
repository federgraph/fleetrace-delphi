unit RiggVar.EM.SpeedBtnMgr;

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
  RiggVar.EM.SpeedBtn;

function BtnLayoutName(LayoutID: Integer): string;
function CreateBtnLayout(LayoutName: string): TItemLayout;
function CreateItemLayout(LayoutID: Integer): TItemLayout;

implementation

uses
  RiggVar.EM.SpeedBtn01,
  RiggVar.EM.SpeedBtn02,
  RiggVar.EM.SpeedBtn03;

function CreateBtnLayout(LayoutName: string): TItemLayout;
begin
  if LayoutName = 'ToolButton' then
    result := CreateItemLayout(1)
  else if LayoutName = 'SpeedBar' then
    result := CreateItemLayout(2)
  else if LayoutName = 'SpeedPanel' then
    result := CreateItemLayout(3)
  else
    result := TItemLayout.Create;
end;

function CreateItemLayout(LayoutID: Integer): TItemLayout;
begin
  case LayoutID of
   1: result := TToolButtonLayout.Create;
   2: result := TSpeedBarLayout.Create;
   3: result := TSpeedPanelLayout.Create
  else
   result := TItemLayout.Create;
  end;
end;

function BtnLayoutName(LayoutID: Integer): string;
begin
  case LayoutID of
   1: result := 'ToolButton';
   2: result := 'SpeedBar';
   3: result := 'SpeedPanel'
  else
   result := '';
  end;
end;

end.
