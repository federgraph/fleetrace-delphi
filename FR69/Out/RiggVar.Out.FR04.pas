unit RiggVar.Out.FR04;

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
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  RiggVar.Out.FR03;

type
  TOutput4 = class(TOutput3)
  public
    procedure GetMsg(aSL: TStrings; PathInfo: string);
  end;

implementation

uses
  RiggVar.BO.Def;

{ TOutput4 }

procedure TOutput4.GetMsg(aSL: TStrings; PathInfo: string);
var
  i: Integer;
begin
  SortColName := 'col_Bib';
  Node.Layout := 1;

  GetContent(PathInfo);
  for i := 0 to SL.Count-1 do
    aSL.Add(SL[i]);
end;

end.
