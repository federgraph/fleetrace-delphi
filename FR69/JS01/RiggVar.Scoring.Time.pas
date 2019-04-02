unit RiggVar.Scoring.Time;

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
  System.SysUtils;

type
  SailTime = class
    class function Int64ToString(dt: Int64): string;
    class function ForceToLong(dt: string): Int64;
  end;

implementation

{ SailTime }

class function SailTime.ForceToLong(dt: string): Int64;
begin
  result := StrToIntDef(dt, 0);
end;

class function SailTime.Int64ToString(dt: Int64): string;
begin
  result := IntToStr(dt);
end;

end.

