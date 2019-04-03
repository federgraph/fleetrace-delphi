unit RiggVar.Scoring.Utils;

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
  RiggVar.Grid.ColBase;

type
  Utils = class
  private
  public
    class procedure WriteStackTrace(e: Exception);
    class procedure WriteLine(s: string);
    class function CompareWithNull(left, right: TBaseObject): Integer;
    class function EqualsWithNull(left, right: TBaseObject): Boolean;
    class function IsTrue(Value: string): Boolean;
  end;

var
  DebugIsOn: Boolean = false;
  TraceIsOn: Boolean = false;

implementation

uses
  Winapi.Windows;

{ Utils }

class function Utils.CompareWithNull(left, right: TBaseObject): Integer;
begin
  if (left = nil) then
  begin
    if (right = nil) then
      result := 0
    else
    begin
      result := 1;
    end;
  end
  else if (right = nil) then
    result := -1
  else
    result := left.compareTo(right);
end;

class function Utils.EqualsWithNull(left, right: TBaseObject): Boolean;
begin
  try
    result := left.Equals(right);
  except //NullReferenceException
    result := right = nil;
  end;
end;

class procedure Utils.WriteLine(s: string);
begin
  if DebugIsOn then
    Winapi.Windows.OutputDebugString(PChar(s));
end;

class procedure Utils.WriteStackTrace(e: Exception);
begin
  if DebugIsOn then
    Winapi.Windows.OutputDebugString(PChar(e.Message));
end;

class function Utils.IsTrue(Value: string): Boolean;
begin
  result := UpperCase(Value) = 'TRUE';
end;

end.
