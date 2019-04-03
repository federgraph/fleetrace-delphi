unit RiggVar.EM.ConFile;

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
  RiggVar.EM.Connection;

type
  TFileCon = class(TEventMenuConnection)
  public
    function Get: string; override;
    procedure Post(const s: string); override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TEventMenuConnection }

function TFileCon.Get: string;
var
  SL: TStringList;
  fn: string;
begin
  result := '';
  fn := StringReplace(Url, '/', '\', []);
  if FileExists(fn) then
  begin
    SL := TStringList.Create;
    try
      try
        SL.LoadFromFile(fn);
        result := SL.Text;
        result := AdjustLineBreaks(result, tlbsCRLF);
      except
       on e: Exception do
        result := e.Message;
      end;
    finally
      SL.Free;
    end;
  end
  else
    result := 'file does not exit';
end;

procedure TFileCon.Post(const s: string);
var
  SL: TStringList;
  fn: string;
begin
  fn := StringReplace(Url, '/', '\', []);
  if FileExists(fn) then
  begin
    SL := TStringList.Create;
    SL.Text := s;
    try
      try
        //SL.SaveToFile(fn, TEncoding.UTF8);
        Main.StoreAdapter.StringListSaveToFileUTF8(SL, fn);
      except
      end;
    finally
      SL.Free;
    end;
  end;
end;

end.
