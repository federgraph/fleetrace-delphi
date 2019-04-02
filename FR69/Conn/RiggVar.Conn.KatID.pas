unit RiggVar.Conn.KatID;

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
  RiggVar.Conn.Def,
  RiggVar.BO.TemplateIDs;

type
  TKatIDServer = class(TBaseServer)
  private
    KatID: Integer;
  public
    procedure InjectMsg(Sender: TObject; s: string); override;
  end;

implementation

{ TKatIDServer }

procedure TKatIDServer.InjectMsg(Sender: TObject; s: string);
begin
  if Copy(s, 1, 24) = #2'RiggVar.RegisterClient.' then
    KatID := TemplateIDByName(Copy(s, 24+1, Length(s)-1-24))
  else
    inherited;
end;

end.
