unit RiggVar.Web1.TW00;

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
  SysUtils, Classes,
  RiggVar.Web1.EventArgs;

type
  TTW00 = class
  protected
    procedure WriteTimePointSelectorForm(RaceCount, ITCount, race, it: Integer; action: string);
    procedure WriteSelectorForm(RaceCount, ITCount, race, it: Integer; action: string); virtual;
  public
    HL: TStrings;
    SL: TStrings;
    constructor Create;
    procedure WriteWidget(RaceCount, ITCount, race, it: Integer); virtual;
    procedure WriteSelector(RaceCount, ITCount, race, it: Integer); virtual;
    procedure WriteInfo; virtual;
  end;

implementation

constructor TTW00.Create;
begin
  inherited Create;
end;

procedure TTW00.WriteWidget(RaceCount, ITCount, race, it: Integer);
begin
  //virtual
end;

procedure TTW00.WriteInfo;
begin
  //virtual
end;

procedure TTW00.WriteSelector(RaceCount, ITCount, race, it: Integer);
begin

end;

procedure TTW00.WriteSelectorForm(RaceCount, ITCount, race, it: Integer; action: string);
begin
HL.Add('<meta name="viewport" content="width=200" />');

SL.Add('<p>select a TimePoint:</p>');
WriteTimePointSelectorForm(RaceCount, ITCount, race, it, action);
end;

procedure TTW00.WriteTimePointSelectorForm(
  RaceCount, ITCount, race, it: Integer; action: string);
var
  i: Integer;
begin
  SL.Add('<div id="timepoint-selector">');
  SL.Add(Format('<form method="get" action="%s">', [action]));
	SL.Add('<table id="BtnTable" border="1" cellpadding="1" cellspacing="1">');
	SL.Add('<tr><td>Race</td><td>IT</td><td>&nbsp;</td></tr>');
	SL.Add('<tr><td><select name="Race">');
  for i := 1 to RaceCount do
  begin
    if i = race then
    	SL.Add(Format('<option selected="selected" value="%d">%d</option>', [i, i]))
    else
    	SL.Add(Format('<option value="%d">%d</option>', [i, i]));
  end;
  SL.Add('</select></td><td><select name="IT">');
  for i := 0 to ITCount do
  begin
    if i = it then
    	SL.Add(Format('<option selected="selected" value="%d">%d</option>', [i, i]))
    else
    	SL.Add(Format('<option value="%d">%d</option>', [i, i]));
  end;
  SL.Add('</select></td><td><input type="submit" name="SubmitBtn" value="Show"/></td></tr>');
	SL.Add('</table></form>');
  SL.Add('</div>');
end;

end.
