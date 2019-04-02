unit RiggVar.Web1.FormBuilder;

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
  SysUtils, Classes;

type
  TWebFormBuilder = class
  public
    SL: TStrings;
    constructor Create(ASL: TStrings);
    procedure WriteFrmMenuSwap;
    procedure WriteOfflineMsg;
    procedure WriteEntryForm(msg: string; snr: Integer);
    procedure WriteManageCmdForm(msg: string);
    procedure WriteRaceForm(RaceCount, ITCount, race, it: Integer);
    procedure WriteRaceSelectorForm(RaceCount, race: Integer; action: string);
    procedure WriteRaceValueForm(msg: string);
    procedure WriteXmlReportLinks;
    procedure WriteInfo;
    procedure WriteWidgetMenu;
    procedure WriteEventSelectorForm(EventNameList: TStrings);
    procedure WriteChooseEventNameForSaveAs(en: string);
    procedure WriteChooseEventNameForDelete(EventNameList: TStrings);
    procedure WriteFrmWorkspace(WorkspaceType, WorkspaceID: Integer);
    procedure WriteFrmEventParams(RaceCount, ITCount, StartlistCount: Integer);
    procedure WriteFrmFleetProps(UseFleets: Boolean; TargetFleetSize, FirstFinalRace: Integer);
  end;

implementation

constructor TWebFormBuilder.Create(ASL: TStrings);
begin
  inherited Create;
  SL := ASL;
end;

procedure TWebFormBuilder.WriteFrmMenuSwap;
begin
  SL.Add('<h3>Select Menu</h3>');
  SL.Add('<ul>');
  SL.Add('<li><a href="menu?value=0">menu 1</a></li>');
  SL.Add('<li><a href="menu?value=1">menu 2</a></li>');
  SL.Add('<li><a href="menu?value=2">menu 3</a></li>');
  SL.Add('<li><a href="menu?value=3">menu 4</a></li>');
  SL.Add('</ul>');
end;

procedure TWebFormBuilder.WriteManageCmdForm(msg: string);
var
  f: string;
  s: string;
  seemsOK: Boolean;
begin
  SL.Add('<p>Send application commands to FR62.</p>');
  SL.Add('<div class="example">');
  SL.Add('<h3>Examples</h3>');
  SL.Add('<pre>');
  SL.Add('Manage.Backup');
  SL.Add('Manage.Restore');
  SL.Add('Manage.Save');
  SL.Add('Manage.Clear');
  SL.Add('</pre>');
  SL.Add('</div>');

  //check old msg
  seemsOK := (Length(msg) > 16)
    and (Length(msg) < 26)
    and (Pos('=', msg) > 0)
    and (Pos('Manage.', msg) = 1);

  //prepare content of msg text field
  if seemsOK then
    s := msg //last command issued
  else
    s := 'Manage.'; //default message template

  SL.Add('<br />');

  SL.Add('<form method="post" id="race-value-form">');
  //msg text field
  SL.Add('<p>');
  f := '<input type="text" name="msg" value="%s" maxlength="26" spellcheck="false"/>';
  SL.Add(Format(f, [s]));
  SL.Add('<label for="msg">msg</label>');
  SL.Add('</p>');
  //submit button
  SL.Add('<p>');
  SL.Add('<input type="submit" name="submit-btn" value="Send" spellcheck="false"/>');
  SL.Add('</p>');
  SL.Add('</form>');
end;

procedure TWebFormBuilder.WriteRaceValueForm(msg: string);
var
  f: string;
  s: string;
  seemsOK: Boolean;
begin
  //check last msg
  seemsOK := (Length(msg) > 16)
    and (Length(msg) < 26)
    and (Pos('.RV', msg) > 0)
    and (Pos('FR.', msg) = 0);

  //prepare content for msg text field
  if seemsOK then
    s := msg
  else
    s := 'FR.*.W1.Bib1.RV='; //default message template

  SL.Add('<p>Send RaceValue messages to FR62.</p>');
  SL.Add('<div class="example">');
  SL.Add('<h3>Examples</h3>');
  SL.Add('<pre>');
  SL.Add('FR.*.W1.Bib1.RV=2');
  SL.Add('FR.*.W1.Bib2.RV=dns');
  SL.Add('FR.*.W1.Bib2.RV=-f');
  SL.Add('</pre>');
  SL.Add('</div>');

  SL.Add('<br />');

  SL.Add('<form method="post" id="race-value-form">');
  //msg text field
  SL.Add('<p>');
  f := '<input type="text" name="msg" value="%s" maxlength="26" spellcheck="false"/>';
  SL.Add(Format(f, [s]));
  SL.Add('<label for="msg">msg</label>');
  SL.Add('</p>');
  //submit button
  SL.Add('<p>');
  SL.Add('<input type="submit" name="submit-btn" value="Send" spellcheck="false"/>');
  SL.Add('</p>');
  SL.Add('</form>');
end;

procedure TWebFormBuilder.WriteEntryForm(msg: string; snr: Integer);
var
  f: string;
  s: string;
  seemsOK: Boolean;
begin
  //check last msg
  seemsOK := (Length(msg) > 3)
    and (Length(msg) < 17)
    and (Pos('=', msg) > 0);

  //prepare content of msg text field
  if seemsOK then
    s := msg
  else
    s := 'N1=';

  SL.Add('<p>Post messages in Key=Value form.<br />');
  SL.Add('SNR must be a positive integer number.</p>');

  SL.Add('<div class="example">');
  SL.Add('<h3>Examples</h3>');
  SL.Add('<pre>');
  SL.Add('  position keys: N1, N2, N3, N4, N5, N6');
  SL.Add('     named keys: FN, LN, SN, NC, GR, PB');
  SL.Add('');
  SL.Add('F1=Paul');
  SL.Add('NC=GER');
  SL.Add('</pre>');
  SL.Add('</div>');

  SL.Add('<br />');

  SL.Add('<form method="post" id="entry-form">');
  //snr text field
  SL.Add('<p>');
  f := '<input type="text" name="snr" value="%d" size="4" maxlength="6" spellcheck="false"/>';
  SL.Add(Format(f, [snr]));
  SL.Add('<label for="snr">SNR (integer number &gt; 0)</label>');
  SL.Add('</p>');
  //msg text field
  SL.Add('<p>');
  f := '<input type="text" name="msg" value="%s" maxlength="16" spellcheck="false"/>';
  SL.Add(Format(f, [s]));
  SL.Add('<label for="msg">msg for SNR in format Key=Value.</label>');
  SL.Add('</p>');
  //submit button
  SL.Add('<p>');
  SL.Add('<input type="submit" name="submit-btn" value="Send" spellcheck="false"/>');
  SL.Add('</p>');
  SL.Add('</form>');

  SL.Add('<br />');
end;

procedure TWebFormBuilder.WriteRaceForm(RaceCount, ITCount, race, it: Integer);
var
  i: Integer;
begin
  SL.Add('<form method="get">');
	SL.Add('<table id="BtnTable" style="WIDTH: 224px; HEIGHT: 56px" cellspacing="1" cellpadding="1" width="224"	border="1">');
	SL.Add('<tr><td style="WIDTH: 74px">Race</td><td style="WIDTH: 71px">IT</td><td>&nbsp;</td></tr>');
	SL.Add('<tr><td style="WIDTH: 74px"><select name="Race" style="width:56px;">');
  for i := 1 to RaceCount do
  begin
    if i = race then
    	SL.Add(Format('<option selected="selected" value="%d">%d</option>', [i, i]))
    else
    	SL.Add(Format('<option value="%d">%d</option>', [i, i]));
  end;
  SL.Add('</select></td><td style="WIDTH: 71px"><select name="IT" style="width:58px;">');
  for i := 0 to ITCount do
  begin
    if i = it then
    	SL.Add(Format('<option selected="selected" value="%d">%d</option>', [i, i]))
    else
    	SL.Add(Format('<option value="%d">%d</option>', [i, i]));
  end;
  SL.Add('</select></td><td><input type="submit" name="Btn" value="Show" spellcheck="false"/></td></tr>');
	SL.Add('</table></form>');
end;

procedure TWebFormBuilder.WriteRaceSelectorForm(RaceCount, race: Integer; action: string);
var
  i: Integer;
begin
  SL.Add('<div id="timepoint-selector">');
  SL.Add(Format('<form method="get" action="%s">', [action]));
	SL.Add('<table id="BtnTable" ');
  SL.Add('  cellspacing="1" cellpadding="1" width="150"	border="1">');
	SL.Add('<tr><td>Race</td><td>&nbsp;</td></tr>');
	SL.Add('<tr><td><select name="Race" style="width:56px;">');
  for i := 1 to RaceCount do
  begin
    if i = race then
    	SL.Add(Format('<option selected="selected" value="%d">%d</option>', [i, i]))
    else
    	SL.Add(Format('<option value="%d">%d</option>', [i, i]));
  end;
  SL.Add('</select></td>');
  SL.Add('<td><input type="submit" name="Btn" value="Show" spellcheck="false"/></td></tr>');
	SL.Add('</table></form></div>');
end;

procedure TWebFormBuilder.WriteXmlReportLinks;
begin
  SL.Add('<p>');
  SL.Add('<a href="xml?Report=1001">Athletes</a><br>');
  SL.Add('<a href="xml?Report=1002">Event</a><br>');
  SL.Add('<a href="xml?Report=1003">Proxy Input</a><br>');
  SL.Add('<a href="xml?Report=1004">Proxy Output</a><br>');
  SL.Add('<a href="xml?Report=1005">JSXML</a><br>');
  SL.Add('<a href="xml?Report=1006">Data</a>');
  SL.Add('</p>');
end;

procedure TWebFormBuilder.WriteOfflineMsg;
begin
  SL.Add('<h2>FR62</h2>');
  SL.Add('<p>The WebApplication is currently in Offline mode.</p>');
  SL.Add('<p>On Desktop-Server: toggle Offline mode with PublishMenu/MenuItem.</p>');
  SL.Add('<p>ServerTime: %s</p>');
end;

procedure TWebFormBuilder.WriteInfo;
begin
  SL.Add('<h3>About RiggVar</h3>');
  SL.Add('<p><img src="images/rgg_anigif_01.gif" alt="RiggVar-AniGif" /></p>');
end;

procedure TWebFormBuilder.WriteWidgetMenu;
begin
SL.Add('<ul>');

SL.Add('<li><a href="tw01">TW01</a>');
SL.Add('<p></p>');
SL.Add('</li>');

SL.Add('<li><a href="tw02">TW02</a>');
SL.Add('<p></p>');
SL.Add('</li>');

SL.Add('<li><a href="tw03">TW03</a>');
SL.Add('<p></p>');
SL.Add('</li>');

SL.Add('</ul>');
end;

procedure TWebFormBuilder.WriteEventSelectorForm(EventNameList: TStrings);
var
  i: Integer;
begin
  SL.Add('<div id="event-selector">');
  SL.Add('<form method="post" action="open">');
  SL.Add('<p>Select EventName:</p>');
  SL.Add('<select name="EventName">');
  for i := 0 to EventNameList.Count-1 do
  begin
    SL.Add(Format('<option>%s</option>', [EventNameList[i]]));
  end;
  SL.Add('</select>');
  SL.Add('<input type="submit" name="Btn" value="Load" spellcheck="false"/>');
  SL.Add('</form>');
  SL.Add('</div>');
end;

procedure TWebFormBuilder.WriteChooseEventNameForSaveAs(en: string);
begin
  SL.Add('<div id="event-name-selector">');
  SL.Add('<form method="post" action="saveas">');
  SL.Add('<p>Choose new EventName:</p>');
  SL.Add(Format('<input type="text" name="EventName" value="%s" spellcheck="false"/>', [en]));
  SL.Add('<input type="submit" name="Btn" value="Save" spellcheck="false"/>');
  SL.Add('</form>');
  SL.Add('</div>');
end;

procedure TWebFormBuilder.WriteChooseEventNameForDelete(EventNameList: TStrings);
var
  i: Integer;
begin
  SL.Add('<div id="event-selector">');
  SL.Add('<form method="post" action="delete">');
  SL.Add('<p>Select Event to delete:</p>');
  SL.Add('<select name="EventName">');
  for i := 0 to EventNameList.Count-1 do
  begin
    SL.Add(Format('<option>%s</option>', [EventNameList[i]]));
  end;
  SL.Add('</select>');
  SL.Add('<input type="submit" name="Btn" value="Delete" spellcheck="false"/>');
  SL.Add('</form>');
  SL.Add('</div>');
end;

procedure TWebFormBuilder.WriteFrmWorkspace(WorkspaceType, WorkspaceID: Integer);
var
  name: string;
  value: Integer;
  text: string;
  checked: boolean;
  i: Integer;
begin
  //radio buttons: if value is remains set to 0 the item is not displayed

  SL.Add('<div id="workspace-selector">');
  SL.Add('<form method="post" action="workspace-location">');
  SL.Add('<h4>Type</h4>');
  SL.Add('<p>');
  for i := 1 to 6 do
  begin
    value := 0;
    case i of
      1: begin
//        if IsLibrary then
//        begin
          name := 'WorkspaceType';
          value := 1;
          text := 'Shared FS';
//        end;
      end;
      2: begin
        name := 'WorkspaceType';
        value := 2;
        text := 'AppLocal FS';
      end;
      3: begin
        name := 'WorkspaceType';
        value := 6;
        text := 'Fixed FS';
      end;
      4: begin
        name := 'WorkspaceType';
        value := 3;
        text := 'Local DB';
      end;
      5: begin
        name := 'WorkspaceType';
        value := 4;
        text := 'Remote DB';
      end;
      6: begin
        name := 'WorkspaceType';
        value := 5;
        text := 'Web Service';
      end;
    end;
    if value > 0 then
    begin
      checked := value = WorkspaceType;
      if checked then
      begin
        SL.Add(Format('<input type="radio" name="%s" value="%d" checked="true" spellcheck="false"/> %s<br/>',
        [name, value, text]))
      end
      else
      begin
        SL.Add(Format('<input type="radio" name="%s" value="%d" spellcheck="false"/> %s<br/>',
        [name, value, text]))
      end;
    end;
  end;
  SL.Add('</p>');
  SL.Add('<h4>ID</h4>');
  SL.Add(Format('<p><input type="text" name="WorkspaceID" value="%d" size="6" spellcheck="false"/></p>', [WorkspaceID]));
  SL.Add('<p><input type="submit" name="Btn" value="OK" spellcheck="false"/>');
  SL.Add('<input type="submit" name="Btn" value="Cancel" spellcheck="false"/></p>');
  SL.Add('</form>');
  SL.Add('</div>');
end;

procedure TWebFormBuilder.WriteFrmEventParams(RaceCount, ITCount,
  StartlistCount: Integer);
begin
  SL.Add('<div id="event-params">');
  SL.Add('<form method="post" action="event-params">');
  SL.Add('<h3>Event Params</h3>');

  SL.Add('<h4>RaceCount</h4>');
  SL.Add(Format('<p><input type="text" name="RaceCount" value="%d" maxlength="2" size="2" spellcheck="false"/></p>', [RaceCount]));

  SL.Add('<h4>ITCount</h4>');
  SL.Add(Format('<p><input type="text" name="ITCount" value="%d" maxlength="2" size="2" spellcheck="false"/></p>', [ITCount]));

  SL.Add('<h4>StartlistCount</h4>');
  SL.Add(Format('<p><input type="text" name="StartlistCount" value="%d" maxlength="4" size="4" spellcheck="false"/></p>', [StartlistCount]));

  SL.Add('<p><input type="submit" name="Btn" value="OK" spellcheck="false"/>');
  SL.Add('<input type="submit" name="Btn" value="Cancel" spellcheck="false"/></p>');

  SL.Add('</form>');
  SL.Add('</div>');
end;

procedure TWebFormBuilder.WriteFrmFleetProps(
UseFleets: Boolean; TargetFleetSize, FirstFinalRace: Integer);
begin
  SL.Add('<div id="fleet-properties">');
  SL.Add('<form method="post" action="fleet-props">');
  SL.Add('<h3>Fleet Properties</h3>');

  SL.Add('<dl>');

  SL.Add('<dt>UseFleets</dt>');
  if UseFleets then
    SL.Add('<dd><input type="checkbox" name="UseFleets" checked="checked" spellcheck="false"/></dd>')
  else
    SL.Add('<dd><input type="checkbox" name="UseFleets" spellcheck="false"/></dd>');

  SL.Add('<dt>TargetFleetSize</dt>');
  SL.Add(Format('<dd><input type="text" name="TargetFleetSize" value="%d" maxlength="4" size="4" spellcheck="false"/></dd>', [TargetFleetSize]));

  SL.Add('<dt>FirstFinalRace</dt>');
  SL.Add(Format('<dd><input type="text" name="FirstFinalRace" value="%d" maxlength="2" size="2" spellcheck="false"/></dd>', [FirstFinalRace]));

  SL.Add('</dl>');

  SL.Add('<p><input type="submit" name="Btn" value="OK" spellcheck="false"/>');
  SL.Add('<input type="submit" name="Btn" value="Cancel" spellcheck="false"/></p>');

  SL.Add('</form>');
  SL.Add('</div>');
end;

end.
