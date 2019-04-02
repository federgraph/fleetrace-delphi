unit RiggVar.Web1.TW08;

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
  RiggVar.Web1.TW00;

type
  TTW08 = class(TTW00)
  public
    procedure WriteWidget(RaceCount, ITCount, race, it: Integer); override;
    procedure WriteSelector(RaceCount, ITCount, race, it: Integer); override;
    procedure WriteInfo; override;
    procedure WriteEmbeddedInfo;
    procedure WriteEmbeddedSelector(RaceCount, ITCount, race, it: Integer);
    procedure WriteEmbeddedWidget(RaceCount, ITCount, race, it: Integer);
  end;

implementation

procedure TTW08.WriteWidget(RaceCount, ITCount, race, it: Integer);
begin
SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
SL.Add('<html>');
SL.Add('<head>');
SL.Add('<title>tw08</title>');
SL.Add('<link rel="stylesheet" href="stylesheets/rvtw08.css" type="text/css" />');
SL.Add('<script type="text/javascript" src="javascripts/core.js"></script>');
SL.Add('<script type="text/javascript" src="javascripts/rvtw08.js"></script>');
SL.Add('<meta name="viewport" content="user-scalable=no, width = 320" />');
SL.Add('<meta name="description" content="FR timing widget for iPod Touch." />');
SL.Add('</head>');
SL.Add('<body>');
SL.Add('');
SL.Add('<div id="widget">');
SL.Add('');
SL.Add('<div id="timepoint-params">');
SL.Add(Format('<span id="race-param">%d</span>', [race]));
SL.Add(Format('<span id="it-param">%d</span>', [it]));
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="widget-caption">');
SL.Add(Format('<h3>R%d - IT%d</h3>', [race, it]));
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="ziffern-block">');
SL.Add('<table>');
SL.Add('<tr>');
SL.Add('<td id="bib" colspan="2">bib</td>');
SL.Add('<td id="clear-bib">clear</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b00">0</td>');
SL.Add('<td id="record" colspan="2">record</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b01">1</td>');
SL.Add('<td id="b02">2</td>');
SL.Add('<td id="b03">3</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b04">4</td>');
SL.Add('<td id="b05">5</td>');
SL.Add('<td id="b06">6</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b07">7</td>');
SL.Add('<td id="b08">8</td>');
SL.Add('<td id="b09">9</td>');
SL.Add('</tr>');
SL.Add('</table>');
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="response-text">response-text</div>');
SL.Add('');
SL.Add('<div id="button-panel">');
SL.Add('<a href="index">home</a> |');
SL.Add('<a href="tw08-selector">Select TP</a> |');
SL.Add('<a href="tw08-info">Info</a>');
SL.Add('</div>');
SL.Add('');
SL.Add('</div>');
SL.Add('');
SL.Add('</body>');
SL.Add('</html>');
end;

procedure TTW08.WriteSelector(RaceCount, ITCount, race, it: Integer);
begin
  WriteSelectorForm(RaceCount, ITCount, race, it, 'tw08-widget');
end;

procedure TTW08.WriteInfo;
begin
SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
SL.Add('<html xmlns="http://www.w3.org/1999/xhtml">');
SL.Add('<head>');
SL.Add('<title>tw08</title>');
SL.Add('<meta name="viewport" content="width=160" />');
SL.Add('</head>');
SL.Add('<body>');
SL.Add('<p>This is the TW08 Info page:</p>');
SL.Add('<ul>');

SL.Add('<li>Make sure that Race and IT (Intermediate TimePoint) match');
SL.Add('the TimePoint you are sending messages for. IT0=Finish.</li>');

SL.Add('<li>Use numbered buttons to select the bib number.</li>');

SL.Add('<li>A single click on the record button triggers');
SL.Add('a msg to the server via ajax.</li>');

SL.Add('<li>The time for the bib is generated on the server');
SL.Add('and shown below the keypad when the ajax-call returns.</li>');

SL.Add('</ul>');
SL.Add('<p><a href="tw08.htm">back</a></p>');
SL.Add('</body>');
SL.Add('</html>');
end;

procedure TTW08.WriteEmbeddedWidget(RaceCount, ITCount, race, it: Integer);
begin
//SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
//SL.Add('<html>');
//SL.Add('<head>');
HL.Add('<title>tw02</title>');
HL.Add('<link rel="stylesheet" href="stylesheets/rvtw08.css" type="text/css" />');
HL.Add('<script type="text/javascript" src="javascripts/core.js"></script>');
HL.Add('<script type="text/javascript" src="javascripts/rvtw08.js"></script>');
//HL.Add('<meta name="viewport" content="user-scalable=no, width = 320" />');
//HL.Add('<meta name="description" content="FR timing widget for iPod Touch." />');
//SL.Add('</head>');
//SL.Add('<body>');
SL.Add('<h1>TW02</h1>');
SL.Add('<div id="widget-embedded">');
SL.Add('');
SL.Add('<div id="timepoint-params">');
SL.Add(Format('<span id="race-param">%d</span>', [race]));
SL.Add(Format('<span id="it-param">%d</span>', [it]));
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="widget-caption">');
SL.Add(Format('<h3>R%d - IT%d</h3>', [race, it]));
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="ziffern-block">');
SL.Add('<table>');
SL.Add('<tr>');
SL.Add('<td id="bib" colspan="2">bib</td>');
SL.Add('<td id="clear-bib">clear</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b00">0</td>');
SL.Add('<td id="record" colspan="2">record</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b01">1</td>');
SL.Add('<td id="b02">2</td>');
SL.Add('<td id="b03">3</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b04">4</td>');
SL.Add('<td id="b05">5</td>');
SL.Add('<td id="b06">6</td>');
SL.Add('</tr>');
SL.Add('<tr>');
SL.Add('<td id="b07">7</td>');
SL.Add('<td id="b08">8</td>');
SL.Add('<td id="b09">9</td>');
SL.Add('</tr>');
SL.Add('</table>');
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="response-text">response-text</div>');
SL.Add('');
SL.Add('<div id="button-panel">');
//SL.Add('<a href="index">home</a> |');
SL.Add('<a href="tw02-selector">Select TP</a> |');
SL.Add('<a href="tw02-info">Info</a>');
SL.Add('</div>');
SL.Add('');
SL.Add('</div>');
//SL.Add('');
//SL.Add('</body>');
//SL.Add('</html>');
end;

procedure TTW08.WriteEmbeddedSelector(RaceCount, ITCount, race, it: Integer);
begin
  SL.Add('<h1>TW02 Timepoint Selector</h1>');
  WriteSelectorForm(RaceCount, ITCount, race, it, 'tw02-widget');
end;

procedure TTW08.WriteEmbeddedInfo;
begin
//SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
//SL.Add('<html xmlns="http://www.w3.org/1999/xhtml">');
//SL.Add('<head>');
HL.Add('<title>tw08</title>');
//HL.Add('<meta name="viewport" content="width=160" />');
//SL.Add('</head>');
//SL.Add('<body>');
SL.Add('<h1>TW02 Info</h1>');
SL.Add('<ul>');

SL.Add('<li>Make sure that Race and IT (Intermediate TimePoint) match');
SL.Add('the TimePoint you are sending messages for. IT0=Finish.</li>');

SL.Add('<li>Use numbered buttons to select the bib number.</li>');

SL.Add('<li>A single click on the record button triggers');
SL.Add('a msg to the server via ajax.</li>');

SL.Add('<li>The time for the bib is generated on the server');
SL.Add('and shown below the keypad when the ajax-call returns.</li>');

SL.Add('</ul>');
SL.Add('<p><a href="tw02.htm">back</a></p>');
//SL.Add('</body>');
//SL.Add('</html>');
end;
end.
