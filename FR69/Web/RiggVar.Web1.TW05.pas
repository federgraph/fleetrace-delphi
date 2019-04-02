unit RiggVar.Web1.TW05;

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
  TTW05 = class(TTW00)
  public
    procedure WriteWidget(RaceCount, ITCount, race, it: Integer); override;
    procedure WriteSelector(RaceCount, ITCount, race, it: Integer); override;
    procedure WriteInfo; override;
    procedure WriteEmbeddedInfo;
    procedure WriteEmbeddedSelector(RaceCount, ITCount, race, it: Integer);
    procedure WriteEmbeddedWidget(RaceCount, ITCount, race, it: Integer);
  end;

implementation

procedure TTW05.WriteWidget(RaceCount, ITCount, race, it: Integer);
begin
SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
SL.Add('<html>');
SL.Add('<head>');
SL.Add('<title>tw05</title>');
SL.Add('<link rel="stylesheet" href="stylesheets/rvtw05.css" type="text/css" />');
SL.Add('<script type="text/javascript" src="javascripts/core.js"></script>');
SL.Add('<script type="text/javascript" src="javascripts/rvtw05.js"></script>');
SL.Add('<meta name="viewport" content="user-scalable=no, width = 320" />');
SL.Add('<meta name="description" content="FR Timing Widget for iPod Touch." />');
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
SL.Add('<div class="b" id="b0M"><input class="i2" type="button" value="M" /></div>');
SL.Add('<div class="b" id="b00"><input class="i" type="button" value="10" /></div>');
SL.Add('<div class="b" id="b0P"><input class="i2" type="button" value="P" /></div>');
SL.Add('');
SL.Add('<div class="b" id="b01"><input class="i" type="button" value="11" /></div>');
SL.Add('<div class="b" id="b02"><input class="i" type="button" value="12" /></div>');
SL.Add('<div class="b" id="b03"><input class="i" type="button" value="13" /></div>');
SL.Add('');
SL.Add('<div class="b" id="b04"><input class="i" type="button" value="14" /></div>');
SL.Add('<div class="b" id="b05"><input class="i" type="button" value="15" /></div>');
SL.Add('<div class="b" id="b06"><input class="i" type="button" value="16" /></div>');
SL.Add('');
SL.Add('<div class="b" id="b07"><input class="i" type="button" value="17" /></div>');
SL.Add('<div class="b" id="b08"><input class="i" type="button" value="18" /></div>');
SL.Add('<div class="b" id="b09"><input class="i" type="button" value="19" /></div>');
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="response-text">response-text</div>');
SL.Add('');
SL.Add('<div id="button-panel">');
SL.Add('<a href="index">home</a> |');
SL.Add('<a href="tw05-selector">Select TP</a> |');
SL.Add('<a href="tw05-info">Info</a>');
SL.Add('</div>');
SL.Add('');
SL.Add('</div>');
SL.Add('');
SL.Add('</body>');
SL.Add('</html>');
end;

procedure TTW05.WriteSelector(RaceCount, ITCount, race, it: Integer);
begin
  WriteSelectorForm(RaceCount, ITCount, race, it, 'tw05-widget');
end;

procedure TTW05.WriteInfo;
begin
SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
SL.Add('<html xmlns="http://www.w3.org/1999/xhtml">');
SL.Add('<head>');
SL.Add('<title>tw05</title>');
SL.Add('<meta name="viewport" content="width=160" />');
SL.Add('</head>');
SL.Add('<body>');

SL.Add('<p>This is the TW05 Info page:</p>');
SL.Add('<ul>');

SL.Add('<li>Make sure that Race and IT (Intermediate TimePoint) match');
SL.Add('the TimePoint you are sending messages for. IT0=Finish.</li>');

SL.Add('<li>Use M (Minus) and P (Plus) to update the bib number range.</li>');

SL.Add('<li>A single click on the numbered buttons triggers');
SL.Add('a msg to the server via ajax.</li>');

SL.Add('<li>The time for the bib is generated on the server');
SL.Add('and shown below the keypad when the ajax-call returns.</li>');

SL.Add('</ul>');
SL.Add('<p><a href="tw05.htm">back</a></p>');
SL.Add('</body>');
SL.Add('</html>');
end;

procedure TTW05.WriteEmbeddedWidget(RaceCount, ITCount, race, it: Integer);
begin
//SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
//SL.Add('<html>');
//SL.Add('<head>');
HL.Add('<title>tw05</title>');
HL.Add('<link rel="stylesheet" href="stylesheets/rvtw05.css" type="text/css" />');
HL.Add('<script type="text/javascript" src="javascripts/core.js"></script>');
HL.Add('<script type="text/javascript" src="javascripts/rvtw05.js"></script>');
HL.Add('<meta name="viewport" content="user-scalable=no, width = 320" />');
HL.Add('<meta name="description" content="FR Timing Widget for iPod Touch." />');
//SL.Add('</head>');
//SL.Add('<body>');
SL.Add('<h1>TW01</h1>');
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
SL.Add('<div class="b" id="b0M"><input class="i2" type="button" value="M" /></div>');
SL.Add('<div class="b" id="b00"><input class="i" type="button" value="10" /></div>');
SL.Add('<div class="b" id="b0P"><input class="i2" type="button" value="P" /></div>');
SL.Add('');
SL.Add('<div class="b" id="b01"><input class="i" type="button" value="11" /></div>');
SL.Add('<div class="b" id="b02"><input class="i" type="button" value="12" /></div>');
SL.Add('<div class="b" id="b03"><input class="i" type="button" value="13" /></div>');
SL.Add('');
SL.Add('<div class="b" id="b04"><input class="i" type="button" value="14" /></div>');
SL.Add('<div class="b" id="b05"><input class="i" type="button" value="15" /></div>');
SL.Add('<div class="b" id="b06"><input class="i" type="button" value="16" /></div>');
SL.Add('');
SL.Add('<div class="b" id="b07"><input class="i" type="button" value="17" /></div>');
SL.Add('<div class="b" id="b08"><input class="i" type="button" value="18" /></div>');
SL.Add('<div class="b" id="b09"><input class="i" type="button" value="19" /></div>');
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="response-text">response-text</div>');
SL.Add('');
SL.Add('<div id="button-panel">');
//SL.Add('<a href="index">home</a> |');
SL.Add('<a href="tw01-selector">Select TP</a> |');
SL.Add('<a href="tw01-info">Info</a>');
SL.Add('</div>');
SL.Add('');
SL.Add('</div>');
SL.Add('');
//SL.Add('</body>');
//SL.Add('</html>');
end;

procedure TTW05.WriteEmbeddedSelector(RaceCount, ITCount, race, it: Integer);
begin
  SL.Add('<h1>TW01 Timepoint Selector</h1>');
  WriteSelectorForm(RaceCount, ITCount, race, it, 'tw01-widget');
end;

procedure TTW05.WriteEmbeddedInfo;
begin
//SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
//SL.Add('<html xmlns="http://www.w3.org/1999/xhtml">');
//SL.Add('<head>');
HL.Add('<title>tw01</title>');
HL.Add('<meta name="viewport" content="width=160" />');
//HL.Add('</head>');
//SL.Add('<body>');

SL.Add('<p>TW01 Info Page</p>');
SL.Add('<ul>');

SL.Add('<li>Make sure that Race and IT (Intermediate TimePoint) match');
SL.Add('the TimePoint you are sending messages for. IT0=Finish.</li>');

SL.Add('<li>Use M (Minus) and P (Plus) to update the bib number range.</li>');

SL.Add('<li>A single click on the numbered buttons triggers');
SL.Add('a msg to the server via ajax.</li>');

SL.Add('<li>The time for the bib is generated on the server');
SL.Add('and shown below the keypad when the ajax-call returns.</li>');

SL.Add('</ul>');
SL.Add('<p><a href="tw01.htm">back</a></p>');
//SL.Add('</body>');
//SL.Add('</html>');
end;

end.
