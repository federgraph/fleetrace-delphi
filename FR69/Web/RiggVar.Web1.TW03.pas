unit RiggVar.Web1.TW03;

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
  TTW03 = class(TTW00)
  private
    procedure WriteTimePointTable(Race: Integer);
  public
    procedure WriteWidget(RaceCount, ITCount, race, it: Integer); override;
  end;

implementation

uses
  RiggVar.Web1.Proxy;

procedure TTW03.WriteWidget(RaceCount, ITCount, race, it: Integer);
begin
//SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
//SL.Add('<html>');
//SL.Add('<head>');
HL.Add('<script type="text/javascript" src="javascripts/core.js"></script>');
HL.Add('<script type="text/javascript" src="javascripts/rvtw03.js"></script>');
HL.Add('<link rel="stylesheet" href="stylesheets/rvtw03.css" type="text/css" />');
//SL.Add('</head>');
//SL.Add('<body>');
//SL.Add('');
SL.Add('<h1>TW03</h1>');
SL.Add('');
WriteTimePointSelectorForm(RaceCount, ITCount, race, it, 'tw03-widget');
SL.Add('');
SL.Add('<div id="timepoint-params">');
SL.Add(Format('<p class="param">Race: <span id="race-param">%d</span></p>', [race]));
SL.Add(Format('<p class="param">IT: <span id="it-param">%d</span></p>', [it]));
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="widget">');
SL.Add('<div id="widget-caption">');
SL.Add(Format('<h3>R%d - IT%d</h3>', [race, it]));
SL.Add('</div>');
SL.Add('<div id="ziffern-block">');
SL.Add('<div class="b" id="b0M"><input class="i2" type="button" value="M" /></div>');
SL.Add('<div class="b" id="b00"><input class="i" type="button" value="10" /></div>');
SL.Add('<div class="b" id="b0P"><input class="i2" type="button" value="P" /></div>');
SL.Add('<div class="b" id="b01"><input class="i" type="button" value="11" /></div>');
SL.Add('<div class="b" id="b02"><input class="i" type="button" value="12" /></div>');
SL.Add('<div class="b" id="b03"><input class="i" type="button" value="13" /></div>');
SL.Add('<div class="b" id="b04"><input class="i" type="button" value="14" /></div>');
SL.Add('<div class="b" id="b05"><input class="i" type="button" value="15" /></div>');
SL.Add('<div class="b" id="b06"><input class="i" type="button" value="16" /></div>');
SL.Add('<div class="b" id="b07"><input class="i" type="button" value="17" /></div>');
SL.Add('<div class="b" id="b08"><input class="i" type="button" value="18" /></div>');
SL.Add('<div class="b" id="b09"><input class="i" type="button" value="19" /></div>');
SL.Add('</div>');
SL.Add('</div>');
SL.Add('');
SL.Add('<div id="timepoint-table">');
WriteTimePointTable(race);
SL.Add('</div>');
SL.Add('<div id="content-strecher" style="height: 507px">');
SL.Add('</div>');

//SL.Add('');
//SL.Add('</body>');
//SL.Add('</html>');
end;

procedure TTW03.WriteTimePointTable(Race: Integer);
var
  s: string;
  TL: TStringList;
  i: Integer;
begin
  TL := TStringList.Create;
  try
    s := WebProxy.Handle_TW_Report(Race);
    TL.Text := s;
    for i := 0 to TL.Count - 1 do
      SL.Add(TL[i]);
  finally
    TL.Free;
  end;
end;

end.
