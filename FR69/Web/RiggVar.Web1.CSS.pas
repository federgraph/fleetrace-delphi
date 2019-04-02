unit RiggVar.Web1.CSS;

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
  System.Classes;

function InitFR42Css(SL: TStrings): string;
function InitFR62Css(SL: TStrings): string;
function InitHomeStyleCss(SL: TStrings; vd: string): string;
function InitRemoteStyleCss(SL: TStrings; vd: string): string;

implementation

function InitHomeStyleCss(SL: TStrings; vd: string): string;
begin
  SL.Clear;
  SL.Add('@import url(''style.css'');');
  SL.Add('#sidebar a { color:#555; }');
  SL.Add('#header {	background-image:url(''' + vd + 'images/bg/bg-brown.png'')}');
  SL.Add('#footer {	background-image:url(''' + vd + 'images/bg/bg-brown.png'')}');
  result := SL.Text;
end;

function InitRemoteStyleCss(SL: TStrings; vd: string): string;
begin
  SL.Clear;
  SL.Add('@import url(''style.css'');');
  SL.Add('#sidebar a { color:#555; }');
  SL.Add('#header {	background-image:url(''' + vd + 'images/bg/bg-red.png'')}');
  SL.Add('#footer {	background-image:url(''' + vd + 'images/bg/bg-red.png'')}');
  result := SL.Text;
end;

function InitFR42Css(SL: TStrings): string;
begin
  SL.Clear;

  SL.Add('.eventname{color:#FF7F50}');

//  SL.Add('.h{background-color:#F5F5DC; text-align:right}');
//  SL.Add('.hl{background-color:#F5F5DC; text-align:left}');
//  SL.Add('.sm{background-color:#87CEFA; text-align:right}');
//  SL.Add('.sml{background-color:#87CEFA; text-align:left}');
//  SL.Add('.n{background-color:#FFFFE0; text-align:right}');
//  SL.Add('.nl{background-color:#FFFFE0; text-align:left}');
//  SL.Add('.a{background-color:#FFF8E4; text-align:right}');
//  SL.Add('.al{background-color:#FFF8E4; text-align:left}');
//  SL.Add('.e{background-color:#F0F8FF; text-align:right}');
//  SL.Add('.el{background-color:#F0F8FF; text-align:left}');
//  SL.Add('.ae{background-color:#F0F8FF; text-align:right}');
//  SL.Add('.ael{background-color:#F0F8FF; text-align:left}');
//  SL.Add('.c{background-color:#8888FF; text-align:right}');
//  SL.Add('.cl{background-color:#89AAF5; text-align:left}');
//  SL.Add('.t{background-color:#89AAF5; text-align:right}');
//  SL.Add('.tl{background-color:#89AAF5; text-align:left}');

  SL.Add('.g0 {}');
//  SL.Add('.g1 {background-color:#FFFFCC}');
//  SL.Add('.g2 {background-color:#CCFFFF}');
//  SL.Add('.g3 {background-color:#FFCCCC}');
//  SL.Add('.g4 {background-color:#CCFFCC}');

  SL.Add('.g0 {text-align: right;border-right: 3px yellowgreen solid;}');
  SL.Add('.g1 {	border-right: 3px orange solid;}');
  SL.Add('.g2 {	border-right: 3px cornflowerblue solid;}');
  SL.Add('.g3 {	border-right: 3px tomato solid;}');
  SL.Add('.g4 {	border-right: 3px yellowgreen solid;}');

  SL.Add('th { margin: 1px;	padding: 1px;	font-family: verdana, arial, helvetica;	font-size: 8pt;}');
  SL.Add('td { text-align: right;	margin: 1px; padding: 1px; font-family: verdana, arial, helvetica; font-size: 8pt;}');
  SL.Add('select {font-family:verdana, arial, helvetica;font-size:8pt;}');
  SL.Add('input {font-family:verdana, arial, helvetica;font-size:8pt;}');

  SL.Add('table.fr tr.highlight {background-color:#FFCCFF;}');

  SL.Add('table.fr {width:100%;border-collapse: collapse;}');

  SL.Add('table.fr thead tr th {');
	SL.Add('text-align: right;');
	SL.Add('color: #0079c2;');
	SL.Add('line-height: 31px;');
	SL.Add('border-top-color: #6699FF;');
	SL.Add('border-top-width: 1px;');
	SL.Add('border-top-style: solid;');
	SL.Add('border-bottom-color: #6699FF;');
	SL.Add('border-bottom-width: 1px;');
	SL.Add('border-bottom-style: solid;');
  SL.Add('}');

  SL.Add('table.fr tbody tr {');
	SL.Add('border-bottom: 1px #6699FF solid;');
  SL.Add('}');

  SL.Add('table.fr tbody tr td {padding: 6px 3px;}');

  SL.Add('.sortable {}');
  SL.Add('#index_table {display: none}');

  result := SL.Text;
end;

function InitFR62Css(SL: TStrings): string;
begin
  SL.Clear;
  SL.Add('.example');
  SL.Add('{');
  SL.Add('padding: 10px;');
  SL.Add('width: 500px;');
  SL.Add('border-style:solid;');
  SL.Add('border-width:medium;');
  SL.Add('border-color: orange;');
  SL.Add('background-color: white;');
  SL.Add('}');
  SL.Add('form');
  SL.Add('{');
  SL.Add('padding: 10px;');
  SL.Add('width: 500px;');
  SL.Add('border-style: solid;');
  SL.Add('border-width:medium;');
  SL.Add('border-color: teal;');
  SL.Add('background-color: #EEEEEE;');
  SL.Add('}');
  SL.Add('th {font-family:verdana, arial, helvetica;font-size:8pt;}');
  SL.Add('td {font-family:verdana, arial, helvetica;font-size:8pt;}');
  SL.Add('select {font-family:verdana, arial, helvetica;font-size:8pt;}');
  SL.Add('input {font-family:verdana, arial, helvetica;font-size:8pt;}');
  result := SL.Text;
end;

end.
