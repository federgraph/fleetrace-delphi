unit RiggVar.Out.FR10;

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
  RiggVar.Col.Race;

type
  TOutput10 = class
  private
    procedure AddCell(ASL: TStrings; cr: TRaceRowCollectionItem; j: Integer);
    procedure AddHeaderCell(ASL: TStrings; j: Integer);
  public
    procedure TimePointXML(ASL: TStrings; QueryString: string);
    procedure TimePointTable(ASL: TStrings; QueryString: string);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Util.Classes,
  RiggVar.BO.Def;

procedure TOutput10.TimePointXML(ASL: TStrings; QueryString: string);
var
  TokenParser: TTokenParser;
  Race: Integer;
  IT: Integer;
  Bib: Integer;
  rn: TRaceNode;
  cr: TRaceRowCollectionItem;
  tp: TTimePoint;
  t: string;
begin
  //Rx.ITx.Bibx
  TokenParser := TTokenParser.Create;
  TokenParser.sRest := QueryString;
  Race := TokenParser.NextTokenX('R');
  IT := TokenParser.NextTokenX('IT');
  Bib := TokenParser.NextTokenX('Bib');
  t := 'no time';
  if Assigned(BO) and (Race > 0) and (Race < BO.BOParams.RaceCount) then
  begin
    rn := BO.RNode[Race];
    cr := rn.FindBib(Bib);
    if Assigned(cr) then
    begin
      if (IT >= 0) and (IT <= BO.BOParams.ITCount) then
      begin
        tp := cr.IT[IT];
        t := tp.BPL.ToString();
      end;
    end;
  end;
  TokenParser.Free;
  ASL.Add(Format('<TW><b>%d</b><t>%s</t></TW>', [Bib, t]));
end;

procedure TOutput10.TimePointTable(ASL: TStrings; QueryString: string);
var
  TokenParser: TTokenParser;
  Race: Integer;
  rn: TRaceNode;
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  cr2: TRaceRowCollectionItem;
  i, j: Integer;
begin
  //QueryString = Rx
  TokenParser := TTokenParser.Create;
  TokenParser.sRest := QueryString;
  Race := TokenParser.NextTokenX('R');
  TokenParser.Free;

  if Assigned(BO) and (Race > 0) and (Race <= BO.BOParams.RaceCount) then
  begin
    rn := BO.RNode[Race];
    cl := rn.RaceRowCollection;

    ASL.Add('<table border="1" cellpadding="1" cellspacing="1">');

    //Header row
    ASL.Add('<tr>');
    ASL.Add('<th>Pos</th>');
    for j := 1 to BO.BOParams.ITCount do
    begin
      AddHeaderCell(ASL, j);
    end;
    AddHeaderCell(ASL, 0);
    ASL.Add('</tr>');

    //Body rows
    for i := 0 to cl.Count - 1 do
    begin
      cr := cl.Items[i];
      ASL.Add('<tr>');
      ASL.Add(Format('<td>%d</td>', [i+1]));
      for j := 1 to cr.ITCount-1 do //cr.ITCount returns the Length of dynamic Array
      begin
        cr2 := cl.Items[cr.IT[j].PLZ];
        AddCell(ASL, cr2, j);
      end;
      cr2 := cl.Items[cr.IT[0].PLZ];
      AddCell(ASL, cr2, 0);
      ASL.Add('</tr>');
    end;

    ASL.Add('</table>');
  end
  else
  ASL.Add('<p>TimePointReport: invalid race param</p>');
end;

procedure TOutput10.AddCell(ASL: TStrings; cr: TRaceRowCollectionItem; j: Integer);
var
  tp: TTimePoint;
  t: string;
begin
  if Assigned(cr) then
  begin
    tp := cr.IT[j];
    if Assigned(tp) then
    begin
      t := tp.Behind.ToString;

      //show penalty only if finish column and no time is present
      if (j = 0) and (t = '') then
        t := t + ' ' + cr.QU.ToString;

      ASL.Add(Format('<td>Bib %d<br/>%s</td>', [cr.Bib, t]));
    end
    else
    begin
      ASL.Add('<td>-<br/>tp</td>');
    end;
  end
  else
  begin
    ASL.Add('<td>-<br/>-</td>');
  end;
end;

procedure TOutput10.AddHeaderCell(ASL: TStrings; j: Integer);
var
  s: string;
begin
  if j = 0 then
    s := 'FT'
  else
    s := 'IT' + IntToStr(j);
  ASL.Add(Format('<th>%s</th>', [s]));
end;

end.
