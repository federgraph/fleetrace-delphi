unit RiggVar.Out.FR01;

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
  RiggVar.Util.Classes,
  RiggVar.BO.MsgToken,
  RiggVar.Col.BaseEntry,
  RiggVar.Out.Intf,
  RiggVar.Out.Base,
  RiggVar.Col.Race,
  RiggVar.Col.Event,
  RiggVar.Col.Stammdaten,
  RiggVar.BO.Params,
  RiggVar.BO.EventProps;

type
  TOutput1 = class
  private
    bout: TBaseOutput; //not owned
    qe: TRaceRemoteObject;
    fe: TEventRemoteObject;
    se: TStammdatenRemoteObject;
    SL: TStrings; //not owned
    TokenParser: TTokenParser; //not owned
  public
    constructor Create(aBaseOutput: TBaseOutput);
    destructor Destroy; override;

    procedure Welcome;
    procedure Params;
    procedure Athletes;
    procedure Properties;
    procedure Backup;
    procedure BackupPreTXT;
    procedure EventResult(fn: TEventNode; IncludeHeader, IncludeFooter: Boolean);
    procedure RaceResult(qn: TRaceNode);

    procedure RaceXml(Selector: string);
  end;

implementation

uses
  RiggVar.BO.Def;

{ TOutput1 }

constructor TOutput1.Create(aBaseOutput: TBaseOutput);
begin
  inherited Create;
  bout := aBaseOutput;
  SL := bout.SL;
  TokenParser := bout.TokenParser;

  qe := TRaceRemoteObject.Create(BO.BOParams.ITCount);
  fe := TEventRemoteObject.Create(BO.BOParams.RaceCount);
  se := TStammdatenRemoteObject.Create;
end;

destructor TOutput1.Destroy;
begin
  fe.Free;
  qe.Free;
  se.Free;
  inherited Destroy;
end;

procedure TOutput1.BackupPreTXT;
begin
  SL.Clear;
  SL.Add('<pre>');
  BO.BackupToText(SL);
  SL.Add('</pre>');
end;

procedure TOutput1.Params;
var
  p: TBOParams;
begin
  p := BO.BOParams;
  SL.Clear;
  bout.WantPageHeader := False;
  SL.Add(Format('RaceCount=%d', [p.RaceCount]));
  SL.Add(Format('ITCount=%d', [p.ITCount]));
end;

procedure TOutput1.Properties;
var
  p: TBOParams;
  e: TEventProps;
begin
  p := BO.BOParams;
  e := BO.EventProps;
  SL.Clear;
  bout.WantPageHeader := False;
  SL.Add('RaceCount=' + IntToStr(p.RaceCount));
  SL.Add('ITCount=' + IntToStr(p.ITCount));
  SL.Add('StartlistCount=' + IntToStr(p.StartlistCount));
  SL.Add('DivisionName=' + p.DivisionName);
  SL.Add('EventName=' + e.EventName);
  SL.Add('EventDates=' + e.EventDates);
  SL.Add('HostClub=' + e.HostClub);
  SL.Add('PRO=' + e.PRO);
  SL.Add('JuryHead=' + e.JuryHead);
end;

procedure TOutput1.RaceXml(Selector: string);
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  Race: Integer;
  IT: Integer;
  i: Integer;
  tp: TTimePoint;
begin
  SL.Clear;
  try
    TokenParser.sRest := Selector;
    TokenParser.NextToken(); //RiggVar.
    TokenParser.NextToken(); //FR.
    Race := TokenParser.NextTokenX('Race');
    IT := TokenParser.NextTokenX('IT');

    if Race < 1 then Race := 1;
    if IT < 0 then IT := 0;

    if Race > BO.BOParams.RaceCount then
      Race := BO.BOParams.RaceCount;
    if IT > BO.BOParams.ITCount then
      IT := BO.BOParams.ITCount;

    cl := BO.RNode[Race].RaceRowCollection;

    SL.Add(Format('<RaceXml Race="%d" IT="%d">', [Race, IT]));
    for i := 0 to cl.Count - 1 do
    begin
      cr := cl.Items[i];
      tp := cr.IT[IT];
      SL.Add(Format(
      '<Entry Bib="%d" SNR="%d" PosR="%d" OTime="%s" Behind="%s"/>',
      [cr.Bib,
       cr.SNR,
       tp.PosR,
       tp.OTime.ToString,
       tp.Behind.ToString
       ]));
    end;
    SL.Add('</RaceXml>');
  except
    SL.Clear;
    SL.Add('<RaceXml>' + Selector + '</RaceXml>');
  end;
end;

procedure TOutput1.Welcome;
begin
  SL.Add('Welcome, the Server is FR62.');
  SL.Add('ServerTime: ' + DateTimeToStr(Now));
  SL.Add('EventName: ' + BO.EventProps.EventName);
  SL.Add('RaceCount: ' + IntToStr(BO.BOParams.RaceCount));
  SL.Add('ITCount: ' + IntToStr(BO.BOParams.ITCount));
  SL.Add('StartlistCount: ' + IntToStr(BO.BOParams.StartlistCount));
end;

procedure TOutput1.Athletes;
var
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  i: Integer;
begin
  cl := BO.StammdatenNode.StammdatenRowCollection;
  se.GetHeader(SL, bout.OutputType, 'Athletes', bout.XMLSection);
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    se.Assign(cr);
    case bout.OutputType of
      otCSV: SL.Add(se.GetCSV);
      otHTM: SL.Add(se.GetHTM);
      otXML: SL.Add(se.GetXML('A'));
    end;
  end;
  se.GetFooter(SL, bout.OutputType, 'Athletes', bout.XMLSection);
end;

procedure TOutput1.Backup;
var
  s: string;
  i: Integer;
  j: Integer;
  sName, sValue: string;
begin
  j := 0;
  case bout.OutputType of
    otCSV: ;
    otHTM:
    begin
      SL.Add('<table border="1" cellspacing="0" cellpadding="1">');
      SL.Add('<caption>Backup</caption>');
      SL.Add('<tr><th>Messagelist</th></tr>');
      SL.Add('<tr><td>');
      SL.Add('<pre>');
    end;
    otXML: if bout.XMLSection then SL.Add('<Backup>');
  end;

  BO.BackupToText(SL);

  case bout.OutputType of
    otCSV: ;
    otHTM:
    begin
      SL.Add('</pre>');
      SL.Add('</td></tr></table>');
    end;
    otXML:
    begin
      for i := SL.Count-1 downto 0 do
      begin
        s := SL[i];
        if s = '' then
        begin
          SL.Delete(i);
          Continue;
        end;
        if (s[1] <> '<') then
        begin
          if (s = '') or (s[1] = '#') or (Copy(s, 1, 2) = '//') then
          begin
            SL.Delete(i);
            Continue;
          end
          else
          begin
            sName := Trim(SL.Names[i]);
            sValue := Trim(SL.ValueFromIndex[i]);
            Inc(j);
            s := '<B I="' + IntToStr(j) + '" N="' + sName + '" V="' + sValue + '" />';
            s := StringReplace(s, cTokenA + '.' + cTokenB + '.', '', []);
            SL[i] := s;
          end;
        end;
      end;
      if bout.XMLSection then SL.Add('</Backup>');
    end;
  end;
end;

procedure TOutput1.RaceResult(qn: TRaceNode);
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  cl := qn.RaceRowCollection;
  qe.RunID := '';
  qe.GetHeader(SL, bout.OutputType, 'Race', bout.XMLSection);
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    qe.Assign(cr);
    case bout.OutputType of
      otCSV: SL.Add(qe.GetCSV);
      otHTM: SL.Add(qe.GetHTM);
      otXML: SL.Add(qe.GetXML('Q'));
    end;
  end;
  qe.GetFooter(SL, bout.OutputType, 'Race', bout.XMLSection);
end;

procedure TOutput1.EventResult(fn: TEventNode; IncludeHeader,
  IncludeFooter: Boolean);
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  cl := fn.EventRowCollection;
  if IncludeHeader then
    fe.GetHeader(SL, bout.OutputType, 'Event', bout.XMLSection);
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    fe.Assign(cr);
    case bout.OutputType of
      otCSV: SL.Add(fe.GetCSV);
      otHTM: SL.Add(fe.GetHTM);
      otXML: SL.Add(fe.GetXML('F'));
    end;
  end;
  if IncludeFooter then
    fe.GetFooter(SL, bout.OutputType, 'Event', bout.XMLSection);
end;

end.
