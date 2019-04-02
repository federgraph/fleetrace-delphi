unit RiggVar.Out.FR08;

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
  RiggVar.Util.Classes,
  RiggVar.BO.Params,
  RiggVar.BO.EventProps,
  RiggVar.Col.Race,
  RiggVar.Col.Event,
  RiggVar.Col.Stammdaten;

type
  TOutput8 = class
  private
    SLCommaText: TStringList;
    sep: char;

    rro: TRaceRemoteObject;
    ero: TEventRemoteObject;

    wnr: string;
    lnr: string;
    stat: string;

    nwnr: Integer; //Report
    nlnr: Integer; //Race
    nstat: Integer; //IT

//    procedure HandleER;
//    procedure HandleRR;

    procedure HandleRSL;
    procedure HandleMSL;

    procedure HandleEVD;
    procedure HandleJUR;
    procedure HandleDAT;
    procedure HandleSTL;
    procedure HandleEVR;
    procedure HandleRES;
  public
    SL: TStrings;
    constructor Create;
    destructor Destroy; override;
    procedure DispatchProt(msg: string);
  end;

implementation

{
RES0 //letzter Datensatz
RES1 //es folgen Datensätze
}

uses
  RiggVar.BO.Def;

constructor TOutput8.Create;
begin
  SLCommaText := TStringList.Create;
  SLCommaText.Delimiter := ';';
  sep := ';';
  rro := TRaceRemoteObject.Create(BO.BOParams.ITCount);
  ero := TEventRemoteObject.Create(BO.BOParams.RaceCount);
end;

destructor TOutput8.Destroy;
begin
  ero.Free;
  rro.Free;

  SLCommaText.Free;
  inherited;
end;

procedure TOutput8.DispatchProt(msg: string);
var
  i: Integer;
  s: string;
  prot: string;
  protID: string;
  protParams: string;
begin
  wnr := '0';
  lnr := '0';
  stat := '1';

  prot := UpperCase(msg);

  i := Length(prot);
  if (i < 4) then
    Exit;
  if prot[1] <> '?' then
    Exit;

  protID := Copy(prot, 2, 3);
  protParams := Trim(Copy(prot, 6, Length(prot)));

  if protID = 'RES' then
  begin
    s := TUtils.Cut(',', protParams, wnr);
    s := TUtils.Cut(',', s, lnr);
    s := TUtils.Cut(',', s, stat);
  end;

  nwnr := StrToIntDef(wnr, 0);
  nlnr := StrToIntDef(lnr, 0);
  nstat := StrToIntDef(stat, 0);

  if protID = 'RSL' then
    HandleRSL
  else if protID = 'MSL' then
    HandleMSL
  else if protID = 'EVD' then
    HandleEVD
  else if protID = 'JUR' then
    HandleJUR
  else if protID = 'DAT' then
    HandleDAT
  else if protID = 'STL' then
    HandleSTL
  else if protID = 'RES' then
    HandleRES
  else if protID = 'EVR' then
    HandleEVR
end;

procedure TOutput8.HandleDAT;
var
  cl: TStammdatenRowCollection;
  cr: TStammdatenRowCollectionItem;
  i, j: Integer;
  c: Integer;
begin
  cl := BO.StammdatenNode.StammdatenRowCollection;
  c := cl.Count-1;

  //DAT!;Division;FieldCount;Count
  SLCommaText.Clear;
  SLCommaText.Add('DAT!');
  SLCommaText.Add(BO.EventProps.DivisionName);
  SLCommaText.Add(IntToStr(cl.FieldCount));
  SLCommaText.Add(IntToStr(cl.Count));
  SL.Add(SLCommaText.DelimitedText);

  SL.Add('DAT!' + sep + IntToStr(cl.Count));

  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    //DAT;SNR;FN;LN;SN;NC;GR;PB;N7;N8;N9;N10;N11;N12;N13;N14;N15;N16
    SLCommaText.Clear;
    SLCommaText.Add(IntToStr(cr.SNR));
    for j := 1 to cr.FieldCount - 1 do
    begin
      SLCommaText.Add(cr.FieldValue[i]);
      if j > 16 then
        break;
    end;
    for j := cr.FieldCount to 15 do
    begin
      SLCommaText.Add(sep);
    end;
    if i = c then
      SLCommaText.Add('DAT0' + sep + SLCommaText.DelimitedText)
    else
      SLCommaText.Add('DAT1' + sep + SLCommaText.DelimitedText);
  end;
end;

procedure TOutput8.HandleRSL;
var
  RaceCount: Integer;
  i: Integer;
begin
  RaceCount := BO.BOParams.RaceCount;
  SL.Add('RSL!' + sep + IntToStr(RaceCount));
  for i := 1 to RaceCount do
  begin
    SLCommaText.Clear;
    SLCommaText.Add(IntToStr(i));
    SLCommaText.Add('Race ' + IntToStr(i));
    if i = RaceCount then
      SL.Add('RSL0' + sep + SLCommaText.DelimitedText)
    else
      SL.Add('RSL1' + sep + SLCommaText.DelimitedText);
  end;
end;

procedure TOutput8.HandleMSL;
var
  ITCount: Integer;
  i: Integer;
begin
  ITCount := BO.BOParams.ITCount;
  SL.Add('MSL!' + sep + IntToStr(ITCount));
  for i := 1 to ITCount do
  begin
    SLCommaText.Clear;
    SLCommaText.Add(IntToStr(i));
    SLCommaText.Add('Mark ' + IntToStr(i));
    if i = ITCount then
      SL.Add('MSL0' + sep + SLCommaText.DelimitedText)
    else
      SL.Add('MSL1' + sep + SLCommaText.DelimitedText);
  end;
end;

procedure TOutput8.HandleEVD;
var
  EventProps: TEventProps;
  BOParams: TBOParams;
begin
  EventProps := BO.EventProps;
  BOParams := BO.BOParams;

  //EVD!;DivisionName;RaceCount;ITCount;StartlistCount
  SLCommaText.Clear;
  SLCommaText.Add('EVD!');
  SLCommaText.Add(EventProps.DivisionName);
  SLCommaText.Add(IntToStr(BOParams.RaceCount));
  SLCommaText.Add(IntToStr(BOParams.ITCount));
  SLCommaText.Add(IntToStr(BOParams.StartlistCount));
  SL.Add(SLCommaText.DelimitedText);

  //EVD;EventName;EventDates;HostClub
  SLCommaText.Clear;
  SLCommaText.Add('EVD');
  SLCommaText.Add(EventProps.EventName);
  SLCommaText.Add(EventProps.EventDates);
  SLCommaText.Add(EventProps.HostClub);
  SL.Add(SLCommaText.DelimitedText);
end;

procedure TOutput8.HandleJUR;
var
  EventProps: TEventProps;
begin
  EventProps := BO.EventProps;

  SLCommaText.Clear;
  SLCommaText.Add('JUR');
  SLCommaText.Add(EventProps.PRO);
  SLCommaText.Add(EventProps.JuryHead);
  SL.Add(SLCommaText.DelimitedText);
end;

procedure TOutput8.HandleSTL;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  c: Integer;
begin
  cl := BO.EventNode.EventRowCollection;
  c := cl.Count-1;

  //STL!;Division;Count
  SLCommaText.Clear;
  SLCommaText.Add('STL!');
  SLCommaText.Add(BO.EventProps.DivisionName);
  SLCommaText.Add(IntToStr(cl.Count));
  SL.Add(SLCommaText.DelimitedText);

  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];

    SLCommaText.Clear;
    SLCommaText.Add(IntToStr(cr.SNR));
    SLCommaText.Add(IntToStr(cr.Bib));
    SLCommaText.Add(cr.FN);
    SLCommaText.Add(cr.LN);
    SLCommaText.Add(cr.SN);
    SLCommaText.Add(cr.NC);
    SLCommaText.Add(cr.GR);
    SLCommaText.Add(cr.PB);
    SLCommaText.Add(cr.DN);
    if i = c then
      SL.Add('STL0' + sep + SLCommaText.DelimitedText)
    else
      SL.Add('STL1' + sep + SLCommaText.DelimitedText);
  end;
end;

//procedure TOutputM.HandleRR;
//var
//  cl: TRaceRowCollection;
//  cr: TRaceRowCollectionItem;
//  i: Integer;
//  c: Integer;
//begin
//  cl := BO.RNode[0].RaceRowCollection;
//  c := cl.Count-1;
//  SL.Add('RES!' + sep + IntToStr(cl.Count));
//  for i := 0 to cl.Count-1 do
//  begin
//    cr := cl.Items[i];
//    rro.Assign(cr);
//    if i = c then
//      SL.Add('RES0' + sep + rro.GetCommaText(SLCommaText))
//    else
//      SL.Add('RES1' + sep + rro.GetCommaText(SLCommaText));
//  end;
//end;
//
//procedure TOutputM.HandleER;
//var
//  cl: TEventRowCollection;
//  cr: TEventRowCollectionItem;
//  i: Integer;
//  c: Integer;
//begin
//  cl := BO.EventNode.EventRowCollection;
//  c := cl.Count-1;
//  SL.Add('RES!' + sep + IntToStr(cl.Count));
//  for i := 0 to cl.Count-1 do
//  begin
//    cr := cl.Items[i];
//    ero.Assign(cr);
//    if i = c then
//      SL.Add('RES0' + sep + ero.GetCommaText(SLCommaText))
//    else
//      SL.Add('RES1' + sep + ero.GetCommaText(SLCommaText));
//  end;
//end;

procedure TOutput8.HandleEVR;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  c: Integer;
begin
  cl := BO.EventNode.EventRowCollection;
  c := cl.Count-1;

  //EVR!;Division;Count
  SLCommaText.Clear;
  SLCommaText.Add('EVR!');
  SLCommaText.Add(BO.BOParams.DivisionName);
  SLCommaText.Add(IntToStr(cl.Count));
  SL.Add(SLCommaText.DelimitedText);

  for i := 0 to cl.Count-1 do
  begin
    //SNR;Bib;DN;NOC;Points;Rank;PosR
    cr := cl.Items[i];
    SLCommaText.Clear;
    SLCommaText.Add(IntToStr(cr.SNR));
    SLCommaText.Add(IntToStr(cr.Bib));
    SLCommaText.Add(cr.DN);
    SLCommaText.Add(cr.NC);
    SLCommaText.Add(cr.GPoints);
    SLCommaText.Add(IntToStr(cr.GRank));
    SLCommaText.Add(IntToStr(cr.GPosR));
    if i = c then
      SL.Add('EVR0' + sep + SLCommaText.DelimitedText)
    else
      SL.Add('EVR1' + sep + SLCommaText.DelimitedText);
  end;
end;

procedure TOutput8.HandleRES;
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
  c: Integer;
  tp: TTimePoint;
  RaceCount: Integer;
  ITCount: Integer;
begin
  RaceCount := BO.BOParams.RaceCount;
  ITCount := BO.BOParams.ITCount;

  { get Race }
  if (nlnr > 0) and (nlnr <= RaceCount) then
    cl := BO.RNode[nlnr].RaceRowCollection
  else
    cl := BO.RNode[0].RaceRowCollection;

  c := cl.Count-1;

  //RES!;Division;Race;IT;Count
  SLCommaText.Clear;
  SLCommaText.Add('RES!');
  SLCommaText.Add(BO.BOParams.DivisionName);
  SLCommaText.Add(IntToStr(RaceCount));
  SLCommaText.Add(IntToStr(ITCount));
  SLCommaText.Add(IntToStr(cl.Count));
  SL.Add(SLCommaText.DelimitedText);

  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];

    { get Intermediate Timepoint }
    if (nstat > 0) and (nstat <= ITCount) then
      TP := cr.IT[nstat]
    else
      TP := cr.IT[0];

    SLCommaText.Clear;

    //SNR,BIB,DN,NC,QU,DG,ST,
    SLCommaText.Add(IntToStr(cr.SNR));
    SLCommaText.Add(IntToStr(cr.Bib));
    SLCommaText.Add(cr.DN);
    SLCommaText.Add(cr.NC);
//    SLCommaText.Add(IntToStr(cr.MRank));
    SLCommaText.Add(cr.QU.ToString);
    SLCommaText.Add(IntToStr(cr.DG));
    SLCommaText.Add(cr.ST.ToString);

    //OTime,Behind,BFT,BPL,Rank,PosR,PLZ
    SLCommaText.Add(tp.OTime.ToString);
    SLCommaText.Add(tp.Behind.ToString);
    SLCommaText.Add(tp.BFT.ToString);
    SLCommaText.Add(tp.BPL.ToString);
    SLCommaText.Add(IntToStr(tp.Rank));
    SLCommaText.Add(IntToStr(tp.PosR));
    SLCommaText.Add(IntToStr(tp.PLZ));

    //ft_OTime,ft_Behind,ft_BFT,ft_BPL,ft_Rank,ft_PosR,ft_PLZ
//    SLCommaText.Add(cr.FT.OTime.ToString);
//    SLCommaText.Add(IntToStr(cr.FT.ORank));
//    SLCommaText.Add(IntToStr(cr.FT.Rank));
//    SLCommaText.Add(IntToStr(cr.FT.PosR));
//    SLCommaText.Add(IntToStr(cr.FT.PLZ));

    if i < c then
      SLCommaText.Add('RES1' + sep + SLCommaText.DelimitedText)
    else
      SLCommaText.Add('RES0' + sep + SLCommaText.DelimitedText);
  end;
end;

end.
