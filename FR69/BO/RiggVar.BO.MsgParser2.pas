unit RiggVar.BO.MsgParser2;

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

{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  RiggVar.BO.MsgToken,
  RiggVar.Util.Classes;

type
  TMsgType = (
    mtNone,
    mtRaceCount,
    mtITCount,
    mtStartlistCount,
    mtEventName,
    mtInputMode,
    mtRank,
    mtQU,
    mtRV,
    mtTime
  );

  TMsgParser2 = class
  private
    function ParseLn(const t: string): Boolean;
  protected
    reRaceCount: TRegEx;
    reITCount: TRegEx;
    reStartlistCount: TRegEx;

    reEventName: TRegEx;
    reInputMode: TRegEx;

    reRK: TRegEx;
    reIT: TRegEx;
    reFT: TRegEx;
    reQU: TRegEx;
    reRV: TRegEx;

    procedure ResetGroups;
    procedure ResetVars;
    procedure Test(const s: string);

    procedure RetrieveGroupMatch(mc: TMatchCollection);
    function TrimLine(const s: string): string;
    procedure PrepareRegex;
  public
    sDivision: string;
    sRace: string;
    sBib: string;
    sIT: string;

    sValue: string;

    sRank: string;
    sTime: string;
    sQU: string;

    gc: Integer;
    g1: string;
    g2: string;
    g3: string;
    g4: string;

    MsgType: TMsgType;

    StartlistCount: Integer;
    RaceCount: Integer;
    ITCount: Integer;

    EventName: string;
    InputMode: Boolean;

    Race: Integer;
    IT: Integer;
    Bib: Integer;
    Rank: Integer;

    ErrorMsg: string;

    constructor Create;

    procedure InitTestData(ML: TStrings);

    function ParseLine(const ALine: string): Boolean;

    function GenMsg: string;
    function GenOne(mt: TMsgType): string;
    procedure GenAll(ML: TStrings);
  end;

implementation

constructor TMsgParser2.Create;
begin
  inherited Create;
  PrepareRegex;
end;

procedure TMsgParser2.Test(const s: string);
begin

end;

procedure TMsgParser2.ResetVars;
begin
  ErrorMsg := '';
  MsgType := TMsgType.mtNone;

  RaceCount := 2;
  ITCount := 0;
  StartlistCount := 8;

  EventName := 'EN';
  InputMode := False;

  sDivision := '*';
  sRace := '0';
  sIT := '0';
  sBib := '0';
  sRank := '0';
  sTime := '00:00:00.00';
  sQU := 'ok';
  sValue := '';
end;

procedure TMsgParser2.ResetGroups;
begin
  gc := 0;
  g1 := 'g1';
  g2 := 'g2';
  g3 := 'g3';
  g4 := 'g4';
end;

function TMsgParser2.TrimLine(const s: string): string;
var
  i: Integer;
begin
  i := Pos('=', s);
  if i > 0 then
    result := Trim(Copy(s, 1, i - 1)) + '=' + Trim(Copy(s, i + 1, Length(s)))
  else
    result := s;
end;

procedure TMsgParser2.RetrieveGroupMatch(mc: TMatchCollection);
var
  mi: TMatch;
begin
  gc := 0;
  if mc.Count > 0 then
  begin
    mi := mc.Item[0];
    gc := mi.Groups.Count;
    if gc > 4 then
      g4 := mi.Groups.Item[4].Value;
    if gc > 3 then
      g3 := mi.Groups.Item[3].Value;
    if gc > 2 then
      g2 := mi.Groups.Item[2].Value;
    if gc > 1 then
      g1 := mi.Groups.Item[1].Value;
  end;
end;

function TMsgParser2.ParseLine(const ALine: string): Boolean;
var
  t: string;
begin
  ResetGroups;
  ResetVars;
  t := TrimLine(ALine);
  try
    result := ParseLn(t);
  except
    on e: Exception do
    begin
      result := False;
      ErrorMsg := e.Message;
    end;
  end;

  Race := StrToIntDef(sRace, 99);
  IT := StrToIntDef(sIT, 99);
  Bib := StrToIntDef(sBib, 99);
  Rank := StrToIntDef(sRank, 0);
end;

function TMsgParser2.ParseLn(const t: string): Boolean;
var
  mc: TMatchCollection;
begin
  result := True;

  Test('FR.*.W1.Bib3.Rank = 5');
  //regex.Create('^FR.*.W(\d+).Bib(\d+).Rank=(-*\d+)$');
  mc := reRK.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 4 then
  begin
    MsgType := TMsgType.mtRank;
    sDivision := '*';
    sRace := g1;
    sBib := g2;
    sRank := g3;
    sValue := g3;
    Exit;
  end;

  Test('FR.*.W1.Bib2.FT = 12:03:58.20');
  //reFT.Create('^FR.*.W(\d+).Bib(\d+).FT=(\S+)$');
  mc := reFT.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 4 then
  begin
    MsgType := TMsgType.mtTime;
    sDivision := '*';
    sRace := g1;
    sBib := g2;
    sIT := '0';
    sTime := g3;
    sValue := g3;
    Exit;
  end;

  Test('FR.420.W22.Bib34.IT2 = 05:03:57.21');
  //reIT.Create('^FR.*.W(\d+).Bib(\d+).IT(\d+)=(\S+)$');
  mc := reIT.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 5 then
  begin
    MsgType := TMsgType.mtTime;
    sDivision := '*';
    sRace := g1;
    sBib := g2;
    sIT := g3;
    sTime := g4;
    sValue := g4;
    Exit;
  end;

  Test('FR.*.W1.Bib3.QU = dnf');
  //reQU.Create('^FR.*.W(\d+).Bib(\d+).QU=(dns|dnf|dsq)$');
  mc := reQU.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 4 then
  begin
    MsgType := TMsgType.mtQU;
    sDivision := '*';
    sRace := g1;
    sBib := g2;
    sQU := g3;
    sValue := g3;
    Exit;
  end;

  Test('FR.*.W1.Bib1.RV = 500');
  //reRV.Create('^FR.*.W(\d+).Bib(\d+).RV=(-*\d+)$');
  mc := reRV.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 4 then
  begin
    MsgType := TMsgType.mtRV;
    sDivision := '*';
    sRace := g1;
    sBib := g2;
    sValue := g3;
    Exit;
  end;

  Test('DP.RaceCount = 2');
  //reRaceCount.Create('^DP.RaceCount=(\d+)$');
  mc := reRaceCount.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 2 then
  begin
    MsgType := TMsgType.mtRaceCount;
    RaceCount := StrToIntDef(g1, 2);
    sValue := g1;
    Exit;
  end;

  Test('DP.ITCount = 1');
  //reITCount.Create('^DP.ITCount=(\d+)$');
  mc := reITCount.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 2 then
  begin
    MsgType := TMsgType.mtITCount;
    ITCount := StrToIntDef(g1, 0);
    sIT := g1;
    sValue := g1;
    Exit;
  end;

  Test('DP.StartistCount = 8');
  //reStartlistCount.Create('^DP.StartlistCount=(\d+)$');
  mc := reStartlistCount.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 2 then
  begin
    MsgType := TMsgType.mtStartlistCount;
    StartlistCount := StrToIntDef(g1, 8);
    sValue := g1;
    Exit;
  end;

  Test('EP.Name = TestEvent');
  //reEventName.Create('^EP.Name=(\w+\s\w+)$');
  mc := reEventName.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 2 then
  begin
    MsgType := TMsgType.mtEventName;
    sValue := g1;
    Exit;
  end;

  Test('EP.IM = Strict');
  //reInputMode.Create('^EP.IM=([Strict|Relaxed])$');
  mc := reInputMode.Matches(t);
  RetrieveGroupMatch(mc);
  if gc = 3 then
  begin
    MsgType := TMsgType.mtInputMode;
    InputMode := g2 = 'Strict';
    sValue := g2;
    Exit;
  end;

  result := False;
end;

procedure TMsgParser2.PrepareRegex;
begin
  Test('FR.*.W1.Bib3.Rank = 5');
  reRK.Create('^FR.*.W(\d+).Bib(\d+).Rank=(-*\d+)$');

  Test('FR.*.W1.Bib1.RV = 500');
  reRV.Create('^FR.*.W(\d+).Bib(\d+).RV=(-*\d+)$');

  Test('FR.*.W1.Bib2.FT = 12:03:58.20');
  reFT.Create('^FR.*.W(\d+).Bib(\d+).FT=(\S+)$');

  Test('FR.420.W22.Bib34.IT2 = 05:03:57.21');
  reIT.Create('^FR.*.W(\d+).Bib(\d+).IT(\d+)=(\S+)$');

  Test('FR.*.W1.Bib3.QU = dnf');
  reQU.Create('^FR.*.W(\d+).Bib(\d+).QU=(DNS|DNF|DSQ|OK|dns|dnf|dsq|ok)$');

  Test('DP.RaceCount = 2');
  reRaceCount.Create('^DP.RaceCount=(\d+)$');

  Test('DP.ITCount = 1');
  reITCount.Create('^DP.ITCount=(\d+)$');

  Test('DP.StartistCount = 8');
  reStartlistCount.Create('^DP.StartlistCount=(\d+)$');

  Test('EP.Name = TestEvent');
  reEventName.Create('^EP.Name=(\w+\s*\w+)$');

  Test('EP.IM = Strict');
  reInputMode.Create('^EP.(IM|InputMode)=(Strict|Relaxed)$');
end;

function TMsgParser2.GenMsg: string;
begin
  result := GenOne(MsgType);
end;

function TMsgParser2.GenOne(mt: TMsgType): string;
begin
  case mt of
    TMsgType.mtRaceCount:
      result := Format('DP.RaceCount=%d', [RaceCount]);
    TMsgType.mtITCount:
      result := Format('DP.ITCount=%d', [ITCount]);
    TMsgType.mtStartlistCount:
      result := Format('DP.StartlistCount=%d', [StartlistCount]);

    TMsgType.mtEventName:
      result := Format('EP.Name=%s', [EventName]);

    TMsgType.mtInputMode:
    begin
      if InputMode then
        result := 'EP.IM=Strict'
      else
        result := 'EP.IM=Relaxed'
    end;

    TMsgType.mtQU:
      result := Format('FR.*.W%d.Bib%d.QU=%s', [Race, Bib, sQU]);
    TMsgType.mtRank:
      result := Format('FR.*.W%d.Bib%d.Rank=%s', [Race, Bib, sRank]);
    TMsgType.mtRV:
      result := Format('FR.*.W%d.Bib%d.RV=%s', [Race, Bib, sValue]);

    TMsgType.mtTime:
      result := Format('FR.*.W%d.Bib%d.IT%d=%s', [Race, Bib, IT, sTime]);

  else
    result := '?';
  end;
end;

procedure TMsgParser2.GenAll(ML: TStrings);
var
  mt: TMsgType;
begin
  for mt := TMsgType.mtRaceCount to High(TMsgType) do
    ML.Add(GenOne(mt));
end;

procedure TMsgParser2.InitTestData(ML: TStrings);
begin
  ML.Clear;
  ML.Add('DP.RaceCount = 3');
  ML.Add('DP.ITCount = 2');
  ML.Add('DP.StartlistCount=7');
  ML.Add('');
  ML.Add('EP.Name = TestEvent');
  ML.Add('EP.Name = Test Event');
  ML.Add('');
  ML.Add('FR.*.W1.Bib5.QU=dns');
  ML.Add('FR.*.W2.Bib6.QU=dsq');
  ML.Add('FR.*.W3.Bib7.QU =dnf');
  ML.Add('FR.*.W4.Bib8.QU =ok');
  ML.Add('');
   ML.Add('FR.*.W1.Bib5.Rank=1');
  ML.Add('FR.*.W2.Bib6.Rank = 22');
  ML.Add('FR.*.W3.Bib7.Rank= 333');
  ML.Add('FR.*.W4.Bib8.Rank=-1');
  ML.Add('');
  ML.Add('FR.Division.W97.Bib98.RV=99');
  ML.Add('FR.*.W51.Bib52.RV = 53');
  ML.Add('FR.*.W5.Bib7.RV=-1');
  ML.Add('');
  ML.Add('FR.*.W1.Bib2.FT=12:03:58.20');
  ML.Add('FR.*.W1.Bib3.IT1=12:03:57.23');
  ML.Add('FR.420.W22.Bib34.IT2 = 05:03:57.21');
  ML.Add('FR.Cadet.W103.Bib52.IT3=12:44:55.66');
  ML.Add('');
  ML.Add('EP.IM = Strict');
  ML.Add('EP.IM = Relaxed');
end;

end.
