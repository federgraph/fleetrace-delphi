unit RiggVar.BO.MsgParser;

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
  RiggVar.BO.MsgToken,
  RiggVar.Util.Classes;

type
  TCommandPathError = (
    Error_None,
    Error_Division,
    Error_RunID,
    Error_Bib,
    Error_Command,
    Error_Value,
    Error_Pos,
    Error_Athlete,
    Error_MsgID,
    Error_Race,
    Error_Option
    );

const
  CommandPathErrorStrings: array[TCommandPathError] of string = (
    'Error_None',
    'Error_Division',
    'Error_RunID',
    'Error_Bib',
    'Error_Command',
    'Error_Value',
    'Error_Pos',
    'Error_Athlete',
    'Error_MsgID',
    'Error_Race',
    'Error_Graph'
    );

type
  TMsgParser = class(TLineParser)
  private
    SL: TStringList;
    FIsValid: Boolean;
    FLastError: TCommandPathError;
    FInput: string;
    FKey: string;
    FValue: string;
    SLCompare: TStringList;
    sToken: string;
    sRest: string;
  protected    
    procedure NextToken;
    function NextTokenX(TokenName: string): Integer;
    function TestTokenName(TokenName: string): Boolean;
    function CompareToken(t: string): string;
    function ParseLeaf: Boolean;
    //
    function ParseDivision: Boolean;
    function ParseRunID: Boolean;
    function ParseCommand: Boolean;
    //
    function ParseAthlete: Boolean;
    function ParseRace: Boolean;
    function ParseBib: Boolean;
    function ParsePos: Boolean;
    function ParseMsgID: Boolean;
    //
    function ParseValue: Boolean;
    function ParseTimeValue: Boolean;
    function ParseStatusValue: Boolean;
    function ParsePositiveIntegerValue: Boolean;
    function ParseBooleanValue: Boolean;
    //
    function IsTimeCommand: Boolean;
    function IsRunID: Boolean;
    function IsStartlistCommand: Boolean;
    function IsAthleteCommand: Boolean;
    function IsProp(Token: string): Boolean;
    function IsNameCommand(Token: string): Boolean;

    function ParseKeyValue(sKey, sValue: string): Boolean; override;
  public
    sAthlete: string;
    sPos: string;
    sMsgID: string;
    //
    sDivision: string;
    sRunID: string;
    sBib: string;
    sCommand: string;
    sValue: string;
    //
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IsValid: Boolean;
    function ErrorVal: Integer;
    
    procedure Report(Memo: TStrings);
    function ReportProt: string;
    function ReportLongProt: string;
  end;

implementation

uses
  RiggVar.BO.Def,
  RiggVar.App.Main;

function TMsgParser.IsAthleteCommand: Boolean;
begin
  result :=
       (sCommand = 'FN')
    or (sCommand = 'LN')
    or (sCommand = 'SN')
    or (sCommand = 'NC')
    or (sCommand = 'GR')
    or (sCommand = 'PB')

    or (sCommand = N_FN)
    or (sCommand = N_LN)
    or (sCommand = N_SN)
    or (sCommand = N_NC)
    or (sCommand = N_GR)
    or (sCommand = N_PB)

    or IsProp(sCommand)
    or IsNameCommand(sCommand);
end;

function TMsgParser.IsStartlistCommand: Boolean;
begin
  result := False;
  if (sCommand = 'Bib') or (sCommand = 'SNR') or (sCommand = 'Count') then
    result := True;
end;

function TMsgParser.IsTimeCommand: Boolean;
begin
  result := (sCommand = 'ST')
    or (Copy(sCommand, 1, 2) = 'IT')
    or (sCommand = 'FT');
end;

function TMsgParser.ParseAthlete: Boolean;
var
  temp: Integer;
begin
  if not Main.Params.WantNames then
  begin
    result := false;
    Exit;
  end;

  temp := NextTokenX(cTokenID);
  if temp > -1 then
  begin
    sAthlete := sToken;
    result := True;
  end
  else
  begin
    sAthlete := '';
    result := False
  end;
  if not result then
    FLastError := Error_Athlete;
end;

function TMsgParser.ParseRace: Boolean;
var
  temp: Integer;
begin
  temp := NextTokenX(cTokenRace);
  if temp > -1 then
  begin
    sRunID := 'W' + sToken;
    result := True;
  end
  else
  begin
    sRunID := '';
    result := False
  end;
  if not result then
    FLastError := Error_Race;
end;

function TMsgParser.ParseBib: Boolean;
var
  temp: Integer;
begin
  temp := NextTokenX('Bib');
  if temp > -1 then
  begin
    sBib := sToken;
    result := True;
  end
  else
  begin
    sBib := '';
    result := False
  end;
  if not Result then
    FLastError := Error_Bib;
end;

function TMsgParser.ParseDivision: Boolean;
begin
  NextToken;
  SLCompare.Clear;
  SLCompare.Add(cTokenB);
  SLCompare.Add('*');
  sDivision := CompareToken(sToken);
  result := sDivision <> '';
  if not result then
    FLastError := Error_Division;
end;

function TMsgParser.ParsePos: Boolean;
var
  temp: Integer;
begin
  temp := NextTokenX('Pos');
  if temp > -1 then
  begin
    sPos := sToken;
    result := True;
  end
  else
  begin
    sPos := '';
    result := False
  end;
  if not result then
    FLastError := Error_Pos;
end;

function TMsgParser.ParsePositiveIntegerValue: Boolean;
begin
  result := StrToIntDef(FValue, -1) > -1;
end;

function TMsgParser.ParseRunID: Boolean;
begin
  NextToken;
  SLCompare.Clear;
  SLCompare.Add(cTokenRace + '1');
  sRunID := CompareToken(sToken);
  result := sRunID <> '';
  if not result then
    FLastError := Error_RunID;
end;

function TMsgParser.ParseStatusValue: Boolean;
begin
  result := true;
  //###
  {
  result := (FValue = 'dnf')
    or (FValue = 'dsq')
    or (FValue = 'dns')
    or (FValue = 'ok')
    or (FValue = '*');
  }
end;

function TMsgParser.ParseTimeValue: Boolean;
begin
  result := True;
end;

function TMsgParser.IsRunID: Boolean;
var
  s: string;
  i: Integer;
  RaceCount: Integer;
begin
  if BO <> nil then
    RaceCount := BO.BOParams.RaceCount
  else
    RaceCount := 1;

  s := Copy(sRunID, 1, 1);
  i := StrToIntDef(Copy(sRunID, 2, Length(sRunID)), -1);
  result := (s = 'W') and (i > 0) and (i <= RaceCount);
end;

function TMsgParser.ParseBooleanValue: Boolean;
var
  s: string;
begin
  s := lowercase(FValue);
  result := (s = 'false') or (s = 'true');
end;

function TMsgParser.IsNameCommand(Token: string): Boolean;
begin
  if (Token = '') then
    result := false
  else if (Length(Token) < 2) then
    result := false
  else if (Token[1] <> 'N') then
    result := false
  else if StrToIntDef(Copy(Token, 2, Length(Token)), -1) = - 1 then
    result := false
  else
    result := true;
end;

constructor TMsgParser.Create;
begin
  inherited Create;
  SL := TStringList.Create;
  SLCompare := TStringList.Create;
end;

destructor TMsgParser.Destroy;
begin
  SL.Free;
  SLCompare.Free;
  inherited;
end;

function TMsgParser.IsValid: Boolean;
begin
  result := FIsValid;
end;

function TMsgParser.ErrorVal: Integer;
begin
  result := Integer(FLastError);
end;

procedure TMsgParser.Clear;
begin
  sAthlete := '';
  sDivision := '';
  sRunID := '';
  sBib := '';
  sCommand := '';
  sValue := '';
  sPos := '';
  //
  FLastError := Error_None;
  FIsValid := False;
  FInput := '';
  FKey := '';
  FValue := '';
end;

procedure TMsgParser.Report(Memo: TStrings);
begin
  Memo.Add('Input: ' + FInput);
  Memo.Add('Division: ' + sDivision);
  Memo.Add('RunID: ' + sRunID);
  Memo.Add('Bib: ' + sBib);
  if IsStartlistCommand then
  begin
    if sCommand = 'Count' then
      Memo.Add('Startlist.Count: ' + sValue)
    else
      Memo.Add('Pos: ' + sPos);
  end;
  if IsAthleteCommand then
    Memo.Add('Athlete: ' + sAthlete);
  Memo.Add('Command: ' + sCommand);
  Memo.Add('Value: ' + sValue);
  if not IsValid then
    Memo.Add('Error: ' + CommandPathErrorStrings[FLastError] + '(' + IntToStr(ErrorVal) + ')');
end;

function TMsgParser.ParseKeyValue(sKey, sValue: string): Boolean;
begin
  result := False;
  Clear;
  FKey := sKey;
  FValue := sValue;
  FInput := sKey + '=' + sValue;
  sRest := sKey;

  if TestTokenName(cTokenA) then
    NextToken;

  if (sToken = cTokenA) and TestTokenName('Input') then
    {  consume Token Input }
    NextToken;

  if TestTokenName('Msg') then
    if not ParseMsgID then Exit;

  { Division }
  if not ParseDivision then Exit;

  { property Division.Penalty }
  if ParseLeaf then
  begin
    result := True;
    Exit;
  end;

  if TestTokenName(cTokenID) then
  begin
    if not ParseAthlete then Exit;
  end

  else
  begin
    if TestTokenName(cTokenRace) then
    begin
      if not ParseRace then Exit;

      { property RaceX.IsRacing }
      if ParseLeaf then
      begin
        result := True;
        Exit;
      end;
    end

    { RunID }
    else if not ParseRunID then exit;

    if TestTokenName('STL') then
    begin
      NextToken;

      { property Startlist.Count }
      if ParseLeaf then
      begin
        result := True;
        Exit;
      end;

      { Pos }
      if not ParsePos then Exit;
    end

    else if IsRunID then
    begin
      if TestTokenName('Pos') then
      begin
        { Pos }
        if not ParsePos then Exit;
      end
      else
      begin
        { Bib }
        if not ParseBib then Exit;
      end;
    end;
  end;

  result := ParseLeaf;
end;

function TMsgParser.ParseCommand: Boolean;
var
  I: Integer;
begin
  SLCompare.Clear;

  SLCompare.Add('XX');
  SLCompare.Add('IsRacing');

  SLCompare.Add('FM');
  SLCompare.Add('QU');
  SLCompare.Add('ST');
  if BO <> nil then
    for i := 0 to BO.BOParams.ITCount do
      SLCompare.Add('IT' + IntToStr(i));
  SLCompare.Add('FT');
  SLCompare.Add('DG');
  SLCompare.Add('Rank');
  SLCompare.Add('RV');

  if sPos <> '' then
  begin
    SLCompare.Add('SNR');
    SLCompare.Add('Bib');
  end;

  if sAthlete <> '' then
  begin
    SLCompare.Add('SNR');
    //
    SLCompare.Add('FN');
    SLCompare.Add('LN');
    SLCompare.Add('SN');
    SLCompare.Add('NC');
    SLCompare.Add('GR');
    SLCompare.Add('PB');
    //
    SLCompare.Add(N_FN);
    SLCompare.Add(N_LN);
    SLCompare.Add(N_SN);
    SLCompare.Add(N_NC);
    SLCompare.Add(N_GR);
    SLCompare.Add(N_PB);

    for i := 0 to BO.StammdatenNode.StammdatenRowCollection.FieldCount do
      SLCompare.Add('N' + IntToStr(i));
  end;

  SLCompare.Add('Count');

  sCommand := CompareToken(sRest);
  result := sCommand <> '';
end;

function TMsgParser.ParseValue: Boolean;
begin
  sValue := '';
  result := False;

  if sCommand = 'XX' then
    result := True

  else if (sCommand = 'QU') then
    result := ParseStatusValue

  else if IsTimeCommand then
    result := ParseTimeValue

  else if (sCommand = 'DG')
    or (sCommand = 'Bib')
    or (sCommand = 'SNR')
    or (sCommand = 'Count')
    or (sCommand='Rank')
    or (sCommand='FM')
  then
    result := ParsePositiveIntegerValue

  else if IsAthleteCommand then
    result := True

  else if sCommand = 'IsRacing' then
    result := ParseBooleanValue

  else if (sCommand = 'RV') then
    result := true; //ParseRaceValue

  if result then
    sValue := FValue;
end;

function TMsgParser.ReportProt: string;
var
  sep: string;
begin
  sep := '.';

  if not Self.IsValid then
    result := 'Error: ' + CommandPathErrorStrings[FLastError] + '(' + IntToStr(ErrorVal) + ')'

  else if Self.IsAthleteCommand then
    result :=
      cTokenID + sAthlete + sep +
      sCommand + ' := ' +
      sValue + ';'

  else if sCommand = 'Count' then
    result :=
      sDivision + sep +
      sRunID + sep +
      'STL' + sep +
      sCommand + ' := ' +
      sValue + ';'

  else if Self.IsStartlistCommand then
    result :=
      sDivision + sep +
      sRunID + sep +
      'STL' + sep +
      sPos + sep +
      sCommand + ' := ' +
      sValue + ';'

  else if Self.IsRunID then
  begin
    if sPos <> '' then
      result :=
        sDivision + sep +
        sRunID + sep +
        sPos + sep +
        sCommand + ' := ' +
        sValue + ';'
    else if sBib <> '' then
      result :=
        sDivision + sep +
        sRunID + sep +
        sBib + sep +
        sCommand + ' := ' +
        sValue + ';'
  end;
end;

function TMsgParser.ReportLongProt: string;
var
  sep: string;
begin
  sep := '.';

  if not Self.IsValid then
    result := 'Error: ' + CommandPathErrorStrings[FLastError] + '(' + IntToStr(ErrorVal) + ')'

  else if Self.IsAthleteCommand then
    result :=
      cTokenID + sAthlete + sep +
      LongToken(sCommand) + ' := ' +
      sValue + ';'

  else if sCommand = 'Count' then
    result :=
      LongToken(sDivision) + sep +
      LongToken(sRunID) + sep +
      LongToken('STL') + sep +
      sCommand + ' := ' +
      sValue + ';'

  else if Self.IsStartlistCommand then
    result :=
      LongToken(sDivision) + sep +
      LongToken(sRunID) + sep +
      LongToken('STL') + sep +
      'Pos' + sPos + sep +
      LongToken(sCommand) + ' := ' +
      sValue + ';'

  else if Self.IsRunID then
  begin
    if sPos <> '' then
      result :=
        LongToken(sDivision) + sep +
        LongToken(sRunID) + sep +
        'Pos' + sBib + sep +
        LongToken(sCommand) + ' := ' +
        sValue + ';'
    else if sBib <> '' then
      result :=
        LongToken(sDivision) + sep +
        LongToken(sRunID) + sep +
        'Bib' + sBib + sep +
        LongToken(sCommand) + ' := ' +
        sValue + ';'
  end;
end;

function TMsgParser.ParseMsgID: Boolean;
var
  temp: Integer;
begin
  temp := NextTokenX('Msg');
  if temp > -1 then
  begin
    sMsgID := sToken;
    result := True;
  end
  else
  begin
    sMsgID := '';
    result := False
  end;
  if not result then
    FLastError := Error_MsgID;
end;

procedure TMsgParser.NextToken;
begin
  sRest := TUtils.Cut('.', sRest, sToken);
end;

function TMsgParser.NextTokenX(TokenName: string): Integer;
var
  l: Integer;
begin
  NextToken;
  result := -1;
  if TUtils.StartsWith(sToken, TokenName) then
  begin
    l := Length(TokenName);
    sToken := Copy(sToken, l+1, Length(sToken) - l);
    result := StrToIntDef(sToken, -1);
  end;
end;

function TMsgParser.TestTokenName(TokenName: string): Boolean;
var
  LongTokenName: string;
begin
  LongTokenName := LongToken(TokenName);
  result := TUtils.StartsWith(sRest, TokenName) or TUtils.StartsWith(sRest, LongTokenName);
end;

function TMsgParser.CompareToken(t: string): string;
var
  i: Integer;
  s: string;
begin
  result := '';
  for i := 0 to SLCompare.Count-1 do
  begin
    s := SLCompare[i];
    if (t = s) or (t = LongToken(s)) then
    begin
      result := s;
      exit;
    end
  end;

  if IsProp(t) then
    result := t;
end;

function TMsgParser.IsProp(Token: string): Boolean;
begin
  result := TUtils.StartsWith(Token, 'Prop_');
end;

function TMsgParser.ParseLeaf: Boolean;
begin
  result := False;
  FIsValid := False;

  { Command }
  if not ParseCommand then
  begin
    FLastError := Error_Command;
    Exit;
  end;
  { Value }
  if not ParseValue then
  begin
    FLastError := Error_Value;
    Exit;
  end;

  FIsValid := True;
  result := True;
end;

end.
