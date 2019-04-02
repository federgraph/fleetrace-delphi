unit RiggVar.BO.Msg;

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
  RiggVar.BO.MsgBase,
  RiggVar.BO.MsgToken,
  RiggVar.BO.MsgParser,
  RiggVar.Col.Event,
  RiggVar.Col.Race,
  RiggVar.Util.Classes;

type
  TBOMsg = class(TBaseMsg)
  private
    SLRequest: TStrings;
    procedure ProcessRequestHeader;
    procedure ProcessRequestInput;
    function GetColName: string;
    procedure HandleProt;
    function GetRaceIndex: Integer;
  protected
    function FindRaceCR: TRaceRowCollectionItem;
    function HandleRaceMsg(cr: TRaceRowCollectionItem): Boolean;
  public
    MsgParser: TMsgParser;
    ItemPos: Integer;
    AthleteID: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure ClearResult; override;
    function DispatchProt: Boolean; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TBOMsg }

constructor TBOMsg.Create;
begin
  inherited Create;
  MsgParser := TMsgParser.Create;
  SLRequest := TStringList.Create;
  Inc(Main.TestCounter.MsgCreateCounter);
end;

destructor TBOMsg.Destroy;
begin
  MsgParser.Free;
  SLRequest.Free;
  Inc(Main.TestCounter.MsgDestroyCounter);
  inherited;
end;

function TBOMsg.DispatchProt: Boolean;
var
  l: Integer;
  s: string;
  IsRequest: Boolean;
begin
  Inc(Main.TestCounter.MsgDispatchCounter);
  result := False;
  ClearResult;

  { ignore previously reported Errors in text }
  if TUtils.StartsWith(prot, 'Error') then
  begin
    Inc(Main.TestCounter.MsgErrorCounter);
    result := true;
    Exit;
  end;

  { ignore empty lines or comments }
  if (prot = '') or TUtils.StartsWith(prot, '//') or TUtils.StartsWith(prot, '#') then
  begin
    result := True;
    Exit;
  end;

  if (Copy(prot, 1, Length('Manage.')) = 'Manage.') then
  begin
    BO.StatusFeedback.ParseLine(prot);
    result := True;
    Exit;
  end;

  if TUtils.StartsWith(prot, 'EP.') or TUtils.StartsWith(prot, 'Event.Prop_') then
  begin
    BO.EventProps.ParseLine(prot);
    result := True;
    Exit;
  end;

  { ignore params here }
  if TUtils.StartsWith(prot, 'DP.') or TUtils.StartsWith(prot, 'Event.') then
  begin
    result := True;
    Exit;
  end;

  { test for request}
  IsRequest := False;
  { 1st try }
  s := cTokenA + '.' + cTokenB + '.Request.';
  l := Length(s);
  if (Copy(prot, 1, l) = s) then
    IsRequest := True;
  { 2nd try }
  if not IsRequest then
  begin
    s := cTokenAnonymousRequest;
    l := Length(s);
    if (Copy(prot, 1, l) = s) then
      IsRequest := True;
  end;
  { 3rd try }
  if not IsRequest then
  begin
    s := 'RiggVar.Request.';
    l := Length(s);
    if (Copy(prot, 1, l) = s) then
      IsRequest := True;
  end;

  { if IsRequest then handle it }
  if IsRequest then
  begin
    SLRequest.Text := prot;
    if SLRequest.Count > 0 then
    begin
      { eine Request-Zeile, wie bisher }
      MsgValue := Copy(SLRequest[0], l + 1, Length(SLRequest[0]));
      Cmd := cTokenOutput;
      MsgValue := cTokenOutput + MsgValue;
      SLRequest.Delete(0);
      OutputRequestList.Add(MsgValue);

      { alle weiteren Output-Request-Zeilen}
      ProcessRequestHeader;

      { process body of message }
      ProcessRequestInput;

      prot := ''; //wird nicht mehr gebraucht
      result := True; //send answer later
      { Msg will be added to the queue,
        retrieved after calculation,
        and only then is output generated and sent. }
    end;
    MsgResult := 1; //do not reply with MsgID now
    Exit;
  end;

  l := Length(cTokenA);
  if Copy(prot, 1, l + 1) = cTokenA + '.' then
  begin
    result := MsgParser.ParseLine(prot);
    if result then
    begin
      Self.Division := MsgParser.sDivision;
      Self.RunID := MsgParser.sRunID;
      Self.Bib := StrToIntDef(MsgParser.sBib, -1);
      Self.MsgValue := MsgParser.sValue;
      Self.Cmd := MsgParser.sCommand;
      Self.ItemPos := StrToIntDef(MsgParser.sPos, -1);
      Self.AthleteID := StrToIntDef(MsgParser.sAthlete, -1);
      Self.DBID := StrToIntDef(MsgParser.sMsgID, -1);
      HandleProt; //setzt MsgResult auf Null, wenn ok
    end;
  end;
end;

procedure TBOMsg.HandleProt;
var
  MsgHandled: Boolean;
  wr: TRaceRowCollectionItem;
  temp: string;
begin
  MsgResult := 1;
  MsgHandled := False;

  { Testmessage }
  if Cmd = 'XX' then
  begin
    //if Verbose then Trace('HandleProt: Testmessage');
  end

  else if Cmd = 'Count' then
    MsgHandled := BO.UpdateStartlistCount({Division +} RunID, StrToIntDef(MsgValue, -1))

  else if AthleteID > 0 then
  begin
    MsgHandled := BO.UpdateAthlete(AthleteID, Cmd, MsgValue);
  end

  else if Cmd = 'IsRacing' then
  begin
    if BO.FindRaceIndex(RunID) > -1 then
      BO.SetRunIsRacing(RunID, MsgValue = BoolStr[True]);
  end

  else
  begin
    temp := LowerCase(MsgValue);
    if (temp = 'empty') or (temp = 'null') or (temp = '99:99:99.99') then
      MsgValue := '-1';

    wr := FindRaceCR;
    if Assigned(wr) then
      MsgHandled := HandleRaceMsg(wr);
  end;

  if MsgHandled then
  begin
    Inc(BO.CounterMsgHandled);
    MsgResult := 0;
  end;
end;

function TBOMsg.HandleRaceMsg(cr: TRaceRowCollectionItem): Boolean;
var
  o: TRaceBO;
  s: string;
  channel: Integer;
  crev: TEventRowCollectionItem;
  r: Integer;
begin
  o := cr.ru.RaceBO;

  s := MsgValue;

  if (Cmd = 'ST') or (Cmd = 'SC') then
    o.EditST(cr, s)

  else if (Copy(Cmd, 1, 2) = 'IT') or (Copy(Cmd, 1, 2) = 'FC') then
  begin
    channel := StrToIntDef(Copy(Cmd, 3, Length(Cmd)),  -1);
    if channel > -1 then
      o.EditIT(cr, s, 'col_IT' + IntToStr(channel));
  end

  else if (Cmd = 'FT') or (Cmd = 'FC') then
    o.EditFT(cr, s)
  else if (Cmd = 'QU') then
    o.EditQU(cr, s)
  else if (Cmd = 'DG') then
    o.EditDG(cr, s)
  else if (Cmd = 'Rank') then
    o.EditOTime(cr, s)

  else if (Cmd = 'RV') then
  begin
    crev := BO.EventNode.EventRowCollection.Items[cr.Index];
    if Assigned(crev) then
      BO.EventBO.EditRaceValue(crev, s, GetColName);
  end
  else if (Cmd = 'FM') then
  begin
    crev := BO.EventNode.EventRowCollection.Items[cr.Index];
    r := GetRaceIndex;
    if r <> -1 then
      crev.Race[r].Fleet := StrToIntDef(s, crev.Race[r].Fleet);
  end

  else if (Cmd = 'Bib') then
    o.EditBib(cr, s) //--> wird horizontal kopiert, bo.Bib[Index] := cr.Bib
  else if (Cmd = 'SNR') then
    o.EditSNR(cr, s); //--> wird horizontal kopiert, bo.SNR[Index] := cr.SNR

  result := True;
end;

function TBOMsg.GetColName: string;
var
  i: Integer;
  s: string;
begin
  result := '';
  if Copy(RunID, 1, 1) <> 'W' then
    exit;
  s := Copy(RunID, 2, Length(RunID));
  i := StrToIntDef(s, -1);
  if (i < 1) or (i > BO.BOParams.RaceCount) then
    exit;
  result := 'col_R' + IntToStr(i);
end;

function TBOMsg.GetRaceIndex: Integer;
var
  i: Integer;
  s: string;
begin
  result := -1;
  if Copy(RunID, 1, 1) <> 'W' then
    exit;
  s := Copy(RunID, 2, Length(RunID));
  i := StrToIntDef(s, -1);
  if (i < 1) or (i > BO.BOParams.RaceCount) then
    i := -1;
  result := i;
end;

function TBOMsg.FindRaceCR: TRaceRowCollectionItem;
var
  qn: TRaceNode;
begin
  result := nil;
  qn := bo.FindNode(RunID);
  if Assigned(qn) then
  begin
    if ItemPos > 0 then
      result := qn.RaceRowCollection.Items[ItemPos - 1]
    else
      result := qn.FindBib(Bib);
  end;
end;

procedure TBOMsg.ClearResult;
begin
  inherited;
  ItemPos := -1;
end;

procedure TBOMsg.ProcessRequestHeader;
var
  s: string;
  b: Boolean;
  l: Integer;
begin
  l := Length(cTokenA + '.Request.');
  repeat
    if SLRequest.Count > 0 then
      b := Copy(SLRequest[0], 1, l) = cTokenA + '.Request.'
    else
      b := false;
    if b then
    begin
      s := Copy(SLRequest[0], l + 1, Length(SLRequest[0]));
      OutputRequestList.Add(cTokenOutput + s);
      SLRequest.Delete(0);
    end
  until
    b = False;
end;

procedure TBOMsg.ProcessRequestInput;
var
  i: Integer;
  msg: TBOMsg;
begin
  if SLRequest.Count > 0 then
  begin
    msg := TBOMsg.Create;
    try
      for i := 0 to SLRequest.Count - 1 do
      begin
        msg.prot := SLRequest[i];
        msg.DispatchProt;
      end;
    finally
      msg.Free;
    end;
  end;
  SLRequest.Clear;
end;

end.
