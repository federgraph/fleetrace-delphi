unit RiggVar.BO.MsgToken;

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

const
  //NameSchema_N0
  N0_FN = 'FN';
  N0_LN = 'LN';
  N0_SN = 'SN';
  N0_NC = 'NC';
  N0_GR = 'GR';
  N0_PB = 'PB';

  //NameSchema_N1
  N1_FN = 'FirstName';
  N1_LN = 'LastName';
  N1_SN = 'ShortName';
  N1_NC = 'NOC';
  N1_GR = 'Gender';
  N1_PB = 'PersonalBest';

  //FNameSchema_N2
  N2_FN = 'N1';
  N2_LN = 'N2';
  N2_SN = 'N3';
  N2_NC = 'N4';
  N2_GR = 'N5';
  N2_PB = 'N6';

  FixFieldCount = 6;

var
  N_FN: string = N0_FN;
  N_LN: string = N0_LN;
  N_SN: string = N0_SN;
  N_NC: string = N0_NC;
  N_GR: string = N0_GR;
  N_PB: string = N0_PB;

  cAppTitle: string = 'App';

  cTokenID: string = 'SNR';
  cTokenBib: string = 'Bib';
  cTokenCount: string = 'Count';
  cTokenMsg: string = 'Msg';

  //master tokens, specific for application category:
  cTokenA: string = 'FR';
  cTokenB: string = '*';
  cTokenRace: string = 'W';
  cTokenIT: string = 'IT';
  cTokenOption: string = 'Graph';
  cTokenQU: string = 'QU';


  //slave tokens, updated in SetDivisionName at runtime:
  cTokenSport: string = 'FR.*.';
  cTokenOutput: string = 'FR.*.Output.';
  cTokenAnonymousRequest: string = 'FR.*.Request.';
  cTokenAnonymousOutput: string = 'FR.*.Output.';

  cDefaultPortSet: Integer = 3;

function LongToken(t: string): string;
procedure SetDivisionName(const Value: string);

implementation

function LongToken(t: string): string;
begin
  { Command }
  if t = 'QU' then result := 'Status'
  else if t = 'FM' then result := 'Fleet' //FleetMembership
  else if t = 'ST' then result := 'StartTime'
  else if Copy(t, 1, 2) = 'IT' then result := 'IntermediateTime' + Copy(t, 3, Length(t))
  else if t = 'FT' then result := 'FinishTime'
  else if t = 'XX' then result := 'TestMessage'
  else if t = 'DG' then result := 'DSQGate'

  { Startlist }
  else if t = 'STL' then result := 'Startlist'
  else if t = 'Bib' then result := 'Bib'
  else if t = 'SNR' then result := 'AthleteID'

  { Athlete }
  else if t = 'FN' then result := 'FirstName'
  else if t = 'LN' then result := 'LastName'
  else if t = 'SN' then result := 'ShortName'
  //else if t = 'NOC' then result := 'NOC'
  //else if t = 'Gender' then result := 'Gender'
  else if t = 'PB' then result := 'PersonalBest'

  else if t = cTokenA then result := cTokenA + '.OVR.Input'

  else result := t;
end;

procedure SetDivisionName(const Value: string);
begin
  cTokenB := Value;
  cTokenSport := cTokenA + '.' + Value + '.';
  cTokenOutput := cTokenSport + 'Output.';
  cTokenAnonymousRequest := cTokenA + '.*.Request.';
  cTokenAnonymousOutput := cTokenA + '.*.Output.';
end;

end.
