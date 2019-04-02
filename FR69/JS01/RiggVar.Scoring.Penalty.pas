unit RiggVar.Scoring.Penalty;

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

(*
--------------------------------------------------------------------------------
APPENDIX A - SCORING
See rule 88.3.

A1 NUMBER OF RACES
The number of races scheduled and the number required to be completed to
constitute a series shall be stated in the sailing instructions.

A2 SERIES SCORES
Each boat's series score shall be the total of her race scores excluding her
worst score. (The sailing instructions may make a different arrangement by
providing, for example, that no score will be excluded, that two or more
scores will be excluded, or that a specified number of scores will be
excluded if a specified number of races are completed.) If a boat has two
or more equal worst scores, the score( s) for the race( s) sailed earliest
in the series shall be excluded. The boat with the lowest series score wins
and others shall be ranked accordingly.

A3 STARTING TIMES AND FINISHING PLACES
The time of a boat's starting signal shall be her starting time, and the order
in which boats finish a race shall determine their finishing places. However,
when a handicap system is used a boat's elapsed time, corrected to the nearest
second, shall determine her finishing place.

A4 LOW POINT AND BONUS POINT SYSTEMS
Most series are scored using either the Low Point System or the Bonus Point
System. The Low Point System uses a boat's finishing place as her race score.
The Bonus Point System benefits the first six finishers because of the greater
difficulty in advancing from fourth place to third, for example, than from
fourteenth place to thirteenth. The system chosen may be made to apply by
stating in the sailing instructions that, for example, 'The series will be
scored as provided in Appendix A of the racing rules using the
[Low] [Bonus] Point System.'

A4.1 Each boat starting and finishing and not thereafter retiring, being
penalized or given redress shall be scored points as follows:

Finishing place Low Point System Bonus Point System
First 1 0
Second 2 3
Third 3 5.7
Fourth 4 8
Fifth 5 10
Sixth 6 11.7
Seventh 7 13
Each place thereafter Add 1 point Add 1 point

A4.2 A boat that did not start, did not finish, retired after finishing or was
disqualified shall be scored points for the finishing place one more than the
number of boats entered in the series. A boat penalized under rule 30.2 or 44.3
shall be scored points as provided in rule 44.3(c).

A5 SCORES DETERMINED BY THE RACE COMMITTEE
A boat that did not start, comply with rule 30.2 or 30.3, or finish, or that
takes a penalty under rule 44.3 or retires after finishing, shall be scored
accordingly by the race committee without a hearing. Only the protest committee
may take other scoring actions that worsen a boat's score.

A6 CHANGES IN PLACES AND SCORES OF OTHER BOATS
(a) If a boat is disqualified from a race or retires after finishing, each boat
that finished after her shall be moved up one place.

(b) If the protest committee decides to give redress by adjusting a boat's
score, the scores of other boats shall not be changed unless the protest
committee decides otherwise.

A7 RACE TIES
If boats are tied at the finishing line or if a handicap system is used and
boats have equal corrected times, the points for the place for which the boats
have tied and for the place(s) immediately below shall be added together and
divided equally. Boats tied for a race prize shall share it or be given
equal prizes.

A8 SERIES TIES
A8.1 If there is a series score tie between two or more boats, each boat's
race scores shall be listed in order of best to worst, and at the first point(s)
where there is a difference the tie shall be broken in favour of the boat(s)
with the best score(s). No excluded scores shall be used.

A8.2 If a tie remains between two boats, it shall be broken in favour of the
boat that scored better than the other boat in more races. If more than two
boats are tied, they shall be ranked in order of the number of times each boat
scored better than another of the tied boats. No race for which a tied boat's
score has been excluded shall be used. [Rule A8.2 is deleted and rule A8.3 is
renumbered as A8.2. Change effective as from 1 June 2002.]

A8.3 2 If a tie still remains between two or more boats, they shall be ranked
in order of their scores in the last race. Any remaining ties shall be broken
by using the tied boats' scores in the next-to-last race and so on until all
ties are broken. These scores shall be used even if some of them are excluded
scores.

A9 RACE SCORES IN A SERIES LONGER THAN A REGATTA
For a series that is held over a period of time longer than a regatta, a boat
that came to the starting area but did not start, did not finish, retired after
finishing or was disqualified shall be scored points for the finishing place
one more than the number of boats that came to the starting area. A boat that
did not come to the starting area shall be scored points for the finishing
place one more than the number of boats entered in the series.

A10 GUIDANCE ON REDRESS
If the protest committee decides to give redress by adjusting a boat's score
for a race, it is advised to consider scoring her

(a) points equal to the average, to the nearest tenth of a point
(0.05 to be rounded upward), of her points in all the races in the series
except the race in question;

(b) points equal to the average, to the nearest tenth of a point (0.05 to be
rounded upward), of her points in all the races before the race in question;

or

(c) points based on the position of the boat in the race at the time of the
incident that justified redress.

A11 SCORING ABBREVIATIONS
These abbreviations are recommended for recording the circumstances described:

DNC Did not start; did not come to the starting area
DNS Did not start (other than DNC and OCS)
OCS Did not start; on the course side of the starting line and broke rule 29.1 or 30.1
ZFP 20% penalty under rule 30. 2
UFD U-Flag Discqualification under rule 30.3
BFD Black-Flag Disqualification under rule 30.4
DGM Disqualification Gross Misconduct
SCP Took a scoring penalty under rule 44.3
DNF Did not finish
RAF Retired after finishing
DSQ Disqualification
DNE Disqualification not excludable under rule 88.3(b)
RDG Redress given
DPI Descretionary penalty imposed
--------------------------------------------------------------------------------
*)

const
	SAILTIME_NOTIME: Int64 = High(Int64); //MaxInt;
	SAILTIME_NOTIME_STRING: string = 'No Time';

const
	TLE_DNF = 0;
	TLE_FINISHERSPLUS1 = 1;
	TLE_FINISHERSPLUS2 = 2;
	TLE_AVERAGE = 3;

	THROWOUT_BYNUMRACES = 1;
	THROWOUT_PERXRACES = 2;
	THROWOUT_BESTXRACES = 3;
	THROWOUT_NONE = 4;

  TIEBREAK_INCREMENT = 0.0001;

	TIE_RRS_DEFAULT = 1;
	TIE_RRS_A82_ONLY = 2;

const
  ISAF_NOP = $0000; // no penalty

{ Penalty values are/should be used only internally to the program and NOT
written out to persistent storage (that is done by string name). Therefore it
should be safe to reset the values, provided the orders are not changed and the
types of penalties keep their bit-boundaries straight. }

{ Disqualification penalties
are the various ways a boat can be disqualified.
These are not bitwise, as a boat can only be disqualified once.
But a boat can be disqualified with or without a valid finish.
So a boat can carry both a non-finish penalty and a disqualification penalty.
See Penalty class. }

  ISAF_DSQ = $0001; //Disqualification
  ISAF_DNE = $0002; //Disqualification not excludable under rule 88.3(b)
  ISAF_RAF = $0003; //Retired after finishing
  ISAF_OCS = $0004; //Did not start; on the course side of the starting line and broke rule 29.1 or 30.1
  ISAF_BFD = $0005; //Disqualification under rule 30.3 - black flag
  ISAF_DGM = $0006; //disqualified for gross misconduct, 2005 addition
  ISAF_UFD = $0007; ///Disqualification under rule 30.4 - U-Flag

{ Other scoring penalties
Application uses a set of these Other-Penalties.
These are the various other ways a boat can be "dinged".
These include check-in penalties, redress given, percentage penalties, etc.
These ARE Bit-wise penalties, and a boat can have more than one of them.
Also, a boat CAN have both a non-finish penalty and other-penalty,
but may not have a disqualification penalty and "other" penalty.
See Penalty class. }

  ISAF_STP = $0008; //Olympic Data Feed - Standard Penalty
  ISAF_TIM = $0010; //time limit
  ISAF_ZFP = $0020; //20% penalty under rule 30.2
  ISAF_AVG = $0040; //average
  ISAF_SCP = $0080; //Took a scoring penalty under rule 44.3
  ISAF_RDG = $0100; //Redress given
  ISAF_MAN = $0200; //manual
  ISAF_CNF = $0400; //check-in failure
  ISAF_TMP = $0800; //scoring time penalty, pct (percent) of time
  ISAF_DPI = $1000; //descretionary penalty imposed

{ Non-finishing penalties
They show up in the finish order column
They can get set as Finish "Positions" - meaning no finish recorded.
Non-Finish penalty values are for boats that do not have a valid Finish.
These are used in BOTH the FinishPosition class and in the Penalty class.
These are not-bitwise penalties.
A boat cannot have more than one of these at a time.
See FinishPosition class. }

  ISAF_HIF = $1FFF; //highest possible finish
  ISAF_TLE = $6000;
  ISAF_DNF = $8000; //Did not finish
  ISAF_DNS = $A000; //Did not start (other than DNC and OCS)
  ISAF_DNC = $C000; //Did not start; did not come to the starting area
  ISAF_NOF = $E000; //unspecified NoFinish value

  { These masks break up the integer into the portions reserved for each penalty type. }

  ISAF_DSQ_MASK = $0007;
  ISAF_OTH_MASK = $1FF8;
  ISAF_NOF_MASK = $E000;

type
  TISAFPenaltyDSQ = (
    NoDSQ,
    DSQ,
    DNE,
    RAF,
    OCS,
    BFD,
    DGM,
    UFD
  );

  TISAFPenaltyNoFinish = (
    NoFinishBlank,
    TLE,
    DNF,
    DNS,
    DNC
  //NOF
    );

  TISAFPenaltyOther = (
    STP, //standard penalty
    TIM, //time limit
    ZFP,
    AVG, //average
    SCP, //scoring penalty, pct (percent) of finish position
    RDG, //redress given
    MAN, //manual
    CNF, //check-in failure
    TMP, //scoring time penalty, pct (percent) of time
    DPI  //descretionary penalty imposed
    );

  TISAFPenaltySet = set of TISAFPenaltyOther;

const
  PenaltyDSQStrings: array[TISAFPenaltyDSQ] of string = (
    '',
    'DSQ',
    'DNE',
    'RAF',
    'OCS',
    'BFD',
    'DGM',
    'UFD'
    );

  PenaltyNoFinishStrings: array[TISAFPenaltyNoFinish] of string = (
    '',
    'TLE',
    'DNF',
    'DNS',
    'DNC'
  //'NOF'
    );

  PenaltyOtherStrings: array[TISAFPenaltyOther] of string = (
    'STP',
    'TIM',
    'ZFP',
    'AVG',
    'SCP',
    'RDG',
    'MAN',
    'CNF',
    'TMP',
    'DPI'
    );

implementation

end.

