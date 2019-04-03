unit RiggVar.BO.UniquaPoints;

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
  RiggVar.Col.Event,
  RiggVar.BO.EventProps,
  RiggVar.BO.Def;

type
  TUniquaPoints = class
  public
    class procedure Calc(qn: TEventNode);
  end;

  TExternCup = class
  private
    f: double; // Faktor
    s: Integer; // Zahl der mindestens einmal gezeiteten Boote
    z: Integer; // Anzahl der Wettfahrten
    P1: double; // Punktzahl des Gesamtersten
    PL: double; // Punktzahl des fiktiven Letzten
    function DoExternalForEach_RA(Value: double): double; //Beispiel RA
    function DoExternalForEach_QR(Value: Integer): double; //Beispiel QR
    procedure DoExternalOnce(aGezeitet, aGesegelt, aScoringSystem2: Integer; aFaktor, aP1, aPL: double);
  public
    procedure DoInternal(qn: TEventNode);
  end;

implementation

class procedure TUniquaPoints.Calc(qn: TEventNode);
var
  u: TEventProps;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  cr1: TEventRowCollectionItem;
  i: Integer;
  //---
  f: double; // Faktor
  s: Integer; // Zahl der mindestens einmal gezeiteten Boote
  z: Integer; // Anzahl der Wettfahrten
  //m: Integer; // Multiplikator
  PL: double; // Punktzahl des fiktiven Letzten
  P1: double; // Punktzahl des Gesamtersten
  PX: double; // Punktzahl des Bootes
  //RA: double; // RanglistenPunkte aus dieser Regatta (kann m mal eingehen)
  Platz: Integer;
  //QR: double; // Punkte aus dieser Regatta for WMA/EMA
begin
  try
    u := BO.EventProps;

    //Ranglistenpunkte
    f := u.Faktor; //Ranglistenfaktor der Regatta
    s := u.Gezeitet; //Zahl der mindestens einmal gezeiteten Boote
    if s = 0 then exit;

    z := u.Gesegelt; //Anzahl der Wettfahrten
    //m := 1; //Multiplikator

    { Punktzahl des fiktiven letzten }
    if u.ScoringSystem = LowPoint then
      PL := s * z // Low Point System
    else
      PL := (s + 6) * z; //Bonus System

    cl := qn.EventRowCollection;

    { Punktzahl des ersten }
    cr1 := cl.Items[cl.Items[0].PLZ];
    if Assigned(cr1) then
      P1 := cr1.GRace.CPoints //schnelle Variante via Platzziffer
    else
    begin
      P1 := 0;
      for i := 0 to cl.Count-1 do
      begin
        cr := cl.Items[i];
        if cr.GRace.CPoints < P1 then
          P1 := cr.GRace.CPoints;
      end;
    end;

    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];

      { Ranglistenpunkte }
      PX := cr.GRace.CPoints; //Punktzahl des Bootes
      if PL - P1 <> 0 then
        cr.RA := f * 100 * (PL - PX) / (PL - P1)
      else
        cr.RA := 0;
      { begrenzen }
      if cr.RA < 0 then
        cr.RA := 0;

      { Punkte for WMA/EMA }
      Platz := cr.GRace.Rank;
      if s <> 0 then
        cr.QR := 100 * (s + 1 - Platz) / s
      else
        cr.QR := 0;
      { begrenzen }
      if cr.QR < 0 then
        cr.QR := 0;

    end;
  except
  end;
end;

{ TExternCup }

function TExternCup.DoExternalForEach_RA(Value: double): double;
var
  PX: double;
  RA: double; // RanglistenPunkte aus dieser Regatta (kann m mal eingehen)
begin
  PX := Value;
  if PL - P1 <> 0 then
    RA := f * 100 * (PL - PX) / (PL - P1)
  else
    RA := 0;
  { begrenzen }
  if RA < 0 then
    RA := 0;

  result := RA;
end;

function TExternCup.DoExternalForEach_QR(Value: Integer): double;
var
  Platz: Integer;
  QR: double; // Punkte aus dieser Regatta for WMA/EMA
begin
  Platz := Value;
  if s <> 0 then
    QR := 100 * (s + 1 - Platz) / s
  else
    QR := 0;
  { begrenzen }
  if QR < 0 then
    QR := 0;

  result := QR;
end;

procedure TExternCup.DoExternalOnce(
    aGezeitet: Integer; // Zahl der mindestens einmal gezeiteten Boote
    aGesegelt: Integer; // Anzahl der Wettfahrten
    aScoringSystem2: Integer;
    aFaktor: double; // Faktor
    aP1: double; // Punktzahl des Gesamtersten
    aPL: double // Punktzahl des fiktiven Letzten
  );
begin
  s := aGesegelt;
  z := aGezeitet;
  //ScoringSystem2 := aScoringSystem2;
  f := aFaktor;
  P1 := aP1;
  PL := aPL;
end;

procedure TExternCup.DoInternal(qn: TEventNode);
var
  u: TEventProps;
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  cr1: TEventRowCollectionItem;
  i: Integer;
begin
  try
    u := BO.EventProps;
    if u.Gezeitet = 0 then exit;

    cl := qn.EventRowCollection;

    { Punktzahl des ersten }
    cr1 := cl.Items[cl.Items[0].PLZ];
    if Assigned(cr1) then
      P1 := cr1.GRace.CPoints //schnelle Variante via Platzziffer
    else
    begin
      P1 := 0;
      for i := 0 to cl.Count-1 do
      begin
        cr := cl.Items[i];
        if cr.GRace.CPoints < P1 then
          P1 := cr.GRace.CPoints;
      end;
    end;

    { Punktzahl des fiktiven Letzten }
    if u.ScoringSystem2 = 0 then
      PL := u.Gezeitet * u.Gesegelt // Low Point System
    else
      PL := (u.Gezeitet + 6) * u.Gesegelt; //Bonus System

    DoExternalOnce(
      u.Gezeitet,
      u.Gesegelt,
      u.ScoringSystem2,
      u.Faktor,
      P1,
      PL
      );

    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];

      { Ranglistenpunkte }
      cr.RA := DoExternalForEach_RA(cr.GRace.CPoints); //Paramter Punktzahl

      { Punkte for WMA/EMA }
      cr.QR := DoExternalForEach_QR(cr.GRace.Rank); //Parameter Platz
    end;
  except
  end;
end;

end.

