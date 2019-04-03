unit RiggVar.Calc.EventProxy01;

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
  System.Classes,
  RiggVar.Calc.EV,
  RiggVar.Col.Event,
  RiggVar.BO.Time;

type
  { Proxy for simple internal scoring }
  TSimpleCalcEventProxy = class(TCalcEventProxy)
  private
    { in }
    Bib: array of Integer;
    DSQGate: array of Integer;
    Status: array of Integer;
    OTime: array of Integer;

    { out }
    Rank: array of Integer;
    PosR: array of Integer;
    PLZ: array of Integer;
    BTime: array of Integer; //behind best at TimePoint
    //
    BestIndex: Integer;
    BestOTime: Integer;

    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
  protected
    procedure Calc1;
    procedure Calc_BestIdx;
    procedure Calc_BTime;
    procedure EncodeDSQGateAndStatus;
    procedure Calc_Rank_PosR_Encoded;
    function IsOut(Value: Integer): Boolean;
    function IsOK(Value: Integer): Boolean;
  public
    procedure Calc(qn: TEventNode); override;
    procedure LoadProxy(qn: TEventNode; channel: Integer);
    procedure UnLoadProxy(qn: TEventNode; channel: Integer);
    property Count: Integer read GetSize write SetSize;
  end;

implementation

{ TCalcEventProxy1 }

function TSimpleCalcEventProxy.IsOK(Value: Integer): Boolean;
begin
  result := (Value = Status_OK) or (Value = Status_DSQPending);
end;

function TSimpleCalcEventProxy.IsOut(Value: Integer): Boolean;
begin
  result := (Value = Status_DSQ) or (Value = Status_DNF) or (Value = Status_DNS);
end;

function TSimpleCalcEventProxy.GetSize: Integer;
begin
  result := Length(Rank);
end;

procedure TSimpleCalcEventProxy.SetSize(const Value: Integer);
begin
  if (Value <> Length(Rank)) and (Value >= 0) then
  begin
    SetLength(Bib, Value);
    SetLength(DSQGate, Value);
    SetLength(Status, Value);
    SetLength(OTime, Value);
    SetLength(BTime, Value);
    SetLength(Rank, Value);
    SetLength(PosR, Value);
    SetLength(PLZ, Value);
  end;
end;

procedure TSimpleCalcEventProxy.Calc1;
begin
  //Calc_BestIdx;
  //Calc_BTime;
  EncodeDSQGateAndStatus;
  Calc_Rank_PosR_Encoded;
end;

procedure TSimpleCalcEventProxy.Calc_BestIdx;
var
  i: Integer;
  t: Integer;
begin
  BestIndex := 0;
  BestOTime := MaxInt;
  for i := 0 to Count-1 do
  begin
    t := OTime[i];
    if (t > 0) and (t < BestOTime) and IsOK(Status[i]) then
    begin
      BestIndex := i;
      BestOTime := OTime[i];
    end;
  end;
end;

procedure TSimpleCalcEventProxy.Calc_BTime;
var
  i: Integer;
begin
  if BestOTime = TimeNULL then
  begin
    for i := 0 to Count-1 do
      BTime[i] := TimeNULL;
  end
  else
  begin
    for i := 0 to Count-1 do
      if OTime[i] > 0 then
        BTime[i] :=  OTime[i] - BestOTime
      else
        BTime[i] := TimeNULL;
  end;
end;

procedure TSimpleCalcEventProxy.EncodeDSQGateAndStatus;
var
  i: Integer;
  temp: Integer;
begin
  for i := 0 to Count-1 do
  begin
    temp := OTime[i];
    if Status[i] = Status_DNF then
      temp := MaxInt - 300
    else if Status[i] = Status_DSQ then
      temp := MaxInt - 200
    else if Status[i] = Status_DNS then
      temp := MaxInt - 100;
    //temp := temp - DSQGate[i];
    OTime[i] := temp;
  end;
end;

procedure TSimpleCalcEventProxy.Calc_Rank_PosR_Encoded;
var
  j, l: Integer;
  t1, t2: Integer; //Zeit1 und Zeit2
  BibMerker: LongInt; //wegen 'Highest Bib goes first'
  temp: Integer;
begin
  { reset }
  for j := 0 to Count - 1 do
  begin
    Rank[j] := 1;
    PosR[j] := 1;
    PLZ[j] := -1;
  end;

  { new calculation }
  for j := 0 to count - 1 do
  begin
    t2 := OTime[j];
    BibMerker := Bib[j];
    { TimePresent = False }
    if t2 <= 0 then
    begin
      Rank[j] := 0;
      PosR[j] := 0;
    end
    { TimePresent }
    else
    begin
      for l := j + 1 to count - 1 do
      begin
        t1 := OTime[l];
        if t1 > 0 then
        begin
          if t1 < t2 then
          begin
            { increment Rank and PosR for j }
            Rank[j] := Rank[j] + 1;
            PosR[j] := PosR[j] + 1;
          end;

          if t1 > t2 then
          begin
            { increment Rank and PosR for l }
            Rank[l] := Rank[l] + 1;
            PosR[l] := PosR[l] + 1;
          end;

          if t1 = t2 then
          begin
            { do not increment Rank if Times are equal
              increment PosR for one of the riders, j or l }
            if HighestBibGoesFirst then
            begin
              if BibMerker < Bib[l] then
                PosR[l] := PosR[l] + 1
              else
                PosR[j] := PosR[j] + 1;
            end
            else
            begin
              if BibMerker > Bib[l] then
                PosR[l] := PosR[l] + 1
              else
                PosR[j] := PosR[j] + 1;
            end;
          end;
        end;
      end;
      if PosR[j] > 0 then
      begin
        temp := PosR[j];
        PLZ[temp-1] := j;
      end;
    end;
  end;
end;

procedure TSimpleCalcEventProxy.LoadProxy(qn: TEventNode; channel: Integer);
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  cl := qn.EventRowCollection;
  Count := cl.Count;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    Bib[i] := cr.Bib;
    DSQGate[i] := cr.Race[channel].DG;
    Status[i] := cr.Race[channel].QU;
    if channel = channel_FT then
      OTime[i] := cr.Race[channel].CTime1
    else
      OTime[i] := cr.Race[channel].OTime;
  end;
end;

procedure TSimpleCalcEventProxy.UnLoadProxy(qn: TEventNode; channel: Integer);
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
begin
  cl := qn.EventRowCollection;
  if Count <> cl.Count then exit;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    //cr.Race[channel].BTime := BTime[i];
    //cr.ru.BestTime[channel] := BestOTime;
    //cr.ru.BestIndex[channel] := BestIndex;
    cr.Race[channel].Rank := Rank[i];
    cr.Race[channel].PosR := PosR[i];
    cr.Race[channel].PLZ := PLZ[i];
  end;
end;

procedure TSimpleCalcEventProxy.Calc(qn: TEventNode);
var
  i: Integer;
  RaceCount: Integer;
  //
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  j: Integer;
  GPoints: Integer;
begin
  if qn.EventRowCollection.Count < 1 then exit;
  RaceCount := qn.EventRowCollection.Items[0].RCount;
  for i := 1 to RaceCount-1 do
  begin
    LoadProxy(qn, i);
    Calc1;
    UnLoadProxy(qn, i);
  end;

  { Points }
  cl := qn.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    GPoints := 0;
    for j := 1 to cr.RCount-1 do
    begin
      { RacePoints }
      cr.Race[j].CTime1 := cr.Race[j].Rank;

      cr.Race[j].Drop := False;
      if cr.Race[j].QU <> 0 then
      begin
        cr.Race[j].CTime1 := 400;
        cr.Race[j].Drop := True;
      end;
      {
      case cr.Race[j].QU of
        Status_DNF: cr.Race[j].CTime := 100;
        Status_DSQ: cr.Race[j].CTime := 200;
        Status_DNS: cr.Race[j].CTime := 300;
      end;
      }

      { EventPoints }
      GPoints := GPoints + cr.Race[j].CTime1;
    end;
    cr.GRace.CTime1 := GPoints;
  end;

  LoadProxy(qn, channel_FT);
  Calc1;
  UnLoadProxy(qn, channel_FT);
end;

end.
