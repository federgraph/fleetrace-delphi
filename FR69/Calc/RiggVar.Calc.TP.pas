unit RiggVar.Calc.TP;

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
  RiggVar.Grid.ColGrid,
  RiggVar.Col.Race,
  RiggVar.BO.Time,
  RiggVar.BO.Params;

type
  TIntArray = array of Integer;

  TQProxy = class
  private
    FHighestBibGoesFirst: Boolean;
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
  public
    { in }
    Bib: array of Integer;
    DSQGate: array of Integer;
    Status: array of Integer;
    OTime: array of Integer;

    { out }
    ORank: array of Integer;
    Rank: array of Integer;
    PosR: array of Integer;
    PLZ: array of Integer;
    TimeBehind: array of Integer; //behind best at TimePoint
    TimeBehindPreviousLeader: array of Integer; //behind previous best at TimePoint
    TimeBehindLeader: array of Integer; //behind previous best at Finish
    //
    BestIndex: Integer;
    BestOTime: Integer;

    procedure Calc; virtual;
    function IsOut(Value: Integer): Boolean;
    function IsOK(Value: Integer): Boolean;
    property Count: Integer read GetSize write SetSize;
    property HighestBibGoesFirst: Boolean read FHighestBibGoesFirst write FHighestBibGoesFirst;
  end;

  TQProxy1 = class(TQProxy)
  private
  protected
    procedure Calc_BestIdx;
    procedure Calc_TimeBehind;
    procedure EncodeDSQGateAndStatus;
    procedure Calc_Rank_PosR_Encoded;
    procedure Calc_ORank;
  public
    procedure Calc; override;
  end;

  TCalcTP = class
  private
    qn: TRaceNode;
    FProxy: TQProxy;
    procedure LoadProxy(qn: TRaceNode; channel: Integer);
    procedure UnLoadProxy(qn: TRaceNode; channel: Integer);
    procedure CalcQA;
    function GetHighestBibGoesFirst: Boolean;
    procedure SetHighestBibGoesFirst(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Calc(aqn: TRaceNode);
    procedure UpdateDynamicBehind(bo: TRaceBO; cr: TRaceRowCollectionItem; channel: Integer);
    property HighestBibGoesFirst: Boolean read GetHighestBibGoesFirst write SetHighestBibGoesFirst;
  end;

implementation

uses
  RiggVar.BO.Def;

{ TQProxy }

function TQProxy.IsOK(Value: Integer): Boolean;
begin
  result := (Value = Status_OK) or (Value = Status_DSQPending);
end;

function TQProxy.IsOut(Value: Integer): Boolean;
begin
  result := (Value = Status_DSQ) or (Value = Status_DNF) or (Value = Status_DNS);
end;

procedure TQProxy.Calc;
begin
  //virtual
end;

function TQProxy.GetSize: Integer;
begin
  result := Length(Rank);
end;

procedure TQProxy.SetSize(const Value: Integer);
begin
  if (Value <> Length(Rank)) and (Value >= 0) then
  begin
    SetLength(Bib, Value);
    SetLength(DSQGate, Value);
    SetLength(Status, Value);
    SetLength(OTime, Value);
    SetLength(ORank, Value);
    SetLength(Rank, Value);
    SetLength(PosR, Value);
    SetLength(TimeBehind, Value);
    SetLength(PLZ, Value);
  end;
end;

{ TQProxy1 }

procedure TQProxy1.Calc;
begin
  Calc_ORank;
  Calc_BestIdx;
  Calc_TimeBehind;
  EncodeDSQGateAndStatus;
  Calc_Rank_PosR_Encoded;
end;

procedure TQProxy1.Calc_BestIdx;
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

procedure TQProxy1.Calc_TimeBehind;
var
  i: Integer;
begin
  if BestOTime = TimeNULL then
  begin
    for i := 0 to Count-1 do
      TimeBehind[i] := TimeNULL;
  end
  else
  begin
    for i := 0 to Count-1 do
      if OTime[i] > 0 then
        TimeBehind[i] :=  OTime[i] - BestOTime
      else
        TimeBehind[i] := TimeNULL;
  end;
end;

procedure TQProxy1.EncodeDSQGateAndStatus;
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
    temp := temp - DSQGate[i];
    OTime[i] := temp;
  end;
end;

procedure TQProxy1.Calc_Rank_PosR_Encoded;
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
              if BibMerker > Bib[l] then
                PosR[l] := PosR[l] + 1
              else
                PosR[j] := PosR[j] + 1;
            end
            else
            begin
              if BibMerker < Bib[l] then
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

procedure TQProxy1.Calc_ORank;
var
  j, l: Integer;
  t1, t2: Integer;
begin
  for j := 0 to Count - 1 do
    ORank[j] := 1;
  for j := 0 to count - 1 do
  begin
    t2 := OTime[j];
    if t2 <= 0 then
      ORank[j] := 0
    else
    begin
      for l := j + 1 to count - 1 do
      begin
        t1 := OTime[l];
        if t1 > 0 then
        begin
          if t1 < t2 then
            ORank[j] := ORank[j] + 1;
          if t1 > t2 then
            ORank[l] := ORank[l] + 1;
        end;
      end;
    end;
  end;
end;

{ TCalcTP }

constructor TCalcTP.Create;
begin
  inherited Create;
  FProxy := TQProxy1.Create;
end;

destructor TCalcTP.Destroy;
begin
  FProxy.Free;
  inherited;
end;

procedure TCalcTP.LoadProxy(qn: TRaceNode; channel: Integer);
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  cl := qn.RaceRowCollection;
  FProxy.Count := cl.Count;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    FProxy.Bib[i] := cr.Bib;
    FProxy.DSQGate[i] := cr.DG;

    //QU.AsInteger (PenaltyISAF) does not map to expected values
    //--> map all penalties to 'Status_DSQ'
    //Status <> 0 causes inclusion in ranking
    if cr.QU.IsOut then
      FProxy.Status[i] := Status_DSQ //cr.QU.AsInteger;
    else
      FProxy.Status[i] := 0;

    FProxy.OTime[i] := cr.IT[channel].OTime.AsInteger;
  end;
end;

procedure TCalcTP.UnLoadProxy(qn: TRaceNode; channel: Integer);
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
begin
  cl := qn.RaceRowCollection;
  if FProxy.Count <> cl.Count then exit;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    cr.IT[channel].Behind.AsInteger := FProxy.TimeBehind[i];
    cr.IT[channel].ORank := FProxy.ORank[i];
    cr.IT[channel].Rank := FProxy.Rank[i];
    cr.IT[channel].PosR := FProxy.PosR[i];
    cr.IT[channel].PLZ := FProxy.PLZ[i];
    cr.ru.BestTime[channel] := FProxy.BestOTime;
    cr.ru.BestIndex[channel] := FProxy.BestIndex;
  end;
end;

procedure TCalcTP.Calc(aqn: TRaceNode);
begin
  qn := aqn;
  CalcQA;
end;

procedure TCalcTP.CalcQA;
var
  i: Integer;
begin
  for i := 0 to BO.BOParams.ITCount do
  begin
    LoadProxy(qn, i);
    FProxy.Calc;
    UnLoadProxy(qn, i);
  end;
end;

function TCalcTP.GetHighestBibGoesFirst: Boolean;
begin
  result := FProxy.HighestBibGoesFirst;
end;

procedure TCalcTP.SetHighestBibGoesFirst(const Value: Boolean);
begin
  FProxy.HighestBibGoesFirst := Value;
end;

procedure TCalcTP.UpdateDynamicBehind(bo: TRaceBO;
  cr: TRaceRowCollectionItem; channel: Integer);
var
  cl: TRaceRowCollection;
  rb: TBaseNode;
  rd: TRaceNode;
  refTime: TPTime;
begin
  if cr = nil then exit;
  rb := cr.GetBaseNode;
  if rb is TRaceNode then
  begin
    rd := cr.GetBaseNode as TRaceNode;

    { Zwischenzeiten }
    if channel > 0 then
    begin
      { TimeBehind in Bezug auf die Zwischenzeit des IT-Besten }
      cr.IT[channel].BPL.UpdateQualiTimeBehind(
        rd.BestTime[channel],
        cr.IT[channel].OTime.AsInteger);

      { TimeBehind in Bezug auf die Zwischenzeit des FT-Besten }
      cl := rd.RaceRowCollection;
      refTime := cl.Items[rd.BestIndex[channel_FT]].IT[channel].OTime;
      if (rd.BestIndex[channel_FT] <> cr.Index) and (refTime.TimePresent) then
        cr.IT[channel].BFT.UpdateQualiTimeBehind(
          refTime.AsInteger,
          cr.IT[channel].OTime.AsInteger)
      else
        cr.IT[channel].BFT.Clear;

    end

    { Zielzeit }
    else
      cr.FT.BPL.UpdateQualiTimeBehind(
        rd.BestTime[channel_FT],
        cr.FT.OTime.AsInteger);

  end;
end;

end.
