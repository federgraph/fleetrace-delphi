unit RiggVar.RM.TimingButtons;

interface

uses
  System.SysUtils,
  System.Classes;

type
  TTimingButtonHelper = class
  private
    SL: TStrings;
    function CheckRace(r: Integer): Integer;
  protected
    procedure UpdateFromTimepoint;
    procedure UpdateFromEvent;
    procedure FilterOutFinishedBibs;
    procedure InitBowsFromBibs;
  public
    Bibs: TStrings;
    Bows: TStrings;

    constructor Create;
    destructor Destroy; override;

    procedure Update;
  end;

implementation

uses
  RiggVar.BO.Def,
  RiggVar.Col.Event,
  RiggVar.Col.Race;

constructor TTimingButtonHelper.Create;
begin
  SL := TStringList.Create;
  Bibs := TStringList.Create;
  Bows := TStringList.Create;
end;

destructor TTimingButtonHelper.Destroy;
begin
  Bibs.Free;
  Bows.Free;
  SL.Free;
  inherited;
end;

function TTimingButtonHelper.CheckRace(r: Integer): Integer;
begin
  if (r > BO.BOParams.RaceCount) then
    r := BO.BOParams.RaceCount;
  result := r;
end;

procedure TTimingButtonHelper.UpdateFromEvent;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  r: Integer;
  bib: Integer;
  ere: TEventRaceEntry;
begin
  SL.Clear;
  r := CheckRace(BO.Race);

  cl := BO.EventNode.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    bib := cr.Bib;
    ere := cr.Race[r];
    if (ere.OTime = 0) and (ere.Penalty.IsOK) then
    begin
      SL.Add(IntToStr(Bib));
    end;
  end;
  Bibs.Text := SL.Text;
end;

procedure TTimingButtonHelper.FilterOutFinishedBibs;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  r: Integer;
  bib: Integer;
  ere: TEventRaceEntry;
  t: Integer;
begin
  SL.Clear;
  r := CheckRace(BO.Race);

  cl := BO.EventNode.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    bib := cr.Bib;
    ere := cr.Race[r];
    if (ere.OTime <> 0)then
    begin
      SL.Add(IntToStr(Bib));
    end;
  end;

  for i := SL.Count-1 downto 0 do
  begin
    t := Bibs.IndexOf(SL[i]);
    if t >= 0 then
      Bibs.Delete(t);
  end;
end;

procedure TTimingButtonHelper.UpdateFromTimePoint;
var
  cl: TRaceRowCollection;
  cr: TRaceRowCollectionItem;
  i: Integer;
  r: Integer;
  bib: Integer;
  t: Integer;
  tp: TTimePoint;
begin
  SL.Clear;
  r := CheckRace(BO.Race);
  t := BO.IT;

  cl := BO.RNode[r].RaceRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    bib := cr.Bib;
    tp := cr.IT[t];
    if (tp <> nil) and not tp.OTime.TimePresent and cr.QU.IsOK then
    begin
      SL.Add(IntToStr(bib));
    end;
  end;
  Bibs.Text := SL.Text;
end;

procedure TTimingButtonHelper.InitBowsFromBibs;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  s: string;
begin
  SL.Clear;
  Bows.Clear;
  cl := BO.EventNode.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    s := IntToStr(cr.Bib);
    if Bibs.IndexOf(s) = -1 then
      Bows.Add(s);
  end;
end;

procedure TTimingButtonHelper.Update;
begin
  //UpdateFromEvent;
  UpdateFromTimepoint;
  FilterOutFinishedBibs;
  InitBowsFromBibs;
end;


end.
