unit RiggVar.BO.Validation;

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
  RiggVar.Col.Event,
  RiggVar.BO.MsgToken;

type
  TOTimeErrorList = class(TEventErrorList)
  private
    fl: TCRList;
    FErrorList: TStringList;
    FSortedSL: TStringList;
    function CheckOTimeForAll(cl: TEventRowCollection): Boolean;
    function CheckOTimeForFleet(fl: TCRList; r: Integer): Boolean;
    function CheckOTime(ev: TEventNode): Boolean;
    function CheckBib(ev: TEventNode): Boolean;
    procedure ClearFlags(ev: TEventNode);
    function CheckSNR(ev: TEventNode): Boolean;
    function IsPreconditionForFleet(r: Integer): Boolean;
    function IsPreconditionForAll(cl: TEventRowCollection): Boolean;
    procedure AddContiguousError(cl: TEventRowCollection; r, position: Integer); overload;
    procedure AddContiguousError(fl: TCRList; r, position: Integer); overload;
  protected
    procedure AddEntryError(cr: TEventRowCollectionItem; e: TEntryError);
    procedure AddFinishError(r: Integer; cr: TEventRowCollectionItem; e: TFinishError);
  public
    constructor Create;
    destructor Destroy; override;
    //
    function IsPreconditionForStrictInputMode(ev: TEventNode): Boolean; override;
    function CheckAll(ev: TEventNode): Boolean; override;
    procedure GetMsg(Memo: TStrings); override;
    function HasErrors: Boolean; override;
  end;

const
  FinishErrorStrings: array[TFinishError] of String = (
    'Finish below zero', //error_OutOfRange_OTime_Min,
    'Finish beyond EntryCount', //error_OutOfRange_OTime_Max,
    'duplicate Finish', //error_Duplicate_OTime,
    'Finish gap' //error_Contiguous_OTime,
    );

  EntryErrorStrings: array[TEntryError] of String = (
    'duplicate SNR', //error_Duplicate_SNR,
    'duplicate Bib', //error_Duplicate_Bib,
    'Bib out of range', //error_OutOfRange_Bib,
    'Bib out of range' //error_OutOfRange_SNR
    );

implementation

{ TOTimeErrorList }

constructor TOTimeErrorList.Create;
begin
  inherited Create;
  FErrorList := TStringList.Create;
  FSortedSL := TStringList.Create;
  FSortedSL.Sorted := True;
  fl := TCRList.Create(false);
end;

destructor TOTimeErrorList.Destroy;
begin
  FSortedSL.Free;
  FErrorList.Free;
  fl.Free;
  inherited;
end;

procedure TOTimeErrorList.GetMsg(Memo: TStrings);
var
  i: Integer;
begin
  for i := 0 to FErrorList.Count-1 do
  begin
    Memo.Add(FErrorList[i]);
  end;
end;

function TOTimeErrorList.IsPreconditionForStrictInputMode(ev: TEventNode): Boolean;
var
  f, r: Integer;
  cl: TEventRowCollection;
  fc: Integer;
begin
  result := True;

  if ev.UseFleets then
  begin
    cl := ev.EventRowCollection;
    if cl.Count < 2 then Exit;
    for r := 1 to cl.RCount-1 do
    begin
      fc := cl.FleetCount(r);
      for f := 0 to fc do
      begin
        cl.FillFleetList(fl, r, f);
        if fl.Count > 0 then
          result := IsPreconditionForFleet(r);
        if not result then
          break;
      end;
      if not result then
        break;
    end;
    fl.Clear;
  end
  else
    result := IsPreconditionForAll(ev.EventRowCollection);
end;

function TOTimeErrorList.IsPreconditionForAll(cl: TEventRowCollection): Boolean;
var
  i, r: Integer;
  a: array of Integer;
  cr: TEventRowCollectionItem;
  EntryCount: Integer;
  temp: Integer;
  HasNull: Boolean;
begin
  result := False;

  //Exit at the first encounter of an error, only boolean result is important
  if cl.Count < 2 then Exit;
  EntryCount := cl.Count;
  SetLength(a, EntryCount + 1);
  for r := 1 to cl.RCount-1 do
  begin
    { clear array slots }
    for i := 0 to Length(a)-1 do
    begin
      a[i] := 0;
    end;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      temp := cr.Race[r].OTime;
      if temp < 0 then Exit; //below lower limit
      if temp > EntryCount then Exit; //beyond upper limit
      if (temp > 0) and (a[temp] = 1) then Exit; //has duplicates
      a[temp] := 1;
    end;
    HasNull := False;
    for i := 1 to cl.Count-1 do
    begin
      if (a[i] = 1) and HasNull then Exit; //not contiguous
      if a[i] = 0 then
        HasNull := True;
    end;
  end;
  result := True;
end;

function TOTimeErrorList.IsPreconditionForFleet(r: Integer): Boolean;
var
  i: Integer;
  a: array of Integer;
  cr: TEventRowCollectionItem;
  EntryCount: Integer;
  temp: Integer;
  HasNull: Boolean;
begin
  result := False;

  //Exit at the first encounter of an error, only boolean result is important
  if fl.Count < 2 then Exit;
  EntryCount := fl.Count;
  SetLength(a, EntryCount + 1);

  { clear array slots }
  for i := 0 to Length(a)-1 do
  begin
    a[i] := 0;
  end;
  for i := 0 to fl.Count-1 do
  begin
    cr := fl.CR[i];
    temp := cr.Race[r].OTime;
    if temp < 0 then Exit; //below lower limit
    if temp > EntryCount then Exit; //beyond upper limit
    if (temp > 0) and (a[temp] = 1) then Exit; //has duplicates
    a[temp] := 1;
  end;
  HasNull := False;
  for i := 1 to fl.Count-1 do
  begin
    if (a[i] = 1) and HasNull then Exit; //not contiguous
    if a[i] = 0 then
      HasNull := True;
  end;

  result := True;
end;

function TOTimeErrorList.CheckAll(ev: TEventNode): Boolean;
begin
  FErrorList.Clear;
  ClearFlags(ev);
  CheckOTime(ev);
  CheckBib(ev);
  CheckSNR(ev);
  result := FErrorList.Count > 0;
end;

procedure TOTimeErrorList.ClearFlags(ev: TEventNode);
var
  i, r: Integer;
  cr: TEventRowCollectionItem;
  cl: TEventRowCollection;
begin
  cl := ev.EventRowCollection;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    cr.EntryErrors := [];
    for r := 1 to cl.RCount-1 do
      cr.Race[r].FinishErrors := [];
  end;
end;

function TOTimeErrorList.CheckBib(ev: TEventNode): Boolean;
var
  i: Integer;
  cr: TEventRowCollectionItem;
  cl: TEventRowCollection;
  s: string;
begin
  { Bib must be unique,
    should be > 0 }
  result := True;
  cl := ev.EventRowCollection;

  FSortedSL.Clear;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    s := IntToStr(cr.Bib);
    { check for duplicates }
    if FSortedSL.IndexOf(s) > -1 then
    begin
      Include(cr.EntryErrors, error_Duplicate_Bib);
      AddEntryError(cr, error_Duplicate_Bib);
      result := False;
    end
    else
      FSortedSL.Add(s);
  end;
end;

function TOTimeErrorList.CheckSNR(ev: TEventNode): Boolean;
var
  i: Integer;
  cr: TEventRowCollectionItem;
  cl: TEventRowCollection;
  s: string;
begin
  { SNR must be unique,
    must be > 0 }
  result := True;
  cl := ev.EventRowCollection;

  FSortedSL.Clear;
  for i := 0 to cl.Count-1 do
  begin
    cr := cl.Items[i];
    s := IntToStr(cr.SNR);
    { check for duplicates }
    if FSortedSL.IndexOf(s) > -1 then
    begin
      Include(cr.EntryErrors, error_Duplicate_SNR);
      AddEntryError(cr, error_Duplicate_SNR);
      result := False;
    end
    else
      FSortedSL.Add(s);
  end;
end;

function TOTimeErrorList.CheckOTime(ev: TEventNode): Boolean;
var
  r, f, fc: Integer;
  cl: TEventRowCollection;
  hasError: Boolean;
begin
  hasError := false;

  if ev.UseFleets then
  begin
    cl := ev.EventRowCollection;
    for r := 1 to cl.RCount-1 do
    begin
      fc := cl.FleetCount(r);
      for f := 0 to fc do
      begin
        ev.EventRowCollection.FillFleetList(fl, r, f);
        if fl.Count > 0 then
          if CheckOTimeForFleet(fl, r) then
            hasError := True;
      end;
    end;
    fl.Clear;
  end
  else
    hasError := CheckOTimeForAll(ev.EventRowCollection);

  result := hasError;
end;

function TOTimeErrorList.CheckOTimeForFleet(fl: TCRList; r: Integer): Boolean;
var
  i: Integer;
  a: array of Integer;
  cr: TEventRowCollectionItem;
  EntryCount: Integer;
  temp: Integer;
  HasNull: Boolean;
  re: TEventRaceEntry;
  position: Integer;
  oldErrorCount: Integer;
begin
  oldErrorCount := FErrorList.Count;
  EntryCount := fl.Count;
  SetLength(a, EntryCount + 1);

  { clear array slots }
  for i := 0 to Length(a)-1 do
  begin
    a[i] := 0;
  end;
  for i := 0 to fl.Count-1 do
  begin
    cr := fl.CR[i];
    re := cr.Race[r];
    temp := re.OTime;
    if temp < 0 then
    begin
      Include(re.FinishErrors, error_OutOfRange_OTime_Min);
      AddFinishError(r, cr, error_OutOfRange_OTime_Min) //below lower limit
    end
    else if temp > EntryCount then
    begin
      Include(re.FinishErrors, error_OutOfRange_OTime_Max);
      AddFinishError(r, cr, error_OutOfRange_OTime_Max) //beyond upper limit
    end
    else if (temp > 0) and (a[temp] = 1) then
    begin
      Include(re.FinishErrors, error_Duplicate_OTime);
      AddFinishError(r, cr, error_Duplicate_OTime) //has duplicates
    end
    else
      a[temp] := 1;
  end;
  HasNull := False;
  for position := 1 to EntryCount do
  begin
    if (a[position] = 1) and HasNull then
    begin
      AddContiguousError(fl, r, position);
      HasNull := False;
    end;
    if a[position] = 0 then
      HasNull := True;
  end;
  result := FErrorList.Count > oldErrorCount;
end;

procedure TOTimeErrorList.AddContiguousError(cl: TEventRowCollection; r: Integer; position: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
  re: TEventRaceEntry;
begin
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    re := cr.Race[r];
    if re.OTime = position then
    begin
      Include(re.FinishErrors, error_Contiguous_OTime);
      AddFinishError(r, cr, error_Contiguous_OTime); //not contiguous
    end;
  end;
end;

procedure TOTimeErrorList.AddContiguousError(fl: TCRList; r: Integer; position: Integer);
var
  i: Integer;
  cr: TEventRowCollectionItem;
  re: TEventRaceEntry;
begin
  for i := 0 to fl.Count - 1 do
  begin
    cr := fl.CR[i];
    re := cr.Race[r];
    if re.OTime = position then
    begin
      Include(re.FinishErrors, error_Contiguous_OTime);
      AddFinishError(r, cr, error_Contiguous_OTime); //not contiguous
    end;
  end;
end;

function TOTimeErrorList.CheckOTimeForAll(cl: TEventRowCollection): Boolean;
var
  i, r: Integer;
  a: array of Integer;
  cr: TEventRowCollectionItem;
  EntryCount: Integer;
  temp: Integer;
  HasNull: Boolean;
  re: TEventRaceEntry;
  position: Integer;
  oldErrorCount: Integer;
begin
  oldErrorCount := FErrorList.Count;
  EntryCount := cl.Count;
  SetLength(a, EntryCount + 1);
  for r := 1 to cl.RCount-1 do
  begin
    { clear array slots }
    for i := 0 to Length(a)-1 do
    begin
      a[i] := 0;
    end;
    for i := 0 to cl.Count-1 do
    begin
      cr := cl.Items[i];
      re := cr.Race[r];
      temp := re.OTime;
      if temp < 0 then
      begin
        Include(re.FinishErrors, error_OutOfRange_OTime_Min);
        AddFinishError(r, cr, error_OutOfRange_OTime_Min) //below lower limit
      end
      else if temp > EntryCount then
      begin
        Include(re.FinishErrors, error_OutOfRange_OTime_Max);
        AddFinishError(r, cr, error_OutOfRange_OTime_Max) //beyond upper limit
      end
      else if (temp > 0) and (a[temp] = 1) then
      begin
        Include(re.FinishErrors, error_Duplicate_OTime);
        AddFinishError(r, cr, error_Duplicate_OTime) //has duplicates
      end
      else
        a[temp] := 1;
    end;
    HasNull := False;
    for position := 1 to EntryCount do
    begin
      if (a[position] = 1) and HasNull then
      begin
        AddContiguousError(cl, r, position);
        HasNull := False;
      end;
      if a[position] = 0 then
        HasNull := True;
    end;
  end;
  result := FErrorList.Count > oldErrorCount;
end;

procedure TOTimeErrorList.AddFinishError(r: Integer; cr: TEventRowCollectionItem; e: TFinishError);
var
  s: string;
begin
  s := 'Error.' + cTokenSport + cTokenRace + IntToStr(r);
  if Assigned(cr) then
    s := s + '.ID' + IntToStr(cr.BaseID);
  s := s + ' = ' + FinishErrorStrings[e];
  FErrorList.Add(s)
end;

procedure TOTimeErrorList.AddEntryError(cr: TEventRowCollectionItem; e: TEntryError);
var
  s: string;
begin
  s := 'Error.' + cTokenSport;
  if Assigned(cr) then
    s := s + 'ID' + IntToStr(cr.BaseID);
  s := s + ' = ' + EntryErrorStrings[e];
  FErrorList.Add(s)
end;

function TOTimeErrorList.HasErrors: Boolean;
begin
  result := FErrorList.Count > 0;
end;

end.

