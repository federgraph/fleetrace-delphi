unit RiggVar.Util.LoggerMemo;

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
  Windows, SysUtils, Classes, StdCtrls,
  RiggVar.Util.Logger,
  RiggVar.DAL.Redirector;

type
  TMemoLogger = class(TLogger)
  private
    Verbose: Boolean;

    TraceList: TStringList;
    FTraceFileName: string;

    FPause: Boolean;
    FTraceEnabled: Boolean;
    FStartTime: TDateTime;
    FStartCounter: Int64;
    FPerfFrequenz: Int64;
    procedure SeTMemoLoggerEnabled(Value: Boolean);
    procedure SetPause(const Value: Boolean);
  protected
    function GetTraceFileName: string;
  public
    InsertFirst: Boolean;
    MaxLines: Integer;
    TraceMemo: TMemo;

    constructor Create;
    destructor Destroy; override;

    procedure AddLine(Sender: TObject; s: string);
    procedure AddToTraceMemo(s: string);
    procedure AddToTraceMemoT(s: string; T: Int64);
    procedure BeginPause;
    procedure EndPause;

    procedure TraceTimeStat(const s: string; T: TDateTime; stat: string = '----');
    procedure TraceDest(const s: string; Dest: TTraceDestination);

    function GetDisplayString(const s: string): string;
    function RemoveCtrlChars(const s: string): string;

    property Pause: Boolean read FPause write SetPause;
    property TraceEnabled: Boolean read FTraceEnabled write SeTMemoLoggerEnabled;
    property TraceFileName: string read GetTraceFileName;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TMemoLogger.Create;
begin
  inherited Create;

  Verbose := True;
  //InsertFirst := True;
  MaxLines := 50;
  FStartTime := Now;
  QueryPerformanceCounter(FStartCounter);
  QueryPerformanceFrequency(FPerfFrequenz);

  TraceList := TDBStringList.Create;
end;

destructor TMemoLogger.Destroy;
begin
  TraceList.Free;
  inherited;
end;

procedure TMemoLogger.AddLine(Sender: TObject; s: string);
begin
  Info(s);
end;

procedure TMemoLogger.SeTMemoLoggerEnabled(Value: Boolean);
begin
  if not Assigned(TraceMemo) then
  begin
    FTraceEnabled := False;
    Exit;
  end;

  FTraceEnabled := Value;
  if Value = False then
    TraceMemo.Clear;
  if Value = True then
    TraceMemo.Lines := TraceList;
end;

procedure TMemoLogger.BeginPause;
begin
  if not Assigned(TraceMemo) then
  begin
    FTraceEnabled := False;
    Exit;
  end;
  TraceMemo.Lines.BeginUpdate;
  try
    TraceMemo.Lines := TraceList;
    FPause := True;
  finally
    TraceMemo.Lines.EndUpdate;
  end;
  TraceMemo.Lines.Add('');
end;

procedure TMemoLogger.EndPause;
begin
  if not Assigned(TraceMemo) then
  begin
    FTraceEnabled := False;
    Exit;
  end;
  TraceMemo.Lines.Clear;
  FPause := False;
end;

procedure TMemoLogger.AddToTraceMemo(s: string);
var
  i, j: Integer;
  M: TStrings;
begin
  if not Assigned(TraceMemo) then
    Exit;
  if not TraceMemo.Visible then
    Exit;
  if TraceEnabled then
  begin
    M := TraceMemo.Lines;
    if InsertFirst then
    begin
      j := M.Count - MaxLines;
      if j > 1 then
        for i := 1 to j do
          M.Delete(j);

      j := M.Count - MaxLines;
      if j > 0 then
        M.Delete(M.Count - 1);
      M.Insert(0, s);
    end
    else
    begin
      j := TraceMemo.Lines.Count - MaxLines;
      if j > 1 then
        for i := 1 to j do
          TraceMemo.Lines.Delete(0);

      j := TraceMemo.Lines.Count - MaxLines;
      if j > 0 then
        TraceMemo.Lines.Delete(0);
      TraceMemo.Lines.Add(s);
    end;
  end;
end;

procedure TMemoLogger.AddToTraceMemoT(s: string; T: Int64);
var
  Txt, DateStr: string;
  Datum: TDateTime;
  Hour, Min, Sec, MSec: Word;
  temp: Int64;
begin
  try
    temp := Round(((T - FStartCounter) / FPerfFrequenz) * 1000);
    MSec := temp mod 1000;

    temp := temp div 1000;
    Sec := temp mod 60;

    temp := temp div 60;
    Min := temp mod 60;

    temp := temp div 60;
    Hour := temp mod 60;

    Datum := EncodeTime(Hour, Min, Sec, MSec);
    Datum := FStartTime + Datum;
    DecodeTime(Datum, Hour, Min, Sec, MSec);
    DateStr := Format('%2.2d:%2.2d:%2.2d:%3.3d ', [Hour, Min, Sec, MSec]);

    Txt := DateStr + s;
  except
    Txt := s;
    Datum := Now;
  end;

  TraceTimeStat(s, Datum);
end;

procedure TMemoLogger.SetPause(const Value: Boolean);
begin
  if Value then
    BeginPause
  else
    EndPause;
end;

function TMemoLogger.GetTraceFileName: string;
begin
  if FTraceFileName = '' then
  begin
    FTraceFileName := Main.FolderInfo.TracePath +
      '_Trace.txt';
    if FindCmdLineSwitch('T', ['-','/'], False) then
      FTraceFileName := Main.FolderInfo.TracePath +
        '_Trace_' + FormatDateTime('yymmdd_hhnnss', Now) + '.txt';
  end;
  result := FTraceFileName;
end;

function TMemoLogger.RemoveCtrlChars(const s: string): string;
var
  i: Integer;
  C: Char;
  b: Byte;
  temp: string;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    C := s[i];
    b := Byte(C);
    if b < 32 then
      temp := '' //temp := Format('<%d>', [b])
    else
      temp := C;
    Result := Result + temp;
  end;
end;

function TMemoLogger.GetDisplayString(const s: string): string;
var
  i: Integer;
  C: Char;
  b: Byte;
  temp: string;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    C := s[i];
    b := Byte(C);
    if b < 32 then
      temp := Format('<%d>', [b])
    else
      temp := C;
    Result := Result + temp;
  end;
end;

procedure TMemoLogger.TraceDest(const s: string; Dest: TTraceDestination);
begin
  case Dest of
    tdDelphiEventLog: OutputDebugString(PChar(s));
  else
    TraceTimeStat(s, 0);
  end;
end;

procedure TMemoLogger.TraceTimeStat(const s: string; T: TDateTime; Stat: string = '----');
var
  Txt, DateStr: string;
  Datum: TDateTime;
  Hour, Min, Sec, MSec: Word;
  f: TextFile;
begin
  try
    {String aufbereiten}
    Txt := GetDisplayString(s);

    if T = 0 then
      Datum := Now
    else
      Datum := T;
    DecodeTime(Datum, Hour, Min, Sec, MSec);
    DateStr := Format('%2.2d:%2.2d:%2.2d.%3.3d', [Hour, Min, Sec, MSec]);

    Txt := Format('%s; %s; %s', [DateStr, Stat, Txt]);

    //String in TraceList vom Typ TStringList schreiben
    if TraceList.Count = 200 then
      TraceList.Delete(0);
    TraceList.Add(Txt);

    //String in TraceMemo schreiben
    if not Pause then
      AddToTraceMemo(Txt);

    if IsConsole then
      WriteLn(Txt);

    //String in Datei schreiben
    if GetTraceFileName = '' then Exit;
    AssignFile(f, GetTraceFileName);
    Append(f);
    try
      WriteLn(f, Txt);
    finally
      CloseFile(f);
    end;
  except
  end;
end;

end.
