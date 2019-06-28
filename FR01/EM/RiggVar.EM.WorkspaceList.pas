unit RiggVar.EM.WorkspaceList;

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
  RiggVar.Util.Classes,
  RiggVar.EM.WorkspaceListBase;

type
  TWorkspaceList = class(TWorkspaceListBase)
  private
    FStop: Boolean;
    FError: string;
    FCounter: Integer;
    FWantNames: Boolean;

    procedure SetWantNames(const Value: Boolean);
    function GetWorkspaceListFileName: string;

    procedure ProcessTL;
    function GetIsEmpty: Boolean;
    function GetWantDefault: Boolean;
    procedure Reset;
  protected
    NL: TStringList;
    FK: string;
    FV: TWorkspaceUrl;
//    procedure DecomposeUrl(s: string);
    procedure ParseLine(const s: string);
    procedure AddEntry;
    function GetAppDataRoot: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(FromMemo: Boolean); override;
    procedure FillCombo(Combo: TStrings); override;

    procedure Load; override;
    procedure Save; override;

    function GetName(i: Integer): string; override;
    function GetUrl(i: Integer): string; override;
    function IsWritable(i: Integer): Boolean; override;

    function GetText: string; override;
    procedure SetText(Value: string); override;

    property WantNames: Boolean read FWantNames write SetWantNames;
    property IsEmpty: Boolean read GetIsEmpty;
    property WantDefault: Boolean read GetWantDefault;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.SHFolder,
  RiggVar.Util.AppUtils;

const
  StrWorkspaceList = 'fr-workspace-urls.txt';

constructor TWorkspaceList.Create;
begin
  inherited Create;
  Inc(FCounter);
  NL := TStringList.Create;
  FV := TWorkspaceUrl.Create;
  WantNames := true;
end;

destructor TWorkspaceList.Destroy;
begin
  FV.Free;
  NL.Free;
  inherited;
end;

procedure TWorkspaceList.Reset;
begin
  FStop := False;
  VL.Clear;
  NL.Clear;
end;

procedure TWorkspaceList.Init(FromMemo: Boolean);
begin
  Reset;
  if FromMemo then
  begin
    { assume TL was loaded externally }
    ProcessTL;
  end
  else
  begin
    Load;
    ProcessTL;
    if IsEmpty or WantDefault then
    begin
      LoadDefault;
      ProcessTL;
    end;
  end;
end;

procedure TWorkspaceList.FillCombo(Combo: TStrings);
var
  i: Integer;
  s: string;
begin
  Combo.Clear;
  Assert(NL.Count = VL.Count, 'NL.Count <> VL.Count');
  Assert(NL.Count > 0, 'NL.Count must be > 0');
  for i := 0 to NL.Count - 1 do
  begin
    if WantNames and (NL[i] <> '') then
       s := NL[i]
    else
      s := VL[i];
    Combo.Add(s)
  end;
end;

procedure TWorkspaceList.ProcessTL;
var
  i: Integer;
  s: string;
begin
  for i := 0 to TL.Count-1 do
  begin
    s := Trim(TL[i]);
    if s = 'stop' then
    begin
      FStop := true;
      break;
    end;

    if (s = '') or TUtils.StartsWith(s, '//') or TUtils.StartsWith(s, '#') then
      Continue;

    ParseLine(s);

    case FV.GetScheme of
      usHttp: AddEntry;
      usFile: if FileExists(FV.Value) then AddEntry;
      usApp:
    end;
  end;
end;

function TWorkspaceList.GetAppDataRoot: string;
begin
  result := TAppUtils.GetSpecialFolderPath(CSIDL_LOCAL_APPDATA, True);
  result := IncludeTrailingPathDelimiter(result) + 'RiggVar\FR\';
  ForceDirectories(result);
end;

function TWorkspaceList.GetIsEmpty: Boolean;
begin
  result := VL.Count = 0;
end;

function TWorkspaceList.GetWantDefault: Boolean;
begin
  result := not FStop;
end;

function TWorkspaceList.GetWorkspaceListFileName: string;
var
  dn: string;
begin
  result := '';
  dn := GetAppDataRoot;
  if dn <> '' then
    result := IncludeTrailingPathDelimiter(dn) + StrWorkspaceList;
end;

function TWorkspaceList.GetName(i: Integer): string;
begin
  result := '';
  if (i >= 0) and (i < NL.Count) then
    result := NL[i];
end;

function TWorkspaceList.GetUrl(i: Integer): string;
begin
  result := '';
  if (i >= 0) and (i < VL.Count) then
    result := VL[i];
end;

function TWorkspaceList.IsWritable(i: Integer): Boolean;
begin
  result := TUtils.StartsWith(GetName(i), '*');
end;

procedure TWorkspaceList.AddEntry;
begin
  NL.Add(FK);
  VL.Add(FV.Value);
end;

procedure TWorkspaceList.ParseLine(const s: string);
var
  i: Integer;
begin
  i := Pos('=', s);
  if i > 0 then
  begin
    FK := Trim(Copy(s, 1, i-1));
    FV.Value := Trim(Copy(s, i+1, Length(s)));
  end
  else
  begin
    FK := '';
    FV.Value := Trim(s);
  end;
end;

procedure TWorkspaceList.SetWantNames(const Value: Boolean);
begin
  FWantNames := Value;
end;

//procedure TWorkspaceList.DecomposeUrl(s: string);
//begin
//  ParseLine(s);
//  AddEntry;
//end;

function TWorkspaceList.GetText: string;
var
  i: Integer;
  n, v, s: string;
begin
  TL.Clear;
  for i := 0 to NL.Count-1 do
  begin
    n := NL[i];
    v := VL[i];
    if n <> '' then
      s := n + '=' + v
    else
      s := v;
    TL.Add(s);
  end;
  result := TL.Text;
end;

procedure TWorkspaceList.SetText(Value: string);
begin
  TL.Text := Value;
end;

procedure TWorkspaceList.Load;
var
  fn: string;
begin
  try
    fn  := GetWorkspaceListFileName;
    if (fn <> '') and FileExists(fn) then
      TL.LoadFromFile(fn)
    else
      TL.Clear;
  except
    on e: Exception do
    begin
      FError := e.Message;
      TL.Clear;
    end;
  end;
end;

procedure TWorkspaceList.Save;
var
  fn: string;
  i: Integer;
  s: string;
begin
  fn := GetWorkspaceListFileName;
  for i := TL.Count-1 downto 0 do
  begin
    s := TL[i];
    if TUtils.StartsWith(s, 'Example: ')  then
      TL.Delete(i);
    if Pos('Local Workspace', s) > 0  then
      TL.Delete(i);
  end;
  TL.SaveToFile(fn);
end;

end.
