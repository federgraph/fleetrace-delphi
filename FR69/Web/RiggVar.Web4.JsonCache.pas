unit RiggVar.Web4.JsonCache;

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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  RiggVar.Col.JsonCache;

type
  TJsonCache = class
  private
    StopWatch: TStopWatch;
    SL: TStrings;
    function GetCount: Integer;
    function GetCacheDir: string;
    function GetExtension: string;
  protected
    procedure RestoreAll; virtual;
    procedure BackupAll; virtual;
    procedure SaveAll; virtual;
  public
    Node: TCacheNode;
    ColBO: TCacheBO;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Backup;
    procedure Save;
    procedure Restore;
    procedure Init(c: Integer);
    procedure Update(i: Integer);
    procedure Push(i: Integer; const value: string);
    function Data(i: Integer): string;
    property Count: Integer read GetCount;
    property CacheDir: string read GetCacheDir;
  end;

implementation

uses
  Winapi.SHFolder,
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Def,
  RiggVar.Util.AppUtils;

constructor TJsonCache.Create;
begin
  ColBO := TCacheBO.Create;
  Node := TCacheNode.Create(ColBO);
  SL := TStringList.Create;
  StopWatch := TStopWatch.Create;
end;

destructor TJsonCache.Destroy;
begin
  Node.Free;
  ColBO.Free;
  SL.Free;
  inherited;
end;

function TJsonCache.GetCount: Integer;
begin
  result := Node.CacheRowCollection.Count;
end;

procedure TJsonCache.Init(c: Integer);
var
  i: Integer;
  cr: TCacheRowCollectionItem;
begin
  Clear;
  for i := 1 to c do
  begin
    cr := Node.CacheRowCollection.Add;
    cr.Caption := 'ED' + IntToStr(i);
    cr.Url := 'ED' + IntToStr(i);
    if i = 2 then
      cr.EventName := 'EventDataJson';
    if i = 3 then
      cr.EventName := 'RaceDataJson';
  end;
  //Main.GuiManager.GuiInterface.HandleInform(JsonGridChanged);
end;

procedure TJsonCache.Clear;
begin
  Node.CacheRowCollection.Clear;
end;

function TJsonCache.GetCacheDir: string;
begin
  result := TAppUtils.GetSpecialFolderPath(CSIDL_PERSONAL, True);
  result := IncludeTrailingPathDelimiter(result) + 'FR\JsonCache\';
  ForceDirectories(result);
end;

procedure TJsonCache.Save;
begin
  SaveAll;
end;

procedure TJsonCache.Backup;
begin
  BackupAll;
end;

procedure TJsonCache.Restore;
begin
  RestoreAll;
end;

procedure TJsonCache.BackupAll;
var
  dn: string;
  fn: string;
  i: Integer;
  ed: string;
  cr: TCacheRowCollectionItem;
begin
  dn := GetCacheDir;
  if DirectoryExists(dn) then
  begin
    for i := 0 to Count - 1 do
    begin
      fn := dn + 'ED' + IntToStr(i + 1) + GetExtension;
      cr := Node.CacheRowCollection.Items[i];
      ed := cr.Data;
      SL.Text := ed;
      SL.SaveToFile(fn, TEncoding.UTF8);
    end;
  end;
end;

procedure TJsonCache.SaveAll;
var
  en: string;
  dn: string;
  fn: string;
  i: Integer;
  ed: string;
  cr: TCacheRowCollectionItem;
begin
  dn := GetCacheDir;
  if DirectoryExists(dn) then
  begin
    for i := 0 to Count - 1 do
    begin
      cr := Node.CacheRowCollection.Items[i];
      en := cr.EventName;
      en := StringReplace(en, ' ', '-', [rfReplaceAll, rfIgnoreCase]);
      fn := dn + en + GetExtension;
      ed := cr.Data;
      SL.Text := ed;
      SL.SaveToFile(fn, TEncoding.UTF8);
    end;
  end;
end;

function TJsonCache.GetExtension: string;
begin
  result := 'json';
end;

procedure TJsonCache.RestoreAll;
var
  dn: string;
  fn: string;
  i: Integer;
  ed: string;
  cr: TCacheRowCollectionItem;
begin
  dn := GetCacheDir;
  if DirectoryExists(dn) then
  begin
    for i := 0 to Count - 1 do
    begin
      fn := dn + 'ED' + IntToStr(i + 1) + GetExtension;
      cr := Node.CacheRowCollection.Items[i];
      if FileExists(fn) then
      begin
        SL.LoadFromFile(fn);
        ed := SL.Text;
        if (ed <> '') then
          cr.Data := ed;
      end;
    end;
  end;
end;

function TJsonCache.Data(i: Integer): string;
var
  cr: TCacheRowCollectionItem;
begin
  if (i > 0) and (i <= Node.CacheRowCollection.Count) then
  begin
    cr := Node.CacheRowCollection.Items[i - 1];
    result := cr.Data;
    cr.Hits := cr.Hits + 1;
  end
  else
    result := '{}';
end;

procedure TJsonCache.Update(i: Integer);
var
  cr: TCacheRowCollectionItem;
  millies: Cardinal;
  ed: string;
  en: string;
begin
  if (i > 0) and (i <= Node.CacheRowCollection.Count) then
  begin
    cr := Node.CacheRowCollection.Items[i - 1];
    StopWatch.Start;
    ed := BO.ToJson;
    en := BO.EventProps.EventName;
    StopWatch.Stop;
    millies := StopWatch.ElapsedMilliseconds;
    cr.StoreData(ed, en, millies);
    cr.DataFormat := 0;
  end;
  Main.GuiManager.GuiInterface.HandleInform(JsonGridChanged);
end;

procedure TJsonCache.Push(i: Integer; const value: string);
var
  cr: TCacheRowCollectionItem;
  millies: Cardinal;
  ed: string;
  en: string;
begin
  if (i > 0) and (i <= Node.CacheRowCollection.Count) then
  begin
    cr := Node.CacheRowCollection.Items[i - 1];
    StopWatch.Start;
    ed := value;
    //en := BO.EventProps.EventName;
    StopWatch.Stop;
    millies := StopWatch.ElapsedMilliseconds;
    cr.StoreData(ed, en, millies);
    cr.DataFormat := 0;
  end;
  Main.GuiManager.GuiInterface.HandleInform(JsonGridChanged);
end;

end.
