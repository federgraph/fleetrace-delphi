unit RiggVar.DAL.LNK;

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
  Windows, SysUtils, Classes,
  RiggVar.DAL.Intf,
  RiggVar.DAL.Redirector;

type
  TDBEventLNK = class(TInterfacedObject, IDBEvent)
  private
    FDir: string;
    FExt: string;
    SL: TStrings;
    function GetFileName(KatID: Integer; EventName: string): string;
    function ExtractScopeInfo(const EventName: string): string;
    function GetEventNamesDB(KatID: Integer): string;
    function GetEventNamesFS(KatID: Integer): string;
    function GetDir: string;
  public
    constructor Create;
    destructor Destroy; override;
    { IDBEvent }
    function Load(KatID: Integer; EventName: string): string;
    procedure Save(KatID: Integer; EventName: string; Data: string);
    procedure Delete(KatID: Integer; EventName: string);
    function GetEventNames(KatID: Integer): string;
    procedure Close;
    property Dir: string read GetDir;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Util.AppUtils;

{ TDBEventLNK }

constructor TDBEventLNK.Create;
begin
  inherited Create;
  FDir := Main.FolderInfo.DataPath;
  //Fext := 'fr62';
  //FExt := '.' + LowerCase(ChangeFileExt(ExtractFileName(Application.ExeName), ''));
  FExt := '.' + TAppUtils.GetFileNameWithoutExtension;
  SL := TDBStringList.Create;
end;

destructor TDBEventLNK.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

procedure TDBEventLNK.Delete(KatID: Integer; EventName: string);
var
  fn: string;
begin
  fn := GetFileName(KatID, EventName);
  if DBFileExists(fn) then
  begin
    DBDeleteFile(fn);
  end;
end;

function TDBEventLNK.GetDir: string;
begin
  if Main.UseDB then
    result := Main.FolderInfo.DataPath
  else
    result := FDir;
end;

function TDBEventLNK.GetEventNames(KatID: Integer): string;
begin
  if Main.UseDB then
    result := GetEventNamesDB(KatID)
  else
    result := GetEventNamesFS(KatID);
end;

function TDBEventLNK.GetEventNamesDB(KatID: Integer): string;
begin
  result := Main.StoreAdapter.GetDBWorkspace.DBGetEventNames(FExt);
end;

function TDBEventLNK.GetEventNamesFS(KatID: Integer): string;
var
  Found: integer;
  SearchRec: TSearchRec;
  Attr: integer;
  s: string;
begin
  result := '';
  SL.Clear;
  Attr := faAnyFile;
  s := FDir + '\*' + FExt;
  Found := SysUtils.FindFirst(s, Attr, SearchRec);
  while Found = 0 do
  begin
    s := ExtractFileName(SearchRec.Name);
    s := ChangeFileExt(s, '');
    SL.Add(s);
    Found := SysUtils.FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);
  result := SL.Text;
end;

function TDBEventLNK.GetFileName(KatID: Integer; EventName: string): string;
begin
  if (Main.UseDB = False) and not DirectoryExists(Dir) then
  begin
    FDir := Main.FolderInfo.DataPath;
  end;
  result := Dir + EventName + FExt;
end;

function TDBEventLNK.Load(KatID: Integer; EventName: string): string;
var
  fn: string;
  en: string;
begin
  result := '';
  //first call to Load must be made with full FileName, see MainForm
  //initializes FDir and FExt from EventName
  en := ExtractScopeInfo(EventName);
  fn := GetFileName(KatID, en);
  if DBFileExists(fn) then
  begin
    SL.LoadFromFile(fn, TEncoding.UTF8);
    result := SL.Text;
  end;
end;

procedure TDBEventLNK.Save(KatID: Integer; EventName, Data: string);
var
  fn: string;
begin
  fn := GetFileName(KatID, EventName);
  if DBDirectoryExists(FDir) then
  begin
    SL.Text := Data;
    Main.StoreAdapter.StringListSaveToFileUTF8(SL, fn);
  end;
end;

procedure TDBEventLNK.Close;
begin
  //
end;

function TDBEventLNK.ExtractScopeInfo(const EventName: string): string;
begin
  if ExtractFilePath(EventName) <> '' then
    FDir := IncludeTrailingPathDelimiter(ExtractFilePath(EventName));
  if ExtractFileExt(EventName) <> '' then
    FExt := ExtractFileExt(EventName);
  result := ChangeFileExt(ExtractFileName(EventName), '');
end;

end.
