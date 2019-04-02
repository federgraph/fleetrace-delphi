unit RiggVar.DAL.XML;

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
  RiggVar.EM.TransformerMSXML,
  RiggVar.DAL.Redirector,
  RiggVar.BO.ResourceManager;

type
  TDBEventXML = class(TInterfacedObject, IDBEvent)
  private
    FDir: string;
    FExt: string;
    SL: TStringList;
    Transformer: TXslTransformer;
    XSL: TStrings;
    TXT: TStrings;
    function GetFileName(KatID: Integer; EventName: string): string;
    function ExtractScopeInfo(const EventName: string): string;
    procedure InitXSL;
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

{ TDBEventXML }

constructor TDBEventXML.Create;
begin
  inherited Create;
  FDir := Main.FolderInfo.DataPath;
  //Fext := 'fr62';
  //FExt := LowerCase(ChangeFileExt(ExtractFileName(Application.ExeName), ''));
  FExt := TAppUtils.GetFileNameWithoutExtension;
  FExt := '.' + FExt + 'x'; //e.g '.fr62x'
  SL := TDBStringList.Create;
  XSL := TStringList.Create;
  TXT := TStringList.Create;
  Transformer := TXslTransformer.Create;
  InitXSL;
end;

destructor TDBEventXML.Destroy;
begin
  Transformer.Free;
  SL.Free;
  XSL.Free;
  TXT.Free;
  inherited Destroy;
end;

procedure TDBEventXML.Delete(KatID: Integer; EventName: string);
var
  fn: string;
begin
  fn := GetFileName(KatID, EventName);
  if DBFileExists(fn) then
  begin
    DBDeleteFile(fn);
  end;
end;

function TDBEventXML.GetDir: string;
begin
  if Main.UseDB then
    result := Main.FolderInfo.DataPath
  else
    result := FDir;
end;

function TDBEventXML.GetEventNames(KatID: Integer): string;
begin
  if Main.UseDB then
    result := GetEventNamesDB(KatID)
  else
    result := GetEventNamesFS(KatID);
end;

function TDBEventXML.GetEventNamesDB(KatID: Integer): string;
begin
  result := Main.StoreAdapter.GetDBWorkspace.DBGetEventNames(FExt);
end;

function TDBEventXML.GetEventNamesFS(KatID: Integer): string;
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

function TDBEventXML.GetFileName(KatID: Integer; EventName: string): string;
begin
  if (Main.UseDB = False) and not DirectoryExists(Dir) then
  begin
    FDir := Main.FolderInfo.DataPath;
  end;
  result := Dir + EventName + FExt;
end;

function TDBEventXML.Load(KatID: Integer; EventName: string): string;
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
    SL.LoadFromFile(fn);
    Transformer.TransformSL(SL, XSL, TXT);
    if Transformer.OK then
      result := TXT.Text
    else
      result := Transformer.Error;
  end;
end;

procedure TDBEventXML.Save(KatID: Integer; EventName, Data: string);
var
  fn: string;
begin
  fn := GetFileName(KatID, EventName);
  if DirectoryExists(Dir) then
  begin
    SL.Text := Data;
    Main.StoreAdapter.StringListSaveToFile(SL, fn);
  end;
end;

procedure TDBEventXML.Close;
begin
  //
end;

function TDBEventXML.ExtractScopeInfo(const EventName: string): string;
begin
  if not Main.UseDB then
  begin
    if ExtractFilePath(EventName) <> '' then
      FDir := IncludeTrailingPathDelimiter(ExtractFilePath(EventName));
  end;

  if ExtractFileExt(EventName) <> '' then
    FExt := ExtractFileExt(EventName);
  result := ChangeFileExt(ExtractFileName(EventName), '');
end;

procedure TDBEventXML.InitXSL;
begin
  XSL.Text := Main.ResourceManager.LoadText(FRXML20_xsl)
end;

end.
