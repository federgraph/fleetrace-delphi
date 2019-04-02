unit RiggVar.DAL.Intf;

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
  System.Classes;

type
  IDBEvent = interface
  ['{A36B9505-C1B9-4B57-8E40-1796908C252F}']
    function Load(KatID: Integer; EventName: string): string;
    procedure Save(KatID: Integer; EventName: string; Data: string);
    procedure Delete(KatID: Integer; EventName: string);
    function GetEventNames(KatID: Integer): string;
    procedure Close;
  end;

  TDBEventMock = class(TInterfacedObject, IDBEvent)
  public
    { IDBEvent }
    function Load(KatID: Integer; EventName: string): string;
    procedure Save(KatID: Integer; EventName: string; Data: string);
    procedure Delete(KatID: Integer; EventName: string);
    function GetEventNames(KatID: Integer): string;
    procedure Close;
  end;

  TDBEvent = class(TInterfacedObject, IDBEvent)
  private
    SL: TStrings;
    function GetPrefix(KatID: Integer): string;
    function GetSuffix(KatID: Integer): string;
    function GetFileName(KatID: Integer; EventName: string): string;
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
  RiggVar.DAL.Redirector,
  RiggVar.BO.TemplateIDs;

{ TDBEvent }

constructor TDBEvent.Create;
begin
  inherited Create;
  SL := TDBStringList.Create;
end;

destructor TDBEvent.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TDBEvent.GetPrefix(KatID: Integer): string;
begin
  case KatID of
    TypFREvent: result := 'FR_';
    else
      result := IntToStr(KatID) + '_';
  end;
end;

function TDBEvent.GetSuffix(KatID: Integer): string;
begin
  case KatID of
    TypFREvent: result := '_FRData';
    else
      result := '';
  end;
end;

procedure TDBEvent.Delete(KatID: Integer; EventName: string);
var
  fn: string;
begin
  fn := GetFileName(KatID, EventName);
  if DBFileExists(fn) then
  begin
    DBDeleteFile(fn);
  end;
end;

function TDBEvent.GetDir: string;
begin
  result := Main.FolderInfo.DataPath;
end;

function TDBEvent.GetEventNames(KatID: Integer): string;
begin
  try
    if Main.UseDB then
      result := GetEventNamesDB(KatID) //DataBase
    else
      result := GetEventNamesFS(KatID); //FileSystem
  except
    result := '';
  end;
end;

function TDBEvent.GetEventNamesDB(KatID: Integer): string;
begin
  result := Main.StoreAdapter.GetDBWorkspace.DBGetEventNames('.txt');
end;

function TDBEvent.GetEventNamesFS(KatID: Integer): string;
var
  Found: integer;
  SearchRec: TSearchRec;
  Attr: integer;
  s: string;
  prefix: string;
  suffix: string;
  l1: Integer;
  l2: Integer;
begin
  result := '';
  SL.Clear;
  prefix := GetPrefix(KatID);
  suffix := GetSuffix(KatID);
  l1 := Length(prefix);
  l2 := Length(suffix);
  Attr := faAnyFile;
  s := Dir + prefix + '*' + suffix + '.txt';
  Found := System.SysUtils.FindFirst(s, Attr, SearchRec);
  while Found = 0 do
  begin
    s := ChangefileExt(SearchRec.Name, '');
    s := Copy(s, l1 + 1, Length(s) - l1 - l2);
    SL.Add(s);
    Found := System.SysUtils.FindNext(SearchRec);
  end;
  System.SysUtils.FindClose(SearchRec);
  result := SL.Text;
end;

function TDBEvent.GetFileName(KatID: Integer; EventName: string): string;
begin
  if Main.UseDB then
    result := Dir + EventName + '.txt'
  else
    result := Dir + GetPrefix(KatID) + EventName + GetSuffix(KatID) + '.txt';
end;

function TDBEvent.Load(KatID: Integer; EventName: string): string;
var
  fn: string;
begin
  result := '';
  fn := GetFileName(KatID, EventName);
  if DBFileExists(fn) then
  begin
    SL.LoadFromFile(fn);
    result := SL.Text;
  end;
end;

procedure TDBEvent.Save(KatID: Integer; EventName, Data: string);
var
  fn: string;
begin
  fn := GetFileName(KatID, EventName);
  SL.Text := Data;
  SL.SaveToFile(fn);
end;

procedure TDBEvent.Close;
begin
  //
end;

{ TDBEventMock }

procedure TDBEventMock.Close;
begin

end;

procedure TDBEventMock.Delete(KatID: Integer; EventName: string);
begin

end;

function TDBEventMock.GetEventNames(KatID: Integer): string;
begin
  result := '';
end;

function TDBEventMock.Load(KatID: Integer; EventName: string): string;
begin
  result := '';
end;

procedure TDBEventMock.Save(KatID: Integer; EventName, Data: string);
begin

end;

end.
