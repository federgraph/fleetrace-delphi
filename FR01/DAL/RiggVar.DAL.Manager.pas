unit RiggVar.DAL.Manager;

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
  RiggVar.App.Config,
  RiggVar.DAL.Intf;

type
  TDocManager = class
  private
    SLDocsAvail: TStringList;
    FEventName: string;
    FEventType: Integer;
    FDBInterface: string;
    function GetEventCount: Integer;
  public
    DB: IDBEvent;
    SaveEnabled: Boolean;
    SaveAsEnabled: Boolean;
    DeleteEnabled: Boolean;

    constructor Create(DBTypeName: string);
    destructor Destroy; override;

    function EditDBEvent: Boolean;
    procedure InitDBInterface(DBTypeName: string);

    function DocDownload: string;
    function DocDownloadByName(en: string): string;
    procedure DocSave;
    procedure DocSaveAs;
    procedure DocDelete;
    procedure RawDelete(en: string);

    procedure FillEventNameList(SL: TStrings);
    function DownloadByName(en: string): string;

    property EventCount: Integer read GetEventCount;
    property EventName: string read FEventName write FEventName;
    property EventType: Integer read FEventType write FEventType;
    property DBInterface: string read FDBInterface;
  end;

implementation

uses
  RiggVar.App.Main;

{ TDocManager }

constructor TDocManager.Create(DBTypeName: string);
begin
  inherited Create;
  SLDocsAvail := TStringList.Create;
  EventName := C_DefaultEventName;
  EventType := C_DefaultEventType;
  InitDBInterface(DBTypeName);
end;

procedure TDocManager.InitDBInterface(DBTypeName: string);
begin
  SaveEnabled := false;
  SaveAsEnabled := false;
  DeleteEnabled := false;
  DB := nil;
  Main.StoreAdapter.InitDBInterface(self, DBTypeName);
  if Assigned(DB) then
  begin
    FDBInterface := DBTypeName
  end
  else
  begin
    DB := TDBEventMock.Create;
    FDBInterface := '-';
    SaveEnabled := true;
    SaveAsEnabled := true;
    DeleteEnabled := true;
  end;
  Main.IniImage.DBInterface := FDBInterface;
end;

function TDocManager.EditDBEvent: Boolean;
begin
  result := false;
  if Main.ChooseDB then
  begin
    if FDBInterface <> Main.IniImage.DBInterface then
    begin
      InitDBInterface(Main.IniImage.DBInterface);
      result := true;
    end;
  end;
end;

destructor TDocManager.Destroy;
begin
  DB.Close;
  DB := nil;
  SLDocsAvail.Free;
  inherited;
end;

procedure TDocManager.FillEventNameList(SL: TStrings);
begin
  SL.Text := DB.GetEventNames(FEventType);
end;

function TDocManager.GetEventCount: Integer;
var
  t: TStringList;
begin
  t := TStringList.Create;
  t.Text := DB.GetEventNames(FEventType);
  result := t.Count;
  t.Free;
end;

function TDocManager.DocDownload: string;
var
  en: string;
begin
  SLDocsAvail.Text := DB.GetEventNames(FEventType);
  en := Main.ChooseDocAvail(SLDocsAvail);
  result := DocDownloadByName(en);
end;

function TDocManager.DownloadByName(en: string): string;
begin
  //Call this only if EventName should NOT! be set to en
  result := '';
  if en <> '' then
  begin
    //EventName := en; //not set!
    result := DB.Load(FEventType, en)
  end;
end;

function TDocManager.DocDownloadByName(en: string): string;
begin
  //Call this only if UPDATE of EventName is intended
  result := '';
  if en <> '' then
  begin
    EventName := en;
    result := DB.Load(FEventType, en)
    //BO.LoadNew(); //this is now done by caller
    // + further care must be taken (recreating views) by caller
  end;
end;

procedure TDocManager.DocDelete;
var
  s: string;
begin
  SLDocsAvail.Text := DB.GetEventNames(FEventType);
  { Get Name of Document to be deleted }
  s := Main.ChooseDocAvail(SLDocsAvail);
  if s <> '' then
    DB.Delete(FEventType, s);
end;

procedure TDocManager.RawDelete(en: string);
begin
  if en <> '' then
    DB.Delete(FEventType, en);
end;

procedure TDocManager.DocSave;
begin
  DB.Save(FEventType, EventName, Main.Save(DBInterface = 'XML'));
end;

procedure TDocManager.DocSaveAs;
var
  s: string;
begin
  s := Main.ChooseNewEventName;
  if s <> '' then
  begin
    EventName := s;
    DocSave;
  end;
end;

end.
