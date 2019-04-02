unit RiggVar.DAL.WorkspaceSQL;

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
  Winapi.OleDB,
  System.SysUtils,
  System.Classes,
  System.Win.ComObj,
  System.WideStrings,
  Data.DB,
  Data.Win.ADODB,
  RiggVar.DAL.WorkspaceIntf;

type
  TdmSqlWorkspaceFiles = class(TInterfacedObject, IDBWorkspace)
  private
    DBConnection: TADOConnection;
    Query: TADOQuery;
    SL: TStrings;
    ql: TStrings;
    pl: TParameters;
    FWorkspaceID: Integer;
    function OpenDB: Boolean;
    procedure CloseDB;
    function UDL2Str(FileName: WideString): WideString;
    function ExecuteSQL: Integer;
    procedure ClearQuery;
    procedure AddFile(wf: TWorkspaceFile);
  protected
    procedure SetWorkspaceID(WorkspaceID: Integer); virtual;
    function GetWorkspaceID: Integer;
    function DBGetEventNames(ExtensionFilter: string): string;
    function DBFileExists(const fn: string): Boolean;
    function DBDirectoryExists(const dn: string): Boolean;
    procedure DBLoadFromFile(const fn: string; SL: TStrings);
    procedure DBSaveToFile(const fn: string; SL: TStrings);
    function DBDeleteWorkspace: Boolean;
    function DBDeleteFile(const fn: string): Boolean;
    function GetDataSet: TDataSet; virtual;
    procedure ExportFile(const WorkspaceDir: string); virtual;
    procedure ExportFiles(const WorkspaceDir: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSqlWorkspaceFiles = class(TdmSQLWorkspaceFiles)
  private
    ds: TADOQuery;
    procedure InitQuery;
  protected
    procedure SetWorkspaceID(WorkspaceID: Integer); override;
    function GetDataSet: TDataSet; override;
    procedure ExportFile(const WorkspaceDir: string); override;
    procedure ExportFiles(const WorkspaceDir: string); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TdmSqlWorkspaceFiles }

constructor TdmSqlWorkspaceFiles.Create;
begin
  inherited Create;
  FWorkspaceID := 1;
  SL := TStringList.Create;

  DBConnection := TADOConnection.Create(nil);
  DBConnection.ConnectionString := 'FILE NAME=WorkspaceFiles.udl';
  DBConnection.DefaultDatabase := 'WorkspaceFiles';
  DBConnection.LoginPrompt := False;
  DBConnection.Provider := 'WorkspaceFiles.udl';

  Query := TADOQuery.Create(nil);
  Query.Connection := DBConnection;

  OpenDB;
  ql := Query.SQL;
  pl := Query.Parameters;
end;

destructor TdmSqlWorkspaceFiles.Destroy;
begin
  CloseDB;
  Query.Free;
  DBConnection.Free;
  SL.Free;
  inherited Destroy;
end;

function TdmSqlWorkspaceFiles.OpenDB: Boolean;
begin
  Result := False;
  if DBConnection.Connected then
    DBConnection.Connected := false;
  try
    { get connection string from udl file }
    DBConnection.ConnectionString := '';
    DBConnection.ConnectionString :=
      UDL2Str(ExtractFilePath(ParamStr(0)) + 'WorkspaceFiles.udl');
    DBConnection.Connected := true;
    if DBConnection.Connected then
      Result := True;
  except
  end;
end;

procedure TdmSqlWorkspaceFiles.CloseDB;
begin
  DBConnection.Close;
end;

function TdmSqlWorkspaceFiles.UDL2Str(FileName: WideString): WideString;
var
  DI: IDataInitialize;
  connectionString: PWideChar;
begin
  DI := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  OleCheck(DI.LoadStringFromStorage(PWideChar(FileName), connectionString));
  result := connectionString;
end;

procedure TdmSqlWorkspaceFiles.ClearQuery;
begin
  if Query.Active then
    Query.Close;
  ql.Clear;
  pl.Clear;
end;

function TdmSqlWorkspaceFiles.ExecuteSQL: Integer;
begin
  result := -1;
  try
    result := Query.ExecSQL;
  except
    on E: Exception do
    begin
      OutputDebugString(PChar(e.Message));
    end;
  end;
end;

procedure TdmSqlWorkspaceFiles.SetWorkspaceID(WorkspaceID: Integer);
begin
  FWorkspaceID := WorkspaceID;
end;

function TdmSqlWorkspaceFiles.GetWorkspaceID: Integer;
begin
  result := FWorkspaceID;
end;

function TdmSqlWorkspaceFiles.DBDeleteWorkspace: Boolean;
begin
  ClearQuery;
  ql.Add('delete from WorkspaceFiles');
  ql.Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
  ExecuteSQL;
  result := Query.RowsAffected > 0;
end;

function TdmSqlWorkspaceFiles.DBDeleteFile(const fn: string): Boolean;
begin
  ClearQuery;
  ql.Add('delete from WorkspaceFiles');
  ql.Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
  ql.Add(Format('and ItemKey = ''%s''', [fn]));
  ExecuteSQL;
  result := Query.RowsAffected > 0;
end;

function TdmSqlWorkspaceFiles.DBDirectoryExists(const dn: string): Boolean;
begin
  result := true;
end;

function TdmSqlWorkspaceFiles.DBFileExists(const fn: string): Boolean;
var
  c: Integer;
begin
  result := false;
  ClearQuery;
  ql.Add('select count(*) from WorkspaceFiles');
  ql.Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
  ql.Add(Format('and ItemKey = ''%s''', [fn]));
  try
    Query.Open;
    c := 0;
    if Query.Fields.Count > 0 then
      c := Query.Fields[0].AsInteger;
    if c > 0 then
      result := true;
  except
  end;
  if Query.Active then
    Query.Close;
end;

procedure TdmSqlWorkspaceFiles.DBLoadFromFile(const fn: string; SL: TStrings);
var
  s: string;
begin
  ClearQuery;
  ql.Add('select ItemValue from WorkspaceFiles');
  ql.Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
  ql.Add(Format('and ItemKey = ''%s''', [fn]));
  try
    Query.Open;
    if Query.Fields.Count > 0 then
    begin
      s := Query.Fields[0].AsString;
      SL.Text := s;
    end;
  except
  end;
  if Query.Active then
    Query.Close;
end;

procedure TdmSqlWorkspaceFiles.DBSaveToFile(const fn: string; SL: TStrings);
var
  p: TParameter;
  wf: TWorkspaceFile;
begin
  if DBFileExists(fn) then
  begin
    ClearQuery;

    ql.Add('update WorkspaceFiles');
    ql.Add('set ItemValue = :ItemValue');
    ql.Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
    ql.Add(Format('and ItemKey = ''%s''', [fn]));

    pl.Clear;

    p := pl.AddParameter;
    p.Name := 'ItemValue';
    p.Value := SL.Text;

    ExecuteSQL;
  end
  else
  begin
    wf.WorkspaceID := FWorkspaceID;
    wf.Key := fn;
    wf.Value := SL.Text;
    wf.Prepare;
    AddFile(wf);
  end;
end;

procedure TdmSqlWorkspaceFiles.AddFile(wf: TWorkspaceFile);
var
  p: TParameter;
begin
  if DBConnection.Connected then
  begin
    ClearQuery;

    ql.Add('insert into WorkspaceFiles');
    ql.Add('(WorkspaceID,ItemKey,ItemValue,ItemPath,ItemName,Ext)');
    ql.Add('Values');
    ql.Add('(:WorkspaceID,:ItemKey,:ItemValue,:ItemPath,:ItemName,:Ext)');

    pl.Clear;

    p := pl.AddParameter;
    p.DataType := ftInteger;
    p.Name := 'WorkspaceID';
    p.Value := wf.WorkspaceID;

    p := pl.AddParameter;
    p.Name := 'ItemKey';
    p.Value := wf.Key;

    p := pl.AddParameter;
    p.Name := 'ItemValue';
    p.Value := wf.Value;

    p := pl.AddParameter;
    p.Name := 'ItemPath';
    p.Value := wf.Path;

    p := pl.AddParameter;
    p.Name := 'ItemName';
    p.Value := wf.Name;

    p := pl.AddParameter;
    p.Name := 'Ext';
    p.Value := wf.Ext;

    ExecuteSQL;
  end;
end;

function TdmSqlWorkspaceFiles.DBGetEventNames(ExtensionFilter: string): string;
var
  SL: TStrings;
  n: string;
begin
  result := '';
  ClearQuery;
  ql.Add('select ItemName from WorkspaceFiles');
  ql.Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
  ql.Add('and ItemPath = ''\DBEvent\''');
  ql.Add(Format('and Ext = ''%s''', [ExtensionFilter]));
  ql.Add('order by ItemName');

  SL := TStringList.Create;
  try
    try
      Query.Open;
      Query.First;
      while not Query.Eof do
      begin
        n := Query.FieldByName('ItemName').AsString;
        SL.Add(n);
        Query.Next;
      end;
      result := SL.Text;
    except
    end;
    if Query.Active then
      Query.Close;
  finally
    SL.Free;
  end;
end;

function TdmSqlWorkspaceFiles.GetDataSet: TDataSet;
begin
  result := nil; //not implemented here
end;

procedure TdmSqlWorkspaceFiles.ExportFile(const WorkspaceDir: string);
begin
  //not implemented here
end;

procedure TdmSqlWorkspaceFiles.ExportFiles(const WorkspaceDir: string);
begin
  //not implemented here
end;

{ TSqlWorkspaceFiles }

constructor TSqlWorkspaceFiles.Create;
begin
  inherited Create;
  ds := TADOQuery.Create(nil);
  ds.Connection := DBConnection;
  InitQuery;
end;

destructor TSqlWorkspaceFiles.Destroy;
begin
  ds.Free;
  inherited Destroy;
end;

procedure TSqlWorkspaceFiles.InitQuery;
begin
  with ds.SQL do
  begin
    //do not select ItemValue here
    Clear;
    Add('select ID, WorkspaceID, ItemPath, ItemName, Ext, ItemKey');
    Add('from WorkspaceFiles');
    Add('where WorkspaceID = ' + IntToStr(FWorkspaceID));
  end;
end;

procedure TSqlWorkspaceFiles.SetWorkspaceID(WorkspaceID: Integer);
begin
  inherited;
  InitQuery;
end;

function TSqlWorkspaceFiles.GetDataSet: TDataSet;
begin
  if not ds.Active then
    ds.Active := True;
  result := ds;
end;

procedure TSqlWorkspaceFiles.ExportFiles(const WorkspaceDir: string);
var
  wf: TWorkspaceFile;
  fn: string;
  dn: string;
  SL: TStringList;
begin
  ClearQuery;
  ql.Add('select * from WorkspaceFiles');
  ql.Add('where WorkspaceID = ''' + IntToStr(FWorkspaceID) + '''');
  ql.Add('order by ItemKey');

  Query.Open;
  SL := TStringList.Create;
  try
    Query.First;
    while not Query.eof do
    begin
      wf.WorkspaceID := ds.FieldByName('WorkspaceID').AsInteger;
      wf.Key := ds.FieldByName('ItemKey').AsString;
      wf.Value := ds.FieldByName('ItemValue').AsString;
      wf.Prepare;
      fn := WorkspaceDir + wf.Key;
      dn := Workspacedir + wf.Path;
      if DirectoryExists(dn) then
      begin
        SL.Text := wf.Value;
        SL.SaveToFile(fn);
      end;
      Query.Next;
    end;
  finally
    SL.Free;
  end;
  Query.Close;
end;

procedure TSqlWorkspaceFiles.ExportFile(const WorkspaceDir: string);
var
  wf: TWorkspaceFile;
  dn: string;
  fn: string;
  SL: TStringList;
  ds: TDataSet;
begin
  SL := TStringList.Create;
  try
    ds := GetDataSet;
    wf.WorkspaceID := ds.FieldByName('WorkspaceID').AsInteger;
    wf.Key := ds.FieldByName('ItemKey').AsString;
    wf.Prepare;
    DBLoadFromFile(wf.Key, SL);
    if SL.Count > 0 then
    begin
      dn := WorkspaceDir + wf.Path;
      if DirectoryExists(dn) then
      begin
        fn := WorkspaceDir + wf.Key;
        SL.SaveToFile(fn);
      end;
    end;
  finally
    SL.Free;
  end;
end;

end.
