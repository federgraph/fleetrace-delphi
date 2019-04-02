unit RiggVar.DAL.Redirector;

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
  System.IniFiles;

type
  TDBStringList = class(TStringList)
  private
    function RemovePreamble(const s: string): string;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;

  {
    hack needed, because FFileName and LoadValues are private in TMemIniFile
    FileName is set to '' to prevent private inherited LoadValues from beeing effective
    use DBFileName instead of FileName
  }
  TDBIniFile = class(TMemIniFile)
  private
    FDBFileName: string;
    procedure DBLoadValues;
  public
    constructor Create(const aFileName: string);
    procedure UpdateFile; override;
    property DBFileName: string read FDBFileName;
  end;

//  IWSDB = interface
//    function DBFileExists(const fn: string): Boolean;
//    function DBDirectoryExists(const dn: string): Boolean;
//    function DBCreateDir(const dn: string): Boolean;
//    function DBDeleteFile(const fn: string): Boolean;
//    procedure DBLoadFromFile(const fn: string; SL: TStrings);
//    procedure DBSaveToFile(const fn: string; SL: TStrings);
//  end;

function DBFileExists(const fn: string): Boolean;
function DBDirectoryExists(const dn: string): Boolean;
function DBCreateDir(const dn: string): Boolean;
function DBDeleteFile(const fn: string): Boolean;

implementation

uses
  RiggVar.App.Main,
  RiggVar.DAL.WorkspaceIntf;

function DBWorkspace: IDBWorkspace;
begin
  result := Main.StoreAdapter.GetDBWorkspace;
end;

function DBDirectoryExists(const dn: string): Boolean;
begin
  if Main.BlankOut then
    result := false
  else if Main.UseDB then
    result := true
  else
    result := DirectoryExists(dn);
end;

function DBFileExists(const fn: string): Boolean;
begin
  if Main.BlankOut then
    result := false
  else if Main.UseDB then
    result := DBWorkspace.DBFileExists(fn)
  else
    result := FileExists(fn);
end;

function DBCreateDir(const dn: string): Boolean;
begin
  if Main.BlankOut then
    result := false
  else if Main.UseDB then
    result := true
  else
    result := System.SysUtils.CreateDir(dn);
end;

function DBDeleteFile(const fn: string): Boolean;
begin
  if Main.BlankOut then
    result := false
  else if Main.UseDB then
    result := DBWorkspace.DBDeleteFile(fn)
  else
    result := System.SysUtils.DeleteFile(fn);
end;

procedure DBSaveToFile(const fn: string; SL: TStrings);
begin
  if Main.BlankOut then
  begin
    //do nothing;
  end
  else if Main.UseDB then
  begin
    DBWorkspace.DBSaveToFile(fn, SL);
  end
  else
  begin
    SL.SaveToFile(fn, TEncoding.UTF8);
  end;
end;

procedure DBLoadFromFile(const fn: string; SL: TStrings);
begin
  if Main.BlankOut then
  begin
    //do nothing;
  end
  else if Main.UseDB then
  begin
    DBWorkspace.DBLoadFromFile(fn, SL);
  end
  else
  begin
    SL.LoadFromFile(fn);
  end;
end;

{ TDBStringList }

procedure TDBStringList.LoadFromFile(const FileName: string);
begin
  if Main.BlankOut then
  begin
    //do nothing;
  end
  else if Main.UseDB then
  begin
    DBLoadFromFile(FileName, self);
  end
  else
  begin
    inherited;
    //handle one special case: Preamble is UTF-8, but File is not
    if Count = 0 then
    begin
      inherited LoadFromFile(FileName, TEncoding.Default);
      if Count > 0 then
      begin
        Strings[0] := RemovePreamble(Strings[0]);
      end;
    end;
  end;
end;

procedure TDBStringList.SaveToFile(const FileName: string);
begin
  if Main.BlankOut then
  begin
    //do nothing;
  end
  else if Main.UseDB then
  begin
    DBSaveToFile(FileName, self);
  end
  else
  begin
    if Main.IniImage.UseUnicode then
      inherited SaveToFile(FileName, TEncoding.UTF8)
    else
      inherited SaveToFile(FileName);
  end;
end;

function TDBStringList.RemovePreamble(const s: string): string;
var
  up: string;
begin
  result := s;
  up := StringOf(TEncoding.UTF8.GetPreamble);
  if Pos(up, s) = 1 then
  begin
    if Length(s) > 3 then
      result := Copy(s, 4);
  end
  else if Pos('o;?', s) = 1 then
  begin
    if Length(s) > 3 then
      result := Copy(s, 4);
  end;
end;

{ TDBIniFile }

constructor TDBIniFile.Create(const aFileName: string);
begin
  inherited Create(''); //do not load from file
  //self.FileName := aFileName; //FFileName is private and readonly
  FDBFileName := aFileName;
  self.DBLoadValues;
end;

procedure TDBIniFile.DBLoadValues;
var
  List: TStringList;
begin
  //this is a replacement for private LoadValues method of TMemIniFile
  if (FileName <> '') and FileExists(FileName) then
  begin
    List := TStringList.Create;
    try
      DBLoadFromFile(DBFileName, List);
      SetStrings(List);
    finally
      List.Free;
    end;
  end
  else
    Clear;
end;

procedure TDBIniFile.UpdateFile;
var
  List: TStringList;
begin
  //do not call inherited!
  //this is a complete replacement for inherited UpdateFile,
  //which uses private FFileName, which would be ''

  if Main.BlankOut then
  begin
    //do nothing;
    Exit;
  end;

  List := TStringList.Create;
  try
    GetStrings(List);
    DBSaveToFile(DBFileName, List);
  finally
    List.Free;
  end;
end;

end.
