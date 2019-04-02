unit RiggVar.Util.UnicodeScanner;

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
  System.Types,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.ComCtrls,
  Vcl.Grids;

type
  TUnicodeScanner = class
  private
    EL: TStringList;
    SL: TStringList;
    FSourceDir: string;
    dn: string;
    procedure SetSourceDir(const Value: string);
  public
    LV: TListView;
    constructor Create;
    destructor Destroy; override;

    function IsDetailFile(ext: string): Boolean;
    function RemovePreamble(s: string): string;

    procedure ListNames(lb: TStrings);
    procedure ListLineCount(lb: TStrings);
    procedure FillGrid(lb: TStrings);
    procedure ShowFile(Name: string; Memo: TStrings);
    procedure ShowDetail(Name: string; Memo: TStrings; Encoding: TEncoding);
    procedure SaveFile(Name: string; Memo: TStrings);

    procedure ListAll;
    procedure ListFR;
    procedure ListGrouped;

    property SourceDir: string read FSourceDir write SetSourceDir;
  end;

implementation

uses
  RiggVar.App.Main;

{ TUnicodeScanner }

constructor TUnicodeScanner.Create;
begin
  EL := TStringList.Create;
  EL.Add('.txt');
  EL.Add('.fr62');
  EL.Add('.fr62x');
  EL.Add('.ini');
  EL.Add('.xml');
  SL := TStringList.Create;
  //SourceDir := 'C:\Users\Gustav\Documents\RiggVar Workspace\';
  SourceDir := Main.FolderInfo.WorkspacePath;
end;

destructor TUnicodeScanner.Destroy;
begin
  EL.Free;
  SL.Free;
  inherited;
end;

procedure TUnicodeScanner.ListNames(lb: TStrings);
var
  fn: string;
begin
  for fn in TDirectory.GetFiles(dn) do
  begin
    lb.Add(ExtractFileName(fn));
  end;
end;

procedure TUnicodeScanner.ListLineCount(lb: TStrings);
var
  fn: string;
begin
  for fn in TDirectory.GetFiles(dn) do
  begin
    SL.LoadFromFile(fn);
    lb.Add(IntToStr(SL.Count));
  end;
end;

procedure TUnicodeScanner.FillGrid(lb: TStrings);
var
  fn: string;
begin
  for fn in TDirectory.GetFiles(dn, TSearchOption.soTopDirectoryOnly,
      function(const Path: string; const SearchRec: TSearchRec): Boolean
      var
        b1: Boolean;
        //b2: Boolean;
        name, ext: string;
      begin
        name := SearchRec.Name;
        ext := ExtractFileExt(name);
        b1 := (Pos('FR_', name) = 1) and (ext = '.txt');
        //b2 := (ext = '.fr62') or (ext = '.fr62x');
        result := b1; //b1 or b2;
      end
      ) do
  begin
    SL.LoadFromFile(fn);
    lb.AddObject(ExtractFileName(fn), TObject(SL.Count));
  end;
end;

procedure TUnicodeScanner.ShowFile(Name: string; Memo: TStrings);
var
  fn: string;
begin
  fn := TPath.Combine(dn, Name);
  try
    SL.LoadFromFile(fn);
    if SL.Count = 0 then
    begin
      SL.LoadFromFile(fn, TEncoding.Default); //TEncoding.Ascii
      //if SL.Count > 0 then
      //  SL[0] := RemovePreamble(SL[0]);
    end;
    Memo.Text := SL.Text;
  except on e: Exception do
    Memo.Text := e.Message;
  end;
end;

procedure TUnicodeScanner.ShowDetail(Name: string; Memo: TStrings; Encoding: TEncoding);
var
  fn: string;
begin
  fn := TPath.Combine(dn, Name);
  try
    SL.LoadFromFile(fn, Encoding);
    Memo.Text := SL.Text;
  except on e: Exception do
    Memo.Text := e.Message;
  end;
end;

procedure TUnicodeScanner.SaveFile(Name: string; Memo: TStrings);
var
  fn: string;
begin
  fn := TPath.Combine(dn, Name);
  try
    Memo.SaveToFile(fn, TEncoding.UTF8); //ok, MemoStrings.SaveToFile works
//    SL.Text := Memo.Text;
//    SL.SaveToFile(fn, TEncoding.UTF8);
  except on e: Exception do
    Memo.Text := e.Message;
  end;
end;

procedure TUnicodeScanner.SetSourceDir(const Value: string);
begin
  Assert(DirectoryExists(Value));
  FSourceDir := Value;
  dn := TPath.Combine(FSourceDir, 'DBEvent');
  Assert(dn = FSourceDir + 'DBEvent');
  Assert(DirectoryExists(dn));
end;

procedure TUnicodeScanner.ListAll;
var
  s: string;
begin
  LV.GroupView := false;
  LV.Groups.Clear;
  LV.Items.Clear;
  for s in TDirectory.GetFiles(dn) do
    LV.Items.Add.Caption := ExtractFileName(s);
end;

procedure TUnicodeScanner.ListFR;
var
  s: string;
begin
  LV.GroupView := false;
  LV.Groups.Clear;
  LV.Items.Clear;
  for s in TDirectory.GetFiles(SourceDir, TSearchOption.soAllDirectories,
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
      result := Pos('FR_', SearchRec.Name) = 1;
    end) do
  LV.Items.Add.Caption := ExtractFileName(s);
end;

procedure TUnicodeScanner.ListGrouped;
begin
  LV.GroupView := True;
  LV.Groups.Clear;
  LV.Items.Clear;

  LV.Items.BeginUpdate;
  try
    TDirectory.GetFiles(dn, TSearchOption.soTopDirectoryOnly,
      function(const Path: string; const SearchRec: TSearchRec): Boolean
      var
        i: Integer;
        LGroup: TListGroup;
        LItem: TListItem;
        b1, b2: Boolean;
        name, ext: string;
        fn: string;
      begin
        result := false;
        name := SearchRec.Name;
        ext := ExtractFileExt(name);
        fn := TPath.Combine(Path, name);
        b1 := (Pos('FR_', name) = 1) and (ext = '.txt');
        b2 := (ext = '.fr62') or (ext = '.fr62x');
        if b1 or b2 then
        begin
          LGroup := nil;
          for i := 0 to LV.Groups.Count - 1 do
            if AnsiCompareText(LV.Groups[i].Header, ext) = 0 then
            begin
              LGroup := LV.Groups[i];
              break;
            end;

          if LGroup = nil then
          begin
            LGroup := LV.Groups.Add;
            LGroup.Header := ext;
          end;

          LItem := LV.Items.Add;
          LItem.Caption := name;
          LItem.GroupID := LGroup.Index;
          SL.LoadFromFile(fn);
          LItem.SubItems.Add(IntToStr(SL.Count));
        end;
      end
      );
  finally
    LV.Items.EndUpdate;
  end;
end;

function TUnicodeScanner.RemovePreamble(s: string): string;
var
  up: string;
begin
  result := s;
  up := StringOf(TEncoding.UTF8.GetPreamble);
  Assert(up = 'ï»¿');
  if Pos(up, s) = 1 then
  //if Pos('ï»¿', s) = 1 then //TEncoding.Default
  begin
    if Length(s) > Length(up) then
      s := Copy(s, Length(up)+1);
    result := s;
  end
  else if Pos('o;?', s) = 1 then //TEncoding.Ascii
  begin
    if Length(s) > 3 then
      s := Copy(s, 4);
    result := s;
  end;
end;

function TUnicodeScanner.IsDetailFile(ext: string): Boolean;
begin
  result := EL.IndexOf(ext) > -1;
end;

end.
