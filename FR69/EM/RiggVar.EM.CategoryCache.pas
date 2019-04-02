unit RiggVar.EM.CategoryCache;

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
  RiggVar.EM.Combo,
  RiggVar.EM.WorkspaceListBase,
  RiggVar.Col.CategoryCache;

type
  TCacheMode = (cmHtmlFragment, cmHtmlDoc, cmXml, cmTxt);
  TCategoryCache = class
  private
    StopWatch: TStopWatch;
    SL: TStrings;
    FEnabled: Boolean;
    FActive: Boolean;
    function GetCount: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    function DFInt(ff: TDataFormat): Integer;
    function GetCacheDir: string;
    function GetIsActivated: Boolean;
    procedure WrapFragmentIntoDoc(i: Integer);
    function GetExtension: string;
  protected
    procedure RestoreAll; virtual;
    procedure BackupAll; virtual;
    procedure SaveAll; virtual;
  public
    CacheMode: TCacheMode;
    WantWrapper: Boolean;
    Node: TCacheNode;
    ColBO: TCacheBO;
    UserName: string;
    WorkspaceName: string;
    WorkspaceUrl: string;
    CategoryName: string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Backup;
    procedure Save;
    procedure Restore;
    procedure Init(EventCombo: TEventCombo);
    procedure Update(i: Integer; df: TDataFormat);
    function Data(i: Integer): string;
    property Count: Integer read GetCount;
    property Active: Boolean read FActive write SetActive;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property IsActivated: Boolean read GetIsActivated;
    property CacheDir: string read GetCacheDir;
  end;

implementation

uses
  RiggVar.BO.Def,
  SHFolder,
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.Util.AppUtils;

constructor TCategoryCache.Create;
begin
  ColBO := TCacheBO.Create;
  Node := TCacheNode.Create(ColBO);
  SL := TStringList.Create;
  StopWatch := TStopWatch.Create;
  WantWrapper := True;
end;

destructor TCategoryCache.Destroy;
begin
  Node.Free;
  ColBO.Free;
  SL.Free;
  inherited;
end;

function TCategoryCache.GetCount: Integer;
begin
  result := Node.CacheRowCollection.Count;
end;

procedure TCategoryCache.Init(EventCombo: TEventCombo);
var
  i: Integer;
  cr: TCacheRowCollectionItem;
begin
  Clear;
  for i := 1 to EventCombo.Count do
  begin
    cr := Node.CacheRowCollection.Add;
    cr.Caption := EventCombo.GetCaption(i);
    cr.Url := EventCombo.GetDataUrl(i);
  end;
  Main.GuiManager.GuiInterface.HandleInform(CategoryGridChanged);
end;

procedure TCategoryCache.Clear;
begin
  Node.CacheRowCollection.Clear;
end;

function TCategoryCache.GetCacheDir: string;
begin
  result := TAppUtils.GetSpecialFolderPath(CSIDL_PERSONAL, True);
  result := IncludeTrailingPathDelimiter(result) + 'FR\Cache\';
  ForceDirectories(result);
end;

procedure TCategoryCache.Save;
begin
  if IsActivated then
    SaveAll;
end;

procedure TCategoryCache.Backup;
begin
  if IsActivated then
    BackupAll;
end;

procedure TCategoryCache.Restore;
begin
  RestoreAll;
end;

procedure TCategoryCache.BackupAll;
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
      if CacheMode = cmHTMLDoc then
        WrapFragmentIntoDoc(i);
      SL.SaveToFile(fn, TEncoding.UTF8);
    end;
  end;
end;

procedure TCategoryCache.SaveAll;
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
      if CacheMode = cmHTMLDoc then
        WrapFragmentIntoDoc(i);
      SL.SaveToFile(fn, TEncoding.UTF8);
    end;
  end;
end;

procedure TCategoryCache.WrapFragmentIntoDoc(i: Integer);
begin
  SL.Insert(0,'<html>');
  SL.Insert(1,'<head>');
  SL.Insert(2,'<title>FR01 Cache - ED' + IntToStr(i) + '</title>');
  SL.Insert(3,'</head>');
  SL.Insert(4,'<body>');
  SL.Insert(5, '');

  SL.Add('');
  SL.Add('</body>');
  SL.Add('</html>');
end;

function TCategoryCache.GetExtension: string;
begin
  case CacheMode of
    cmHtmlFragment: result := '.html';
    cmHtmlDoc: result := '.html';
    cmXml: result := '.xml';
    cmTxt: result := '.txt';
  end;
end;

procedure TCategoryCache.RestoreAll;
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
      fn := dn + 'ED' + IntToStr(i + 1) + '.html';
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

procedure TCategoryCache.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TCategoryCache.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TCategoryCache.Data(i: Integer): string;
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
    result := '<p>empty</p>';
end;

procedure TCategoryCache.Update(i: Integer; df: TDataFormat);
var
  cr: TCacheRowCollectionItem;
  millies: Cardinal;
  ed: string;
  en: string;
begin
  if (i > 0) and (i <= Node.CacheRowCollection.Count) then
  begin
    cr := Node.CacheRowCollection.Items[i - 1];
    if IsActivated then
    begin
      StopWatch.Start;
      case CacheMode of
        cmHtmlFragment: ed := BO.ToHtml;
        cmHtmlDoc: ed := BO.ToHtml;
        cmXml: ed := BO.ToXml;
        cmTxt: ed := BO.ToTxt;
      end;
      en := BO.EventProps.EventName;
      StopWatch.Stop;
      millies := StopWatch.ElapsedMilliseconds;
      cr.StoreData(ed, en, millies);
    end;
    cr.DataFormat := DFInt(df);
  end;
  Main.GuiManager.GuiInterface.HandleInform(CategoryGridChanged);
end;

function TCategoryCache.DFInt(ff: TDataFormat): Integer;
begin
  case ff of
    ffTxt:
      result := 1;
    ffXml:
      result := 2;
    ffHtml:
      result := 3;
  else
    result := 0;
  end;
end;

function TCategoryCache.GetIsActivated: Boolean;
begin
  //result := (Main.PixelData.B1 and Main.PixelData.B2);
  result := Main.HaveAppPermission;
end;

end.
