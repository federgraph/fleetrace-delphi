unit RiggVar.App.Translation;

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
  TAppTranslator = class
  private
    TemplateFileName: string;
    FileName: string;
    SL: TStringList;
    VL: TStringList;
    TL: TStringList;

    MainFormIndex: Integer;
    CacheIndex: Integer;
    WorkspaceIndex: Integer;
    KeyingIndex: Integer;
    EventIndex: Integer;
    EventMenuIndex: Integer;
    FHasLocalText: Boolean;

    function GetSectionName(i: Integer): string;
    function GetAppDataRoot: string;
    function GetFileName: string;
    function GetAppDataFileName: string;
    function GetTemplateFileName: string;
    procedure LoadStrings(ini: TCustomIniFile);
    procedure SaveTemplateStrings(ini: TCustomIniFile);
    function GetTemplateText(key: string): string;
    procedure SetHasLocalText(const Value: Boolean);
    procedure SetText(key: string; value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure SaveTemplate;
    procedure GetTemplateStrings(Memo: TStrings);
    function GetText(key: string): string;
    procedure SetTemplateText(key: string; value: string);
    property HasLocalText: Boolean read FHasLocalText write SetHasLocalText;
  end;

function GetText(key: string): string;
procedure SetText(key: string; value: string);

const
  Section_MainForm = 'MainForm';

  MF_ClearBtn_Caption = 'MF_ClearBtn_Caption';
  MF_TestDataBtn_Caption = 'MF_TestDataBtn_Caption';
  MF_EventMenuBtn_Caption = 'MF_EventMenuBtn_Caption';
  MF_TimingBtn_Caption = 'MF_TimingBtn_Caption';
  MF_InfoBtn_Caption = 'MF_InfoBtn_Caption';
  MF_StyleBtn_Caption = 'MF_StyleBtn_Caption';
  MF_RowHeightBtn_Caption = 'MF_RowHeightBtn_Caption';
  MF_LanguageBtn_Caption = 'MF_LanguageBtn_Caption';
  MF_FeatureBtn_Caption = 'MF_FeatureBtn_Caption';
  MF_NorthBtn_Caption = 'MF_NorthBtn_Caption';
  MF_SouthBtn_Caption = 'MF_SouthBtn_Caption';
  MF_WestBtn_Caption = 'MF_WestBtn_Caption';
  MF_TestBtn_Caption = 'FM_TestBtn_Caption';

  MF_ClearBtn_Hint = 'MF_ClearBtn_Hint';
  MF_TestDataBtn_Hint = 'MF_TestDataBtn_Hint';
  MF_EventMenuBtn_Hint = 'MF_EventMenuBtn_Hint';
  MF_TimingBtn_Hint = 'MF_TimingBtn_Hint';
  MF_InfoBtn_Hint = 'MF_InfoBtn_Hint';
  MF_StyleBtn_Hint = 'MF_StyleBtn_Hint';
  MF_RowHeightBtn_Hint = 'MF_RowHeightBtn_Hint';
  MF_LanguageBtn_Hint = 'MF_LanguageBtn_Hint';
  MF_FeatureBtn_Hint = 'MF_FeatureBtn_Hint';
  MF_NorthBtn_Hint = 'MF_NorthBtn_Hint';
  MF_SouthBtn_Hint = 'MF_SouthBtn_Hint';
  MF_WestBtn_Hint = 'MF_WestBtn_Hint';
  MF_TestBtn_Hint = 'FM_TestBtn_Hint';

  Section_Cache = 'Cache';

  CA_HtmlFragmentBtn_Hint = 'CA_HtmlFragmentBtn_Hint';
  CA_HtmlDocBtn_Hint = 'CA_HtmlDocBtn_Hint';
  CA_XmlBtn_Hint = 'CA_XmlBtn_Hint';
  CA_TextBtn_Hint = 'CA_TextBtn_Hint';

  CA_BackupBtn_Hint = 'CA_BackupBtn_Hint';
  CA_SaveBtn_Hint = 'CA_SaveBtn_Hint';
  CA_BatchBtn_Hint = 'CA_BatchBtn_Hint';
  CA_ExportBtn_Hint = 'CA_ExportBtn_Hint';

  Section_Workspace = 'Workspace';

  WS_ShowDefaultBtn_Hint = 'WS_ShowDefaultBtn_Hint';
  WS_ShowComboBtn_Hint = 'WS_ShowComboBtn_Hint';
  WS_LoadBtn_Hint = 'WS_LoadBtn_Hint';
  WS_SaveBtn_Hint = 'WS_SaveBtn_Hint';
  WS_InitMemoBtn_Hint = 'WS_InitMemoBtn_Hint';
  WS_InitFileBtn_Hint = 'WS_InitFileBtn_Hint';

  Section_Keying = 'Keying';

  KT_RaceDownBtn_Hint = 'KT_RaceDownBtn_Hint';
  KT_RaceBtn_Hint = 'KT_RaceBtn_Hint';
  KT_RaceUpBtn_Hint = 'KT_RaceUpBtn_Hint';
  KT_ClearBtn_Hint = 'KT_ClearBtn_Hint';
  KT_SendBtn_Hint = 'KT_SendBtn_Hint';
  KT_AutoSendBtn_Hint = 'KT_AutoSendBtn_Hint';

  Section_Event = 'Event';

  EV_PointsBtn_Caption = 'EV_PointsBtn_Caption';
  EV_FinishBtn_Caption = 'EV_FinishBtn_Caption';
  EV_SelectRaceBtn_Caption = 'EV_SelectRaceBtn_Caption';

  EV_PointsBtn_Hint = 'EV_PointsBtn_Hint';
  EV_FinishBtn_Hint = 'EV_FinishBtn_Hint';
  EV_StrictBtn_Hint = 'EV_StrictBtn_Hint';
  EV_ThrowoutMinusBtn_Hint = 'EV_ThrowoutMinusBtn_Hint';
  EV_ThrowoutBtn_Hint = 'EV_ThrowoutBtn_Hint';
  EV_ThrowoutPlusBtn_Hint = 'EV_ThrowoutPlusBtn_Hint';
  EV_DollarBtn_Hint = 'EV_RaceEnabledBtn_Hint';
  EV_ColorBtn_Hint = 'EV_ColorBtn_Hint';
  EV_SelectRaceBtn_Hint = 'EV_SelectRaceBtn_Hint';

  Section_EventMenu = 'EventMenu';

  EM_UrlCombo_Hint = 'EM_UrlCombo_Hint';
  EM_CategoryCombo_Hint = 'EM_CategoryCombo_Hint';

  EM_GetBtn_Hint = 'EM_GetBtn_Hint';
  EM_DebugModeBtn_Hint = 'EM_MoreBtn_Hint';

  EM_TextBtn_Hint = 'EM_TextBtn_Hint';
  EM_XmlBtn_Hint = 'EM_XmlBtn_Hint';

  EM_DownloadBtn_Hint = 'EM_DownloadBtn_Hint';
  EM_TransformBtn_Hint = 'EM_TransformBtn_Hint';
  EM_ConvertBtn_Hint = 'EM_ConvertBtn_Hint';

  EM_SkipDownloadBtn_Hint = 'EM_SkipDownloadBtn_Hint';
  EM_SkipImportBtn_Hint = 'EM_SkipImportBtn_Hint';

  EM_PostModeBtn_Hint = 'EM_PostModeBtn_Hint';
  EM_UrlInfoBtn_Hint = 'EM_UrlInfoBtn_Hint';
  EM_ReadBtn_Hint = 'EM_ReadBtn_Hint';
  EM_WriteBtn_Hint = 'EM_WriteBtn_Hint';

implementation

uses
  Windows,
  SHFolder,
  RiggVar.Util.AppUtils,
  RiggVar.Util.Classes,
  RiggVar.App.Main;

function GetText(key: string): string;
begin
  result := Main.AppTranslator.GetText(key);
end;

procedure SetText(key: string; value: string);
begin
  Main.AppTranslator.SetTemplateText(key, value);
end;

constructor TAppTranslator.Create;
begin
  FileName := 'fr01-translation.txt';
  TemplateFileName := 'fr01-translation-template.txt';
  VL := TStringList.Create;
  SL := TStringList.Create;
  TL := TStringList.Create;

  MainFormIndex := SL.Count;

  SL.Add(MF_ClearBtn_Caption);
  SL.Add(MF_TestDataBtn_Caption);
  SL.Add(MF_EventMenuBtn_Caption);
  SL.Add(MF_TimingBtn_Caption);
  SL.Add(MF_InfoBtn_Caption);
  SL.Add(MF_StyleBtn_Caption);
  SL.Add(MF_RowHeightBtn_Caption);
  SL.Add(MF_LanguageBtn_Caption);
  SL.Add(MF_FeatureBtn_Caption);
  SL.Add(MF_NorthBtn_Caption);
  SL.Add(MF_SouthBtn_Caption);
  SL.Add(MF_WestBtn_Caption);
  SL.Add(MF_TestBtn_Caption);

  SL.Add(MF_ClearBtn_Hint);
  SL.Add(MF_TestDataBtn_Hint);
  SL.Add(MF_EventMenuBtn_Hint);
  SL.Add(MF_TimingBtn_Hint);
  SL.Add(MF_InfoBtn_Hint);
  SL.Add(MF_StyleBtn_Hint);
  SL.Add(MF_RowHeightBtn_Hint);
  SL.Add(MF_LanguageBtn_Hint);
  SL.Add(MF_FeatureBtn_Hint);
  SL.Add(MF_NorthBtn_Hint);
  SL.Add(MF_SouthBtn_Hint);
  SL.Add(MF_WestBtn_Hint);
  SL.Add(MF_TestBtn_Hint);

  CacheIndex := SL.Count;

  SL.Add(CA_HtmlFragmentBtn_Hint);
  SL.Add(CA_HtmlDocBtn_Hint);
  SL.Add(CA_XmlBtn_Hint);
  SL.Add(CA_TextBtn_Hint);
  SL.Add(CA_BackupBtn_Hint);
  SL.Add(CA_SaveBtn_Hint);
  SL.Add(CA_BatchBtn_Hint);
  SL.Add(CA_ExportBtn_Hint);

  WorkspaceIndex := SL.Count;

  SL.Add(WS_ShowDefaultBtn_Hint);
  SL.Add(WS_ShowComboBtn_Hint);
  SL.Add(WS_LoadBtn_Hint);
  SL.Add(WS_SaveBtn_Hint);
  SL.Add(WS_InitMemoBtn_Hint);
  SL.Add(WS_InitFileBtn_Hint);

  KeyingIndex := SL.Count;

  SL.Add(KT_RaceDownBtn_Hint);
  SL.Add(KT_RaceBtn_Hint);
  SL.Add(KT_RaceUpBtn_Hint);
  SL.Add(KT_ClearBtn_Hint);
  SL.Add(KT_SendBtn_Hint);
  SL.Add(KT_AutoSendBtn_Hint);

  EventIndex := SL.Count;

  SL.Add(EV_PointsBtn_Caption);
  SL.Add(EV_FinishBtn_Caption);
  SL.Add(EV_SelectRaceBtn_Caption);

  SL.Add(EV_PointsBtn_Hint);
  SL.Add(EV_FinishBtn_Hint);
  SL.Add(EV_StrictBtn_Hint);
  SL.Add(EV_ThrowoutMinusBtn_Hint);
  SL.Add(EV_ThrowoutBtn_Hint);
  SL.Add(EV_ThrowoutPlusBtn_Hint);
  SL.Add(EV_DollarBtn_Hint);
  SL.Add(EV_ColorBtn_Hint);
  SL.Add(EV_SelectRaceBtn_Hint);

  EventMenuIndex := SL.Count;

  SL.Add(EM_UrlCombo_Hint);
  SL.Add(EM_CategoryCombo_Hint);
  SL.Add(EM_GetBtn_Hint);
  SL.Add(EM_DebugModeBtn_Hint);
  SL.Add(EM_TextBtn_Hint);
  SL.Add(EM_XmlBtn_Hint);
  SL.Add(EM_DownloadBtn_Hint);
  SL.Add(EM_TransformBtn_Hint);
  SL.Add(EM_ConvertBtn_Hint);
  SL.Add(EM_SkipDownloadBtn_Hint);
  SL.Add(EM_SkipImportBtn_Hint);
  SL.Add(EM_PostModeBtn_Hint);
  SL.Add(EM_UrlInfoBtn_Hint);
  SL.Add(EM_ReadBtn_Hint);
  SL.Add(EM_WriteBtn_Hint);
end;

destructor TAppTranslator.Destroy;
begin
  SL.Free;
  VL.Free;
  TL.Free;
  inherited;
end;

function TAppTranslator.GetText(key: string): string;
begin
  result := '';
  if SL.IndexOf(key) > -1 then
  begin
    result := VL.Values[key];
  end;
end;

function TAppTranslator.GetTemplateText(key: string): string;
begin
  result := '';
  if SL.IndexOf(key) > -1 then
  begin
    result := TL.Values[key];
  end;
end;

procedure TAppTranslator.SetText(key, value: string);
begin
  if SL.IndexOf(key) > -1 then
  begin
    VL.Values[key] := value;
  end;
end;

procedure TAppTranslator.SetHasLocalText(const Value: Boolean);
begin
  FHasLocalText := Value;
end;

procedure TAppTranslator.SetTemplateText(key, value: string);
begin
  if SL.IndexOf(key) > -1 then
  begin
    TL.Values[key] := value;
  end;
end;

procedure TAppTranslator.Load;
var
  fn: string;
  ini: TMemIniFile;
begin
  fn := GetFileName;
  if (fn <> '') and FileExists(fn) then
  begin
    ini := TMemIniFile.Create(fn);
    //ini.Encoding := TEncoding.UTF8;
    try
      LoadStrings(ini);
      HasLocalText := True;
    finally
      ini.Free;
    end;
  end;
end;

procedure TAppTranslator.SaveTemplate;
var
  fn: string;
  ini: TMemIniFile;
begin
  fn := GetTemplateFileName;
  if (fn <> '') {and FileExists(fn)} then
  begin
    ini := TMemIniFile.Create(fn);
    try
      SaveTemplateStrings(ini);
      ini.Encoding := TEncoding.UTF8;
      ini.UpdateFile;
    finally
      ini.Free;
    end;
  end;
end;

procedure TAppTranslator.GetTemplateStrings(Memo: TStrings);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create('');
  try
    SaveTemplateStrings(ini);
    ini.GetStrings(Memo);
  finally
    ini.Free;
  end;
end;

procedure TAppTranslator.SaveTemplateStrings(ini: TCustomIniFile);
var
  n: string;
  k: string;
  v: string;
  i: Integer;
begin
  for i := 0 to SL.Count - 1 do
  begin
    k := SL[i];
    v := GetTemplateText(k);
    if v <> '' then
    begin
      n := GetSectionName(i);
      if n <> '' then
      begin
        ini.WriteString(n, k, v);
      end;
    end;
  end;
end;

procedure TAppTranslator.LoadStrings(ini: TCustomIniFile);
var
  n: string;
  k: string;
  v: string;
  i: Integer;
begin
  for i := 0 to SL.Count - 1 do
  begin
    k := SL[i];
    v := GetText(k);
    n := GetSectionName(i);
    SetText(k, ini.ReadString(n, k, v));
  end;
end;

function TAppTranslator.GetSectionName(i: Integer): string;
begin
  result := '';
  if i >= EventMenuIndex then
    result := Section_EventMenu
  else if i >= EventIndex then
    result := Section_Event
  else if i >= KeyingIndex then
    result := Section_Keying
  else if i >= WorkspaceIndex then
    result := Section_Workspace
  else if i >= CacheIndex then
    result := Section_Cache
  else if i >= MainFormIndex then
    result := Section_MainForm
end;


function TAppTranslator.GetAppDataRoot: string;
begin
  result := TAppUtils.GetSpecialFolderPath(CSIDL_LOCAL_APPDATA, True);
  result := IncludeTrailingPathDelimiter(result) + 'RiggVar\FR\';
  ForceDirectories(result);
end;

function TAppTranslator.GetFileName: string;
begin
  if FileExists(FileName) then
    result := FileName
  else
    result := GetAppDataFileName;
end;

function TAppTranslator.GetAppDataFileName: string;
var
  dn: string;
begin
  result := '';
  dn := GetAppDataRoot;
  if dn <> '' then
    result := IncludeTrailingPathDelimiter(dn) + FileName;
end;

function TAppTranslator.GetTemplateFileName: string;
var
  dn: string;
begin
  result := '';
  dn := GetAppDataRoot;
  if dn <> '' then
    result := IncludeTrailingPathDelimiter(dn) + TemplateFileName;
end;

end.
