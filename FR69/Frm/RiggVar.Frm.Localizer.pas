unit RiggVar.Frm.Localizer;

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
  Vcl.Forms,
  RiggVar.BO.Localizer,
  FrmMain,
  FrmMainRes,
  FrmDocManager,
  FrmDocManagerRes,
  FrmRegattaProps,
  FrmRegattaPropsRes,
  FrmUniquaProps,
  FrmUniquaPropsRes,
  FrmOptions,
  FrmOptionsRes,
  FrmSwitchProps,
  FrmSwitchPropsRes;

type
  TLanguageManager = class(TBaseLanguageManager)
  public
    constructor Create;
    function CreateLocalizer(Language: string): TBaseLocalizer; override;
    function SelectLanguage: Boolean; override;
  end;

  TLocalizer = class(TBaseLocalizer)
  private
    function GetFileName: string;
  protected
    FormFR62Res: TFR62Res;
    FormDocManagerRes: TFormDocManagerRes;
    FormRegattaPropsRes: TFormRegattaPropsRes;
    FormUniquaPropsRes: TFormUniquaPropsRes;
    FormOptionsRes: TFormOptionsRes;
    FormSwitchRes: TFormSwitchRes;
    function GetBaseLocalizer(c: TForm): TBaseLocalizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExportData; override;
    procedure ImportData; override;
    procedure Localize(c: TForm); override;
    procedure Extract(c: TForm); override;
    procedure SetLanguage(Value: string); override;
    property FileName: string read GetFileName;
  end;

implementation

uses
  RiggVar.App.Main,
  FrmLanguage,
  RiggVar.DAL.Redirector;

{ TLanguageManager. }

constructor TLanguageManager.Create;
begin
  inherited;
  SL.Add('extractor');
end;

function TLanguageManager.CreateLocalizer(Language: string): TBaseLocalizer;
begin
  result := TLocalizer.Create;
  Localizer := result;
  Localizer.SetLanguage(Language);
end;

function TLanguageManager.SelectLanguage: Boolean;
begin
  result := EditLanguage(self);
end;

{ TLocalizer }

constructor TLocalizer.Create;
begin
  FormFR62Res := TFR62Res.Create('FormFR62');
  FormDocManagerRes := TFormDocManagerRes.Create('FormDocManager');
  FormRegattaPropsRes := TFormRegattaPropsRes.Create('FormRegattaProps');
  FormUniquaPropsRes := TFormUniquaPropsRes.Create('FormUniquaProps');
  FormOptionsRes := TFormOptionsRes.Create('FormEventParams');
  FormSwitchRes := TFormSwitchRes.Create('FormSwitch');
end;

destructor TLocalizer.Destroy;
begin
  FormFR62Res.Free;
  FormDocManagerRes.Free;
  FormRegattaPropsRes.Free;
  FormUniquaPropsRes.Free;
  FormOptionsRes.Free;
  FormSwitchRes.Free;
  inherited;
end;

procedure TLocalizer.Localize(c: TForm);
var
  f: TBaseLocalizer;
begin
  f := GetBaseLocalizer(c);
  if Assigned(f) then
    f.Localize(c);
end;

procedure TLocalizer.SetLanguage(Value: string);
begin
  inherited;
  FormFR62Res.SetLanguage(Value);
  FormDocManagerRes.SetLanguage(Value);
  FormRegattaPropsRes.SetLanguage(Value);
  FormUniquaPropsRes.SetLanguage(Value);
  FormOptionsRes.SetLanguage(Value);
  FormSwitchRes.SetLanguage(Value);
end;

procedure TLocalizer.Extract(c: TForm);
var
  f: TBaseLocalizer;
begin
  f := GetBaseLocalizer(c);
  if Assigned(f) then
    f.Extract(c);
end;

function TLocalizer.GetBaseLocalizer(c: TForm): TBaseLocalizer;
begin
  if c.ClassType = TFormFR62 then
    result := FormFR62Res
  else if c.ClassType = TFormDocManager then
    result := FormDocManagerRes
  else if c.ClassType = TFormRegattaProps then
    result := FormRegattaPropsRes
  else if c.ClassType = TFormUniquaProps then
    result := FormUniquaPropsRes
  else if c.ClassType = TFormOptions then
    result := FormOptionsRes
  else if c.ClassType = TFormSwitchProps then
    result := FormSwitchRes
  else
    result := nil;
end;

function TLocalizer.GetFileName: string;
begin
  if Assigned(Main) and Assigned(Main.FolderInfo) then
    result := Main.FolderInfo.SettingsPath + 'FR62_L10N.txt'
  else
    result := '';
end;

procedure TLocalizer.ExportData;
var
  ini: TDBIniFile;
begin
  if FileName <> '' then
  begin
    ini := TDBIniFile.Create(FileName);
    FormFR62Res.SaveToFile(ini);
    FormDocManagerRes.SaveToFile(ini);
    FormRegattaPropsRes.SaveToFile(ini);
    FormUniquaPropsRes.SaveToFile(ini);
    FormOptionsRes.SaveToFile(ini);
    FormSwitchRes.SaveToFile(ini);
    ini.AutoSave := True;
    ini.Free;
  end;
end;

procedure TLocalizer.ImportData;
var
  ini: TDBIniFile;
begin
  if FileName <> '' then
  begin
    ini := TDBIniFile.Create(FileName);
    FormFR62Res.LoadFromFile(ini);
    FormDocManagerRes.LoadFromFile(ini);
    FormRegattaPropsRes.LoadFromFile(ini);
    FormUniquaPropsRes.LoadFromFile(ini);
    FormOptionsRes.LoadFromFile(ini);
    FormSwitchRes.LoadFromFile(ini);
    ini.Free;
  end;
end;

end.
