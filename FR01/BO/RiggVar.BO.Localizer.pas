unit RiggVar.BO.Localizer;

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
  Vcl.Forms;

type
  TBaseLocalizer = class
  private
    FLanguage: string;
    FSectionName: string;
  protected
    procedure InitDefault; virtual;
    procedure Init_de; virtual;
    procedure Init_en; virtual;
  public
    constructor Create(aSectionName: string);
    procedure ExportData; virtual;
    procedure ImportData; virtual;
    procedure Localize(f: TForm); virtual;
    procedure Extract(f: TForm); virtual;
    procedure InitLanguage(Value: string);
    function GetLanguage: string; virtual;
    procedure SetLanguage(Value: string); virtual;
    function GetSectionName: string;
  end;

  TBaseLanguageManager = class
  private
    procedure SetLanguage(const Value: String);
    function GetLanguage: String;
    function GetLanguages: TStrings;
  protected
    SL: TStringList;
    Localizer: TBaseLocalizer;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateLocalizer(Language: string): TBaseLocalizer; virtual;
    function SelectLanguage: Boolean; virtual;
    procedure ExportData;
    procedure ImportData;
    procedure Localize(c: TForm);
    procedure ToggleLanguage;
    property Languages: TStrings read GetLanguages;
    property Language: String read GetLanguage write SetLanguage;
  end;

implementation

{ TBaseLanguageManager }

constructor TBaseLanguageManager.Create;
begin
  inherited;
  SL := TStringList.Create;
  SL.Add('default');
  SL.Add('en');
  SL.Add('de');
end;

function TBaseLanguageManager.CreateLocalizer(Language: string): TBaseLocalizer;
begin
  result := TBaseLocalizer.Create('LanguageManager');
  Localizer := result;
end;

destructor TBaseLanguageManager.Destroy;
begin
  SL.Free;
  if Assigned(Localizer) then
    Localizer.Free;
  inherited;
end;

procedure TBaseLanguageManager.ExportData;
begin
  if Assigned(Localizer) then
    Localizer.ExportData;
end;

function TBaseLanguageManager.GetLanguage: String;
begin
  if not Assigned(Localizer) then
    CreateLocalizer('en');
  result := Localizer.GetLanguage
end;

function TBaseLanguageManager.GetLanguages: TStrings;
begin
  result := SL;
end;

procedure TBaseLanguageManager.ImportData;
begin
  if Assigned(Localizer) then
    Localizer.ImportData;
end;

procedure TBaseLanguageManager.Localize(c: TForm);
begin
  if Assigned(Localizer) then
    Localizer.Localize(c);
end;

function TBaseLanguageManager.SelectLanguage: Boolean;
begin
  result := False;
end;

procedure TBaseLanguageManager.SetLanguage(const Value: String);
begin
  if Assigned(Localizer) then
    Localizer.SetLanguage(Value);
end;

procedure TBaseLanguageManager.ToggleLanguage;
begin
  if Assigned(Localizer) then
  begin
    if Localizer.GetLanguage = 'de' then
      Localizer.SetLanguage('en')
    else
      Localizer.SetLanguage('de');
  end;
end;

{ TBaseLocalizer }

constructor TBaseLocalizer.Create(aSectionName: string);
begin
  FSectionName := aSectionName;
  InitDefault;
end;

procedure TBaseLocalizer.Localize(f: TForm);
begin

end;

procedure TBaseLocalizer.ExportData;
begin

end;

procedure TBaseLocalizer.Extract(f: TForm);
begin
end;

procedure TBaseLocalizer.ImportData;
begin

end;

procedure TBaseLocalizer.InitDefault;
begin
end;

procedure TBaseLocalizer.Init_de;
begin
end;

procedure TBaseLocalizer.Init_en;
begin
end;

function TBaseLocalizer.GetLanguage: string;
begin
  result := FLanguage;
end;

function TBaseLocalizer.GetSectionName: string;
begin
  result := FSectionName;
end;

procedure TBaseLocalizer.SetLanguage(Value: string);
begin
  if Value <> FLanguage then
  begin
    FLanguage := Value;
    InitLanguage(Value);
  end;
end;

procedure TBaseLocalizer.InitLanguage(Value: string);
begin
  FLanguage := Value;
  if FLanguage = 'de' then
    Init_de
  else if FLanguage = 'en' then
    Init_en
  else
    InitDefault;
end;

end.
