unit FrmRegattaPropsRes;

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
  Vcl.Forms,
  System.IniFiles,
  RiggVar.BO.Localizer,
  FrmRegattaProps;

type
  TFormRegattaPropsRes = class(TBaseLocalizer)
  protected
    procedure InitDefault; override;
    procedure Init_de; override;
    procedure Init_en; override;
    procedure LocalizeFormData(f: TFormRegattaProps);
    procedure ExtractFormData(f: TFormRegattaProps);
  public
    Caption: string;
    LabelEventNameCaption: string;
    LabelEventDatesCaption: string;
    LabelHostClubCaption: string;
    LabelPROCaption: string;
    LabelJuryHeadCaption: string;
    LabelScoringSystemCaption: string;
    LabelDivisionCaption: string;
    LabelThrowoutSchemeCaption: string;
    LabelNumberOfThrowoutsCaption: string;
    LabelThrowoutsEditInfoCaption: string;
    LabelScoringSystemNameCaption: string;
    OKBtnCaption: string;
    CancelBtnCaption: string;
    procedure LoadFromFile(ini: TCustomIniFile);
    procedure SaveToFile(ini: TCustomIniFile);
    procedure Localize(f: TForm); override;
    procedure Extract(f: TForm); override;
 end;

implementation

{ TFormRegattaProps }

procedure TFormRegattaPropsRes.InitDefault;
begin
  Caption := 'Regatta Properties';
  LabelEventNameCaption := 'Event Name';
  LabelEventDatesCaption := 'Event Date(s)';
  LabelHostClubCaption := 'HostClub';
  LabelPROCaption := 'PRO';
  LabelJuryHeadCaption := 'JuryHead';
  LabelScoringSystemCaption := 'Scoring System';
  LabelDivisionCaption := 'Division';
  LabelThrowoutSchemeCaption := 'Throwout Scheme';
  LabelNumberOfThrowoutsCaption := 'Number of Throwouts';
  LabelThrowoutsEditInfoCaption := '(edit number of throwouts with control on main view toolbar)';
  LabelScoringSystemNameCaption := 'External Scoring System ID';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormRegattaPropsRes.Init_de;
begin
  Caption := 'Regatta Eigenschaften';
  LabelEventNameCaption := 'Event Name';
  LabelEventDatesCaption := 'Event Datum';
  LabelHostclubCaption := 'Ausrichter / HostClub';
  LabelPROCaption := 'Wettfahrtleiter / PRO';
  LabelJuryHeadCaption := 'Schiedsrichter / JuryHead';
  LabelScoringSystemCaption := 'Internes Scoring System';
  LabelDivisionCaption := 'Bootsklasse / Division';
  LabelThrowoutSchemeCaption := 'Streicher-Modus'; //Throwout Scheme
  LabelNumberOfThrowoutsCaption := 'Anzahl Streicher'; //'Number of Throwouts';
  LabelThrowoutsEditInfoCaption := '(editiere Anzahl Streicher mit Spinner im  Hauptformular)';
  LabelScoringSystemNameCaption := 'Externe Scoring System ID';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Abbrechen';
end;

procedure TFormRegattaPropsRes.Init_en;
begin
  Caption := 'Event Properties';
  LabelEventNameCaption := 'Event Name';
  LabelEventDatesCaption := 'Event Date(s)';
  LabelHostClubCaption := 'HostClub';
  LabelPROCaption := 'PRO';
  LabelJuryHeadCaption := 'JuryHead';
  LabelScoringSystemCaption := 'Internal Scoring System';
  LabelDivisionCaption := 'Division';
  LabelThrowoutSchemeCaption := 'Throwout Scheme';
  LabelNumberOfThrowoutsCaption := 'Number of Throwouts';
  LabelThrowoutsEditInfoCaption := '(edit number of throwouts with control on main view toolbar)';
  LabelScoringSystemNameCaption := 'External Scoring System ID';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormRegattaPropsRes.Extract(f: TForm);
begin
  if f is TFormRegattaProps then
    ExtractFormData(f as TFormRegattaProps);
end;

procedure TFormRegattaPropsRes.ExtractFormData(f: TFormRegattaProps);
begin
  Caption := f.Caption;
  LabelEventNameCaption := f.lblEventName.Caption;
  LabelEventDatesCaption := f.lblEventDate.Caption;
  LabelHostClubCaption := f.lblOrganizer.Caption;
  LabelPROCaption := f.lblPRO.Caption;
  LabelJuryHeadCaption := f.lblJuryHead.Caption;
  LabelScoringSystemCaption := f.lblScoringSystem.Caption;
  LabelDivisionCaption := f.lblDivision.Caption;
  LabelThrowoutSchemeCaption := f.lblThrowoutScheme.Caption;
  LabelNumberOfThrowoutsCaption := f.lblNumberOfThrowouts.Caption;
  LabelThrowoutsEditInfoCaption := f.lblThrowoutsEditInfo.Caption;
  LabelScoringSystemNameCaption := f.lblScoringSystemName.Caption;
  OKBtnCaption := f.OKBtn.Caption;
  CancelBtnCaption := f.CancelBtn.Caption;
end;

procedure TFormRegattaPropsRes.Localize(f: TForm);
begin
  if f is TFormRegattaProps then
    LocalizeFormData(f as TFormRegattaProps);
end;

procedure TFormRegattaPropsRes.LocalizeFormData(f: TFormRegattaProps);
begin
  f.Caption := Caption;
  f.lblEventName.Caption := LabelEventNameCaption;
  f.lblEventDate.Caption := LabelEventDatesCaption;
  f.lblOrganizer.Caption := LabelHostClubCaption;
  f.lblPRO.Caption := LabelPROCaption;
  f.lblJuryHead.Caption := LabelJuryHeadCaption;
  f.lblScoringSystem.Caption := LabelScoringSystemCaption;
  f.lblDivision.Caption := LabelDivisionCaption;
  f.lblThrowoutScheme.Caption := LabelThrowoutSchemeCaption;
  f.lblNumberOfThrowouts.Caption := LabelNumberOfThrowoutsCaption;
  f.lblThrowoutsEditInfo.Caption := LabelThrowoutsEditInfoCaption;
  f.lblScoringSystemName.Caption := LabelScoringSystemNameCaption;
  f.OKBtn.Caption := OKBtnCaption;
  f.CancelBtn.Caption := CancelBtnCaption;
end;

procedure TFormRegattaPropsRes.SaveToFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  ini.WriteString(n, 'Caption', Caption);
  ini.WriteString(n, 'LabelEventNameCaption', LabelEventNameCaption);
  ini.WriteString(n, 'LabelEventDatesCaption', LabelEventDatesCaption);
  ini.WriteString(n, 'LabelHostClubCaption', LabelHostClubCaption);
  ini.WriteString(n, 'LabelPROCaption', LabelPROCaption);
  ini.WriteString(n, 'LabelJuryHeadCaption', LabelJuryHeadCaption);
  ini.WriteString(n, 'LabelScoringSystemCaption', LabelScoringSystemCaption);
  ini.WriteString(n, 'LabelDivisionCaption', LabelDivisionCaption);
  ini.WriteString(n, 'LabelThrowoutSchemeCaption', LabelThrowoutSchemeCaption);
  ini.WriteString(n, 'LabelNumberOfThrowoutsCaption', LabelNumberOfThrowoutsCaption);
  ini.WriteString(n, 'LabelThrowoutsEditInfoCaption', LabelThrowoutsEditInfoCaption);
  ini.WriteString(n, 'LabelScoringSystemNameCaption', LabelScoringSystemNameCaption);
  ini.WriteString(n, 'OKBtnCaption', OKBtnCaption);
  ini.WriteString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

procedure TFormRegattaPropsRes.LoadFromFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  Caption := ini.ReadString(n, 'Caption', Caption);
  LabelEventNameCaption := ini.ReadString(n, 'LabelEventNameCaption', LabelEventNameCaption);
  LabelEventDatesCaption := ini.ReadString(n, 'LabelEventDatesCaption', LabelEventDatesCaption);
  LabelHostClubCaption := ini.ReadString(n, 'LabelHostClubCaption', LabelHostClubCaption);
  LabelPROCaption := ini.ReadString(n, 'LabelPROCaption', LabelPROCaption);
  LabelJuryHeadCaption := ini.ReadString(n, 'LabelJuryHeadCaption', LabelJuryHeadCaption);
  LabelScoringSystemCaption := ini.ReadString(n, 'LabelScoringSystemCaption', LabelScoringSystemCaption);
  LabelDivisionCaption := ini.ReadString(n, 'LabelDivisionCaption', LabelDivisionCaption);
  LabelThrowoutSchemeCaption := ini.ReadString(n, 'LabelThrowoutSchemeCaption', LabelThrowoutSchemeCaption);
  LabelNumberOfThrowoutsCaption := ini.ReadString(n, 'LabelNumberOfThrowoutsCaption', LabelNumberOfThrowoutsCaption);
  LabelThrowoutsEditInfoCaption := ini.ReadString(n, 'LabelThrowoutsEditInfoCaption', LabelThrowoutsEditInfoCaption);
  LabelScoringSystemNameCaption := ini.ReadString(n, 'LabelScoringSystemNameCaption', LabelScoringSystemNameCaption);
  OKBtnCaption := ini.ReadString(n, 'OKBtnCaption', OKBtnCaption);
  CancelBtnCaption := ini.ReadString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

end.
