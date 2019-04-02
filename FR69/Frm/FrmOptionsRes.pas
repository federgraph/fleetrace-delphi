unit FrmOptionsRes;

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
  FrmOptions;

type
  TFormOptionsRes = class(TBaseLocalizer)
  protected
    procedure InitDefault; override;
    procedure Init_de; override;
    procedure Init_en; override;
    procedure LocalizeFormData(f: TFormOptions);
    procedure ExtractFormData(f: TFormOptions);
  public
    Caption: string;
    LabelRaceCountCaption: string;
    LabelITCountCaption: string;
    LabelStartlistCountCaption: string;
    OKBtnCaption: string;
    CancelBtnCaption: string;
    procedure LoadFromFile(ini: TCustomIniFile);
    procedure SaveToFile(ini: TCustomIniFile);
    procedure Localize(f: TForm); override;
    procedure Extract(f: TForm); override;
  end;

implementation

{ TFormDocManagerRes }

procedure TFormOptionsRes.InitDefault;
begin
  Caption := 'Event Params';
  LabelRaceCountCaption := 'RaceCount';
  LabelITCountCaption := 'ITCount';
  LabelStartlistCountCaption := 'StartlistCount';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormOptionsRes.Init_de;
begin
  Caption := 'Event Parameter';
  LabelRaceCountCaption := 'Anzahl Wettfahrten (RaceCount)';
  LabelITCountCaption := 'Anzahl Zwischenzeiten (ITCount)';
  LabelStartlistCountCaption := 'Anzahl Teilnehmer (StartlistCount)';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Abbrechen';
end;

procedure TFormOptionsRes.Init_en;
begin
  Caption := 'Event Params';
  OKBtnCaption := 'OK';
  LabelRaceCountCaption := 'RaceCount';
  LabelITCountCaption := 'ITCount (Intermediate Time)';
  LabelStartlistCountCaption := 'StartlistCount';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormOptionsRes.Extract(f: TForm);
begin
  if f is TFormOptions then
    ExtractFormData(f as TFormOptions);
end;

procedure TFormOptionsRes.ExtractFormData(f: TFormOptions);
begin
  Caption := f.Caption;
  OKBtnCaption := f.OKBtn.Caption;
  LabelRaceCountCaption := f.lblRaceCount.Caption;
  LabelITCountCaption := f.lblTCount.Caption;
  LabelStartlistCountCaption := f.lblStarlistCount.Caption;
  CancelBtnCaption := f.CancelBtn.Caption;
end;

procedure TFormOptionsRes.Localize(f: TForm);
begin
  if f is TFormOptions then
    LocalizeFormData(f as TFormOptions);
end;

procedure TFormOptionsRes.LocalizeFormData(f: TFormOptions);
begin
  f.Caption := Caption;
  f.OKBtn.Caption := OKBtnCaption;
  f.lblRaceCount.Caption := LabelRaceCountCaption;
  f.lblTCount.Caption := LabelITCountCaption;
  f.lblStarlistCount.Caption := LabelStartlistCountCaption;
  f.CancelBtn.Caption := CancelBtnCaption;
end;

procedure TFormOptionsRes.SaveToFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  ini.WriteString(n, 'Caption', Caption);
  ini.WriteString(n, 'LabelRaceCountCaption', LabelRaceCountCaption);
  ini.WriteString(n, 'LabelITCountCaption', LabelITCountCaption);
  ini.WriteString(n, 'LabelStartlistCountCaption', LabelStartlistCountCaption);
  ini.WriteString(n, 'OKBtnCaption', OKBtnCaption);
  ini.WriteString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

procedure TFormOptionsRes.LoadFromFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  Caption := ini.ReadString(n, 'Caption', Caption);
  LabelRaceCountCaption := ini.ReadString(n, 'LabelRaceCountCaption', LabelRaceCountCaption);
  LabelITCountCaption := ini.ReadString(n, 'LabelITCountCaption', LabelITCountCaption);
  LabelStartlistCountCaption := ini.ReadString(n, 'LabelStartlistCountCaption', LabelStartlistCountCaption);
  OKBtnCaption := ini.ReadString(n, 'OKBtnCaption', OKBtnCaption);
  CancelBtnCaption := ini.ReadString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

end.
