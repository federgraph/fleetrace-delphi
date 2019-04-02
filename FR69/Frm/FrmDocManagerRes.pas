unit FrmDocManagerRes;

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
  System.IniFiles,
  Vcl.Forms,
  RiggVar.BO.Localizer,
  FrmDocManager;

type
  TFormDocManagerRes = class(TBaseLocalizer)
  protected
    procedure InitDefault; override;
    procedure Init_de; override;
    procedure Init_en; override;
    procedure LocalizeFormData(f: TFormDocManager);
    procedure ExtractFormData(f: TFormDocManager);
  public
    Caption: string;
    LabelTxtCaption: string;
    LabelMdbCaption: string;
    LabelWebCaption: string;
    OKBtnCaption: string;
    CancelBtnCaption: string;
    procedure LoadFromFile(ini: TCustomIniFile);
    procedure SaveToFile(ini: TCustomIniFile);
    procedure Localize(f: TForm); override;
    procedure Extract(f: TForm); override;
  end;

implementation

{ TFrmDocManagerRes }

procedure TFormDocManagerRes.InitDefault;
begin
  Caption := 'Select Document Manager';
  LabelTxtCaption := 'text file folder';
  LabelMdbCaption := 'connection definition file';
  LabelWebCaption := 'Web Application Url';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormDocManagerRes.Init_de;
begin
  Caption := 'Auswahl des Dokumenten-Managers';
  LabelTxtCaption := 'Text Datei Ordner';
  LabelMdbCaption := 'Datei mit Verbindungs-Definitionen';
  LabelWebCaption := 'Web-Anwendungs-Url';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Abbrechen';
end;

procedure TFormDocManagerRes.Init_en;
begin
  Caption := 'Select Document Manager';
  LabelTxtCaption := 'text file folder';
  LabelMdbCaption := 'connection definition file';
  LabelWebCaption := 'Web Application Url';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormDocManagerRes.Extract(f: TForm);
begin
  if f is TFormDocManager then
    ExtractFormData(f as TFormDocManager);
end;

procedure TFormDocManagerRes.ExtractFormData(f: TFormDocManager);
begin
  Caption := f.Caption;
  LabelTxtCaption := f.LabelTxt.Caption;
  LabelMdbCaption := f.LabelMdb.Caption;
  LabelWebCaption := f.LabelWeb.Caption;
  OKBtnCaption := f.OKBtn.Caption;
  CancelBtnCaption := f.CancelBtn.Caption;
end;

procedure TFormDocManagerRes.Localize(f: TForm);
begin
  if f is TFormDocManager then
    LocalizeFormData(f as TFormDocManager);
end;

procedure TFormDocManagerRes.LocalizeFormData(f: TFormDocManager);
begin
  f.Caption := Caption;
  f.LabelTxt.Caption := LabelTxtCaption;
  f.LabelMdb.Caption := LabelMdbCaption;
  f.LabelWeb.Caption := LabelWebCaption;
  f.OKBtn.Caption := OKBtnCaption;
  f.CancelBtn.Caption := CancelBtnCaption;
end;

procedure TFormDocManagerRes.SaveToFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  ini.WriteString(n, 'Caption', Caption);
  ini.WriteString(n, 'LabelTxtCaption', LabelTxtCaption);
  ini.WriteString(n, 'LabelMdbCaption', LabelMdbCaption);
  ini.WriteString(n, 'LabelWebCaption', LabelWebCaption);
  ini.WriteString(n, 'OKBtnCaption', OKBtnCaption);
  ini.WriteString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

procedure TFormDocManagerRes.LoadFromFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  Caption := ini.ReadString(n, 'Caption', Caption);
  LabelTxtCaption := ini.ReadString(n, 'LabelTxtCaption', LabelTxtCaption);
  LabelMdbCaption := ini.ReadString(n, 'LabelMdbCaption', LabelMdbCaption);
  LabelWebCaption := ini.ReadString(n, 'LabelWebCaption', LabelWebCaption);
  OKBtnCaption := ini.ReadString(n, 'OKBtnCaption', OKBtnCaption);
  CancelBtnCaption := ini.ReadString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

end.
