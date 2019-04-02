unit FrmUniquaPropsRes;

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
  FrmUniquaProps;

type
  TFormUniquaPropsRes = class(TBaseLocalizer)
  protected
    procedure InitDefault; override;
    procedure Init_de; override;
    procedure Init_en; override;
    procedure LocalizeFormData(f: TFormUniquaProps);
    procedure ExtractFormData(f: TFormUniquaProps);
  public
    Caption: string;
    GbUniquaCaption: string;
    StrGemeldet: string;
    StrGezeitet: string;
    StrGesegelt: string;
    LabelFaktorCaption: string;
    CbLockCaption: string;
    OKBtnCaption: string;
    CancelBtnCaption: string;
    procedure LoadFromFile(ini: TCustomIniFile);
    procedure SaveToFile(ini: TCustomIniFile);
    procedure Localize(f: TForm); override;
    procedure Extract(f: TForm); override;
  end;

implementation

{ TFormUniquaPropsRes }

procedure TFormUniquaPropsRes.Extract(f: TForm);
begin
  if f is TFormUniquaProps then
    ExtractFormData(f as TFormUniquaProps);
end;

procedure TFormUniquaPropsRes.ExtractFormData(f: TFormUniquaProps);
begin
  Caption := f.Caption;
  GbUniquaCaption := f.gbUniqua.Caption;
  LabelFaktorCaption := f.lblFaktor.Caption;
  CbLockCaption := f.cbLock.Caption;
  OKBtnCaption := f.OKBtn.Caption;
  CancelBtnCaption := f.CancelBtn.Caption;
end;

procedure TFormUniquaPropsRes.InitDefault;
begin
  Caption := 'Uniqua Properties';
  GbUniquaCaption := 'fuer Rangliste';
  StrGemeldet := 'Gemeldet';
  StrGezeitet := 'Gezeitet';
  StrGesegelt := 'Gesegelt';
  LabelFaktorCaption := 'Faktor';
  CbLockCaption := 'Override calculated Values';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormUniquaPropsRes.Init_de;
begin
  Caption := 'Uniqua Eigenschaften';
  GbUniquaCaption := 'für Rangliste';
  StrGemeldet := 'Gemeldet';
  StrGezeitet := 'Gezeitet';
  StrGesegelt := 'Gesegelt';
  LabelFaktorCaption := 'Faktor';
  CbLockCaption := 'Überschreibe berechnete Werte';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Abbrechen';
end;

procedure TFormUniquaPropsRes.Init_en;
begin
  Caption := 'Uniqua Properties';
  GbUniquaCaption := 'for Rangliste';
  StrGemeldet := 'Entered';
  StrGezeitet := 'Finished';
  StrGesegelt := 'Sailed';
  LabelFaktorCaption := 'Factor';
  CbLockCaption := 'override calculated values';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormUniquaPropsRes.Localize(f: TForm);
begin
  if f is TFormUniquaProps then
    LocalizeFormData(f as TFormUniquaProps);
end;

procedure TFormUniquaPropsRes.LocalizeFormData(f: TFormUniquaProps);
begin
  f.Caption := Caption;
  f.gbUniqua.Caption := GbUniquaCaption;
  f.StrGemeldet := StrGemeldet;
  f.StrGezeitet := StrGezeitet;
  f.StrGesegelt := StrGesegelt;
  f.lblFaktor.Caption := LabelFaktorCaption;
  f.cbLock.Caption := CbLockCaption;
  f.OKBtn.Caption := OKBtnCaption;
  f.CancelBtn.Caption := CancelBtnCaption;
end;

procedure TFormUniquaPropsRes.SaveToFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  ini.WriteString(n, 'Caption', Caption);
  ini.WriteString(n, 'GbUniquaCaption', GbUniquaCaption);
  ini.WriteString(n, 'StrGemeldet', StrGemeldet);
  ini.WriteString(n, 'StrGezeitet', StrGezeitet);
  ini.WriteString(n, 'StrGesegelt', StrGesegelt);
  ini.WriteString(n, 'LabelFaktorCaption', LabelFaktorCaption);
  ini.WriteString(n, 'CbLockCaption', CbLockCaption);
  ini.WriteString(n, 'OKBtnCaption', OKBtnCaption);
  ini.WriteString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

procedure TFormUniquaPropsRes.LoadFromFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  Caption := ini.ReadString(n, 'Caption', Caption);
  GbUniquaCaption := ini.ReadString(n, 'GbUniquaCaption', GbUniquaCaption);
  StrGemeldet := ini.ReadString(n, 'StrGemeldet', StrGemeldet);
  StrGezeitet := ini.ReadString(n, 'StrGezeitet', StrGezeitet);
  StrGesegelt := ini.ReadString(n, 'StrGesegelt', StrGesegelt);
  LabelFaktorCaption := ini.ReadString(n, 'LabelFaktorCaption', LabelFaktorCaption);
  CbLockCaption := ini.ReadString(n, 'CbLockCaption', CbLockCaption);
  OKBtnCaption := ini.ReadString(n, 'OKBtnCaption', OKBtnCaption);
  CancelBtnCaption := ini.ReadString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

end.
