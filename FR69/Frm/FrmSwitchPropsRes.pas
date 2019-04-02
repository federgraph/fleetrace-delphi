unit FrmSwitchPropsRes;

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
  FrmSwitchProps;

type
  TFormSwitchRes = class(TBaseLocalizer)
  protected
    procedure InitDefault; override;
    procedure Init_de; override;
    procedure Init_en; override;
    procedure LocalizeFormData(f: TFormSwitchProps);
    procedure ExtractFormData(f: TFormSwitchProps);
  public
    Caption: string;
    LabelSwitchHostCaption: string;
    LabelSwitchPortCaption: string;
    LabelSwitchPortHTTPCaption: string;
    LabelRouterHostCaption: string;
    cbUseRouterHostCaption: string;
    DownloadBtnCaption: string;
    OKBtnCaption: string;
    CancelBtnCaption: string;
    procedure LoadFromFile(ini: TCustomIniFile);
    procedure SaveToFile(ini: TCustomIniFile);
    procedure Localize(f: TForm); override;
    procedure Extract(f: TForm); override;
  end;

implementation

{ TFormDocManagerRes }

procedure TFormSwitchRes.InitDefault;
begin
  Caption := 'Switch Properties';
  LabelSwitchHostCaption := 'Switch Host';
  LabelSwitchPortCaption := 'Switch Port';
  LabelSwitchPortHTTPCaption := 'Switch Port HTTP';
  LabelRouterHostCaption := 'Router Host';
  cbUseRouterHostCaption := 'Use Router';
  DownloadBtnCaption := 'Test HTTP Download';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormSwitchRes.Init_de;
begin
  Caption := 'Switch Einstellungen';
  LabelSwitchHostCaption := 'Switch Host';
  LabelSwitchPortCaption := 'Switch Port';
  LabelSwitchPortHTTPCaption := 'Switch Port HTTP';
  LabelRouterHostCaption := 'Router Host';
  cbUseRouterHostCaption := 'Router benutzen';
  DownloadBtnCaption := 'HTTP Download Testen';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Abbrechen';
end;

procedure TFormSwitchRes.Init_en;
begin
  Caption := 'Switch Properties';
  LabelSwitchHostCaption := 'Switch Host';
  LabelSwitchPortCaption := 'Switch Port';
  LabelSwitchPortHTTPCaption := 'Switch Port HTTP';
  LabelRouterHostCaption := 'Router Host';
  cbUseRouterHostCaption := 'Use Router';
  DownloadBtnCaption := 'Test HTTP Download';
  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';
end;

procedure TFormSwitchRes.Extract(f: TForm);
begin
  if f is TFormSwitchProps then
    ExtractFormData(f as TFormSwitchProps);
end;

procedure TFormSwitchRes.ExtractFormData(f: TFormSwitchProps);
begin
  Caption := f.Caption;
  OKBtnCaption := f.OKBtn.Caption;
  LabelSwitchHostCaption := f.LabelSwitchHost.Caption;
  LabelSwitchPortCaption := f.LabelSwitchPort.Caption;
  LabelSwitchPortHTTPCaption := f.LabelSwitchPort.Caption;
  LabelRouterHostCaption := f.LabelRouterHost.Caption;
  cbUseRouterHostCaption := f.cbUseRouterHost.Caption;
  DownloadBtnCaption := f.DownloadBtn.Caption;
  CancelBtnCaption := f.CancelBtn.Caption;
end;

procedure TFormSwitchRes.Localize(f: TForm);
begin
  if f is TFormSwitchProps then
    LocalizeFormData(f as TFormSwitchProps);
end;

procedure TFormSwitchRes.LocalizeFormData(f: TFormSwitchProps);
begin
  f.Caption := Caption;
  f.OKBtn.Caption := OKBtnCaption;
  f.LabelSwitchHost.Caption := LabelSwitchHostCaption;
  f.LabelSwitchPort.Caption := LabelSwitchPortCaption;
  f.LabelSwitchPortHTTP.Caption := LabelSwitchPortHTTPCaption;
  f.LabelRouterHost.Caption := LabelRouterHostCaption;
  f.cbUseRouterHost.Caption := cbUseRouterHostCaption;
  f.DownloadBtn.Caption := DownloadBtnCaption;
  f.CancelBtn.Caption := CancelBtnCaption;
end;

procedure TFormSwitchRes.SaveToFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  ini.WriteString(n, 'Caption', Caption);
  ini.WriteString(n, 'LabelSwitchHostCaption', LabelSwitchHostCaption);
  ini.WriteString(n, 'LabelSwitchPortCaption', LabelSwitchPortCaption);
  ini.WriteString(n, 'LabelSwitchPortHTTPCaption', LabelSwitchPortHTTPCaption);
  ini.WriteString(n, 'LabelRouterHostCaption', LabelRouterHostCaption);
  ini.WriteString(n, 'cbUseRouterHostCaption', cbUseRouterHostCaption);
  ini.WriteString(n, 'DownloadBtnCaption', DownloadBtnCaption);
  ini.WriteString(n, 'OKBtnCaption', OKBtnCaption);
  ini.WriteString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

procedure TFormSwitchRes.LoadFromFile(ini: TCustomIniFile);
var
  n: string;
begin
  n := GetSectionName;
  Caption := ini.ReadString(n, 'Caption', Caption);
  LabelSwitchHostCaption := ini.ReadString(n, 'LabelSwitchHostCaption', LabelSwitchHostCaption);
  LabelSwitchPortCaption := ini.ReadString(n, 'LabelSwitchPortCaption', LabelSwitchPortCaption);
  LabelSwitchPortHTTPCaption := ini.ReadString(n, 'LabelSwitchPortHTTPCaption', LabelSwitchPortHTTPCaption);
  LabelRouterHostCaption := ini.ReadString(n, 'LabelRouterHostCaption', LabelRouterHostCaption);
  cbUseRouterHostCaption := ini.ReadString(n, 'cbUseRouterHostCaption', cbUseRouterHostCaption);
  DownloadBtnCaption := ini.ReadString(n, 'DownloadBtnCaption', DownloadBtnCaption);
  OKBtnCaption := ini.ReadString(n, 'OKBtnCaption', OKBtnCaption);
  CancelBtnCaption := ini.ReadString(n, 'CancelBtnCaption', CancelBtnCaption);
end;

end.
