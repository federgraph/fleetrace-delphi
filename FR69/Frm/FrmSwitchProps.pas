unit FrmSwitchProps;

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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP;

type
  TFormSwitchProps = class(TForm)
    OKBtn: TButton;
    edSwitchHost: TEdit;
    LabelSwitchHost: TLabel;
    edSwitchPort: TEdit;
    LabelSwitchPort: TLabel;
    Memo: TMemo;
    DownloadBtn: TButton;
    edSwitchPortHTTP: TEdit;
    LabelSwitchPortHTTP: TLabel;
    edRouterHost: TEdit;
    LabelRouterHost: TLabel;
    cbUseRouterHost: TCheckBox;
    CancelBtn: TButton;
    procedure DownloadBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    HTTPClient: TIdHTTP;
  public
    procedure LoadModel(Model: TObject);
    procedure SaveModel(Model: TObject);
    function PlugoutNeeded: Boolean;
  end;

var
  FormSwitchProps: TFormSwitchProps;

function EditSwitchProps(Model: TObject): Boolean;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.IniImage;

{$R *.dfm}

function EditSwitchProps(Model: TObject): Boolean;
begin
  result := False;
  if not Assigned(FormSwitchProps) then
    FormSwitchProps := TFormSwitchProps.Create(Application);

  FormSwitchProps.LoadModel(Model);
  if FormSwitchProps.ShowModal = mrOK then
  begin
    if Main.PeerController.PlugTouched and FormSwitchProps.PlugoutNeeded then
      Main.PeerController.Plugout;
    FormSwitchProps.SaveModel(Model);
    result := True;
  end;
end;

{ TFormSwitch }

function TFormSwitchProps.PlugoutNeeded: Boolean;
begin
  result := false;
  if (cbUseRouterHost.Checked <> Main.IniImage.UseRouterHost)
  or (edRouterhost.Text <> Main.IniImage.RouterHost)
  or (edSwitchHost.Text <> Main.IniImage.SwitchHost)
  or (edSwitchPort.Text <> IntToStr(Main.IniImage.SwitchPort))
  or (edSwitchPortHTTP.Text <> IntToStr(Main.IniImage.SwitchPortHTTP))
  then
    result := true;
end;

procedure TFormSwitchProps.LoadModel(Model: TObject);
begin
  self.cbUseRouterHost.Checked := Main.IniImage.UseRouterHost;
  self.edRouterhost.Text := Main.IniImage.RouterHost;
  self.edSwitchHost.Text := Main.IniImage.SwitchHost;
  self.edSwitchPort.Text := IntToStr(Main.IniImage.SwitchPort);
  self.edSwitchPortHTTP.Text := IntToStr(Main.IniImage.SwitchPortHTTP);
end;

procedure TFormSwitchProps.SaveModel(Model: TObject);
var
  ini: TBaseIniImage;
begin
  ini := Main.IniImage;
  ini.UseRouterHost := cbUseRouterHost.Checked;
  ini.RouterHost := edRouterHost.Text;
  ini.SwitchHost := edSwitchHost.Text;
  ini.SwitchPort := StrToIntDef(edSwitchPort.Text, ini.SwitchPort);
  ini.SwitchPortHTTP := StrToIntDef(edSwitchPortHTTP.Text, ini.SwitchPortHTTP);
end;

procedure TFormSwitchProps.DownloadBtnClick(Sender: TObject);
var
  url: string;
  port: Integer;
  host: string;
  eventType: Integer;
begin
  try
  if not Assigned(HTTPClient) then
    HTTPClient := TIdHTTP.Create(Self);
    port := StrToIntDef(edSwitchPortHTTP.Text, Main.IniImage.SwitchPortHTTP);
    host := edSwitchHost.Text;
    eventType := Main.IniImage.DefaultEventType;
    url := Format('http://%s:%d/Data?EventType=%d', [host, port, eventType]);
    Memo.Text := HTTPClient.Get(url);
    edSwitchPortHTTP.Text := IntToStr(port);
    if Memo.Text = '' then
      Memo.Text := 'empty answer';
  except
    on e: Exception do
    Memo.Text := e.Message;
  end;
end;

procedure TFormSwitchProps.FormShow(Sender: TObject);
begin
  Memo.Clear;
end;

end.
