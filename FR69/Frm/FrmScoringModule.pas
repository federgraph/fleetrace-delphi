unit FrmScoringModule;

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
  Vcl.Dialogs,
  Vcl.StdCtrls,
  RiggVar.App.Config,
  RiggVar.Calc.EV;

type
  TFormScoringModule = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBoxScoringModule: TGroupBox;
    SimpleTestBtn: TRadioButton;
    InlineBtn: TRadioButton;
    DLLBtn: TRadioButton;
    RemoteBtn: TRadioButton;
    edDLLFileName: TEdit;
    DLLNameLabel: TLabel;
    edHost: TEdit;
    edPort: TEdit;
    PortLabel: TLabel;
    HostLabel: TLabel;
    WebServiceBtn: TRadioButton;
    edServiceUrl: TEdit;
    UrlLabel: TLabel;
    cbFR81: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProviderID: Integer;
    DLLFileName: string;
    RemoteHostName: string;
    RemotePort: Integer;
    WebApplicationUrl: string;
    function CheckDLL: Boolean;
    function CheckRemote: Boolean;
    function CheckWebService: Boolean;
  public
    { Public-Deklarationen }
    OnOK: TNotifyEvent;
    procedure LoadModel(Model: TCalcEvent);
    procedure SaveModel(Model: TCalcEvent);
  end;

function EditScoringModule(Model: TCalcEvent): Boolean;

var
  FormScoringModule: TFormScoringModule;

implementation

uses
  RiggVar.App.Main,
  ScoringService;

{$R *.dfm}

function EditScoringModule(Model: TCalcEvent): Boolean;
begin
  result := False;
  if not Assigned(FormScoringModule) then
    FormScoringModule := TFormScoringModule.Create(Application);

  FormScoringModule.LoadModel(Model);
  if FormScoringModule.ShowModal = mrOK then
  begin
    FormScoringModule.SaveModel(Model);
    result := True;
  end;
end;

{ TFormScoringModule }

function TFormScoringModule.CheckDLL: Boolean;
begin
  result := false;
  DLLFileName := self.edDLLFileName.Text;
  if FileExists(DLLFileName)
    and (ExtractFileExt(DLLFileName) = '.dll') then
    result := true;
  if not result then
  ShowMessage('operation canceled, invalid DLLFileName');
end;

function TFormScoringModule.CheckRemote: Boolean;
begin
  result := false;
  //Test Host
  RemoteHostName := self.edHost.Text;
  if (RemoteHostName = 'localhost')
    or (RemoteHostName = 'thinkpad')
    or (Copy(RemoteHostName, 1, 2) = 'gs')
    or (Copy(RemoteHostName, 1, 2) = 'VC')
  then
    result := true;
  if not result then
    ShowMessage('operation canceled, invalid host')
  else
  begin
    //Test Port
    RemotePort := StrToIntDef(edPort.Text, -1);
    if RemotePort = -1 then
    begin
      result := false;
      ShowMessage('operation canceled, invalid port');
    end;
  end;
end;

function TFormScoringModule.CheckWebService: Boolean;
begin
  try
    WebApplicationUrl := self.edServiceUrl.Text;
    if cbFR81.Checked then
      ScoringService.InitScoringServiceSoap(ScoringService.ServerType_Java)
    else
      ScoringService.InitScoringServiceSoap(ScoringService.ServerType_ASPNET);
    ScoringService.ScoringServerName := WebApplicationUrl;
    result := ScoringService.GetScoringServiceSoap.HelloWorld() <> '';
  except
    ShowMessage('operation canceled, WebService.HelloWorld() is not responding');
    result := false;
  end;
end;

procedure TFormScoringModule.LoadModel(Model: TCalcEvent);
begin
  case Model.ModuleType of
    ScoringProvider_SimpleTest: self.SimpleTestBtn.Checked := true; //1
    ScoringProvider_Inline: self.InlineBtn.Checked := true; //2
    ScoringProvider_ProxyDLL: self.DLLBtn.Checked := true; //3
    ScoringProvider_ProxyXML: self.RemoteBtn.Checked := true; //4
    ScoringProvider_WebService: self.WebServiceBtn.Checked := true; //5
  end;
  self.edHost.Text := Main.IniImage.CalcHost;
  self.edPort.Text := IntToStr(Main.IniImage.CalcPort);
  self.edServiceUrl.Text := Main.IniImage.WebApplicationUrl;
end;

procedure TFormScoringModule.SaveModel(Model: TCalcEvent);
begin
  ProviderID := Model.ModuleType;
  if SimpleTestBtn.Checked then
    ProviderID := 1
  else if self.InlineBtn.Checked then
    ProviderID := 2
  else if self.DLLBtn.Checked and CheckDLL then
    ProviderID := 3
  else if self.RemoteBtn.Checked and CheckRemote then
  begin
    ProviderID := 4;
    Main.IniImage.CalcHost := self.RemoteHostName;
    Main.IniImage.CalcPort := self.RemotePort;
  end
  else if self.WebServiceBtn.Checked and CheckWebService then
  begin
    if cbFR81.Checked then
      ProviderID := 6
    else
      ProviderID := 5;
    Main.IniImage.WebApplicationUrl := WebApplicationUrl;
  end;

  if ProviderID <> Model.ModuleType then
    Model.InitModule(ProviderID);
end;

procedure TFormScoringModule.OKBtnClick(Sender: TObject);
begin
  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

end.
