unit FrmSwitchTemplate;

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
  RiggVar.BR.SwitchController,
  RiggVar.Col.Switch;

type
  TFormSwitchTemplate = class(TForm)
    LabelRouterHost: TLabel;
    edRouterHost: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    TestBtn: TButton;
    HostLabel: TLabel;
    PortLabel: TLabel;
    edHost: TEdit;
    edPort: TEdit;
    LabelHomePage: TLabel;
    edHomePage: TEdit;
    LabelPortHTTP: TLabel;
    edPortHTTP: TEdit;
    TemplateCombo: TComboBox;
    Template: TLabel;
    LabelTestResult: TLabel;
    CurrentBtn: TButton;
    cbUseRouterHost: TCheckBox;
    procedure TestBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TemplateComboChange(Sender: TObject);
    procedure CurrentBtnClick(Sender: TObject);
    procedure ShowAllBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    SwitchItem: TSwitchItem;
    SwitchItems: TSwitchItems;
    procedure Test;
    procedure UpdateFields(cr: TSwitchItem);
  public
    { Public-Deklarationen }
    procedure LoadModel(Model: TSwitchItem);
    procedure SaveModel(Model: TSwitchItem);
  end;

function EditSwitchItem(Model: TSwitchItem): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

var
  FormSwitchTemplate: TFormSwitchTemplate;

function EditSwitchItem(Model: TSwitchItem): Boolean;
begin
  result := False;
  if not Assigned(FormSwitchTemplate) then
    FormSwitchTemplate := TFormSwitchTemplate.Create(Application);

  FormSwitchTemplate.LoadModel(Model);
  if FormSwitchTemplate.ShowModal = mrOK then
  begin
    FormSwitchTemplate.SaveModel(Model);
    result := True;
  end;
end;

procedure TFormSwitchTemplate.FormCreate(Sender: TObject);
begin
  SwitchItems := TSwitchItems.Create;
  SwitchItems.InitProxyInfo(TemplateCombo.Items);
end;

procedure TFormSwitchTemplate.FormDestroy(Sender: TObject);
begin
  SwitchItems.Free;
end;

procedure TFormSwitchTemplate.LoadModel(Model: TSwitchItem);
var
  cr: TSwitchItem;
begin
  SwitchItem := Model;

  cr := SwitchItems.Current;
  cr.Host := Model.Host;
  cr.Port := Model.Port;
  cr.PortHTTP := Model.PortHTTP;
  cr.RouterHost := Model.RouterHost;
  cr.UseRouterHost := Model.UseRouterHost;

  UpdateFields(cr);
end;

procedure TFormSwitchTemplate.SaveModel(Model: TSwitchItem);
begin
  Model.Host := edHost.Text;
  Model.Port := StrToIntDef(edPort.Text, Model.Port);
  Model.PortHTTP := StrToIntDef(edPortHTTP.Text, Model.PortHTTP);
  Model.RouterHost := edRouterHost.Text;
  Model.UseRouterHost := cbUseRouterHost.Checked;

  SwitchItem := nil;
end;

procedure TFormSwitchTemplate.ShowAllBtnClick(Sender: TObject);
begin
  SwitchItems.InitProxyInfo(TemplateCombo.Items);
end;

procedure TFormSwitchTemplate.OKBtnClick(Sender: TObject);
begin
  Test;
end;

procedure TFormSwitchTemplate.TestBtnClick(Sender: TObject);
begin
  Test;
end;

procedure TFormSwitchTemplate.Test;
begin
  //todo
end;

procedure TFormSwitchTemplate.TemplateComboChange(Sender: TObject);
var
  cr: TSwitchItem;
begin
  cr := SwitchItems.FindItem(TemplateCombo.Text);
  if (cr <> nil) then
    UpdateFields(cr);
end;

procedure TFormSwitchTemplate.CurrentBtnClick(Sender: TObject);
var
  cr: TSwitchItem;
begin
  cr := SwitchItems.Current;
  UpdateFields(cr);
end;

procedure TFormSwitchTemplate.UpdateFields(cr: TSwitchItem);
begin
  edHost.Text := cr.Host;
  edPort.Text := IntToStr(cr.Port);
  edPortHTTP.Text := IntToStr(cr.PortHTTP);
  edRouterHost.Text := cr.RouterHost;
  cbUseRouterHost.Checked := cr.UseRouterHost;
end;

end.
