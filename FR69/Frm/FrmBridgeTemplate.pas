unit FrmBridgeTemplate;

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
  RiggVar.BR.BridgeController,
  RiggVar.Col.Bridge;

type
  TFormBridgeTemplate = class(TForm)
    LabelUrl: TLabel;
    edUrl: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    TestBtn: TButton;
    LabelTaktOut: TLabel;
    edTaktOut: TEdit;
    LabelTaktIn: TLabel;
    edTaktIn: TEdit;
    HostLabel: TLabel;
    PortLabel: TLabel;
    edHost: TEdit;
    edPort: TEdit;
    LabelHomePage: TLabel;
    edHomePage: TEdit;
    LabelPortHTTP: TLabel;
    edPortHTTP: TEdit;
    ProxyTypeCombo: TComboBox;
    TemplateCombo: TComboBox;
    LabelProxyType: TLabel;
    Template: TLabel;
    LabelTestResult: TLabel;
    CurrentBtn: TButton;
    ShowAllBtn: TButton;
    procedure TestBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProxyTypeComboChange(Sender: TObject);
    procedure TemplateComboChange(Sender: TObject);
    procedure CurrentBtnClick(Sender: TObject);
    procedure ShowAllBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    BridgeItem: TBridgeItem;
    BridgeItems: TBridgeItems;
    procedure Test;
    procedure UpdateFields(cr: TBridgeItem);
    procedure EnableFields(pt: TBridgeProxyType);
    procedure UpdateLabels;
    procedure EnableLabel(l: TLabel; b: Boolean);
    procedure ApplyPatches(pt: TBridgeProxyType);
  public
    { Public-Deklarationen }
    procedure LoadModel(Model: TBridgeItem);
    procedure SaveModel(Model: TBridgeItem);
  end;

function EditBridgeItem(Model: TBridgeItem): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

var
  FormBridgeTemplate: TFormBridgeTemplate;

function EditBridgeItem(Model: TBridgeItem): Boolean;
begin
  result := False;
  if not Assigned(FormBridgeTemplate) then
    FormBridgeTemplate := TFormBridgeTemplate.Create(Application);

  FormBridgeTemplate.LoadModel(Model);
  if FormBridgeTemplate.ShowModal = mrOK then
  begin
    FormBridgeTemplate.SaveModel(Model);
    result := True;
  end;
end;

procedure TFormBridgeTemplate.FormCreate(Sender: TObject);
begin
  BridgeItems := TBridgeItems.Create;
  BridgeItems.InitProxyType(ProxyTypeCombo.Items);
  BridgeItems.InitBridgeInfo(TemplateCombo.Items);
end;

procedure TFormBridgeTemplate.FormDestroy(Sender: TObject);
begin
  BridgeItems.Free;
end;

procedure TFormBridgeTemplate.LoadModel(Model: TBridgeItem);
var
  cr: TBridgeItem;
begin
  BridgeItem := Model;

  cr := BridgeItems.Current;
  cr.ProxyType := Model.ProxyType;
  cr.Host := Model.Host;
  cr.Port := Model.Port;
  cr.PortHTTP := Model.PortHTTP;
  cr.Url := Model.Url;
  cr.HomePage := Model.HomePage;
  cr.TaktIn := Model.TaktIn;
  cr.TaktOut := Model.TaktOut;

  UpdateFields(cr);
end;

procedure TFormBridgeTemplate.SaveModel(Model: TBridgeItem);
begin
  Model.ProxyType := ProxyTypeCombo.ItemIndex;
  Model.Host := edHost.Text;
  Model.Port := StrToIntDef(edPort.Text, Model.Port);
  Model.PortHTTP := StrToIntDef(edPortHTTP.Text, Model.PortHTTP);
  Model.Url := edUrl.Text;
  Model.HomePage := edHomePage.Text;
  Model.TaktOut := StrToIntDef(edTaktOut.Text, Model.TaktOut);
  Model.TaktIn := StrToIntDef(edTaktIn.Text, Model.TaktIn);

  BridgeItem := nil;
end;

procedure TFormBridgeTemplate.ShowAllBtnClick(Sender: TObject);
begin
  BridgeItems.InitBridgeInfo(TemplateCombo.Items);
end;

procedure TFormBridgeTemplate.OKBtnClick(Sender: TObject);
begin
  Test;
end;

procedure TFormBridgeTemplate.TestBtnClick(Sender: TObject);
begin
  Test;
end;

procedure TFormBridgeTemplate.Test;
var
  temp: Integer;
begin
  temp := StrToIntDef(edTaktOut.Text, 5);
  if temp < -1 then
    temp := -1;
  if temp > 100 then
    temp := 100;
  edTaktOut.Text := IntToStr(temp);

  temp := StrToIntDef(edTaktIn.Text, 5);
  if temp < -1 then
    temp := -1;
  if temp > 100 then
    temp := 100;
  edTaktIn.Text := IntToStr(temp);
end;

procedure TFormBridgeTemplate.ProxyTypeComboChange(Sender: TObject);
var
  pt: TBridgeProxyType;
begin
  pt := TBridgeProxyType(ProxyTypeCombo.ItemIndex);
  EnableFields(pt);
  ApplyPatches(pt);
  BridgeItems.InitBridgeInfoFiltered(TemplateCombo.Items, pt);
end;

procedure TFormBridgeTemplate.TemplateComboChange(Sender: TObject);
var
  cr: TBridgeItem;
begin
  cr := BridgeItems.FindItem(TemplateCombo.Text);
  if (cr <> nil) then
    UpdateFields(cr);
end;

procedure TFormBridgeTemplate.CurrentBtnClick(Sender: TObject);
var
  cr: TBridgeItem;
begin
  cr := BridgeItems.Current;
  UpdateFields(cr);
end;

procedure TFormBridgeTemplate.UpdateFields(cr: TBridgeItem);
begin
  ProxyTypeCombo.ItemIndex := cr.ProxyType;
  edHost.Text := cr.Host;
  edPort.Text := IntToStr(cr.Port);
  edPortHTTP.Text := IntToStr(cr.PortHTTP);
  edUrl.Text := cr.Url;
  edHomePage.Text := cr.HomePage;
  edTaktIn.Text := IntToStr(cr.TaktIn);
  edTaktOut.Text := IntToStr(cr.TaktOut);
  EnableFields(TBridgeProxyType(cr.ProxyType));
end;

procedure TFormBridgeTemplate.EnableFields(pt: TBridgeProxyType);
var
  cr: TBridgeItem;
begin
  cr := BridgeItems.BridgeItem[ProxyTypeCombo.ItemIndex];
  if (cr <> nil) then
  begin
    edHost.Enabled := cr.Enabled(pt, bfHost);
    edPort.Enabled := cr.Enabled(pt, bfPort);
    edPortHTTP.Enabled := cr.Enabled(pt, bfPortHTTP);
    edUrl.Enabled := cr.Enabled(pt, bfUrl);
    edHomePage.Enabled := cr.Enabled(pt, bfHomePage);
    edTaktIn.Enabled := cr.Enabled(pt, bfTaktIn);
    edTaktOut.Enabled := cr.Enabled(pt, bfTaktOut);
  end;
  UpdateLabels;
end;

procedure TFormBridgeTemplate.UpdateLabels;
begin
  EnableLabel(HostLabel, edHost.Enabled);
  EnableLabel(PortLabel, edPort.Enabled);
  EnableLabel(LabelPortHTTP, edPortHTTP.Enabled);
  EnableLabel(LabelUrl, edUrl.Enabled);
  EnableLabel(LabelHomePage, edHomePage.Enabled);
  EnableLabel(LabelTaktIn, edTaktIn.Enabled);
  EnableLabel(LabelTaktOut, edTaktOut.Enabled);
end;

procedure TFormBridgeTemplate.EnableLabel(l: TLabel; b: Boolean);
begin
  l.Enabled := b;
end;

procedure TFormBridgeTemplate.ApplyPatches(pt: TBridgeProxyType);
begin
  if pt = ptServer then
  begin
    edTaktIn.Text := IntToStr(0);
    edTaktOut.Text := IntToStr(0);
  end
  else
  begin
    //'Current' Values
    edTaktIn.Text := IntToStr(BridgeItem.TaktIn);
    edTaktOut.Text := IntToStr(BridgeItem.TaktOut);
  end;
end;

end.
