unit FrmBridgeManager;

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
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  RiggVar.Col.Bridge,
  RiggVar.Col.Switch,
  RiggVar.Col.Output,
  RiggVar.BO.IniImage,
  RiggVar.DAL.Redirector;

type
  TSwitchInfo = record
    Changed: Boolean;
    SwitchHost: string;
    SwitchPort: Integer;
    SwitchPortHTTP: Integer;
    RouterHost: string;
    UseRouterHost: Boolean;
  end;

  TBridgeInfo = record
    Changed: Boolean;
    ProxyType: Integer;
    Host: string;
    Port: Integer;
    PortHTTP: Integer;
    Url: string;
    HomePage: string;
    TaktIn: Integer;
    TaktOut: Integer;
  end;

  TOutputInfo = record
    Changed: Boolean;
    OutputHost: string;
    OutputPort: Integer;
  end;

  TFormBridgeManager = class(TForm)
    com: TGroupBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    rbMock: TRadioButton;
    rbSwitch: TRadioButton;
    rbBridge: TRadioButton;
    BridgeCombo: TComboBox;
    SwitchCombo: TComboBox;
    Memo: TMemo;
    SwitchTemplateBtn: TButton;
    BridgeTempateBtn: TButton;
    rbOutput: TRadioButton;
    OutputCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BridgeComboClick(Sender: TObject);
    procedure SwitchComboClick(Sender: TObject);
    procedure OutputComboClick(Sender: TObject);
    procedure SwitchTemplateBtnClick(Sender: TObject);
    procedure BridgeTempateBtnClick(Sender: TObject);
    procedure rbMockClick(Sender: TObject);
    procedure rbSwitchClick(Sender: TObject);
    procedure rbBridgeClick(Sender: TObject);
    procedure rbOutputClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ini: TCustomIniFile;
    FIniCreated: Boolean;
    BridgeModel: TBridgeItem;
    SwitchModel: TSwitchItem;
    OutputModel: TOutputItem;
    procedure InitBridgeCombo;
    procedure InitSwitchCombo;
    procedure InitOutputCombo;
    function GetIni: TCustomIniFile;
    function GetProviderID: Integer;
    procedure InitBridgeModel(cr: TBridgeItem);
    procedure InitSwitchModel(cr: TSwitchItem);
    procedure InitOutputModel(cr: TOutputItem);
    procedure UpdateBridgeInfoFromBridgeItem(cr: TBridgeItem);
    procedure UpdateSwitchInfoFromSwitchItem(cr: TSwitchItem);
    procedure UpdateOutputInfoFromOutputItem(cr: TOutputItem);
  public
    { Public-Deklarationen }
    BridgeInfo: TBridgeInfo;
    SwitchInfo: TSwitchInfo;
    OutputInfo: TOutputInfo;
    procedure LoadModel(providerID: Integer);
    procedure SaveModel(ini: TBaseIniImage);
    property ProviderID: Integer read GetProviderID;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.Util.Classes,
  FrmSwitchTemplate,
  FrmBridgeTemplate,
  RiggVar.App.Main;

{ TFormBridgeManager }

procedure TFormBridgeManager.FormCreate(Sender: TObject);
begin
  Memo.Clear;
  BridgeModel := TBridgeItem.Create;
  SwitchModel := TSwitchItem.Create;
  OutputModel := TOutputItem.Create;
end;

procedure TFormBridgeManager.FormDestroy(Sender: TObject);
begin
  BridgeModel.Free;
  SwitchModel.Free;
  OutputModel.Free;
  if ini <> nil then
    ini.Free;
end;

procedure TFormBridgeManager.LoadModel(providerID: Integer);
begin
  InitBridgeModel(BridgeModel);
  InitSwitchModel(SwitchModel);
  InitOutputModel(OutputModel);
  BridgeInfo.Changed := False;
  SwitchInfo.Changed := False;
  OutputInfo.Changed := False;
  InitBridgeCombo;
  InitSwitchCombo;
  InitOutputCombo;

  //Löst EventHandler aus RadioBtn_Click --> Model.Show(Memo)
  case providerID of
    0: rbMock.Checked := True;
    1: rbSwitch.Checked := True;
    2: rbBridge.Checked := True;
    3: rbOutput.Checked := True;
    else
      rbMock.Checked := True;
  end;
end;

procedure TFormBridgeManager.SaveModel(ini: TBaseIniImage);
begin
  if (ProviderID = 1) and (SwitchInfo.Changed) then
  begin
    ini.SwitchHost := SwitchInfo.SwitchHost;
    ini.SwitchPort := SwitchInfo.SwitchPort;
    ini.SwitchPortHTTP := SwitchInfo.SwitchPortHTTP;
    ini.RouterHost := SwitchInfo.RouterHost;
    ini.UseRouterHost := SwitchInfo.UseRouterHost;
  end;
  if (ProviderID = 2) and (BridgeInfo.Changed) then
  begin
    ini.BridgeProxyType := BridgeInfo.ProxyType;
    ini.BridgeHost := BridgeInfo.Host;
    ini.BridgePort := BridgeInfo.Port;
    ini.BridgePortHTTP := BridgeInfo.PortHTTP;
    ini.BridgeUrl := BridgeInfo.Url;
    ini.BridgeHomePage := BridgeInfo.HomePage;
    ini.TaktIn := BridgeInfo.TaktIn;
    ini.TaktOut := BridgeInfo.TaktOut;
  end;
  if (ProviderID = 3) and (OutputInfo.Changed) then
  begin
    ini.OutputHost := OutputInfo.OutputHost;
    ini.OutputPort := OutputInfo.OutputPort;
  end;
end;

function TFormBridgeManager.GetIni: TCustomIniFile;
var
  FileName: string;
begin
  if (ini = nil) and not FIniCreated then
  begin
    FIniCreated := True;
    FileName := Main.FolderInfo.SettingsPath + 'BridgeInfo.ini';
    if DBFileExists(FileName) then
      ini := TDBIniFile.Create(FileName);
  end;
  result := ini;
end;

function TFormBridgeManager.GetProviderID: Integer;
begin
  if rbOutput.Checked then
    result := 3
  else if rbBridge.Checked then
    result := 2
  else if rbSwitch.Checked then
    result := 1
  else
    result := 0;
end;

procedure TFormBridgeManager.InitBridgeCombo;
begin
  ini := GetIni;
  if (ini <> nil) and ini.SectionExists('BridgeInfo') then
  begin
    ini.ReadSectionValues('BridgeInfo', BridgeCombo.Items);
  end;
  BridgeCombo.Items.Insert(0, '0=Unchanged');
  BridgeCombo.ItemIndex := 0;
end;

procedure TFormBridgeManager.InitSwitchCombo;
begin
  ini := GetIni;
  if (ini <> nil) and ini.SectionExists('SwitchInfo') then
  begin
    ini.ReadSectionValues('SwitchInfo', SwitchCombo.Items);
  end;
  SwitchCombo.Items.Insert(0, '0=Unchanged');
  SwitchCombo.ItemIndex := 0;
end;

procedure TFormBridgeManager.InitOutputCombo;
begin
  ini := GetIni;
  if (ini <> nil) and ini.SectionExists('OutputInfo') then
  begin
    ini.ReadSectionValues('OutputInfo', OutputCombo.Items);
  end;
  OutputCombo.Items.Insert(0, '0=Unchanged');
  OutputCombo.ItemIndex := 0;
end;

procedure TFormBridgeManager.rbMockClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TFormBridgeManager.rbSwitchClick(Sender: TObject);
begin
  UpdateSwitchInfoFromSwitchItem(SwitchModel);
  Memo.Clear;
  SwitchModel.Show(Memo.Lines);
end;

procedure TFormBridgeManager.rbBridgeClick(Sender: TObject);
begin
  UpdateBridgeInfoFromBridgeItem(BridgeModel);
  Memo.Clear;
  BridgeModel.Show(Memo.Lines);
end;

procedure TFormBridgeManager.rbOutputClick(Sender: TObject);
begin
  UpdateOutputInfoFromOutputItem(OutputModel);
  Memo.Clear;
  OutputModel.Show(Memo.Lines);
end;

procedure TFormBridgeManager.BridgeComboClick(Sender: TObject);
var
  s: string;
  SL: TStrings;
begin
  if BridgeCombo.ItemIndex = 0 then
  begin
    Memo.Clear;
    BridgeModel.Show(Memo.Lines);
    exit;
  end;

  if BridgeCombo.ItemIndex < 1 then
  begin
    Memo.Text := '';
    exit;
  end;

  s := BridgeCombo.Items.ValueFromIndex[BridgeCombo.itemIndex];
  ini := GetIni;
  if ini = nil then
  begin
    Memo.Text :=
      Format('section [%s] does not exist in IniFile.', [s]);
    exit;
  end;
  if ini.SectionExists(s) then
  begin
    ini.ReadSectionValues(s, Memo.Lines);
    SL := Memo.Lines;
    BridgeInfo.ProxyType := StrToIntDef(SL.Values['ProxyType'], 0);
    BridgeInfo.Host := SL.Values['Host'];
    BridgeInfo.Port := StrToIntDef(SL.Values['Port'], 4030);
    BridgeInfo.PortHTTP := StrToIntDef(SL.Values['PortHTTP'], 8087);
    BridgeInfo.Url := SL.Values['Url'];
    BridgeInfo.HomePage := SL.Values['HomePage'];
    BridgeInfo.TaktIn := StrToIntDef(SL.Values['TaktIn'], 60);
    BridgeInfo.TaktOut := StrToIntDef(SL.Values['TaktOut'], 60);
    BridgeInfo.Changed := True;
  end;
end;

procedure TFormBridgeManager.SwitchComboClick(Sender: TObject);
var
  s: string;
  SL: TStrings;
begin
  if SwitchCombo.ItemIndex = 0 then
  begin
    Memo.Clear;
    SwitchModel.Show(Memo.Lines);
    exit;
  end;

  if SwitchCombo.ItemIndex < 1 then
  begin
    Memo.Text := '';
    exit;
  end;

  ini := GetIni;
  s := SwitchCombo.Items.ValueFromIndex[SwitchCombo.itemIndex];
  if ini = nil then
  begin
    Memo.Text :=
      Format('section [%s] does not exist in IniFile.', [s]);
    exit;
  end;
  if ini.SectionExists(s) then
  begin
    ini.ReadSectionValues(s, Memo.Lines);
    SL := Memo.Lines;
    SwitchInfo.SwitchHost := SL.Values['SwitchHost'];
    SwitchInfo.SwitchPort := StrToIntDef(SL.Values['SwitchPort'], 4029);
    SwitchInfo.SwitchPortHTTP := StrToIntDef(SL.Values['SwitchPortHTTP'], 8085);
    SwitchInfo.RouterHost := SL.Values['RouterHost'];
    SwitchInfo.UseRouterHost := TUtils.IsTrue(SL.Values['UseRouterHost']);
    SwitchInfo.Changed := True;
  end;
end;

procedure TFormBridgeManager.OutputComboClick(Sender: TObject);
var
  s: string;
  SL: TStrings;
begin
  if OutputCombo.ItemIndex = 0 then
  begin
    Memo.Clear;
    OutputModel.Show(Memo.Lines);
    exit;
  end;

  if OutputCombo.ItemIndex < 1 then
  begin
    Memo.Text := '';
    exit;
  end;

  ini := GetIni;
  s := OutputCombo.Items.ValueFromIndex[OutputCombo.itemIndex];
  if ini = nil then
  begin
    Memo.Text :=
      Format('section [%s] does not exist in IniFile.', [s]);
    exit;
  end;
  if ini.SectionExists(s) then
  begin
    ini.ReadSectionValues(s, Memo.Lines);
    SL := Memo.Lines;
    OutputInfo.OutputHost := SL.Values['OutputHost'];
    OutputInfo.OutputPort := StrToIntDef(SL.Values['OutputPort'], 4029);
    OutputInfo.Changed := True;
  end;
end;

procedure TFormBridgeManager.InitSwitchModel(cr: TSwitchItem);
var
  ini: TBaseIniImage;
begin
  ini := Main.IniImage;
  cr.Host := ini.SwitchHost;
  cr.Port := ini.SwitchPort;
  cr.PortHTTP := ini.SwitchPortHTTP;
  cr.RouterHost := ini.RouterHost;
  cr.UseRouterHost := ini.UseRouterHost;
end;

procedure TFormBridgeManager.InitBridgeModel(cr: TBridgeItem);
var
  ini: TBaseIniImage;
begin
  ini := Main.IniImage;
  cr.ProxyType := ini.BridgeProxyType;
  cr.Host := ini.BridgeHost;
  cr.Port := ini.BridgePort;
  cr.PortHTTP := ini.BridgePortHTTP;
  cr.Url := ini.BridgeUrl;
  cr.HomePage := ini.BridgeHomePage;
  cr.TaktIn := ini.TaktIn;
  cr.TaktOut := ini.TaktOut;
end;

procedure TFormBridgeManager.InitOutputModel(cr: TOutputItem);
var
  ini: TBaseIniImage;
begin
  ini := Main.IniImage;
  cr.Host := ini.OutputHost;
  cr.Port := ini.OutputPort;
end;

procedure TFormBridgeManager.UpdateSwitchInfoFromSwitchItem(cr: TSwitchItem);
begin
  SwitchInfo.SwitchHost := cr.Host;
  SwitchInfo.SwitchPort := cr.Port;
  SwitchInfo.SwitchPortHTTP := cr.PortHTTP;
  SwitchInfo.RouterHost := cr.RouterHost;
  SwitchInfo.UseRouterHost := cr.UseRouterHost;
  SwitchInfo.Changed := True;
end;

procedure TFormBridgeManager.UpdateBridgeInfoFromBridgeItem(cr: TBridgeItem);
begin
  BridgeInfo.ProxyType := cr.ProxyType;
  BridgeInfo.Host := cr.Host;
  BridgeInfo.Port := cr.Port;
  BridgeInfo.PortHTTP := cr.PortHTTP;
  BridgeInfo.Url := cr.Url;
  BridgeInfo.HomePage := cr.HomePage;
  BridgeInfo.TaktIn := cr.TaktIn;
  BridgeInfo.TaktOut := cr.TaktOut;
  BridgeInfo.Changed := True;
end;

procedure TFormBridgeManager.UpdateOutputInfoFromOutputItem(cr: TOutputItem);
begin
  OutputInfo.OutputHost := cr.Host;
  OutputInfo.OutputPort := cr.Port;
  OutputInfo.Changed := True;
end;

procedure TFormBridgeManager.SwitchTemplateBtnClick(Sender: TObject);
begin
  if EditSwitchItem(SwitchModel) then
  begin
    UpdateSwitchInfoFromSwitchItem(SwitchModel);
    Memo.Clear;
    SwitchModel.Show(Memo.Lines);
    SwitchCombo.Items[0] := 'Changed';
    SwitchCombo.ItemIndex := 0;
  end;
end;

procedure TFormBridgeManager.BridgeTempateBtnClick(Sender: TObject);
begin
  if EditBridgeItem(BridgeModel) then
  begin
    UpdateBridgeInfoFromBridgeItem(BridgeModel);
    Memo.Clear;
    BridgeModel.Show(Memo.Lines);
    BridgeCombo.Items[0] := 'Changed';
    BridgeCombo.ItemIndex := 0;
  end;
end;

end.
