unit FmWeb;

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
  RiggVar.App.GuiManager,
  RiggVar.EM.CategoryCache;

type
  TWebTab = class(TFrame)
    HostCombo: TComboBox;
    InitBtn: TButton;
    GroupBoxUrl: TGroupBox;
    HomeLabel: TLabel;
    PublishRemoteWebItem: TButton;
    PublishSilverlightWebItem: TButton;
    cbRemote: TCheckBox;
    cbSilverlight: TCheckBox;
    CopyUrlBtn: TButton;
    edTemp: TEdit;
    GroupBoxData: TGroupBox;
    SaveBtn: TButton;
    ClearBtn: TButton;
    LoadBtn: TButton;
    CopyDataBtn: TButton;
    Label1: TLabel;
    PublishAngularWebItem: TButton;
    cbAngular: TCheckBox;
    procedure InitBtnClick(Sender: TObject);
    procedure CopyUrlBtnClick(Sender: TObject);
    procedure PublishRemoteWebItemClick(Sender: TObject);
    procedure PublishSilverlightWebItemClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure CopyDataBtnClick(Sender: TObject);
    procedure PublishAngularWebItemClick(Sender: TObject);
  private
    function GetGuiManager: TGuiManager;
    function GetDataBO: TCategoryCache;
    procedure UpdateHostCombo;
    procedure ShowDataDir;
  public
    procedure Init;
    procedure UpdateFormStatus;
    property GM: TGuiManager read GetGuiManager;
    property DataBO: TCategoryCache read GetDataBO;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.Util.WebUtils,
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Util.AppUtils;

procedure TWebTab.Init;
begin
  HomeLabel.Font.Color := clGray;
  edTemp.Visible := False;

  UpdateHostCombo;
  UpdateFormStatus;

  ShowDataDir;

  { disabled because Host will be determined by Scenario }
  HostCombo.Enabled := False;

  { components will be started according to scenario }
  InitBtn.Enabled := False;
end;

function TWebTab.GetGuiManager: TGuiManager;
begin
  result := Main.GuiManager;
end;

function TWebTab.GetDataBO: TCategoryCache;
begin
  result := Main.CategoryCache;
end;

procedure TWebTab.InitBtnClick(Sender: TObject);
begin
  Main.IniImage.WebServerHost := HostCombo.Text;
  GM.InitWeb;
  InitBtn.Enabled := False;
  HomeLabel.Caption := GM.WebMotorHome.Url;
  HomeLabel.Font.Color := clNavy;
  UpdateFormStatus;
end;

procedure TWebTab.PublishSilverlightWebItemClick(Sender: TObject);
begin
  if Assigned(GM.SilverlightWeb) then
  begin
    GM.SilverlightWeb.IsOffline := not GM.SilverlightWeb.IsOffline;
    cbSilverlight.Checked := GM.SilverlightWeb.IsOnline;
  end;
end;

procedure TWebTab.PublishAngularWebItemClick(Sender: TObject);
begin
  if Assigned(GM.AngularWeb) then
  begin
    GM.AngularWeb.IsOffline := not GM.AngularWeb.IsOffline;
    cbAngular.Checked := GM.AngularWeb.IsOnline;
  end;
end;

procedure TWebTab.PublishRemoteWebItemClick(Sender: TObject);
begin
  if Assigned(GM.WebMotorRemote) then
  begin
    GM.WebMotorRemote.IsOffline := not GM.WebMotorRemote.IsOffline;
    cbRemote.Checked := GM.WebMotorRemote.IsOnline;
  end;
end;

procedure TWebTab.CopyUrlBtnClick(Sender: TObject);
begin
  edTemp.Text := HomeLabel.Caption;
  edTemp.SelectAll;
  edTemp.CopyToClipboard;
end;

procedure TWebTab.UpdateHostCombo;
var
  s: string;
begin
  s := WebUtils.GetLocalHostName;
  //s := WebUtils.ResolveHost(s);
  HostCombo.Items.Add('localhost');
  HostCombo.Items.Add(s);
  HostCombo.ItemIndex := 0;
end;

procedure TWebTab.UpdateFormStatus;
begin
//  if Assigned(Main.GuiManager.HomeRouter) then
//  begin
//    StatusBar.Panels[StatusPanelHomePort].Text := IntToStr(Main.IniImage.WebServerHomePort);
//  end;
//
//  if Assigned(Main.GuiManager.RemoteRouter) then
//  begin
//    StatusBar.Panels[StatusPanelRemotePort].Text := IntToStr(Main.IniImage.WebServerRemotePort);
//  end;

  //cbHome.Checked := GM.WebMotorHome.IsOnline;
  cbRemote.Checked := GM.WebMotorRemote.IsOnline;
  cbSilverlight.Checked := GM.SilverlightWeb.IsOnline;
  cbAngular.Checked := GM.AngularWeb.IsOnline;

  HomeLabel.Caption := GM.WebMotorHome.Url;

  ShowDataDir;
end;

procedure TWebTab.ClearBtnClick(Sender: TObject);
begin
  DataBO.Clear;
end;

procedure TWebTab.LoadBtnClick(Sender: TObject);
begin
  DataBO.Restore;
end;

procedure TWebTab.SaveBtnClick(Sender: TObject);
begin
  DataBO.Backup;
end;

procedure TWebTab.CopyDataBtnClick(Sender: TObject);
begin
  edTemp.Text := DataBO.CacheDir; //TAppUtils.GetAppDataDir;
  edTemp.SelectAll;
  edTemp.CopyToClipboard;
end;

procedure TWebTab.ShowDataDir;
var
  dn: string;
begin
  //dn := TAppUtils.GetAppDataDir;
  dn := DataBO.CacheDir;
  GroupBoxData.Caption := Format('Data in %s', [dn]);
end;

end.
