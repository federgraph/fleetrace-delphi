unit FmBrowser;

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

//{$define WantOleObject} //no, no longer wanted, has problems

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  SHDocVw,
{$ifdef WantOleObject}
  System.Win.ComObj,
{$endif}
  RiggVar.Util.UserAgentBrowser,
  RiggVar.Web1.MotorBase,
  RiggVar.Web1.MotorRemote,
  RiggVar.Web1.MotorHome;

type
  TBrowserTab = class(TFrame)
    ToolBar: TToolBar;
    HomeIndexBtn: TSpeedButton;
    HomeBtn: TSpeedButton;
    BackBtn: TSpeedButton;
    RefreshBtn: TSpeedButton;
    OptionsBtn: TSpeedButton;
    RemoteIndexBtn: TSpeedButton;
    PlainBtn: TSpeedButton;
    procedure HomeIndexBtnClick(Sender: TObject);
    procedure HomeBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure WebBrowserDocumentComplete(Sender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
    procedure OptionsBtnClick(Sender: TObject);
    procedure MenuBtnClick(Sender: TObject);
    procedure RemoteIndexBtnClick(Sender: TObject);
    procedure PlainBtnClick(Sender: TObject);
  private
    function GetWebMotorRemote: TWebMotorRemote;
    function GetWebMotorHome: TWebMotor;
  public
    Level: Integer;
    NoCache: OleVariant;
    Nav2Flags: OleVariant;
    WebBrowser: TUserAgentBrowser;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateBrowserOptions;
    property WebMotorRemote: TWebMotorRemote read GetWebMotorRemote;
    property WebMotorHome: TWebMotor read GetWebMotorHome;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.Web2.Master;

constructor TBrowserTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);

  NoCache := 'pragma:nocache';
  Nav2Flags := navNoHistory + navNoReadFromCache + navNoWriteToCache;

  WebBrowser := TUserAgentBrowser.Create(Self);
  WebBrowser.UserAgent := 'FR62';
  WebBrowser.HostDockSite := self;
  WebBrowser.Align := alClient;
  WebBrowser.OnDocumentComplete := WebBrowserDocumentComplete;

{$ifndef WantOleObject}
  HomeIndexBtn.Visible := False;
  RemoteIndexBtn.Visible := False;
{$endif}
end;

destructor TBrowserTab.Destroy;
begin
  inherited;
end;

procedure TBrowserTab.WebBrowserDocumentComplete(Sender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  Inc(Level);
  if Level > 1 then
  begin
    BackBtn.Enabled := True;
    RefreshBtn.Enabled := True;
  end;
end;

procedure TBrowserTab.RefreshBtnClick(Sender: TObject);
begin
  if Level > 0 then
    WebBrowser.Refresh2(NoCache)
  else
  WebBrowser.Refresh;
end;

procedure TBrowserTab.BackBtnClick(Sender: TObject);
begin
  if (Level > 2) and (WebBrowser <> nil) then
  begin
    WebBrowser.GoBack;
  end;
  Level := 1;
  BackBtn.Enabled := False;
end;

procedure TBrowserTab.UpdateBrowserOptions;
begin
  if Main.IniImage.WantXSL then
    OptionsBtn.Caption := 'XSL'
  else
    OptionsBtn.Caption := 'XML';

  if TMasterPage.IsPlain then
    PlainBtn.Caption := 'Plain'
  else
    PlainBtn.Caption := 'Styled';
end;

procedure TBrowserTab.HomeBtnClick(Sender: TObject);
var
  Url: OleVariant;
begin
  BackBtn.Enabled := False;
  Level := 0;
  if Assigned(Main.GuiManager.RemoteRouter) then
  begin
    Url := Main.GuiManager.RemoteRouter.Url;
    Main.GuiManager.RemoteRouter.HomeBtnFlag := true;
    WebBrowser.Navigate2(Url, Nav2Flags);
  end;
end;

procedure TBrowserTab.MenuBtnClick(Sender: TObject);
begin
end;

//procedure TBrowserFrame.MenuBtnClick(Sender: TObject);
//begin
//  if Assigned(Main.GuiManager.WebMotorRemote) then
//  begin
//    WebMotorRemote.CurrentMenu := WebMotorRemote.CurrentMenu + 1;
//    if Level > 0 then
//      WebBrowser.Refresh2(NoCache);
//  end;
//end;

procedure TBrowserTab.HomeIndexBtnClick(Sender: TObject);
{$ifdef WantOleObject}
var
  ie: IWebBrowser2;
  Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
{$endif}
begin
{$ifdef WantOleObject}
  if Assigned(Main.GuiManager.WebMotorHome) then
  begin
    // Uses ComObj + SHDocVw
    ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
    Url := Main.GuiManager.HomeRouter.Url + 'Home/Index';
    ie.Navigate2(Url, Flags, TargetFrameName, PostData, Headers);
    ie.Visible := true;
  end;
{$endif}
end;

procedure TBrowserTab.RemoteIndexBtnClick(Sender: TObject);
{$ifdef WantOleObject}
var
  ie: IWebBrowser2;
  Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
{$endif}
begin
{$ifdef WantOleObject}
  if Assigned(Main.GuiManager.WebMotorRemote) then
  begin
    // Uses ComObj + SHDocVw
    ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
    Url := Main.GuiManager.RemoteRouter.Url + 'Remote/Index';
    ie.Navigate2(Url, Flags, TargetFrameName, PostData, Headers);
    ie.Visible := true;
  end;
{$endif}
end;

procedure TBrowserTab.OptionsBtnClick(Sender: TObject);
begin
  Main.IniImage.WantXSL := not Main.IniImage.WantXSL;
  UpdateBrowserOptions;
end;

procedure TBrowserTab.PlainBtnClick(Sender: TObject);
begin
  TMasterPage.IsPlain := not TMasterPage.IsPlain;
  UpdateBrowserOptions;
  if Level > 0 then
    WebBrowser.Refresh2(NoCache);
end;

function TBrowserTab.GetWebMotorHome: TWebMotor;
begin
  result := Main.GuiManager.WebMotorHome;
end;

function TBrowserTab.GetWebMotorRemote: TWebMotorRemote;
begin
  result := Main.GuiManager.WebMotorRemote;
end;

end.
