unit FmWorkspace;

(*
-     F
-    * *  *
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
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Buttons,
  Vcl.StdCtrls,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  RiggVar.EM.WorkspaceListBase;

type
  TWorkspaceTab = class(TFrame)
    ToolBarWorkspace: TToolBar;
    Memo: TMemo;
    LoadBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    ShowComboBtn: TSpeedButton;
    InitFileBtn: TSpeedButton;
    InitMemoBtn: TSpeedButton;
    ShowDefaultBtn: TSpeedButton;
    procedure ShowComboBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure InitFileBtnClick(Sender: TObject);
    procedure InitMemoBtnClick(Sender: TObject);
    procedure ShowDefaultBtnClick(Sender: TObject);
  private
    function GetWorkspaceList: TWorkspaceListBase;
    procedure Reset;
    procedure InitDefaultBtnText;
    procedure InitAlternativeBtnText;
    procedure InitPrimaryBtnText;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExportBtnText;
    procedure InitBtnText;
    property WorkspaceList: TWorkspaceListBase read GetWorkspaceList;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.App.Translation,
  RiggVar.Util.AppUtils;

constructor TWorkspaceTab.Create(AOwner: TComponent);
begin
  inherited;
  Memo.Align := alClient;
  Reset;
  InitBtnText;
end;

function TWorkspaceTab.GetWorkspaceList: TWorkspaceListBase;
begin
  result := Main.WorkspaceList;
end;

procedure TWorkspaceTab.ShowComboBtnClick(Sender: TObject);
begin
  Reset;
  Memo.Text := WorkspaceList.GetText;
end;

procedure TWorkspaceTab.ShowDefaultBtnClick(Sender: TObject);
begin
  Reset;
  WorkspaceList.LoadDefault;
  Memo.Text := WorkspaceList.GetTemp;
end;

procedure TWorkspaceTab.LoadBtnClick(Sender: TObject);
begin
  WorkspaceList.Load;
  Memo.Text := WorkspaceList.GetTemp;
  if Memo.Lines.Count > 0 then
  begin
    InitFileBtn.Enabled := True;
  end;
  SaveBtn.Enabled := True;
end;

procedure TWorkspaceTab.SaveBtnClick(Sender: TObject);
begin
  WorkspaceList.SetText(Memo.Text);
  WorkspaceList.Save;
end;

procedure TWorkspaceTab.InitMemoBtnClick(Sender: TObject);
begin
  WorkspaceList.SetText(Memo.Text);
  WorkspaceList.Init(True);
  Main.GuiManager.GuiInterface.HandleInform(WorkspaceListChanged);
end;

procedure TWorkspaceTab.InitFileBtnClick(Sender: TObject);
begin
  WorkspaceList.Init(False);
  Main.GuiManager.GuiInterface.HandleInform(WorkspaceListChanged);
end;

procedure TWorkspaceTab.Reset;
begin
  SaveBtn.Enabled := False;
  InitFileBtn.Enabled := False;
end;

procedure TWorkspaceTab.InitBtnText;
begin
  if Main.Params.WantLocalText then
  begin
    ShowDefaultBtn.Hint := GetText(WS_ShowDefaultBtn_Hint);
    ShowComboBtn.Hint := GetText(WS_ShowComboBtn_Hint);
    LoadBtn.Hint := GetText(WS_LoadBtn_Hint);
    SaveBtn.Hint := GetText(WS_SaveBtn_Hint);
    InitMemoBtn.Hint := GetText(WS_InitMemoBtn_Hint);
    InitFileBtn.Hint := GetText(WS_InitFileBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TWorkspaceTab.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TWorkspaceTab.InitPrimaryBtnText;
begin
  ShowDefaultBtn.Hint := 'show default urls';
  ShowComboBtn.Hint := 'show list of urls (as of last normal load)';
  LoadBtn.Hint := 'load file into Memo';
  SaveBtn.Hint := 'save Memo content to file';
  InitMemoBtn.Hint := 'init UrlCombo from Memo';
  InitFileBtn.Hint := 'init UrlCombo normally (file + default)';
end;

procedure TWorkspaceTab.InitAlternativeBtnText;
begin
  ShowDefaultBtn.Hint := 'zeige Standard-Urls';
  ShowComboBtn.Hint := 'zeige Liste der zuletzt geladenen Urls';
  LoadBtn.Hint := 'lade Datei in das Memo';
  SaveBtn.Hint := 'speichere den Inhalt des Memos in Datei';
  InitMemoBtn.Hint := 'initialisiere die UrlCombo vom Memo';
  InitFileBtn.Hint := 'initialisiere die UrlCombo ganz normal von Datei (plus Standardeinträge)';
end;

procedure TWorkspaceTab.ExportBtnText;
begin
  InitDefaultBtnText;
  SetText(WS_ShowDefaultBtn_Hint, ShowDefaultBtn.Hint);
  SetText(WS_ShowComboBtn_Hint, ShowComboBtn.Hint);
  SetText(WS_LoadBtn_Hint, LoadBtn.Hint);
  SetText(WS_SaveBtn_Hint, SaveBtn.Hint);
  SetText(WS_InitMemoBtn_Hint, InitMemoBtn.Hint);
  SetText(WS_InitFileBtn_Hint, InitFileBtn.Hint);
end;

end.
