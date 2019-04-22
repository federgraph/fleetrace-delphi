unit FmMenu;

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
  System.Math,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  RiggVar.Util.Classes,
  RiggVar.EM.Intf,
  RiggVar.EM.Combo,
  RiggVar.EM.TestData,
  RiggVar.EM.EventData,
  RiggVar.EM.Connection,
  RiggVar.EM.ConFile,
  RiggVar.EM.ConHttp,
  RiggVar.EM.ConApp,
  RiggVar.EM.SpeedBtn,
  RiggVar.EM.WorkspaceListBase,
  RiggVar.EM.CategoryCache;

type
  TTestMode = (tmError, tmTxt, tmXml, tmLive);

  TMenuTab = class(TFrame)
    ComboPanel: TPanel;
    BackgroundPanel: TPanel;
    TestBtnPanel: TPanel;
    GetBtn: TSpeedButton;
    UrlCombo: TComboBox;
    CategoryCombo: TComboBox;
    DebugModeBtn: TSpeedButton;
    PostModeBtn: TSpeedButton;
    TextBtn: TSpeedButton;
    XmlBtn: TSpeedButton;
    TransformBtn: TSpeedButton;
    DownloadBtn: TSpeedButton;
    SkipDownloadBtn: TSpeedButton;
    SkipImportBtn: TSpeedButton;
    ConvertBtn: TSpeedButton;
    WriteBtn: TSpeedButton;
    UrlBtn: TSpeedButton;
    CurrentWorkspacePanel: TPanel;
    CurrentWorkspaceLabel: TLabel;
    SelectedWorkspacePanel: TPanel;
    SelectedWorkspaceLabel: TLabel;
    ReadBtn: TSpeedButton;
    procedure GetBtnClick(Sender: TObject);
    procedure CategoryComboChange(Sender: TObject);
    procedure TextBtnClick(Sender: TObject);
    procedure XmlBtnClick(Sender: TObject);
    procedure TransformBtnClick(Sender: TObject);
    procedure DownloadBtnClick(Sender: TObject);
    procedure PostModeBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
    procedure DebugModeBtnClick(Sender: TObject);
    procedure WriteBtnClick(Sender: TObject);
    procedure UrlBtnClick(Sender: TObject);
    procedure UrlComboChange(Sender: TObject);
    procedure ReadBtnClick(Sender: TObject);
  private
    CurrentEventIndex: Integer;
    CurrentWorkspaceUrl: string;
    FPostMode: Boolean;
    TestMode: TTestMode;
    TestData: TEventMenuTestData;
    FUpdatingButtons: Boolean;
    BtnCount: Integer;
    EventCombo: TEventCombo;
    EventData: TEventData;
    HttpCon: THttpCon;
    FileCon: TFileCon;
    AppCon: TAppCon;
    FDebugMode: Boolean;
    NormalHeight: Integer;
    DebugHeight: Integer;
    procedure BtnClick(Sender: TObject);
    function DownloadEventMenu: string;
    procedure InitCategoryCombo;
    procedure GetEventMenuXml;
    function Download(const url: string): string;
    function Upload(const url, ed: string): string;
    function IsHttpScheme(const url: string): Boolean;
    function IsFileScheme(const url: string): Boolean;
    function IsAppScheme(const url: string): Boolean;
    procedure SetUpdatingButtons(const Value: Boolean);
    procedure SetPostMode(const Value: Boolean);
    procedure GetEventData(i: Integer);
    procedure PostEventData(i: Integer);
    procedure ShowMemo;
    procedure EnsureMemo;
    procedure ShowEvent;
    function GetScheme(const url: string): TUrlScheme;
    function GetCon(const url: string): TEventMenuConnection;
    procedure SetDebugMode(const Value: Boolean);
    function GetSelectedUrl: string;
    function GetIsWritableUrl: Boolean;
    function GetCurrentUrl: string;
    procedure ShowCurrentWorkspace;
    procedure ShowSelectedWorkspace;
    function GetWorkspaceList: TWorkspaceListBase;
    procedure Reset;
    procedure InitDefaultBtnText;
    procedure InitAlternativeBtnText;
    procedure InitPrimaryBtnText;
  protected
    LayoutType: Integer;
    procedure CreateBar;
    procedure InitBar;
    procedure InitItems;
    procedure UpdateItems;
    procedure ClearItems;
    procedure InitDebugSettings;
    procedure UrlInfo1(ASL: TStrings);
    procedure UrlInfo2(ASL: TStrings);
    procedure UrlInfo3(ASL: TStrings);
  public
    Memo: TStrings;
    TestMemo: TStrings;
    ItemLayout: TItemLayout;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExportBtnText;
    procedure InitBtnText;

    procedure Batch;

    procedure StartProfileRun(s: string);
    procedure RecordProfileStep(s: string);
    procedure ShowProfile;

    procedure InitUrlCombo;

    property UpdatingButtons: Boolean read FUpdatingButtons write
      SetUpdatingButtons;

    property PostMode: Boolean read FPostMode write SetPostMode;
    property DebugMode: Boolean read FDebugMode write SetDebugMode;
    property CurrentUrl: string read GetCurrentUrl;
    property SelectedUrl: string read GetSelectedUrl;
    property isWritableUrl: Boolean read GetIsWritableUrl;
    property WorkspaceList: TWorkspaceListBase read GetWorkspaceList;
  end;

implementation

{$r *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.App.Translation,
  RiggVar.BO.Def,
  RiggVar.BO.MsgToken,
  FrmMain,
  RiggVar.EM.ConHttp01,
  RiggVar.EM.SpeedBtnMgr;

constructor TMenuTab.Create(AOwner: TComponent);
begin
  inherited;

  BtnCount := 12;
  EventCombo := TEventCombo.Create;

  HttpCon := THttpCon01.Create;
  FileCon := TFileCon.Create;
  AppCon := TAppCon.Create;
  TestData := TEventMenuTestData.Create;
  EventData := TEventData.Create;

  NormalHeight := Height;
  DebugHeight := NormalHeight - TestBtnPanel.Height;

  LayoutType := 3;
  CreateBar;
  InitBar;
  InitItems;

  InitUrlCombo;

  ComboPanel.BevelOuter := bvNone;
  SelectedWorkspacePanel.BevelOuter := bvNone;
  BackgroundPanel.BevelOuter := bvNone;
  CurrentWorkspacePanel.BevelOuter := bvNone;
  TestBtnPanel.BevelOuter := bvNone;

  //InitDebugSettings;
  PostModeBtn.Visible := False;
  DebugModeBtn.Enabled := Main.HaveAppPermission;
  WriteBtn.Enabled := False; //Main.HaveAppPermission;
  ReadBtn.Enabled := Main.HaveAppPermission;

  InitBtnText;
end;

procedure TMenuTab.Reset;
begin
  CurrentEventIndex := -1;
  CurrentWorkspaceUrl := '';
  WriteBtn.Enabled := False;
  PostMode := False;
  DebugMode := False;
  UrlCombo.Clear;
end;

procedure TMenuTab.InitDebugSettings;
begin
  DebugMode := True;
  UrlCombo.ItemIndex := 3;
  DownloadBtnClick(nil);
end;

procedure TMenuTab.DebugModeBtnClick(Sender: TObject);
begin
  DebugMode := not DebugMode;
end;

destructor TMenuTab.Destroy;
begin
  ItemLayout.Free;
  FileCon.Free;
  HttpCon.Free;
  AppCon.Free;
  TestData.Free;
  EventCombo.Free;
  EventData.Free;
  inherited;
end;

procedure TMenuTab.ClearItems;
begin
  ItemLayout.ClearItems;
end;

procedure TMenuTab.InitUrlCombo;
begin
  Reset;
  WorkspaceList.FillCombo(UrlCombo.Items);
  if UrlCombo.Items.Count > 0 then
    UrlCombo.ItemIndex := 0;
end;

procedure TMenuTab.CreateBar;
begin
  ItemLayout := CreateItemLayout(LayoutType);
  ItemLayout.BtnCount := BtnCount;
  ItemLayout.BtnClick := BtnClick;
  ItemLayout.EventCombo := EventCombo;
  ItemLayout.CreateBar(BackgroundPanel);
end;

procedure TMenuTab.InitBar;
begin
  ItemLayout.InitBar;
end;

procedure TMenuTab.InitItems;
begin
  EnsureMemo;
  TestMemo.Clear;
  ItemLayout.InitItems;
end;

procedure TMenuTab.UpdateItems;
  begin
  UpdatingButtons := true;
  ItemLayout.UpdateItems;
  UpdatingButtons := false;
  Main.CategoryCache.CategoryName := CategoryCombo.Text;
  Main.CategoryCache.Init(EventCombo);
end;

procedure TMenuTab.GetEventMenuXml;
var
  em: string;
begin
  em := DownloadEventMenu;
  RecordProfileStep('Xml Download completed');
  EventCombo.Load(em);
  RecordProfileStep('Loading Text completed');
  UpdatingButtons := true;
  try
    InitCategoryCombo;
    RecordProfileStep('InitWorkspaceCombo completed');
  finally
    UpdatingButtons := false;
  end;
  UpdateItems;
  RecordProfileStep('Update Buttons completed');
end;

procedure TMenuTab.InitCategoryCombo;
var
  i: Integer;
  mc: IEventMenu;
begin
  PostMode := false;
  PostModeBtn.Enabled := IsFileScheme(SelectedUrl);

  CategoryCombo.ItemIndex := -1;
  CategoryCombo.Clear;
  for i := 0 to EventCombo.MenuCollection.Count - 1 do
  begin
    mc := EventCombo.MenuCollection.NodeAt(i);
    CategoryCombo.Items.Add(mc.ComboCaption);
  end;
  CategoryCombo.ItemIndex := 0;

  WriteBtn.Enabled := IsWritableUrl;
end;

procedure TMenuTab.CategoryComboChange(Sender: TObject);
begin
  if not UpdatingButtons then
  begin
    EventCombo.MenuIndex := CategoryCombo.ItemIndex;
    UpdateItems;
  end;
end;

procedure TMenuTab.BtnClick(Sender: TObject);
var
  i: Integer;
begin
  CurrentEventIndex := -1;

  StartProfileRun('Activity started: EventButton');
  if (SkipDownloadBtn.Down) or (SkipImportBtn.Down) then
    ShowMemo
  else
    ShowEvent;

  i := ItemLayout.GetItemTag(Sender);
  TestMemo.Clear;
  TestMemo.Add(ItemLayout.GetItemCaption(Sender));
  TestMemo.Add('Tag = ' + IntToStr(i));
  TestMemo.Add('Caption = ' + EventCombo.GetCaption(i));
  TestMemo.Add('DataUrl = ' + EventCombo.GetDataUrl(i));
  TestMemo.Add('ImgUrl = ' + EventCombo.GetImageUrl(i));

  RecordProfileStep('TestMemo updated');

  if PostMode then
    PostEventData(i)
  else
    GetEventData(i);

  CurrentEventIndex := i;
  RecordProfileStep('Activity completed');
  ShowProfile;
end;

procedure TMenuTab.PostEventData(i: Integer);
begin
  if PostModeBtn.Enabled then
  begin
    Upload(EventCombo.GetDataUrl(i), BO.ToString);
  end;
  PostMode := false;
end;

procedure TMenuTab.GetEventData(i: Integer);
var
  en: string;
  ed: string;
  fn: string;
begin
  if SkipDownloadBtn.Down then
  begin
    Memo.Text := 'downloading data skipped';
  end
  else
    try
      en := EventCombo.GetCaption(i);
      fn := EventCombo.GetDataUrl(i);

      if EventCombo.IsMock then
      begin
        ed := 'download skipped (Error/Timeout before or url not selected)'
      end
      else
      begin
        ed := Download(fn);
        RecordProfileStep('Download completed');
      end;

      if not SkipImportBtn.Down then
      begin
        EventData.Load(ed);
        RecordProfileStep('Load completed');
        if EventData.IsOK then
        begin
          Main.DocManager.EventName := en;
          Main.GuiManager.SwapEvent(EventData.Text);
          RecordProfileStep('SwapEvent completed');
          Main.CategoryCache.Update(i, EventData.DataFormat);
        end;
      end
      else
      begin
        Memo.Text := ed;
        RecordProfileStep('Memo.Text assigned');
      end;
    except
      on e: EEventMenuException do
      begin
        RecordProfileStep('EventData Download failed');
        TestMemo.Text := DateTimeToStr(Now) + ' : ' + e.Message;
        ShowMemo;
      end;
      on e: Exception do
      begin
        Memo.Text := e.Message;
        RecordProfileStep('Exception logged');
      end;
    end;
end;

procedure TMenuTab.GetBtnClick(Sender: TObject);
begin
  CurrentEventIndex := -1;
  if Assigned(Memo) then
  begin
    StartProfileRun('Activity started: EventMenu Get');
    Memo.Clear;
    TestMemo.Clear;
    TestMode := tmLive;
    try
      GetEventMenuXml;
      CurrentWorkspaceUrl := SelectedUrl;
      ShowCurrentWorkspace;
      RecordProfileStep('Activity completed');
      Main.CategoryCache.UserName := cAppTitle;
      Main.CategoryCache.WorkspaceName := UrlCombo.Text;
      Main.CategoryCache.WorkspaceUrl := CurrentWorkspaceUrl;
    except
      on e: EEventMenuException do
      begin
        RecordProfileStep('EventMenu Download failed');
        TestMemo.Text := DateTimeToStr(Now) + ' : ' + e.Message;
        ShowMemo;
      end;
    end;
    ShowProfile;
  end;
end;

procedure TMenuTab.TextBtnClick(Sender: TObject);
begin
  ShowMemo;
  TestMemo.Clear;
  TestMode := tmTxt;
  Memo.Text := DownloadEventMenu;
end;

procedure TMenuTab.XmlBtnClick(Sender: TObject);
begin
  ShowMemo;
  TestMemo.Clear;
  TestMode := tmXml;
  Memo.Text := DownloadEventMenu;
end;

procedure TMenuTab.DownloadBtnClick(Sender: TObject);
var
  s: string;
begin
  ShowMemo;
  TestMemo.Clear;
  TestMode := tmLive;
  s := DownloadEventMenu;
  Memo.Text := s;
end;

procedure TMenuTab.TransformBtnClick(Sender: TObject);
begin
  ShowMemo;
  TestMemo.Clear;
  Memo.Text := TestData.Transform(Memo.Text);
end;

procedure TMenuTab.ConvertBtnClick(Sender: TObject);
begin
  ShowMemo;
  TestMemo.Clear;
  EventData.Load(Memo.Text);
  Memo.Text := EventData.Text;
end;

function TMenuTab.DownloadEventMenu: string;
begin
  case TestMode of
    tmTxt:
      result := TestData.Txt;
    tmXml:
      result := TestData.Xml;
    tmLive:
      result := Download(SelectedUrl);
    tmError:
      result := '<EventMenu />';
  else
    result := '';
  end;
end;

function TMenuTab.GetScheme(const url: string): TUrlScheme;
begin
  result := usFile;
  if IsHttpScheme(url) then
    result := usHttp
  else if IsAppScheme(url) then
    result := usApp;
end;

function TMenuTab.GetSelectedUrl: string;
var
  i: Integer;
begin
  i := UrlCombo.ItemIndex;
  result := WorkspaceList.GetUrl(i);
end;

function TMenuTab.GetWorkspaceList: TWorkspaceListBase;
begin
  result := Main.WorkspaceList;
end;

function TMenuTab.GetIsWritableUrl: Boolean;
var
  i: Integer;
begin
  i := UrlCombo.ItemIndex;
  result := WorkspaceList.IsWritable(i);
end;

function TMenuTab.IsFileScheme(const url: string): Boolean;
begin
  result := true;
  if IsHttpScheme(url) then
    result := false;
  if IsAppScheme(url) then
    result := false;
end;

function TMenuTab.IsHttpScheme(const url: string): Boolean;
begin
  result := TUtils.StartsWith(url, 'http');
end;

function TMenuTab.IsAppScheme(const url: string): Boolean;
begin
  result := TUtils.StartsWith(url, 'app');
end;

procedure TMenuTab.SetUpdatingButtons(const Value: Boolean);
begin
  if Value <> FUpdatingButtons then
  begin
    FUpdatingButtons := Value;
    ItemLayout.Enabled := not Value;
  end;
end;

procedure TMenuTab.SetDebugMode(const Value: Boolean);
begin
  FDebugMode := Value;
  if Value then
  begin
    SelectedWorkspacePanel.Visible := true;
    CurrentWorkspacePanel.Visible := true;
    TestBtnPanel.Visible := true;
    SkipImportBtn.Down := true;
    DebugModeBtn.Caption := 'Less';
  end
  else
  begin
    SelectedWorkspacePanel.Visible := false;
    CurrentWorkspacePanel.Visible := false;
    TestBtnPanel.Visible := false;
    SkipDownloadBtn.Down := false;
    SkipImportBtn.Down := false;
    DebugModeBtn.Caption := 'More';
  end;
end;

procedure TMenuTab.SetPostMode(const Value: Boolean);
begin
  FPostMode := Value;
  if FPostMode then
  begin
    TestBtnPanel.Color := clRed;
    PostModeBtn.Caption := 'Post';
  end
  else
  begin
    TestBtnPanel.Color := clSkyBlue;
    PostModeBtn.Caption := 'Get';
  end;
end;

procedure TMenuTab.PostModeBtnClick(Sender: TObject);
begin
  PostMode := not PostMode;
end;

function TMenuTab.Download(const url: string): string;
var
  c: TEventMenuConnection;
begin
  result := '';
  c := GetCon(url);
  if Assigned(c) then
  begin
    c.Url := url;
    result := c.Get;
  end;
end;

function TMenuTab.Upload(const url, ed: string): string;
var
  c: TEventMenuConnection;
begin
  result := '';
  c := GetCon(url);
  if Assigned(c) then
  begin
    c.Url := url;
    c.Post(ed);
  end;
end;

function TMenuTab.GetCon(const url: string): TEventMenuConnection;
begin
  result := FileCon;
  case GetScheme(url) of
    usHttp: result := HttpCon;
    usFile: result := FileCon;
    usApp: result := AppCon;
  end;
end;

function TMenuTab.GetCurrentUrl: string;
begin
  EventCombo.GetDataUrl(CurrentEventIndex);
end;

procedure TMenuTab.ShowEvent;
begin
  //FormFR62.PageControl.ActivePage := FormFR62.tsEvent;
end;

procedure TMenuTab.ShowMemo;
begin
  FormFR62.ShowMemo;
end;

procedure TMenuTab.EnsureMemo;
begin
  if not Assigned(TestMemo) then
    TestMemo := FormFR62.TestMemo;
  if not Assigned(Memo) then
    Memo := FormFR62.StatusMemo;
end;

procedure TMenuTab.StartProfileRun(s: string);
begin

end;

procedure TMenuTab.RecordProfileStep(s: string);
begin

end;

procedure TMenuTab.ShowProfile;
begin

end;

procedure TMenuTab.ReadBtnClick(Sender: TObject);
var
  ed: string;
begin
  try
    StartProfileRun('Load EventData from Memo');
    ed := Memo.Text;
    EventData.Load(ed);
    RecordProfileStep('Load from Memo completed');
    if EventData.IsOK then
    begin
      //Main.DocManager.EventName := en;
      Main.GuiManager.SwapEvent(EventData.Text);
      RecordProfileStep('SwapEvent completed');
      Main.CategoryCache.Update(CurrentEventIndex, EventData.DataFormat);
    end;
  except
    on e: Exception do
    begin
      Memo.Text := e.Message;
      RecordProfileStep('Exception logged');
    end;
  end;
end;

procedure TMenuTab.WriteBtnClick(Sender: TObject);
var
  i: Integer;
  u: string;
  s: string;
  n: string;
begin
  i := CurrentEventIndex;
  if i > -1 then
  begin
    u := EventCombo.GetDataUrl(i);

    TestMemo.Clear;
    TestMemo.Add('DataUrl = ' + u);

    n := LowerCase(WorkspaceList.GetName(i));
    if Pos('htm', n) > 0 then
      s := BO.ToHtml
    else if Pos('xml', n) > 0 then
      s := BO.ToXml
    else if Pos('txt', n) > 0 then
      s := BO.ToTxt
    else if IsFileScheme(SelectedUrl) then
    begin
      case WorkspaceList.GetWriteFormat(u) of
        ffTxt: s := BO.ToTxt;
        ffXml: s := BO.ToXml;
        ffHtml: s := BO.ToHtml;
        else
          s := BO.ToString;
      end;
    end
    else
    begin
      s := BO.ToHtml;
    end;

    Memo.Text := s;

    Upload(u, s);
  end
  else
  begin
    TestMemo.Add('CurrentEventIndex = -1, cannot post data.');
    Memo.Text := '';
  end;
end;

procedure TMenuTab.UrlBtnClick(Sender: TObject);
begin
  ShowMemo;
  TestMemo.Clear;

  UrlInfo1(TestMemo);
end;

procedure TMenuTab.UrlComboChange(Sender: TObject);
begin
  ShowSelectedWorkspace;
end;

procedure TMenuTab.UrlInfo1(ASL: TStrings);
var
  fs1, fs2, fs3, fs4: string;
begin
  {
  Selected ComboEntry (WorkspaceMenuUrl): 14 - RN05
    http://www.fleetrace.org/DemoIndex.xml
  Current Button (EventDataUrl) = 2 - FleetTest
    http://www.fleetrace.org/Demo/Test/NameTest.xml
  }

  fs1 := 'Selected ComboEntry (EventMenu): %d (%s)';
  fs2 := '  %s';
  fs3 := 'Current Button (EventData): %d (%s)';
  fs4 := '  %s';

  ASL.Add(Format(fs1, [UrlCombo.ItemIndex, UrlCombo.Text]));
  ASL.Add(Format(fs2, [SelectedUrl]));
  ASL.Add(Format(fs3, [CurrentEventIndex, EventCombo.GetCaption(CurrentEventIndex)]));
  ASL.Add(Format(fs4, [EventCombo.GetDataUrl(CurrentEventIndex)]));
end;

procedure TMenuTab.UrlInfo2(ASL: TStrings);
var
  fs1, fs2, fs3, fs4: string;

  UrlComboItemIndex: Integer;
  UrlComboText: string;
  SelectedUrlString: string;

  CurrentBtnIndex: Integer;
  CurrentBtnCaption: string;
  CurrentBtnUrl: string;
begin
  {
  Selected ComboEntry (WorkspaceMenuUrl): 14 - RN05
    http://www.fleetrace.org/DemoIndex.xml
  Current Button (EventDataUrl) = 2 - FleetTest
    http://www.fleetrace.org/Demo/Test/NameTest.xml
  }

  fs1 := 'Selected ComboEntry (EventMenu): %d (%s)';
  fs2 := '  %s';
  fs3 := 'Current Button (EventData): %d (%s)';
  fs4 := '  %s';

	UrlComboItemIndex := UrlCombo.ItemIndex;
	UrlComboText := UrlCombo.Text;
	SelectedUrlString := SelectedUrl;

	CurrentBtnIndex := CurrentEventIndex;
	CurrentBtnCaption := EventCombo.GetCaption(CurrentEventIndex);
	CurrentBtnUrl := EventCombo.GetDataUrl(CurrentEventIndex);

  fs1 := Format(fs1, [UrlComboItemIndex, UrlComboText]);
  fs2 := Format(fs2, [SelectedUrlString]);
  fs3 := Format(fs3, [CurrentBtnIndex, CurrentBtnCaption]);
  fs4 := Format(fs4, [CurrentBtnUrl]);

  ASL.Add(fs1);
  ASL.Add(fs2);
  ASL.Add(fs3);
  ASL.Add(fs4);
end;

procedure TMenuTab.UrlInfo3(ASL: TStrings);
var
  i: Integer;
  s: string;
begin
  i := UrlCombo.ItemIndex;
  ASL.Add('Selected WorkspaceUrl Index = ' + IntToStr(i));
  s := '';
  if i > -1 then
    s := SelectedUrl;
  ASL.Add('Selected WorkspaceUrl = ' + s);

  i := CurrentEventIndex;
  ASL.Add('Current EventIndex = ' + IntToStr(i));
  s := '';
  if i > -1 then
    s := EventCombo.GetDataUrl(i);
  ASL.Add('Current DataUrl = ' + s);
end;

procedure TMenuTab.ShowCurrentWorkspace;
begin
  CurrentWorkspaceLabel.Caption := 'current workspace: ' + CurrentWorkspaceUrl;
end;

procedure TMenuTab.ShowSelectedWorkspace;
begin
  SelectedWorkspaceLabel.Caption := 'selected workspace: ' + SelectedUrl;
end;

procedure TMenuTab.Batch;
var
  o: Integer;
  i: Integer;
begin
  o := CurrentEventIndex;
  for i := 1 to EventCombo.Count do
  begin
    GetEventData(i);
  end;
  GetEventData(o);
  Main.CategoryCache.Save;
end;

procedure TMenuTab.InitBtnText;
begin
  if Main.Params.WantLocalText then
  begin
    UrlCombo.Hint := GetText(EM_UrlCombo_Hint);
    CategoryCombo.Hint := GetText(EM_CategoryCombo_Hint);
    GetBtn.Hint := GetText(EM_GetBtn_Hint);
    DebugModeBtn.Hint := GetText(EM_DebugModeBtn_Hint);
    TextBtn.Hint := GetText(EM_TextBtn_Hint);
    XmlBtn.Hint := GetText(EM_XmlBtn_Hint);
    DownloadBtn.Hint := GetText(EM_DownloadBtn_Hint);
    TransformBtn.Hint := GetText(EM_TransformBtn_Hint);
    ConvertBtn.Hint := GetText(EM_ConvertBtn_Hint);

    SkipDownloadBtn.Hint := GetText(EM_SkipDownloadBtn_Hint);
    SkipImportBtn.Hint := GetText(EM_SkipImportBtn_Hint);

    PostModeBtn.Hint := GetText(EM_PostModeBtn_Hint);
    UrlBtn.Hint := GetText(EM_UrlInfoBtn_Hint);
    ReadBtn.Hint := GetText(EM_ReadBtn_Hint);
    WriteBtn.Hint := GetText(EM_WriteBtn_Hint);
  end
  else
    InitDefaultBtnText;
end;

procedure TMenuTab.InitDefaultBtnText;
begin
  if Main.Params.WantAlternativeText then
    InitAlternativeBtnText
  else
    InitPrimaryBtnText;
end;

procedure TMenuTab.InitPrimaryBtnText;
begin
  UrlCombo.Hint := 'UrlCombo for selection of workspace';
  CategoryCombo.Hint := 'CategoryCombo for selection of category (group of events within workspace)';

  GetBtn.Hint := 'read contents file with category/button info';
  DebugModeBtn.Hint := 'show More or Less buttons (debug mode)';
  TextBtn.Hint := 'show Example of EventMenu.xml (transformed to text)';
  XmlBtn.Hint := 'show Example of EventMenu.xml in Memo';
  DownloadBtn.Hint := 'download EventMenu.xml using UrlCombo.Text';
  TransformBtn.Hint := 'transform EventMenu.xml (from Memo)';
  ConvertBtn.Hint := 'convert EventData.* (from Memo)';

  SkipDownloadBtn.Hint := 'when checked, show url for download in TestMemo only';
  SkipImportBtn.Hint := 'when checked, download EventData but do not load into application';

  PostModeBtn.Hint := 'toggle EventData-Button-Modus (between post and get)';
  UrlBtn.Hint := 'show Info for selected Url and current Event';
  ReadBtn.Hint := 'import EventData from Memo';
  WriteBtn.Hint := 'write EventData back to Url';
end;

procedure TMenuTab.InitAlternativeBtnText;
begin
  UrlCombo.Hint := 'UrlCombo für die Auswahl des Workspace';
  CategoryCombo.Hint := 'CategoryCombo für die Auswahl der Kategorie (Gruppe von Events im Workspace)';

  GetBtn.Hint := 'Inhaltsverzeichnis lesen mit Kategorie/Button Info';
  DebugModeBtn.Hint := 'Mehr oder Weniger Schaltflächen anzeigen (Debugmodus)';
  TextBtn.Hint := 'Beispiel für EventMenu.xml anzeigen (transformiert zu Text)';
  XmlBtn.Hint := 'Beispiel für EventMenu.xml in Memo anzeigen';
  DownloadBtn.Hint := 'EventMenu.xml herunterladen von UrlCombo.Text';
  TransformBtn.Hint := 'EventMenu.xml transformieren (vom Inhalt des Memos)';
  ConvertBtn.Hint := 'EventData.* konvertieren (vom Inhalt des Memos)';

  SkipDownloadBtn.Hint := 'wenn aktiv: zeige die Download-Url lediglich im Memo an, kein Download!';
  SkipImportBtn.Hint := 'wenn aktiv: lade die EventDaten herunter, verarbeite diese aber nicht!';

  PostModeBtn.Hint := 'EventData-Button-Modus umschalten (zwischen Post und Get)';
  UrlBtn.Hint := 'Info für ausgewählte Url und aktiven Event anzeigen';
  ReadBtn.Hint := 'EventData vom Memo einlesen';
  WriteBtn.Hint := 'EventData zur Url zurückschreiben';
end;

procedure TMenuTab.ExportBtnText;
begin
  InitDefaultBtnText;

  SetText(EM_UrlCombo_Hint, UrlCombo.Hint);
  SetText(EM_CategoryCombo_Hint, CategoryCombo.Hint);
  SetText(EM_GetBtn_Hint, GetBtn.Hint);
  SetText(EM_DebugModeBtn_Hint, DebugModeBtn.Hint);
  SetText(EM_TextBtn_Hint, TextBtn.Hint);
  SetText(EM_XmlBtn_Hint, XmlBtn.Hint);
  SetText(EM_DownloadBtn_Hint, DownloadBtn.Hint);
  SetText(EM_TransformBtn_Hint, TransformBtn.Hint);
  SetText(EM_ConvertBtn_Hint, ConvertBtn.Hint);
  SetText(EM_SkipDownloadBtn_Hint, SkipDownloadBtn.Hint);
  SetText(EM_SkipImportBtn_Hint, SkipImportBtn.Hint);
  SetText(EM_PostModeBtn_Hint, PostModeBtn.Hint);
  SetText(EM_UrlInfoBtn_Hint, UrlBtn.Hint);
  SetText(EM_ReadBtn_Hint, ReadBtn.Hint);
  SetText(EM_WriteBtn_Hint, WriteBtn.Hint);
end;

end.
