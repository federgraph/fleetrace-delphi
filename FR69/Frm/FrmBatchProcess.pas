unit FrmBatchProcess;

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
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  RiggVar.BO.Def,
  RiggVar.DAL.Redirector,
  RiggVar.Conn.Intern;

type
  TFormBatchProcess = class(TForm)
    StartBtn: TButton;
    CancelBtn: TButton;
    InformationLabel: TLabel;
    gbProgress: TGroupBox;
    ProcessTypeLabel: TLabel;
    ProgressLabel: TLabel;
    CloseBtn: TButton;
    ClearBtn: TButton;
    RequestCombo: TComboBox;
    cbCalc: TCheckBox;
    EventNameList: TCheckListBox;
    InvertAllBtn: TButton;
    InvertSelectedBtn: TButton;
    CheckAllBtn: TButton;
    cbXml: TCheckBox;
    procedure StartBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure InvertAllBtnClick(Sender: TObject);
    procedure InvertSelectedBtnClick(Sender: TObject);
    procedure CheckAllBtnClick(Sender: TObject);
  private
    Request: string;
    RequestIndex: Integer;
    WantCalc: Boolean;
    WantXml: Boolean;
    EventData: string;
    SLEventNames: TStrings;
    SLEventData: TStrings;
    HasFinished: Boolean;
    procedure BatchReport;
    procedure BatchTest;
    procedure BatchDownload;
    function GetProcessType: string;
    function GetInformation: string;
    procedure UpdateProgress(EventName: string);
    function GetDirectoryName: string;
    function Prepare: Boolean;
    procedure DoBatchJob;
    procedure ClearInFileSystem;
    public procedure ProcessBatch;
    property ProcessType: string read GetProcessType;
    property StartInformation: string read GetInformation;
    property DirectoryName: string read GetDirectoryName;
  public
    { Public-Deklarationen }
    BatchID: Integer;
  end;

var
  FormBatchProcess: TFormBatchProcess;

const
  BatchID_Test = 1;
  BatchID_Download = 2;
  BatchID_Report = 3;

  Request_JSXML = 0;
  Request_ProxyXmlInput = 1;
  Request_ProxyXmlOutput = 2;
  Request_EventHTM = 3;
  Request_FinishReport = 4;
  Request_PointsReport = 5;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormBatchProcess.FormCreate(Sender: TObject);
begin
  HasFinished := True;
  SLEventNames := TStringList.Create;
  SLEventData := TDBStringList.Create;
  Main.InitBatchRequestItems(RequestCombo.Items);
  EventNameList.MultiSelect := True;
end;

procedure TFormBatchProcess.FormDestroy(Sender: TObject);
begin
  SLEventNames.Free;
  SLEventData.Free;
end;

procedure TFormBatchProcess.StartBtnClick(Sender: TObject);
begin
  CancelBtn.Enabled := False;
  ProcessBatch;
end;

procedure TFormBatchProcess.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBatchProcess.ClearBtnClick(Sender: TObject);
begin
  if Main.Params.UseDB then
  begin
    //todo: implement clear for BatchProcess in DB
  end
  else
    ClearInFileSystem;
end;

procedure TFormBatchProcess.ClearInFileSystem;
var
  dn: string;
  fn: string;
  Found: integer;
  SearchRec: TSearchRec;
  Attr: Integer;
  mask: string;
begin
  dn := DirectoryName;
  Attr := faAnyFile;
  mask := dn + 'FR_*.*';
  Found := System.SysUtils.FindFirst(mask, Attr, SearchRec);
  while Found = 0 do
  begin
    fn := dn + SearchRec.Name;
    DeleteFile(fn);
    Found := System.SysUtils.FindNext(SearchRec);
  end;
  System.SysUtils.FindClose(SearchRec);
end;

procedure TFormBatchProcess.CloseBtnClick(Sender: TObject);
begin
  if HasFinished then
    ModalResult := mrOK;
end;

procedure TFormBatchProcess.FormShow(Sender: TObject);
begin
  ProcessTypeLabel.Caption := ProcessType;
  InformationLabel.Caption := StartInformation;
  StartBtn.Enabled := True;
  CancelBtn.Enabled := False;
  CloseBtn.Enabled := True;
  ClearBtn.Enabled := True;
  if HasFinished then
    ProgressLabel.Caption := 'ready';

  SLEventNames.Clear;
  Main.DocManager.FillEventNameList(SLEventNames);
  EventNameList.Items.Assign(SLEventNames);

  case BatchID of
    BatchID_Test:
    begin
      RequestCombo.Text := '';
      RequestCombo.Enabled := False;
      cbCalc.Enabled := True;
      cbXml.Enabled := True;
    end;
    BatchID_Download:
    begin
      RequestCombo.Text := '';
      RequestCombo.Enabled := False;
      cbCalc.Enabled := False;
      cbXml.Enabled := False;
    end;
    BatchID_Report:
    begin
      RequestCombo.ItemIndex := 0;
      RequestCombo.Enabled := True;
      cbCalc.Enabled := False;
      cbXml.Enabled := False;
    end;
  end;
end;

procedure TFormBatchProcess.CheckAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to EventNameList.Items.Count - 1 do
    EventNameList.Checked[i] := True;
end;

procedure TFormBatchProcess.InvertSelectedBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to EventNameList.Items.Count - 1 do
    if EventNameList.Selected[i] then
      EventNameList.Checked[i] := not EventNameList.Checked[i];
end;

procedure TFormBatchProcess.InvertAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to EventNameList.Items.Count - 1 do
    EventNameList.Checked[i] := not EventNameList.Checked[i];
end;

function TFormBatchProcess.GetProcessType: string;
begin
  case BatchID of
    BatchID_Test: result := 'Batch Test';
    BatchID_Download: result := 'Batch Download';
    BatchID_Report: result := 'Batch Report';
  end;
end;

function TFormBatchProcess.GetInformation: string;
begin
  case BatchID of
    BatchID_Test: result := 'write eventdata files to ./BatchTest';
    BatchID_Download: result := 'download eventdata files to ./BatchDownload';
    BatchID_Report: result := 'create JS xml files in ./BatchReports';
  end;
end;

function TFormBatchProcess.GetDirectoryName: string;
begin
  case BatchID of
    BatchID_Test: result := 'BatchTest\';
    BatchID_Download: result := 'BatchDownload\';
    BatchID_Report: result := 'BatchReports\';
    else
      result := 'temp\';
  end;
  result := Main.FolderInfo.WorkspacePath + result;
end;

procedure TFormBatchProcess.UpdateProgress(EventName: string);
begin
  ProgressLabel.Caption := EventName;
  ProgressLabel.Update;
end;

procedure TFormBatchProcess.ProcessBatch;
begin
  //Request := 'FR.*.Request.JavaScore.XML';
  Request := RequestCombo.Text;
  RequestIndex := RequestCombo.Items.IndexOf(Request);
  WantCalc := cbCalc.Checked;
  WantXml := cbXml.Checked;

  HasFinished := False;
  CancelBtn.Enabled := True;
  CloseBtn.Enabled := False;
  ClearBtn.Enabled := False;
  ProgressLabel.Caption := 'started...';
  if Prepare then
  begin
    DoBatchJob;
    ProgressLabel.Caption := 'finished.';
  end;
  HasFinished := True;
  CancelBtn.Enabled := False;
  CloseBtn.Enabled := True;
  ClearBtn.Enabled := True;
end;

function TFormBatchProcess.Prepare: Boolean;
var
  i: Integer;
begin
  result := False;

  if not Main.HaveFilePermission then
  begin
    UpdateProgress('file-permission missing');
    exit;
  end;

  if not DBDirectoryExists(DirectoryName) then
  begin
    UpdateProgress('directory does not exist');
    exit;
  end;

  SLEventNames.Clear;
  for i := 0 to EventNameList.Count - 1 do
  begin
    if EventNameList.Checked[i] then
      SLEventNames.Add(EventNameList.Items[i]);
  end;
  result := SLEventNames.Count > 0;
end;

procedure TFormBatchProcess.DoBatchJob;
var
  dn: string; //directory name
  en: string; //event name
  fn: string; //file name
  i: Integer;
begin
  dn := DirectoryName;
  for i := 0 to SLEventNames.Count - 1 do
  begin
    SLEventData.Clear;
    en := SLEventNames[i];
    UpdateProgress(en);
    fn := dn + 'FR_' + en;
    case BatchID of
      BatchID_Test:     fn := fn + '_FRData.txt';
      BatchID_Download: fn := fn + '_FRData.txt';
      BatchID_Report:
      begin
        if RequestIndex = Request_JSXML then
          fn := dn + en + '.regatta'
        else if RequestIndex = Request_ProxyXmlInput then
          fn := fn + '_ProxyXmlInput.xml'
        else if RequestIndex = Request_ProxyXmlOutput then
          fn := fn + '_ProxyXmlOutput.xml'
        else if RequestIndex = Request_EventHTM then
          fn := fn + '.htm'
        else if RequestIndex = Request_FinishReport then
          fn := fn + '_F.htm'
        else if RequestIndex = Request_PointsReport then
          fn := fn + '_P.htm'
        else
          fn := fn + '.txt';
      end;
    end;
    EventData := Main.DocManager.DownloadByName(en);
    if (EventData <> '') then
    begin
      case BatchID of
        BatchID_Test: BatchTest;
        BatchID_Download: BatchDownload;
        BatchID_Report: BatchReport;
      end;
      SLEventData.SaveToFile(fn);
    end;
  end;
end;

procedure TFormBatchProcess.BatchTest;
begin
  BO.LoadNew(EventData);
  if WantCalc then
    BO.Calc; //only for Uniqua.Gezeitet
  if WantXml then
    BO.BackupToXML(SLEventData)
  else
    BO.BackupToText(SLEventData);
end;

procedure TFormBatchProcess.BatchDownload;
begin
   SLEventData.Text := EventData;
end;

procedure TFormBatchProcess.BatchReport;
var
  tempInputConnection: TConnection;
begin
  BO.LoadNew(EventData);
  //BO.OnIdle;
  tempInputConnection := BO.InputServer.Server.Connect('BatchProcess.Input.LocalVar');
  SLEventData.Text := tempInputConnection.HandleMsg(Request);
  tempInputConnection.Free;
end;

end.
