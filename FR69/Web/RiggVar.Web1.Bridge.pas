unit RiggVar.Web1.Bridge;

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
  SysUtils, Classes,
  RiggVar.Util.Classes,
  RiggVar.Web1.Action,
  RiggVar.Web1.Stub;

type
  TWebBridgeBase = class
  protected
    SL: TStringList;
    MsgCounterIn: Integer;
    MsgCounterOut: Integer;
    MsgText: string;
    procedure AddOp(op: Integer);
    procedure AddStr(s: string);
    procedure AddInt(i: Integer);
    procedure AddBool(b: Boolean);
    function PopInt: Integer;
    function PopStr: string;
    function PopBool: Boolean;
    function Send(s: string): string; virtual;
    function Receive(msg: string): string;
    function DispatchMsg(Action: Integer): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TWebBridge = class(TWebBridgeBase)
  private
    WebStub: TWebStub;
  protected
    FProxyProps: TProxyProps;
    FEventName: string;
    FRace: Integer;
    FIT: Integer;
    FBib: Integer;
    FSortColumn: Integer;
    FReportID: Integer;
    FFlag: Boolean;
    FRequest: string;
    FRaceCount: Integer;
    FITCount: Integer;
    FStartlistCount: Integer;
    FWorkspaceType: Integer;
    FWorkspaceID: Integer;
    FUseFleets: Boolean;
    FTargetFleetSize: Integer;
    FFirstFinalRace: Integer;
    function DispatchMsg(Action: Integer): string; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure QueryProxyProps(Value: TProxyProps);

    procedure ActionCommand(Action: Integer);
    procedure EventCommand(Action: Integer; EventName: string);
    function ReportAction(Action: Integer): string;
    function RequestAction(Action: Integer; Request: string): string;

    function GetEventReport(ReportID: Integer; SortColumn: Integer; b: Boolean): string;
    function GetRaceReport(ReportID: Integer; Race, IT: Integer): string;
    function GetRaceXML(Race, IT: Integer): string;
    function GetXMLReport(ReportID: Integer): string;

    procedure UpdateWorkspace(WorkspaceType, WorkspaceID: Integer);
    procedure UpdateEventParams(RaceCount, ITCount, StartlistCount: Integer);
    procedure UpdateFleetProps(UseFleets: Boolean; TargetFleetSize, FirstFinalRace: Integer);

    function Handle_TW_Report(Race: Integer): string;
    function Handle_TW_Table(Race, IT, Bib: Integer): string;
    function Handle_TW_XML(Race, IT, Bib: Integer): string;
    function Handle_TW_Ajax(Race, IT, Bib: Integer): string;
  end;

implementation

{ TWebBridge }

constructor TWebBridge.Create;
begin
  inherited Create;
  FProxyProps := TProxyProps.Create;
  WebStub := TWebStub.Create;
end;

destructor TWebBridge.Destroy;
begin
  FProxyProps.Free;
  WebStub.Free;
  inherited;
end;

procedure TWebBridge.QueryProxyProps(Value: TProxyProps);
begin
  AddOp(Action_QueryProps);
  SL.Text := Send(SL.Text);
  FProxyProps.Clear;

  FProxyProps.EventName := PopStr;
  FProxyProps.RaceCount := PopInt;
  FProxyProps.ITCount := PopInt;
  FProxyProps.StartlistCount := PopInt;
  FProxyProps.Race := PopInt;
  FProxyProps.IT := PopInt;
  FProxyProps.UseFleets := PopBool;
  FProxyProps.TargetFleetSize := PopInt;
  FProxyProps.FirstFinalRace := PopInt;
  FProxyProps.WorkspaceType := PopInt;
  FProxyProps.WorkspaceID := PopInt;

  if FProxyProps.IsValid then
    Value.Assign(FProxyProps);
end;

procedure TWebBridge.ActionCommand(Action: Integer);
begin
  AddOp(Action);
  Send(SL.Text);
end;

procedure TWebBridge.EventCommand(Action: Integer; EventName: string);
begin
  AddOp(Action);
  AddStr(EventName);
  Send(SL.Text);
end;

function TWebBridge.GetEventReport(ReportID, SortColumn: Integer;
  b: Boolean): string;
begin
  AddOp(Action_GetEventReport);
  AddInt(ReportID);
  AddInt(SortColumn);
  AddBool(b);
  result := Send(SL.Text);
end;

function TWebBridge.GetRaceReport(ReportID, Race, IT: Integer): string;
begin
  AddOp(Action_GetRaceReport);
  AddInt(ReportID);
  AddInt(Race);
  AddInt(IT);
  result := Send(SL.Text);
end;

function TWebBridge.GetRaceXML(Race, IT: Integer): string;
begin
  AddOp(Action_GetRaceXML);
  AddInt(Race);
  AddInt(IT);
  result := Send(SL.Text);
end;

function TWebBridge.GetXMLReport(ReportID: Integer): string;
begin
  AddOp(Action_GetXMLReport);
  AddInt(ReportID);
  result := Send(SL.Text);
end;

function TWebBridge.ReportAction(Action: Integer): string;
begin
  AddOp(Action);
  result := Send(SL.Text);
end;

function TWebBridge.RequestAction(Action: Integer; Request: string): string;
begin
  AddOp(Action);
  AddStr(Request);
  result := Send(SL.Text);
end;

procedure TWebBridge.UpdateEventParams(RaceCount, ITCount,
  StartlistCount: Integer);
begin
  AddOp(Action_UpdateEventParams);
  AddInt(RaceCount);
  AddInt(ITCount);
  AddInt(StartlistCount);
  Send(SL.Text);
end;

procedure TWebBridge.UpdateFleetProps(UseFleets: Boolean; TargetFleetSize,
  FirstFinalRace: Integer);
begin
  AddOp(Action_UpdateFleetProps);
  AddBool(UseFleets);
  AddInt(TargetFleetSize);
  AddInt(FirstFinalRace);
  Send(SL.Text);
end;

procedure TWebBridge.UpdateWorkspace(WorkspaceType, WorkspaceID: Integer);
begin
  AddOp(Action_UpdateWorkspace);
  AddInt(WorkspaceType);
  AddInt(WorkspaceID);
  Send(SL.Text);
end;

function TWebBridge.Handle_TW_Report(Race: Integer): string;
begin
  AddOp(Action_Handle_TW_Report);
  AddInt(Race);
  result := Send(SL.Text);
end;

function TWebBridge.Handle_TW_XML(Race, IT, Bib: Integer): string;
begin
  AddOp(Action_Handle_TW_XML);
  AddInt(Race);
  AddInt(IT);
  AddInt(Bib);
  result := Send(SL.Text);
end;

function TWebBridge.Handle_TW_Table(Race, IT, Bib: Integer): string;
begin
  AddOp(Action_Handle_TW_Table);
  AddInt(Race);
  AddInt(IT);
  AddInt(Bib);
  result := Send(SL.Text);
end;

function TWebBridge.Handle_TW_Ajax(Race, IT, Bib: Integer): string;
begin
  AddOp(Action_Handle_TW_Ajax);
  AddInt(Race);
  AddInt(IT);
  AddInt(Bib);
  result := Send(SL.Text);
end;

function TWebBridge.DispatchMsg(Action: Integer): string;
begin
  case Action of
    Action_QueryProps:
    begin
      SL.Clear;
      WebStub.QueryProps(SL);
      result := SL.Text;
    end;

    Action_Open:
    begin
      FEventName := PopStr;
      WebStub.OpenEventExecute(FEventName);
    end;

    Action_Save: WebStub.SaveEventExecute;

    Action_SaveAs:
    begin
      FEventName := PopStr;
      WebStub.SaveEventAsExecute(FEventName);
    end;

    Action_Delete:
    begin
      FEventName := PopStr;
      WebStub.DeleteEventExecute(FEventName);
    end;

    Action_Connect: WebStub.ConnectExecute;
    Action_Disconnect: WebStub.DisconnectExecute;
    Action_Backup: WebStub.BackupExecute;
    Action_Recreate: WebStub.RecreateExecute;
    Action_Clear: WebStub.ClearExecute;

    Action_Plugin: WebStub.PluginExecute;
    Action_Plugout: WebStub.PlugoutExecute;
    Action_Synchronize: WebStub.SynchronizeExecute;
    Action_Upload: WebStub.UploadExecute;
    Action_Download: WebStub.DownloadExecute;

    Action_Undo: WebStub.UndoExecute;
    Action_Redo: WebStub.RedoExecute;

    Action_LoadTestData: WebStub.LoadTestDataExecute;
    Action_SynchronizeCache: WebStub.SynchronizeCacheExecute;

    Action_StrictMode: WebStub.StrictModeExecute;
    Action_RelaxedMode: WebStub.RelaxedModeExecute;
    Action_ColorCycle: WebStub.ColorCycleExecute;

    Action_FillEventNameList:
    begin
      SL.Clear;
      WebStub.FillEventNameList(SL);
      result := SL.Text;
    end;

    Action_GetReport:
    begin
      FRequest := PopStr;
      result := WebStub.GetReport(FRequest);
    end;

    Action_ToTxt: result := WebStub.ToTXT;
    Action_ToXML: result := WebStub.ToXML;

    Action_GetJavaScoreXML: result := WebStub.GetJavaScoreXML;
    Action_GetRaceDataXML: result := WebStub.GetRaceDataXML;

    Action_GetUndoManagerLog: result := WebStub.GetUndoManagerLog;
    Action_GetUndoManagerUndo: result := WebStub.GetUndoManagerUndo;
    Action_GetUndoManagerRedo: result := WebStub.GetUndoManagerRedo;
    Action_GetUndoManagerUndoRedo: result := WebStub.GetUndoManagerUndoRedo;

    //Action_GetEventName: result := WebStub.EventName;

    Action_GetStatusString: result := WebStub.GetStatusString;
    Action_WebStatusString: result := WebStub.WebStatusString;
    Action_GetConvertedEventData: result := WebStub.GetConvertedEventData;
    Action_GetIniImage: result := WebStub.GetIniImage;
    Action_GetWorkspaceInfoReport: result := WebStub.GetWorkspaceInfoReport;

    Action_FinishReport: result := WebStub.FinishReport;
    Action_PointsReport: result := WebStub.PointsReport;
    Action_TimePointReport: result := WebStub.TimePointReport;
    Action_CacheReport: result := WebStub.CacheReport;

    Action_GetEventReport:
    begin
      FReportID := PopInt;
      FSortColumn := PopInt;
      FFlag := PopBool;
      result := WebStub.GetEventReport(FReportID, FSortColumn, FFlag);
    end;

    Action_GetRaceReport:
    begin
      FReportID := PopInt;
      FRace := PopInt;
      FIT := PopInt;
      result := WebStub.GetRaceReport(FReportID, FRace, FIT);
    end;

    Action_GetRaceXML:
    begin
      FRace := PopInt;
      FIT := PopInt;
      result := WebStub.GetRaceXML(FRace, FIT);
    end;

    Action_GetXMLReport:
    begin
      FReportID := PopInt;
      result := WebStub.GetXMLReport(FReportID);
    end;

    Action_UpdateWorkspace:
    begin
      FWorkspaceType := PopInt;
      FWorkspaceID := PopInt;
      WebStub.UpdateWorkspace(FWorkspaceType, FWorkspaceID);
    end;

    Action_UpdateEventParams:
    begin
      FRaceCount := PopInt;
      FITCount := PopInt;
      FStartlistCount := PopInt;
      WebStub.UpdateEventParams(FRaceCount, FITCount, FStartlistCount);
    end;

    Action_UpdateFleetProps:
    begin
      FUseFleets := PopBool;
      FTargetFleetSize := PopInt;
      FFirstFinalRace := PopInt;
      WebStub.UpdateFleetProps(FUseFleets, FTargetFleetSize, FFirstFinalRace);
    end;

    Action_UpdateEntry:
    begin
      FRequest := PopStr;
      result := WebStub.UpdateEntry(FRequest);
    end;

    Action_UpdateRaceValue:
    begin
      FRequest := PopStr;
      result := WebStub.UpdateRaceValue(FRequest);
    end;

    Action_ExecuteCommand:
    begin
      FRequest := PopStr;
      result := WebStub.ExecuteCommand(FRequest);
    end;

    Action_Handle_TW_Report:
    begin
      FRace := PopInt;
      result := WebStub.Handle_TW_Report(FRace);
    end;

    Action_Handle_TW_XML:
    begin
      FRace := PopInt;
      FIT := PopInt;
      FBib := PopInt;
      result := WebStub.Handle_TW_XML(FRace, FIT, FBib);
    end;

    Action_Handle_TW_Table:
    begin
      FRace := PopInt;
      FIT := PopInt;
      FBib := PopInt;
      result := WebStub.Handle_TW_Table(FRace, FIT, FBib);
    end;

    Action_Handle_TW_Ajax:
    begin
      FRace := PopInt;
      FIT := PopInt;
      FBib := PopInt;
      result := WebStub.Handle_TW_Ajax(FRace, FIT, FBib);
    end;

  end;

end;

{ TWebBridgeBase }

procedure TWebBridgeBase.AddBool(b: Boolean);
begin
  SL.Add(BoolStr[b]);
end;

procedure TWebBridgeBase.AddInt(i: Integer);
begin
  SL.Add(IntToStr(i));
end;

procedure TWebBridgeBase.AddOp(op: Integer);
begin
  SL.Clear;
  AddInt(op);
end;

procedure TWebBridgeBase.AddStr(s: string);
begin
  SL.Add(s);
end;

constructor TWebBridgeBase.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TWebBridgeBase.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TWebBridgeBase.PopInt: Integer;
begin
  result := -1;
  if SL.Count > 0 then
  begin
    result := StrToIntDef(SL[0], -1);
    SL.Delete(0);
  end;
end;

function TWebBridgeBase.PopStr: string;
begin
  result := '';
  if SL.Count > 0 then
  begin
    result := SL[0];
    SL.Delete(0);
  end;
end;

function TWebBridgeBase.PopBool: Boolean;
begin
  result := False;
  if SL.Count > 0 then
  begin
    result := TUtils.IsTrue(SL[0]);
    SL.Delete(0);
  end;
end;

function TWebBridgeBase.Send(s: string): string;
begin
  result := Receive(s);
end;

function TWebBridgeBase.Receive(msg: string): string;
var
  Action: Integer;
begin
  result := '';
  Inc(MsgCounterIn);
  MsgText := msg;
  if msg <> '' then
  begin
    SL.Text := msg;
    Action := PopInt;
    if Action <> -1 then
    begin
      result := DispatchMsg(Action);
    end;
  end;
end;

function TWebBridgeBase.DispatchMsg(Action: Integer): string;
begin
  //virtual
end;

end.
