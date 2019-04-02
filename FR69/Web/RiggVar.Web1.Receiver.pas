unit RiggVar.Web1.Receiver;

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
  TWebReceiverBase = class
  protected
    SL: TStringList;
    MsgCounterIn: Integer;
    MsgCounterOut: Integer;
    procedure AddOp(op: Integer);
    procedure AddStr(s: string);
    procedure AddInt(i: Integer);
    procedure AddBool(b: Boolean);
    function PopInt: Integer;
    function PopStr: string;
    function PopBool: Boolean;
    function DispatchMsg(Action: Integer): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Receive(msg: string): string;
  end;

  TWebReceiver = class(TWebReceiverBase)
  private
    WebStub: TWebStub;
  protected
    FEventName: string;

    FRaceCount: Integer;
    FITCount: Integer;
    FStartlistCount: Integer;

    FRace: Integer;
    FIT: Integer;
    FBib: Integer;

    FUseFleets: Boolean;
    FTargetFleetSize: Integer;
    FFirstFinalRace: Integer;

    FWorkspaceType: Integer;
    FWorkspaceID: Integer;

    FRequest: string;

    FReportID: Integer;
    FSortColumn: Integer;
    FFlag: Boolean;

    function DispatchMsg(Action: Integer): string; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TWebReceiver }

constructor TWebReceiver.Create;
begin
  inherited Create;
  WebStub := TWebStub.Create;
end;

destructor TWebReceiver.Destroy;
begin
  WebStub.Free;
  inherited;
end;

function TWebReceiver.DispatchMsg(Action: Integer): string;
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

    Action_Save:
    begin
      WebStub.SaveEventExecute;
      result := 'Save OK';
    end;

    Action_SaveAs:
    begin
      FEventName := PopStr;
      WebStub.SaveEventAsExecute(FEventName);
      result := 'SaveAs OK: ' + FEventName;
    end;

    Action_Delete:
    begin
      FEventName := PopStr;
      WebStub.DeleteEventExecute(FEventName);
      result := 'Delete OK: ' + FEventName;
    end;

    Action_Connect:
    begin
      WebStub.ConnectExecute;
      result := 'Connect OK';
    end;

    Action_Disconnect:
    begin
      WebStub.DisconnectExecute;
      result := 'Disconnect OK';
    end;

    Action_Backup:
    begin
      WebStub.BackupExecute;
      result := 'Backup OK';
    end;

    Action_Recreate:
    begin
      WebStub.RecreateExecute;
      result := 'Recreate OK';
    end;

    Action_Clear:
    begin
      WebStub.ClearExecute;
      result := 'Clear OK';
    end;

    Action_Plugin:
    begin
      WebStub.PluginExecute;
      result := 'Plugin OK';
    end;

    Action_Plugout:
    begin
      WebStub.PlugoutExecute;
      result := 'Plugout OK';
    end;

    Action_Synchronize:
    begin
      WebStub.SynchronizeExecute;
      result := 'Synchronize OK';
    end;

    Action_Upload:
    begin
      WebStub.UploadExecute;
      result := 'Upload OK';
    end;

    Action_Download:
    begin
      WebStub.DownloadExecute;
      result := 'Download OK';
    end;

    Action_Undo:
    begin
      WebStub.UndoExecute;
      result := 'Undo OK';
    end;

    Action_Redo:
    begin
      WebStub.RedoExecute;
      result := 'Redo OK';
    end;

    Action_LoadTestData:
    begin
      WebStub.LoadTestDataExecute;
      result := 'LoadTestData OK';
    end;

    Action_SynchronizeCache:
    begin
      WebStub.SynchronizeCacheExecute;
      result := 'SynchronizeCache OK';
    end;

    Action_StrictMode:
    begin
      WebStub.StrictModeExecute;
      result := 'StrictMode OK';
    end;

    Action_RelaxedMode:
    begin
      WebStub.RelaxedModeExecute;
      result := 'RelaxedMode OK';
    end;

    Action_ColorCycle:
    begin
      WebStub.ColorCycleExecute;
      result := 'ColorCycle OK';
    end;

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

    Action_ToTXT: result := WebStub.ToTXT;
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
      result := 'UpdateWorkspace OK';
    end;

    Action_UpdateEventParams:
    begin
      FRaceCount := PopInt;
      FITCount := PopInt;
      FStartlistCount := PopInt;
      WebStub.UpdateEventParams(FRaceCount, FITCount, FStartlistCount);
      result := 'UpdateEventParams OK';
    end;

    Action_UpdateFleetProps:
    begin
      FUseFleets := PopBool;
      FTargetFleetSize := PopInt;
      FFirstFinalRace := PopInt;
      WebStub.UpdateFleetProps(FUseFleets, FTargetFleetSize, FFirstFinalRace);
      result := 'UpdateFleetProps OK';
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

{ TWebReceiverBase }

procedure TWebReceiverBase.AddBool(b: Boolean);
begin
  SL.Add(BoolStr[b]);
end;

procedure TWebReceiverBase.AddInt(i: Integer);
begin
  SL.Add(IntToStr(i));
end;

procedure TWebReceiverBase.AddOp(op: Integer);
begin
  SL.Clear;
  AddInt(op);
end;

procedure TWebReceiverBase.AddStr(s: string);
begin
  SL.Add(s);
end;

constructor TWebReceiverBase.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TWebReceiverBase.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TWebReceiverBase.PopInt: Integer;
begin
  result := -1;
  if SL.Count > 0 then
  begin
    result := StrToIntDef(SL[0], -1);
    SL.Delete(0);
  end;
end;

function TWebReceiverBase.PopStr: string;
begin
  result := '';
  if SL.Count > 0 then
  begin
    result := SL[0];
    SL.Delete(0);
  end;
end;

function TWebReceiverBase.PopBool: Boolean;
begin
  result := False;
  if SL.Count > 0 then
  begin
    result := TUtils.IsTrue(SL[0]);
    SL.Delete(0);
  end;
end;

function TWebReceiverBase.Receive(msg: string): string;
var
  Action: Integer;
begin
  result := '';
  Inc(MsgCounterIn);
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

function TWebReceiverBase.DispatchMsg(Action: Integer): string;
begin
  //virtual
end;

end.
