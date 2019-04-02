unit RiggVar.Web1.Debugger;

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
  Classes,
  RiggVar.Web1.Action,
  RiggVar.Web1.Bridge;

type
  TWebDebugger = class
  private
    WebBridge: TWebBridge;
    UseShortCut: Boolean;
    ProxyProps: TProxyProps;
    function GetIT: Integer;
    function GetITCount: Integer;
    function GetRace: Integer;
    function GetRaceCount: Integer;
    function GetStartlistCount: Integer;
    function GetTargetFleetSize: Integer;
    function GetUseFleets: Boolean;
    function GetFirstFinalRace: Integer;
    function GetWorkspaceID: Integer;
    function GetWorkspaceType: Integer;
    function GetEventName: string;
  protected
    procedure QueryProps;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenEventExecute(EventName: string);
    procedure SaveEventExecute;
    procedure SaveEventAsExecute(EventName: string);
    procedure DeleteEventExecute(EventName: string);

    procedure ConnectExecute;
    procedure DisconnectExecute;
    procedure BackupExecute;
    procedure RecreateExecute;
    procedure ClearExecute;

    procedure PluginExecute;
    procedure PlugoutExecute;
    procedure SynchronizeExecute;
    procedure UploadExecute;
    procedure DownloadExecute;

    procedure UndoExecute;
    procedure RedoExecute;

    procedure LoadTestDataExecute;
    procedure SynchronizeCacheExecute;

    procedure StrictModeExecute;
    procedure RelaxedModeExecute;
    procedure ColorCycleExecute;

    procedure FillEventNameList(NL: TStrings);

    function GetReport(request: string): string;

    function ToTXT: string;
    function ToXML: string;
    function GetJavaScoreXML: string;
    function GetRaceDataXML: string;

    function GetUndoManagerLog: string;
    function GetUndoManagerUndo: string;
    function GetUndoManagerRedo: string;
    function GetUndoManagerUndoRedo: string;

    function GetStatusString: string;
    function WebStatusString: string;
    function GetConvertedEventData: string;
    function GetIniImage: string;
    function GetWorkspaceInfoReport: string;

    function FinishReport: string;
    function PointsReport: string;
    function TimePointReport: string;
    function CacheReport: string;

    function GetEventReport(ReportID: Integer; SortColumn: Integer; b: Boolean): string;
    function GetRaceReport(ReportID: Integer; Race, IT: Integer): string;
    function GetRaceXML(Race, IT: Integer): string;
    function GetXMLReport(ReportID: Integer): string;

    procedure UpdateWorkspace(WorkspaceType, WorkspaceID: Integer);
    procedure UpdateEventParams(RaceCount, ITCount, StartlistCount: Integer);
    procedure UpdateFleetProps(UseFleets: Boolean; TargetFleetSize, FirstFinalRace: Integer);

    function UpdateEntry(request: string): string;
    function UpdateRaceValue(request: string): string;
    function ExecuteCommand(request: string): string;

    function Handle_TW_Report(Race: Integer): string;
    function Handle_TW_XML(Race, IT, Bib: Integer): string;
    function Handle_TW_Table(Race, IT, Bib: Integer): string;
    function Handle_TW_Ajax(Race, IT, Bib: Integer): string;

    property EventName: string read GetEventName;
    property RaceCount: Integer read GetRaceCount;
    property ITCount: Integer read GetITCount;
    property StartlistCount: Integer read GetStartlistCount;
    property UseFleets: Boolean read getUseFleets;
    property TargetFleetSize: Integer read GetTargetFleetSize;
    property FirstFinalRace: Integer read GetFirstFinalRace;
    property Race: Integer read GetRace;
    property IT: Integer read GetIT;
    property WorkspaceID: Integer read GetWorkspaceID;
    property WorkspaceType: Integer read GetWorkspaceType;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TWebDebugger }

constructor TWebDebugger.Create;
begin
  ProxyProps := TProxyProps.Create;
  UseShortCut := True;
  WebBridge := TWebBridge.Create;
end;

destructor TWebDebugger.Destroy;
begin
  ProxyProps.Free;
  WebBridge.Free;
  inherited;
end;

function TWebDebugger.CacheReport: string;
begin
  //result := Main.GuiManager.CacheMotor.Cache.GetHTM;
  result := WebBridge.ReportAction(Action_CacheReport);
end;

procedure TWebDebugger.ConnectExecute;
begin
  //BO.Connect;
  WebBridge.ActionCommand(Action_Connect);
end;

function TWebDebugger.GetConvertedEventData: string;
begin
  //result := BO.ConvertedData;
  result := WebBridge.ReportAction(Action_GetConvertedEventData);
end;

procedure TWebDebugger.DisconnectExecute;
begin
  //BO.Disconnect;
  WebBridge.ActionCommand(Action_Disconnect);
end;

function TWebDebugger.GetEventName: string;
begin
  if UseShortCut then
    result := Main.DocManager.EventName
  else
    result := ProxyProps.EventName;
end;

procedure TWebDebugger.FillEventNameList(NL: TStrings);
begin
  //Main.DocManager.FillEventNameList(NL);
  NL.Text := WebBridge.ReportAction(Action_FillEventNameList);
end;

function TWebDebugger.FinishReport: string;
begin
  //result := Main.GuiManager.CacheMotor.FinishReport;
  result := WebBridge.ReportAction(Action_FinishReport);
end;

function TWebDebugger.GetEventReport(ReportID, SortColumn: Integer;
  b: Boolean): string;
begin
  //result := Main.GuiManager.CacheMotor.GetEventReport(ReportID, SortColumn, b);
  result := WebBridge.GetEventReport(ReportID, SortColumn, b);
end;

function TWebDebugger.GetFirstFinalRace: Integer;
begin
  if UseShortCut then
    result := BO.EventProps.FirstFinalRace
  else
    result := ProxyProps.FirstFinalRace;
end;

function TWebDebugger.GetRaceReport(ReportID, Race, IT: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.GetRaceReport(ReportID, Race, IT);
  result := WebBridge.GetRaceReport(ReportID, Race, IT);
end;

function TWebDebugger.GetRaceXML(Race, IT: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.GetRaceXML(Race, IT);
  result := WebBridge.GetRaceXML(Race, IT);
end;

function TWebDebugger.GetStartlistCount: Integer;
begin
  if UseShortCut then
    result := BO.BOParams.StartlistCount
  else
    result := ProxyProps.StartlistCount;
end;

function TWebDebugger.GetStatusString: string;
begin
  //result := Main.GetStatusString;
  result := WebBridge.ReportAction(Action_GetStatusString);
end;

function TWebDebugger.GetXMLReport(ReportID: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.GetXMLReport(ReportID);
  result := WebBridge.GetXMLReport(ReportID);
end;

procedure TWebDebugger.BackupExecute;
begin
  //Main.GuiManager.BackupExecute(nil);
  WebBridge.ActionCommand(Action_Backup);
end;

procedure TWebDebugger.ClearExecute;
begin
  //Main.GuiManager.ClearExecute(nil);
  WebBridge.ActionCommand(Action_Clear);
end;

procedure TWebDebugger.ColorCycleExecute;
begin
  //Main.GuiManager.ColorCycleExecute(nil);
  WebBridge.ActionCommand(Action_ColorCycle);
end;

procedure TWebDebugger.RecreateExecute;
begin
  //Main.GuiManager.RecreateExecute(nil);
  WebBridge.ActionCommand(Action_Recreate);
end;

procedure TWebDebugger.RedoExecute;
begin
  //Main.GuiManager.RedoExecute(nil);
  WebBridge.ActionCommand(Action_Redo);
end;

procedure TWebDebugger.RelaxedModeExecute;
begin
  //Main.GuiManager.RelaxedModeExecute(nil);
  WebBridge.ActionCommand(Action_RelaxedMode);
end;

procedure TWebDebugger.StrictModeExecute;
begin
  //Main.GuiManager.StrictModeExecute(nil);
  WebBridge.ActionCommand(Action_StrictMode);
end;

procedure TWebDebugger.UndoExecute;
begin
  //Main.GuiManager.UndoExecute(nil);
  WebBridge.ActionCommand(Action_Undo);
end;

procedure TWebDebugger.SynchronizeCacheExecute;
begin
  //Main.GuiManager.CacheMotor.Synchronize;
  WebBridge.ActionCommand(Action_SynchronizeCache);
end;

procedure TWebDebugger.DeleteEventExecute(EventName: string);
begin
  //Main.GuiManager.DeleteEvent(EventName);
  WebBridge.ActionCommand(Action_Delete);
end;

procedure TWebDebugger.LoadTestDataExecute;
begin
  //Main.GuiManager.LoadTestDataItemClick(nil);
  WebBridge.ActionCommand(Action_LoadTestData);
end;

procedure TWebDebugger.OpenEventExecute(EventName: string);
begin
  //Main.GuiManager.OpenEvent(EventName);
  WebBridge.ActionCommand(Action_Open);
end;

procedure TWebDebugger.SaveEventExecute;
begin
  //Main.GuiManager.SaveEvent;
  WebBridge.ActionCommand(Action_Save);
end;

procedure TWebDebugger.SaveEventAsExecute(EventName: string);
begin
  //Main.GuiManager.SaveEventAs(EventName);
  WebBridge.ActionCommand(Action_SaveAs);
end;

procedure TWebDebugger.UpdateEventParams(RaceCount, ITCount,
  StartlistCount: Integer);
begin
  //Main.GuiManager.UpdateEventParams(RaceCount, ITCount, StartlistCount);
  WebBridge.UpdateEventParams(RaceCount, ITCount, StartlistCount);
end;

procedure TWebDebugger.UpdateFleetProps(UseFleets: Boolean;
  TargetFleetSize, FirstFinalRace: Integer);
begin
  //Main.GuiManager.UpdateFleetProps(UseFleets, TargetFleetSize, FirstFinalRace);
  WebBridge.UpdateFleetProps(UseFleets, TargetFleetSize, FirstFinalRace);
end;

procedure TWebDebugger.UpdateWorkspace(WorkspaceType,
  WorkspaceID: Integer);
begin
  //Main.GuiManager.UpdateWorkspace(WorkspaceType, WorkspaceID);
  WebBridge.UpdateWorkspace(WorkspaceType, WorkspaceID);
end;

function TWebDebugger.GetUseFleets: Boolean;
begin
  if UseShortCut then
    result := BO.EventProps.UseFleets
  else
    result := ProxyProps.UseFleets;
end;

function TWebDebugger.UpdateEntry(request: string): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_Entries(request);
  result := WebBridge.RequestAction(Action_UpdateEntry, request);
end;

function TWebDebugger.ExecuteCommand(request: string): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_Manage(request);
  result := WebBridge.RequestAction(Action_ExecuteCommand, request);
end;

function TWebDebugger.UpdateRaceValue(request: string): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_RV(request);
  result := WebBridge.RequestAction(Action_UpdateRaceValue, request);
end;

function TWebDebugger.Handle_TW_Ajax(Race, IT, Bib: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_TW_Ajax(Race, IT, Bib);
  result := WebBridge.Handle_TW_Ajax(Race, IT, Bib);
end;

function TWebDebugger.Handle_TW_Report(Race: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_TW_TimePointReport(Race);
  result := WebBridge.Handle_TW_Report(Race);
end;

function TWebDebugger.Handle_TW_Table(Race, IT, Bib: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_TW_TimePointTable(Race, IT, Bib);
  result := WebBridge.Handle_TW_Table(Race, IT, Bib);
end;

function TWebDebugger.Handle_TW_XML(Race, IT, Bib: Integer): string;
begin
  //result := Main.GuiManager.CacheMotor.Handle_TW_TimePointXML(Race, IT, Bib);
  result := WebBridge.Handle_TW_XML(Race, IT, Bib);
end;

function TWebDebugger.GetIniImage: string;
begin
  //result := Main.IniImage.ToString;
  result := WebBridge.ReportAction(Action_GetIniImage);
end;

function TWebDebugger.GetIT: Integer;
begin
  if UseShortCut then
    result := Main.GuiManager.IT
  else
    result := ProxyProps.IT;
end;

function TWebDebugger.GetITCount: Integer;
begin
  if UseShortCut then
    result := BO.BOParams.ITCount
  else
    result := ProxyProps.ITCount;
end;

function TWebDebugger.GetJavaScoreXML: string;
begin
  //result := BO.JavaScoreXML.ToString;
  result := WebBridge.ReportAction(Action_GetJavaScoreXML);
end;

function TWebDebugger.GetReport(request: string): string;
begin
  //result := BO.Output.GetMsg(request);
  result := WebBridge.RequestAction(Action_GetReport, request);
end;

procedure TWebDebugger.DownloadExecute;
begin
  //Main.GuiManager.SwapEvent(Main.PeerController.Download);
  WebBridge.ActionCommand(Action_Download);
end;

procedure TWebDebugger.PluginExecute;
begin
  //Main.PeerController.Plugin;
  WebBridge.ActionCommand(Action_Plugin);
end;

procedure TWebDebugger.PlugoutExecute;
begin
  //Main.PeerController.Plugout;
  WebBridge.ActionCommand(Action_Plugout);
end;

procedure TWebDebugger.SynchronizeExecute;
begin
  //Main.PeerController.Synchronize;
  WebBridge.ActionCommand(Action_Synchronize);
end;

procedure TWebDebugger.UploadExecute;
begin
  //Main.PeerController.Upload(BO.Save);
  WebBridge.ActionCommand(Action_Upload);
end;

function TWebDebugger.PointsReport: string;
begin
  //result := Main.GuiManager.CacheMotor.PointsReport;
  result := WebBridge.ReportAction(Action_PointsReport);
end;

procedure TWebDebugger.QueryProps;
begin
  WebBridge.QueryProxyProps(ProxyProps);
end;

function TWebDebugger.GetRace: Integer;
begin
  if UseShortCut then
    result := Main.GuiManager.Race
  else
    result := ProxyProps.Race;
end;

function TWebDebugger.GetRaceCount: Integer;
begin
  if UseShortCut then
    result := BO.BOParams.RaceCount
  else
    result := ProxyProps.RaceCount;
end;

function TWebDebugger.GetRaceDataXML: string;
begin
  //result := BO.RaceDataXML.ToString;
  result := WebBridge.ReportAction(Action_GetRaceDataXML);
end;

function TWebDebugger.GetTargetFleetSize: Integer;
begin
  if UseShortCut then
    result := BO.EventProps.TargetFleetSize
  else
    result := ProxyProps.TargetFleetSize;
end;

function TWebDebugger.TimePointReport: string;
begin
  //result := Main.GuiManager.CacheMotor.TimePointReport;
  result := WebBridge.ReportAction(Action_TimePointReport);
end;

function TWebDebugger.ToTXT: string;
begin
  //result := BO.ToTXT;
  result := WebBridge.ReportAction(Action_ToTxt);
end;

function TWebDebugger.ToXML: string;
begin
  //result := BO.ToXML;
  result := WebBridge.ReportAction(Action_ToXml);
end;

function TWebDebugger.GetUndoManagerLog: string;
begin
  //result := BO.UndoManager.GetLog;
  result := WebBridge.ReportAction(Action_GetUndoManagerLog);
end;

function TWebDebugger.GetUndoManagerRedo: string;
begin
  //result := BO.UndoManager.GetRedo;
  result := WebBridge.ReportAction(Action_GetUndoManagerRedo);
end;

function TWebDebugger.GetUndoManagerUndo: string;
begin
  //result := BO.UndoManager.GetUndo;
  result := WebBridge.ReportAction(Action_GetUndoManagerUndo);
end;

function TWebDebugger.GetUndoManagerUndoRedo: string;
begin
  //result := BO.UndoManager.GetUndoRedo;
  result := WebBridge.ReportAction(Action_GetUndoManagerUndoRedo);
end;

function TWebDebugger.WebStatusString: string;
begin
  //result := Main.WebStatusString;
  result := WebBridge.ReportAction(Action_WebStatusString);
end;

function TWebDebugger.GetWorkspaceInfoReport: string;
begin
  //result := Main.WorkspaceInfo.ToString;
  result := WebBridge.ReportAction(Action_GetWorkspaceInfoReport);
end;

function TWebDebugger.GetWorkspaceID: Integer;
begin
  if UseShortCut then
    result := Main.WorkspaceInfo.WorkspaceID
  else
    result := ProxyProps.WorkspaceID;
end;

function TWebDebugger.GetWorkspaceType: Integer;
begin
  if UseShortCut then
    result := Main.WorkspaceInfo.WorkspaceType
  else
    result := ProxyProps.WorkspaceType;
end;

end.
