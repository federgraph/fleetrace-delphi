unit RiggVar.Web1.Stub;

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
  SysUtils, Classes;

type
  TWebStub = class
  private
    function GetEventName: string;
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
    function GetContextPath: string;
  public
    procedure QueryProps(SL: TStrings);

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

    property ContextPath: string read GetContextPath;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Util.Classes;

{ TWebStub }

function TWebStub.CacheReport: string;
begin
  result := Main.GuiManager.CacheMotor.Cache.GetHTM;
end;

procedure TWebStub.ConnectExecute;
begin
  BO.Connect;
end;

function TWebStub.GetContextPath: string;
begin
  result := Main.Params.ContextPath;
end;

function TWebStub.GetConvertedEventData: string;
begin
  result := BO.ConvertedData;
end;

procedure TWebStub.DisconnectExecute;
begin
  BO.Disconnect;
end;

function TWebStub.GetEventName: string;
begin
  result := Main.DocManager.EventName;
end;

procedure TWebStub.FillEventNameList(NL: TStrings);
begin
  Main.DocManager.FillEventNameList(NL);
end;

function TWebStub.FinishReport: string;
begin
  result := Main.GuiManager.CacheMotor.FinishReport;
end;

function TWebStub.GetEventReport(ReportID, SortColumn: Integer;
  b: Boolean): string;
begin
  result := Main.GuiManager.CacheMotor.GetEventReport(ReportID, SortColumn, b);
end;

function TWebStub.GetFirstFinalRace: Integer;
begin
  result := BO.EventProps.FirstFinalRace;
end;

function TWebStub.GetRaceReport(ReportID, Race, IT: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.GetRaceReport(ReportID, Race, IT);
end;

function TWebStub.GetRaceXML(Race, IT: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.GetRaceXML(Race, IT);
end;

function TWebStub.GetStartlistCount: Integer;
begin
  result := BO.BOParams.StartlistCount;
end;

function TWebStub.GetStatusString: string;
begin
  result := Main.GetStatusString;
end;

function TWebStub.GetXMLReport(ReportID: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.GetXMLReport(ReportID);
end;

procedure TWebStub.BackupExecute;
begin
  Main.GuiManager.BackupExecute(nil);
end;

procedure TWebStub.ClearExecute;
begin
  Main.GuiManager.ClearExecute(nil);
end;

procedure TWebStub.ColorCycleExecute;
begin
  Main.GuiManager.ColorCycleExecute(nil);
end;

procedure TWebStub.RecreateExecute;
begin
  Main.GuiManager.RecreateExecute(nil);
end;

procedure TWebStub.RedoExecute;
begin
  Main.GuiManager.RedoExecute(nil);
end;

procedure TWebStub.RelaxedModeExecute;
begin
  Main.GuiManager.RelaxedModeExecute(nil);
end;

procedure TWebStub.StrictModeExecute;
begin
  Main.GuiManager.StrictModeExecute(nil);
end;

procedure TWebStub.UndoExecute;
begin
  Main.GuiManager.UndoExecute(nil);
end;

procedure TWebStub.SynchronizeCacheExecute;
begin
  Main.GuiManager.CacheMotor.Synchronize;
end;

procedure TWebStub.DeleteEventExecute(EventName: string);
begin
  Main.GuiManager.DeleteEvent(EventName);
end;

procedure TWebStub.LoadTestDataExecute;
begin
  Main.GuiManager.LoadTestDataItemClick(nil);
end;

procedure TWebStub.OpenEventExecute(EventName: string);
begin
  Main.GuiManager.OpenEvent(EventName);
end;

procedure TWebStub.SaveEventExecute;
begin
  Main.GuiManager.SaveEvent;
end;

procedure TWebStub.SaveEventAsExecute(EventName: string);
begin
  Main.GuiManager.SaveEventAs(EventName);
end;

procedure TWebStub.UpdateEventParams(RaceCount, ITCount,
  StartlistCount: Integer);
begin
  Main.GuiManager.UpdateEventParams(RaceCount, ITCount, StartlistCount);
end;

procedure TWebStub.UpdateFleetProps(UseFleets: Boolean;
  TargetFleetSize, FirstFinalRace: Integer);
begin
  Main.GuiManager.UpdateFleetProps(UseFleets, TargetFleetSize, FirstFinalRace);
end;

procedure TWebStub.UpdateWorkspace(WorkspaceType,
  WorkspaceID: Integer);
begin
  Main.GuiManager.UpdateWorkspace(WorkspaceType, WorkspaceID);
end;

function TWebStub.GetUseFleets: Boolean;
begin
  result := BO.EventProps.UseFleets;
end;

function TWebStub.UpdateEntry(request: string): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_Entries(request);
end;

function TWebStub.ExecuteCommand(request: string): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_Manage(request);
end;

function TWebStub.UpdateRaceValue(request: string): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_RV(request);
end;

function TWebStub.Handle_TW_Ajax(Race, IT, Bib: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_TW_Ajax(Race, IT, Bib);
end;

function TWebStub.Handle_TW_Report(Race: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_TW_TimePointReport(Race);
end;

function TWebStub.Handle_TW_Table(Race, IT, Bib: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_TW_TimePointTable(Race, IT, Bib);
end;

function TWebStub.Handle_TW_XML(Race, IT, Bib: Integer): string;
begin
  result := Main.GuiManager.CacheMotor.Handle_TW_TimePointXML(Race, IT, Bib);
end;

function TWebStub.GetIniImage: string;
begin
  result := Main.IniImage.ToString;
end;

function TWebStub.GetIT: Integer;
begin
  result := Main.GuiManager.IT;
end;

function TWebStub.GetITCount: Integer;
begin
  result := BO.BOParams.ITCount;
end;

function TWebStub.GetJavaScoreXML: string;
begin
  result := BO.JavaScoreXML.ToString;
end;

function TWebStub.GetReport(request: string): string;
begin
  result := BO.Output.GetMsg(request);
end;

procedure TWebStub.DownloadExecute;
begin
  Main.GuiManager.SwapEvent(Main.PeerController.Download);
end;

procedure TWebStub.PluginExecute;
begin
  Main.PeerController.Plugin;
end;

procedure TWebStub.PlugoutExecute;
begin
  Main.PeerController.Plugout;
end;

procedure TWebStub.SynchronizeExecute;
begin
  Main.PeerController.Synchronize;
end;

procedure TWebStub.UploadExecute;
begin
  Main.PeerController.Upload(BO.Save);
end;

function TWebStub.PointsReport: string;
begin
  result := Main.GuiManager.CacheMotor.PointsReport;
end;

procedure TWebStub.QueryProps(SL: TStrings);
begin
  SL.Add(EventName);

  SL.Add(IntToStr(RaceCount));
  SL.Add(IntToStr(ITCount));
  SL.Add(IntToStr(StartlistCount));

  SL.Add(IntToStr(Race));
  SL.Add(IntToStr(IT));

  SL.Add(BoolStr[UseFleets]);
  SL.Add(IntToStr(TargetFleetSize));
  SL.Add(IntToStr(FirstFinalRace));

  SL.Add(IntToStr(WorkspaceType));
  SL.Add(IntToStr(WorkspaceID));
end;

function TWebStub.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

function TWebStub.GetRaceCount: Integer;
begin
  result := BO.BOParams.RaceCount;
end;

function TWebStub.GetRaceDataXML: string;
begin
  result := BO.RaceDataXML.ToString;
end;

function TWebStub.GetTargetFleetSize: Integer;
begin
   result := BO.EventProps.TargetFleetSize;
end;

function TWebStub.TimePointReport: string;
begin
  result := Main.GuiManager.CacheMotor.TimePointReport;
end;

function TWebStub.ToTXT: string;
begin
  result := BO.ToTXT;
end;

function TWebStub.ToXML: string;
begin
  result := BO.ToXML;
end;

function TWebStub.GetUndoManagerLog: string;
begin
  result := BO.UndoManager.GetLog;
end;

function TWebStub.GetUndoManagerRedo: string;
begin
  result := BO.UndoManager.GetRedo;
end;

function TWebStub.GetUndoManagerUndo: string;
begin
  result := BO.UndoManager.GetUndo;
end;

function TWebStub.GetUndoManagerUndoRedo: string;
begin
  result := BO.UndoManager.GetUndoRedo;
end;

function TWebStub.WebStatusString: string;
begin
  result := Main.WebStatusString;
end;

function TWebStub.GetWorkspaceInfoReport: string;
begin
  result := Main.WorkspaceInfo.ToString;
end;

function TWebStub.GetWorkspaceID: Integer;
begin
  result := Main.WorkspaceInfo.WorkspaceID;
end;

function TWebStub.GetWorkspaceType: Integer;
begin
  result := Main.WorkspaceInfo.WorkspaceType;
end;

end.
