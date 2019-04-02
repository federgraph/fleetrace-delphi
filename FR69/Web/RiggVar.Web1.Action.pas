unit RiggVar.Web1.Action;

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

type
  TProxyProps = class
  private
    function GetIsValid: Boolean;
  public
    EventName: string;

    RaceCount: Integer;
    ITCount: Integer;
    StartlistCount: Integer;

    Race: Integer;
    IT: Integer;

    UseFleets: Boolean;
    TargetFleetSize: Integer;
    FirstFinalRace: Integer;

    WorkspaceType: Integer;
    WorkspaceID: Integer;

    constructor Create;

    procedure Clear;
    procedure Default;
    procedure Assign(o: TProxyProps);
    property IsValid: Boolean read GetIsValid;
  end;

const
  Action_QueryProps = 0;

  Action_Open = 1;
  Action_Save = 2;
  Action_SaveAs = 3;
  Action_Delete = 4;

  Action_Connect = 5;
  Action_Disconnect = 6;
  Action_Backup = 7;
  Action_Recreate = 8;
  Action_Clear = 9;

  Action_Plugin = 10;
  Action_Plugout = 11;
  Action_Synchronize = 12;
  Action_Upload = 13;
  Action_Download = 14;

  Action_Undo = 15;
  Action_Redo = 16;

  Action_LoadTestData = 17;
  Action_SynchronizeCache = 18;

  Action_StrictMode = 19;
  Action_RelaxedMode = 20;
  Action_ColorCycle = 21;

  Action_FillEventNameList = 22;

  Action_GetReport = 23;

  Action_ToTXT = 24;
  Action_ToXML = 25;
  Action_GetJavaScoreXML = 26;
  Action_GetRaceDataXML = 27;

  Action_GetUndoManagerLog = 28;
  Action_GetUndoManagerUndo = 29;
  Action_GetUndoManagerRedo = 30;
  Action_GetUndoManagerUndoRedo = 31;

  //Action_GetEventName = 32;
  Action_GetStatusString = 33;
  Action_WebStatusString = 34;
  Action_GetConvertedEventData = 35;
  Action_GetIniImage = 36;
  Action_GetWorkspaceInfoReport = 37;

  Action_FinishReport = 38;
  Action_PointsReport = 39;
  Action_TimePointReport = 40;
  Action_CacheReport = 41;

  Action_GetEventReport = 42;
  Action_GetRaceReport = 43;
  Action_GetRaceXML = 44;
  Action_GetXMLReport = 45;

  Action_UpdateWorkspace = 46;
  Action_UpdateEventParams = 47;
  Action_UpdateFleetProps = 48;

  Action_UpdateEntry = 49;
  Action_UpdateRaceValue = 50;
  Action_ExecuteCommand = 51;

  Action_Handle_TW_Report = 52;
  Action_Handle_TW_XML = 53;
  Action_Handle_TW_Table = 54;
  Action_Handle_TW_Ajax = 55;

implementation

{ TProxyProps }

procedure TProxyProps.Assign(o: TProxyProps);
begin
  EventName := o.EventName;

  RaceCount := o.RaceCount;
  ITCount := o.ITCount;
  StartlistCount := o.StartlistCount;

  Race := o.Race;
  IT := o.IT;

  UseFleets := o.UseFleets;
  TargetFleetSize := o.TargetFleetSize;
  FirstFinalRace := o.FirstFinalRace;

  WorkspaceType := o.WorkspaceType;
  WorkspaceID := o.WorkspaceID;
end;

procedure TProxyProps.Clear;
begin
  EventName := '';

  RaceCount := 0;
  ITCount := 0;
  StartlistCount := 0;

  Race := 0;
  IT := 0;

  UseFleets := False;
  TargetFleetSize := 0;
  FirstFinalRace := 0;

  WorkspaceType := 0;
  WorkspaceID := 0;
end;

constructor TProxyProps.Create;
begin
  Default;
end;

procedure TProxyProps.Default;
begin
  EventName := 'FR';

  RaceCount := 2;
  ITCount := 0;
  StartlistCount := 8;

  Race := 1;
  IT := 0;

  UseFleets := False;
  TargetFleetSize := 8;
  FirstFinalRace := 20;

  WorkspaceType := 1;
  WorkspaceID := 1;
end;

function TProxyProps.GetIsValid: Boolean;
begin
  result := (Length(EventName) > 0)
  and (RaceCount > 0)
  and (ITCount >= 0)
  and (StartlistCount > 1)
  and (Race > 0)
  and (IT >= 0)
  and (Race <= RaceCount)
  and (IT <= ITCount)
  and (TargetFleetSize > 2)
  and (FirstFinalRace > 0)
  and (WorkspaceType > 0)
  and (WorkspaceID > 0)
end;

end.
