unit RiggVar.App.GuiInterface;

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
  TGuiAction = (
    AlwaysDoOnIdle,
    SometimesDoOnIdle,
    acRestore,
    acClear,
    acUndo,
    acRedo,
    acColor,
    acStrict,
    acRelaxed,
    ITChanged,
    RaceChanged,
    EventChanged,
    ThrowoutNumberChanged,
    CaptionChanged,
    CategoryGridChanged,
    JsonGridChanged,
    CacheGridChanged,
    WorkspaceStatusChanged,
    ScheduleEventUpdate,
    ScheduleRaceUpdate,
    ScheduleProfileUpdate,
    WorkspaceListChanged,
    ExportTranslation,
    StartBatch,
    MarkChanged,
    MarkCountChanged,
    TimePointChanged,
    TimingDataChanged,
    InitCacheGui,
    DisposeCacheGui,
    ClearAge,
    UpdateCurrent,
    AutoSendChanged,
    AutoUpdateEventChanged
  );

  IGuiInterface = interface
  ['{2957A32C-BB36-4D65-B609-2D44B7FC0D47}']
    procedure HandleInform(Action: TGuiAction);
    procedure UpdateCaption;
    procedure UpdateWorkspaceStatus;

    procedure InitViews;
    procedure DisposeViews;
  end;

implementation

end.
