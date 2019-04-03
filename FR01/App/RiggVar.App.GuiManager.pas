unit RiggVar.App.GuiManager;

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
  RiggVar.App.GuiInterface,
  RiggVar.BO.CacheMotor,
  RiggVar.BO.Def,
  RiggVar.BO.EventProps,
  RiggVar.BO.Params,
  RiggVar.Col.Event;

type
  TGuiManager = class
  private
    FRace: Integer;
    FIT: Integer;

    ExceptionLabel_Caption: string;

    procedure SetRace(const Value: Integer);
    procedure SetIT(const Value: Integer);
    procedure InitNewBO;
    function GetThrowouts: Integer;
    procedure SetThrowouts(const Value: Integer);
  public
    GuiInterface: IGuiInterface; //injected reference
    AppName: string;

    CacheMotor: TCacheMotorMock;

    constructor Create;
    destructor Destroy; override;

    procedure InitCache;
    procedure DisposeCache;

    procedure InitViews;
    procedure DisposeViews;

    procedure PlaySound(sid: Integer);
    procedure UpdateCaption;
    procedure SwapEvent(EventData: string);
    procedure DoOnIdle;
    procedure DoOnException(Sender: TObject; E: Exception);

    procedure OpenEvent(en: string);
    procedure SaveEvent;
    procedure SaveEventAs(en: string);
    procedure DeleteEvent(en: string);

    procedure BackupExecute(Sender: TObject);
    procedure RestoreExecute(Sender: TObject);
    procedure RecreateExecute(Sender: TObject);
    procedure ClearExecute(Sender: TObject);

    procedure UndoExecute(Sender: TObject);
    procedure RedoExecute(Sender: TObject);

    procedure ColorCycleExecute(Sender: TObject);
    procedure StrictModeExecute(Sender: TObject);
    procedure RelaxedModeExecute(Sender: TObject);

    procedure LoadTestDataItemClick(Sender: TObject);
    procedure SynchronizeCache;

    procedure UpdateWorkspace(WorkspaceType, WorkspaceID: Integer);
    procedure UpdateEventParams(RaceCount, ITCount, StartlistCount: Integer);
    procedure UpdateFleetProps(UseFleets: Boolean; TargetFleetSize, FirstFinalRace: Integer);

    property Throwouts: Integer read GetThrowouts write SetThrowouts;
    property Race: Integer read FRace write SetRace;
    property IT: Integer read FIT write SetIT;
  end;

implementation

uses
  RiggVar.App.Main;

{ TGuiManager }

constructor TGuiManager.Create;
begin
  inherited Create;

  Main.GuiManager := self;

  AppName := Main.FolderInfo.AppName;

  CacheMotor := TCacheMotorMock.Create;
  InitCache;

  //prepare for calling property setter
  FIT := -1;
  FRace := 0;

  //make sure property setter is executed once with changed values
  IT := 0;
  Race := 1;
end;

destructor TGuiManager.Destroy;
begin
  if not IsWinGUI then
  begin
    DisposeViews;
    GuiInterface := nil;
  end;

  CacheMotor.Free;
  CacheMotor := nil;
  inherited Destroy;
end;

procedure TGuiManager.DoOnException(Sender: TObject;
  E: Exception);
begin
  Main.Logger.Error(E.Message);
  ExceptionLabel_Caption := E.Message;
end;

procedure TGuiManager.DoOnIdle;
begin
  BO.OnIdle;
  CacheMotor.DoOnIdle;
  //Main.PeerController.DoOnIdle;
end;

function TGuiManager.GetThrowouts: Integer;
begin
  result := BO.EventProps.Throwouts;
end;

procedure TGuiManager.InitViews;
begin
  if Assigned(GuiInterface) then
    GuiInterface.InitViews
  else
    InitCache;
end;

procedure TGuiManager.DisposeViews;
begin
  FRace := 1;
  FIT := 0;
  if Assigned(GuiInterface) then
    GuiInterface.DisposeViews
  else
    DisposeCache;
end;

procedure TGuiManager.InitNewBO;
begin
  { if BO is recreated, Views must immediately be reinitialized,
    because object references become invalid;
    in MDI all Views are destroyed, in SDI this is not possible }
  DisposeViews;
  InitViews;
  Race := 1;
  IT := 0;
end;

procedure TGuiManager.OpenEvent(en: string);
var
  s: string;
begin
  s := Main.DocManager.DocDownloadByName(en);
  if (s <> '') then
    SwapEvent(s);
end;

procedure TGuiManager.SaveEvent;
begin
  Main.DocManager.DocSave;
  BO.UndoManager.Clear;
end;

procedure TGuiManager.SaveEventAs(en: string);
begin
  //called from WebInterface, after input of new event name
  Main.DocManager.EventName := en;
  Main.DocManager.DocSave;
  UpdateCaption;
  BO.UndoManager.Clear;
end;

procedure TGuiManager.DeleteEvent(en: string);
begin
  Main.DocManager.RawDelete(en);
end;

procedure TGuiManager.UpdateWorkspace(WorkspaceType, WorkspaceID: Integer);
begin
end;

procedure TGuiManager.UpdateEventParams(RaceCount, ITCount, StartlistCount: Integer);
var
  wasConnected: Boolean;
  newParams: TBOParams;
begin
  newParams := TBOParams.Create;
  try
    newParams.RaceCount := RaceCount;
    newParams.ITCount := ITCount;
    newParams.StartlistCount := StartlistCount;
    if newParams.IsWithinLimits then
    begin
      wasConnected := BO.Connected;
      DisposeViews;
      Main.RecreateBO(newParams);
      InitNewBO;
      if (wasConnected) then
        BO.Connect;
    end;
  finally
    newParams.Free;
  end;
end;

procedure TGuiManager.UpdateFleetProps(UseFleets: Boolean; TargetFleetSize,
  FirstFinalRace: Integer);
var
  Model: TEventProps;
begin
  Model := BO.EventProps;
  Model.UseFleets := UseFleets;
  Model.FirstFinalRace := FirstFinalRace;
  Model.TargetFleetSize := TargetFleetSize;
  BO.EventNode.Modified := true;
end;

procedure TGuiManager.SwapEvent(EventData: string);
var
  wasConnected: Boolean;
begin
  Main.BeforeDeleteBO(self);
  if EventData <> '' then
  begin
    wasConnected := BO.Connected;
    DisposeViews;
    BO.LoadNew(EventData);
    InitNewBO;
    if (wasConnected) then
    begin
      BO.Connect;
    end;
    if Assigned(GuiInterface) then
    begin
      GuiInterface.UpdateCaption;
      GuiInterface.HandleInform(EventChanged);
    end;

    //PlaySound(Sound_Recycle);
  end;
  Main.AfterDeleteBO(self);
end;

procedure TGuiManager.UpdateCaption;
begin
  if Assigned(GuiInterface) then
    GuiInterface.UpdateCaption;
end;

procedure TGuiManager.BackupExecute(Sender: TObject);
begin
  BO.Backup;
  BO.UndoManager.Clear;
end;

procedure TGuiManager.RecreateExecute(Sender: TObject);
var
  wasConnected: Boolean;
begin
  wasConnected := BO.Connected;
  DisposeViews;
  Main.RecreateBOFromBackup;
  InitNewBO;
  if (wasConnected) then
    BO.Connect;
end;

procedure TGuiManager.RestoreExecute(Sender: TObject);
begin
  DisposeViews;
  BO.Restore;
  BO.Calc;
  InitViews;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(acRestore);
end;

procedure TGuiManager.ClearExecute(Sender: TObject);
begin
  BO.ClearBtnClick;
  BO.Calc;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(acClear);
  BO.UndoManager.Clear;
end;

procedure TGuiManager.LoadTestDataItemClick(Sender: TObject);
begin
  SwapEvent(Main.TestData);
end;

procedure TGuiManager.SetIT(const Value: Integer);
begin
  if (Value >= 0) and (Value <= BO.BOParams.ITCount) and (Value <> FIT) then
  begin
    FIT := Value;
  end;
end;

procedure TGuiManager.SetRace(const Value: Integer);
begin
  if (Value > 0) and (Value <= BO.BOParams.RaceCount) and (Value <> FRace) then
  begin
    FRace := Value;
    if Assigned(GuiInterface) then
      GuiInterface.HandleInform(RaceChanged);
  end;
end;

procedure TGuiManager.SetThrowouts(const Value: Integer);
begin
  if (Value >= 0) and (Value < BO.BOParams.RaceCount) then
  begin
    if BO.EventProps.Throwouts <> Value then
    begin
      BO.EventProps.Throwouts := Value;
      BO.EventNode.Modified := True;
      BO.Calc;
      if Assigned(GuiInterface) then
        GuiInterface.HandleInform(ThrowoutNumberChanged);
    end;
  end;
end;

procedure TGuiManager.InitCache;
begin
  if Assigned(CacheMotor.InputConnection) then
    CacheMotor.InputConnection.Free;
  CacheMotor.InputConnection := BO.InputServer.Server.Connect('Cache.Input');
  //CacheMotor.SwapEvent;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(InitCacheGui);
end;

procedure TGuiManager.DisposeCache;
begin
  if Assigned(CacheMotor) then
  begin
    CacheMotor.CacheEnabled := false;
    CacheMotor.InputConnection.Free;
    CacheMotor.InputConnection := nil;
  end;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(DisposeCacheGui);
end;

procedure TGuiManager.UndoExecute(Sender: TObject);
var
  s: string;
begin
  if BO.UndoManager.UndoCount > 0 then
  begin
    s := BO.UndoManager.Undo;
    if Assigned(BO.UndoConnection) then
    begin
      BO.UndoAgent.UndoFlag := true;
      try
        BO.UndoConnection.InjectMsg(s);
      finally
        BO.UndoAgent.UndoFlag := false;
      end;
      if Assigned(GuiInterface) then
        GuiInterface.HandleInform(acUndo);
    end;
  end;
end;

procedure TGuiManager.RedoExecute(Sender: TObject);
var
  s: string;
begin
  if BO.UndoManager.RedoCount > 0 then
  begin
    s := BO.UndoManager.Redo;
    if Assigned(BO.UndoConnection) then
    begin
      BO.UndoAgent.UndoFlag := true;
      try
        BO.UndoConnection.InjectMsg(s);
      finally
        BO.UndoAgent.UndoFlag := false;
      end;
      if Assigned(GuiInterface) then
        GuiInterface.HandleInform(acRedo);
    end;
  end;
end;

procedure TGuiManager.ColorCycleExecute(Sender: TObject);
var
  ColorMode: TColorMode;
begin
  ColorMode := BO.EventNode.ColorMode;
  if ColorMode < High(TColorMode) then
    Inc(ColorMode)
  else
    ColorMode := Low(TColorMode);
  BO.EventNode.ColorMode := ColorMode;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(acRedo);
end;

procedure TGuiManager.StrictModeExecute(Sender: TObject);
begin
  BO.EventBO.RelaxedInputMode := false;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(acStrict);
end;

procedure TGuiManager.RelaxedModeExecute(Sender: TObject);
begin
  BO.EventBO.RelaxedInputMode := true;
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(acStrict);
end;

procedure TGuiManager.PlaySound(sid: Integer);
begin
  //Main.SoundManager.PlayWav(sid);
end;

procedure TGuiManager.SynchronizeCache;
begin
  if Main.Params.WantAutoSync then
    CacheMotor.Synchronize;
end;

end.
