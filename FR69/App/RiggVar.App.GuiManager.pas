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
  RiggVar.BO.ScenarioManager,
  RiggVar.Col.Cache,
  RiggVar.Col.Event,
  RiggVar.Web1.Receiver,
  RiggVar.Web1.MotorBase,
  RiggVar.Web1.MotorHome,
  RiggVar.Web1.MotorRemote,
  RiggVar.Web2.Page,
  RiggVar.Web2.Router,
  RiggVar.Web2.Master,
  RiggVar.Web3.Server,
  RiggVar.Web4.Server;

type
  TGuiManager = class
  private
    FRace: Integer;
    FIT: Integer;
    FMarkCount: Integer;

    ExceptionLabel_Caption: string;
    FAutoUpdateEvent: Boolean;
    FAutoSend: Boolean;

    procedure SetRace(const Value: Integer);
    procedure SetIT(const Value: Integer);
    procedure InitNewBO;
    function GetThrowouts: Integer;
    procedure SetThrowouts(const Value: Integer);
    function GetUseWeb: Boolean;
    procedure Inform(ga: TGuiAction);
    procedure SetAutoUpdateEvent(const Value: Boolean);
    procedure SetAutoSend(const Value: Boolean);
  public
    GuiInterface: IGuiInterface; //injected reference
    AppName: string;

    HomeRouter: TWebRouter;
    RemoteRouter: TWebRouter;

    HomeIndexPage: TIndexPage;
    RemoteIndexPage: TIndexPage;

    WebMotorHome: TWebMotorHome;
    WebMotorRemote: TWebMotorRemote;
    SilverlightWeb: TSilverlightWeb;
    AngularWeb: TAngularWeb;

    WebReceiver: TWebReceiver;
    CacheMotor: TCacheMotor;

    constructor Create;
    destructor Destroy; override;

    procedure InitWeb;
    procedure CloseWeb;

    procedure SetMarkCount(const Value: Integer);

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
    property MarkCount: Integer read FMarkCount write SetMarkCount;
    property UseWeb: Boolean read GetUseWeb;
    property AutoSend: Boolean read FAutoSend write SetAutoSend;
    property AutoUpdateEvent: Boolean read FAutoUpdateEvent write SetAutoUpdateEvent;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BR.PeerController,
  RiggVar.DAL.Manager,
  RiggVar.Web1.Stub,
  RiggVar.Web1.Proxy,
  RiggVar.Web1.Debugger,
  RiggVar.Util.Sound;

{ TGuiManager }

constructor TGuiManager.Create;
begin
  inherited Create;

  Main.GuiManager := self;

  AppName := Main.FolderInfo.AppName;

  WebProxy := TWebStub.Create;
  WebReceiver := TWebReceiver.Create;

  if Main.Params.WantWeb then
    InitWeb;

  CacheMotor := TCacheMotor.Create;
  InitCache;

  if UseWeb and not IsWinGUI then
  begin
    if not IsTest then
    begin
      WebMotorRemote.Init;
      WebMotorHome.Init;
    end;
  end;

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

  CloseWeb;

  if Assigned(WebProxy) then
  begin
    WebProxy.Free;
    WebProxy := nil;
  end;

  WebReceiver.Free;
  WebReceiver := nil;

  CacheMotor.Free;
  CacheMotor := nil;
  inherited Destroy;
end;

procedure TGuiManager.InitWeb;
var
  samePort: Boolean;
begin
  samePort := Main.IniImage.WebServerRemotePort = Main.IniImage.WebServerHomePort;

  RemoteRouter := TWebRouter.Create;
  RemoteRouter.Host := Main.IniImage.WebServerHost;
  RemoteRouter.Port := Main.IniImage.WebServerRemotePort;
  RemoteRouter.ImageManager := Main.ImageManager;
  RemoteRouter.ResourceManager := Main.ResourceManager;
  RemoteRouter.UseResource := Main.IniImage.UseResource;

  if samePort then
    HomeRouter := RemoteRouter
  else
  begin
    HomeRouter := TWebRouter.Create;
    HomeRouter.Host := Main.IniImage.WebServerHost;
    HomeRouter.Port := Main.IniImage.WebServerHomePort;
    HomeRouter.ImageManager := Main.ImageManager;
    HomeRouter.ResourceManager := Main.ResourceManager;
    HomeRouter.UseResource := Main.IniImage.UseResource;
  end;

  TMasterPage.IsHomeWeb := true;

  HomeIndexPage := TIndexPage.Create;
  HomeIndexPage.Path := '/';
  HomeIndexPage.HTTPServer := HomeRouter;

  WebMotorHome := TWebMotorHome.Create;
  WebMotorHome.Path := '/home';
  WebMotorHome.HTTPServer := HomeRouter;

  SilverlightWeb := TSilverlightWeb.Create;
  SilverlightWeb.Path := '/silverlight';
  SilverlightWeb.HttpServer := HomeRouter;

  AngularWeb := TAngularWeb.Create;
  AngularWeb.Path := '/angular';
  AngularWeb.HttpServer := HomeRouter;

  TMasterPage.IsHomeWeb := false;

  RemoteIndexPage := TIndexPage.Create;
  RemoteIndexPage.Path := '/';
  RemoteIndexPage.HTTPServer := RemoteRouter;

  WebMotorRemote := TWebMotorRemote.Create;
  WebMotorRemote.Path := '/remote';
  WebMotorRemote.HTTPServer := RemoteRouter;

  if Assigned(Main.ServerBridge) then
  begin
    Main.ServerBridge.BridgeWeb.Path := '/bridge';
    Main.ServerBridge.BridgeWeb.HTTPServer := RemoteRouter;
    RemoteRouter.BridgeController := Main.ServerBridge.BridgeWeb;
    Main.IniImage.BridgeUrl := Main.ServerBridge.BridgeWeb.Url;
  end;

  HomeRouter.IndexPage := HomeIndexPage;
  HomeRouter.RemoteController := nil;
  HomeRouter.HomeController := WebMotorHome;
  HomeRouter.SilverlightController := SilverlightWeb;
  HomeRouter.AngularController := AngularWeb;

  RemoteRouter.IndexPage := RemoteIndexPage;
  RemoteRouter.RemoteController := WebMotorRemote;
  if samePort then
  begin
    RemoteRouter.HomeController := WebMotorHome;
    RemoteRouter.SilverlightController := SilverlightWeb;
  end;

  if HomeIndexPage.MasterPage.UseTemplate then
    HomeRouter.UseResource := false;
  HomeRouter.Init;

  if RemoteIndexPage.MasterPage.UseTemplate then
    RemoteRouter.UseResource := false;
  RemoteRouter.Init;
end;

procedure TGuiManager.CloseWeb;
begin
  if AngularWeb <> nil then
  begin
    AngularWeb.Free;
    AngularWeb := nil;
  end;

  if SilverlightWeb <> nil then
  begin
    SilverlightWeb.Free;
    SilverlightWeb := nil;
  end;

  if HomeIndexPage <> nil then
  begin
  HomeIndexPage.Free;
  HomeIndexPage := nil;
  end;

  if RemoteIndexPage <> nil then
  begin
  RemoteIndexPage.Free;
  RemoteIndexPage := nil;
  end;

  if WebMotorHome <> nil then
  begin
  WebMotorHome.Free;
  WebMotorHome := nil;
  end;

  if WebMotorRemote <> nil then
  begin
  WebMotorRemote.Free;
  WebMotorRemote := nil;
  end;

  if HomeRouter <> RemoteRouter then
  begin
    HomeRouter.Free;
    HomeRouter := nil;
  end;

  if RemoteRouter <> nil then
  begin
    RemoteRouter.Free;
    RemoteRouter := nil;
  end;

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
  Main.PeerController.DoOnIdle;
end;

function TGuiManager.GetUseWeb: Boolean;
begin
  result := true;
  if not Assigned(WebMotorHome) then
    result := false
  else if not Assigned(WebMotorRemote) then
    result := false
  else if not Assigned(SilverlightWeb) then
    result := false
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
  Main.StoreAdapter.UpdateWorkspace(WorkspaceType, WorkspaceID);
  if Assigned(GuiInterface) then
    GuiInterface.UpdateWorkspaceStatus;
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
      Inform(TGuiAction.UpdateCurrent);
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

    if Main.PeerController.IsEnabled(SwitchOp_Upload) then
    begin
      if Main.Params.AutoUpload then
      begin
        Main.PeerController.Upload(BO.Save(false));
      end;
    end;

    PlaySound(Sound_Recycle);
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

procedure TGuiManager.SetAutoSend(const Value: Boolean);
begin
  if Value <> FAutoSend then
  begin
    FAutoSend := Value;
    Inform(TGuiAction.AutoSendChanged);
  end;
end;

procedure TGuiManager.SetAutoUpdateEvent(const Value: Boolean);
begin
  if Value <> FAutoUpdateEvent then
  begin
    FAutoUpdateEvent := Value;
    Inform(TGuiAction.AutoUpdateEventChanged);
  end;
end;

procedure TGuiManager.SetIT(const Value: Integer);
begin
  if (Value >= 0) and (Value <= BO.BOParams.ITCount) and (Value <> FIT) then
  begin
    FIT := Value;
    if Assigned(WebMotorRemote) then
      WebMotorRemote.CurrentIT := FIT;
    if Assigned(WebMotorHome) then
      WebMotorHome.CurrentIT := FIT;
    Inform(TimepointChanged);
    Inform(UpdateCurrent);
  end;
end;

procedure TGuiManager.SetMarkCount(const Value: Integer);
begin
  FMarkCount := Value;
  Inform(MarkCountChanged);
end;

procedure TGuiManager.SetRace(const Value: Integer);
begin
  if (Value > 0) and (Value <= BO.BOParams.RaceCount) and (Value <> FRace) then
  begin
    FRace := Value;
    if Assigned(WebMotorRemote) then
      WebMotorRemote.CurrentRace := FRace;
    if Assigned(WebMotorHome) then
      WebMotorHome.CurrentRace := FRace;
    Inform(RaceChanged);
    Inform(UpdateCurrent);
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
      Inform(ThrowoutNumberChanged);
    end;
  end;
end;

procedure TGuiManager.InitCache;
begin
  if Assigned(CacheMotor.InputConnection) then
    CacheMotor.InputConnection.Free;
  CacheMotor.InputConnection := BO.InputServer.Server.Connect('Cache.Input');
  CacheMotor.SwapEvent;
  Inform(InitCacheGui);
end;

procedure TGuiManager.DisposeCache;
begin
  if Assigned(CacheMotor) then
  begin
    CacheMotor.CacheEnabled := false;
    CacheMotor.InputConnection.Free;
    CacheMotor.InputConnection := nil;
  end;
  Inform(DisposeCacheGui);
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
      Inform(acUndo);
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
      Inform(acRedo);
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
  Inform(acRedo);
end;

procedure TGuiManager.StrictModeExecute(Sender: TObject);
begin
  BO.EventBO.RelaxedInputMode := false;
  Inform(acStrict);
end;

procedure TGuiManager.RelaxedModeExecute(Sender: TObject);
begin
  BO.EventBO.RelaxedInputMode := true;
  Inform(acStrict);
end;

procedure TGuiManager.PlaySound(sid: Integer);
begin
  Main.SoundManager.PlayWav(sid);
end;

procedure TGuiManager.SynchronizeCache;
begin
  if Main.Params.WantAutoSync then
    CacheMotor.Synchronize;
end;

procedure TGuiManager.Inform(ga: TGuiAction);
begin
  if Assigned(GuiInterface) then
    GuiInterface.HandleInform(ga);
end;

end.
