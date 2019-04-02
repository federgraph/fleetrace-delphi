unit RiggVar.BR.PeerManager;

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
  RiggVar.BO.IniImage,
  RiggVar.BR.PeerController;

type
  TPeerManager = class
  private
    FPeerController: TPeerController;
    FProviderID: Integer;
    FMax: Integer;
    procedure SetProviderID(const Value: Integer);
    procedure InitPeer(const Value: Integer);
  protected
    function CreatePeer(BridgeProvider: Integer): TPeerController; virtual;
  public
    constructor Create(aBridgeProvider: Integer);
    destructor Destroy; override;
    procedure EditBridgeProvider; virtual;
    property PeerController: TPeerController read FPeerController;
    property ProviderID: Integer read FProviderID write SetProviderID;
  end;

implementation

uses
  RiggVar.BR.SwitchController,
  RiggVar.BR.BridgeController,
  RiggVar.BR.OutputController,
  RiggVar.App.Main;

{ TPeerManager }

constructor TPeerManager.Create(aBridgeProvider: Integer);
begin
  FMax := 3;
  InitPeer(aBridgeProvider);
  if FPeerController = nil then
    FPeerController := TPeerController.Create;
end;

destructor TPeerManager.Destroy;
begin
  FPeerController.Close;
  FPeerController.Free;
  inherited;
end;

procedure TPeerManager.EditBridgeProvider;
begin
  ProviderID := Main.FormAdapter.EditBridgeProviderID(ProviderID);
end;

function TPeerManager.CreatePeer(BridgeProvider: Integer): TPeerController;
begin
  case BridgeProvider of
    1: result := TSwitchController.Create;
    2: result := TBridgeController.Create;
    3: result := TOutputController.Create;
  else
    result := TPeerController.Create;
  end;
end;

procedure TPeerManager.InitPeer(const Value: Integer);
begin
  if (Value >= 0) and (Value <= FMax) then
  begin
    FPeerController := CreatePeer(Value);
    FProviderID := Value;
  end;
end;

procedure TPeerManager.SetProviderID(const Value: Integer);
var
  temp: TPeerController;
  NeedRecreateController: Boolean;
begin
  if (Value < 0) or (Value > FMax) then
    Exit;

  //e.g ServerBridge cannot recreated because PortCheck,  port already open
  NeedRecreateController := FPeerController.AllowRecreate;

  //always recreate if ControllerType changed
  if Value <> FProviderID then
    NeedRecreateController := True;

  if NeedRecreateController then
  begin
    FPeerController.Disconnect;
    FPeerController.Free;

    temp := CreatePeer(Value);

    FPeerController := temp;
    FPeerController.Connect;

    FProviderID := Value;
    Main.IniImage.BridgeProvider := Value;

    Main.InitPeer; //AutoPlugin, AutoUpload
  end;
end;

end.
