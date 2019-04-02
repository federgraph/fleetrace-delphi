unit RiggVar.BR.OutputController;

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
  Vcl.Graphics,
  RiggVar.BR.PeerController,
  RiggVar.Conn.ResultClient,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern;

type
  TOutputController = class(TPeerController)
  private
    SL: TStringList;
    MsgCounterIn: Integer;
    MsgCounterOut: Integer;
    LED_Brush_Color: TColor;
    ResultClient: TResultClient;
    procedure CreateClient;
    procedure HandleConnectedChanged(Sender: TObject);
    procedure PaintLED;
  protected
    function GetConnected: Boolean; override;
    procedure HandleMsg(Sender: TObject; msg: string);
    function CheckBackup: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EditProps; override;
    procedure GetStatusReport(Memo: TStrings); override;

    procedure Plugin; override;
    procedure Plugout; override;
    function Download: string; override;

    function IsEnabled(Op: TSwitchOp): Boolean; override;
    procedure UpdateStatusBar(Memo: TStrings);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BO.IniImage,
  RiggVar.Util.Classes,
  RiggVar.Util.Sound;

{ TOutputController }

constructor TOutputController.Create;
begin
  inherited Create;
  SL := TStringList.Create;
  CreateClient;
  LED_Brush_Color := clRed;
end;

destructor TOutputController.Destroy;
begin
  Plugout;
  ResultClient.Free;
  SL.Free;
  inherited Destroy;
end;

procedure TOutputController.Plugin;
begin
  if (Main.IniImage.OutputHost = ResultClient.Client.Host)
    and (Main.IniImage.OutputPort = ResultClient.Client.Port)then
    ResultClient.Connect
  else
  begin
    CreateClient;
    ResultClient.Connect;
  end;
  LED_Brush_Color := clYellow;
end;

procedure TOutputController.Plugout;
begin
  ResultClient.Disconnect;
  PaintLED;
end;

procedure TOutputController.CreateClient;
begin
  if Assigned(ResultClient) then
    ResultClient.Free;
  ResultClient := nil;

  ResultClient := TResultClient.Create(Main.IniImage.OutputHost, Main.IniImage.OutputPort);
  ResultClient.Client.OnHandleMsg := HandleMsg;
  ResultClient.Client.OnConnectedChanged := HandleConnectedChanged;
end;

procedure TOutputController.HandleConnectedChanged(Sender: TObject);
begin
  PaintLED;
end;

function TOutputController.IsEnabled(Op: TSwitchOp): Boolean;
begin
  case Op of
    SwitchOp_Plugin:
      result := LED_Brush_Color <> clLime; //not ResultClient.Connected;
    SwitchOp_Plugout:
      result := LED_Brush_Color = clLime; //ResultClient.Connected;
    SwitchOp_Synchronize:
      result := false;
    SwitchOp_Upload:
      result := false;
    SwitchOp_Download:
      result := LED_Brush_Color = clLime; //ResultClient.Connected;
    else
      result := false;
  end;
end;

procedure TOutputController.PaintLED;
begin
  if ResultClient.Connected then
    LED_Brush_Color := clLime
  else
    LED_Brush_Color := clRed;
end;

procedure TOutputController.UpdateStatusBar(Memo: TStrings);
begin
  Memo.Add('Host: ' + ResultClient.Client.Host);
  Memo.Add('Port: ' + IntToStr(ResultClient.Client.Port));
  Memo.Add('In: ' + IntToStr(MsgCounterIn));
  Memo.Add('Out: ' + IntToStr(MsgCounterOut));
end;

function TOutputController.Download: string;
begin
  if ResultClient.Connected then
  begin
    ResultClient.Client.SendMsg('FR.*.Output.CSV.Data.B');
    Inc(MsgCounterOut);
  end;
  result := ''; //asynchron, do nothing now, call DoOnBackup later
end;

procedure TOutputController.HandleMsg(Sender: TObject; msg: string);
var
  cm: TContextMsg;
begin
  cm := TContextMsg.Create;
  cm.MsgSource := msBridge;
  cm.Sender := Sender;
  cm.msg := msg;
  cm.DecodeHeader;

  SL.Text := cm.msg;

  if SL.Count = 1 then
  begin
    Main.GuiManager.PlaySound(Sound_Click03);
    Main.InjectClientBridgeMsg(cm.msg);
    Inc(MsgCounterIn);
  end
  else if CheckBackup then
  begin
    DoOnBackup(msg);
    Inc(MsgCounterIn);
  end;

  if not cm.IsQueued then
    cm.Free;

  SL.Clear;
end;

function TOutputController.CheckBackup: Boolean;
var
  i: Integer;
  p1: Boolean;
  p2: Boolean;
  line: string;
begin
//  //remove <pre> and </pre> lines (when .CVM. is used
//  for i := SL.Count - 1 downto 0 do
//  begin
//    if StartsWith(SL[i], '<') then
//      SL.Delete(i);
//  end;

  //plausibility test - params must be present in Backup
  p1 := False;
  p2 := False;
  i := 0;
  while (i < SL.Count - 1) and not (p1 and p2) do
  begin
    Inc(i);
    line := SL[i];
    if Pos('StartlistCount', line) > 0 then
      p1 := true;
    if Pos('RaceCount', line) > 0 then
      p2 := true;
  end;

  result := p1 and p2;
end;

procedure TOutputController.EditProps;
begin
  Main.FormAdapter.EditConnectionProps(csOutput);
end;

function TOutputController.GetConnected: Boolean;
begin
  result := LED_Brush_Color = clLime;
end;

procedure TOutputController.GetStatusReport(Memo: TStrings);
begin
  inherited;
  Memo.Add('ClassType: TOutputController');
  Memo.Add('Host: ' + ResultClient.Client.Host);
  Memo.Add('Port: ' + IntToStr(ResultClient.Client.Port));
  Memo.Add('In: ' + IntToStr(MsgCounterIn));
  Memo.Add('Out: ' + IntToStr(MsgCounterOut));
end;

end.
