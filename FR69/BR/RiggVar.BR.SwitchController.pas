unit RiggVar.BR.SwitchController;

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
  Winapi.WinSock,
  System.SysUtils,
  System.Classes,
{$IFNDEF VER150}
  IdIOHandlerStack,
{$ENDIF}
  IdHTTP,
  RiggVar.Conn.Def,
  RiggVar.Conn.ClientMsg,
  RiggVar.BO.TemplateIDs,
  RiggVar.BR.PeerController;

type
  TSwitchController = class(TPeerController)
  private
    procedure Plug(mOperation: string);
    function GetLocalHostName: string;
  protected
    function GetConnected: Boolean; override;
    function GetHomeUrl: string; override;
    function GetIsMaster: Boolean; override;
  public
    procedure EditProps; override;
    function IsEnabled(Op: TSwitchOp): Boolean; override;
    //
    procedure Plugin; override;
    procedure Plugout; override;
    procedure Synchronize; override;
    procedure Upload(s: string); override;
    function Download: string; override;
    //
    procedure Close; override;
    procedure GetStatusReport(Memo: TStrings); override;
  end;

implementation

uses
  RiggVar.App.Main;

procedure TSwitchController.Plug(mOperation: string);
var
  mHost: string;
  mPortIn: Integer;
  mPortOut: Integer;
  mEventType: Integer;
  m: string;
begin
  if Assigned(Main.AdapterBO) then
  begin
    mHost := GetLocalHostName;
    mPortIn := Main.AdapterBO.InputServer.Server.Port;
    mPortOut := Main.AdapterBO.OutputServer.Server.Port;
    mEventType := Main.IniImage.DefaultEventType;
    m := mHost + ',' +
      IntToStr(mPortIn) + ',' +
      IntToStr(mPortOut) + ',' +
      IntToStr(PlugInTypeServer) + ',' +
      mOperation + ',' +
      IntToStr(mEventType);
    RiggVar.Conn.ClientMsg.SendMsg(Main.IniImage.SwitchHost, Main.IniImage.SwitchPort, m, false, 2000);
  end;
end;

procedure TSwitchController.Upload(s: string);
begin
  if Connected then
  begin
    Main.AdapterBO.InputServer.Server.Reply(SwitchSender, SwitchTokenUpload);
    Main.AdapterBO.InputServer.Server.Reply(SwitchSender, s);
  end;
end;

procedure TSwitchController.Synchronize;
begin
  if Connected then
  begin
    Main.AdapterBO.InputServer.Server.Reply(SwitchSender, SwitchTokenSynchronize);
  end;
end;

{$IFDEF VER150}
function TSwitchController.Download: string;
var
  c: TIdHTTP;
  h: string;
  et: Integer;
begin
  result := '';
  h := Main.IniImage.SwitchHost;
  et := Main.IniImage.DefaultEventType;
  c := TIdHTTP.Create(nil);
  try
    try
      c.Host := Main.IniImage.SwitchHost;
      c.Port := Main.IniImage.SwitchPortHTTP;
      c.Connect(1000);
      result := c.Get(SwitchTokenData + '?EventType=' + IntToStr(et));
      c.Disconnect;
    finally
      c.free;
    end;
  except
    result := '';
  end;
end;
{$ELSE}
function TSwitchController.Download: string;
var
  c: TIdHTTP;
  ioh: TIdIOHandlerStack;
  url: string;
  h: string;
  p: Integer;
  et: Integer;
begin
  result := '';
  h := Main.IniImage.SwitchHost;
  p := Main.IniImage.SwitchPortHttp;
  et := Main.IniImage.DefaultEventType;
  c := TIdHTTP.Create(nil);
  try
    try
      ioh := TIdIOHandlerStack.Create(c);
      ioh.ConnectTimeout := 1000;
      c.IOHandler := ioh;
      c.Connect(h, p);
      url := Format('http://%s:%d/%s?EventType=%d', [h, p, SwitchTokenData, et]);
      result := c.Get(url);
      c.Disconnect;
    finally
      c.free;
    end;
  except
    result := '';
  end;
end;
{$ENDIF}

procedure TSwitchController.EditProps;
begin
  Main.FormAdapter.EditSwitchProps(Main.IniImage);
end;

procedure TSwitchController.Close;
begin
  if PlugTouched then
    Plug(SwitchTokenDisconnect);
end;

procedure TSwitchController.Plugin;
begin
  PlugTouched := True;
  Plug(SwitchTokenConnect);
end;

procedure TSwitchController.Plugout;
begin
  PlugTouched := False;
  Plug(SwitchTokenDisconnect);
end;

function TSwitchController.GetLocalHostName: string;
var
  LocalName: array [0..255] of AnsiChar;
begin
  result := 'localhost';
  if Main.IniImage.UseRouterHost then
    result := Main.IniImage.RouterHost
  else
  begin
    //assumes that WSAStartup has been called (by Indy)
    if GetHostname(@LocalName, SizeOf(LocalName)) = 0 then
      result := string(LocalName);
  end;
end;

procedure TSwitchController.GetStatusReport(Memo: TStrings);
begin
  Memo.Add('ClassType: TSwitchController');
end;

function TSwitchController.IsEnabled(Op: TSwitchOp): Boolean;
begin
  case Op of
    SwitchOp_Plugin:
      result := true;
    SwitchOp_Plugout:
      result := true;
    SwitchOp_Synchronize:
      result := IsMaster;
    SwitchOp_Upload:
      result := Connected and IsMaster;
    SwitchOp_Download:
      result := true;
    else
      result := false;
  end;
end;

function TSwitchController.GetConnected: Boolean;
begin
  result := PlugTouched and (SwitchSender <> nil);
end;

function TSwitchController.GetHomeUrl: string;
begin
  result := Format('http://%s:%d/%s',
  [Main.IniImage.SwitchHost, Main.IniImage.SwitchPortHTTP, 'connections']);
end;

function TSwitchController.GetIsMaster: Boolean;
begin
  result := Main.IniImage.IsMaster;
end;

end.

