unit RiggVar.Web3.FeatureMap;

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
  System.Classes;

type
  TFeatureMap = class
  private
    SL: TStringList;
    procedure AddComponentEntry(CID: string; Port: Integer; Path: string;
      IsConnected: Boolean; IsHome: Boolean);
  protected
    function GetXml: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Xml: string read GetXml;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.BR.PeerController,
  RiggVar.Util.Classes;

{ TFeatureMapXml }

constructor TFeatureMap.Create;
begin
  SL := TStringList.Create;
end;

destructor TFeatureMap.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TFeatureMap.AddComponentEntry(
  CID: string; Port: Integer; Path: string; IsConnected: Boolean; IsHome: Boolean);
var
  fs: string;
  p: string;
begin
  if Path = '' then
  begin
    fs := '<Component CID="%s" Port="%d" IsConnected="%s" />';
    SL.Add(Format(fs, [CID, Port, BoolStr[IsConnected]]));
  end
  else if Path <> '' then
  begin
    p := Path;
    if TUtils.StartsWith(p, '/') then
      p := Copy(Path, 2, MaxInt);
    if p[Length(p)] <> '/' then
      p := p + '/';

    if Main.IsScenarioEC2 then
    begin
    end
    else if Main.Params.UseProxyBase then
    begin
      if IsHome then
        p := Main.IniImage.HomeProxyAuthority + Main.IniImage.HomeProxyBase + p
      else
        p := Main.IniImage.RemoteProxyAuthority + Main.IniImage.RemoteProxyBase + p
    end;

    fs := '<Component CID="%s" Port="%d" Path="%s" IsConnected="%s" />';
    SL.Add(Format(fs, [CID, Port, p, BoolStr[IsConnected]]));
  end
end;

function TFeatureMap.GetXml: string;
var
  p: Integer;
  c: Boolean;
  s: string;
  h: Boolean;
begin
  SL.Clear;
  SL.Add('<?xml version="1.0" ?>');
  if Main.Params.UseProxyBase then
    s := Main.IniImage.ProxyDomain
  else
    s := Main.GuiManager.RemoteRouter.Host;
  SL.Add(Format('<FeatureMap Host="%s">', [s]));
  SL.Add('<PortMap>');

  try
    if Assigned(Main.AdapterBO) and Main.BOManager.Connected then
    begin
      h := false;
      c := Main.BOManager.BOConnected;

      p := Main.AdapterBO.InputServer.Server.Port;
      AddComponentEntry('IS', p, '', c, h);

      p := Main.AdapterBO.OutputServer.Server.Port;
      AddComponentEntry('OS', p, '', c, h);
    end;

    if Main.IsServerBridge then
    begin
      //p := 4530;
      h := false;
      p := Main.ServerBridge.BridgeNCP.Server.Port;
      c := Main.PeerController.IsEnabled(SwitchOp_Plugout);
      AddComponentEntry('BS', p, '', c, h);
    end;

    if Assigned(Main.PolicyProvider.PolicyNCP) then
    begin
      h := false;
      //p := 943;
      p := Main.PolicyProvider.PolicyNCP.Server.Port;
      c := true;
      AddComponentEntry('PS', p, '', c, h);
    end;

    if Assigned(Main.GuiManager.WebMotorHome) then
    begin
      h := true;
      //p := 8086;
      p := Main.GuiManager.WebMotorHome.Port;
      c := Main.GuiManager.WebMotorHome.IsActive;
      s := Main.GuiManager.WebMotorHome.Path;
      AddComponentEntry('HW', p, s, c, h);
    end;

    if Assigned(Main.GuiManager.WebMotorRemote) then
    begin
      h := false;
      //p := 9086;
      p := Main.GuiManager.WebMotorRemote.Port;
      c := Main.GuiManager.WebMotorRemote.IsActive;
      s := Main.GuiManager.WebMotorRemote.Path;
      AddComponentEntry('RW', p, s, c, h);
    end;

    if Assigned(Main.ServerBridge) and Assigned(Main.ServerBridge.BridgeWeb) then
    begin
      h := false;
      //p := 8087;
      p := Main.ServerBridge.BridgeWeb.Port;
      c := Main.ServerBridge.BridgeWeb.IsActive;
      s := Main.ServerBridge.BridgeWeb.Path;
      AddComponentEntry('BW', p, s, c, h);
    end;

    if Assigned(Main.GuiManager.SilverlightWeb) then
    begin
      h := true;
      //p := 4510;
      p := Main.GuiManager.SilverlightWeb.Port;
      c := Main.GuiManager.SilverlightWeb.IsActive;
      s := Main.GuiManager.SilverlightWeb.Path;
      AddComponentEntry('SW', p, s, c, h);
    end;

  except
  end;

  SL.Add('</PortMap>');
  SL.Add('</FeatureMap>');
  result := SL.Text;
  SL.Clear;
end;

end.
