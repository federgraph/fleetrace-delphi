unit RiggVar.Conn.StatusFeedback;

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
  RiggVar.Util.Classes,
  IdUdpClient;

type
  TStatusFeedBack = class(TLineParser)
  private
    udp: TIdUdpClient;
  protected
    function ParseKeyValue(Key: string; Value: string): Boolean; override;
  public
    Enabled: Boolean;
    Host: string;
    Port: Integer;
    procedure SendStatus(Msg: string);
    destructor Destroy; override;
  end;

implementation

uses
  RiggVar.BO.Def;

{ TStatusFeedBack }

destructor TStatusFeedBack.Destroy;
begin
  udp.Free;
  inherited;
end;

function TStatusFeedBack.ParseKeyValue(Key, Value: string): Boolean;
begin
  result := True;

  if Pos('Manage.', Key) = 1 then
    Key := Copy(Key, Length('Manage.') + 1, Length(Key));

  if Key = 'StatusTrigger' then
  begin
    SendStatus(BO.GetHash);
  end
  else if Key = 'Clear' then
  begin
    BO.ClearCommand;
  end
  else if Key = 'Calc' then
  begin
    BO.Calc;
  end
  else if Key = 'Feedback.Host' then
    Host := Value
  else if Key = 'Feedback.Port' then
    Port := StrToIntDef(Value, Port)
  else
    result := False;
end;

procedure TStatusFeedBack.SendStatus(Msg: string);
begin
  if Enabled then
    try
      if udp = nil then
      begin
        udp := TIdUdpClient.Create(nil);
        udp.Host := Host;
        udp.Port := Port;
        udp.Active := True;
      end;
      if (udp.Host <> Host) or (udp.Port <> Port) then
      begin
        udp.Active := False;
        udp.Host := Host;
        udp.Port := Port;
        udp.Active := True;
      end;
      udp.Send(Msg);
    except
      Enabled := False;
    end;
end;

end.
