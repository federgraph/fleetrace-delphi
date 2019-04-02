unit RiggVar.Conn.IO;

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

{$define UsingAdapterBO}

uses
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  RiggVar.Conn.Def,
  RiggVar.Conn.Intern,
  RiggVar.BO.MsgToken,
  RiggVar.BO.MsgBase;

type
  TContextMsgQueue = class
  private
    FList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(cm: TContextMsg);
    function Dequeue: TContextMsg;
    function Count: Integer;
  end;

  TInputNCP = class(TBaseNCP)
  private
    procedure Trace(Sender: TObject; s: string);
  public
    MsgQueue: TContextMsgQueue;
    constructor Create(ts: TServerIntern);
    destructor Destroy; override;
    procedure InjectMsg(Sender: TObject; ms: TMsgSource; s: string); override;
    procedure HandleMsg(cm: TContextMsg); override;
    procedure ProcessQueue;
  end;

  TOutputNCP = class(TBaseNCP)
  public
    procedure HandleMsg(cm: TContextMsg); override;
    procedure InjectMsg(Sender: TObject; ms: TMsgSource; s: string); override;
    procedure SendMsg(cm: TContextMsg);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TContextMsgQueue }

constructor TContextMsgQueue.Create;
begin
  inherited Create;
  FList := TObjectList.Create(False);
end;

destructor TContextMsgQueue.Destroy;
var
  cm: TContextMsg;
begin
  while FList.Count > 0 do
  begin
    cm := Dequeue;
    if not cm.IsOwned then
      cm.Free;
  end;
  FList.Free;
  inherited;
end;

procedure TContextMsgQueue.Enqueue(cm: TContextMsg);
begin
  FList.Add(cm);
  cm.IsQueued := True;
  Inc(Main.TestCounter.MsgQueueAddCounter);
end;

function TContextMsgQueue.Dequeue: TContextMsg;
begin
  result := nil;
  if FList.Count > 0 then
  begin
    result := TContextMsg(FList[0]);
    FList.Delete(0);
    Inc(Main.TestCounter.MsgQueueRemoveCounter);
  end;
end;

function TContextMsgQueue.Count: Integer;
begin
  result := FList.Count;
end;

{ TInputNCP }

constructor TInputNCP.Create(ts: TServerIntern);
begin
  inherited Create(ts);
  MsgQueue := TContextMsgQueue.Create;
end;

destructor TInputNCP.Destroy;
begin
  MsgQueue.Free;
  inherited;
end;

procedure TInputNCP.Trace(Sender: TObject; s: string);
//var
//  so: IConnection;
begin
//  so := nil;
//  if Assigned(Sender) then
//    Sender.GetInterface(IConnection, so);
//  if so <> nil then
//    Main.Logger.LogInt(s, so.GetPort)
//  else
    Main.Logger.Info(s);
end;

procedure TInputNCP.InjectMsg(Sender: TObject; ms: TMsgSource; s: string);
var
  cm: TContextMsg;
begin
  Inc(Counter);
  cm := TContextMsg.Create;
  cm.MsgSource := ms;
  cm.Sender := Sender;
  cm.msg := s;
  HandleMsg(cm);
  if not cm.IsQueued then
    cm.Free;
end;

procedure TInputNCP.HandleMsg(cm: TContextMsg);
var
  msg: TBaseMsg;
begin
  Trace(cm.Sender, 'I: ' + cm.msg);
  msg := BO.NewMsg;
  msg.prot := cm.msg;
  if msg.DispatchProt then
  begin
    if Main.Params.WantAutoSync then
      Main.GuiManager.SynchronizeCache;

    //Quittung senden
    if (msg.MsgResult = 0) and (msg.DBID > 0) then
    begin
{$ifdef UsingAdapterBO}
      Main.AdapterBO.InputServer.Server.Reply(cm.Sender, IntToStr(msg.DBID));
{$endif}
    end;

    if cm.IsSwitchMsg and (msg.OutputRequestList.Count = 0) then
    begin
      //was vom Switch kommt nicht weitersenden,
      //aber trigger eventuell (bei SKK) Calc und Paint
      if Main.IniImage.WantInputNCPCalc then
      begin
        BO.Calc;
        BO.OutputServer.SendMsg(cm);
      end;
    end
    else
    begin
      //msg erst nach Neuberechnung weitersenden
      if msg.Cmd = cTokenA + '.' + cTokenB + '.Output.' then
      begin
        cm.HasRequest := True;
        if msg.OutputRequestList.Count > 1 then
        begin
          cm.OutputRequestList := TStringList.Create;
          cm.OutputRequestList.Assign(msg.OutputRequestList);
        end
//        else if msg.OutputRequestList.Count = 1 then
//          cm.msg := msg.OutputRequestList[0]
        else
          cm.msg := msg.MsgValue;
     end;
     MsgQueue.Enqueue(cm);
    end;
  end;
  msg.Free;

  SwitchLocked := false;
end;

procedure TInputNCP.ProcessQueue;
var
  cm: TContextMsg;
begin
  while MsgQueue.Count > 0 do
  begin
    cm := MsgQueue.Dequeue;
    if Assigned(cm) then
    begin
      if cm.HasRequest then
      begin
        if Assigned(cm.OutputRequestList)  then
        begin
          Assert(cm.OutputRequestList.Count > 1);
          cm.Answer := BO.Output.GetAll(cm.OutputRequestList);
        end
        else
        begin
          cm.Answer := BO.Output.GetMsg(cm.msg);
        end;
{$ifdef UsingAdapterBO}
        if cm.IsAdapterMsg then
          Main.AdapterBO.InputServer.Server.Reply(cm.Sender, cm.Answer)
        else
{$endif}
          Server.Reply(cm.Sender, cm.Answer);
      end
      else if Assigned(BO.OutputServer) then
      begin
        BO.OutputServer.SendMsg(cm);
      end;

      Assert(cm.IsOwned = False);

      if not cm.IsOwned then
        cm.Free;
    end;
  end;
end;

{ TOutputNCP }

procedure TOutputNCP.HandleMsg(cm: TContextMsg);
begin
  //eventuell hier den allgemeine Token durch den speziellen ersetzen
  //direkt antworten auf Anfragen am Ausgang (ohne Berechnung)
  Server.Reply(cm.Sender, BO.Output.GetMsg(cm.msg));
end;

procedure TOutputNCP.SendMsg(cm: TContextMsg);
begin
  BO.Watches.MsgOut := cm.msg;
  Server.SendMsg(cm);
  Main.Logger.Info('O: ' + cm.msg);
end;

procedure TOutputNCP.InjectMsg(Sender: TObject; ms: TMsgSource; s: string);
var
  cm: TContextMsg;
begin
  Inc(Counter);
  cm := TContextMsg.Create;
  cm.MsgSource := ms;
  cm.Sender := Sender;
  cm.msg := s;
  SendMsg(cm);
  cm.Free;
end;

end.
