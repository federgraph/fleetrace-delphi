unit RiggVar.BR.BridgeSynchron;

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

{
  used as BaseClass for IndyBridge
}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.BR.BridgeAbstract;

type
  TSynchronBridge = class(TBridge)
  protected
    SL: TStringList;
    procedure Post(msg: string); virtual;
    function Request(msg: string): string; virtual;
  public
    constructor Create(Capacity: Integer);
    destructor Destroy; override;
    //
    function Plugin(): Integer; override;
    procedure Plugout(SwitchID: Integer); override;
    //
    function SendBackupAndLog(SwitchID: Integer; Backup: string; Log: string): Integer; override;
    procedure SendDiffLog(SwitchID: Integer; DiffLog: string); override;
    procedure SendMsg(SwitchID: Integer; msg: string); override;
    //
    function GetBackup: string; override;
    function GetNewMessages(SwitchID: Integer; StartMsgID: Integer): string; override;
    //
    function LogValid: Boolean; override;
    //
    function GetBackupSwitchID: Integer; override;
    function GetLastBackupID: Integer; override;
    function GetLastMsgID: Integer; override;
    function CheckForBackup(SwitchID: Integer; StartBackupID: Integer): Boolean; override;
    function CheckForLog(SwitchID: Integer; StartMsgID: Integer): Boolean; override;
  end;

implementation

uses
  RiggVar.Util.Classes;

{ TSynchronBridge }

constructor TSynchronBridge.Create(Capacity: Integer);
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TSynchronBridge.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TSynchronBridge.Plugin: Integer;
var
  s: string;
  i: Integer;
begin
  s := Request('Plugin');
  i := StrToIntDef(s, -1);
  result := i;
end;

procedure TSynchronBridge.Plugout(SwitchID: Integer);
begin
  SL.Clear;
  SL.Add('Plugout');
  SL.Add(IntToStr(SwitchID));
  Post(SL.Text);
end;

function TSynchronBridge.SendBackupAndLog(SwitchID: Integer; Backup,
  Log: string): Integer;
var
  s: string;
  i: Integer;
begin
  //Log is always '' (in case of ResultServer)
  SL.Text := Backup;
  SL.Insert(0,'SendBackup');
  SL.Insert(0, IntToStr(SwitchID));
  s := Request(SL.Text);
  i := StrToIntDef(s, -1);
  result := i;
end;

procedure TSynchronBridge.SendDiffLog(SwitchID: Integer; DiffLog: string);
begin
  SL.Text := DiffLog;
  SL.Insert(0,'SendDiffLog');
  SL.Insert(0, IntToStr(SwitchID));
  Post(SL.Text);
end;

procedure TSynchronBridge.SendMsg(SwitchID: Integer; msg: string);
begin
  SL.Clear;
  SL.Add('SendMsg');
  SL.Add(IntToStr(SwitchID));
  SL.Add(msg); //was missing, detected 17.10.2007
  Post(SL.Text);
end;

function TSynchronBridge.GetBackup: string;
var
  s: string;
begin
  s := Request('GetBackup');
  if (s = 'e') then
    result := ''
  else
    result := s;
end;

function TSynchronBridge.GetNewMessages(SwitchID, StartMsgID: Integer): string;
var
  s: string;
begin
  SL.Clear;
  SL.Add('GetNewMessages');
  SL.Add(IntToStr(SwitchID));
  SL.Add(IntToStr(StartMsgID));
  s := Request(SL.Text);
  if (s = 'e') then
    result := ''
  else
    result := s;
end;

function TSynchronBridge.LogValid: Boolean;
var
  s: string;
begin
  SL.Clear;
  SL.Add('LogValid');
  s := Request(SL.Text);
  result := TUtils.IsTrue(s);
end;

function TSynchronBridge.GetBackupSwitchID: Integer;
var
  s: string;
  i: Integer;
begin
  s := Request('LogValid');
  i := StrToIntDef(s, -1);
  result := i;
end;

function TSynchronBridge.GetLastBackupID: Integer;
var
  s: string;
  i: Integer;
begin
  s := Request('GetLastBackupID');
  i := StrToIntDef(s, -1);
  result := i;
end;

function TSynchronBridge.GetLastMsgID: Integer;
var
  s: string;
  i: Integer;
begin
  s := Request('GetLastMsgID');
  i := StrToIntDef(s, -1);
  result := i;
end;

function TSynchronBridge.CheckForBackup(SwitchID,
  StartBackupID: Integer): Boolean;
var
  s: string;
begin
  SL.Clear;
  SL.Add('CheckForBackup');
  SL.Add(IntToStr(SwitchID));
  SL.Add(IntToStr(StartBackupID));
  s := Request(SL.Text);
  result := TUtils.IsTrue(s);
end;

function TSynchronBridge.CheckForLog(SwitchID, StartMsgID: Integer): Boolean;
var
  s: string;
begin
  SL.Clear;
  SL.Add('CheckForLog');
  SL.Add(IntToStr(SwitchID));
  SL.Add(IntToStr(StartMsgID));
  s := Request(SL.Text);
  result := TUtils.IsTrue(s);
end;

procedure TSynchronBridge.Post(msg: string);
begin
end;

function TSynchronBridge.Request(msg: string): string;
begin
end;

end.
