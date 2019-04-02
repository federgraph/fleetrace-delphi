unit RiggVar.Conn.MsgRingImpl;

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
  RiggVar.Conn.MsgRingIntf;

type
  TMsgRing01 = class(TMsgRing)
  private
    procedure AddLine(SwitchID: Integer; const s: string);
    procedure MergeLine(SwitchID: Integer; const s: string);
  protected
    SL: TStringList;
    SLMerge: TStringList;
    FLogValid: Boolean;
  public
    constructor Create(Capacity: Integer);
    destructor Destroy; override;
    procedure GetDelta(SwitchID: Integer; DiffLog: TStrings; StartMsgID: Integer); override;
    procedure AddMsg(SwitchID: Integer; s: string); override;
    function LogValid(): Boolean; override;
    procedure Clear; override;
  end;

implementation

{ TMsgRing01 }

constructor TMsgRing01.Create(Capacity: Integer);
begin
  inherited Create(Capacity);
  SL := TStringList.Create;
  SLMerge := TStringList.Create;
  FLogValid := True;
  UseMerge := True;
end;

destructor TMsgRing01.Destroy;
begin
  SL.Free;
  SLMerge.Free;
  inherited;
end;

procedure TMsgRing01.Clear;
begin
  SL.Clear;
  FLogValid := True;
  MsgID := 0;
end;

procedure TMsgRing01.AddMsg(SwitchID: Integer; s: string);
begin
  Inc(MsgID);

  if UseMerge then
    MergeLine(SwitchID, s)
  else
    AddLine(SwitchID, s);

  {
  SL.Add(IntToStr(SwitchID) + '=' + s);
  if SL.Count > FCapacity then
  begin
    SL.Delete(0);
    FLogValid := False;
  end;
  }
end;

procedure TMsgRing01.AddLine(SwitchID: Integer; const s: string);
begin
  SL.AddObject(s, TObject(SwitchID));
  if SL.Count > FCapacity then
  begin
    SL.Delete(0);
    FLogValid := False;
  end;
end;

procedure TMsgRing01.MergeLine(SwitchID: Integer; const s: string);
var
  sK: string;
  sV: string;
  temp: string;
  i: Integer;
  LogMsgIndex: Integer;
begin
  SLMerge.Clear;
  i := Pos('=', s);
  if i > 0 then
    temp := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)))
  else
    temp := StringReplace(Trim(s), ' ', '_', [rfReplaceAll]);

  if Pos('=', temp) = 0 then
    temp := temp + '=';
  SLMerge.Add(temp);

  sK := SLMerge.Names[0];
  sV := SLMerge.Values[sK];
  //StringReplace(sV, '_', ' ', [rfReplaceAll]);

  LogMsgIndex := SL.IndexOfName(sK);
  if LogMsgIndex >= 0 then
  begin
    SL[LogMsgIndex] := sK + '=' + sV;
    SL.Objects[LogMsgIndex] := TObject(SwitchID);
  end
  else
  begin
    if SL.Count > FCapacity then
      SL.Delete(0);
    //SL.Add(sK + '=' + sV);
    SL.AddObject(sK + '=' + sV, TObject(SwitchID));
  end;
end;

procedure TMsgRing01.GetDelta(SwitchID: Integer; DiffLog: TStrings; StartMsgID: Integer);
var
  i: Integer;
  MsgCount: Integer;
  StartValue: Integer;
//  tempSwitchID: Integer;
begin
  if LogValid() then
  begin
    MsgCount := MsgID - StartMsgID;
    if MsgCount <= 0 then
      exit;

    StartValue := SL.Count - MsgCount;
    if StartValue < 0 then
      StartValue := 0;

    for i := StartValue to SL.Count - 1 do
    begin
      //if StrToInt(SL.Names[i]) <> SwitchID then
      //  DiffLog.Add(SL.ValueFromIndex[i]);

      //SwitchID is no longer evaluated
      //Silverlight-Client (always) uses SwitchID=0
      //and so will not receive messages sent from any Silverlight-Client
      //but I want all messages always,
      //so that bridge difflog shows all changes
      //so that download gets all changes in FRIA03

      //tempSwitchID := Integer(SL.Objects[i]);
      //if tempSwitchID <> SwitchID then
        DiffLog.Add(SL[i]);
    end;
  end;
end;

function TMsgRing01.LogValid(): Boolean;
begin
  result := FLogValid;
end;

end.
