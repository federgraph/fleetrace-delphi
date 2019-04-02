unit RiggVar.BO.UndoManager;

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
  RiggVar.Grid.ColGrid,
  RiggVar.BO.TemplateIDs,
  RiggVar.BO.MsgTree,
  RiggVar.BO.MsgToken,
  RiggVar.BO.Watches,
  RiggVar.Conn.Def;

type
  TUndoAgent = class
  private
    function GetUndoFlag: Boolean;
    procedure WriteUndoFlag(const Value: Boolean);
  protected
    procedure SaveMsg(Sender: TObject; s: string);
  public
    UndoMsg: string;
    RedoMsg: string;
    MsgTree: TMsgTree;
    IsUndo: Boolean;
    constructor Create;
    destructor Destroy; override;
    property UndoFlag: Boolean read GetUndoFlag write WriteUndoFlag;
  end;

  TUndoManager = class
  private
    FIndex: Integer;
    BaseList: TStringList;
    LogList: TStringList;
    CurrentList: TStringList;
    UndoList: TStringList;
    RedoList: TStringList;
    function GetBOF: Boolean;
    function GetEOF: Boolean;
    function GetRedoCount: Integer;
    function GetUndoCount: Integer;
    function GetLogCount: Integer;
    function SubList(SL: TStrings; Index1, Index2: Integer): string;
  public
    ProxyXML: TStringList; //buffer xml for debugging purpose only
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Trim;
    procedure AddMsg(const UndoMsg, RedoMsg: string);
    procedure UpdateBase(s: string);
    procedure UpdateCurrent(s: string);
    function Undo: string;
    function Redo: string;
    function GetBase: string;
    function GetCurrent: string;
    function GetLog: string;
    function GetUndo: string;
    function GetRedo: string;
    function GetUndoRedo: string;
    property EOF: Boolean read GetEOF;
    property BOF: Boolean read GetBOF;
    property UndoCount: Integer read GetUndoCount;
    property RedoCount: Integer read GetRedoCount;
    property LogCount: Integer read GetLogCount;
  end;
implementation

uses
  RiggVar.BO.Def;

var
  FUndoFlag: Boolean; //if undo or redo button was pressed

{ TUndoManager }

constructor TUndoManager.Create;
begin
  inherited Create;
  ProxyXML := TStringList.Create;
  BaseList := TStringList.Create;
  LogList := TStringList.Create;
  CurrentList := TStringList.Create;
  UndoList := TStringList.Create;
  RedoList := TStringList.Create;
end;

destructor TUndoManager.Destroy;
begin
  ProxyXML.Free;
  BaseList.Free;
  LogList.Free;
  CurrentList.Free;
  UndoList.Free;
  RedoList.Free;
  inherited;
end;

procedure TUndoManager.Clear;
begin
  UndoList.Clear;
  RedoList.Clear;
  LogList.Clear;
  CurrentList.Clear;
  //BaseList.Clear; ###
  FIndex := 0;
end;

procedure TUndoManager.AddMsg(const UndoMsg, RedoMsg: string);
begin
  Trim;
  UndoList.Add(UndoMsg);
  RedoList.Add(RedoMsg);
  LogList.Add(RedoMsg);
  FIndex := UndoList.Count;
  BO.LocalWatches.Undo := UndoMsg;
  BO.LocalWatches.Redo := RedoMsg;

  //Avoid endless loop: do not broadcast messages from Switch.
  if not (SwitchLocked or WebLocked) then
    BO.OutputServer.InjectMsg(nil, msUndoRedo, RedoMsg);
end;

function TUndoManager.GetBOF: Boolean;
begin
  result := FIndex < 1;
end;

function TUndoManager.GetEOF: Boolean;
begin
  result := FIndex >= UndoList.Count;
end;

function TUndoManager.GetRedoCount: Integer;
begin
  result := RedoList.Count - FIndex;
end;

function TUndoManager.GetUndoCount: Integer;
begin
  result := FIndex;
end;

function TUndoManager.GetLogCount: Integer;
begin
 result := LogList.Count;
end;

function TUndoManager.Redo: string;
begin
  if (FIndex >= 0) and (FIndex < RedoList.Count) then
  begin
    result := RedoList[FIndex];
    FIndex := FIndex + 1;
    LogList.Add(result);
  end
  else
    result := '';
end;

function TUndoManager.Undo: string;
begin
  if (FIndex > 0) and (FIndex <= UndoList.Count) then
  begin
    FIndex := FIndex - 1;
    result := UndoList[FIndex];
    LogList.Add(result);
  end
  else
    result := '';
end;

procedure TUndoManager.UpdateBase(s: string);
begin
  BaseList.Text := s;
end;

procedure TUndoManager.UpdateCurrent(s: string);
begin
  CurrentList.Text := s;
end;

function TUndoManager.GetBase: string;
begin
  result := BaseList.Text;
end;

function TUndoManager.GetCurrent: string;
begin
  result := CurrentList.Text;
end;

function TUndoManager.GetLog: string;
begin
  result := LogList.Text;
  if result = '' then
    result := 'Log list is empty.'
end;

function TUndoManager.GetRedo: string;
begin
  result := SubList(RedoList, FIndex, RedoList.Count - 1);
  if result = '' then
    result := 'Redo sublist is empty.'
end;

function TUndoManager.GetUndo: string;
begin
  result := SubList(UndoList, 0, FIndex-1);
  if result = '' then
    result := 'Undo sublist is empty.'
end;

function TUndoManager.SubList(SL: TStrings; Index1, Index2: Integer): string;
var
  TL: TStrings;
  i: Integer;
begin
  result := '';
  If (Index1 >= 0) and (Index2 < SL.Count) then
  begin
    TL := TStringList.Create;
    for i := Index1 to Index2 do
    begin
      TL.Add(SL[i]);
    end;
    result := TL.Text;
    TL.Free;
  end;
end;

function TUndoManager.GetUndoRedo: string;
var
  TL: TStrings;
  i: Integer;
begin
  result := '';
  If (UndoList.Count > 0) then
  begin
    TL := TStringList.Create;
    for i := 0 to UndoList.Count-1 do
    begin
      TL.Add(UndoList[i] + ' | ' + RedoList[i]);
    end;
    result := TL.Text;
    TL.Free;
  end;
  if result = '' then
    result := 'UndoRedo combined list is empty.'
end;

procedure TUndoManager.Trim;
var
  i: Integer;
begin
  for i := UndoList.Count-1 downto FIndex do
  begin
    UndoList.Delete(i);
    RedoList.Delete(i);
  end;
end;

{ TUndoableBO }

constructor TUndoAgent.Create;
var
  InputAction: TInputAction;
begin
  inherited Create;
  InputAction := TInputAction.Create;
  MsgTree := TMsgTree.Create(nil, cTokenA, UndoActionID);
  InputAction.OnSend := SaveMsg;
  TInputActionManager.UndoActionRef := InputAction;
end;

destructor TUndoAgent.Destroy;
begin
  MsgTree.Free;
  TInputActionManager.UndoActionRef.Free;
  TInputActionManager.UndoActionRef := nil;
  inherited;
end;

function TUndoAgent.GetUndoFlag: Boolean;
begin
  result := FUndoFlag or BO.Loading;
end;

procedure TUndoAgent.SaveMsg(Sender: TObject; s: string);
begin
  if IsUndo then
  begin
    UndoMsg := s;
    IsUndo := False;
  end
  else
  begin
    RedoMsg := s;
    BO.UndoManager.AddMsg(UndoMsg, RedoMsg);
  end;
end;

procedure TUndoAgent.WriteUndoFlag(const Value: Boolean);
begin
  FUndoFlag := Value;
end;

end.
