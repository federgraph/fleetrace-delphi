unit RiggVar.BO.Watches;

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

{ == Message-Context }

uses
  System.Types,
  System.SysUtils, 
  System.Classes, 
  Contnrs;

type
  TAdapterWatches = class
  protected
    FMsgInID: Integer;
    FMsgIn: string;
    FMsgInCount: Integer;
    FMsgOut: string;
    FMsgOutCount: Integer;
    FMsgOffset: Integer;
    procedure SetMsgIn(const Value: string);
    procedure SetMsgOut(const Value: string);
  public
    procedure Clear; virtual;
    procedure Update(LabelID: Integer); virtual;
    property MsgIn: string read FMsgIn write SetMsgIn;
    property MsgOut: string read FMsgOut write SetMsgOut;
    property MsgInCount: Integer read FMsgInCount;
    property MsgOutCount: Integer read FMsgOutCount;
  end;

  TGlobalWatches = class(TAdapterWatches)
  private
    FList: TObjectList; //Observers
  public
    constructor Create;
    destructor Destroy; override;
    procedure Subscribe(Subject: TAdapterWatches);
    procedure UnSubscribe(Subject: TAdapterWatches);
    procedure Update(LabelID: Integer); override;
  end;

  IWatchGUI = interface
  ['{C07A7836-79F3-4780-8F3D-5F21CA5921E2}']
    procedure Show;
    procedure Hide;
    function IsNew: Boolean;
    function IsVisible: Boolean;
    procedure UpdateFormCaption(Titel, EventName: string);
    procedure InitLabel(LabelID: Integer; Caption: string);
    procedure UpdateValue(LabelID: Integer; Content: string);
  end;

  TLocalWatches = class(TAdapterWatches)
  private
    FWatchGUI: IWatchGUI;
    FUndo: string;
    FRedo: string;
    procedure SetUndo(const Value: string);
    procedure SetRedo(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Init;
    procedure SetWatchGUI(Value: IWatchGUI);
    procedure Update(LabelID: Integer); override;
    procedure UpdateAll;
    procedure Show(EventName: string);
    property Undo: string read FUndo write SetUndo;
    property Redo: string read FRedo write SetRedo;
  end;

var
  GlobalWatches: TGlobalWatches;

implementation

{ TAdapterWatches }

procedure TAdapterWatches.Clear;
begin
  FMsgIn := '';
  FMsgInCount := 0;
  FMsgOut := '';
  FMsgOutCount := 0;
end;

procedure TAdapterWatches.SetMsgIn(const Value: string);
begin
  FMsgIn := Value;
  Inc(FMsgInCount);
  if FMsgInCount = -1 then
    FMsgInCount := 1;
  Update(FMsgOffset + 3);
end;

procedure TAdapterWatches.SetMsgOut(const Value: string);
begin
  FMsgOut := Value;
  Inc(FMsgOutCount);
  if FMsgOutCount = -1 then
    FMsgOutCount := 1;
  Update(FMsgOffset + 5);
end;

procedure TAdapterWatches.Update(LabelID: Integer);
begin
  //nothing here
end;

{ TGlobalWatches }

constructor TGlobalWatches.Create;
begin
  inherited Create;
  FList := TObjectList.Create(false);
end;

destructor TGlobalWatches.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TGlobalWatches.Subscribe(Subject: TAdapterWatches);
begin
  FList.Add(Subject);
end;

procedure TGlobalWatches.UnSubscribe(Subject: TAdapterWatches);
begin
  FList.Remove(Subject);
end;

procedure TGlobalWatches.Update(LabelID: Integer);
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
  begin
    TAdapterWatches(FList[i]).Update(LabelID);
  end;
end;

{ TLocalWatches }

constructor TLocalWatches.Create;
begin
  inherited Create;
  FMsgOffset := 4;
  GlobalWatches.Subscribe(self);
end;

destructor TLocalWatches.Destroy;
begin
  GlobalWatches.UnSubscribe(self);
  if FWatchGUI <> nil then
  begin
    FWatchGUI.Hide;
    FWatchGUI := nil;
  end;
  inherited Destroy;
end;

procedure TLocalWatches.Clear;
begin
  inherited Clear;
  FUndo := '';
  FRedo := '';
end;

procedure TLocalWatches.Init;
begin
  if Assigned(FWatchGUI) then
  begin
    FWatchGUI.InitLabel(1, 'Undo');
    FWatchGUI.InitLabel(2, 'Redo');
    FWatchGUI.InitLabel(3, 'AdapterMsgIn');
    FWatchGUI.InitLabel(4, 'AdapterMsgInCount');
    FWatchGUI.InitLabel(5, 'AdapterMsgOut');
    FWatchGUI.InitLabel(6, 'AdapterMsgOutCount');
    FWatchGUI.InitLabel(7, 'MsgIn');
    FWatchGUI.InitLabel(8, 'MsgInCount');
    FWatchGUI.InitLabel(9, 'MsgOut');
    FWatchGUI.InitLabel(10, 'MsgOutCount');
  end;
end;

procedure TLocalWatches.Update(LabelID: Integer);
begin
  if Assigned(FWatchGUI) and (FWatchGUI.IsVisible) then
  case LabelID of
    1: FWatchGUI.UpdateValue(1, Undo);
    2: FWatchGUI.UpdateValue(2, Redo);
    3:
    begin
      FWatchGUI.UpdateValue(3, GlobalWatches.MsgIn);
      FWatchGUI.UpdateValue(4, IntToStr(GlobalWatches.FMsgInCount));
    end;
    5:
    begin
      FWatchGUI.UpdateValue(5, GlobalWatches.MsgOut);
      FWatchGUI.UpdateValue(6, IntToStr(GlobalWatches.FMsgOutCount));
    end;
    7:
    begin
      FWatchGUI.UpdateValue(7, MsgIn);
      FWatchGUI.UpdateValue(8, IntToStr(FMsgInCount));
    end;
    9:
    begin
      FWatchGUI.UpdateValue(9, MsgOut);
      FWatchGUI.UpdateValue(10, IntToStr(FMsgOutCount));
    end;
  end;
end;

procedure TLocalWatches.UpdateAll;
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    Update(i);
  end;
end;

procedure TLocalWatches.SetUndo(const Value: string);
begin
  FUndo := Value;
  Update(1);
end;

procedure TLocalWatches.SetWatchGUI(Value: IWatchGUI);
begin
  FWatchGUI := Value;
end;

procedure TLocalWatches.SetRedo(const Value: string);
begin
  FRedo := Value;
  Update(2);
end;

procedure TLocalWatches.Show(EventName: string);
begin
  if Assigned(FWatchGUI) then
  begin
    FWatchGUI.Show;
    if (FWatchGUI.IsNew) then
    begin
      Init;
      if EventName <> '' then
        FWatchGUI.UpdateFormCaption('FR Watches', EventName);
    end;
    UpdateAll;
  end;
end;

initialization
  GlobalWatches := TGlobalWatches.Create;

finalization
  GlobalWatches.Free;

end.
