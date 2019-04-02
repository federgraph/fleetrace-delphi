unit RiggVar.App.TestCounter;

interface

uses
  System.SysUtils,
  System.Classes;

type
  TTestCounter = class
  private
    SL: TStrings;
    procedure Add(s: string; i: Integer);
  public
    Temp: Integer;
    MsgCreateCounter: Integer;
    MsgDestroyCounter: Integer;
    MsgDispatchCounter: Integer;
    MsgErrorCounter: Integer;
    ContextMsgCreateCounter: Integer;
    ContextMsgDestroyCounter: Integer;
    MsgQueueAddCounter: Integer;
    MsgQueueRemoveCounter: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Show(ML: TStrings);
  end;

implementation

{ TTestCounter }

constructor TTestCounter.Create;
begin
  SL := TStringList.Create;
end;

destructor TTestCounter.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TTestCounter.Add(s: string; i: Integer);
begin
  SL.Add(Format('%s = %d', [s, i]));
end;

procedure TTestCounter.Clear;
begin
  MsgCreateCounter := 0;
  MsgDestroyCounter := 0;
  MsgDispatchCounter := 0;
  MsgErrorCounter := 0;
  ContextMsgCreateCounter := 0;
  ContextMsgDestroyCounter := 0;
  MsgQueueAddCounter := 0;
  MsgQueueRemoveCounter := 0;
  Temp := 0;
end;

procedure TTestCounter.Show(ML: TStrings);
var
  i: Integer;
begin
  SL.Clear;

  Add('Temp', Temp);
  Add('MsgCreateCounter', MsgCreateCounter);
  Add('MsgDestroyCounter', MsgDestroyCounter);
  Add('MsgDispatchCounter', MsgDispatchCounter);
  Add('MsgErrorCounter', MsgErrorCounter);
  Add('ContextMsgCreateCounter', ContextMsgCreateCounter);
  Add('ContextMsgDestroyCounter', ContextMsgDestroyCounter);
  Add('MsgQueueAddCounter', MsgQueueAddCounter);
  Add('MsgQueueRemoveCounter', MsgQueueRemoveCounter);

  for i := 0 to SL.Count-1 do
    ML.Add(SL[i]);

  SL.Clear;
end;

end.
