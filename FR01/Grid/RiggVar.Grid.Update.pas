unit RiggVar.Grid.Update;

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
  System.DateUtils;

type
  TGridUpdate = class
  private
    FDelay: Integer;
    FOnUpdateView: TNotifyEvent;
    procedure UpdateView;
    procedure SetOnUpdateView(const Value: TNotifyEvent);
    procedure SetDelay(const Value: Integer);
  public
    FLastUpdateTime: TDateTime;
    IMarker: Boolean;
    ViewEnabled: Boolean;
    procedure DoOnIdle;
    procedure HandleInform;
    procedure DelayUpdate;
    procedure InvalidateView;
    constructor Create;
    property OnUpdateView: TNotifyEvent read FOnUpdateView write SetOnUpdateView;
    property Delay: Integer read FDelay write SetDelay;
  end;

implementation

{ TGridUpdate }

constructor TGridUpdate.Create;
begin
  Delay := 1000;
end;

procedure TGridUpdate.DelayUpdate;
begin
  if not IMarker then
  begin
    FLastUpdateTime := Now;
  end
  else
    IMarker := False;
end;

procedure TGridUpdate.InvalidateView;
begin
  FLastUpdateTime := IncMilliSecond(FLastUpdateTime, -(Delay+100));
  IMarker := True;
end;

procedure TGridUpdate.DoOnIdle;
var
  Hour, Min, Sec, MSec: Word;
begin
  if ViewEnabled then
  begin
    DecodeTime(Now - FLastUpdateTime, Hour, Min, Sec, MSec);
    if Sec * 1000 + MSec > Delay then
    begin
      UpdateView;
      FLastUpdateTime := Now;
    end;
  end;
end;

procedure TGridUpdate.UpdateView;
begin
  if Assigned(OnUpdateView) then
    OnUpdateView(Self);
end;

procedure TGridUpdate.HandleInform;
begin
  if ViewEnabled then
    UpdateView;
end;

procedure TGridUpdate.SetDelay(const Value: Integer);
begin
  if Value > 100 then
  begin
    FDelay := Value;
  end
  else
    FDelay := 100;
end;

procedure TGridUpdate.SetOnUpdateView(const Value: TNotifyEvent);
begin
  FOnUpdateView := Value;
end;

end.
