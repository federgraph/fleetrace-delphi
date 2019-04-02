unit RiggVar.EM.SpeedBtn;

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
  System.Classes,
  Vcl.Controls,
  RiggVar.EM.Intf;

type
  TItemLayout = class
  private
    FBtnClick: TNotifyEvent;
    FBtnCount: Integer;
    procedure SetBtnClick(const Value: TNotifyEvent);
    procedure SetBtnCount(const Value: Integer);
  protected
    procedure ClickHandler(Sender: TObject);
    procedure SetEnabled(const Value: Boolean); virtual;
  public
    EventCombo: IEventMenu;

    procedure CreateBar(AOwner: TComponent); virtual;
    procedure InitBar; virtual;
    procedure InitItems; virtual;
    procedure UpdateItems; virtual;
    procedure ClearItems; virtual;

    function GetItemTag(Sender: TObject): Integer; virtual;
    function GetItemCaption(Sender: TObject): string; virtual;

    property BtnCount: Integer read FBtnCount write SetBtnCount;
    property BtnClick: TNotifyEvent read FBtnClick write SetBtnClick;
    property Enabled: Boolean write SetEnabled;
  end;

implementation

{ TItemLayout }

procedure TItemLayout.ClearItems;
begin

end;

procedure TItemLayout.CreateBar(AOwner: TComponent);
begin
  Assert(AOwner is TWinControl);
end;

procedure TItemLayout.ClickHandler(Sender: TObject);
begin
  if Assigned(BtnClick) then
    BtnClick(Sender);
end;

procedure TItemLayout.InitBar;
begin

end;

procedure TItemLayout.InitItems;
begin

end;

procedure TItemLayout.SetBtnClick(const Value: TNotifyEvent);
begin
  FBtnClick := Value;
end;

procedure TItemLayout.SetBtnCount(const Value: Integer);
begin
  FBtnCount := Value;
end;

function TItemLayout.GetItemCaption(Sender: TObject): string;
begin
  result := '---'
end;

function TItemLayout.GetItemTag(Sender: TObject): Integer;
begin
  result := -1;
end;

procedure TItemLayout.SetEnabled(const Value: Boolean);
begin

end;

procedure TItemLayout.UpdateItems;
begin

end;

end.
