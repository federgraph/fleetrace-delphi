unit RiggVar.EM.SpeedBtn03;

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

{$ifdef FPC}
{$mode Delphi}
{$endif}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.UITypes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  RiggVar.EM.SpeedBtn;

type
  TSpeedPanelLayout = class(TItemLayout)
  private
    EventBtnPanel: TPanel;
  protected
    procedure SetEnabled(const Value: Boolean); override;
  public
    function GetItemTag(Sender: TObject): Integer; override;
    function GetItemCaption(Sender: TObject): string; override;

    procedure CreateBar(AOwner: TComponent); override;
    procedure InitBar; override;
    procedure InitItems; override;
    procedure UpdateItems; override;
    procedure ClearItems; override;
  end;

implementation

{ TSpeedPanelLayout }

procedure TSpeedPanelLayout.ClearItems;
var
  i: Integer;
begin
  for i := EventBtnPanel.ControlCount - 1 downto 0 do
    EventBtnPanel.Controls[i].Free;
  Assert(EventBtnPanel.ControlCount = 0,
    'EventBtnPanel.ControlCount not 0 in clear');
end;

procedure TSpeedPanelLayout.CreateBar(AOwner: TComponent);
begin
  inherited;
  EventBtnPanel := TPanel.Create(AOwner);
  EventBtnPanel.Parent := AOwner as TWinControl;
  EventBtnPanel.BevelOuter := bvNone;
end;

function TSpeedPanelLayout.GetItemCaption(Sender: TObject): string;
begin
  result := TSpeedButton(Sender).Caption;
end;

function TSpeedPanelLayout.GetItemTag(Sender: TObject): Integer;
begin
  result := TSpeedButton(Sender).Tag;
end;

procedure TSpeedPanelLayout.InitBar;
begin
  inherited;
  EventBtnPanel.Align := alClient;
  ClearItems;
end;

procedure TSpeedPanelLayout.InitItems;
var
  i: Integer;
  sb: TSpeedButton;
begin
  ClearItems;
  for i := 0 to BtnCount - 1 do
  begin
    sb := TSpeedButton.Create(EventBtnPanel);
    sb.Parent := EventBtnPanel;
    sb.Width := 75;
    sb.Height := 33;
    sb.Left := 2 + i * 77;
    sb.Top := 10;
    sb.Tag := i + 1;
    sb.Name := 'B' + IntToStr(i + 1);
    sb.Caption := sb.Name;
    sb.OnClick := ClickHandler;
    sb.ParentShowHint := false;
  end;
  Assert(EventBtnPanel.ControlCount = BtnCount, 'BtnCount does not match');
end;

procedure TSpeedPanelLayout.SetEnabled(const Value: Boolean);
begin
  EventBtnPanel.Enabled := Value;
end;

procedure TSpeedPanelLayout.UpdateItems;
var
  c: Integer;
  i: Integer;
  sb: TSpeedButton;
begin
  c := Min(EventBtnPanel.ControlCount, EventCombo.Count);
  for i := 0 to EventBtnPanel.ControlCount - 1 do
  begin
    if EventBtnPanel.Controls[i] is TSpeedButton then
    begin
      sb := EventBtnPanel.Controls[i] as TSpeedButton;
      if i < c then
      begin
        sb.Caption := EventCombo.GetCaption(i + 1);
        sb.Visible := true;
      end
      else
      begin
        sb.Caption := '';
        sb.Visible := false;
      end;
    end;
  end;
end;

end.
