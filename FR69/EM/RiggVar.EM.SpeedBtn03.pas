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
    FPanel: TPanel;
    FStyleID: Integer;
    procedure SetStyleID(const Value: Integer);
  protected
    sbWidth: Integer;
    sbHeight: Integer;
    sbTop: Integer;
    sbMargin: Integer;
    procedure InitStyle;
    procedure SetEnabled(const Value: Boolean); override;
  public
    function GetItemTag(Sender: TObject): Integer; override;
    function GetItemCaption(Sender: TObject): string; override;

    procedure CreateBar(AOwner: TComponent); override;
    procedure InitBar; override;
    procedure InitItems; override;
    procedure UpdateItems; override;
    procedure ClearItems; override;
    property StyleID: Integer read FStyleID write SetStyleID;
  end;

implementation

{ TSpeedPanelLayout }

procedure TSpeedPanelLayout.ClearItems;
var
  i: Integer;
begin
  InitStyle;
  for i := FPanel.ControlCount - 1 downto 0 do
    FPanel.Controls[i].Free;
  Assert(FPanel.ControlCount = 0,
    'Panel.ControlCount not 0 in ClearItems');
end;

procedure TSpeedPanelLayout.CreateBar(AOwner: TComponent);
begin
  inherited;
  FPanel := TPanel.Create(AOwner);
  FPanel.Parent := AOwner as TWinControl;
  FPanel.BevelOuter := bvNone;
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
  FPanel.Align := alClient;
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
    sb := TSpeedButton.Create(FPanel);
    sb.Parent := FPanel;
    sb.Width := sbWidth;
    sb.Height := sbHeight;
    sb.Top := sbTop;
    sb.Left := sbMargin + i * (sbMargin + sbWidth);
    sb.Tag := i + 1;
    sb.Name := 'B' + IntToStr(i + 1);
    sb.Caption := sb.Name;
    sb.OnClick := ClickHandler;
    sb.ParentShowHint := false;
  end;
  Assert(FPanel.ControlCount = BtnCount, 'BtnCount does not match');
end;

procedure TSpeedPanelLayout.SetEnabled(const Value: Boolean);
begin
  FPanel.Enabled := Value;
end;

procedure TSpeedPanelLayout.SetStyleID(const Value: Integer);
begin
  FStyleID := Value;
  InitStyle;
end;

procedure TSpeedPanelLayout.UpdateItems;
var
  c: Integer;
  i: Integer;
  sb: TSpeedButton;
begin
  c := Min(FPanel.ControlCount, EventCombo.Count);
  for i := 0 to FPanel.ControlCount - 1 do
  begin
    if FPanel.Controls[i] is TSpeedButton then
    begin
      sb := FPanel.Controls[i] as TSpeedButton;
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

procedure TSpeedPanelLayout.InitStyle;
begin
  case FStyleID of
    1:
    begin
      sbMargin := 2;
      sbWidth := 75;
      sbHeight := 28;
      sbTop := 5;
    end;

    else
    begin
      sbMargin := 2;
      sbWidth := 75;
      sbHeight := 33;
      sbTop := 10;
    end;

  end;
end;

end.
