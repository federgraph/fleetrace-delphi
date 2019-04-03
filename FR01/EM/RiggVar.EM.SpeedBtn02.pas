unit RiggVar.EM.SpeedBtn02;

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
  System.Math,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  RiggVar.Grid.Color,
  RiggVar.EM.SpeedBtn;

  type
  TSpeedBarLayout = class(TItemLayout)
  private
    MenuToolBar: TToolBar;
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

{ TSpeedBarLayout }

procedure TSpeedBarLayout.ClearItems;
var
  i: Integer;
begin
  for i := MenuToolBar.ControlCount - 1 downto 0 do
    MenuToolBar.Controls[i].Free;
  Assert(MenuToolBar.ControlCount = 0, 'ToolBar.ControlCount not 0 in clear');
end;

procedure TSpeedBarLayout.CreateBar(AOwner: TComponent);
begin
  inherited;
  MenuToolBar := TToolBar.Create(AOwner);
  MenuToolBar.Parent := AOwner as TWinControl;
  MenuToolBar.EdgeBorders := [];
end;

function TSpeedBarLayout.GetItemCaption(Sender: TObject): string;
begin
  result := TSpeedButton(Sender).Caption;
end;

function TSpeedBarLayout.GetItemTag(Sender: TObject): Integer;
begin
  result := TSpeedButton(Sender).Tag;
end;

procedure TSpeedBarLayout.InitBar;
var
  tb: TToolBar;
begin
  tb := MenuToolBar;
  tb.ButtonHeight := 40;
  tb.Align := alClient;
  tb.ShowCaptions := true;
  tb.Wrapable := false;
  tb.Font.Height := -14;
  tb.Font.Name := 'Comic Sans MS';
  tb.Font.Color := clEventBtn;
  ClearItems;
end;

procedure TSpeedBarLayout.InitItems;
var
  i: Integer;
  sb: TSpeedButton;
begin
  ClearItems;
  for i := BtnCount - 1 downto 0 do
  begin
    sb := TSpeedButton.Create(MenuToolBar);
    sb.Parent := MenuToolBar;
    sb.Width := 75;
    sb.Height := 33;
    sb.Tag := i + 1;
    sb.Name := 'B' + IntToStr(i + 1);
    sb.Caption := sb.Name;
    sb.OnClick := ClickHandler;
  end;
  Assert(MenuToolBar.ControlCount = BtnCount, 'BtnCount does not match');
end;

procedure TSpeedBarLayout.SetEnabled(const Value: Boolean);
begin
  MenuToolBar.Enabled := Value;
end;

procedure TSpeedBarLayout.UpdateItems;
var
  h: Integer;
  c: Integer;
  i: Integer;
  sb: TSpeedButton;
  t: string;
begin
  c := Min(MenuToolBar.ControlCount, EventCombo.Count);
  h := MenuToolBar.ControlCount - 1;
  for i := 0 to MenuToolBar.ControlCount - 1 do
  begin
    if MenuToolBar.Controls[h - i] is TSpeedButton then
    begin
      sb := MenuToolBar.Controls[h - i] as TSpeedButton;
      if i < c then
      begin
        t := EventCombo.GetCaption(i + 1);
        sb.Caption := t;
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
