unit RiggVar.EM.SpeedBtn01;

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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  RiggVar.EM.SpeedBtn;

type
  TToolButtonLayout = class(TItemLayout)
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

{ TToolButtonBar }

procedure TToolButtonLayout.ClearItems;
var
  i: Integer;
begin
  for i := MenuToolBar.ControlCount - 1 downto 0 do
    MenuToolBar.Controls[i].Free;
  Assert(MenuToolBar.ControlCount = 0, 'ToolBar.ControlCount not 0 in clear');
end;

procedure TToolButtonLayout.CreateBar(AOwner: TComponent);
begin
  inherited;
  MenuToolBar := TToolBar.Create(AOwner);
  MenuToolBar.Parent := AOwner as TWinControl;
  MenuToolBar.EdgeBorders := [];
end;

function TToolButtonLayout.GetItemCaption(Sender: TObject): string;
begin
  result := TToolButton(Sender).Caption;
end;

function TToolButtonLayout.GetItemTag(Sender: TObject): Integer;
begin
  result := TToolButton(Sender).Tag;
end;

procedure TToolButtonLayout.InitBar;
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
  tb.Font.Color := clGreen;
  ClearItems;
end;

procedure TToolButtonLayout.InitItems;
var
  i: Integer;
  sb: TToolButton;
begin
  ClearItems;
  for i := BtnCount - 1 downto 0 do
  begin
    sb := TToolButton.Create(MenuToolBar);
    sb.Parent := MenuToolBar;
    sb.Tag := i + 1;
    sb.Name := 'B' + IntToStr(i + 1);
    sb.Caption := sb.Name;
    sb.OnClick := ClickHandler;
  end;
  Assert(MenuToolBar.ControlCount = BtnCount, 'BtnCount does not match');
end;

procedure TToolButtonLayout.SetEnabled(const Value: Boolean);
begin
  MenuToolBar.Enabled := Value;
end;

procedure TToolButtonLayout.UpdateItems;
var
  h: Integer;
  c: Integer;
  i: Integer;
  sb: TToolButton;
  t: string;
begin
  c := Min(MenuToolBar.ControlCount, EventCombo.Count);
  h := MenuToolBar.ControlCount - 1;
  for i := 0 to MenuToolBar.ControlCount - 1 do
  begin
    if MenuToolBar.Controls[h - i] is TToolButton then
    begin
      sb := MenuToolBar.Controls[h - i] as TToolButton;
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
