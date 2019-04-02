unit FrmWatches;

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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  RiggVar.BO.Watches;

type
  TFormWatches = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    ed1: TLabel;
    ed2: TLabel;
    ed3: TLabel;
    ed4: TLabel;
    ed5: TLabel;
    ed6: TLabel;
    ed7: TLabel;
    ed8: TLabel;
    ed9: TLabel;
    ed10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    procedure InitLabels(LabelID: Integer; Caption: string);
    procedure UpdateValue(LabelID: Integer; Content: string);
  end;

  TWatchGUI = class(TInterfacedObject, IWatchGUI)
  private
    FIsNew: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    function IsNew: Boolean;
    function IsVisible: Boolean;
    procedure UpdateFormCaption(Title, EventName: string);
    procedure InitLabel(LabelID: Integer; Caption: string);
    procedure UpdateValue(LabelID: Integer; Content: string);
  end;

implementation

{$R *.dfm}

var
  FormWatches: TFormWatches;

procedure TFormWatches.FormCreate(Sender: TObject);
begin
  ed1.Caption := '';
  ed2.Caption := '';
  ed3.Caption := '';
  ed4.Caption := '';
  ed5.Caption := '';
  ed6.Caption := '';
  ed7.Caption := '';
  ed8.Caption := '';
  ed9.Caption := '';
  ed10.Caption := '';
end;

procedure TFormWatches.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Escape then
    Close;
end;

procedure TFormWatches.InitLabels(LabelID: Integer; Caption: string);
begin
  case LabelID of
    1: Label1.Caption := Caption;
    2: Label2.Caption := Caption;
    3: Label3.Caption := Caption;
    4: Label4.Caption := Caption;
    5: Label5.Caption := Caption;
    6: Label6.Caption := Caption;
    7: Label7.Caption := Caption;
    8: Label8.Caption := Caption;
    9: Label9.Caption := Caption;
    10: Label10.Caption := Caption;
  end;
end;

procedure TFormWatches.UpdateValue(LabelID: Integer; Content: string);
begin
  case LabelID of
    1: ed1.Caption := Content;
    2: ed2.Caption := Content;
    3: ed3.Caption := Content;
    4: ed4.Caption := Content;
    5: ed5.Caption := Content;
    6: ed6.Caption := Content;
    7: ed7.Caption := Content;
    8: ed8.Caption := Content;
    9: ed9.Caption := Content;
    10: ed10.Caption := Content;
  end;
end;

{ TWatchGUI }

procedure TWatchGUI.Show;
begin
  if not Assigned(FormWatches) then
  begin
    FormWatches := TFormWatches.Create(nil);
  end;
  if Assigned(FormWatches) then
    FormWatches.Show;
end;

constructor TWatchGUI.Create;
begin
  FIsNew := True;
end;

destructor TWatchGUI.Destroy;
begin
  if Assigned(FormWatches) then
  begin
    FormWatches.Free;
    FormWatches := nil;
  end;
  inherited;
end;

procedure TWatchGUI.Hide;
begin
  if Assigned(FormWatches) then
    FormWatches.Hide;
end;

function TWatchGUI.IsNew: Boolean;
begin
  result := FIsNew;
end;

function TWatchGUI.IsVisible: Boolean;
begin
  if Assigned(FormWatches) then
    result := FormWatches.Visible
  else
    result := false;
end;

procedure TWatchGUI.UpdateFormCaption(Title, EventName: string);
begin
  if Assigned(FormWatches) then
    FormWatches.Caption := Title + ' ' + EventName;
end;

procedure TWatchGUI.InitLabel(LabelID: Integer; Caption: string);
begin
  if Assigned(FormWatches) then
  begin
    FormWatches.InitLabels(LabelID, Caption);
    FIsNew := False;
  end;
end;

procedure TWatchGUI.UpdateValue(LabelID: Integer; Content: string);
begin
  if Assigned(FormWatches) then
    FormWatches.UpdateValue(LabelID, Content);
end;

end.
