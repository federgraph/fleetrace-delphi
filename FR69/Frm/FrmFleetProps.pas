unit FrmFleetProps;

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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  RiggVar.BO.EventProps;

type
  TFormFleetProps = class(TForm)
    LabelTargetFleetSize: TLabel;
    cbUseFleets: TCheckBox;
    edTargetFleetSize: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    edFirstFinalRace: TEdit;
    LabelFirstFinalRace: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    OnOK: TNotifyEvent;
    procedure LoadModel(Model: TEventProps);
    procedure SaveModel(Model: TEventProps);
  end;

var
  FormFleetProps: TFormFleetProps;

function EditFleetProps(Model: TEventProps): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

function EditFleetProps(Model: TEventProps): Boolean;
begin
  result := False;
  if not Assigned(FormFleetProps) then
    FormFleetProps := TFormFleetProps.Create(Application);

  FormFleetProps.LoadModel(Model);
  if FormFleetProps.ShowModal = mrOK then
  begin
    FormFleetProps.SaveModel(Model);
    result := True;
  end;
end;

{ TFormFleetProps }

procedure TFormFleetProps.FormShow(Sender: TObject);
begin
  //Main.Localizer.Localize(self);
end;

procedure TFormFleetProps.LoadModel(Model: TEventProps);
begin
  cbUseFleets.Checked := Model.UseFleets;
  edFirstFinalRace.Text := IntToStr(Model.FirstFinalRace);
  edTargetFleetSize.Text := IntToStr(Model.TargetFleetSize);
end;

procedure TFormFleetProps.SaveModel(Model: TEventProps);
begin
  Model.UseFleets := cbUseFleets.Checked;
  Model.FirstFinalRace := StrToIntDef(edFirstFinalRace.Text, Model.FirstFinalRace);
  Model.TargetFleetSize := StrToIntDef(edTargetFleetSize.Text, Model.TargetFleetSize);
end;

procedure TFormFleetProps.OKBtnClick(Sender: TObject);
begin
  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

end.
