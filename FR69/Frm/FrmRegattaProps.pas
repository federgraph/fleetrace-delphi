unit FrmRegattaProps;

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
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  RiggVar.BO.EventProps;

type
  TFormRegattaProps = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    gbScoringSystemProps: TGroupBox;
    lblScoringSystem: TLabel;
    lblScoringSystemName: TLabel;
    cbScoringSystem: TComboBox;
    edScoringSystemName: TEdit;
    lblThrowoutScheme: TLabel;
    lblNumberOfThrowouts: TLabel;
    lblThrowoutsEditInfo: TLabel;
    cbThrowoutScheme: TComboBox;
    edThrowouts: TEdit;
    gbGeneralRegattaProps: TGroupBox;
    lblEventName: TLabel;
    lblEventDate: TLabel;
    lblOrganizer: TLabel;
    lblPRO: TLabel;
    lblJuryHead: TLabel;
    lblDivision: TLabel;
    edEventName: TEdit;
    edEventDate: TEdit;
    edHostClub: TEdit;
    edPRO: TEdit;
    edJuryHead: TEdit;
    cbDivision: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  FormRegattaProps: TFormRegattaProps;

function EditRegattaProps(Model: TEventProps): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

function EditRegattaProps(Model: TEventProps): Boolean;
begin
  result := False;
  if not Assigned(FormRegattaProps) then
    FormRegattaProps := TFormRegattaProps.Create(Application);

  FormRegattaProps.LoadModel(Model);
  if FormRegattaProps.ShowModal = mrOK then
  begin
    FormRegattaProps.SaveModel(Model);
    result := True;
  end;
end;

{ TFormRegattaProps }

procedure TFormRegattaProps.FormCreate(Sender: TObject);
begin
  Caption := 'Regatta Properties';
end;

procedure TFormRegattaProps.FormShow(Sender: TObject);
begin
  Main.LanguageManager.Localize(self);
end;

procedure TFormRegattaProps.LoadModel(Model: TEventProps);
var
  i: TScoringSystem;
begin
  edEventName.Text := Model.EventName;
  edEventDate.Text := Model.EventDates;
  edHostClub.Text := Model.HostClub;
  edPRO.Text := Model.PRO;
  edJuryHead.Text := Model.JuryHead;

  edThrowouts.Text := IntToStr(Model.Throwouts);

  cbScoringSystem.Clear;
  for i := Low(TScoringSystem) to High(TScoringSystem) do
  begin
    cbScoringSystem.Items.Add(JavaScore_ScoringSystemStrings[i]);
    if Model.ScoringSystem = i then
      cbScoringSystem.ItemIndex := Integer(i);
  end;
  edScoringSystemName.Text := IntToStr(Model.ScoringSystem2);

  cbThrowoutScheme.Clear;
  cbThrowoutScheme.Items.Add('By Num Races');
  cbThrowoutScheme.Items.Add('Per X Races');
  cbThrowoutScheme.Items.Add('Best X Races');
  cbThrowoutScheme.Items.Add('None');
  case Model.ThrowoutScheme of
    0: cbThrowoutScheme.ItemIndex := 0; //THROWOUT_BYNUMRACES
    1: cbThrowoutScheme.ItemIndex := 1; //THROWOUT_PERXRACES
    2: cbThrowoutScheme.ItemIndex := 2; //THROWOUT_BESTXRACES
    3: cbThrowoutScheme.ItemIndex := 3; //THROWOUT_NONE
  end;

  cbDivision.Clear;
  cbDivision.Items.Add('Opti');
  cbDivision.Items.Add('Cadet');
  cbDivision.Items.Add('Pirat');
  cbDivision.Items.Add('Snipe');
  cbDivision.Items.Add('420');
  cbDivision.Items.Add('470');
  cbDivision.Items.Add('470M');
  cbDivision.Items.Add('470W');
  cbDivision.Items.Add('505');
  cbDivision.Items.Add('Europe');
  cbDivision.Items.Add('Laser');
  cbDivision.Items.Add('LaserRadial');
  cbDivision.Items.Add('Finn');
  cbDivision.Items.Add('Star');
  cbDivision.Items.Add('Yngling');
  cbDivision.Items.Add('49er');
  cbDivision.Items.Add('Tornado');
  cbDivision.Items.Add('RSXM');
  cbDivision.Items.Add('RSXW');
  cbDivision.Items.Add('FD');
  cbDivision.Items.Add('H');
  cbDivision.Items.Add('O');
  cbDivision.Items.Add('P');
  cbDivision.Items.Add('R');
  cbDivision.Items.Add('XY');
  cbDivision.Items.Add('J22');
  cbDivision.Items.Add('J24');

  if cbDivision.Items.IndexOf(Model.DivisionName) < 0 then
    cbDivision.Items.Add(Model.DivisionName);
  cbDivision.Text := Model.DivisionName;
end;

procedure TFormRegattaProps.SaveModel(Model: TEventProps);
begin
  Model.EventName := edEventName.Text;
  Model.EventDates := edEventDate.Text;
  Model.HostClub := edHostClub.Text;
  Model.PRO := edPRO.Text;
  Model.JuryHead := edJuryHead.Text;

  Model.ScoringSystem := TScoringSystem(cbScoringSystem.ItemIndex);

  case cbThrowoutScheme.ItemIndex of
    0: Model.ThrowoutScheme := 0; //THROWOUT_BYNUMRACES;
    1: Model.ThrowoutScheme := 1; //THROWOUT_PERXRACES;
    2: Model.ThrowoutScheme := 2; //THROWOUT_BESTXRACES;
    3: Model.ThrowoutScheme := 3; //THROWOUT_NONE;
  end;

  Model.DivisionName := cbDivision.Text;
  Model.ScoringSystem2 := StrToIntDef(edScoringSystemName.Text, Model.ScoringSystem2);
end;

procedure TFormRegattaProps.OKBtnClick(Sender: TObject);
begin
  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

end.
