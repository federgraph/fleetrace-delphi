unit FrmUniquaProps;

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
  RiggVar.BO.EventProps;

type
  TFormUniquaProps = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    gbUniqua: TGroupBox;
    lblGemeldet: TLabel;
    lblGezeitet: TLabel;
    lblGesegelt: TLabel;
    edGemeldet: TEdit;
    udGemeldet: TUpDown;
    edGezeitet: TEdit;
    udGezeitet: TUpDown;
    edGesegelt: TEdit;
    udGesegelt: TUpDown;
    cbLock: TCheckBox;
    lblFaktor: TLabel;
    edFaktor: TEdit;
    cbShowCupColumn: TCheckBox;
    procedure cbLockClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    UniquaGemeldet: Integer;
    UniquaGezeitet: Integer;
    UniquaGesegelt: Integer;
    FRGemeldet: Integer;
    FRGezeitet: Integer;
    FRGesegelt: Integer;
    DoClickEvent: Boolean;
    procedure EnableUniquaProps(Value: Boolean);
  public
    StrGemeldet: string;
    StrGezeitet: string;
    StrGesegelt: string;
    OnOK: TNotifyEvent;
    procedure LoadModel(Model: TEventProps);
    procedure SaveModel(Model: TEventProps);
  end;

var
  FormUniquaProps: TFormUniquaProps;

function EditUniquaProps(Model: TEventProps): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

function EditUniquaProps(Model: TEventProps): Boolean;
begin
  result := False;
  if not Assigned(FormUniquaProps) then
    FormUniquaProps := TFormUniquaProps.Create(Application);

  FormUniquaProps.LoadModel(Model);
  if FormUniquaProps.ShowModal = mrOK then
  begin
    FormUniquaProps.SaveModel(Model);
    result := True;
  end;
end;

{ TFormUniquaProps }

procedure TFormUniquaProps.LoadModel(Model: TEventProps);
begin
  Main.LanguageManager.Localize(self);

  //Cach both sets of Uniqua-Props
  UniquaGemeldet := Model.UniquaGemeldet;
  UniquaGezeitet := Model.UniquaGezeitet;
  UniquaGesegelt := Model.UniquaGesegelt;
  FRGemeldet := Model.FRGemeldet;
  FRGezeitet := Model.FRGezeitet;
  FRGesegelt := Model.FRGesegelt;
  //Labels
  lblGemeldet.Caption := Format('%s (%d)', [StrGemeldet, FRGemeldet]);
  lblGezeitet.Caption := Format('%s (%d)', [StrGezeitet, FRGezeitet]);
  lblGesegelt.Caption := Format('%s (%d)', [StrGesegelt, FRGesegelt]);
  //Edits
  if Model.EnableUniquaProps then
  begin
    udGemeldet.Position := UniquaGemeldet;
    udGezeitet.Position := UniquaGezeitet;
    udGesegelt.Position := UniquaGesegelt;
  end
  else
  begin
    udGemeldet.Position := FRGemeldet;
    udGezeitet.Position := FRGezeitet;
    udGesegelt.Position := FRGesegelt;
  end;
  EnableUniquaProps(Model.EnableUniquaProps);
  //Checkbox
  DoClickEvent := False;
  cbLock.Checked := Model.EnableUniquaProps;
  DoClickEvent := True;
  cbShowCupColumn.Checked := Model.ShowCupColumn;
  edFaktor.Text := FormatFloat('0.00', Model.Faktor);
end;

procedure TFormUniquaProps.SaveModel(Model: TEventProps);
begin
  Model.ShowCupColumn := cbShowCupColumn.Checked;
  Model.EnableUniquaProps := cbLock.Checked;
  if cbLock.Checked then
  begin
    //this set is current, get values from edits
    Model.UniquaGemeldet := udGemeldet.Position;
    Model.UniquaGezeitet := udGezeitet.Position;
    Model.UniquaGesegelt := udGesegelt.Position;
  end
  else
  begin
    //save cach, may be changed, so the user can continue to change values
    //from here later in this session, but do not persist to disk.
    Model.UniquaGemeldet := UniquaGemeldet;
    Model.UniquaGezeitet := UniquaGezeitet;
    Model.UniquaGesegelt := UniquaGesegelt;
  end;
  Model.Faktor := StrToFloatDef(edFaktor.Text, Model.Faktor);
end;

procedure TFormUniquaProps.cbLockClick(Sender: TObject);
begin
  if not DoClickEvent then exit;

  EnableUniquaProps(cbLock.Checked);

  if cbLock.Checked then
  begin
    //swap manual Uniqua Prop-Set
    udGemeldet.Position := UniquaGemeldet;
    udGezeitet.Position := UniquaGezeitet;
    udGesegelt.Position := UniquaGesegelt;
  end
  else
  begin
    //update manual Uniqua Prop-Set cache
    UniquaGemeldet := udGemeldet.Position;
    UniquaGezeitet := udGezeitet.Position;
    UniquaGesegelt := udGesegelt.Position;
    //and swap in constant, calculated FleetRace Prop-Set from cache
    udGemeldet.Position := FRGemeldet;
    udGezeitet.Position := FRGezeitet;
    udGesegelt.Position := FRGesegelt;
  end;
end;

procedure TFormUniquaProps.EnableUniquaProps(Value: Boolean);
begin
  edGesegelt.Enabled := Value;
  edGemeldet.Enabled := Value;
  edGezeitet.Enabled := Value;
end;

procedure TFormUniquaProps.OKBtnClick(Sender: TObject);
begin
  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

end.
