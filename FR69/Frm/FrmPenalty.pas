unit FrmPenalty;

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
  Vcl.ExtCtrls,
  Vcl.CheckLst,
  RiggVar.Scoring.Penalty,
  RiggVar.BO.PenaltyISAF,
  RiggVar.Util.Classes;

type
  TFormPenalty = class(TForm)
    rgPenaltyDSQ: TRadioGroup;
    rgPenaltyNoFinish: TRadioGroup;
    cbPenaltyOther: TCheckListBox;
    LabelPenaltyOther: TLabel;
    RulerLabel: TLabel;
    BitsLabel: TLabel;
    rgPenaltyOther: TRadioGroup;
    edPenaltyOther: TEdit;
    LabelPenaltyOtherValue: TLabel;
    AddPenaltyOtherBtn: TButton;
    RemovePenaltyOtherBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    StringLabel: TLabel;
    ClearBtn: TButton;
    MsgLabel: TLabel;
    Memo: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    ClearStaticsBtn: TButton;
    Label1: TLabel;
    procedure rgPenaltyDSQClick(Sender: TObject);
    procedure rgPenaltyNoFinishClick(Sender: TObject);
    procedure AddPenaltyOtherBtnClick(Sender: TObject);
    procedure RemovePenaltyOtherBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ClearStaticsBtnClick(Sender: TObject);
  private
    SL: TStringList;
    p: TPenaltyISAF;
    function GetPenaltyCode: string;
    procedure Parse(s: string);
  public
    procedure LoadModel(Model: TPenaltyISAF);
    procedure SaveModel(Model: TPenaltyISAF);
  end;

function EditPenalty(Model: TPenaltyISAF): Boolean;

var
  FormPenalty: TFormPenalty;

implementation

{$R *.dfm}

function EditPenalty(Model: TPenaltyISAF): Boolean;
begin
  result := False;
  if not Assigned(FormPenalty) then
    FormPenalty := TFormPenalty.Create(Application);

  FormPenalty.LoadModel(Model);
  if FormPenalty.ShowModal = mrOK then
  begin
    FormPenalty.SaveModel(Model);
    result := True;
  end;
end;

{ TFormPenalty }

procedure TFormPenalty.FormCreate(Sender: TObject);
begin
  p := TPenaltyISAF.Create;
  SL := TStringList.Create;
end;

procedure TFormPenalty.FormDestroy(Sender: TObject);
begin
  p.Free;
  SL.Free;
end;

procedure TFormPenalty.LoadModel(Model: TPenaltyISAF);
begin
  //p.Assign(Model);
  self.rgPenaltyDSQ.ItemIndex := Integer(p.PenaltyDSQ);
  self.rgPenaltyNoFinish.ItemIndex := Integer(p.PenaltyNoFinish);

  cbPenaltyOther.Checked[0] := TIM in p.PenaltyOther;
  cbPenaltyOther.Checked[1] := ZFP in p.PenaltyOther;
  cbPenaltyOther.Checked[2] := AVG in p.PenaltyOther;
  cbPenaltyOther.Checked[3] := SCP in p.PenaltyOther;
  cbPenaltyOther.Checked[4] := RDG in p.PenaltyOther;
  cbPenaltyOther.Checked[5] := MAN in p.PenaltyOther;
  cbPenaltyOther.Checked[6] := CNF in p.PenaltyOther;
  cbPenaltyOther.Checked[7] := TMP in p.PenaltyOther;
  cbPenaltyOther.Checked[8] := DPI in p.PenaltyOther;

  StringLabel.Caption := p.ToString;
  BitsLabel.Caption := GetPenaltyCode;

  SL.Clear;
  SL.Add(Format('AsInteger: Integer = %d', [p.AsInteger]));
  SL.Add(Format('IsOK: Boolean = %s', [BoolStr[p.IsOK]]));
  SL.Add(Format('IsOut: Boolean = %s', [BoolStr[p.IsOut]]));
  SL.Add(Format('Points: double = %g', [p.Points]));
  SL.Add(Format('Percent: Integer = %d', [p.Percent]));
  SL.Add(Format('TimePenalty: Int64 = %d', [p.TimePenalty]));
  SL.Add(Format('DSQPending: Boolean = %s', [BoolStr[p.IsDSQPending]]));
  SL.Add(Format('Note: string = %s', [p.Note]));
  SL.Add('');
  {
	SL.Add(Format('static sLastTime: string = %s', [sLastTime]));
  SL.Add(Format('static sLastDays: Integer = %d', [sLastDays]));
  SL.Add(Format('static sIsLongDistance: Boolean = %s', [BoolStr[sIsLongDistance]]));
  SL.Add(Format('static sTimeZero: Int64 = %d', [sTimeZero]));
	//SL.Add(Format('static dateFmt: string = %s', [dateFmt])); //'dd-MMM-yyyy';
  SL.Add(Format('static sDayMillis: Int64 = %d', [sDayMillis]));
  }
  Memo.Text := SL.Text;
end;

procedure TFormPenalty.SaveModel(Model: TPenaltyISAF);
begin
  //
end;

procedure TFormPenalty.rgPenaltyDSQClick(Sender: TObject);
begin
  Parse(rgPenaltyDSQ.Items[rgPenaltyDSQ.ItemIndex]);
end;

procedure TFormPenalty.rgPenaltyNoFinishClick(Sender: TObject);
begin
  Parse(rgPenaltyNoFinish.Items[rgPenaltyNoFinish.ItemIndex]);
end;

procedure TFormPenalty.AddPenaltyOtherBtnClick(Sender: TObject);
begin
  Parse(rgPenaltyOther.Items[rgPenaltyOther.ItemIndex] + '/' + edPenaltyOther.Text);
end;

procedure TFormPenalty.RemovePenaltyOtherBtnClick(Sender: TObject);
begin
  Parse('-' + rgPenaltyOther.Items[rgPenaltyOther.ItemIndex]);
end;

function TFormPenalty.GetPenaltyCode: string;
var
  i, j, k: Integer;
  b: Boolean;
begin
  j := p.AsInteger;
  result := '';
  for i := 15 downto 0 do
  begin
    k := 1 shl i;
    b := (j and k) > 0;
    if b then
      result := result + '1'
    else
      result := result + '0';
  end;
end;

procedure TFormPenalty.ClearBtnClick(Sender: TObject);
begin
  p.AsInteger := 0;
  LoadModel(p);
end;

procedure TFormPenalty.Parse(s: string);
begin
  MsgLabel.Caption := s;
  p.Parse(s);
  LoadModel(p);
end;

procedure TFormPenalty.ClearStaticsBtnClick(Sender: TObject);
begin
  {
  sDayMillis := 24 * 60 * 60 * 1000; // number of milliseconds in a 24 hour day
  sLastTime := '00:00:00';
  sLastDays := 0;
  //sTimeZero := SailTime.forceToLong('0/00:00:00');
  sIsLongDistance := false;
  }
  LoadModel(p);
end;

end.
