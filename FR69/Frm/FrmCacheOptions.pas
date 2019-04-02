unit FrmCacheOptions;

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
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  RiggVar.BO.CacheMotor;

type
  TFormCacheOptions = class(TForm)
    TrackBar: TTrackBar;
    LabelIdleDelay: TLabel;
    OKBtn: TButton;
    LabelTicks: TLabel;
    NewValue: TLabel;
    OldLabel: TLabel;
    NewLabel: TLabel;
    OldValue: TLabel;
    cbActive: TCheckBox;
    procedure TrackBarChange(Sender: TObject);
  private
    function TickToMillies(Tick: Integer): Integer;
    function MilliesToTick(Millies: Integer): Integer;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure LoadModel(Model: TCacheMotor);
    procedure SaveModel(Model: TCacheMotor);
  end;

var
  FormCacheOptions: TFormCacheOptions;

function EditCacheOptions(Model: TCacheMotor): Boolean;

implementation

{$R *.dfm}

function EditCacheOptions(Model: TCacheMotor): Boolean;
begin
  result := False;
  if not Assigned(FormCacheOptions) then
    FormCacheOptions := TFormCacheOptions.Create(Application);

  FormCacheOptions.LoadModel(Model);
  if FormCacheOptions.ShowModal = mrOK then
  begin
    FormCacheOptions.SaveModel(Model);
    result := True;
  end;
end;

{ TFormCacheOptions }

procedure TFormCacheOptions.LoadModel(Model: TCacheMotor);
begin
  cbActive.Checked := Model.Active;
  TrackBar.Position := MilliesToTick(Model.IdleDelay);
  OldValue.Caption := IntToStr(Model.IdleDelay);
end;

procedure TFormCacheOptions.SaveModel(Model: TCacheMotor);
begin
  Model.IdleDelay := TickToMillies(TrackBar.Position);
  Model.Active := cbActive.Checked;
end;

function TFormCacheOptions.MilliesToTick(Millies: Integer): Integer;
var
  ve: Extended;
  vi: Integer;
begin
  ve := Millies;
  ve := ve + 40;
  ve := ve / 5;
  ve := Log10(ve);
  vi := Round(ve * 10);
  if vi < TrackBar.Min then
    vi := TrackBar.Min;
  if vi > TrackBar.Max then
    vi := TrackBar.Max;
  result := vi;
end;

function TFormCacheOptions.TickToMillies(Tick: Integer): Integer;
var
  e: Extended;
  ve: Extended;
  vi: Integer;
begin
  e := TrackBar.Position / 10; //e = 1..3
  ve := Power(10, e); //ve = 10...1000
  ve := 5 * ve; //ve = 50...5000
  ve := ve - 40; //ve = 10...4960;
  vi := Round(ve);
  result := vi;
end;

procedure TFormCacheOptions.TrackBarChange(Sender: TObject);
var
  vi: Integer;
begin
  vi := TickToMillies(TrackBar.Position);
  NewValue.Caption := IntToStr(vi);
end;

end.
