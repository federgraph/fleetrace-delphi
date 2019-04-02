unit FrmOptions;

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
  RiggVar.BO.Params;

type
  TFormOptions = class(TForm)
    edStartlistCount: TEdit;
    udStartlistCount: TUpDown;
    lblStarlistCount: TLabel;
    edITCount: TEdit;
    udITCount: TUpDown;
    lblTCount: TLabel;
    edRaceCount: TEdit;
    udRaceCount: TUpDown;
    lblRaceCount: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    BOParams: TBOParams;
    procedure UpdateUI;
  public
    { Public-Deklarationen }
    OnOK: TNotifyEvent;
    procedure LoadModel(Model: TBOParams);
    procedure SaveModel(Model: TBOParams);
  end;

function EditEventParams(Model: TBOParams): Boolean;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

var
  FormOptions: TFormOptions;

function EditEventParams(Model: TBOParams): Boolean;
begin
  result := False;
  if not Assigned(FormOptions) then
    FormOptions := TFormOptions.Create(Application);

  FormOptions.LoadModel(Model);
  if FormOptions.ShowModal = mrOK then
  begin
    FormOptions.SaveModel(Model);
    result := True;
  end;
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  BOParams := TBOParams.Create;
end;

procedure TFormOptions.FormDestroy(Sender: TObject);
begin
  BOParams.Free;
end;

procedure TFormOptions.FormShow(Sender: TObject);
begin
  Main.LanguageManager.Localize(self);
  UpdateUI;
end;

procedure TFormOptions.UpdateUI;
begin
  if Assigned(BOParams) then
  begin
    udRaceCount.Min := BOParams.MinRaceCount;
    udRaceCount.Max := BOParams.MaxRaceCount;
    udRaceCount.Position := BOParams.RaceCount;
    //
    udStartlistCount.Min := BOParams.MinStartlistCount;
    udStartlistCount.Max := BOParams.MaxStartlistCount;
    udStartlistCount.Position := BOParams.StartlistCount;
    //
    udITCount.Min := BOParams.MinITCount;
    udITCount.Max := BOParams.MaxITCount;
    udITCount.Position := BOParams.ITCount;
  end;
end;

procedure TFormOptions.OKBtnClick(Sender: TObject);
begin
  BOParams.RaceCount := udRaceCount.Position;
  BOParams.StartlistCount := udStartlistCount.Position;
  BOParams.ITCount := udITCount.Position;

  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

procedure TFormOptions.LoadModel(Model: TBOParams);
begin
  BOParams.Assign(Model);
  //UpdateUI;

  //UpdateUI need to be called in C#
  //because FormShow is called shown prior to LoadModel()
  //when Dialog is embedded.
end;

procedure TFormOptions.SaveModel(Model: TBOParams);
begin
  Model.Assign(BOParams);
end;

end.

