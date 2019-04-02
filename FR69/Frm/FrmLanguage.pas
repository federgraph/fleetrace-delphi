unit FrmLanguage;

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
  RiggVar.BO.Localizer;


type
  TFormLanguage = class(TForm)
    LanguageCombo: TComboBox;
    ComboLabel: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    ExportBtn: TButton;
    ImportBtn: TButton;
    procedure ImportBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    FModel: TBaseLanguageManager;
    procedure LoadModel(Model: TBaseLanguageManager);
    procedure SaveModel(Model: TBaseLanguageManager);
  end;

var
  FormLanguage: TFormLanguage;

function EditLanguage(Model: TBaseLanguageManager): Boolean;

implementation

{$R *.dfm}

function EditLanguage(Model: TBaseLanguageManager): Boolean;
begin
  result := False;
  if not Assigned(FormLanguage) then
    FormLanguage := TFormLanguage.Create(Application);

  FormLanguage.LoadModel(Model);
  if FormLanguage.ShowModal = mrOK then
  begin
    FormLanguage.SaveModel(Model);
    result := True;
  end;
end;

{ TFormLanguage }

procedure TFormLanguage.ExportBtnClick(Sender: TObject);
begin
  FModel.ExportData;
end;

procedure TFormLanguage.FormCreate(Sender: TObject);
begin
  LanguageCombo.Text := '';
end;

procedure TFormLanguage.ImportBtnClick(Sender: TObject);
begin
  FModel.ImportData;
end;

procedure TFormLanguage.LoadModel(Model: TBaseLanguageManager);
begin
  FModel := Model;
  self.LanguageCombo.Items.Assign(Model.Languages);
  self.LanguageCombo.ItemIndex := self.LanguageCombo.Items.IndexOf(Model.Language);
end;

procedure TFormLanguage.SaveModel(Model: TBaseLanguageManager);
begin
  if self.LanguageCombo.ItemIndex > -1 then
    Model.Language := LanguageCombo.Text;
end;

end.
