unit FrmDocManager;

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
  RiggVar.App.Config,
  RiggVar.DAL.Manager;

type
  TFormDocManager = class(TForm)
    MdbBtn: TRadioButton;
    TxtBtn: TRadioButton;
    WebBtn: TRadioButton;
    LNKBtn: TRadioButton;
    XMLBtn: TRadioButton;
    edTxt: TEdit;
    edMdb: TEdit;
    edWeb: TEdit;
    edREST: TEdit;
    edLNK: TEdit;
    edXML: TEdit;
    LabelMdb: TLabel;
    LabelTxt: TLabel;
    LabelWeb: TLabel;
    LabelREST: TLabel;
    LabelLNK: TLabel;
    LabelXML: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    RESTBtn: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    OnOK: TNotifyEvent;
    procedure LoadModel(Model: TDocManager);
    procedure SaveModel(Model: TDocManager);
  end;

function EditDBEvent(Model: TDocManager): Boolean;

var
  FormDocManager: TFormDocManager;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

function EditDBEvent(Model: TDocManager): Boolean;
begin
  result := False;
  if not Assigned(FormDocManager) then
    FormDocManager := TFormDocManager.Create(Application);

  FormDocManager.LoadModel(Model);
  if FormDocManager.ShowModal = mrOK then
  begin
    FormDocManager.SaveModel(Model);
    result := True;
  end;
end;

{ TFormDocManager }

procedure TFormDocManager.FormShow(Sender: TObject);
begin
  Main.LanguageManager.Localize(self);
end;

procedure TFormDocManager.LoadModel(Model: TDocManager);
begin
  edWeb.Text := Main.IniImage.WebApplicationUrl;
  if Main.IniImage.DBInterface = 'WEB' then
    WebBtn.Checked := true
  else if Main.IniImage.DBInterface = 'MDB' then
    MdbBtn.Checked := true
  else if Main.IniImage.DBInterface = 'REST' then
    RESTBtn.Checked := true
  else if Main.IniImage.DBInterface = 'LNK' then
    LNKBtn.Checked := true
  else if Main.IniImage.DBInterface = 'XML' then
    XMLBtn.Checked := true
  else
    TxtBtn.Checked := true;
end;

procedure TFormDocManager.SaveModel(Model: TDocManager);
begin
  if WebBtn.Checked then
    Main.IniImage.DBInterface := 'WEB'
  else if MdbBtn.Checked then
    Main.IniImage.DBInterface := 'MDB'
  else if RESTBtn.Checked then
    Main.IniImage.DBInterface := 'REST'
  else if LNKBtn.Checked then
    Main.IniImage.DBInterface := 'LNK'
  else if XMLBtn.Checked then
    Main.IniImage.DBInterface := 'XML'
  else
    Main.IniImage.DBInterface := 'TXT'
end;

procedure TFormDocManager.OKBtnClick(Sender: TObject);
begin
  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

end.
