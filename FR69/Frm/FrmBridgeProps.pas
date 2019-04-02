unit FrmBridgeProps;

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
  RiggVar.BR.BridgeController;

type
  TFormBridgeProps = class(TForm)
    LabelBridgeUrl: TLabel;
    edBridgeUrl: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    TestBtn: TButton;
    LabelTaktOut: TLabel;
    edTaktOut: TEdit;
    LabelTaktIn: TLabel;
    edTaktIn: TEdit;
    procedure TestBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    BridgeController: TBridgeController;
    procedure Test;
  public
    { Public-Deklarationen }
    procedure LoadModel(Model: TBridgeController);
    procedure SaveModel(Model: TBridgeController);
  end;

function EditBridgeProps(Model: TBridgeController): Boolean;

implementation

uses
  RiggVar.App.Main;

{$R *.dfm}

var
  FormBridgeProps: TFormBridgeProps;

function EditBridgeProps(Model: TBridgeController): Boolean;
begin
  result := False;
  if not Assigned(FormBridgeProps) then
    FormBridgeProps := TFormBridgeProps.Create(Application);

  FormBridgeProps.LoadModel(Model);
  if FormBridgeProps.ShowModal = mrOK then
  begin
    FormBridgeProps.SaveModel(Model);
    result := True;
  end;
end;

procedure TFormBridgeProps.LoadModel(Model: TBridgeController);
begin
  BridgeController := Model;
  edTaktOut.Text := IntToStr(Model.TaktOut);
  edTaktIn.Text := IntToStr(Model.TaktIn);
  edBridgeUrl.Text := Model.ServerUrl;
end;

procedure TFormBridgeProps.SaveModel(Model: TBridgeController);
begin
  Model.TaktOut := StrToIntDef(edTaktOut.Text, Model.TaktOut);
  Model.TaktIn := StrToIntDef(edTaktIn.Text, Model.TaktIn);
  Model.ServerUrl := edBridgeUrl.Text;
  BridgeController := nil;
end;

procedure TFormBridgeProps.OKBtnClick(Sender: TObject);
begin
  Test;
end;

procedure TFormBridgeProps.TestBtnClick(Sender: TObject);
begin
  Test;
end;

procedure TFormBridgeProps.Test;
var
  temp: Integer;
begin
  temp := StrToIntDef(edTaktOut.Text, 5);
  if temp < -1 then
    temp := -1;
  if temp > 100 then
    temp := 100;
  edTaktOut.Text := IntToStr(temp);

  temp := StrToIntDef(edTaktIn.Text, 5);
  if temp < -1 then
    temp := -1;
  if temp > 100 then
    temp := 100;
  edTaktIn.Text := IntToStr(temp);

  if BridgeController.Bridge.GetLastMsgID = -1 then
    edBridgeUrl.Text := BridgeController.Bridge.GetServerUrl;
end;

end.
