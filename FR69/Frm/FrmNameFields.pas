unit FrmNameFields;

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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.Col.NameField,
  RiggVar.Col.Stammdaten;

type
  TFormNameFields = class(TForm)
    ToolPanel: TPanel;
    LabelFieldMap: TLabel;
    edFieldMap: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    GridPanel: TPanel;
    ToolBar: TToolBar;
    SwapBtn: TSpeedButton;
    AddBtn: TSpeedButton;
    RemoveBtn: TSpeedButton;
    NameSchemaCombo: TComboBox;
    ApplyBtn: TButton;
    NameSchemaLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure SwapBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    en: TStammdatenNode;
  public
    { Public-Deklarationen }
    NameFieldGrid: TNameFieldGrid;
    OnOK: TNotifyEvent;
    procedure LoadModel(Model: TStammdatenNode);
    procedure SaveModel(Model: TStammdatenNode);
  end;

function EditNameFields(Model: TStammdatenNode): Boolean;

implementation

{$R *.dfm}

var
  FormNameFields: TFormNameFields;

function EditNameFields(Model: TStammdatenNode): Boolean;
begin
  result := False;
  if not Assigned(FormNameFields) then
    FormNameFields := TFormNameFields.Create(Application);

  FormNameFields.LoadModel(Model);
  if FormNameFields.ShowModal = mrOK then
  begin
    FormNameFields.SaveModel(Model);
    result := True;
  end;

  FormNameFields.NameFieldGrid.Free;
  FormNameFields.NameFieldGrid := nil;
end;

{ TFormNameFields }

procedure TFormNameFields.FormCreate(Sender: TObject);
begin
  GridPanel.Align := alClient;
  AddBtn.Hint := 'add one row';
  RemoveBtn.Hint := 'remove last row if row-count > 6';
  SwapBtn.Hint := 'swap data between colums swap=1 and swap=2';

  NameSchemaCombo.Items.Add('Default');
  NameSchemaCombo.Items.Add('Long Names');
  NameSchemaCombo.Items.Add('NX');
  NameSchemaCombo.ItemIndex := 0;
end;

procedure TFormNameFields.FormDestroy(Sender: TObject);
begin
  NameFieldGrid.Free;
  NameFieldGrid := nil;
end;

procedure TFormNameFields.LoadModel(Model: TStammdatenNode);
begin
  en := Model;
  if not Assigned(NameFieldGrid) then
    NameFieldGrid := TNameFieldGrid.Create(GridPanel);
  NameFieldGrid.Node.Init(Model);
  NameFieldGrid.GB.ColGrid.UpdateAll;

  edFieldMap.Text := Model.StammdatenRowCollection.FieldMap;
  if Model.SchemaCode < NameSchemaCombo.Items.Count then
    NameSchemaCombo.ItemIndex := Model.SchemaCode;
end;

procedure TFormNameFields.SaveModel(Model: TStammdatenNode);
var
  sl: TStammdatenRowCollection;
  cl: TNameFieldRowCollection;
begin
  sl := Model.StammdatenRowCollection;
  cl := NameFieldGrid.Node.NameFieldRowCollection;
  if cl.Count <> sl.FieldCount then
    sl.FieldCount := cl.Count;
  sl.FieldCaptions := cl.GetFieldCaptions;
  sl.SchemaCode := NameSchemaCombo.ItemIndex;
  Model.StammdatenRowCollection.FieldMap := edFieldMap.Text;
end;

procedure TFormNameFields.AddBtnClick(Sender: TObject);
var
  cl: TNameFieldRowCollection;
begin
  cl := NameFieldGrid.Node.NameFieldRowCollection;
  if cl.Count < 32 then
  begin
    cl.Add;
    NameFieldGrid.GB.ColGrid.UpdateAll;
  end;
end;

procedure TFormNameFields.RemoveBtnClick(Sender: TObject);
var
  cl: TNameFieldRowCollection;
begin
  cl := NameFieldGrid.Node.NameFieldRowCollection;
  if cl.Count > 6 then
  begin
    cl.Delete(cl.Count-1);
    NameFieldGrid.GB.ColGrid.UpdateAll;
  end;
end;

procedure TFormNameFields.SwapBtnClick(Sender: TObject);
var
  cl: TNameFieldRowCollection;
  cr: TNameFieldRowCollectionItem;
  i: Integer;
  f1: Integer;
  f2: Integer;
  ambiguous: Boolean;
  s1, s2: string; //debug only
begin
  cl := NameFieldGrid.Node.NameFieldRowCollection;
  f1 := 0;
  f2 := 0;
  s1 := '';
  s2 := '';
  ambiguous := false;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    if cr.Swap = 1 then
    begin
      if f1 > 0 then
        ambiguous := true;
      f1 := cr.BaseID;
      s1 := cr.FieldName;
    end;
    if cr.Swap = 2 then
    begin
      if f2 > 0 then
        ambiguous := true;
      f2 := cr.BaseID;
      s2 := cr.FieldName;
    end;
  end;
  if (not ambiguous) and (f1 > 0) and (f2 > 0) then
  begin
    en.StammdatenRowCollection.Swap(f1, f2);
  end;
end;

procedure TFormNameFields.ApplyBtnClick(Sender: TObject);
var
  sl: TStammdatenRowCollection;
  cl: TNameFieldRowCollection;
  cr: TNameFieldRowCollectionItem;
  i: Integer;
begin
  sl := en.StammdatenRowCollection;
  cl := NameFieldGrid.Node.NameFieldRowCollection;
  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    cr.Caption := sl.GetStandardFieldCaption(i+1, self.NameSchemaCombo.ItemIndex);
  end;
  NameFieldGrid.GB.ColGrid.UpdateAll;
end;

procedure TFormNameFields.OKBtnClick(Sender: TObject);
begin
  if Assigned(OnOK) then
  begin
    OnOK(self);
  end;
end;

end.
