unit FrmInspector;

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
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  RiggVar.Col.NameValue;

type
  TFormInspector = class(TForm)
    ToolBar: TToolBar;
    LoadBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    OKBtn: TSpeedButton;
    CloseBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    NameValueGrid: TNameValueGrid;
  public
    { Public-Deklarationen }
    OnLoad: TNotifyEvent;
    OnSave: TNotifyEvent;
    OnOK: TNotifyEvent;
    procedure LoadModel;
    procedure SaveModel;
  end;

function InspectProperties(OnLoad, OnSave: TNotifyEvent): Boolean;

implementation

{$R *.dfm}

function InspectProperties(OnLoad, OnSave: TNotifyEvent): Boolean;
var
  Inspector: TFormInspector;
begin
  result := False;
  Inspector := TFormInspector.Create(nil);
  try
    Inspector.OnLoad := OnLoad;
    Inspector.OnSave := OnSave;
    Inspector.LoadModel;
    if Inspector.ShowModal = mrOK then
    begin
      Inspector.SaveModel;
      result := True;
    end;
  finally
    Inspector.Free;
  end;
end;

{ TFormInspector }

procedure TFormInspector.FormCreate(Sender: TObject);
begin
  NameValueGrid := TNameValueGrid.Create(self);
end;

procedure TFormInspector.FormDestroy(Sender: TObject);
begin
  NameValueGrid.Free;
end;

procedure TFormInspector.LoadModel;
begin
  NameValueGrid.Node.NameValueRowCollection.Clear;
  if Assigned(OnLoad) then
    OnLoad(NameValueGrid.Node.NameValueRowCollection);
  NameValueGrid.GB.ColGrid.UpdateAll;
end;

procedure TFormInspector.SaveModel;
begin
  if Assigned(OnSave) then
    OnSave(NameValueGrid.Node.NameValueRowCollection);
end;

procedure TFormInspector.LoadBtnClick(Sender: TObject);
begin
  LoadModel;
end;

procedure TFormInspector.SaveBtnClick(Sender: TObject);
begin
  SaveModel;
end;

procedure TFormInspector.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
  if Assigned(OnOK) then
  begin
    SaveModel;
    OnOK(self);
  end;
end;

procedure TFormInspector.CloseBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
