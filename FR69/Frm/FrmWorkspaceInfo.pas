unit FrmWorkspaceInfo;

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
  RiggVar.DAL.WorkspaceManager,
  RiggVar.DAL.WorkspaceInfo,
  RiggVar.DAL.WorkspaceRepo;

type
  TFormWorkspaceInfo = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    LabelRepository: TLabel;
    LabelWorkspaceType: TLabel;
    LabelWorkspaceID: TLabel;
    LabelWorkspaceUrl: TLabel;
    LabelWorkspaceRoot: TLabel;
    RepoCombo: TComboBox;
    TypeCombo: TComboBox;
    UrlCombo: TComboBox;
    RootCombo: TComboBox;
    gbWorkspaceInfo: TGroupBox;
    edID: TEdit;
    cbAutoSaveIni: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RepoComboChange(Sender: TObject);
  private
    { Private-Deklarationen }
    FWorkspaceType: Integer;
    FWorkspaceInfo: TWorkspaceInfo;
    Repo: TRepoItems;
    wi: TWorkspaceInfo;
    procedure InitFromRepo(ScenarioID: Integer);
    procedure SetWorkspaceType(const Value: Integer);
    procedure SetWorkspaceInfo(const Value: TWorkspaceInfo);
    property WorkspaceType: Integer read FWorkspaceType write SetWorkspaceType;
  public
    { Public-Deklarationen }
    property WorkspaceInfo: TWorkspaceInfo read FWorkspaceInfo write SetWorkspaceInfo;
  end;

implementation

{$R *.dfm}

procedure TFormWorkspaceInfo.FormCreate(Sender: TObject);
begin
  wi := TWorkspaceInfo.Create;
  Repo := TRepoItems.Create;
  Repo.UpdateTypeCombo(TypeCombo.Items);
  Repo.UpdateUrlCombo(UrlCombo.Items);
  Repo.UpdateRootCombo(RootCombo.Items);
  Repo.UpdateRepoCombo(RepoCombo.Items);
end;

procedure TFormWorkspaceInfo.FormDestroy(Sender: TObject);
begin
  wi.Free;
  Repo.Free;
end;

procedure TFormWorkspaceInfo.SetWorkspaceInfo(const Value: TWorkspaceInfo);
begin
  //called with current value after creation, before showing the modal Form
  FWorkspaceInfo := Value; //reference to live WorkspaceInfo
  Repo.Items[0].Assign(Value);
  wi.Assign(Value); //local copy of 'current' item at Repo.Items[0]
  RepoCombo.ItemIndex := 0;
  InitFromRepo(0);
end;

procedure TFormWorkspaceInfo.SetWorkspaceType(const Value: Integer);
begin
  FWorkspaceType := Value;
  if Value < TypeCombo.Items.Count then
    TypeCombo.ItemIndex := Value;
  TypeCombo.Tag := Value;
end;

procedure TFormWorkspaceInfo.InitFromRepo(ScenarioID: Integer);
var
  ri: TWorkspaceInfo;
begin
  ri := Repo.Items[ScenarioID];
  WorkspaceType := ri.WorkspaceType;
  edID.Text := IntToStr(ri.WorkspaceID);
  UrlCombo.Text := ri.WorkspaceUrl;
  RootCombo.Text := ri.WorkspaceRoot;
  cbAutoSaveIni.Checked := ri.AutoSaveIni;
end;

procedure TFormWorkspaceInfo.RepoComboChange(Sender: TObject);
begin
  InitFromRepo(RepoCombo.ItemIndex);
end;

procedure TFormWorkspaceInfo.OKBtnClick(Sender: TObject);
begin
  FWorkspaceInfo.WorkspaceType := TypeCombo.ItemIndex;
  FWorkspaceInfo.WorkspaceID := StrToIntDef(edID.Text, wi.WorkspaceID);
  FWorkspaceInfo.WorkspaceUrl := UrlCombo.Text;
  FWorkspaceInfo.WorkspaceRoot := RootCombo.Text;
  FWorkspaceInfo.AutoSaveIni := cbAutoSaveIni.Checked;
end;

procedure TFormWorkspaceInfo.CancelBtnClick(Sender: TObject);
begin
  //
end;

end.
