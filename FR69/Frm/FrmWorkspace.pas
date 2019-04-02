unit FrmWorkspace;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TFormWorkspace = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    edWorkspaceID: TEdit;
    udWorkspaceID: TUpDown;
    MemoLines: TMemo;
    LabelWorkspaceID: TLabel;
    WorkspaceTypePromt: TLabel;
    LabelCurrentWorkspace: TLabel;
    gbWorkspaceType: TGroupBox;
    rbUserHome: TRadioButton;
    rbAppLocal: TRadioButton;
    rbLocalDB: TRadioButton;
    rbRemoteDB: TRadioButton;
    WorkspaceIDPrompt: TLabel;
    LabelCurrentWorkspaceID: TLabel;
    rbWebService: TRadioButton;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbClick(Sender: TObject);
  private
    { Private-Deklarationen }
    Memo: TStrings;
    OldWorkspaceType: Integer;
    OldWorkspaceID: Integer;
    WorkspaceType: Integer;
    WorkspaceID: Integer;
    procedure UpdateUI;
    procedure InitUI;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.DAL.WorkspaceIntf,
  FrmWorkspaceFiles;

procedure TFormWorkspace.FormCreate(Sender: TObject);
begin
  Memo := MemoLines.Lines;
  //OKBtn.Enabled := false;
  rbLocalDB.Enabled := false; //do not depend on BDE...

  WorkspaceID := Main.StoreAdapter.GetWorkspaceID;
  WorkspaceType := Main.StoreAdapter.GetWorkspaceType;

  OldWorkspaceID := WorkspaceID;
  OldWorkspaceType := WorkspaceType;

  InitUI;
end;

procedure TFormWorkspace.OKBtnClick(Sender: TObject);
begin
  Main.GuiManager.UpdateWorkspace(WorkspaceType, udWorkspaceID.Position);
end;

procedure TFormWorkspace.CancelBtnClick(Sender: TObject);
begin
  //
end;

procedure TFormWorkspace.rbClick(Sender: TObject);
begin
  if Sender = rbUserHome then
    WorkspaceType := WorkspaceType_SharedFS
  else if Sender = rbAppLocal then
    WorkspaceType := WorkspaceType_PrivateFS
  else if Sender = rbLocalDB then
    WorkspaceType := WorkspaceType_LocalDB
  else if Sender = rbRemoteDB then
    WorkspaceType := WorkspaceType_RemoteDB
  else if Sender = rbWebService then
    WorkspaceType := WorkspaceType_WebService;
  UpdateUI;
end;

procedure TFormWorkspace.InitUI;
begin
  udWorkspaceID.Position := OldWorkspaceID;
  case WorkspaceType of
    WorkspaceType_SharedFS:
    begin
      rbUserHome.Checked := true;
      LabelCurrentWorkspace.Caption := 'User Home';
    end;
    WorkspaceType_PrivateFS:
    begin
      rbAppLocal.Checked := true;
      LabelCurrentWorkspace.Caption := 'App Local';
    end;
    WorkspaceType_LocalDB:
    begin
      rbLocalDB.Checked := true;
      LabelCurrentWorkspace.Caption := 'Local DB';
    end;
    WorkspaceType_RemoteDB:
    begin
      rbRemoteDB.Checked := true;
      LabelCurrentWorkspace.Caption := 'Remote DB';
    end;
    WorkspaceType_WebService:
    begin
      rbWebService.Checked := true;
      LabelCurrentWorkspace.Caption := 'Web Service';
    end;
  end;
  UpdateUI;
end;

procedure TFormWorkspace.UpdateUI;
begin
  case WorkspaceType of
    WorkspaceType_SharedFS:
    begin
      rbUserHome.Checked := true;
      edWorkspaceID.Enabled := false;
    end;
    WorkspaceType_PrivateFS:
    begin
      rbAppLocal.Checked := true;
      edWorkspaceID.Enabled := false;
    end;
    WorkspaceType_LocalDB:
    begin
      rbLocalDB.Checked := true;
      edWorkspaceID.Enabled := true;
    end;
    WorkspaceType_RemoteDB:
    begin
      edWorkspaceID.Enabled := true;
      rbRemoteDB.Checked := true;
    end;
    WorkspaceType_WebService:
    begin
      edWorkspaceID.Enabled := true;
      rbWebService.Checked := true;
    end;
    else
    begin
      edWorkspaceID.Enabled := false;
    end;
  end;

  Memo.Clear;

  case WorkspaceType of
    WorkspaceType_SharedFS: begin
      Memo.Add('-- shared FS');
      Memo.Add('<User Home>\RiggVar Workspace\...');
      Memo.Add('WorkspaceType = ' + IntToStr(WorkspaceType));
      Memo.Add('WorkspaceID = 1');
    end;

    WorkspaceType_PrivateFS: begin
      Memo.Add('-- private FS');
      Memo.Add('<App Dir>\...');
      Memo.Add('WorkspaceType = ' + IntToStr(WorkspaceType));
      Memo.Add('WorkspaceID = 1');
    end;

    WorkspaceType_LocalDB: begin
      Memo.Add('-- local DB');
      Memo.Add('Pdx tables in <Exe Dir>\WorkspaceDB');
      Memo.Add('WorkspaceType = ' + IntToStr(WorkspaceType));
      Memo.Add('select WorkspaceID');
    end;

    WorkspaceType_RemoteDB: begin
      Memo.Add('-- remote DB');
      Memo.Add('SQL Server');
      Memo.Add('WorkspaceType = ' + IntToStr(WorkspaceType));
      Memo.Add('select WorkspaceID');
    end;

    WorkspaceType_WebService: begin
      Memo.Add('-- DB via Web Service');
      Memo.Add('SQL Server');
      Memo.Add('WorkspaceType = ' + IntToStr(WorkspaceType));
      Memo.Add('select WorkspaceID');
    end;

  end;
end;

end.
