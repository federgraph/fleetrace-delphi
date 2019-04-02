unit FrmPages;

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
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  RiggVar.App.Config;

type
  TFormPages = class(TForm)
    CheckListBox: TCheckListBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    SL: TStringList;
    procedure InitList(ML: TStrings);
    procedure LoadModel(Model: TWantPages);
    procedure SaveModel(Model: TWantPages);
  end;

var
  FormPages: TFormPages;

function EditPages(Model: TWantPages): Boolean;

implementation

{$R *.dfm}

function EditPages(Model: TWantPages): Boolean;
begin
  result := False;
  if not Assigned(FormPages) then
    FormPages := TFormPages.Create(Application);

  FormPages.LoadModel(Model);
  if FormPages.ShowModal = mrOK then
  begin
    FormPages.SaveModel(Model);
    result := True;
  end;
end;

procedure TFormPages.FormCreate(Sender: TObject);
begin
  SL := TStringList.Create;
  InitList(SL);
  InitList(CheckListBox.Items);
  CheckListBox.Anchors := [akLeft, akTop, akBottom];
end;

procedure TFormPages.FormDestroy(Sender: TObject);
begin
  SL.Free;
end;

procedure TFormPages.InitList(ML: TStrings);
begin
  ML.Add('Browser');
  ML.Add('Cache');
  ML.Add('Entries');
  ML.Add('Race');
  ML.Add('Timing');
  ML.Add('Racing');
  ML.Add('Keying');
  ML.Add('Event');
  ML.Add('Workspace');
  ML.Add('Report');
  ML.Add('Menu');
  ML.Add('Category');
  ML.Add('Mobil');
  ML.Add('Web');
  ML.Add('Json');
  ML.Add('Course');
  ML.Add('Mark');
  ML.Add('Listing');
  ML.Add('Roundings');
end;

procedure TFormPages.LoadModel(Model: TWantPages);
begin
  CheckListBox.Checked[0] := Model.WantBrowser;
  CheckListBox.Checked[1] := Model.WantCache;
  CheckListBox.Checked[2] := Model.WantEntries;
  CheckListBox.Checked[3] := Model.WantRace;
  CheckListBox.Checked[4] := Model.WantTiming;
  CheckListBox.Checked[5] := Model.WantRacing;
  CheckListBox.Checked[6] := Model.WantKeying;
  CheckListBox.Checked[7] := Model.WantEvent;
  CheckListBox.Checked[8] := Model.WantWorkspace;
  CheckListBox.Checked[9] := Model.WantReport;
  CheckListBox.Checked[10] := Model.WantMenu;
  CheckListBox.Checked[11] := Model.WantCategory;
  CheckListBox.Checked[12] := Model.WantMobil;
  CheckListBox.Checked[13] := Model.WantWeb;
  CheckListBox.Checked[14] := Model.WantJson;
  CheckListBox.Checked[15] := Model.WantCourse;
  CheckListBox.Checked[16] := Model.WantMark;
  CheckListBox.Checked[17] := Model.WantListing;
  CheckListBox.Checked[18] := Model.WantRoundings;
end;

procedure TFormPages.SaveModel(Model: TWantPages);
begin
  Model.WantBrowser := CheckListBox.Checked[0];
  Model.WantCache := CheckListBox.Checked[1];
  Model.WantEntries := CheckListBox.Checked[2];
  Model.WantRace := CheckListBox.Checked[3];
  Model.WantTiming := CheckListBox.Checked[4];
  Model.WantRacing := CheckListBox.Checked[5];
  Model.WantKeying := CheckListBox.Checked[6];
  Model.WantEvent := CheckListBox.Checked[7];
  Model.WantWorkspace := CheckListBox.Checked[8];
  Model.WantReport := CheckListBox.Checked[9];
  Model.WantMenu := CheckListBox.Checked[10];
  Model.WantCategory := CheckListBox.Checked[11];
  Model.WantMobil := CheckListBox.Checked[12];
  Model.WantWeb := CheckListBox.Checked[13];
  Model.WantJson := CheckListBox.Checked[14];
  Model.WantCourse := CheckListBox.Checked[15];
  Model.WantMark := CheckListBox.Checked[16];
  Model.WantListing := CheckListBox.Checked[17];
  Model.WantRoundings := CheckListBox.Checked[18];
end;

end.
