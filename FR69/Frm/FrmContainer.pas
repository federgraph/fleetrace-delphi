unit FrmContainer;

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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  RiggVar.BO.Def,
  RiggVar.BO.Params,
  FrmInspector,
  FrmFleetProps,
  FrmRegattaProps,
  FrmUniquaProps,
  FrmNameFields,
  FrmOptions,
  FrmScoringModule,
  FrmDocManager;

type
  TFormContainer = class(TForm)
    ListBox: TListBox;
    Panel: TPanel;
    CaptionPanel: TPanel;
    DetailPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
  private
    FormInspector: TFormInspector;
    FormFleet: TFormFleetProps;
    FormRegatta: TFormRegattaProps;
    FormUniqua: TFormUniquaProps;
    FormNames: TFormNameFields;
    FormParams: TFormOptions;
    FormScoring: TFormScoringModule;
    FormDB: TFormDocManager;

    oldShowPLZColumn: Boolean;
    oldShowPosRColumn: Boolean;
    oldShowCupColumn: Boolean;
    oldNameFieldCount: Integer;
    oldNameFieldOrder: string;

    ChangeSet: set of TEditPage;
    procedure OnPanelChange;
    procedure ShowInspector;
    procedure ConfigureFormAsPanel(f: TForm);
    function FindButtonOnForm(f: TForm; n: string): TButton;
    procedure RememberOldValues(ep: TEditPage);
    procedure UpdateRefreshFlag(ep: TEditPage);
  public
    BOParams: TBOParams;
    RefreshFlag: Boolean;

    OnLoad: TNotifyEvent;
    OnSave: TNotifyEvent;
    OnOK: TNotifyEvent;
    ActivePage: TEditPage;

    procedure InitListBox;
    function GetSelectedPanel(Index: Integer): TEditPage;

    procedure OnFormShow;
    procedure OnFormHide;

    procedure OnPanelShow(ep: TEditPage);
    procedure OnPanelOK(Sender: TObject);
    procedure OnPanelHide(ep: TEditPage);
    function GetPanelCaption(ep: TEditPage): string;

    function HasChanged(ep: TEditPage): Boolean;
    function IsEnabled(ep: TEditPage): Boolean;
  end;

var
  FormContainer: TFormContainer;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormContainer.FormCreate(Sender: TObject);
begin
  Width := 780;
  Height := 500;
  ListBox.Align := alLeft;
  Panel.Align := alClient;
  InitListBox;
  BOParams := TBOParams.Create;
  ListBox.ItemIndex := 0;
  OnPanelChange;
end;

procedure TFormContainer.FormDestroy(Sender: TObject);
begin
  OnFormHide;
  BOParams.Free;
end;

procedure TFormContainer.ListBoxClick(Sender: TObject);
begin
  if GetSelectedPanel(ListBox.ItemIndex) <> ActivePage then
    OnPanelChange;
end;

procedure TFormContainer.ListBoxDblClick(Sender: TObject);
begin
  if GetSelectedPanel(ListBox.ItemIndex) <> ActivePage then
    OnPanelChange;
end;

procedure TFormContainer.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Escape then
    Close;
  if Key = VK_RETURN then
    OnPanelChange;
end;

procedure TFormContainer.OnFormShow;
begin

end;

procedure TFormContainer.OnFormHide;
begin
  FreeAndNil(FormInspector);
  FreeAndNil(FormFleet);
  FreeAndNil(FormRegatta);
  FreeAndNil(FormUniqua);
  FreeAndNil(FormNames);
  FreeAndNil(FormParams);
  FreeAndNil(FormScoring);
  FreeAndNil(FormDB);
end;

function TFormContainer.GetSelectedPanel(Index: Integer): TEditPage;
begin
  result := epNone;
  if (Index >= 0) and (Index <= ListBox.Items.Count) then
  begin
    result := TEditPage(Integer(ListBox.Items.Objects[Index]));
  end;
end;

function TFormContainer.HasChanged(ep: TEditPage): Boolean;
begin
  result := ep in ChangeSet;
end;

procedure TFormContainer.InitListBox;
var
  cl: TStrings;
  ep: TEditPage;
begin
  cl := ListBox.Items;
  for ep := Low(TEditPage) to High(TEditPage) do
  begin
    if not (ep in Main.Params.DisabledEditPages) then
      cl.AddObject(GetPanelCaption(ep), TObject(Integer(ep)));
  end;
end;

procedure TFormContainer.OnPanelChange;
var
  ep: TEditPage;
begin
  Panel.Caption := '';
  if ListBox.ItemIndex > -1 then
  begin
    ep := GetSelectedPanel(ListBox.ItemIndex);
    if ep = epNone then
    begin
      OnPanelHide(ActivePage);
      ActivePage := ep;
      CaptionPanel.Caption := 'please select page';
    end
    else if ep = ActivePage then
    begin
      CaptionPanel.Caption := GetPanelCaption(ep)
    end
    else
    begin
      OnPanelHide(ActivePage);
      ActivePage := ep;
      OnPanelShow(ActivePage);
      CaptionPanel.Caption := GetPanelCaption(ep)
    end;
  end
  else
  begin
    CaptionPanel.Caption := 'nothing selected';
  end;
end;

function TFormContainer.GetPanelCaption(ep: TEditPage): string;
begin
  case ep of
    epNone: result := 'Info';
    epParams: result := 'Params';
    epRegatta: result := 'Regatta';
    epUniqua: result := 'Uniqua';
    epFleet: result := 'Fleet';
    epEvent: result := 'Event';
    epIni: result := 'Ini';
    epNames: result := 'Names';
    epScoring: result := 'Scoring';
    epDB: result := 'DB';
  else
    result := 'unknown';
  end
end;

function TFormContainer.IsEnabled(ep: TEditPage): Boolean;
begin
  case ep of
    epNone: result := true;
    epParams: result := true;
    epRegatta: result := true;
    epUniqua: result := true;
    epFleet: result := true;
    epEvent: result := true;
    epIni: result := true;
    epNames: result := true;
    epScoring: result := true;
    epDB: result := true;
  else
    result := false;
  end;
end;

procedure TFormContainer.OnPanelShow(ep: TEditPage);
begin
  if not IsEnabled(ep) then
    Panel.Caption := GetPanelCaption(ep)
  else
  begin
    RememberOldValues(ep);

    case ep of

      epNone: begin
      end;

      epParams: begin
        if not Assigned(FormParams) then
        begin
          FormParams := TFormOptions.Create(nil);
          ConfigureFormAsPanel(FormParams);
          FormParams.OnOK := OnPanelOK;
        end;
        FormParams.LoadModel(self.BOParams);
        FormParams.Visible := True;
      end;

      epRegatta: begin
        if not Assigned(FormRegatta) then
        begin
          FormRegatta := TFormRegattaProps.Create(nil);
          ConfigureFormAsPanel(FormRegatta);
          FormRegatta.OnOK := OnPanelOK;
        end;
        FormRegatta.LoadModel(BO.EventProps);
        FormRegatta.Visible := True;
      end;

      epUniqua:
      begin
        if not Assigned(FormUniqua) then
        begin
          FormUniqua := TFormUniquaProps.Create(nil);
          ConfigureFormAsPanel(FormUniqua);
          FormUniqua.OnOK := OnPanelOK;
        end;
        FormUniqua.LoadModel(BO.EventProps);
        FormUniqua.Visible := True;
      end;

      epFleet: begin
        if not Assigned(FormFleet) then
        begin
          FormFleet := TFormFleetProps.Create(nil);
          ConfigureFormAsPanel(FormFleet);
          FormFleet.OnOK := OnPanelOK;
        end;
        FormFleet.LoadModel(BO.EventProps);
        FormFleet.Visible := True;
      end;

      epEvent: begin
        OnLoad := BO.EventProps.InspectorOnLoad;
        OnSave := BO.EventProps.InspectorOnSave;
        ShowInspector;
      end;

      epIni: begin
        OnLoad := Main.IniImage.InspectorOnLoad;
        OnSave := Main.IniImage.InspectorOnSave;
        ShowInspector;
      end;

      epNames: begin
        if not Assigned(FormNames) then
        begin
          FormNames := TFormNameFields.Create(nil);
          ConfigureFormAsPanel(FormNames);
          FormNames.OnOK := OnPanelOK;
        end;
        FormNames.LoadModel(BO.StammdatenNode);
        FormNames.Visible := True;
      end;

      epScoring:
      begin
        if not Assigned(FormScoring) then
        begin
          FormScoring := TFormScoringModule.Create(nil);
          ConfigureFormAsPanel(FormScoring);
          FormScoring.OnOK := OnPanelOK;
        end;
        FormScoring.LoadModel(BO.CalcEV);
        FormScoring.Visible := True;
      end;

      epDB: begin
        if not Assigned(FormDB) then
        begin
          FormDB := TFormDocManager.Create(nil);
          ConfigureFormAsPanel(FormDB);
          FormDB.OnOK := OnPanelOK;
        end;
        FormDB.LoadModel(Main.DocManager);
        FormDB.Visible := True;
      end;

    end;
  end;
end;

procedure TFormContainer.OnPanelOK(Sender: TObject);
begin
  case ActivePage of
    epNone: ;

    epParams:
    begin
      FormParams.SaveModel(BOParams);
      Include(ChangeSet, ActivePage);
    end;

    epRegatta:
    begin
      FormRegatta.SaveModel(BO.EventProps);
      Include(ChangeSet, ActivePage);
    end;

    epUniqua:
    begin
      FormUniqua.SaveModel(BO.EventProps);
      Include(ChangeSet, ActivePage);
    end;

    epFleet:
    begin
      FormFleet.SaveModel(BO.EventProps);
      Include(ChangeSet, ActivePage);
    end;

    epEvent:
    begin
      FormInspector.SaveModel;
      Include(ChangeSet, ActivePage);
    end;

    epIni:
    begin
      FormInspector.SaveModel;
      Include(ChangeSet, ActivePage);
    end;

    epNames:
    begin
      FormNames.SaveModel(BO.StammdatenNode);
      Include(ChangeSet, ActivePage);
    end;

    epScoring:
    begin
      FormScoring.SaveModel(BO.CalcEV);
      Include(ChangeSet, ActivePage);
    end;

    epDB:
    begin
      FormDB.SaveModel(Main.DocManager);
      if Main.DocManager.DBInterface <> Main.IniImage.DBInterface then
        Main.DocManager.InitDBInterface(Main.IniImage.DBInterface);
      Include(ChangeSet, ActivePage);
    end;
  end;

  if Assigned(OnOK) and HasChanged(ActivePage) then
  begin
    UpdateRefreshFlag(ActivePage);
    OnOK(self);
    Exclude(ChangeSet, ActivePage);
    RememberOldValues(ActivePage); //prepare for another OKBtn_Click
  end;
end;

procedure TFormContainer.OnPanelHide(ep: TEditPage);
begin
  case ep of
    epNone: ;
    epParams: FormParams.Hide;
    epRegatta: FormRegatta.Hide;
    epUniqua: FormUniqua.Hide;
    epFleet: FormFleet.Hide;
    epEvent: FormInspector.Hide;
    epIni: FormInspector.Hide;
    epNames:
    begin
      FormNames.Hide;
      FormNames.NameFieldGrid.Free;
      FormNames.NameFieldGrid := nil;
    end;
    epScoring: FormScoring.Hide;
    epDB: FormDB.Hide;
  end;
end;

procedure TFormContainer.RememberOldValues(ep: TEditPage);
begin
  RefreshFlag := false;

  case ep of
    epParams:
    begin
      BOParams.Assign(BO.BOParams);
    end;

    epUniqua:
    begin
      oldShowCupColumn := BO.EventProps.ShowCupColumn;
    end;

    epEvent:
    begin
      oldShowPLZColumn := BO.EventProps.ShowPLZColumn;
      oldShowPosRColumn := BO.EventProps.ShowPosRColumn;
      oldNameFieldCount := StrToInt(BO.EventProps.NameFieldCount);
      oldNameFieldOrder := BO.EventProps.NameFieldOrder;
    end;
  end;
end;

procedure TFormContainer.UpdateRefreshFlag(ep: TEditPage);
begin
  case ep of

    epUniqua:
    begin
      if oldShowCupColumn <> BO.EventProps.ShowCupColumn then
      begin
        RefreshFlag := true;
      end;
    end;

    epEvent:
    begin
      if (oldShowPLZColumn <> BO.EventProps.ShowPLZColumn)
      or (oldShowPosRColumn <> BO.EventProps.ShowPosRColumn)
      or (oldNameFieldCount <> StrToInt(BO.EventProps.NameFieldCount))
      or (oldNameFieldOrder <> BO.EventProps.NameFieldOrder) then
      begin
        RefreshFlag := true;
      end;
    end;
  end;
end;

procedure TFormContainer.ShowInspector;
begin
  if not Assigned(FormInspector) then
  begin
    FormInspector := TFormInspector.Create(nil);
    ConfigureFormAsPanel(FormInspector);
    FormInspector.CloseBtn.Visible := false;
    FormInspector.OnOK := OnPanelOK;
  end;

  FormInspector.OnLoad := OnLoad;
  FormInspector.OnSave := OnSave;
  FormInspector.LoadModel;
  FormInspector.Visible := True;
end;

procedure TFormContainer.ConfigureFormAsPanel(f: TForm);
var
  b: TButton;
begin
  f.Parent := DetailPanel;
  f.BorderStyle := bsNone;
  f.Left := 0;
  f.Top := 0;
  b := FindButtonOnForm(f, 'OKBtn');
  if b <> nil then
  begin
    b.ModalResult := mrNone;
    //c.Caption := 'Apply';
  end;
  b := FindButtonOnForm(f, 'CancelBtn');
  if b <> nil then
  begin
    b.Visible := false;
  end;
end;

function TFormContainer.FindButtonOnForm(f: TForm; n: string): TButton;
var
  c: TComponent;
begin
  result := nil;
  c := f.FindComponent(n);
  if (c <> nil) and (c is TButton) then
    result := c as TButton;
end;

end.
