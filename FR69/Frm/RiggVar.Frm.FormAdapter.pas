unit RiggVar.Frm.FormAdapter;

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
  Vcl.Forms, 
  Vcl.Controls,
  Vcl.Dialogs,
  RiggVar.BO.FormAdapter,
  RiggVar.BO.IniImage,
  RiggVar.BO.Watches,
  FrmWatches,
  FrmSelectName;

type
  TFormAdapter = class(TFormAdapterBase)
  private
    FormSelectName: TFormSelectName;
    WatchGui: TWatchGui;
  public
    destructor Destroy; override;

    function EditSwitchProps(Sender: TObject): Boolean; override;
    function EditBridgeProps(Sender: TObject): Boolean; override;
    function EditConnectionProps(Section: TConfigSection): Boolean; override;
    function EditBridgeProviderID(CurrentProviderID: Integer): Integer; override;
    function EditScoringModule(Sender: TObject): Boolean; override;
    function EditRegattaProps(Sender: TObject): Boolean; override;
    function EditUniquaProps(Sender: TObject): Boolean; override;
    function EditFleetProps(Sender: TObject): Boolean; override;
    function EditSchedule(Sender: TObject): Boolean; override;

    procedure AfterDeleteBO(Sender: TObject); override;
    procedure BeforeDeleteBO(Sender: TObject); override;

    function ChooseDB: Boolean; override;
    function ChooseDocAvail(SL: TStringList): string; override;
    function ChooseNewEventName: string; override;
    function ConfirmOperation(Caption: string): Boolean; override;
    procedure DisposeFormWorkspace; override;

    function GetWatchGUI: IWatchGUI; override;
    procedure ShowError(msg: string); override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.App.Config,
  RiggVar.BO.EventProps,
  RiggVar.BR.BridgeController,
  RiggVar.Calc.EV,
  RiggVar.Out.JS00,
  FrmBridgeManager,
  FrmBridgeProps,
  FrmConnectionProps,
  FrmDocManager,
  FrmFleetProps,
  FrmRegattaProps,
  FrmSchedule,
  FrmScoringModule,
  FrmSwitchProps,
  FrmUniquaProps,
  FrmWorkspaceFiles;

{ TFormAdapter }

destructor TFormAdapter.Destroy;
begin
  WatchGui := nil;
  if FormSelectName <> nil then
  begin
    FormSelectName.Free;
    FormSelectName := nil;
  end;
  inherited;
end;

function TFormAdapter.EditBridgeProps(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is TBridgeController then
    result := FrmBridgeProps.EditBridgeProps(Sender as TBridgeController);
end;

function TFormAdapter.EditBridgeProviderID(CurrentProviderID: Integer): Integer;
var
  id: Integer;
  f: TFormBridgeManager;
  ini: TIniImage;
begin
  result := CurrentProviderID;
  f := TFormBridgeManager.Create(nil);
  try
    f.LoadModel(CurrentProviderID);
    if f.ShowModal = mrOK then
    begin
      id := f.ProviderID;
      ini := Main.IniImage;
      f.SaveModel(ini);
      result := id;
    end;
  finally
    f.Free;
  end;
end;

function TFormAdapter.EditConnectionProps(Section: TConfigSection): Boolean;
begin
  result := FrmConnectionProps.EditConnectionProps(Section);
end;

function TFormAdapter.EditFleetProps(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is TEventProps then
    result := FrmFleetProps.EditFleetProps(Sender as TEventProps);
end;

function TFormAdapter.EditRegattaProps(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is TEventProps then
    result := FrmRegattaProps.EditRegattaProps(Sender as TEventProps);
end;

function TFormAdapter.EditSchedule(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is IJavaScoreXMLWriter then
    result := FrmSchedule.EditSchedule(Sender as IJavaScoreXMLWriter);
end;

function TFormAdapter.EditScoringModule(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is TCalcEvent then
    result := FrmScoringModule.EditScoringModule(Sender as TCalcEvent);
end;

function TFormAdapter.EditSwitchProps(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is TIniImage then
    result := FrmSwitchProps.EditSwitchProps(Sender as TIniImage);
end;

function TFormAdapter.EditUniquaProps(Sender: TObject): Boolean;
begin
  result := false;
  if Sender is TEventProps then
    result := FrmUniquaProps.EditUniquaProps(Sender as TEventProps);
end;

function TFormAdapter.GetWatchGUI: IWatchGUI;
begin
  if not Assigned(WatchGUI) then
    WatchGUI := TWatchGUI.Create;
  result := WatchGUI;
end;

procedure TFormAdapter.ShowError(msg: string);
begin
  ShowMessage(msg);
end;

function TFormAdapter.ChooseDB: Boolean;
begin
  result := FrmDocManager.EditDBEvent(Main.DocManager);
end;

function TFormAdapter.ChooseDocAvail(SL: TStringList): string;
begin
  result := '';
  if not Assigned(FormSelectName) then
    FormSelectName := TFormSelectName.Create(nil);
  with FormSelectName do
  begin
    Caption := 'Select Available Document';
    PromptLabel.Caption := 'Documents in Database:';
    ListBox.Items.Assign(SL);
    ShowModal;
    if ModalResult = mrOK then
      Result := SelectedName
  end;
end;

function TFormAdapter.ChooseNewEventName: string;
begin
  result := InputBox('GetNewDocName', 'save file as', Main.DocManager.EventName);
end;

procedure TFormAdapter.DisposeFormWorkspace;
begin
  if Assigned(FormWorkspaceFiles) then
  begin
    { FormWorkspaceFiles becomes invalid:
        - caches Interface Reference
        - holds Reference to DataSet in DataSource
    }
    FormWorkspaceFiles.Free;
    FormWorkspaceFiles := nil; //redundant, see destructor of FormWorkspaceFiles
  end;
end;

procedure TFormAdapter.BeforeDeleteBO(Sender: TObject);
begin
  WatchGUI := nil;
  Application.ProcessMessages;
  try
    if Main.IniImage.AutoSave then
      Main.DocManager.DocSave
    else if not Main.IniImage.NoAutoSave then
    begin
      if Application.MessageBox('Save changes?',
        PChar('closing Event...'), MB_YESNO) = IDYES
      then
        Main.DocManager.DocSave;
    end;
  except;
    Main.Logger.Error('Exception in TBOContainer.Destroy');
  end;
end;

procedure TFormAdapter.AfterDeleteBO(Sender: TObject);
begin
  Application.ProcessMessages;
end;

function TFormAdapter.ConfirmOperation(Caption: string): Boolean;
begin
  result := Application.MessageBox(
    'no return, please confirm operation',
    PWideChar(Caption),
    MB_OKCANCEL) = IDOK;
end;

end.
