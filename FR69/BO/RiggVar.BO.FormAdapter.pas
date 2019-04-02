unit RiggVar.BO.FormAdapter;

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
  System.Classes,
  RiggVar.BO.Watches,
  RiggVar.BO.IniImage;

type
  TFormAdapterBase = class
  public
    function EditSwitchProps(Sender: TObject): Boolean; virtual;
    function EditBridgeProps(Sender: TObject): Boolean; virtual;
    function EditConnectionProps(Section: TConfigSection): Boolean; virtual;
    function EditBridgeProviderID(CurrentProviderID: Integer): Integer; virtual;
    function EditScoringModule(Sender: TObject): Boolean; virtual;
    function EditRegattaProps(Sender: TObject): Boolean; virtual;
    function EditUniquaProps(Sender: TObject): Boolean; virtual;
    function EditFleetProps(Sender: TObject): Boolean; virtual;
    function EditSchedule(Sender: TObject): Boolean; virtual;

    procedure AfterDeleteBO(Sender: TObject); virtual;
    procedure BeforeDeleteBO(Sender: TObject); virtual;
    procedure DisposeFormWorkspace; virtual;

    function ChooseDB: Boolean; virtual;
    function ChooseDocAvail(SL: TStringList): string; virtual;
    function ChooseNewEventName: string; virtual;

    function GetWatchGUI: IWatchGUI; virtual;
    procedure ShowError(msg: string); virtual;
    function ConfirmOperation(Caption: string): Boolean; virtual;
  end;

implementation

{ TFormAdapterBase }

function TFormAdapterBase.EditBridgeProps(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditBridgeProviderID(
  CurrentProviderID: Integer): Integer;
begin
  result := CurrentProviderID;
end;

function TFormAdapterBase.EditConnectionProps(Section: TConfigSection): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditFleetProps(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditRegattaProps(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditSchedule(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditScoringModule(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditSwitchProps(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.EditUniquaProps(Sender: TObject): Boolean;
begin
  result := false;
end;

function TFormAdapterBase.GetWatchGUI: IWatchGUI;
begin
  result := nil;
end;

procedure TFormAdapterBase.ShowError(msg: string);
begin
end;

procedure TFormAdapterBase.AfterDeleteBO(Sender: TObject);
begin

end;

procedure TFormAdapterBase.BeforeDeleteBO(Sender: TObject);
begin

end;

function TFormAdapterBase.ChooseDB: Boolean;
begin
  result := false;
end;

function TFormAdapterBase.ChooseDocAvail(SL: TStringList): string;
begin
  result := '';
end;

function TFormAdapterBase.ChooseNewEventName: string;
begin
  result := '';
end;

function TFormAdapterBase.ConfirmOperation(Caption: string): Boolean;
begin
  result := true;
end;

procedure TFormAdapterBase.DisposeFormWorkspace;
begin

end;

end.
