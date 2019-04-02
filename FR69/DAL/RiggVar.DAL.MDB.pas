unit RiggVar.DAL.MDB;

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
  Data.DB,
  Data.Win.ADODB,
  RiggVar.DAL.Intf;

type
  TdmCompDataMDB = class(TDataModule, IDBEvent)
    ADOConnection: TADOConnection;
    qKeys: TADOQuery;
    qValue: TADOQuery;
    qValueV: TMemoField;
    qKeysK: TWideStringField;
    qDelete: TADOCommand;
    qUpdate: TADOCommand;
    qKey: TADOQuery;
    qKeyK: TWideStringField;
    qInsert: TADOCommand;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    function KeyExists(KatID: Integer; EventName: string): Boolean;
    { IDBEvent }
    function Load(KatID: Integer; EventName: string): string;
    procedure Save(KatID: Integer; EventName: string; Data: string);
    procedure Delete(KatID: Integer; EventName: string);
    function GetEventNames(KatID: Integer): string;
    procedure Close;
  public
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

{ TdmCompDataMDB }

procedure TdmCompDataMDB.DataModuleCreate(Sender: TObject);
var
  fn: string;
begin
  fn := Main.FolderInfo.SettingsPath + 'FleetRace.udl';
  ADOConnection.Connected := False;
  ADOConnection.ConnectionTimeout := 5;
  ADOConnection.ConnectionString := 'FILE NAME=' + fn;
  ADOConnection.Connected := True;
end;

procedure TdmCompDataMDB.DataModuleDestroy(Sender: TObject);
begin
  { Problem:
    - wird nicht aufgerufen, wenn Owner = nil
    - exception, mit Owner = Application, wenn Interface-Referenz nil gesetzt wird
    }
end;

procedure TdmCompDataMDB.Close;
begin
  if ADOConnection.Connected then
    ADOConnection.Close;
end;

procedure TdmCompDataMDB.Delete(KatID: Integer; EventName: string);
begin
  qDelete.Parameters.ParamByName('ParamKatID').Value := KatID;
  qDelete.Parameters.ParamByName('ParamKey').Value := EventName;
  qDelete.Execute;
end;

function TdmCompDataMDB.GetEventNames(KatID: Integer): string;
var
  SL: TStrings;
begin
  result := '';
  qKeys.Active := False;
  qKeys.Parameters.ParamByName('ParamKatID').Value := KatID;
  qKeys.Active := True;
  SL := TStringList.Create;
  try
    qKeys.First;
    while not qKeys.Eof do
    begin
      SL.Add(qKeysK.AsString);
      qKeys.Next;
    end;
    result := SL.Text;
  finally
  SL.Free;
  end;
end;

function TdmCompDataMDB.Load(KatID: Integer; EventName: string): string;
begin
  qValue.Active := False;
  qValue.Parameters.ParamByName('ParamKatID').Value := KatID;
  qValue.Parameters.ParamByName('ParamKey').Value := EventName;
  qValue.Active := True;
  if qValue.Eof then
    result := ''
  else
    result := qValueV.AsString;
  qValue.Active := False;
end;

procedure TdmCompDataMDB.Save(KatID: Integer; EventName, Data: string);
begin
  if KeyExists(KatID, EventName) then
  begin
    qUpdate.Parameters.ParamByName('ParamKatID').Value := KatID;
    qUpdate.Parameters.ParamByName('ParamKey').Value := EventName;
    qUpdate.Parameters.ParamByName('ParamValue').Value := Data;
    qUpdate.Execute;
  end
  else
  begin
    qInsert.Parameters.ParamByName('ParamKatID').Value := KatID;
    qInsert.Parameters.ParamByName('ParamKey').Value := EventName;
    qInsert.Parameters.ParamByName('ParamValue').Value := Data;
    qInsert.Execute;
  end;
end;

function TdmCompDataMDB.KeyExists(KatID: Integer; EventName: string): Boolean;
begin
  result := True;
  qKey.Active := False;
  qKey.Parameters.ParamByName('ParamKatID').Value := KatID;
  qKey.Parameters.ParamByName('ParamKey').Value := EventName;
  qKey.Open;
  if qKey.eof and qKey.Bof then
    result := False;
  qKey.Close;
end;

end.
