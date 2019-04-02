unit RiggVar.DAL.REST;

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
  Windows,
  SysUtils, Classes,
  RiggVar.DAL.Intf,
  RiggVar.BO.IniImage,
  RiggVar.App.Config,
  RiggVar.Conn.REST,
  IdMultipartFormData;

type
  TCompDataREST = class(TInterfacedObject, IDBEvent)
  private
    Connection: TRESTConnection;
    SL: TStringList;
    function GetRESTRoot: string;
  public
    constructor Create;
    destructor Destroy; override;
    { IDBEvent }
    function Load(KatID: Integer; EventName: string): string;
    procedure Save(KatID: Integer; EventName: string; Data: string);
    procedure Delete(KatID: Integer; EventName: string);
    function GetEventNames(KatID: Integer): string;
    procedure Close;
  end;

implementation

uses
  RiggVar.App.Main;

{ TCompDataREST }

constructor TCompDataREST.Create;
begin
  inherited Create;
  Connection := TRESTConnection.Create(0);
  SL := Connection.SL;
  Connection.SetServerUrl(GetRestRoot);
end;

destructor TCompDataREST.Destroy;
begin
  SL := nil;
  Connection.Free;
  inherited;
end;

function TCompDataREST.GetRESTRoot: string;
var
  s: string;
  c: char;
  l: Integer;
begin
  s := Main.IniImage.WebApplicationUrl;
  l := Length(s);
  if l > 0 then
  begin
    c := s[l];
    if c <> '/' then
      s := s + '/';
  end
  else
    s := Connection.GetServerUrl; //Default

  s := s + 'REST/';
  result := s;
end;

function TCompDataREST.Load(KatID: Integer; EventName: string): string;
var
  s: string;
begin
  SL.Clear;
  SL.Add('KatID=' + IntToStr(KatID));
  SL.Add('EventName=' + EventName);
  s := Connection.Post('LoadEventData');
  if (s = 'error') then
    result := ''
  else
    result := s;
end;

procedure TCompDataREST.Save(KatID: Integer; EventName, Data: string);
var
  Password: string;
  s: string;
  M: TIdMultiPartFormDataStream;
begin
  Password := 'abc';
  M := TIdMultiPartFormDataStream.Create;
  M.AddFormField('KatID', IntToStr(KatID));
  M.AddFormField('EventName', EventName);
  M.AddFormField('EventData', Data);
  M.AddFormField('Password', Password);
  s := Connection.MultiLinePost('SaveEventData', M);
  OutputDebugString(PChar(s));
  M.Free;
end;

procedure TCompDataREST.Delete(KatID: Integer; EventName: string);
begin
  //not implemented
end;

function TCompDataREST.GetEventNames(KatID: Integer): string;
var
  EventFilter: string;
begin
  EventFilter := 'FR';

  SL.Clear;
  SL.Add('KatID=' + IntToStr(KatID));
  SL.Add('EventFilter=' + EventFilter);
  result := Connection.Post('LoadEventNames');
end;

procedure TCompDataREST.Close;
begin
  //nothing to be done
end;

end.
