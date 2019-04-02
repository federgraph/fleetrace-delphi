unit RiggVar.DAL.WEB;

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
  RiggVar.DAL.Intf,
  RiggVar.BO.IniImage,
  RiggVar.App.Config,
  DataService;

type
  TCompDataWeb = class(TInterfacedObject, IDBEvent)
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

{ TCompDataWeb }

constructor TCompDataWeb.Create;
begin
  inherited Create;
end;

destructor TCompDataWeb.Destroy;
begin
  inherited;
end;

function TCompDataWeb.Load(KatID: Integer; EventName: string): string;
begin
  try
    DataService.DataServerName := Main.IniImage.WebApplicationUrl;
    result := DataService.GetDataServiceSoap().LoadEventData(KatID, EventName);
  except
    result := '';
  end;
end;

procedure TCompDataWeb.Save(KatID: Integer; EventName, Data: string);
var
  Password: string;
begin
  //for testing only
  try
    Password := 'abc';
    DataService.DataServerName := Main.IniImage.WebApplicationUrl;
    DataService.GetDataServiceSoap().SaveEventData(KatID, EventName, Data, Password);
  except
  end;
end;

procedure TCompDataWeb.Delete(KatID: Integer; EventName: string);
begin
  //not implemented
end;

function TCompDataWeb.GetEventNames(KatID: Integer): string;
var
  EventFilter: string;
begin
  try
    EventFilter := 'FR';
    DataService.DataServerName := Main.IniImage.WebApplicationUrl;
    result := DataService.GetDataServiceSoap().LoadEventNames(KatID, EventFilter);
  except
    result := '';
  end;
end;

procedure TCompDataWeb.Close;
begin
  //nothing to be done
end;

end.
