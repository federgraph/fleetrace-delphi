unit RiggVar.Conn.REST;

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
  IdHTTP,
  IdMultipartFormData;

type
  TRESTConnection = class
  private
    FExtension: string;
    FEventType: Integer;
    FController_Url: string;
    d: TIdHTTP;
  public
    SL: TStringList;
    constructor Create(EventType: Integer);
    destructor Destroy; override;

    function Post(pageName: string): string;
    function Get(pageName: string): string;
    function MultiLinePost(pageName: string; Stream: TIdMultiPartFormDataStream): string;

    function GetPageUrl(pageName: string): string;
    function GetServerUrl: string;
    procedure SetServerUrl(const Value: string);
    procedure SetExtension(const Value: string);
  end;

implementation

constructor TRESTConnection.Create(EventType: Integer);
begin
  FExtension := '.aspx';
  FEventType := EventType;
  FController_Url := 'http://localhost/FR83/REST/'; //overriden by configuration
  SL := TStringList.Create;
  d := TIdHTTP.Create(nil);
{$IFDEF VER150}
  //Timeout...
{$ELSE}
  d.ConnectTimeout := 1000;
{$ENDIF}
end;

destructor TRESTConnection.Destroy;
begin
  d.Free;
  SL.Free;
  inherited;
end;

procedure TRESTConnection.SetExtension(const Value: string);
begin
  FExtension := Value;
end;

procedure TRESTConnection.SetServerUrl(const Value: string);
begin
  FController_Url := Value;
end;

function TRESTConnection.GetServerUrl: string;
begin
  result := FController_Url;
end;

function TRESTConnection.GetPageUrl(pageName: string): string;
begin
  if FEventType = 400 then
    result := GetServerUrl + 'FR/' + pageName + FExtension
  else if FEventType = 600 then
    result := GetServerUrl + 'SKK/' + pageName + FExtension
  else
    result := GetServerUrl + pageName + FExtension
end;

function TRESTConnection.Get(pageName: string): string;
begin
  result := '';
  try
    d.Request.ContentType := '';
    result := d.Get(GetPageUrl(pageName));
  except
   on e: Exception do
    result := e.Message;
  end;
end;

function TRESTConnection.Post(pageName: string): string;
begin
  try
    d.Request.ContentType := '';
    result := d.Post(GetPageUrl(pageName), SL);
  except
   on e: Exception do
    result := e.Message;
  end;
end;

function TRESTConnection.MultiLinePost(pageName: string; Stream: TIdMultiPartFormDataStream): string;
begin
  try
    d.Request.ContentType := Stream.RequestContentType;
    result := d.Post(GetPageUrl(pageName), Stream);
  except
   on e: Exception do
    result := e.Message;
  end;
end;

end.
