unit RiggVar.EM.ConHttp01;

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
  System.Math,
  IdHttp,
  IdMultipartFormData,
  RiggVar.EM.ConHttp,
  RiggVar.EM.Connection;

type
  THttpCon01 = class(THttpCon)
  private
    HttpClient: TIdHTTP;
    FResult: string;
    FError: string;
    SL: TStringList;
  protected
    procedure PostASPNET(const data: string);
    procedure PostDelphi(const data: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Post(const data: string); override;
  end;

implementation

{ THttpCon01 }

constructor THttpCon01.Create;
begin
  inherited Create;
  SL := TStringList.Create;

  HttpClient := TIdHTTP.Create(nil);
{$IFDEF VER150}
  //Timeout...
{$ELSE}
  HttpClient.ConnectTimeout := 1000;
{$ENDIF}
end;

destructor THttpCon01.Destroy;
begin
  HttpClient.Free;
  SL.Free;
  inherited;
end;

procedure THttpCon01.Post(const data: string);
begin
    PostASPNET(data)

//  if Pos('EventPad', url) > 0 then
//    PostASPNET(data)
//  else if Pos('EventData', url) > 0 then
//    PostASPNET(data)
//  else
//    PostDelphi(data);
end;

procedure THttpCon01.PostDelphi(const data: string);
begin
  try
    SL.Text := data;
    FResult := HttpClient.Post(url, SL);
  except
   on e: Exception do
     FError := e.Message;
  end;
end;

procedure THttpCon01.PostASPNET(const data: string);
var
  u: string;
  d: string;
  ms: TStringStream;
begin
  try
    u := StringReplace(url, 'ED', 'UD', []);
    d := data;
    HttpClient.Request.ContentType := 'text/xml';
    ms := TStringStream.Create(d);
    FResult := HttpClient.Post(u, ms);
    ms.Free;
  except
   on e: Exception do
     FError := e.Message;
  end;
end;

end.
