unit RiggVar.Web1.Images;

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
  IdContext,
  IdBaseComponent,
  IdComponent,
  IdTCPServer,
  IdCustomHTTPServer;

type
  TImageManager = class
  private
    dn: string;
    function GetResourceName(fn: string): string;
    function GetResourceType(rn: string): string;
    function GetImageStream(rn: string): TStream;
  public
    UseResources: Boolean;
    constructor Create;
    procedure ServeImage(AContext: TIdContext;
      ri: TIdHttpResponseInfo; fn: string);
  end;

implementation

uses
  RiggVar.Util.Classes,
  RiggVar.Util.AppUtils;

{ TImageManager }

constructor TImageManager.Create;
begin
  inherited Create;
  dn := IncludeTrailingPathDelimiter(ExtractFilePath(TAppUtils.GetFullExeName));
end;

procedure TImageManager.ServeImage(AContext: TIdContext;
  ri: TIdHttpResponseInfo; fn: string);
var
  s: string;
begin
  try
    if UseResources then
    begin
      s := ri.HTTPServer.MIMETable.GetFileMIMEType(fn);
      ri.ContentType := s;
      ri.ContentStream := GetImageStream(GetResourceName(fn));
      ri.FreeContentStream := True; //is already true
      ri.WriteContent; //writes header and releases ContentStream
    end
    else
    begin
      if (fn <> '') and TUtils.StartsWith(fn, '/') then
        s := ExcludeTrailingPathDelimiter(dn) + fn
      else if (fn <> '') and TUtils.StartsWith(fn, '\') then
        s := ExcludeTrailingPathDelimiter(dn) + fn
      else
        s := dn + fn;
      if FileExists(s) then
     begin
        ri.ServeFile(AContext, s);
       end;
    end;
  except
    on e: Exception do
    begin
      OutputDebugString(PChar(e.Message));
      UseResources := False;
    end;
  end;
end;

function TImageManager.GetImageStream(rn: string): TStream;
var
  rt: string;
begin
  rt := GetResourceType(rn);
  result := TResourceStream.Create(HInstance, rn, PChar(rt));
  result.Seek(0, soFromBeginning);
  //do not free the stream, this is done by Indy
end;

function TImageManager.GetResourceName(fn: string): string;
begin
  result := '';

  if fn = '/images/bg/bg-red.png' then
    result := 'bg_red_png'
  else if fn = '/images/bg/bg-brown.png' then
    result := 'bg_brown_png';

  if fn = '/images/msgflow-01.jpg' then
    result := 'msgflow_01_jpg'
  else if fn = '/images/msgflow-02.jpg'then
    result := 'msgflow_02_jpg';

end;

function TImageManager.GetResourceType(rn: string): string;
begin
  if Pos('_gif', rn) > 0 then
    result := 'GIF'
  else if Pos('_png', rn) > 0 then
    result := 'PNG'
  else if Pos('_jpg', rn) > 0 then
    result := 'JPEG'
  else
    result := 'TEXT'
end;

end.
