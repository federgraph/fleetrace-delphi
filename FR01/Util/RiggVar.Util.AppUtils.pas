unit RiggVar.Util.AppUtils;

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

type
  TAppUtils = class
  public
    class function GetFullExeName: string;
    class function GetFileNameWithoutExtension: string;
    class function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
    class function GetAppDataDir: string;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.SHFolder,
  Winapi.SHLObj,
  System.SysUtils,
  RiggVar.Util.Classes;

{$ifdef WebBroker}
class function TAppUtils.GetFullExeName: string;
var
  path: array[0..Max_Path] of char;
  s: string;
  p: Integer;
begin
  try
    SetString(s, path, GetModuleFileName(HInstance, path, SizeOf(path)));
    //s := '\\?\c:\inetpub\wwwroot\Scripts\FR64.exe'
    //s := '\\?\c:\inetpub\FR64.exe'
    p := Pos(':', s);
    if p > 2 then
      s := Copy(s, p-1, MaxInt);
    result := s;
  except
    result := 'error';
  end;
end;
{$else}
class function TAppUtils.GetFullExeName: string;
begin
  result := ParamStr(0);
end;
{$endif}

class function TAppUtils.GetFileNameWithoutExtension: string;
begin
  result := LowerCase(ChangeFileExt(ExtractFileName(GetFullExeName), ''));
end;

class function TAppUtils.GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
var
  FilePath: array [0..255] of char;
begin
  SHGetSpecialFolderPath(0, @FilePath[0], FOLDER, CanCreate);
  Result := FilePath;
end;

class function TAppUtils.GetAppDataDir: string;
begin
  result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA, True);
  result := IncludeTrailingPathDelimiter(result) + 'RiggVar\FR\ED\';
  ForceDirectories(result);
end;

end.
