unit RiggVar.Util.InfoMemo;

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
  RiggVar.Util.Classes;

type
  TInfoMemo = class
  public
    class var WantLogo: Boolean;
    class function GetAppID: Integer; static;
    class function GetDescription(aid: Integer): string; static;
    class function GetApplicationType: Integer; static;
    class function GetEdition: string; static;
    class function GetProductName: string; static;
    class function GetProductDescription: string; static;
    class function GetFileVersion: string; static;
    class procedure WriteLogo(ML: TStrings); static;
    class procedure Fill(ML: TStrings);
  end;

implementation

const
  vqvFmt = '\StringFileInfo\%4.4x%4.4x\%s';
  FileVersionToken = 'FileVersion';

{ TInfoMemo }

class function TInfoMemo.GetAppID: Integer;
var
  e, n: string;
  s, IDName: string;
  i, j, le, ln: Integer;
begin
  result := 0;

  n := ExtractFileName(ParamStr(0));
  e := ExtractFileExt(n);
  ln := Length(n);
  le := Length(e);
  i := Length(n) - le;
  j := n.IndexOf('.');
  if i = j then
  begin
    IDName := Copy(n, 1, i);
    if IDName.StartsWith('FR') and (ln >= le + 2) then
    begin
      s := Copy(IDName, 3);
      result := StrToIntDef(s, 0);
    end;
  end;
end;

class function TInfoMemo.GetDescription(aid: Integer): string;
begin
  case aid of
    01: result := 'FR Application (Event only, without menu)';
    02: result := 'FR Application (Web minimal)';
    03: result := 'FR Application (Web only)';
    04: result := 'FR Application (Server)';

    62: result := 'FR Application (full)';
    90: result := 'FR Application (01 + Sockets)';
    91: result := 'FR Application (91)';
    92: result := 'FR Application (92)';
    93: result := 'FR Application (93)';
    94: result := 'FR Application (Event only)';
    95: result := 'FR Application (Race only)';

    99: result := 'FR Application (01 + Menu)';
    else
    begin
      result := 'FR Application';
    end;
  end;
end;

class procedure TInfoMemo.WriteLogo(ML: TStrings);
begin
  ML.Add('-');
  ML.Add('-       F');
  ML.Add('-      * * *');
  ML.Add('-     *   *   G');
  ML.Add('-    *     * *   *');
  ML.Add('-   E - - - H - - - I');
  ML.Add('-    *     * *         *');
  ML.Add('-     *   *   *           *');
  ML.Add('-      * *     *             *');
  ML.Add('-       D-------A---------------B');
  ML.Add('-                *');
  ML.Add('-                (C)  -  www.federgraph.de');
  ML.Add('-');
end;

class procedure TInfoMemo.Fill(ML: TStrings);
var
  FName: string;
  ApplicationName: string;
  ApplicationDir: string;
  FileVersion: string;
  ProductName: string;
  Description1: string;
  Description2: string;
begin
  ML.Clear;
  ProductName := TInfoMemo.GetProductName;
  Description1 := TInfoMemo.GetDescription(TInfoMemo.GetAppID);
  Description2 := TInfoMemo.GetProductDescription;
  FName := ParamStr(0);
  ApplicationName := ExtractFileName(FName);
  ApplicationDir := ExtractFilePath(ParamStr(0));
  FileVersion := GetFileVersion;

  ML.Add('product name  : ' + ProductName);
  ML.Add('product desc1 : ' + Description1);
  ML.Add('product desc2 : ' + Description2);
  ML.Add('');
  ML.Add('app location  : ' + ApplicationDir);
  ML.Add('app name      : ' + ApplicationName);
  ML.Add('app version   : ' + FileVersion);
  ML.Add('---');
  if WantLogo then
    WriteLogo(ML);
end;

class function TInfoMemo.GetFileVersion: string;
var
  FName: string;
  vlen: DWord;
  vptr: pchar;

  FInfo: pointer;
  FInfoSize: longint;
  FLang: PInteger;

  function GetEntry(KName: string): string;
  begin
    if VerQueryValue(FInfo,
      pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^), KName])),
      pointer(vptr), vlen)
    then
      result := vptr
    else
      result := '';
  end;

begin
  result := '';
  FName := ParamStr(0);
  FInfoSize := GetFileVersionInfoSize(pchar(FName), vlen);
  if FInfoSize > 0 then
  begin
    GetMem(FInfo, FInfoSize);
    if GetFileVersionInfo(pchar(fname), vlen, FInfoSize, FInfo) then
    begin
      VerQueryValue(FInfo, '\VarFileInfo\Translation', pointer(FLang), vlen);
      result := GetEntry(FileVersionToken);
    end;
  end;
  if FInfoSize > 0 then
    FreeMem(FInfo, FInfoSize);
end;

class function TInfoMemo.GetApplicationType: Integer;
var
  an: string;
  key: string;
begin
  result := -1;
  an := Application.Title;
  //1234
  //FRXX
  if (Length(an) = 4) and TUtils.StartsWith(an, 'FR') then
  begin
    key := Copy(an, 3, 2);
    result := StrToIntDef(key, 0);
  end;
end;

class function TInfoMemo.GetEdition: string;
var
  nn: Integer;
  t: string;
begin
  nn := TInfoMemo.GetApplicationType;
  t := '';
  case nn of
    1, 11: t := 'Readonly edition.';
    2, 12: t := 'Webonly edition.';
    3, 13: t := 'Bridge Client edition.';
    4, 14: t := 'Online edition.';
    5, 15: t := 'Home Server edition.';
    6, 16: t := 'EC2 edition.';
    7, 17: t := 'Selector edition.';
    8, 18: t := 'Windows Service.';
    9, 19: t := 'Output Bridge edition.';
  end;
  result := t;
end;

class function TInfoMemo.GetProductName: string;
var
  t: string;
begin
  t := TInfoMemo.GetEdition;
  if Length(t) > 1 then
    result := Format('RiggVar FleetRace FR%.2d', [TInfoMemo.GetApplicationType])
  else
    result := 'RiggVar FleetRace';
end;

class function TInfoMemo.GetProductDescription: string;
var
  t: string;
begin
  t := TInfoMemo.GetEdition;
  if Length(t) > 1 then
    result := 'FR Application, ' + t
  else
    result := 'FR Application';
end;

end.
