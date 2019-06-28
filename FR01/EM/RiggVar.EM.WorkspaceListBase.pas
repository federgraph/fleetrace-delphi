unit RiggVar.EM.WorkspaceListBase;

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
  RiggVar.Util.Classes;

type
  TUrlScheme = (usHttp, usFile, usApp);

  TDataFormat = (
    ffSrc,
    ffTxt,
    ffXml,
    ffHtml
    );

  TWorkspaceUrl = class
  public
    Value: string;
    function GetScheme: TUrlScheme;
    function IsAppScheme: Boolean;
    function IsHttpScheme: Boolean;
  end;

  TWorkspaceListBase = class
  protected
    TL: TStringList;
    VL: TStringList;
    procedure AddUrl(s: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(FromMemo: Boolean); virtual;
    procedure FillCombo(Combo: TStrings); virtual;

    procedure LoadDefault; virtual;

    procedure Load; virtual;
    procedure Save; virtual;

    function GetName(i: Integer): string; virtual;
    function GetUrl(i: Integer): string; virtual;
    function IsWritable(i: Integer): Boolean; virtual;
    function GetWriteFormat(url: string): TDataFormat; virtual;

    function GetTemp: string; virtual;
    function GetText: string; virtual;
    procedure SetText(Value: string); virtual;
  end;

implementation

uses
  Windows,
  SHFolder;

var
  u0: string = 'app://DocManager/EventMenu.xml';

const
  u1 = 'Example: www.fleetrace.org/txt=http://www.fleetrace.org/EventMenu.txt';
  u2 = 'Example: data.riggvar.de/txt=http://data.riggvar.de/EventMenu.txt';

  //using jekyll locally
  u3 = 'Example: localhost:4000/html=http://localhost:4000/html/EventMenu.xml';

  //using jekyll at GitHub Pages
  u4 = 'Example: github/xml=https://federgraph.github.io/fleetrace-example-data/xml/EventMenu.xml';
  u5 = 'Example: github/html=https://federgraph.github.io/fleetrace-example-data/html/EventMenu.xml';
  u6 = 'Example: github/txt=https://federgraph.github.io/fleetrace-example-data/text/EventMenu.txt';

  u7 = '* local workspace = D:\EventMenu\EventMenu.xml';
  u8 = '* local workspace = D:\EventMenu\EventMenuLocal.xml';

  old_u3 = 'Example: data.riggvar.de/xml=http://data.riggvar.de/EventMenu.xml';
  old_u4 = 'Example: www.riggvar.de/xml=http://www.riggvar.de/results/EventMenu.xml';
  old_u5 = 'Example: www.fleetrace.org/demo=http://www.fleetrace.org/DemoIndex.xml';
  old_u6 = 'Example: www.riggvar.de/html=http://www.riggvar.de/results/EventMenuHtml.xml';

{ TWorkspaceListBase }

constructor TWorkspaceListBase.Create;
begin
  TL := TStringList.Create;
  VL := TStringList.Create;
end;

destructor TWorkspaceListBase.Destroy;
begin
  VL.Free;
  TL.Free;
  inherited;
end;

procedure TWorkspaceListBase.Init(FromMemo: Boolean);
begin
  if VL.Count = 0 then
  begin
    LoadDefault;
    VL.Text := TL.Text;
  end;
end;

procedure TWorkspaceListBase.LoadDefault;
var
  WantXML: Boolean;
  WantNewUrls: Boolean;
begin
  TL.Clear;

  WantXml := CheckWin32Version(5,1);

  //Requires OpenSSL DLL's in output folder:
  //for Win32 build: libea32.dll and ssleay32.dll
  //from https://indy.fulgan.com/SSL/
  WantNewUrls := False;

  if WantXml then
  begin
    if WantNewUrls then
    begin
    AddUrl(u3);
    AddUrl(u4);
    AddUrl(u5);
    AddUrl(u6);
    end
    else
    begin
      AddUrl(old_u3);
      AddUrl(old_u4);
      AddUrl(old_u5);
      AddUrl(old_u6);
    end;

    AddUrl(u7);
    AddUrl(u8);
  end
  else
  begin
    AddUrl(u1);
    AddUrl(u2);
  end;
end;

procedure TWorkspaceListBase.AddUrl(s: string);
var
  i: Integer;
  u: string;
begin
  i := Pos('=', s);
  if i > 0 then
    u := Trim(Copy(s, i+1, Length(s)))
  else
    u := Trim(s);
  TL.Add(u);
end;

procedure TWorkspaceListBase.FillCombo(Combo: TStrings);
var
  i: Integer;
begin
  Combo.Clear;
  for i := 0 to VL.Count - 1 do
    Combo.Add(VL[i]);
end;

function TWorkspaceListBase.GetTemp: string;
begin
  result := TL.Text;
end;

function TWorkspaceListBase.GetName(i: Integer): string;
begin
  result := '';
end;

function TWorkspaceListBase.GetUrl(i: Integer): string;
begin
  result := '';
  if (i >= 0) and (i < VL.Count) then
    result := VL[i];
end;

function TWorkspaceListBase.GetWriteFormat(url: string): TDataFormat;
begin
  result := ffTxt;
end;

function TWorkspaceListBase.IsWritable(i: Integer): Boolean;
begin
  result := false;
end;

procedure TWorkspaceListBase.Load;
begin

end;

function TWorkspaceListBase.GetText: string;
begin
  result := VL.Text;
end;

procedure TWorkspaceListBase.Save;
begin

end;

procedure TWorkspaceListBase.SetText(Value: string);
begin
  //not implemented
end;

{ TWorkspaceUrl }

function TWorkspaceUrl.GetScheme: TUrlScheme;
begin
  result := usFile;
  if IsHttpScheme then
    result := usHttp;
  if IsAppScheme then
    result := usApp;
end;

function TWorkspaceUrl.IsAppScheme: Boolean;
begin
  result := TUtils.StartsWith(Value, 'app');
end;

function TWorkspaceUrl.IsHttpScheme: Boolean;
begin
  result := TUtils.StartsWith(Value, 'http');
end;

end.
