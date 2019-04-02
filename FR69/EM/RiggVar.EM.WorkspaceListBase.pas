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

  u3 = 'Example: data.riggvar.de/xml=http://data.riggvar.de/EventMenu.xml';
  u4 = 'Example: www.riggvar.de/xml=http://www.riggvar.de/results/EventMenu.xml';
  u5 = 'Example: www.fleetrace.org/demo=http://www.fleetrace.org/DemoIndex.xml';
  u6 = 'Example: www.riggvar.de/html=http://www.riggvar.de/results/EventMenuHtml.xml';
  u7 = '* local workspace = D:\EventMenu\EventMenu.xml';
  u8 = '* local workspace = D:\EventMenu\EventMenuLocal.xml';

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
  WantXML: boolean;
begin
  TL.Clear;

  WantXml := CheckWin32Version(5,1);

  if WantXml then
  begin
    AddUrl(u3);
    AddUrl(u4);
    AddUrl(u5);
    AddUrl(u6);
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
