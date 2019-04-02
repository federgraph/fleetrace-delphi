unit RiggVar.BO.ResourceManager;

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
  RiggVar.Util.AppUtils;

type
  TResourceKey = (
    FRXML20_xsl,
    rvtp_xsl,
    core_js,
    rvts_js,
    rvtw03_js,
    rvtw03_css,
    rvtw05_js,
    rvtw05_css,
    rvtw08_js,
    rvtw08_css,
    jquery_js,
    site_css,
    style_css,
    style_h_css,
    style_r_css,
    style_p_css
  );

  TResourceManager = class
  private
    dn: string;
    ssFolder: string;
    jsFolder: string;
    stFolder: string;
    FUseResources: Boolean;
    function InitFromFile(fn: string): string;
    function InitFromResource(rn: string): string;
    procedure SetUseResources(const Value: Boolean);
    function GetFileName(rk: TResourceKey): string;
  public
    constructor Create;
    function LoadText(rk: TResourceKey): string;
    function LoadFile(rk: TResourceKey): string;
    function GetTextFileContent(path: string): string;
    procedure ExportAll;
    procedure ExportOne(rk: TResourceKey);
    property UseResources: Boolean read FUseResources write SetUseResources;
  end;

const
  ResourceNames: array[TResourceKey] of string = (
    'FRXML20_xsl',
    'rvtp_xsl',
    'core_js',
    'rvts_js',
    'rvtw03_js',
    'rvtw03_css',
    'rvtw05_js',
    'rvtw05_css',
    'rvtw08_js',
    'rvtw08_css',
    'jquery_js',
    'site_css',
    'style_css',
    'style_h_css',
    'style_r_css',
    'style_p_css'
   );

implementation

{ TReourceManager }

constructor TResourceManager.Create;
begin
  inherited Create;
  dn := IncludeTrailingPathDelimiter(ExtractFilePath(TAppUtils.GetFullExeName));
  jsFolder := dn + 'javascripts\';
  ssFolder := dn + 'stylesheets\';
  stFolder := dn + 'styles\';
  UseResources := True;
end;

procedure TResourceManager.SetUseResources(const Value: Boolean);
begin
  FUseResources := Value;
end;

function TResourceManager.GetFileName(rk: TResourceKey): string;
begin
  result := '';
  case rk of
    FRXML20_xsl: result := ssFolder + 'FRXML20.xsl';
    rvtp_xsl: result := ssFolder + 'rvtp.xsl';

    core_js: result := jsFolder + 'core.js';
    rvts_js: result := jsFolder + 'rvts.js';

    rvtw03_js: result := jsFolder + 'rvtw03.js';
    rvtw03_css: result := ssFolder + 'rvtw03.css';

    rvtw05_js: result := jsFolder + 'rvtw05.js';
    rvtw05_css: result := ssFolder + 'rvtw05.css';

    rvtw08_js: result := jsFolder + 'rvtw08.js';
    rvtw08_css: result := ssFolder + 'rvtw08.css';

    site_css: result := ssFolder + 'Site.css';
    jquery_js: result := jsFolder + 'jquery-1.3.2.min.js';

    style_css: result := stFolder + 'style.css';
    style_h_css: result := stFolder + 'style-h.css';
    style_r_css: result := stFolder + 'style-r.css';
    style_p_css: result := stFolder + 'style-p.css';
  end;
end;

procedure TResourceManager.ExportAll;
var
  rk: TResourceKey;
begin
  try
    for rk := Low(TResourceKey) to High(TResourceKey) do
    begin
      ExportOne(rk);
    end;
  except
  end;
end;

procedure TResourceManager.ExportOne(rk: TResourceKey);
var
  rn: string;
  fn: string;
  SL: TStringList;
begin
  SL := TStringList.Create;
  rn := ResourceNames[rk];
  SL.Text := InitFromResource(rn);
  fn := GetFileName(rk);
  if DirectoryExists(ExtractFilePath(fn)) then
    if not FileExists(fn) then
      SL.SaveTofile(fn);
  SL.Free;
end;

function TResourceManager.LoadText(rk: TResourceKey): string;
begin
  if UseResources then
    result := InitFromResource(ResourceNames[rk])
  else
    result := InitFromFile(GetFileName(rk));
end;

function TResourceManager.LoadFile(rk: TResourceKey): string;
begin
  result := InitFromFile(GetFileName(rk));
end;

function TResourceManager.GetTextFileContent(path: string): string;
var
  SL: TStringList;
  fn: string;
begin
  result := '';
  fn := dn + path;
  if FileExists(fn) then
  begin
    SL := TStringList.Create;
    SL.LoadFromFile(fn);
    result := SL.Text;
    SL.Free;
  end;
end;

function TResourceManager.InitFromFile(fn: string): string;
var
  SL: TStringList;
begin
  result := '';
  if FileExists(fn) then
  begin
    SL := TStringList.Create;
    SL.LoadFromFile(fn);
    result := SL.Text;
    SL.Free;
  end;
end;

function TResourceManager.InitFromResource(rn: string): string;
var
  rs: TResourceStream;
  SL: TStringList;
begin
  result := '';
  try
    rs := TResourceStream.Create(HInstance, rn, 'TEXT');
    SL := TStringList.Create;
    try
      SL.LoadFromStream(rs);
      result := SL.Text;
    finally
      rs.Free;
      SL.Free;
    end;
  except
    on e: Exception do
    begin
      OutputDebugString(PChar(e.Message));
      UseResources := False;
    end;
  end;
end;

end.
