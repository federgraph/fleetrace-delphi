unit RiggVar.DAL.WorkspaceInfo;

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

{
  provides values for WorkspaceType and WorkspaceID
  used in FolderInfo and WorkspaceManager
}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.EM.TransformerMSXML,
  RiggVar.Util.Classes;

type
  TWorkspaceInfo = class
  private
    FWorkspaceType: Integer;
    FWorkspaceID: Integer;
    FAutoSaveIni: Boolean;
    FWorkspaceUrl: string;
    FWorkspaceRoot: string;
    FWorkspaceName: string;
    procedure SetWorkspaceType(const Value: Integer);
    procedure SetWorkspaceID(const Value: Integer);
    procedure ReadParams(SL: TStrings);
    procedure SetAutoSaveIni(const Value: Boolean);
    procedure SetWorkspaceUrl(const Value: string);
    function GetWorkspaceTypeName: string;
    procedure SetWorkspaceRoot(const Value: string);
    procedure SetWorkspaceName(const Value: string);
  protected
    function ReadCmdLine: Boolean;
    procedure ReadTxtFile;
    procedure ReadXmlFile;
    procedure Check;
  public
    IsOK_WorkspaceType: Boolean;
    IsOK_WorkspaceID: Boolean;
    HasError_WorkspaceRoot: Boolean;
    constructor Create;
    procedure Assign(wi: TWorkspaceInfo);
    procedure Load; virtual;
    procedure Save; virtual;
    procedure Clear; virtual;
    function ToString: string; override;
    procedure WorkspaceReport(Memo: TStrings);
    property WorkspaceType: Integer read FWorkspaceType write SetWorkspaceType;
    property WorkspaceID: Integer read FWorkspaceID write SetWorkspaceID;
    property AutoSaveIni: Boolean read FAutoSaveIni write SetAutoSaveIni;
    property WorkspaceUrl: string read FWorkspaceUrl write SetWorkspaceUrl;
    property WorkspaceRoot: string read FWorkspaceRoot write SetWorkspaceRoot;
    property WorkspaceTypeName: string read GetWorkspaceTypeName;
    property WorkspaceName: string read FWorkspaceName write SetWorkspaceName;
  end;

implementation

{ TWorkspaceInfo }

uses
  RiggVar.App.Main;

constructor TWorkspaceInfo.Create;
begin
  Clear;
end;

procedure TWorkspaceInfo.Clear;
begin
  FWorkspaceType := 0;
  FWorkspaceID := 0;
  FAutoSaveIni := True;
  FWorkspaceUrl := 'http://localhost/WebApplication/';
end;

procedure TWorkspaceInfo.Assign(wi: TWorkspaceInfo);
begin
  FWorkspaceType := wi.WorkspaceType;
  FWorkspaceID := wi.FWorkspaceID;
  FAutoSaveIni := wi.AutoSaveIni;
  FWorkspaceUrl := wi.WorkspaceUrl;
  FWorkspaceRoot := wi.WorkspaceRoot;
end;

procedure TWorkspaceInfo.SetAutoSaveIni(const Value: Boolean);
begin
  FAutoSaveIni := Value;
end;

procedure TWorkspaceInfo.SetWorkspaceID(const Value: Integer);
begin
  FWorkspaceID := Value;
end;

procedure TWorkspaceInfo.SetWorkspaceName(const Value: string);
begin
  FWorkspaceName := Value;
end;

procedure TWorkspaceInfo.SetWorkspaceRoot(const Value: string);
begin
  FWorkspaceRoot := Value;
end;

procedure TWorkspaceInfo.SetWorkspaceType(const Value: Integer);
begin
  FWorkspaceType := Value;
end;

procedure TWorkspaceInfo.SetWorkspaceUrl(const Value: string);
begin
  FWorkspaceUrl := Value;
end;

procedure TWorkspaceInfo.Load;
begin
  if not ReadCmdLine then
  begin
    if FileExists('WorkspaceInfo.Config.txt') then
      ReadTxtFile
    else if FileExists('WorkspaceInfo.Config.xml') then
      ReadXmlFile
    else if FileExists('SharedWS.txt') then
      FWorkspaceType := 1
    else if FileExists('PrivateWS.txt') then
      FWorkspaceType := 2
    else if FileExists('LocalDB.config') then
      FWorkspaceType := 3
    else if FileExists('RemoteDB.config') then
      FWorkspaceType := 4
  end;

  Check;
end;

procedure TWorkspaceInfo.Save;
begin
  //not implemented
end;

procedure TWorkspaceInfo.ReadXmlFile;
var
  SL: TStringList;
  Transformer: TXslTransformer;
  XSL: TStrings;
  TXT: TStrings;
  fn: string;
begin
  SL := TStringList.Create;
  XSL := TStringList.Create;
  TXT := TStringList.Create;
  Transformer := TXslTransformer.Create;
  try

    //Init XSL
    with XSL do
    begin
      Add('<?xml version="1.0" encoding="UTF-8"?>');
      Add('<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">');
      Add('<xsl:output method="text" encoding="utf-8"/>');
      Add('<xsl:template match="/">');
      Add('<xsl:text>WorkspaceType=</xsl:text>');
      Add('<xsl:value-of select="WorkspaceInfo/@WorkspaceType" />');
      Add('<xsl:text>&#xA;</xsl:text>');
      Add('<xsl:text>WorkspaceID=</xsl:text>');
      Add('<xsl:value-of select="WorkspaceInfo/@WorkspaceID" />');
      Add('<xsl:text>&#xA;</xsl:text>');
      Add('<xsl:text>AutoSaveIni=</xsl:text>');
      Add('<xsl:value-of select="WorkspaceInfo/@AutoSaveIni" />');
      Add('<xsl:text>&#xA;</xsl:text>');
      Add('<xsl:text>WorksapceUrl=</xsl:text>');
      Add('<xsl:value-of select="WorkspaceInfo/@WorkspaceUrl" />');
      Add('<xsl:text>&#xA;</xsl:text>');
      Add('<xsl:text>WorksapceRoot=</xsl:text>');
      Add('<xsl:value-of select="WorkspaceInfo/@WorkspaceRoot" />');
      Add('<xsl:text>&#xA;</xsl:text>');
      Add('</xsl:template>');
      Add('</xsl:stylesheet>');
    end;

    fn := 'WorkspaceInfo.Config.xml';
    if FileExists(fn) then
    begin
      SL.LoadFromFile(fn);
      Transformer.TransformSL(SL, XSL, TXT);
      if Transformer.OK then
      begin
        ReadParams(TXT);
      end;
    end;

  finally
    Transformer.Free;
    SL.Free;
    XSL.Free;
    TXT.Free;
  end;
end;

procedure TWorkspaceInfo.ReadTxtFile;
var
  SL: TStringList;
  fn: string;
begin
  SL := TStringList.Create;
  fn := 'WorkspaceInfo.Config.txt';
  SL.LoadFromFile(fn);
  try
    ReadParams(SL);
  finally
    SL.Free;
  end;
end;

function TWorkspaceInfo.ReadCmdLine: Boolean;
var
  i: Integer;
  SL: TStringList;
  s: string;
begin
  if ParamCount > 0 then
  begin
    //--Example CmdLine: FR62.exe WorkspaceType=1 WorkspaceID=1
    SL := TStringList.Create;
    try
      for i := 1 to ParamCount do
      begin
        s := ParamStr(i);
        SL.Add(s);
      end;
      ReadParams(SL);
    finally
      SL.Free;
    end;
    result := IsOK_WorkspaceType or IsOK_WorkspaceID;
  end
  else
    result := false;
end;

procedure TWorkspaceInfo.ReadParams(SL: TStrings);
var
  s: string;
  i: Integer;
begin
  s := SL.Values['WorkspaceType'];
  i := StrToIntDef(s, -1);
  if i > -1 then
  begin
    FWorkspaceType := i;
    IsOK_WorkspaceType := true;
  end;

  s := SL.Values['WorkspaceID'];
  i := StrToIntDef(s, -1);
  if i > -1 then
  begin
    FWorkspaceID := i;
    IsOK_WorkspaceID := true;
  end;

  //look if AutoSaveIni is disabled
  s := Trim(SL.Values['AutoSaveIni']);
  if TUtils.StartsWith(LowerCase(s), 'f')
    or TUtils.StartsWith(s, '0') then
  begin
    FAutoSaveIni := false;
  end;

  s := Trim(SL.Values['WorkspaceUrl']);
  if TUtils.StartsWith(LowerCase(s), 'http://') then
  begin
    FWorkspaceUrl := s;
  end;

  s := Trim(SL.Values['WorkspaceRoot']);
  FWorkspaceRoot := s;
end;

procedure TWorkspaceInfo.WorkspaceReport(Memo: TStrings);
begin
  Memo.Add(Format('WorkspaceInfo.WorkspaceType=%d [%s]', [WorkspaceType, WorkspaceTypeName]));
  Memo.Add('WorkspaceInfo.WorkspaceID=' + IntToStr(WorkspaceID));
  Memo.Add('WorkspaceInfo.AutoSaveIni=' + BoolStr[AutoSaveIni]);
  Memo.Add('WorkspaceInfo.WorkspaceUrl=' + WorkspaceUrl);
  Memo.Add('WorkspaceInfo.WorkspaceRoot=' + WorkspaceRoot);
end;

function TWorkspaceInfo.GetWorkspaceTypeName: string;
begin
  result := Main.StoreAdapter.GetWorkspaceTypeName(self);
end;

function TWorkspaceInfo.ToString(): string;
var
  TL: TStringList;
begin
  result := '';
  TL := TStringList.Create;
  try
    TL.Add('WorkspaceTypeName: ' + WorkspaceTypeName);
    TL.Add('WorkspaceType: ' + IntToStr(WorkspaceType));
    TL.Add('WorkspaceID: ' + IntToStr(WorkspaceID));
    TL.Add('AutoSaveIni: ' + BoolStr[AutoSaveIni]);
    TL.Add('WorkspaceUrl: ' + WorkspaceUrl);
    TL.Add('WorkspaceRoot: ' + WorkspaceRoot);
    result := TL.Text;
  finally
    TL.Free;
  end;
end;

procedure TWorkspaceInfo.Check;
begin
  Main.StoreAdapter.CheckWorkspaceInfo(self);
end;

end.
