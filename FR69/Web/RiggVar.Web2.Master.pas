unit RiggVar.Web2.Master;

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
  Web.HTTPProd;

type
  TMasterPage = class
  private
    PL: TStringList;
    NL: TStringList;
    FIsHome: Boolean;
    FHasClientBin: Integer;

    DoctypeString: string;
    StylesheetName: string;
    CopyrightString: string;

    function GetText: string;
    procedure InsertContent(StringList: TStringList);
    procedure GetPlainText;
    procedure ExpandTheme1;
    procedure ExpandTheme2;
    procedure InitTemplate;
    procedure ExpandTemplate;
    function GetIsEmbedded: Boolean;
    procedure OnReplaceTag(Sender: TObject; Tag: TTag; const TagString: string;
      TagParams: TStrings; var ReplaceText: string);
    function GetVirtualDir: string;
    function GetUseTemplate: Boolean;
    function GetIsSamePort: Boolean;
    procedure InitNL;
    function GetHasClientBin: Boolean;
    function InitHasClientBin: Boolean;
    function GetHasAngularClient: Boolean;
  public
    HasTemplate: Boolean;
    PP: TPageProducer;

    EmbeddedFlag: Boolean;
    HL: TStringList;
    ML: TStringList;
    SL: TStringList;
    class var
      IsHomeWeb: Boolean; //flag (constructor param)
      IsPlain: Boolean;

    constructor Create;
    destructor Destroy; override;
    property Text: string read GetText;
    property IsEmbedded: Boolean read GetIsEmbedded;
    property IsHome: Boolean read FIsHome;
    property VirtualDir: string read GetVirtualDir;
    property UseTemplate: Boolean read GetUseTemplate;
    property IsSamePort: Boolean read GetIsSamePort;
    property HasClientBin: Boolean read GetHasClientBin;
    property HasAngularClient: Boolean read GetHasAngularClient;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.Web2.Router;

{ TMasterPage }

constructor TMasterPage.Create;
begin
  IsPlain := False;

  DoctypeString := '<!DOCTYPE html>';
  CopyrightString := 'Copyright © 2010-2018 federgraph.de. All rights reserved.';

  FIsHome := IsHomeWeb;

  PL := TStringList.Create; //Page Content Html
  NL := TStringList.Create; //Navigation LinkItems Html
  HL := TStringList.Create; //Header ContentPlaceHolder
  ML := TStringList.Create; //MenuStrip ContentPlaceHolder
  SL := TStringList.Create; //Body ContentPlaceHolder

  InitTemplate;
end;

destructor TMasterPage.Destroy;
begin
  NL.Free;
  PP.Free;
  PL.Free;

  HL.Free;
  ML.Free;
  SL.Free;
  inherited;
end;

function TMasterPage.GetVirtualDir: string;
begin
  if Main.Params.UseProxyBase then
  begin
    if IsHome then
    result := Main.IniImage.HomeProxyBase
  else
      result := Main.IniImage.RemoteProxyBase;
  end
  else
    result := '/';
end;

procedure TMasterPage.InitTemplate;
var
  fn: string;
begin
  if IsHome then
    fn := 'HomeTemplate.htm'
  else
    fn := 'RemoteTemplate.htm';

  if Main.IniImage.WantTemplate and FileExists(fn) then
  begin
    HasTemplate := True;
    PP := TPageProducer.Create(nil);
    PP.HTMLFile := fn;
    PP.OnHTMLTag := OnReplaceTag;
  end;
end;

function TMasterPage.GetIsEmbedded: Boolean;
begin
  result := EmbeddedFlag or Main.GuiManager.HomeRouter.HomeBtnFlag;
  Main.GuiManager.HomeRouter.HomeBtnFlag := false;
end;

function TMasterPage.GetIsSamePort: Boolean;
begin
  result := Main.IniImage.WebServerRemotePort =
    Main.IniImage.WebServerHomePort;
end;

function TMasterPage.GetHasAngularClient: Boolean;
begin
  result := True;
end;

function TMasterPage.GetHasClientBin: Boolean;
begin
  if FHasClientBin = 0 then
    InitHasClientBin;
  result := FHasClientBin = 1;
end;

function TMasterPage.InitHasClientBin: Boolean;
var
  dn: string;
begin
  dn := Format('%sClientBin', [Main.GuiManager.HomeRouter.dn]);
  if DirectoryExists(dn) then
  begin
    FHasClientBin := 1;
    result := true;
  end
  else
  begin
    FHasClientBin := -1;
    result := false;
  end;
end;

function TMasterPage.GetText: string;
var
  vd: string;
begin
  PL.Clear;
  NL.Clear;

  vd := VirtualDir;
  InitNL;

  if IsPlain then
    GetPlainText
  else if UseTemplate then
     ExpandTemplate
  else
  begin

    case Main.IniImage.WebsiteTheme of
      1:
      begin
        StyleSheetName := vd + 'stylesheets/Site.css';
        ExpandTheme1;
      end;

      3:
      begin
        StyleSheetName := vd + 'styles/style-p.css';
        ExpandTheme2;
      end

      //2: default
      else
      begin
        if IsHome then
          StyleSheetName := vd + 'styles/style.css'
        else
          StyleSheetName := vd + 'styles/style.css';
        ExpandTheme2;
      end;
    end;

  end;
  result := PL.Text;
  PL.Clear;
end;

function TMasterPage.GetUseTemplate: Boolean;
begin
  result := Main.IniImage.WantTemplate and HasTemplate;
end;

procedure TMasterPage.InsertContent(StringList: TStringList);
var
  i: Integer;
begin
  for i := 0 to StringList.Count - 1 do
    PL.Add(StringList[i]);
end;

procedure TMasterPage.InitNL;
var
  fs: string;
begin
  NL.Clear;
  fs := '<li><a href="' + VirtualDir + '%s">%s</a></li>';
  if IsHome then
  begin
    NL.Add(Format(fs, ['Index', 'Index']));
    NL.Add(Format(fs, ['Home/Index', 'Home']));
    if IsSamePort then
    begin
      NL.Add(Format(fs, ['Remote/Index', 'Remote']));
      NL.Add(Format(fs, ['Widget/Index', 'Widget']));
      NL.Add(Format(fs, ['Intern/Index', 'Intern']));
      if Main.GuiManager.RemoteRouter.ShowBridgeTab then
        NL.Add(Format(fs, ['Bridge/Index', 'Bridge']));
    end;
    if HasClientBin and not IsEmbedded then
      NL.Add(Format(fs, ['Silverlight/Index', 'Silverlight']));
    if HasClientBin and not IsEmbedded then
      NL.Add(Format(fs, ['fr/index.html', 'FR03']));
    if HasAngularClient and not IsEmbedded then
      NL.Add(Format(fs, ['Angular/Index', 'Angular']));
  end
  else
  begin
    NL.Add(Format(fs, ['Index','Index']));
    if IsSamePort then
      NL.Add(Format(FS, ['Home/Index','Home']));
    NL.Add(Format(fs, ['Remote/Index','Remote']));
    NL.Add(Format(fs, ['Widget/Index','Widget']));
    NL.Add(Format(fs, ['Intern/Index','Intern']));
    if Main.GuiManager.RemoteRouter.ShowBridgeTab then
      NL.Add(Format(fs, ['Bridge/Index', 'Bridge']));
    if HasClientBin and IsSamePort and not IsEmbedded then
        NL.Add(Format(fs, ['Silverlight/Index', 'Silverlight']));
    if HasAngularClient and IsSamePort and not IsEmbedded then
        NL.Add(Format(fs, ['Angular/Index', 'Angular']));
  end;
end;

procedure TMasterPage.GetPlainText;
begin
  PL.Add(DoctypeString);
  PL.Add('<html><head>');
  InsertContent(HL);
  PL.Add('</head><body>');
  InsertContent(ML);
  InsertContent(SL);
  PL.Add('</body></html>');
end;

procedure TMasterPage.ExpandTheme1;
begin
  PL.Add('<html>');
  PL.Add('<head>');
  PL.Add('<link rel="stylesheet" href="' + StylesheetName + '" type="text/css" />');
  InsertContent(HL);
  PL.Add('</head>');
  PL.Add('<body>');
  PL.Add('<div class="page">');
  PL.Add('<div id="header">');
  PL.Add('<div id="menucontainer">');
  PL.Add('<ul id="menu">');
  InsertContent(NL);
  PL.Add('</ul>');
  PL.Add('</div>');
  PL.Add('</div>');
  PL.Add('<div id="main">');
  InsertContent(ML);
  InsertContent(SL);
  PL.Add('<div id="footer">' + CopyrightString + '</div>');
  PL.Add('</div>');
  PL.Add('</div>');
  PL.Add('</body>');
  PL.Add('</html>');
end;

procedure TMasterPage.ExpandTheme2;
begin
  PL.Add(DoctypeString);
  PL.Add('<html>');
  PL.Add('<head>');
  PL.Add('<meta content="text/html; charset=utf-8" http-equiv="Content-Type" />');
  PL.Add('<link rel="stylesheet" href="' + StylesheetName + '" type="text/css" />');
  InsertContent(HL);
  PL.Add('</head>');
  PL.Add('<body>');
  PL.Add('<div id="container">');
  PL.Add('<div id="header">');
  PL.Add('<p class="breadcrumb">');
  PL.Add('<a href="/">home</a> (lookup help at federgraph.de)');
  PL.Add('</p>');
  PL.Add('<p>FRED / Fleet Race Event Data Server 0.9</p>');
  PL.Add('</div>');
  PL.Add('<div id="page">');
  PL.Add('<div id="sidebar">');
  PL.Add('<ul>');
  InsertContent(NL);
  PL.Add('</ul>');
  PL.Add('</div>');
  PL.Add('<div id="content">');
  InsertContent(ML);
  InsertContent(SL);
  PL.Add('</div>');
  PL.Add('</div>');
  PL.Add('<div id="footer">');
  PL.Add('<p>' + CopyrightString + '</p>');
  PL.Add('</div>');
  PL.Add('</div>');
  PL.Add('</body>');
  PL.Add('</html>');
end;

procedure TMasterPage.ExpandTemplate;
begin
  PL.Text := PP.Content;
end;

procedure TMasterPage.OnReplaceTag(Sender : TObject;Tag: TTag;
    const TagString: string; TagParams: TStrings; var ReplaceText: string);
begin
  if CompareText(TagString, 'NL') = 0 then
    ReplaceText := NL.Text;
  if CompareText(TagString, 'HL') = 0 then
    ReplaceText := HL.Text;
  if CompareText(TagString, 'ML') = 0 then
    ReplaceText := ML.Text;
  if CompareText(TagString, 'SL') = 0 then
    ReplaceText := SL.Text;
end;

end.
