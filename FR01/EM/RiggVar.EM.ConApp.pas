unit RiggVar.EM.ConApp;

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
  RiggVar.EM.Connection;

type
  TEventMenuGenerator = class
  private
    SL: TStringList;
    TL: TStringList;
    function GetText: string;
    procedure AddComboEntry(TL: TStrings;
      ComboCaption: string; StartIndex, EntryCount: Integer);
  protected
    function GetXml(TL: TStrings): string;
  public
    constructor Create;
    destructor Destroy; override;
    property Text: string read GetText;
  end;

  TAppCon = class(TEventMenuConnection)
  private
    EventMenuGenerator: TEventMenuGenerator;
    function GetEventData: string;
    function GetEventMenu: string;
  public
    function Get: string; override;
    procedure Post(const s: string); override;
  end;

implementation

uses
  RiggVar.App.Main;

const
  DataUrl: string = 'app://DocManager/';

{ TEventMenuApplication }

function TAppCon.Get: string;
begin
  if Pos('EventMenu', Url) > 0 then
    result := GetEventMenu
  else
    result := GetEventData;
end;

procedure TAppCon.Post(const s: string);
begin
  //not implemented
end;

function TAppCon.GetEventMenu: string;
begin
  EventMenuGenerator := TEventMenuGenerator.Create;
  try
    result := EventMenuGenerator.GetText;
  finally
    EventMenuGenerator.Free;
  end;
end;

function TAppCon.GetEventData: string;
var
  s: string;
  i: Integer;
begin
  result := '';
  if Length(Url) > Length(DataUrl) then
  begin
    i := Pos(DataUrl, Url);
    if (i > 0) then
    begin
      s := Copy(Url, Length(DataUrl) + 1, MaxInt);
      result := Main.DocManager.DocDownloadByName(s);
    end;
  end;
end;


{ TEventMenuGenerator }

constructor TEventMenuGenerator.Create;
begin
  SL := TStringList.Create;
  TL := TStringList.Create;
end;

destructor TEventMenuGenerator.Destroy;
begin
  SL.Free;
  TL.Free;
  inherited;
end;

function TEventMenuGenerator.GetXml(TL: TStrings): string;
var
  fileIndex: Integer;
  pageIndex: Integer;
  comboCaption: string;
  pageCount: Integer;
  entryLimit: Integer;
begin
  pageCount := 5;
  entryLimit := 100;

  SL.Clear;

  SL.Add('<?xml version="1.0" ?>');
  SL.Add('<EventMenu>');

  fileIndex := 0;
  pageIndex := 1;
  while (fileIndex < TL.Count-1) and (fileIndex < entryLimit) do
  begin
    comboCaption := 'Page ' + IntToStr(pageIndex);
    AddComboEntry(TL, comboCaption, fileIndex, pageCount);
    Inc(fileIndex, pageCount);
    Inc(pageIndex);
  end;

  SL.Add('</EventMenu>');
  result := SL.Text;
  SL.Clear;
end;

procedure TEventMenuGenerator.AddComboEntry(TL: TStrings;
ComboCaption: string; StartIndex: Integer; EntryCount: Integer);
var
  i: Integer;
  s: string;
  fs: string;
begin
  fs := '<Btn Data="%s" Text="%s" />';
  SL.Add(Format('<ComboEntry Caption="%s">', [ComboCaption]));
  SL.Add(Format('<DataFolder Url="%s" >', [DataUrl]));
  SL.Add('<ImgFolder>');
  for i := StartIndex to StartIndex + EntryCount-1 do
  begin
    if (i < 0) or (i > TL.Count-1) then
      break;
    s := TL[i];
    SL.Add(Format(fs, [s, s]));
  end;
  SL.Add('</ImgFolder>');
  SL.Add('</DataFolder>');
  SL.Add('</ComboEntry>');
end;

function TEventMenuGenerator.GetText: string;
begin
  Main.DocManager.FillEventNameList(TL);
  result := GetXml(TL);
end;

end.
