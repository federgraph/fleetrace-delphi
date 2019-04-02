unit RiggVar.Web3.EventMenu;

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
  System.Classes;

type
  TEventMenuXmlBase = class
  protected
    function GetText: string; virtual; abstract;
  public
    Root: string;
    DataUrl: string;
    ImgUrl: string;
    property Text: string read GetText;
  end;

  TWorkspaceEventMenu = class(TEventMenuXmlBase)
  private
    SL: TStringList;
    TL: TStringList;
    procedure AddComboEntry(TL: TStrings;
      ComboCaption: string; StartIndex, EntryCount: Integer);
  protected
    function GetText: string; override;
    function GetXml(TL: TStrings): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDocrootEventMenu = class(TEventMenuXmlBase)
  protected
    function GetText: string; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TEventMenuXml }

constructor TWorkspaceEventMenu.Create;
begin
  DataUrl := 'http://localhost/EventMenu/Data/';
  ImgUrl := 'http://localhost/EventMenu/Img/';
  SL := TStringList.Create;
  TL := TStringList.Create;
end;

destructor TWorkspaceEventMenu.Destroy;
begin
  SL.Free;
  TL.Free;
  inherited;
end;

function TWorkspaceEventMenu.GetXml(TL: TStrings): string;
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

  //insert 'Current' at Index 0
  fileIndex := 0;
  AddComboEntry(TL, 'Current', fileIndex, 1);

  //then page over the rest
  fileIndex := 1;
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

procedure TWorkspaceEventMenu.AddComboEntry(TL: TStrings;
ComboCaption: string; StartIndex: Integer; EntryCount: Integer);
var
  i: Integer;
  j: Integer;
  s: string;
  fs: string;
begin
  fs := '<Btn Data="%d" Img="" Text="%s" />';

  SL.Add(Format('<ComboEntry Caption="%s">', [ComboCaption]));
  SL.Add(Format('<DataFolder Url="%s" >', [DataUrl]));
  SL.Add('<ImgFolder Url="">'); //Url attribute left empty
  if StartIndex = 0 then
  begin
    //Insert 'Current' at Index 0
    SL.Add(Format(fs, [0, 'Current']));
  end
  else
  begin
    //j is 1-based because of inserted 'Current'
    //i is 0-based index into TL
    for j := StartIndex to StartIndex + EntryCount-1 do
    begin
      i := j-1;
      if (i < 0) or (i > TL.Count-1) then
        break;
      s := TL[i];
      SL.Add(Format(fs, [j, s]));
    end;
  end;
  SL.Add('</ImgFolder>');
  SL.Add('</DataFolder>');
  SL.Add('</ComboEntry>');
end;

function TWorkspaceEventMenu.GetText: string;
begin
  Main.DocManager.FillEventNameList(TL);
  result := GetXml(TL);
end;

{ TDocrootEventMenu }

function TDocrootEventMenu.GetText: string;
var
  SL: TStringList;
  fn: string;
begin
  result := '<EventMenu />';
  fn := 'docroot/Results/EventMenu.xml';
  if FileExists(fn) then
  begin
    SL := TStringList.Create;
    try
      try
        SL.LoadFromFile(fn, TEncoding.UTF8);
        if SL.Count > 2 then
        begin
          SL[0] := '<?xml version="1.0" encoding="utf-8" ?>';
          SL[1] := Format('<EventMenu Root="%s">', [Root]);
        end;
        result := SL.Text;
      finally
        SL.Free;
      end;
    except
      on e: Exception do
        result := e.Message;
    end;
  end;
end;

end.
