unit RiggVar.EM.Combo;

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
  System.Classes,
  RiggVar.Util.Classes,
  RiggVar.EM.Intf,
  RiggVar.EM.Collection,
  RiggVar.EM.Parser,
  RiggVar.EM.ParserSimple,
  RiggVar.EM.ParserXslt;

type
  TEventCombo = class(IEventMenu)
  private
    FHasMock: Boolean;
    FMenuCollection: TEventMenuCollection;
    FMenuChanged: TNotifyEvent;
    index: Integer;
    procedure SetMenuIndex(const Value: Integer);
    function GetCurrentMenu: IEventMenu;
    function DataRoot(p: TEventMenuParser): string;
    function ImgRoot(p: TEventMenuParser): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property MenuCollection: TEventMenuCollection read FMenuCollection;
    property MenuChanged: TNotifyEvent read FMenuChanged write FMenuChanged;
    property MenuIndex: Integer read index write SetMenuIndex;
    property CurrentMenu: IEventMenu read GetCurrentMenu;

    procedure Load(data: string);

    function ComboCaption: string; override;
    function Count: Integer; override;
    function GetCaption(i: Integer): string; override;
    function GetImageUrl(i: Integer): string; override;
    function GetDataUrl(i: Integer): string; override;
    function IsMock: Boolean; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.EM.Mock,
  RiggVar.EM.Impl;

{ TEventCombo }

constructor TEventCombo.Create;
begin
  FMenuCollection := TEventMenuCollection.Create;
end;

destructor TEventCombo.Destroy;
begin
  FMenuCollection.Free;
  inherited;
end;

function TEventCombo.GetCurrentMenu: IEventMenu;
begin
  if MenuCollection.Count = 0 then
  begin
    MenuCollection.Add(TEventMenuMock.Create);
    FHasMock := True;
    index := 0;
  end;
  result := MenuCollection.NodeAt(index);
end;

procedure TEventCombo.SetMenuIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < MenuCollection.Count) then
    index := Value;
end;

procedure TEventCombo.Clear;
begin
  FMenuCollection.Clear;
  index := -1;
end;

function TEventCombo.ComboCaption: string;
begin
  result := CurrentMenu.ComboCaption;
end;

function TEventCombo.Count: Integer;
begin
  result := CurrentMenu.Count;
end;

function TEventCombo.GetCaption(i: Integer): string;
begin
  result := CurrentMenu.GetCaption(i);
end;

function TEventCombo.GetDataUrl(i: Integer): string;
begin
  result := CurrentMenu.GetDataUrl(i);
end;

function TEventCombo.GetImageUrl(i: Integer): string;
begin
  result := CurrentMenu.GetImageUrl(i);
end;

procedure TEventCombo.Load(data: string);
var
  SL: TStringList;
  p: TEventMenuParser;
  mc: TEventMenu02;
  cr: TBtnInfo;
begin
  FHasMock := False;
  MenuCollection.Clear;
  SL := TStringList.Create;
  SL.Text := data;

  if Main.IniImage.WantSimpleParser then
    p:= TSimpleEventMenuParser.Create
  else
    p := TEventMenuParserXSLT.Create;

  try
    if p.IsXml(SL) then
    begin
      p.ReadXml(SL);
    end;
    mc := nil;
    while p.Status <> yStop do
    begin
      p.ParseTxt(SL);
      case p.Status of

        yComboEntry:
        begin
          if p.HasCaption then
          begin
            mc := TEventMenu02.Create;
            MenuCollection.Add(mc);
            mc.Caption := p.Caption;
          end;
        end;

        yBtn:
        begin
          if (mc <> nil) and  p.HasData {and p.HasImg} and p.HasText then
          begin
            cr := TBtnInfo.Create;
            mc.Info.Add(cr);
            cr.Data := DataRoot(p) + p.Data;
            if p.HasImg then
            begin
              cr.Img := ImgRoot(p) + p.Img;
            end;
            cr.Text := p.Text;
            p.HasData := false;
            p.HasImg := false;
            p.HasText := false;
          end;
        end;
      end;
    end;
  finally
    SL.Free;
    p.Free;
  end;
  //Assert(mc <> nil);
  //Assert(MenuCollection.Count > 0);
  index := 0;
end;

function TEventCombo.DataRoot(p: TEventMenuParser): string;
begin
  if (p.Root = '') or TUtils.StartsWith(p.DataFolder, 'http') then
    result := p.DataFolder
  else
    result := p.Root + p.DataFolder;
end;

function TEventCombo.ImgRoot(p: TEventMenuParser): string;
begin
  if (p.Root = '') or TUtils.StartsWith(p.ImgFolder, 'http') then
    result := p.ImgFolder
  else
    result := p.Root + p.ImgFolder;
end;

function TEventCombo.IsMock: Boolean;
begin
  result := FHasMock;
end;

end.
