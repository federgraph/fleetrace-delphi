unit RiggVar.EM.Parser;

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
  System.Math;

type
  TParserStop = (
    yStart,
    yRoot,
    yComboEntry,
    yBtn,
    yStop
  );

  TEventMenuParser = class
  public
    Root: string;
    Caption: string;
    DataFolder: string;
    ImgFolder: string;

    Data: string;
    Img: string;
    Text: string;

    HasRoot: Boolean;
    HasCaption: Boolean;
    HasDataFolder: Boolean;
    HasImgFolder: Boolean;

    HasData: Boolean;
    HasImg: Boolean;
    HasText: Boolean;

    Index: Integer;
    Status: TParserStop;

    procedure Clear;
    function IsXml(SL: TStrings): Boolean;
    procedure ReadXml(SL: TStrings); virtual; abstract;
    procedure ParseTxt(SL: TStrings);
    procedure Yield(Reason: TParserStop);
  end;

implementation

procedure TEventMenuParser.Yield(Reason: TParserStop);
begin
  Status := Reason;
end;

procedure TEventMenuParser.ParseTxt(SL: TStrings);
var
  i: Integer;
  s: string;
  k, v: string;
begin
  for i := Index to SL.Count - 1 do
  begin
    Index := i+1;
    s := SL[i];
    if s = '' then
      Continue;
    k := SL.Names[i];
    if k = '' then
      Continue;
    v := SL.ValueFromIndex[i];
//    if v = '' then
//      Continue; //e.g Root attribute is optional

    if k = 'Root' then
    begin
      Root := v;
      HasRoot := true;
      Yield(yRoot);
      Exit;
    end
    else if k = 'Caption' then
    begin
      Caption := v;
      HasCaption := true;
      Yield(yComboEntry);
      Exit;
    end
    else if k = 'DataFolder' then
    begin
      DataFolder := v;
      HasDataFolder := true;
    end
    else if k = 'ImgFolder' then
    begin
      ImgFolder := v;
      HasImgFolder := true;
    end;

    if {HasRoot and} HasCaption and HasDataFolder and HasImgFolder then
    begin
      if k = 'Data' then
      begin
        Data := v;
        HasData := true;
      end
      else if k = 'Img' then
      begin
        Img := v;
        HasImg := true;
      end
      else if k = 'Text' then
      begin
        Text := v;
        HasText := true;
        Yield(yBtn);
        Exit;
      end;
    end;
  end;
  Status := yStop;
end;

function TEventMenuParser.IsXml(SL: TStrings): Boolean;
var
  i: Integer;
  s: string;
  c: Integer;
begin
  result := false;
  c := Min(SL.Count, 10); //max 10 lines
  for i := 0 to c-1 do
  begin
    s := Trim(SL[i]);
    if Pos('<?', s) > 0 then
      result := true;
    if Pos('<', s) = 1 then
      result := true;
    if Pos('/>', s) > 1 then
      result := true;
    if result = true then
      break;
  end;
end;

procedure TEventMenuParser.Clear;
begin
  HasRoot := false;
  HasCaption := false;
  HasDataFolder := false;
  HasImgFolder := false;

  HasData := false;
  HasImg := false;
  HasText := false;
end;

end.
