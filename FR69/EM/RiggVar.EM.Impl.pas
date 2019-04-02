unit RiggVar.EM.Impl;

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
  RiggVar.EM.Intf,
  RiggVar.Util.ItemCollection;

type
  TBtnInfo = class(TItem)
  public
    Data: string;
    Img: string;
    Text: string;
    function Key: string; override;
  end;

  TBtnList = TItemCollection<TBtnInfo>;

  TEventMenu02 = class(IEventMenu)
  public
    Caption: string;
    Info: TBtnList;

    constructor Create;
    destructor Destroy; override;

    function ComboCaption: string; override;
    function Count: Integer; override;
    function GetCaption(i: Integer): string; override;
    function GetImageUrl(i: Integer): string; override;
    function GetDataUrl(i: Integer): string; override;
    function IsMock: Boolean; override;
  end;

implementation

{ TEventMenu02 }

constructor TEventMenu02.Create;
begin
  Info := TBtnList.Create;
end;

destructor TEventMenu02.Destroy;
begin
  Info.Free;
  inherited;
end;

function TEventMenu02.ComboCaption: string;
begin
  result := Caption;
end;

function TEventMenu02.Count: Integer;
begin
  result := Info.Count;
end;

function TEventMenu02.GetCaption(i: Integer): string;
begin
  if i < 0 then
    result := 'no selection'
  else if (i > 0) and (i <= Count) then
      result := Info.NodeAt(i - 1).Text
  else
      result := 'B' + IntToStr(i);
end;

function TEventMenu02.GetDataUrl(i: Integer): string;
begin
  if (i > 0) and (i <= Count) then
      result := Info.NodeAt(i - 1).Data
  else
      result := '';
end;

function TEventMenu02.GetImageUrl(i: Integer): string;
begin
  if (i > 0) and (i <= Count) then
      result := Info.NodeAt(i - 1).Img
  else
      result := '';
end;

function TEventMenu02.IsMock: Boolean;
begin
  result := false;
end;

{ TBtnInfo }

function TBtnInfo.Key: string;
begin
  result := Data;
end;

end.
