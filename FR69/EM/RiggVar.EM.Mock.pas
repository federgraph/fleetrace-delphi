unit RiggVar.EM.Mock;

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
  RiggVar.EM.Intf;

type
  TEventMenuMock = class(IEventMenu)
  public
    function ComboCaption: string; override;
    function Count: Integer; override;
    function GetCaption(i: Integer): string; override;
    function GetImageUrl(i: Integer): string; override;
    function GetDataUrl(i: Integer): string; override;
    function IsMock: Boolean; override;
  end;

implementation

const
  ImageRoot1: string = 'http://gsmac/CubeImages/Images02/';
  ImageRoot2: string = 'http://gsmac/CubeImages/Images03/';
  ResultRoot: string = 'http://gsmac/CubeImages/Results05/';

  { TEventMenu }

function TEventMenuMock.ComboCaption: string;
begin
  result := 'Test Data';
end;

function TEventMenuMock.Count: Integer;
begin
  result := 11;
end;

function TEventMenuMock.GetCaption(i: Integer): string;
begin
  case i of
    -1:
      result := 'no selection';
    1:
      result := '49er';
    2:
      result := '470 W';
    3:
      result := '470 M';
    4:
      result := 'Laser Radial';
    5:
      result := 'Laser';
    6:
      result := 'Finn';
    7:
      result := 'RSX W';
    8:
      result := 'RSX M';
    9:
      result := 'Star';
    10:
      result := '420';
    11:
      result := 'Opti';
  else
    result := 'B' + IntToStr(i);
  end;
end;

function TEventMenuMock.GetDataUrl(i: Integer): string;
begin
  if (i > 0) and (i <= Count) then
    result := ResultRoot + Format('Event-%.2d.xml', [i])
  else
    result := '';
end;

function TEventMenuMock.GetImageUrl(i: Integer): string;
begin
  case i of
    1:
      result := ImageRoot1 + 'Seite-1.png';
    2:
      result := ImageRoot1 + 'Seite-2.png';
    3:
      result := ImageRoot1 + 'Seite-3.png';
    4:
      result := ImageRoot1 + 'Seite-4.png';
    5:
      result := ImageRoot1 + 'Seite-5.png';
    6:
      result := ImageRoot1 + 'Seite-6.png';
    7:
      result := ImageRoot2 + 'Seite-1.png';
    8:
      result := ImageRoot2 + 'Seite-2.png';
    9:
      result := ImageRoot2 + 'Seite-3.png';
    10:
      result := ImageRoot2 + 'Seite-4.png';
    11:
      result := ImageRoot2 + 'Seite-5.png';
  else
    result := '';
  end;
end;

function TEventMenuMock.IsMock: Boolean;
begin
  result := true;
end;

end.
