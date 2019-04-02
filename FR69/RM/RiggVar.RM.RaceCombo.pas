unit RiggVar.RM.RaceCombo;

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
  RiggVar.EM.Intf;

type
  TRaceCombo = class(IEventMenu)
  private
    FCourse: string;
    FMenuIndex: Integer;
    SL: TStringList;
    CL: TStringList;
    procedure SetMenuIndex(const Value: Integer);
    function GetCustomCourse: string;
    procedure SetCustomCourse(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function ComboCaption: string; override;
    function Count: Integer; override;
    function GetCaption(i: Integer): string; override;
    function GetImageUrl(i: Integer): string; override;
    function GetDataUrl(i: Integer): string; override;
    function IsMock: Boolean; override;

    function GetCourse(i: Integer): string;
    property CustomCourse: string read GetCustomCourse write SetCustomCourse;
    property MenuIndex: Integer read FMenuIndex write SetMenuIndex;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.EM.Mock,
  RiggVar.EM.Impl;

const
  C0 = '1,2,Z';
  C1 = '1,2,3,Z';
  C2 = '1,2,3,1,3,Z';
  C3 = '1,2,3,1,3,1,2,3,Z';
  C4 = '1,Z';
  C5 = '1,2,3,2,3,2,3,Z';
  C6 = '1,2,3,2,3,2,3,2,3,Z';

{ TRaceCombo }

constructor TRaceCombo.Create;
begin
  CL := TStringList.Create;
  SL := TStringList.Create;

  CL.Add(C0);
  CL.Add(C1);
  CL.Add(C2);
  CL.Add(C3);
  CL.Add(C4);
  CL.Add(C5);
  CL.Add(C6);

  FCourse := C0;
  SL.CommaText := FCourse;
end;

destructor TRaceCombo.Destroy;
begin
  CL.Free;
  SL.Free;
  inherited;
end;

function TRaceCombo.ComboCaption: string;
begin
  { interface method }
  result := 'CourseCombo';
end;

function TRaceCombo.Count: Integer;
begin
  { interface method }
  result := SL.Count;
end;

function TRaceCombo.GetCaption(i: Integer): string;
begin
  { interface method }
  result := '-';
  if i <= SL.Count then
    result := SL[i-1];
end;

function TRaceCombo.GetDataUrl(i: Integer): string;
begin
  { interface method }
  result := '';
end;

function TRaceCombo.GetImageUrl(i: Integer): string;
begin
  { interface method }
  result := '';
end;

function TRaceCombo.IsMock: Boolean;
begin
  { interface method }
  result := False;
end;

procedure TRaceCombo.SetMenuIndex(const Value: Integer);
begin
  FMenuIndex := Value;
  if Value < CL.Count then
    FCourse := CL[Value]
  else
    FCourse := 'M1,M2,Finish';
  SL.CommaText := FCourse;
end;

function TRaceCombo.GetCustomCourse: string;
begin
  result := CL[0];
end;

procedure TRaceCombo.SetCustomCourse(const Value: string);
begin
  CL[0] := Value;
  MenuIndex := 0;
end;

function TRaceCombo.GetCourse(i: Integer): string;
begin
  if (i >= 0) and (i < CL.Count) then
    result := CL[i]
  else
    result := C0;
end;

end.
