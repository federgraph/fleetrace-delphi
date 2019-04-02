unit RiggVar.RM.RaceBar;
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
  RiggVar.Util.Classes,
  RiggVar.EM.Intf;

type
  TClearButtons = (
    ClearEvent,
    ClearRace,
    ClearTimepoint,
    ResetRace,
    GoBackToRace,
    NextR,
    NextE,
    UpdateCourse
  );

  TButtonBar = class(IEventMenu)
  private
    FComboCaption: string;
    SL: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitDefault;
    procedure InitRaces;
    procedure InitTimepoints;
    procedure InitClearButtons;

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
  RiggVar.BO.Def;

{ TRaceBar }

constructor TButtonBar.Create;
begin
  SL := TStringList.Create;
  InitDefault;
end;

destructor TButtonBar.Destroy;
begin
  SL.Free;
  inherited;
end;

function TButtonBar.ComboCaption: string;
begin
  { interface method }
  result := FComboCaption;
end;

function TButtonBar.Count: Integer;
begin
  { interface method }
  result := SL.Count;
end;

function TButtonBar.GetCaption(i: Integer): string;
begin
  { interface method }
  result := '-';
  if (i > 0) and (i <= SL.Count) then
    result := SL[i-1];
end;

function TButtonBar.GetDataUrl(i: Integer): string;
begin
  { interface method }
  result := '';
end;

function TButtonBar.GetImageUrl(i: Integer): string;
begin
  { interface method }
  result := '';
end;

function TButtonBar.IsMock: Boolean;
begin
  { interface method }
  result := False;
end;

procedure TButtonBar.InitDefault;
var
  i: Integer;
begin
  FComboCaption := 'ButtonBar';
  SL.Clear;
  for i := 0 to BO.BOParams.RaceCount-1 do
    SL.Add(IntToStr(i+1));
end;

procedure TButtonBar.InitTimepoints;
var
  i: Integer;
begin
  FComboCaption := 'ITBar';
  SL.Clear;
  for i := 0 to BO.BOParams.ITCount do
    SL.Add('IT' + IntToStr(i));
end;

procedure TButtonBar.InitRaces;
var
  i: Integer;
begin
  FComboCaption := 'RaceBar';
  SL.Clear;
  for i := 0 to BO.BOParams.RaceCount-1 do
    SL.Add('R' + IntToStr(i+1));
end;

procedure TButtonBar.InitClearButtons;
begin
  FComboCaption := 'ClearButtonBar';
  SL.Clear;
  SL.Add('Clear Event');
  SL.Add('Clear Race');
  SL.Add('Clear IT');
  SL.Add('Reset Race');
  SL.Add('Backto R');
  SL.Add('Next R');
  SL.Add('Next E');
  SL.Add('Refresh');
end;

end.
