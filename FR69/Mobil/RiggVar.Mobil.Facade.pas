unit RiggVar.Mobil.Facade;

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
  RiggVar.App.GuiInterface,
  RiggVar.Col.Event;

type
  TMobilFacade = class
  private
    FCurrentIndex: Integer;
    FOnUpdateUI: TNotifyEvent;
    procedure CheckBib;
    procedure CheckRace;
    procedure SetCurrentIndex(const Value: Integer);
    procedure SetOnUpdateUI(const Value: TNotifyEvent);
  protected
    CurrentRace: Integer;
    CurrentBib: Integer;
    procedure ClearDetails;
    function GetMemoString: string;

    procedure UpdateThrowouts;
    procedure UpdateRace;
    procedure UpdateBib;
    procedure UpdateDetails;

    procedure RaceChanged; virtual;
    procedure IndexChanged; virtual;
    procedure ThrowoutsChanged; virtual;

    procedure ChangeThrowouts(NewValue: Integer);
    procedure ChangeRace(NewValue: Integer);
    procedure ChangeIndex(NewValue: Integer);
    procedure UpdateUI; virtual;
  public
    MemoLines: TStrings;

    edCommandText: string;
    edRankText: string;
    edPointsText: string;

    EventLabelCaption: string;
    RaceLabelCaption: string;

    ThrowoutsBtnCaption: string;
    RaceBtnCaption: string;
    BibBtnCaption: string;

    constructor Create;
    destructor Destroy; override;

    procedure RaceDown;
    procedure RaceUp;
    procedure ThrowoutsDown;
    procedure ThrowoutsUp;
    procedure BibDown;
    procedure BibUp;

    procedure Submit(Input: string);

    procedure InitMobil;
    procedure UpdateCore;

    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    property OnUpdateUI: TNotifyEvent read FOnUpdateUI write SetOnUpdateUI;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TMobilFacade }

constructor TMobilFacade.Create;
begin
  MemoLines := TStringList.Create;
end;

destructor TMobilFacade.Destroy;
begin
  MemoLines.Free;
  inherited;
end;

procedure TMobilFacade.InitMobil;
begin
  CurrentRace := 1;
  if BO.EventNode.EventRowCollection.Count > 0 then
  begin
    CurrentBib := BO.EventNode.EventRowCollection.Items[0].Bib;
    UpdateUI;
  end;
end;

procedure TMobilFacade.CheckRace;
begin
  if CurrentRace > BO.BOParams.RaceCount then
    CurrentRace := 1;
end;

procedure TMobilFacade.CheckBib;
begin
  if BO.EventNode.FindBib(CurrentBib) = nil then
  begin
    CurrentBib := 0;
    if BO.EventNode.EventRowCollection.Count > 0 then
      CurrentBib := BO.EventNode.EventRowCollection.Items[0].Bib;
  end;
end;

procedure TMobilFacade.UpdateUI;
begin
  UpdateCore;
end;

function TMobilFacade.GetMemoString: string;
var
  cr: TEventRowCollectionItem;
  i: Integer;
  b: Integer;
  r: Integer;
  rc: Integer;
  s: string;
begin
  b := CurrentBib;
  r := CurrentRace;
  rc := BO.BOParams.RaceCount;
  cr := BO.EventNode.FindBib(b);
  s := '';
  for i := 1 to rc do
  begin
    if i > 1 then
    begin
      if (i - r = 0) or (i - r = 1) then
        s := s + ' - '
      else
        s := s + ' ; ';
    end;
    s := s + cr.Race[i].RaceValue;
  end;
  result := s;
end;

procedure TMobilFacade.UpdateCore;
begin
  CheckRace;
  CheckBib;
  UpdateThrowouts;
  UpdateRace;
  UpdateBib;
  UpdateDetails;
end;

procedure TMobilFacade.UpdateDetails;
var
  cr: TEventRowCollectionItem;
begin
  cr := BO.EventNode.FindBib(CurrentBib);
  if Assigned(cr) then
  begin
    edCommandText := cr.Race[CurrentRace].RaceValue;

    MemoLines.Clear;
    MemoLines.Add(GetMemoString);
    MemoLines.Add(cr.DN);

    edRankText := IntToStr(cr.GPosR);
    edPointsText := cr.GPoints;

    EventLabelCaption := Format('Event Result for Bib %d', [cr.Bib]);
    RaceLabelCaption := Format('Race Value for Race %d Bib %d', [CurrentRace, CurrentBib]);
  end
  else
  begin
    ClearDetails;
  end;
  if Assigned(OnUpdateUI) then
    OnUpdateUI(Self);
end;

procedure TMobilFacade.ClearDetails;
begin
  edCommandText := '';

  MemoLines.Clear;
  edRankText := '0';
  edPointsText := '0.00';
  EventLabelCaption := 'Event Results (Rank and Points)';
  RaceLabelCaption := 'Race Value for Bib';
end;

procedure TMobilFacade.UpdateThrowouts;
var
  c: Integer;
begin
  c := BO.EventProps.Throwouts;
  ThrowoutsBtnCaption := Format('T%d', [c]);
end;

procedure TMobilFacade.UpdateRace;
var
  c: Integer;
begin
  c := BO.BOParams.RaceCount;
  RaceBtnCaption := Format('Race %d/%d', [CurrentRace, c]);
end;

procedure TMobilFacade.UpdateBib;
var
  c: Integer;
begin
  c := BO.BOParams.StartlistCount;
  BibBtnCaption := Format('Bib %d/%d', [CurrentBib, c]);
end;

procedure TMobilFacade.ChangeThrowouts(NewValue: Integer);
begin
  Main.GuiManager.Throwouts := NewValue;
  UpdateUI;
end;

procedure TMobilFacade.ChangeRace(NewValue: Integer);
begin
  if NewValue < 1 then
    //do nothing
  else if NewValue > BO.BOParams.RaceCount then
    //do nothing
  else
  begin
    if CurrentRace <> NewValue then
    begin
      CurrentRace := NewValue;
      RaceChanged;
      UpdateUI;
    end;
  end;
end;

procedure TMobilFacade.ChangeIndex(NewValue: Integer);
begin
  if NewValue < 0 then
    //do nothing
  else if NewValue > BO.BOParams.StartlistCount - 1 then
    //do nothing
  else
  begin
    if CurrentIndex <> NewValue then
    begin
      CurrentIndex := NewValue;
      IndexChanged;
      UpdateUI;
    end;
  end;
end;

procedure TMobilFacade.BibDown;
begin
  ChangeIndex(FCurrentIndex - 1);
end;

procedure TMobilFacade.BibUp;
begin
  ChangeIndex(FCurrentIndex + 1);
end;

procedure TMobilFacade.ThrowoutsChanged;
begin
  //virtual
end;

procedure TMobilFacade.RaceChanged;
begin
  //virtual
end;

procedure TMobilFacade.IndexChanged;
begin
 //virtual
end;

procedure TMobilFacade.RaceDown;
begin
  ChangeRace(CurrentRace - 1);
end;

procedure TMobilFacade.RaceUp;
begin
  ChangeRace(CurrentRace + 1);
end;

procedure TMobilFacade.ThrowoutsDown;
begin
  ChangeThrowouts(BO.EventProps.Throwouts - 1);
end;

procedure TMobilFacade.ThrowoutsUp;
begin
  ChangeThrowouts(BO.EventProps.Throwouts + 1);
end;

procedure TMobilFacade.SetCurrentIndex(const Value: Integer);
var
  cr: TEventRowCollectionItem;
begin
  FCurrentIndex := Value;
  cr := BO.EventNode.EventRowCollection.Items[Value];
  if Assigned(cr) then
    CurrentBib := cr.Bib
  else
    CurrentBib := 0;
end;

procedure TMobilFacade.SetOnUpdateUI(const Value: TNotifyEvent);
begin
  FOnUpdateUI := Value;
end;

procedure TMobilFacade.Submit(Input: string);
var
  b: Integer;
  cr: TEventRowCollectionItem;
  v: string;
begin
  b := CurrentBib;
  if (b > 0) then
  begin
    cr := BO.EventNode.FindBib(b);
    if Assigned(cr) then
    begin
      v := Input;
      if v <> '' then
      begin
        BO.EventBO.EditRaceValue(cr, v, 'col_R' + IntToStr(CurrentRace));
        edCommandText := v;
        BO.EventNode.Modified := True;
        Main.GuiManager.DoOnIdle;
        UpdateUI;
      end;
    end;
  end;
end;

end.
