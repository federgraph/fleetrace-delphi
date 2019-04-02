unit FrmStartupScenario;

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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Grids,
  RiggVar.BO.ScenarioManager;

type
  TFormStartupScenario = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ScenarioManager: TScenarioManager;
    SG: TStringGrid;
    SL: TStrings;
    procedure InitGrid;
    procedure StringGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FillGrid;
  public
  end;

var
  FormStartupScenario: TFormStartupScenario;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormStartupScenario.FormCreate(Sender: TObject);
begin
  Width := 300;
  Height := 300;
  ScenarioManager := TScenarioManager.Create;
  SL := ScenarioManager.ScenarioList;
  InitGrid;
  FillGrid;
  SG.Row := 1;
end;

procedure TFormStartupScenario.FormDestroy(Sender: TObject);
begin
  ScenarioManager.Free;
end;

procedure TFormStartupScenario.InitGrid;
begin
  SG := TStringGrid.Create(self);
  SG.Parent := self;

  SG.Options := [
    goFixedVertLine,
    goFixedHorzLine,
    goVertLine,
    goHorzLine,
    goColSizing,
    goRowSelect
    ];

  SG.FixedCols := 1;
  SG.ColCount := 2;

  SG.ColWidths[0] := 20;
  SG.ColWidths[1] := 200;

  SG.Cells[0,0] := 'ID';
  SG.Cells[1,0] := 'Startup Scenario';

  SG.Align := alClient;
  SG.OnKeyUp := StringGridKeyUp;
end;

procedure TFormStartupScenario.FillGrid;
var
  r: Integer;
  i: Integer;
begin
  if SL.Count > 0 then
  begin
    SG.RowCount := SL.Count + 1;
    r := 1;
    for i := 0 to SL.Count-1 do
    begin
      SG.Cells[0, r] := IntToStr(i);
      SG.Cells[1, r] := SL[i];
      Inc(r);
    end;
  end;
end;

procedure TFormStartupScenario.StringGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Close;
    Main.ScenarioInfo.Scenario := TScenarioInfo.ParseInt(SG.Row-1);
  end;
end;

end.
