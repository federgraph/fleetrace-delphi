unit FmMobil;

(*
-     F
-    * *  *
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
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  RiggVar.Col.Event,
  RiggVar.Grid.Block,
  RiggVar.Mobil.GridBlock;

type
  TMobilTab = class(TFrame)
    RaceDownBtn: TButton;
    RaceBtn: TButton;
    RaceUpBtn: TButton;
    ThrowoutsDownBtn: TButton;
    edPoints: TEdit;
    Memo: TMemo;
    edRank: TEdit;
    ThrowoutsBtn: TButton;
    ThrowoutsUpBtn: TButton;
    edCommand: TEdit;
    SubmitBtn: TButton;
    EventLabel: TLabel;
    RaceLabel: TLabel;
    GridContainer: TPanel;
    procedure RaceDownBtnClick(Sender: TObject);
    procedure RaceUpBtnClick(Sender: TObject);
    procedure ThrowoutsDownBtnClick(Sender: TObject);
    procedure ThrowoutsUpBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
  private
    function GetGridBlock: TGridBlock;
  public
    MF: TMobilColGrid;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitMobil;
    procedure UpdateDetails(Sender: TObject);
    procedure DoOnIdle;
    procedure InitGrid;
    procedure DisposeGrid;
    property GB: TGridBlock read GetGridBlock;
  end;

implementation

{$R *.dfm}

{ TMobilFrame }

constructor TMobilTab.Create(AOwner: TComponent);
begin
  inherited;
  MF := TMobilColGrid.Create;
  MF.OnUpdateUI := UpdateDetails;
  MF.GB.Parent := GridContainer;
end;

destructor TMobilTab.Destroy;
begin
  MF.Free;
  inherited;
end;

procedure TMobilTab.InitGrid;
begin
  MF.InitGrid;
end;

procedure TMobilTab.DisposeGrid;
begin
  MF.DisposeGrid;
end;

function TMobilTab.GetGridBlock: TGridBlock;
begin
  result := MF.GB;
end;

procedure TMobilTab.DoOnIdle;
begin
  MF.GB.GridUpdate.DoOnIdle;
end;

procedure TMobilTab.InitMobil;
begin
  MF.InitMobil;
end;

procedure TMobilTab.UpdateDetails(Sender: TObject);
begin
  Memo.Lines.Assign(MF.MemoLines);
  edCommand.Text := MF.edCommandText;
  edRank.Text := MF.edRankText;
  edPoints.Text := MF.edPointsText;
  EventLabel.Caption := MF.EventLabelCaption;
  RaceLabel.Caption := MF.RaceLabelCaption;
  ThrowoutsBtn.Caption := MF.ThrowoutsBtnCaption;
  RaceBtn.Caption := MF.RaceBtnCaption;
end;

procedure TMobilTab.RaceDownBtnClick(Sender: TObject);
begin
  MF.RaceDown;
end;

procedure TMobilTab.RaceUpBtnClick(Sender: TObject);
begin
  MF.RaceUp;
end;

procedure TMobilTab.ThrowoutsDownBtnClick(Sender: TObject);
begin
  MF.ThrowoutsDown;
end;

procedure TMobilTab.ThrowoutsUpBtnClick(Sender: TObject);
begin
  MF.ThrowoutsUp;
end;

procedure TMobilTab.SubmitBtnClick(Sender: TObject);
begin
  MF.Submit(edCommand.Text);
end;

end.
