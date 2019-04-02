unit FmCache;

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
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  RiggVar.Grid.Color,
  RiggVar.Grid.ColGrid,
  RiggVar.Grid.Block,
  RiggVar.Col.Cache,
  RiggVar.BO.CacheMotor;

type
  TCacheTab = class(TFrame)
    ToolBar: TToolBar;
    SynchronizeBtn: TSpeedButton;
    OptionsBtn: TSpeedButton;
    UpdateBtn: TSpeedButton;
    CacheLED: TShape;
    StartBtn: TSpeedButton;
    StopBtn: TSpeedButton;
    TurboBtn: TSpeedButton;
    procedure OptionsBtnClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure SynchronizeBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure TurboBtnClick(Sender: TObject);
  private
    tempAutoSync: Boolean;
    function GetCacheMotor: TCacheMotor;
  public
    GB: TGridBlock;
    DisplayTurbo: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitGrid;
    procedure DisposeGrid;
    procedure UpdateLED;
    property CacheMotor: TCacheMotor read GetCacheMotor;
  end;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  FrmCacheOptions;

constructor TCacheTab.Create(AOwner: TComponent);
begin
  inherited;
  Main.ForceToolbarProps(ToolBar);
  GB := TGridBlock.Create;
  GB.Parent := self;
  GB.Name := 'VisibleCacheGrid';
end;

destructor TCacheTab.Destroy;
begin
  GB.Free;
  inherited;
end;

procedure TCacheTab.InitGrid;
begin
  Assert(GB.ColGrid = nil);
  GB.ColBO := CacheMotor.Cache.ColBO;
  GB.Node := CacheMotor.Cache.Node;
  GB.BeginInitGrid;
  GB.ColGrid.ColorSchema := colorMoneyGreen;
  GB.EndInitGrid;
  CacheMotor.CacheEnabled := True;
end;

procedure TCacheTab.DisposeGrid;
begin
  GB.DisposeGrid;
end;

procedure TCacheTab.OptionsBtnClick(Sender: TObject);
begin
  FrmCacheOptions.EditCacheOptions(CacheMotor);
end;

procedure TCacheTab.UpdateBtnClick(Sender: TObject);
begin
  GB.ColGrid.ShowData;
end;

procedure TCacheTab.StartBtnClick(Sender: TObject);
begin
  CacheMotor.Active := True;
  CacheMotor.Cache.Status := CacheStatus_Idle;
  tempAutoSync := Main.Params.WantAutoSync;
  Main.Params.WantAutoSync := False;
end;

procedure TCacheTab.StopBtnClick(Sender: TObject);
begin
  CacheMotor.Active := False;
  Main.Params.WantAutoSync := tempAutoSync;
end;

procedure TCacheTab.SynchronizeBtnClick(Sender: TObject);
begin
  CacheMotor.Synchronize;
end;

procedure TCacheTab.TurboBtnClick(Sender: TObject);
begin
  DisplayTurbo := not DisplayTurbo;
end;

function TCacheTab.GetCacheMotor: TCacheMotor;
begin
  result := Main.GuiManager.CacheMotor;
end;

procedure TCacheTab.UpdateLED;
begin
  if CacheMotor.Active then
    CacheLED.Brush.Color := clHellGruen
  else
    CacheLED.Brush.Color := clHellRot;
end;

end.
