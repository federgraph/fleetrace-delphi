unit RiggVar.BO.Container;

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
  RiggVar.BO.Manager,
  RiggVar.BO.Params,
  RiggVar.DAL.Redirector;

type
  TBOContainer = class(TBOManager)
  private
    SL: TStringList;
    procedure GetDefaultData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitBO;
    function GetTestData: string;
    procedure CreateNew(SL: TStrings);
    procedure LoadNew(const Data: string);
    procedure RecreateBO(BOParams: TBOParams);
    procedure RecreateBOFromBackup;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

{ TBOContainer }

constructor TBOContainer.Create;
begin
  inherited Create;
  SL := TStringList.Create;
  GetDefaultData;
end;

destructor TBOContainer.Destroy;
begin
  Main.BeforeDeleteBO(self);
  try
    DeleteBO;
  except;
    Main.Logger.Error('Exception in TBOContainer.Destroy');
  end;
  Main.AfterDeleteBO(self);
  SL.Free;
  inherited Destroy;
end;

function TBOContainer.GetTestData: string;
begin
  result := SL.Text;
end;

procedure TBOContainer.InitBO;
begin
  CreateNew(SL);
  BO.Load(SL.Text);
end;

procedure TBOContainer.CreateNew(SL: TStrings);
var
  i: Integer;
  s, n, v: string;
  BOParams: TBOParams;
  paramCounter: Integer;
begin
  BOParams := TBOParams.Create;
  try
    paramCounter := 0;
    { read params from file }
    for i := 0 to SL.Count-1 do
    begin
      s := SL[i];
      n := Trim(SL.Names[i]);
      v := SL.ValueFromIndex[i];
      if (n = 'DP.StartlistCount') or (n = 'Event.StartlistCount') then
      begin
        BOParams.StartlistCount := StrToIntDef(v, BOParams.StartlistCount);
        Inc(paramCounter);
      end
      else if (n = 'DP.ITCount') or (n = 'Event.ITCount') then
      begin
        BOParams.ITCount := StrToIntDef(v, BOParams.ITCount);
        Inc(paramCounter);
      end
      else if (n = 'DP.RaceCount') or (n = 'Event.RaceCount') then
      begin
        BOParams.RaceCount := StrToIntDef(v, BOParams.RaceCount);
        Inc(paramCounter);
      end;
      if paramCounter >= 3 then
        break;
    end;
    { create BO with params }
    CreateBO(BOParams);
    //data not loaded form SL here, do this explicitly...
  finally
    BOParams.Free;
  end;
end;

procedure TBOContainer.LoadNew(const Data: string);
var
  ml: TStrings;
begin
  ml := TStringList.Create;
  try
    try
      ml.Text := Data;
    except
      ml.Text := '';
    end;
    DeleteBO;
    CreateNew(ml);
    BO.Load(ml.Text);
  finally
    ml.Free;
  end;
end;

procedure TBOContainer.RecreateBOFromBackup;
var
  ml: TStringList;
begin
  ml := TDBStringList.Create;
  try
    Main.StoreAdapter.LoadBackup(ml);
    if ml.Count > 0 then
    begin
    DeleteBO;
    CreateNew(ml);
    BO.Load(ml.Text);
    BO.Calc;
    end;
  finally
    ml.Free;
  end;
end;

procedure TBOContainer.RecreateBO(BOParams: TBOParams);
var
  ml: TStrings;
  tempParams: TBOParams;
begin
  ml := TStringList.Create;
  try
    try
      ml.Text := BO.Save;
      DeleteBO;
    except
      ml.Text := '';
    end;
    { create new BO with new Params }
    tempParams := TBOParams.Create;
    try
      tempParams.Assign(BOParams);
      CreateBO(tempParams);
    finally
      tempParams.Free;
    end;
    { Load saved data, old params in the stream will be ignored }
    BO.Load(ml.Text);
  finally
    ml.Free;
  end;
end;

procedure TBOContainer.GetDefaultData;
begin

  //#Params

  SL.Add('DP.StartlistCount = 8');
  SL.Add('DP.ITCount = 0');
  SL.Add('DP.RaceCount = 2');

  //#Event Properties

  SL.Add('EP.Name=');
  SL.Add('EP.ScoringSystem=Low Point System');
  SL.Add('EP.Throwouts=0');
  SL.Add('EP.DivisionName=*');
  SL.Add('EP.InputMode=Strict');
  SL.Add('EP.RaceLayout=Finish');
  SL.Add('EP.NameSchema=');
  SL.Add('EP.ColorMode=Normal');

  //#Athletes


  //#Startlist

  SL.Add('FR.*.W1.STL.Pos1.SNR=1001');
  SL.Add('FR.*.W1.STL.Pos2.SNR=1002');
  SL.Add('FR.*.W1.STL.Pos3.SNR=1003');
  SL.Add('FR.*.W1.STL.Pos4.SNR=1004');
  SL.Add('FR.*.W1.STL.Pos5.SNR=1005');
  SL.Add('FR.*.W1.STL.Pos6.SNR=1006');
  SL.Add('FR.*.W1.STL.Pos7.SNR=1007');
  SL.Add('FR.*.W1.STL.Pos8.SNR=1008');

  //#W1

  SL.Add('FR.*.W1.Bib1.Rank=2');
  SL.Add('FR.*.W1.Bib2.Rank=7');
  SL.Add('FR.*.W1.Bib3.Rank=5');
  SL.Add('FR.*.W1.Bib4.Rank=1');
  SL.Add('FR.*.W1.Bib5.Rank=6');
  SL.Add('FR.*.W1.Bib6.Rank=8');
  SL.Add('FR.*.W1.Bib7.Rank=4');
  SL.Add('FR.*.W1.Bib8.Rank=3');

  //#W2

  SL.Add('FR.*.W2.Bib1.Rank=3');
  SL.Add('FR.*.W2.Bib2.Rank=4');
  SL.Add('FR.*.W2.Bib3.Rank=8');
  SL.Add('FR.*.W2.Bib4.Rank=7');
  SL.Add('FR.*.W2.Bib5.Rank=5');
  SL.Add('FR.*.W2.Bib6.Rank=6');
  SL.Add('FR.*.W2.Bib7.Rank=2');
  SL.Add('FR.*.W2.Bib8.Rank=1');

  SL.Add('EP.IM = Strict');
end;

end.
