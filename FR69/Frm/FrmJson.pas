unit FrmJson;

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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  RiggVar.BO.AngularPost,
  RiggVar.BO.WriterJson,
  RiggVar.BO.ExcelImport,
  Vcl.ExtCtrls;

type
  TFormJson = class(TForm)
    ToolBar: TToolBar;
    Memo: TMemo;
    PenaltyInfoBtn: TSpeedButton;
    PropsBtn: TSpeedButton;
    FinishInfoBtn: TSpeedButton;
    ParamsBtn: TSpeedButton;
    TimingInfoBtn: TSpeedButton;
    StartlistBtn: TSpeedButton;
    FleetlistBtn: TSpeedButton;
    NameTableBtn: TSpeedButton;
    RightPanel: TPanel;
    UnescapeBtn: TButton;
    UnframeBtn: TButton;
    TrimBtn: TButton;
    FilterBtn: TButton;
    Panel1: TPanel;
    ETestBtn: TButton;
    RTestBtn: TButton;
    DataBtn: TButton;
    JsonBtn: TButton;
    InitTransBtn: TButton;
    FPBtn: TButton;
    QUBtn: TButton;
    RaceBtn: TButton;
    TransformNBtn: TButton;
    TLBtn: TButton;
    ConvertBtn: TButton;
    CopyMemoBtn: TButton;
    ClearRaceBtn: TButton;
    SendBtn: TButton;
    TransformCBtn: TButton;
    SwapBtn: TButton;
    LoadPartialBtn: TButton;
    PrettyPrintBtn: TButton;
    ShowJsonTypeBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ETestBtnClick(Sender: TObject);
    procedure RTestBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);
    procedure OTestBtnClick(Sender: TObject);
    procedure DataBtnClick(Sender: TObject);
    procedure InitTransBtnClick(Sender: TObject);
    procedure ParamsBtnClick(Sender: TObject);
    procedure PropsBtnClick(Sender: TObject);
    procedure FinishInfoBtnClick(Sender: TObject);
    procedure PenaltyInfoBtnClick(Sender: TObject);
    procedure TimingInfoBtnClick(Sender: TObject);
    procedure StartlistBtnClick(Sender: TObject);
    procedure NameTableBtnClick(Sender: TObject);
    procedure FleetlistBtnClick(Sender: TObject);
    procedure UnescapeBtnClick(Sender: TObject);
    procedure CopyMemoBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
    procedure UnframeBtnClick(Sender: TObject);
    procedure TrimBtnClick(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure FilterBtnClick(Sender: TObject);
    procedure ClearRaceBtnClick(Sender: TObject);
    procedure FPBtnClick(Sender: TObject);
    procedure QUBtnClick(Sender: TObject);
    procedure RaceBtnClick(Sender: TObject);
    procedure TransformCBtnClick(Sender: TObject);
    procedure TransformNBtnClick(Sender: TObject);
    procedure TLBtnClick(Sender: TObject);
    procedure SwapBtnClick(Sender: TObject);
    procedure LoadPartialBtnClick(Sender: TObject);
    procedure PrettyPrintBtnClick(Sender: TObject);
    procedure ShowJsonTypeBtnClick(Sender: TObject);
  private
    SL: TStringList;
    ML: TStringList;
    AngularPost: TAngularPost;
    Transformer: TJsonTransformer;
    ExcelImporter: TExcelImporter;
    procedure ExtractInfo(Key: string);
    procedure Unescape(AL: TStrings);
    procedure Unframe(AL: TStrings);
    procedure Trimm(AL: TStrings);
    procedure Filter(AL: TStrings; ARace: Integer);
    procedure Send(AL: TStrings);
    function GetRace: Integer;
    procedure SetRace(const Value: Integer);
    procedure Clear(ARace: Integer);
    procedure Extract(AL: TStrings; Key: string);
    procedure FP;
    procedure QU;
    procedure Collect;
    procedure FPQU;
    procedure TL;
    procedure Transform(WantConversion: Boolean);
  protected
    property Race: Integer read GetRace write SetRace;
  public
    procedure InitEventDataJson(ML: TStrings);
    procedure InitRaceDataJson(ML: TStrings);
  end;

var
  FormJson: TFormJson;

procedure EditJson;

implementation

uses
  System.Json,
  Rest.JSon,
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Def,
  RiggVar.Conn.Intern,
  RiggVar.Util.Classes;

{$R *.dfm}

procedure EditJson;
begin
  if not Assigned(FormJson) then
    FormJson := TFormJson.Create(Application);

  FormJson.Show;
end;

{ TFormJson }

procedure TFormJson.FormCreate(Sender: TObject);
begin
  Main.ForceToolbarProps(ToolBar);
  Memo.Align := alClient;
  Width := 800;
  Height := 600;
  SL := TStringList.Create;
  ML := TStringList.Create;
  Transformer := TJsonTransformer.Create;
  ExcelImporter := TExcelImporter.Create;
  AngularPost := TAngularPost.Create;
end;

procedure TFormJson.FormDestroy(Sender: TObject);
begin
  SL.Free;
  ML.Free;
  Transformer.Free;
  ExcelImporter.Free;
  AngularPost.Free;
end;

function TFormJson.GetRace: Integer;
begin
  result := Main.GuiManager.Race;
end;

procedure TFormJson.InitEventDataJson(ML: TStrings);
begin
  ML.Clear;
  ML.Add('{');
  ML.Add('  "EventParams":[');
  ML.Add('    "DP.RaceCount=2",');
  ML.Add('    "DP.ITCount=1",');
  ML.Add('    "DP.StartlistCount=8"');
  ML.Add('  ],');
  ML.Add('  "EventProps":[');
  ML.Add('    "EP.Name=Event Name",');
  ML.Add('    "EP.DivisionName=*",');
  ML.Add('    "EP.InputMode=Strict",');
  ML.Add('    "EP.RaceLayout=Finish",');
  ML.Add('    "EP.FieldMap=SN",');
  ML.Add('    "EP.FieldCount=6",');
  ML.Add('    "EP.NameFieldCount=2",');
  ML.Add('    "EP.NameFieldOrder=041256",');
  ML.Add('    "EP.TargetFleetSize=8",');
  ML.Add('    "EP.FirstFinalRace=20",');
  ML.Add('    "EP.UseCompactFormat=true"');
  ML.Add('  ],');
  ML.Add('  "NameTable":[');
  ML.Add('    "NameList.Begin",');
  ML.Add('    "NameList.End"');
  ML.Add('  ],');
  ML.Add('  "StartList":[');
  ML.Add('    "StartList.Begin",');
  ML.Add('    "Pos;SNR;Bib",');
  ML.Add('    "1;1001;1",');
  ML.Add('    "2;1002;2",');
  ML.Add('    "3;1003;3",');
  ML.Add('    "4;1004;4",');
  ML.Add('    "5;1005;5",');
  ML.Add('    "6;1006;6",');
  ML.Add('    "7;1007;7",');
  ML.Add('    "8;1008;8",');
  ML.Add('    "StartList.End"');
  ML.Add('  ],');
  ML.Add('  "FleetList":[');
  ML.Add('    ');
  ML.Add('  ],');
  ML.Add('  "FinishInfo":[');
  ML.Add('    "FinishList.Begin",');
  ML.Add('    "SNR;Bib;R1;R2",');
  ML.Add('      "1001;1;2;3",');
  ML.Add('      "1002;2;7;4",');
  ML.Add('      "1003;3;5;8",');
  ML.Add('      "1004;4;1;7",');
  ML.Add('      "1005;5;6;5",');
  ML.Add('      "1006;6;8;6",');
  ML.Add('      "1007;7;4;2",');
  ML.Add('      "1008;8;3;1",');
  ML.Add('    "FinishList.End"');
  ML.Add('  ],');
  ML.Add('  "TimingInfo":[');
  ML.Add('    [');
  ML.Add('      "TimeList.Begin.R1",');
  ML.Add('      "SNR;Bib;IT1;FT",');
  ML.Add('      "1001;1;11:15:37.46;",');
  ML.Add('      "1002;2;11:15:36.82;",');
  ML.Add('      "1003;3;11:14:55.85;",');
  ML.Add('      "1004;4;11:14:57.58;",');
  ML.Add('      "1005;5;11:14:57.01;",');
  ML.Add('      "1006;6;11:15:35.43;",');
  ML.Add('      "1007;7;;",');
  ML.Add('      "1008;8;11:15:35.87;",');
  ML.Add('      "TimeList.End"');
  ML.Add('    ],');
  ML.Add('    [');
  ML.Add('      "TimeList.Begin.R2",');
  ML.Add('      "SNR;Bib;IT1;FT",');
  ML.Add('      "1001;1;;",');
  ML.Add('      "1002;2;;",');
  ML.Add('      "1003;3;;",');
  ML.Add('      "1004;4;;",');
  ML.Add('      "1005;5;;",');
  ML.Add('      "1006;6;;",');
  ML.Add('      "1007;7;;",');
  ML.Add('      "1008;8;;",');
  ML.Add('      "TimeList.End"');
  ML.Add('    ]');
  ML.Add('  ],');
  ML.Add('  "PenaltyInfo":[');
  ML.Add('    [');
  ML.Add('      "FR.*.W1.Bib7.QU=DNF"');
  ML.Add('    ],');
  ML.Add('    [');
  ML.Add('      ');
  ML.Add('    ]');
  ML.Add('  ]');
  ML.Add('}');
end;

procedure TFormJson.InitRaceDataJson;
begin
  ML.Clear;
  ML.Add('{');
  ML.Add('  "FinishInfo":[');
  ML.Add('    "FinishList.Begin",');
  ML.Add('    "Bib;R1",');
  ML.Add('    "1;4",');
  ML.Add('    "2;8",');
  ML.Add('    "3;6",');
  ML.Add('    "4;1",');
  ML.Add('    "5;7",');
  ML.Add('    "6;2",');
  ML.Add('    "7;5",');
  ML.Add('    "8;3",');
  ML.Add('    "FinishList.End"');
  ML.Add('  ],');
  ML.Add('  "TimingInfo":[');
  ML.Add('    "TimeList.Begin.R1",');
  ML.Add('    "SNR;Bib;IT1;FT",');
  ML.Add('    "1001;1;11:15:37.46;",');
  ML.Add('    "1002;2;11:15:36.82;",');
  ML.Add('    "1003;3;11:14:55.85;",');
  ML.Add('    "1004;4;11:14:57.58;",');
  ML.Add('    "1005;5;11:14:57.01;",');
  ML.Add('    "1006;6;11:15:35.43;",');
  ML.Add('    "1007;7;;",');
  ML.Add('    "1008;8;11:15:35.87;",');
  ML.Add('    "TimeList.End"');
  ML.Add('  ],');
  ML.Add('  "PenaltyInfo":[');
  ML.Add('    "FR.*.W1.Bib7.QU=DNF"');
  ML.Add('  ]');
  ML.Add('}');
end;

procedure TFormJson.ETestBtnClick(Sender: TObject);
begin
  InitEventDataJson(ML);
  BO.Data0 := ML.Text;
  BO.JsonT := EventDataJson;

  Memo.Text := ML.Text;
end;

procedure TFormJson.RTestBtnClick(Sender: TObject);
begin
  InitRaceDataJson(ML);
  BO.Data0 := ML.Text;
  BO.JsonT := RaceDataJson;

  Memo.Text := ML.Text;
end;

procedure TFormJson.OTestBtnClick(Sender: TObject);
begin
  Memo.Text := BO.Output.GetMsg('FR.*.Output.Report.FinishReport.json');
end;

procedure TFormJson.DataBtnClick(Sender: TObject);
begin
  Memo.Text := BO.Data0;
end;

procedure TFormJson.Extract(AL: TStrings; Key: string);
begin
  if not Transformer.HasData then
    AL.Text := ''
  else if Key = 'TimingInfo' then
    AL.Text := Transformer.ExtractTimingInfo
  else if Key = 'PenaltyInfo' then
    AL.Text := Transformer.ExtractPenaltyInfo
  else
    AL.Text := Transformer.ExtractInfo(Key);
end;

procedure TFormJson.Unescape(AL: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to AL.Count-1 do
  begin
     s := AL[i];
     if s.Contains('"') then
     begin
       s := StringReplace(s, '"', '', [rfReplaceAll]);
       AL[i] := s;
     end;
  end;
end;

procedure TFormJson.Unframe(AL: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to AL.Count-1 do
  begin
     s := AL[i];
     if s.Contains('.Begin') then
     begin
       s := '';
       AL[i] := s;
     end;
     if s.Contains('.End') then
     begin
       s := '';
       AL[i] := s;
     end;
  end;
end;

procedure TFormJson.Trimm(AL: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := AL.Count-1 downto 0 do
  begin
    s := AL[i].Trim;
    if s = '' then
      AL.Delete(i)
    else
      AL[i] := s;
  end;
end;

procedure TFormJson.Filter(AL: TStrings; ARace: Integer);
var
  i: Integer;
  s: string;
  t: string;
begin
  t := '.W' + IntToStr(ARace);
  for i := 0 to AL.Count-1 do
  begin
     s := AL[i];
     if not s.Contains(t) then
     begin
       s := '';
       AL[i] := s;
     end;
  end;
end;

procedure TFormJson.Clear(ARace: Integer);
begin
  BO.EventNode.EventRowCollection.ResetRace(ARace);
  Main.GuiManager.GuiInterface.HandleInform(ScheduleEventUpdate);
end;

procedure TFormJson.Send(AL: TStrings);
var
  c: TConnection;
  s: string;
begin
  c := BO.InputServer.Server.Connect('FormJson.Input');
  s := 'RiggVar.Request.'#13#10 + AL.Text;
  c.InjectMsg(s);
  c.Free;
end;

procedure TFormJson.JsonBtnClick(Sender: TObject);
begin
  BO.Data0 := BO.ToJson;
  BO.JsonT := EventDataJson;

  Memo.Text := BO.Data0;
end;

procedure TFormJson.InitTransBtnClick(Sender: TObject);
begin
  Transformer.Init(BO.Data0, BO.JsonT);
  Memo.Text := BO.Data0;
end;

procedure TFormJson.ExtractInfo(Key: string);
begin
  if not Transformer.HasData then
    Memo.Text := 'Transformer.HasData = false;'
  else
  begin
    Extract(ML, Key);
    Memo.Text := ML.Text;
  end;
end;

procedure TFormJson.ParamsBtnClick(Sender: TObject);
begin
  ExtractInfo('EventParams');
end;

procedure TFormJson.PrettyPrintBtnClick(Sender: TObject);
begin
  Memo.Text := TUtils.PrettyFormat(Memo.Text);
end;

procedure TFormJson.PropsBtnClick(Sender: TObject);
begin
  ExtractInfo('EventProps');
end;

procedure TFormJson.NameTableBtnClick(Sender: TObject);
begin
  ExtractInfo('NameTable');
end;

procedure TFormJson.StartlistBtnClick(Sender: TObject);
begin
  ExtractInfo('StartList');
end;

procedure TFormJson.FinishInfoBtnClick(Sender: TObject);
begin
  ExtractInfo('FinishInfo');
end;

procedure TFormJson.FleetlistBtnClick(Sender: TObject);
begin
  ExtractInfo('FleetList');
end;

procedure TFormJson.TimingInfoBtnClick(Sender: TObject);
begin
  ExtractInfo('TimingInfo');
end;

procedure TFormJson.PenaltyInfoBtnClick(Sender: TObject);
begin
  ExtractInfo('PenaltyInfo');
end;

procedure TFormJson.TrimBtnClick(Sender: TObject);
begin
  Trimm(Memo.Lines);
end;

procedure TFormJson.UnescapeBtnClick(Sender: TObject);
begin
  Unescape(Memo.Lines);
end;

procedure TFormJson.UnframeBtnClick(Sender: TObject);
begin
  Unframe(Memo.Lines);
end;

procedure TFormJson.CopyMemoBtnClick(Sender: TObject);
begin
  Trimm(Memo.Lines);
  Memo.SelectAll;
  Memo.CopyToClipboard;
end;

procedure TFormJson.ConvertBtnClick(Sender: TObject);
begin
  ML.Text := Memo.Text;
  ExcelImporter.RunImportFilter(ML.Text, ML);
  Memo.Text := ML.Text;
end;

procedure TFormJson.ClearRaceBtnClick(Sender: TObject);
begin
  Clear(Race);
end;

procedure TFormJson.FilterBtnClick(Sender: TObject);
begin
  Filter(Memo.Lines, Race);
end;

procedure TFormJson.SendBtnClick(Sender: TObject);
begin
  Send(Memo.Lines);
end;

procedure TFormJson.SetRace(const Value: Integer);
begin
  if Value <> Main.GuiManager.Race then
  begin
    Main.GuiManager.Race := Value;
  end;
end;

procedure TFormJson.ShowJsonTypeBtnClick(Sender: TObject);
begin
  case BO.JsonT of
    EventDataJSON: Memo.Text := 'EventDataJson';
    RaceDataJSON: Memo.Text := 'RaceDataJson';
    else
    begin
      Memo.Text := '';
    end;
  end;
end;

procedure TFormJson.FPBtnClick(Sender: TObject);
begin
  FP;
  Memo.Text := ML.Text;
end;

procedure TFormJson.QUBtnClick(Sender: TObject);
begin
  QU;
  Memo.Text := ML.Text;
end;

procedure TFormJson.RaceBtnClick(Sender: TObject);
begin
  FPQU;
  Memo.Text := SL.Text;
  SL.Clear;
end;

procedure TFormJson.TLBtnClick(Sender: TObject);
begin
  TL;
  Memo.Text := ML.Text;
end;

procedure TFormJson.FPQU;
begin
  SL.Clear;
  FP;
  Collect;
  QU;
  Collect;
end;

procedure TFormJson.Collect;
var
  i: Integer;
begin
  for i := 0 to ML.Count-1 do
  begin
    SL.Add(ML[i])
  end;
  if ML.Count > 0 then
    SL.Add('');
end;

procedure TFormJson.FP;
begin
  Transformer.Init(BO.Data0, BO.JsonT);
  Extract(ML, 'FinishInfo');
  Unescape(ML);
  ExcelImporter.RunImportFilter(ML.Text, ML);
  Filter(ML, Race);
  Trimm(ML);
end;

procedure TFormJson.QU;
begin
  Transformer.Init(BO.Data0, BO.JsonT);
  ML.Text := Transformer.ExtractPenaltyInfo;
  Unescape(ML);
  Filter(ML, Race);
  Trimm(ML);
end;

procedure TFormJson.TL;
begin
  Transformer.Init(BO.Data0, BO.JsonT);
  ML.Text := Transformer.ExtractTimingInfo;
  Unescape(ML);
  ExcelImporter.RunImportFilter(ML.Text, ML);
  Filter(ML, Race);
  Trimm(ML);
end;

procedure TFormJson.Transform(WantConversion: Boolean);
begin
  AngularPost.JsonType := BO.JsonT;
  AngularPost.Json := BO.Data0;
  AngularPost.Race := Race;
  AngularPost.WantFilter := true;
  AngularPost.WantConversion := WantConversion;
  AngularPost.Transform;

  Memo.Text := AngularPost.Text;
end;

procedure TFormJson.TransformCBtnClick(Sender: TObject);
begin
  //transformation with conversion
  Transform(true);
end;

procedure TFormJson.TransformNBtnClick(Sender: TObject);
begin
  //transformation w/o conversion
  Transform(false);
end;

procedure TFormJson.SwapBtnClick(Sender: TObject);
begin
  Main.GuiManager.SwapEvent(Memo.Text);
end;

procedure TFormJson.LoadPartialBtnClick(Sender: TObject);
begin
  BO.LoadPartial(Memo.Lines);
end;

end.
