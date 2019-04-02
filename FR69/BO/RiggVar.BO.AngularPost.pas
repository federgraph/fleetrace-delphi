unit RiggVar.BO.AngularPost;

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

{$define RacePart}
{$define EventPart}

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
  Vcl.ExtCtrls,
  RiggVar.BO.WriterJson,
  RiggVar.BO.ExcelImport;

type
  TAngularPost = class
  private
    SL: TStringList;
    ML: TStringList;

    Transformer: TJsonTransformer;
    ExcelImporter: TExcelImporter;

    RaceToClear: Integer;

    procedure Extract(Key: string);
    procedure Convert;

    procedure Unescape;
    procedure Trimm;
    procedure Collect;

    procedure DoParams;
    procedure DoProps;
    procedure DoNames;
    procedure DoStartList;
    procedure DoFleetList;
    procedure DoFinishInfo;
    procedure DoPenaltyInfo;
    procedure DoTimingInfo;
    procedure DoSwapEvent;
    procedure ClearAndSend;
  protected
    procedure DoClearRace(ARace: Integer = 0);
    procedure DoSend;
    procedure Filter;
    procedure Send1;
    procedure Send2;
  public
    Race: Integer;
    Json: string;
    Text: string;
    WantConversion: Boolean;
    WantFilter: Boolean;
    JsonType: TJsonType;

    constructor Create;
    destructor Destroy; override;

    procedure Transform;

    procedure ProcessRace(r: Integer = 0);
    procedure ProcessEvent;

    procedure InitEventDataJson(ML: TStrings);
    procedure InitRaceDataJson(ML: TStrings);
  end;

implementation

uses
  System.Json,
  Rest.JSon,
  RiggVar.App.Main,
  RiggVar.App.GuiInterface,
  RiggVar.BO.Def,
  RiggVar.Conn.Intern,
  RiggVar.Util.Classes;

{ TAngularPost }

constructor TAngularPost.Create;
begin
  SL := TStringList.Create;
  ML := TStringList.Create;
  Transformer := TJsonTransformer.Create;
  ExcelImporter := TExcelImporter.Create;
end;

destructor TAngularPost.Destroy;
begin
  SL.Free;
  ML.Free;
  Transformer.Free;
  ExcelImporter.Free;
  inherited;
end;

procedure TAngularPost.InitEventDataJson(ML: TStrings);
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

procedure TAngularPost.InitRaceDataJson;
begin
  ML.Clear;
  ML.Add('{');
  ML.Add('  "FinishInfo":[');
  ML.Add('    "FinishList.Begin",');
  ML.Add('    "Bib;R1",');
  ML.Add('    "1;0",');
  ML.Add('    "2;0",');
  ML.Add('    "3;0",');
  ML.Add('    "4;0",');
  ML.Add('    "5;0",');
  ML.Add('    "6;0",');
  ML.Add('    "7;0",');
  ML.Add('    "8;0",');
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

procedure TAngularPost.Extract(Key: string);
begin
  if not Transformer.HasData then
    ML.Text := ''
  else if Key = 'TimingInfo' then
    ML.Text := Transformer.ExtractTimingInfo
  else if Key = 'PenaltyInfo' then
    ML.Text := Transformer.ExtractPenaltyInfo
  else
    ML.Text := Transformer.ExtractInfo(Key);
end;

procedure TAngularPost.Unescape;
var
  i: Integer;
  s: string;
begin
  for i := 0 to ML.Count-1 do
  begin
     s := ML[i];
     if s.Contains('"') then
     begin
       s := StringReplace(s, '"', '', [rfReplaceAll]);
       ML[i] := s;
     end;
  end;
end;

procedure TAngularPost.Trimm;
var
  i: Integer;
  s: string;
begin
  for i := ML.Count-1 downto 0 do
  begin
    s := ML[i].Trim;
    if s = '' then
      ML.Delete(i)
    else
      ML[i] := s;
  end;
end;

procedure TAngularPost.Filter;
var
  i: Integer;
  s: string;
  t: string;
begin
  t := '.W' + IntToStr(Race);
  for i := 0 to ML.Count-1 do
  begin
     s := ML[i];
     if not s.Contains(t) then
     begin
       s := '';
       ML[i] := s;
     end;
  end;
end;

procedure TAngularPost.Convert;
begin
  ExcelImporter.RunImportFilter(ML.Text, ML);
end;

procedure TAngularPost.DoClearRace(ARace: Integer);
var
  r: Integer;
begin
  if ARace = 0 then
    r := Race
  else
    r := ARace;

  if (r > 0) and (r <= BO.BOParams.RaceCount) then
  begin
{$ifdef RacePart}
    BO.RNode[r].RaceRowCollection.ClearResult;
{$endif}
{$ifdef EventPart}
    BO.EventNode.EventRowCollection.ResetRace(r);
{$endif}
  end;
end;

procedure TAngularPost.DoSend;
begin
  Send2;
end;

procedure TAngularPost.Send1;
var
  c: TConnection;
  s: string;
begin
  c := BO.InputServer.Server.Connect('FormJson.Input');
  s := 'RiggVar.Request.'#13#10 + SL.Text;
  c.InjectMsg(s);
  c.Free;
end;

procedure TAngularPost.Send2;
begin
  BO.LoadPartial(SL);
end;

procedure TAngularPost.Collect;
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

procedure TAngularPost.Transform;
begin
  SL.Clear;

  Transformer.Init(Json, JsonType);

  DoParams;
  Collect;

  DoProps;
  Collect;

  DoNames;
  Collect;

  DoStartList;
  Collect;

  DoFleetList;
  Collect;

  DoFinishInfo;
  Collect;

  DoTimingInfo;
  Collect;

  DoPenaltyInfo;
  Collect;

  Text := SL.Text;
end;

procedure TAngularPost.DoParams;
begin
  Extract('EventParams');
  Unescape;
  Trimm;
end;

procedure TAngularPost.DoProps;
begin
  Extract('EventProps');
  Unescape;
  Trimm;
end;

procedure TAngularPost.DoNames;
begin
  Extract('NameTable');
  Unescape;
  Trimm;
end;

procedure TAngularPost.DoStartList;
begin
  Extract('StartList');
  Unescape;
  Trimm;
  if WantConversion then
    Convert;
end;

procedure TAngularPost.DoFleetList;
begin
  Extract('FleetList');
  Unescape;
  Trimm;
  if WantConversion then
    Convert;
end;

procedure TAngularPost.DoFinishInfo;
begin
  Extract('FinishInfo');
  Unescape;
  Trimm;
  if WantConversion then
    Convert;
end;

procedure TAngularPost.DoTimingInfo;
begin
  ML.Text := Transformer.ExtractTimingInfo;
  Unescape;
  Trimm;
  if WantConversion then
    Convert;
end;

procedure TAngularPost.DoPenaltyInfo;
begin
  ML.Text := Transformer.ExtractPenaltyInfo;
  Unescape;
  Trimm;
end;

procedure TAngularPost.DoSwapEvent;
begin
  Main.GuiManager.SwapEvent(Text);
end;

procedure TAngularPost.ProcessEvent;
begin
  Json := TUtils.PrettyFormat(Json);
  Transform;
  TThread.Synchronize(nil, DoSwapEvent);
end;

procedure TAngularPost.ProcessRace(r: Integer);
begin
  SL.Clear;
  Json := TUtils.PrettyFormat(Json);

  Transformer.Init(Json, JsonType);

  DoTimingInfo;
  Collect;
  DoFinishInfo;
  Collect;
  DoPenaltyInfo;
  Collect;
  SL.Add('EP.IM = Strict');

  if r > 0 then
  begin
    //must be done on the current race, according to the race-parameter in http-post-request
    RaceToClear := r;
    TThread.Synchronize(nil, ClearAndSend);
    RaceToClear := 0;
  end
  else
    TThread.Synchronize(nil, DoSend);

  SL.Clear;
end;

procedure TAngularPost.ClearAndSend;
begin
  DoClearRace(RaceToClear);
  DoSend;
end;

end.
