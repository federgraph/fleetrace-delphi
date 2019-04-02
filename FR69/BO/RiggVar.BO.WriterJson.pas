unit RiggVar.BO.WriterJson;

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
  System.Json,
  RiggVar.BO.ExcelExport,
  RiggVar.BO.ExcelImport,
  RiggVar.BO.Manager,
  RiggVar.Grid.ColGrid;

type
  EventParamJson = class
  private
    JA: TJsonArray;
    procedure WriteLn(key: string; value: Integer);
  public
    RaceCount: Integer;
    ITCount: Integer;
    StartlistCount: Integer;

    class var prefix: string;

    constructor Create;

    function toArray: TJsonArray;
  end;

  EventPropJson = class
  private
    JA: TJsonArray;
  public
    Name: string;
    ScoringSystem: string;
    Throwouts: Integer;
    DivisionName: string;
    InputMode: string;
    RaceLayout: string;
    NameSchema: string;
    FieldMap: string;
    FieldCaptions: string;
    FieldCount: Integer;
    NameFieldCount: Integer;
    NameFieldOrder: string;
    UseFleets: boolean;
    TargetFleetSize: Integer;
    FirstFinalRace: Integer;
    IsTimed: boolean;
    UseCompactFormat: boolean;

    class var prefix: string;

    constructor Create;

    function toArray: TJsonArray;
    procedure WriteLn(key: string; value: string);
  end;

  JsonInfo = class
  private
    SL: TStringList;
    ee: TExcelExporter;

    procedure updateParamJson(o: EventParamJson);
    procedure updatePropJson(o: EventPropJson);

    function getEventParams: TJsonArray;
    function getEventProps: TJsonArray;
    function getNames: TJsonArray;
    function getStartList: TJsonArray;
    function getFleetList: TJsonArray;
    function getFinishList: TJsonArray;
    function getTL(r: Integer): TJsonArray;
    function getPL(r: Integer): TJsonArray;
    function getTimeLists: TJsonArray;
    function getPenaltyLists: TJsonArray;

    function getEventDataJson: TJSONObject;
    procedure getEventData(o: TJsonObject; ML: TStrings);
  public
    IncludeEmptyList: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure ReadJson(ML: TStrings);
    procedure WriteJson(ML: TStrings);
    function FormatJSON(const s: string): string;
  end;

  TJsonTransformer = class
  private
    o: TJSONObject;
    JL: TStringList;
    function ExtractK(Key: string): string;
    function ExtractP: string;
    function ExtractT: string;
    function GetHasData: Boolean;
  public
    JsonType: TJsonType;

    constructor Create;
    destructor Destroy; override;

    procedure Init(Value: string; jt: TJsonType);

    function ExtractInfo(Key: string): string;

    function ExtractEventParams: string;
    function ExtractEventProps: string;
    function ExtractFinishInfo: string;
    function ExtractFleetList: string;
    function ExtractNameTable: string;
    function ExtractPenaltyInfo: string;
    function ExtractStartList: string;
    function ExtractTimingInfo: string;

    property HasData: Boolean read GetHasData;
  end;

implementation

uses
  Rest.JSon,
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Util.Classes;

constructor EventParamJson.Create;
begin
  EventParamJson.prefix := 'DP';

  RaceCount := 2;
  ITCount := 0;
  StartlistCount := 2;
end;

function EventParamJson.toArray: TJsonArray;
begin
  JA := TJsonArray.Create;
  WriteLn('RaceCount', RaceCount);
  WriteLn('ITCount', ITCount);
  WriteLn('StartlistCount', StartlistCount);
  result := JA;
end;

procedure EventParamJson.WriteLn(key: string; value: Integer);
begin
  JA.AddElement(TJsonString.Create(Format('%s=%d', [key, value])));
end;

{ EventPropJson }

constructor EventPropJson.Create;
begin
  Name := 'Test Event Name';
  ScoringSystem := 'Low Point System';
  Throwouts := 0;
  DivisionName := '*';
  InputMode := 'Strict';
  RaceLayout := 'Finish';
  NameSchema := '';
  FieldMap := 'SN';
  FieldCaptions := '';
  FieldCount := 6;
  NameFieldCount:= 2;
  NameFieldOrder := '041256';
  UseFleets := false;
  TargetFleetSize := 8;
  FirstFinalRace := 20;
  IsTimed := false;
  UseCompactFormat := true;

  EventPropJson.prefix := 'EP';
end;

function EventPropJson.toArray: TJsonArray;
begin
  JA := TJsonArray.Create;
  WriteLn('Name', Name);
  WriteLn('Throwouts', IntToStr(Throwouts));
  WriteLn('DivisionName', DivisionName);
  WriteLn('InputMode', InputMode);
  WriteLn('RaceLayout', RaceLayout);
  WriteLn('NameSchema', NameSchema);
  WriteLn('FieldMap', FieldMap);
  WriteLn('FieldCaptions', FieldCaptions);
  WriteLn('FieldCount', IntToStr(FieldCount));
  WriteLn('NameFieldCount', IntToStr(NameFieldCount));
  WriteLn('NameFieldOrder', NameFieldOrder);
  WriteLn('UseFleets', BoolStr[UseFleets]);
  WriteLn('TargetFleetSize', IntToStr(TargetFleetSize));
  WriteLn('FirstFinalRace', IntToStr(FirstFinalRace));
  WriteLn('IsTimed', BoolStr[IsTimed]);
  WriteLn('UseCompactFormat', BoolStr[UseCompactFormat]);
  result := JA;
end;

procedure EventPropJson.WriteLn(key: string; value: string);
begin
  JA.AddElement(TJsonString.Create(Format('%s=%s', [key, value])));
end;

{ JsonInfo }

constructor JsonInfo.Create;
begin
  SL := TStringList.Create;
  ee := TExcelExporter.Create;
end;

destructor JsonInfo.Destroy;
begin
  ee.Free;
  SL.Free;
end;

function JsonInfo.FormatJSON(const s: string): string;
var
  tmp: TJsonValue;
begin
  tmp := TJSONObject.ParseJSONValue(s);
{$if CompilerVersion >= 33.0}
  Result := tmp.Format(2);
{$else}
  Result := TJson.Format(tmp);
{$endif}
  tmp.Free;
end;

procedure JsonInfo.updateParamJson(o: EventParamJson);
begin
  o.RaceCount := BO.BOParams.RaceCount;
  o.ITCount := BO.BOParams.ITCount;
  o.StartlistCount := BO.BOParams.StartlistCount;
end;

procedure JsonInfo.updatePropJson(o: EventPropJson);
begin
  o.Name := BO.EventProps.EventName;
  //o.ScoringSystem := BO.EventProps.ScoringSystem;
  o.Throwouts := BO.EventProps.Throwouts;
  o.DivisionName := BO.EventProps.DivisionName;
  //o.InputMode := BO.EventProps.InputMode;
  o.RaceLayout := BO.EventProps.RaceLayout;
  o.NameSchema := BO.EventProps.NameSchema;
  o.FieldMap := BO.EventProps.FieldMap;
  o.FieldCaptions := BO.EventProps.FieldCaptions;

  //o.FieldCount := BO.EventProps.FieldCount;
  o.FieldCount := BO.StammdatenNode.StammdatenRowCollection.FieldCount;

  //o.NameFieldCount := BO.EventProps.NameFieldCount;

  o.NameFieldOrder := BO.EventProps.NameFieldOrder;
  o.UseFleets := BO.EventProps.UseFleets;
  o.TargetFleetSize := BO.EventProps.TargetFleetSize;
  o.FirstFinalRace := BO.EventProps.FirstFinalRace;
  o.IsTimed := BO.EventProps.IsTimed;
  o.UseCompactFormat := BO.EventProps.UseCompactFormat;
end;

function JsonInfo.getEventParams: TJsonArray;
var
  o: EventParamJson;
begin
  o := EventParamJson.Create;
  updateParamJson(o);
  result := o.toArray;
  o.Free;
end;

function JsonInfo.getEventProps: TJsonArray;
var
  o: EventPropJson;
begin
  o := EventPropJson.Create;
  updatePropJson(o);
  result := o.toArray;
  o.Free;
end;

function JsonInfo.getNames: TJsonArray;
var
  JA: TJsonArray;
  i: Integer;
begin
  SL.Clear;
  JA := TJsonArray.Create;
  result := JA;

  ee.AddSection(TableID_NameList, SL);
  for i := 0 to SL.Count-1 do
  begin
    if (SL[i] <> '') then
      JA.AddElement(TJsonString.Create(SL[i]));
  end;
end;

function JsonInfo.getStartList: TJsonArray;
var
  JA: TJsonArray;
  i: Integer;
begin
  SL.Clear;
  JA := TJsonArray.Create;
  result := JA;

  ee.AddSection(TableID_StartList, SL);
  for i := 0 to SL.Count-1 do
  begin
    if (SL[i] <> '') then
      JA.AddElement(TJsonString.Create(SL[i]));
  end;
end;

function JsonInfo.getFleetList: TJsonArray;
var
  JA: TJsonArray;
  i: Integer;
begin
  SL.Clear;
  JA := TJsonArray.Create;
  result := JA;

  if (bo.EventNode.UseFleets) then
  begin
    ee.AddSection(TableID_FleetList, SL);
    for i := 0 to SL.Count-1 do
    begin
      if (SL[i] <> '') then
        JA.AddElement(TJsonString.Create(SL[i]));
    end;
  end;
end;

function JsonInfo.getFinishList: TJsonArray;
var
  JA: TJsonArray;
  i: Integer;
begin
  SL.Clear;
  JA := TJsonArray.Create;
  result := JA;

  ee.AddSection(TableID_FinishList, SL);
  for i := 0 to SL.Count-1 do
  begin
    if (SL[i] <> '') then
      JA.AddElement(TJsonString.Create(SL[i]));
  end;
end;

function JsonInfo.getTL(r: Integer): TJsonArray;
var
  JA: TJsonArray;
  i: Integer;
begin
  SL.Clear;
  JA := TJsonArray.Create;
  result := JA;

  ee.AddTimingSection(SL, r);
  for i := 0 to SL.Count-1 do
  begin
    if (SL[i] <> '') then
      JA.AddElement(TJsonString.Create(SL[i]));
  end;
end;

function JsonInfo.getPL(r: Integer): TJsonArray;
var
  JA: TJsonArray;
  i: Integer;
begin
  SL.Clear;
  JA := TJsonArray.Create;
  result := JA;

  if (r > 0) and (r <= bo.BOParams.RaceCount) then
  begin
    BO.BackupPenalties(SL, r);
    for i := 0 to SL.Count-1 do
    begin
      if (SL[i] <> '') then
        JA.AddElement(TJsonString.Create(SL[i]));
    end;
  end
end;

function JsonInfo.getTimeLists: TJsonArray;
var
  JA: TJsonArray;
  r: Integer;
begin
  JA := TJsonArray.Create;
  result := JA;

  if (BO.BOParams.ITCount > 0) or (bo.EventProps.IsTimed) then
  begin
    for r := 1 to BO.BOParams.RaceCount do
    begin
      JA.AddElement(getTL(r));
    end;
  end;
end;

function JsonInfo.getPenaltyLists: TJsonArray;
var
  JA: TJsonArray;
  r: Integer;
begin
  JA := TJsonArray.Create;
  result := JA;

  for r := 1 to BO.BOParams.RaceCount do
  begin
    JA.AddElement(getPL(r));
  end;
end;

function JsonInfo.getEventDataJson: TJSONObject;
var
  o: TJsonObject;
  t: TJSONArray;
begin
  o := TJSONObject.Create;

  result := o;

  o.AddPair('EventParams', getEventParams);
  o.AddPair('EventProps', getEventProps);
  o.AddPair('NameTable', getNames);
  o.AddPair('StartList', getStartList);
  o.AddPair('FleetList', getFleetList);
  o.AddPair('FinishInfo', getFinishList);

  if (bo.BOParams.ITCount > 0) or bo.EventProps.IsTimed then
  begin
    t := getTimeLists;
    if (t <> nil) then
      o.AddPair('TimingInfo', t);
  end;

  o.AddPair('PenaltyInfo', getPenaltyLists);
end;

procedure JsonInfo.getEventData(o: TJsonObject; ML: TStrings);
var
  ti, pi: TJsonValue;
  v: TJsonValue;
  a: TJSONArray;
begin
  a := o.Values['EventParams'] as TJSONArray;
  for v in a do
    ML.Add(v.ToString);

  a := o.Values['EventProps'] as TJSONArray;
  for v in a do
    ML.Add(v.ToString);

  a := o.Values['NameTable'] as TJSONArray;
  if (a.Count > 2) or IncludeEmptyList then
    for v in a do
      ML.Add(v.ToString);

  a := o.Values['StartList'] as TJSONArray;
  for v in a do
    ML.Add(v.ToString);

  a := o.Values['FleetList'] as TJSONArray;
  if (a.Count > 2) or IncludeEmptyList then
    for v in a do
      ML.Add(v.ToString);

  a := o.Values['FinishInfo'] as TJSONArray;
  for v in a do
    ML.Add(v.ToString);

  a := o.Values['TimingInfo'] as TJSONArray;
  if (a <> nil) and (a.Count > 0) then
    for ti in a do
      for v in (ti as TJsonArray) do
        ML.Add(v.ToString);

  a := o.Values['PenaltyInfo'] as TJSONArray;
  if (a <> nil) and (a.Count > 0) then
    for pi in a do
    begin
      if (pi as TJsonArray).Count > 0 then
        for v in (pi as TJsonArray) do
          ML.Add(v.ToString);
    end;

end;

procedure JsonInfo.WriteJson(ML: TStrings);
var
  o: TJSONObject;
begin
  o := getEventDataJson;
  //ML.Text := o.ToJSON;
  //ML.Text := FormatJSON(o.ToJSON);
  ML.Text := TUtils.PrettyFormat(o.ToJSON);
  o.Free;
end;

procedure JsonInfo.ReadJson(ML: TStrings);
var
  o: TJSONObject;
begin
  o := TJSONObject.ParseJSONValue(ML.Text) as TJSONObject;
  ML.Clear;
  getEventData(o, ML);
  o.Free;
end;

{ TJsonTransformer }

constructor TJsonTransformer.Create;
begin
  JL := TStringList.Create;
  JsonType := EventDataJSON;
end;

destructor TJsonTransformer.Destroy;
begin
  JL.Free;
  o.Free;
  inherited;
end;

procedure TJsonTransformer.Init(Value: string; jt: TJsonType);
begin
  JsonType := jt;
  if o <> nil then
    o.Free;
  o := TJSONObject.ParseJSONValue(Value) as TJSONObject;
  JL.Clear;
end;

function TJsonTransformer.ExtractInfo(Key: string): string;
begin
  if Key = 'TimingInfo' then
    result := ExtractT
  else if Key = 'PenaltyInfo' then
    result := ExtractP
  else
    result := ExtractK(Key);
end;

function TJsonTransformer.ExtractK(Key: string): string;
var
  v: TJsonValue;
  a: TJSONArray;
begin
  result := '';
  JL.Clear;

  if o = nil then
    Exit;
  a := o.Values[Key] as TJSONArray;
  if a = nil then
    Exit;

  for v in a do
    JL.Add(v.ToString);

  result := JL.Text;
  JL.Clear;
end;

function TJsonTransformer.ExtractT: string;
var
  v, ti: TJsonValue;
  a: TJSONArray;
begin
  result := '';
  JL.Clear;

  if o = nil then
    Exit;
  a := o.Values['TimingInfo'] as TJSONArray;
  if a = nil then
    Exit;

  case JsonType of
    EventDataJSON:
    begin
      if a.Count > 0 then
        for ti in a do
          if (ti as TJsonArray).Count > 0 then
            for v in (ti as TJsonArray) do
              JL.Add(v.ToString);
    end;


    RaceDataJson:
    begin
      if a.Count > 0 then
        for v in a do
          JL.Add(v.ToString);
    end;

  end;

  result := JL.Text;
  JL.Clear;
end;

function TJsonTransformer.ExtractP: string;
var
  v, pi: TJsonValue;
  a: TJSONArray;
begin
  result := '';
  JL.Clear;

  if o = nil then
    Exit;
  a := o.Values['PenaltyInfo'] as TJSONArray;
  if a = nil then
    Exit;

  case JsonType of
    EventDataJSON:
    begin
      if a.Count > 0 then
        for pi in a do
          if (pi as TJsonArray).Count > 0 then
            for v in (pi as TJsonArray) do
              JL.Add(v.ToString);

    end;

    RaceDataJSON:
    begin
      if a.Count > 0 then
        for v in a do
          JL.Add(v.ToString);
    end;
  end;


  result := JL.Text;
  JL.Clear;
end;

function TJsonTransformer.ExtractEventParams: string;
begin
  result := ExtractInfo('EventParams');
end;

function TJsonTransformer.ExtractEventProps: string;
begin
  result := ExtractInfo('EventProps');
end;

function TJsonTransformer.ExtractNameTable: string;
begin
  result := ExtractInfo('NameTable');
end;

function TJsonTransformer.ExtractStartList: string;
begin
  result := ExtractInfo('StartList');
end;

function TJsonTransformer.ExtractFleetList: string;
begin
  result := ExtractInfo('FleetList');
end;

function TJsonTransformer.ExtractFinishInfo: string;
begin
  result := ExtractInfo('FinishInfo');
end;

function TJsonTransformer.ExtractTimingInfo: string;
begin
  result := ExtractInfo('TimingInfo');
end;

function TJsonTransformer.GetHasData: Boolean;
begin
  result := o <> nil;
end;

function TJsonTransformer.ExtractPenaltyInfo: string;
begin
  result := ExtractInfo('PenaltyInfo');
end;

end.
