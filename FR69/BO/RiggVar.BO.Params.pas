unit RiggVar.BO.Params;

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
  System.Classes;

type
  TEditPage = (
    epNone,
    epParams,
    epRegatta,
    epUniqua,
    epFleet,
    epEvent,
    epIni,
    epNames,
    epScoring,
    epDB
  );

  TAdapterParams = class(TPersistent)
  public
    IsAdapter: Boolean;
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  end;

  TBOParams = class(TAdapterParams)
  public
    MaxStartlistCount: Integer;
    MinStartlistCount: Integer;
    StartlistCount: Integer;
    //
    MinRaceCount: Integer;
    MaxRaceCount: Integer;
    RaceCount: Integer;
    //
    MinITCount: Integer;
    MaxITCount: Integer;
    ITCount: Integer;
    //
    DivisionName: string;
    //
    constructor Create;
    procedure InitVars(Kategory: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure ForceWithinLimits;
    function IsWithinLimits: Boolean;
  end;

  TMainParams = class
  public
    DisabledEditPages: set of TEditPage;

    TimingGridBibCount: Integer;
    TimingGridColCount: Integer;
    TimingGridRowCount: Integer;

    UserLevel: Integer;

    AutoSave: Boolean;
    NoAutoSave: Boolean;
    UseUnicode: Boolean;
    UseDoubleBuffer: Boolean;
    WantXSL: Boolean;
    WantAutoSync: Boolean;
    WantBridgeMsgBroadcast: Boolean;
    UseDB: Boolean;
    ContextPath: string;
    WebRoot: string;
    SkipTestForListeningSocket: Boolean;
    AutoPlugin: Boolean;
    AutoOnline: Boolean;
    AutoUpload: Boolean;
    AutoHomeWeb: Boolean;
    AutoRemoteWeb: Boolean;
    AutoSilverlightWeb: Boolean;
    AutoPolicyServer: Boolean;
    WantDocrootResults: Boolean;
    UseProxyBase: Boolean;
    ThemeID: Integer;
    WantWeb: Boolean;
    IsSpecialVersion: Boolean;
    WantNames: Boolean;
    WantAuthentication: Boolean;
    WantEntryList: Boolean;
    WantFeatures: Boolean;
    WantLocalText: Boolean;
    WantAlternativeText: Boolean;
    constructor Create;
    procedure InspectorOnLoad(Sender: TObject);
    procedure InspectorOnSave(Sender: TObject);
  end;

implementation

uses
  RiggVar.BO.TemplateIDs,
  RiggVar.BO.MsgToken,
  RiggVar.Col.NameValue,
  RiggVar.Util.Classes;

{ TAdapterParams }

constructor TAdapterParams.Create;
begin
  inherited;
  IsAdapter := false;
end;

procedure TAdapterParams.Assign(Source: TPersistent);
var
  o: TAdapterParams;
begin
  if Source is TAdapterParams then
  begin
    o := TAdapterParams(Source);
    IsAdapter := o.IsAdapter;
  end
  else
    inherited Assign(Source)
end;

{ TBOParams }

function TBOParams.IsWithinLimits: Boolean;
begin
  result :=
        (RaceCount >= MinRaceCount)
    and (ITCount >= MinITCount)
    and (StartlistCount >= MinStartlistCount)

    and (RaceCount <= MaxRaceCount)
    and (ITCount <= MaxITCount)
    and (StartlistCount <= MaxStartlistCount)
end;

constructor TBOParams.Create;
var
  i: Integer;
begin
  inherited;
  i := TemplateIDByName(cTokenA);
  if (i = -1) then
    i := TemplateIDByName(cTokenSport);
  InitVars(i);
end;

procedure TBOParams.ForceWithinLimits;
begin
  if StartListCount < MinStartListCount then
    StartListCount := MinStartListCount
  else if StartListCount > MaxStartListCount then
    StartListCount := MaxStartListCount;

  if RaceCount < MinRaceCount then
    RaceCount := MinRaceCount
  else if RaceCount > MaxRaceCount then
    RaceCount := MaxRaceCount;

  if ITCount < MinITCount then
    ITCount := MinITCount
  else if ITCount > MaxITCount then
    ITCount := MaxITCount;
end;

procedure TBOParams.InitVars(Kategory: Integer);
begin
  DivisionName := '*';
  case Kategory of
    TypFREvent:
    begin
      MaxStartlistCount := 256;
      MinStartlistCount := 2;
      StartlistCount := 8;
      //
      MinRaceCount := 1;
      MaxRaceCount := 20;
      RaceCount := 7;
      //
      MinITCount := 0;
      MaxITCount := 10;
      ITCount := 0;
    end;
  end;
end;

procedure TBOParams.Assign(Source: TPersistent);
var
  o: TBOParams;
begin
  if Source is TBOParams then
  begin
    o := TBOParams(Source);
    //
    inherited Assign(Source);
    //
    MaxStartlistCount := o.MaxStartlistCount;
    MinStartlistCount := o.MinStartlistCount;
    StartlistCount := o.StartlistCount;
    //
    MinRaceCount := o.MinRaceCount;
    MaxRaceCount := o.MaxRaceCount;
    RaceCount := o.RaceCount;
    //
    MinITCount := o.MinITCount;
    MaxITCount := o.MaxITCount;
    ITCount := o.ITCount;
    //
    DivisionName := o.DivisionName;
  end
  else
    inherited Assign(Source)
end;

{ TMainParams }

constructor TMainParams.Create;
begin
  TimingGridBibCount := 12;
  TimingGridColCount := 25;
  TimingGridRowCount := 6;
  AutoSave := false;
  NoAutoSave := true;
  UseUnicode := true;
  UseDoubleBuffer := true;
  WantXSL := true;
  WantAutoSync := true;
  WantBridgeMsgBroadcast := true;
  UseDB := false;
  ContextPath := '';
  WebRoot := '';
  SkipTestForListeningSocket := true;
  AutoPlugin := false;
  AutoOnline := true;
  AutoUpload := false;
  AutoHomeWeb := false;
  AutoRemoteWeb := false;
  AutoSilverlightWeb := false;
  AutoPolicyServer := false;
  WantDocrootResults := false;
  ThemeID := 2;
  WantWeb := false;
  WantNames := true;
  WantAuthentication := false;
  WantEntryList := true;
  WantFeatures := false;
end;

procedure TMainParams.InspectorOnLoad(Sender: TObject);
var
  cl: TNameValueRowCollection;
  cr: TNameValueRowCollectionItem;
begin
  if not (Sender is TNameValueRowCollection) then
    exit;

  cl := TNameValueRowCollection(Sender);

  cr := cl.Add;
  cr.Category := 'Cache';
  cr.FieldName := 'WantAutoSync';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[WantAutoSync];
  cr.Caption := 'WantAutoSync';
  cr.Description := 'use pessimistic cache invalidation';

  cr := cl.Add;
  cr.Category := 'Security';
  cr.FieldName := 'WantNames';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[WantNames];
  cr.Caption := 'WantNames';
  cr.Description := 'blocks processing of names if false';

  cr := cl.Add;
  cr.Category := 'Security';
  cr.FieldName := 'WantAuthentication';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[WantAuthentication];
  cr.Caption := 'WantAuthentication';
  cr.Description := 'use basic authentication for remote web site';

  cr := cl.Add;
  cr.Category := 'Web';
  cr.FieldName := 'WantDocrootResults';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[WantDocrootResults];
  cr.Caption := 'WantDocrootResults';
  cr.Description := 'use EventMenu.xml from Results folder in docroot';

  cr := cl.Add;
  cr.Category := 'Web';
  cr.FieldName := 'UseProxyBase';
  cr.FieldType := nvftBoolean;
  cr.FieldValue := BoolStr[UseProxyBase];
  cr.Caption := 'UseProxyBase';
  cr.Description := 'add ProxyBase to url when deployed on HomeServer';

  cr := cl.Add;
  cr.Category := 'Web';
  cr.FieldName := 'ThemeID';
  cr.FieldType := nvftInteger;
  cr.FieldValue := IntToStr(ThemeID);
  cr.Caption := 'ThemeID';
  cr.Description := '1, 2 or default (0)';
end;

procedure TMainParams.InspectorOnSave(Sender: TObject);
var
  cl: TNameValueRowCollection;
  cr: TNameValueRowCollectionItem;
  i: Integer;
begin
  if not (Sender is TNameValueRowCollection) then
    exit;

  cl := TNameValueRowCollection(Sender);

  for i := 0 to cl.Count - 1 do
  begin
    cr := cl.Items[i];
    if cr.FieldName = 'WantNames' then
      WantNames := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'WantAuthentication' then
      WantAuthentication := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'WantAutoSync' then
      WantAutoSync := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'WantDocrootResults' then
      WantDocrootResults := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'UseProxyBase' then
      UseProxyBase := TUtils.IsTrue(cr.FieldValue)
    else if cr.FieldName = 'ThemeID' then
    begin
      ThemeID := StrToIntDef(cr.FieldValue, 0);
    end;
  end;
end;

end.
