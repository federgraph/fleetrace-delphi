unit RiggVar.Web1.MotorBase;

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
  RiggVar.BO.IniImage,
  RiggVar.App.Config,
  RiggVar.BO.ResourceManager,
  RiggVar.Web1.Images,
  RiggVar.Web1.EventArgs,
  RiggVar.Web1.FormBuilder,
  RiggVar.Web1.MenuHelper,
  RiggVar.Web1.CSS,
  RiggVar.Web2.Base;

type
  TWebMotor = class(TWebControllerBase)
  private
    FCurrentRace: Integer;
    FCurrentIT: Integer;
    FOfflineMsg: string;
   protected
    TL: TStringList;
    EA: TWebEventArgs;
    NL: TStringList;
    FormBuilder: TWebFormBuilder;
    MenuBuilder: TMenuBuilder;
    function InitPage(const h, p, pre: string): string;
    function WriteHeader: string;
    function InitFR42Css: string;
    function InitFR62Css: string;
    procedure WrapReport1;
    procedure WrapReport(h2: string);
    procedure InsertMenu;
    procedure InjectMenuIntoAnswer;
    procedure InsertReport(r: string);
    procedure WriteRaceSelectorForm(action: string);
    procedure SetCurrentRace(const Value: Integer);
    procedure SetCurrentIT(const Value: Integer);
    procedure InitOfflineMsg;
    function GetOfflineMsg: string;
    procedure WriteMenu(o: TStrings); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property CurrentRace: Integer read FCurrentRace write SetCurrentRace;
    property CurrentIT: Integer read FCurrentIT write SetCurrentIT;
    property OfflineMsg: string read GetOfflineMsg;
  end;

implementation

uses
  RiggVar.Web1.Proxy;

constructor TWebMotor.Create;
begin
  inherited Create;
  FCurrentRace := 1;
  FCurrentIT := 0;
  TL := TStringList.Create;
  EA := TWebEventArgs.Create;
  NL := TStringList.Create;
  FormBuilder := TWebFormBuilder.Create(SL);
  MenuBuilder := TMenuBuilder.Create;
  InitOfflineMsg;
  IsOffline := True;
end;

destructor TWebMotor.Destroy;
begin
  FormBuilder.Free;
  MenuBuilder.Free;
  NL.Free;
  TL.Free;
  EA.Free;
  inherited;
end;

function TWebMotor.WriteHeader: string;
begin
  HL.Add('<link rel="stylesheet" href="stylesheets/fr62.css" type="text/css" />');
  WriteMenu(ML);
end;

procedure TWebMotor.WrapReport1;
begin
  HL.Add(Format('<script type="text/javascript" src="%sjavascripts/core.js"></script>', [WebProxy.ContextPath]));
  HL.Add(Format('<script type="text/javascript" src="%sjavascripts/rvts.js"></script>', [WebProxy.ContextPath]));
  HL.Add(Format('<link type="text/css" href="%sstylesheets/fr42.css" rel="stylesheet" />', [WebProxy.ContextPath]));

  WriteMenu(ML);

  SL.Text := EA._answer;

  EA._answer := MasterPage.Text;
end;

procedure TWebMotor.WrapReport(h2: string);
begin
  HL.Add(Format('<script type="text/javascript" src="%sjavascripts/core.js"></script>', [WebProxy.ContextPath]));
  HL.Add(Format('<script type="text/javascript" src="%sjavascripts/rvts.js"></script>', [WebProxy.ContextPath]));
  HL.Add(Format('<link type="text/css" href="%sstylesheets/fr42.css" rel="stylesheet" />', [WebProxy.ContextPath]));

  WriteMenu(ML);

  SL.Text := EA._answer;
  SL.Insert(0, '<h2>' + h2 + '</h2>');

  EA._answer := MasterPage.Text;
end;

procedure TWebMotor.InsertMenu;
var
  i: Integer;
  j: Integer;
  s: string;
begin
  TL.Clear;
  WriteMenu(TL);
  j := -1;
  for i := 0 to SL.Count - 1 do
  begin
    s := Trim(SL[i]);
    if s = '<body>' then
      j := i;
  end;
  if (j > -1) and (SL.Count > j) then
  begin
    Inc(j);
    for i := 0 to TL.Count - 1 do
    begin
      SL.Insert(j, TL[i]);
      Inc(j);
    end;
  end;
end;

procedure TWebMotor.InsertReport(r: string);
var
  i: Integer;
begin
  TL.Clear;
  TL.Text := r;
  for i := 0 to TL.Count - 1 do
  begin
    SL.Add(TL[i]);
  end;
end;

procedure TWebMotor.SetCurrentIT(const Value: Integer);
begin
  FCurrentIT := Value;
end;

procedure TWebMotor.SetCurrentRace(const Value: Integer);
begin
  FCurrentRace := Value;
end;

procedure TWebMotor.InjectMenuIntoAnswer;
begin
  SL.Text := EA._answer;
  InsertMenu;
  EA._answer := SL.Text;
end;

function TWebMotor.InitPage(const h, p, pre: string): string;
begin
  WriteHeader;

  if h <> '' then
  begin
  SL.Add('<h2>');
  SL.Add(h);
  SL.Add('</h2>');
  end;

  case EA._page of
    pageNotFound:
    begin
      SL.Add('<p>Access Denied (404)</p>');
    end;

    pageAccessDenied:
    begin
      SL.Add('<p>Access Denied (403)</p>');
    end;

    pageInfo:
      FormBuilder.WriteInfo;

    pageWidget:
      FormBuilder.WriteWidgetMenu;

    pageRace:
    begin
      EA.RaceCount := WebProxy.RaceCount;
      EA.ITCount := WebProxy.ITCount;
      FormBuilder.WriteRaceForm(EA.RaceCount, EA.ITCount, EA._race, EA._it);
    end;

    pageXml:
      FormBuilder.WriteXmlReportLinks;

    pageEntries:
      FormBuilder.WriteEntryForm(EA._msg, EA._snr);

    pageRV:
      FormBuilder.WriteRaceValueForm(EA._msg);

    pageManage:
      FormBuilder.WriteManageCmdForm(EA._msg);

    pageMenuSwap:
      FormBuilder.WriteFrmMenuSwap;

    pageMenu:
      MenuBuilder.WriteMenuRemote(SL);

    pageMenuCommand:
      MenuBuilder.WriteMenuRemote(SL);

    pageOpenEvent:
    begin
      WebProxy.FillEventNameList(NL);
      FormBuilder.WriteEventSelectorForm(NL);
      NL.Clear;
    end;

    pageSaveAs:
      FormBuilder.WriteChooseEventNameForSaveAs(WebProxy.EventName);

    pageDelete:
    begin
      WebProxy.FillEventNameList(NL);
      FormBuilder.WriteChooseEventNameForDelete(NL);
      NL.Clear;
    end;

    pageWorkspace:
      FormBuilder.WriteFrmWorkspace(
        WebProxy.WorkspaceType, WebProxy.WorkspaceID);

    pageEventParams:
        FormBuilder.WriteFrmEventParams(
          WebProxy.RaceCount,
          WebProxy.ITCount,
          WebProxy.StartlistCount);

    pageFleetProps:
        FormBuilder.WriteFrmFleetProps(
          WebProxy.UseFleets,
          WebProxy.TargetFleetSize,
          WebProxy.FirstFinalRace);

  end;

  if (p <> '') then
    SL.Add(p);

  if (pre <> '') then
  begin
    SL.Add('<pre>');
    SL.Add(pre);
    SL.Add('</pre>');
  end;

  result := MasterPage.Text;
end;

procedure TWebMotor.WriteMenu(o: TStrings);
begin
  MenuBuilder.WriteMenu(o);
end;

procedure TWebMotor.WriteRaceSelectorForm(action: string);
begin
  EA.RaceCount := WebProxy.RaceCount;
  FormBuilder.WriteRaceSelectorForm(EA.RaceCount, EA._race, action);
end;

function TWebMotor.InitFR62Css: string;
begin
  result := RiggVar.Web1.CSS.InitFR62Css(SL);
end;

function TWebMotor.InitFR42Css: string;
begin
  result := RiggVar.Web1.CSS.InitFR42Css(SL);
end;

procedure TWebMotor.InitOfflineMsg;
begin
  FormBuilder.WriteOfflineMsg;
  FOfflineMsg := SL.Text;
end;

function TWebMotor.GetOfflineMsg: string;
begin
  result := Format(FOfflineMsg, [DateTimeToStr(Now)]);
end;

end.
