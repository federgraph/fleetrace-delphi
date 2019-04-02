unit RiggVar.Web4.EventArgs;

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
  TPageEnum = (
    pageIndex,
    pageEventMenuJSonN,
    pageEventMenuJSon,
    pageEventMenuData,
    pageEventDataJson,
    pageEventDataJsonAsHtml,
    pageRaceDataJson,
    pageOffline,

    pageDownloadCurrentEventData,
    pageDownloadEventMenuJSON
  );

  TApiEnum = (
    api_none,

    api_rd_json,
    api_ed_json,
    api_event_data,

    api_get_input_connection_status,
    api_get_output_connection_status,
    api_input_wire_connect,
    api_input_wire_disconnect,
    api_output_wire_connect,
    api_output_wire_disconnect,

    api_send_msg,
    api_manage_clear,
    api_manage_clear_timepoint,
    api_manage_clear_race,
    api_manage_go_back_to_race,
    api_query_params,

    api_event_data_html,
    api_event_data_json,
    api_race_data_json,
    api_event_menu_json,

    api_get_simple_text,
    api_get_simple_json,

    api_widget_test,
    api_widget_time,
    api_widget_netto,

    api_widget_do_time,
    api_widget_do_finish,
    api_widget_do_time_for_table,
    api_widget_do_finish_for_table,

    api_widget_do_timing_event_quick,
    api_widget_do_timing_event,
    api_widget_do_timing_event_for_table,

    api_widget_get_race_table_html,
    api_widget_get_race_table_json,
    api_widget_get_narrow_race_table_json,
    api_widget_get_wide_race_table_json,

    api_widget_get_event_table_json,
    api_widget_get_finish_table_json,
    api_widget_get_points_table_json

    );

  TWebEventArgs = class
  public
    _page: TPageEnum;
    _id: Integer;
    _answer: string;
  end;

  TWidgetApiHelper= class
  private
    procedure UpdateFinishPosition;
  public
    Api: TApiEnum;

    { sometimes initialized from query params }
    option: Integer;
    mode: Integer;
    race: Integer;
    it: Integer;
    bib: Integer;
    value: string;

    { internal vars }
    time: string;
    finishPosition: Integer;
    request: string;
    msg: string;

    { for interal use and then contains muliline output }
    RL: TStrings;

    constructor Create;
    destructor Destroy; override;

    procedure GetEventTableJson;
    procedure GetFinishReportJson;
    procedure GetPointsReportJson;

    procedure DoStatus;
    procedure DoTime;
    procedure DoFinish;
    procedure DoTimeForTable;
    procedure DoFinishForTable;

    procedure DoTimingEventQuick;
    procedure DoTimingEvent;
    procedure DoTimingEventForTable;

    procedure GetRaceTableHtml;
    procedure GetNarrowRaceTableJson;
    procedure GetWideRaceTableJson;
    procedure GetRaceTableJson;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Col.Event,
  RiggVar.Out.Json,
  RiggVar.Util.Sound;

{ TWidgetApiHelper }

constructor TWidgetApiHelper.Create;
begin
  RL := TStringList.Create;
end;

destructor TWidgetApiHelper.Destroy;
begin
  RL.Free;
  inherited;
end;

procedure TWidgetApiHelper.GetRaceTableHtml;
begin
  race := Main.GuiManager.Race;
  it := Main.GuiManager.IT;
  msg := Format('FR.*.Request.HTM.Web.Race.Report5.R%d.IT%d', [race, it]);
  RL.Text := BO.UndoConnection.HandleMsg(msg);
end;

procedure TWidgetApiHelper.GetRaceTableJson;
begin
  if mode = 2 then
    GetWideRaceTableJson
  else
    GetNarrowRaceTableJson;
end;

procedure TWidgetApiHelper.GetNarrowRaceTableJson;
begin
  TOutputJson.RequestedRace := race;
  TOutputJson.RequestedIT := it;
  msg := 'FR.*.Request.Report.NarrowRaceTable.json';
  RL.Text := BO.UndoConnection.HandleMsg(msg);
end;

procedure TWidgetApiHelper.GetWideRaceTableJson;
begin
  TOutputJson.RequestedRace := race;
  TOutputJson.RequestedIT := it;
  msg := 'FR.*.Request.Report.WideRaceTable.json';
  RL.Text := BO.UndoConnection.HandleMsg(msg);
end;

procedure TWidgetApiHelper.GetEventTableJson;
begin
  if mode = 2 then
    GetPointsReportJson
  else
    GetFinishReportJson;
end;

procedure TWidgetApiHelper.GetFinishReportJson;
begin
  msg := 'FR.*.Request.Report.FinishReport.json';
  RL.Text := BO.UndoConnection.HandleMsg(msg);
end;

procedure TWidgetApiHelper.GetPointsReportJson;
begin
  msg := 'FR.*.Request.Report.PointsReport.json';
  RL.Text := BO.UndoConnection.HandleMsg(msg);
end;

procedure TWidgetApiHelper.DoTime;
begin
  time := FormatDateTime('hh:mm:ss.zz', Now);
  msg := Format('FR.*.W%d.Bib%d.IT%d=%s', [race, bib, it, time]);
  BO.UndoConnection.InjectMsg(msg);
end;

procedure TWidgetApiHelper.DoFinish;
var
  cr: TEventRowCollectionItem;
begin
  { assume strict mode }
  finishPosition := 0;
  msg := Format('FR.*.W%d.Bib%d.RV=%d', [race, bib, 500]);
  BO.UndoConnection.InjectMsg(msg);
  UpdateFinishPosition;
  cr := BO.EventNode.FindBib(bib);
  if cr <> nil then
  begin
    if (race > 0) and (race <= BO.BOParams.RaceCount) then
      finishPosition := cr.Race[race].OTime;
  end;
end;

procedure TWidgetApiHelper.DoStatus;
begin
  msg := Format('FR.*.W%d.Bib%d.RV=%s', [race, bib, value]);
  BO.UndoConnection.InjectMsg(msg);
end;

procedure TWidgetApiHelper.DoTimeForTable;
begin
  time := FormatDateTime('hh:mm:ss.zz', Now);
  msg := Format('FR.*.W%d.Bib%d.IT%d=%s', [race, bib, it, time]);
  request := 'FR.*.Request.Report.NarrowRaceTable.json';

  RL.Clear;
  RL.Add(request);
  RL.Add(msg);

  TOutputJson.RequestedRace := race;
  TOutputJson.RequestedIT := it;
  RL.Text := BO.UndoConnection.HandleMsg(RL.Text);
end;

procedure TWidgetApiHelper.DoFinishForTable;
begin
  { assume strict mode }
  msg := Format('FR.*.W%d.Bib%d.RV=%d', [race, bib, 500]);
  request := 'FR.*.Request.Report.FinishReport.json';

  RL.Clear;
  RL.Add(request);
  RL.Add(msg);

  RL.Text := BO.UndoConnection.HandleMsg(RL.Text);
end;

procedure TWidgetApiHelper.UpdateFinishPosition;
var
  cr: TEventRowCollectionItem;
begin
  cr := BO.EventNode.FindBib(bib);
  if cr <> nil then
  begin
    if (race > 0) and (race <= BO.BOParams.RaceCount) then
      finishPosition := cr.Race[race].OTime;
  end;
end;

procedure TWidgetApiHelper.DoTimingEventQuick;
begin
  { does not do qu or erase, see DoTimingEventPlus }
  DoTime;
  if it = 0 then
    DoFinish;
end;

procedure TWidgetApiHelper.DoTimingEvent;
begin
  BO.DoTimingEvent(race, it, bib, option);

end;

procedure TWidgetApiHelper.DoTimingEventForTable;
begin
  BO.DoTimingEvent(race, it, bib, option);
  case mode of
    0: GetNarrowRaceTableJson;
    1: GetWideRaceTableJson;
    2: GetFinishReportJson;
    3: GetPointsReportJson;
    else
      GetFinishReportJson;
  end;

end;

end.
