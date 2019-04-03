unit RiggVar.Out.FR00;

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
  RiggVar.BO.MsgToken,
  RiggVar.Out.Base,
  RiggVar.Out.Json,
  RiggVar.Out.FR06;

type
  TOutput = class(TBaseOutput)
  private
    Output6: TOutput6;
    JsonOut: TOutputJson;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMsg(sRequest: string): string; override;
  end;

implementation

{ TOutput }

constructor TOutput.Create;
begin
  inherited Create;
  JsonOut := TOutputJson.Create;
  Output6 := TOutput6.Create;
end;

destructor TOutput.Destroy;
begin
  JsonOut.Free;
  Output6.Free;
  inherited;
end;

function TOutput.GetMsg(sRequest: string): string;
var
  temp: string;
  c: Integer;
begin
  SL.Clear;
  Inc(MsgID);

  c := Pos(cTokenAnonymousOutput, sRequest);
  if c = 1 then
  begin
    temp := Copy(sRequest, Length(cTokenAnonymousOutput) + 1, Length(sRequest));
  end
  else
  begin
    c := Pos(cTokenOutput, sRequest);
    if c = 1 then
      temp := Copy(sRequest, Length(cTokenOutput) + 1, Length(sRequest));
  end;

  if c = 1 then
  begin
    { namespace Report }
    if Copy(temp, 1, 7) = 'Report.' then
    begin
      if Copy(temp, 1, 24) = 'Report.FinishReport.json' then
      begin
        JsonOut.FinishJson(SL);
      end
      else if Copy(temp, 1, 24) = 'Report.PointsReport.json' then
      begin
        JsonOut.PointsJson(SL);
      end
      else if Copy(temp, 1, 19) = 'Report.FinishReport' then
      begin
        Output6.FinishReport(SL);
      end
      else if Copy(temp, 1, 19) = 'Report.PointsReport' then
      begin
        Output6.PointsReport(SL);
      end;
    end;
    result := SL.Text;
  end
  else
    result := 'RiggVar.Output: RequestError';
end;

end.
