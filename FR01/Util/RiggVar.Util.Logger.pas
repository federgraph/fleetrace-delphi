unit RiggVar.Util.Logger;

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

type
  TTraceDestination = (
    tdNone,
    tdFile,
    tdMemo,
    tdDelphiEventLog,
    tdAll
    );

  TTraceHandler = procedure (Sender: TObject; s: string) of object;

  TLogger = class
  private
    FOnTrace: TTraceHandler;
    procedure SetOnTrace(const Value: TTraceHandler);
  protected
    procedure Write(s: string); virtual;
  public
    procedure Error(s: string); virtual;
    procedure Info(s: string); virtual;
    property OnTrace: TTraceHandler read FOnTrace write SetOnTrace;
  end;

implementation

procedure TLogger.Write(s: string);
begin
  if Assigned(OnTrace) then
    OnTrace(nil, s);
end;

procedure TLogger.Error(s: string);
begin
  Write(s);
end;

procedure TLogger.Info(s: string);
begin
  Write(s);
end;

procedure TLogger.SetOnTrace(const Value: TTraceHandler);
begin
  FOnTrace := Value;
end;

end.
