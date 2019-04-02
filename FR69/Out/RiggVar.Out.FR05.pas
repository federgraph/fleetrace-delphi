unit RiggVar.Out.FR05;

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
  System.Classes,
  RiggVar.Util.Classes,
  RiggVar.Out.Intf,
  RiggVar.Out.Base;

type
  TOutput5 = class
  private
    bout: TBaseOutput; //not owned
    SL: TStrings; //not owned
    TokenParser: TTokenParser; //not owned
  public
    constructor Create(aBaseOutput: TBaseOutput);
    destructor Destroy; override;

    procedure ProxyXmlInput;
    procedure ProxyXmlOutput;
  end;

implementation

uses
  RiggVar.BO.Def,
  RiggVar.Calc.EventProxy03;

constructor TOutput5.Create(aBaseOutput: TBaseOutput);
begin
  inherited Create;
  bout := aBaseOutput;
  SL := bout.SL;
  TokenParser := bout.TokenParser;
end;

destructor TOutput5.Destroy;
begin
  inherited Destroy;
end;

procedure TOutput5.ProxyXmlInput;
var
  o: TDLLCalcEventProxy;
begin
  bout.WantPageHeader := False;
  SL.Clear;
  o := TDLLCalcEventProxy.Create;
  SL.Text := o.GetProxyXmlInput(BO.EventNode);
  o.Free;
end;

procedure TOutput5.ProxyXmlOutput;
var
  o: TDLLCalcEventProxy;
  s: string;
begin
  bout.WantPageHeader := False;
  SL.Clear;
  if BO.CalcEV.Proxy is TDLLCalcEventProxy then
  begin
    o := TDLLCalcEventProxy(BO.CalcEV.Proxy);
    s := o.GetProxyXmlOutput(BO.EventNode); //virtual method
  end
  else
  begin
    o := TDLLCalcEventProxy.Create;
    s := o.GetProxyXmlOutput(BO.EventNode);
    o.Free;
  end;
  SL.Text := s;
end;

end.
