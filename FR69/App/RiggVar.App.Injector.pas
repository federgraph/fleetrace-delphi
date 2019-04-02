unit RiggVar.App.Injector;

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
  IInjectorImpl = interface
  ['{0AADBD50-CC90-4826-9255-717CD15B8101}']
    procedure InjectDependencies;
    procedure InjectTestDependencies;
    procedure ShowScenarioSelector;
    procedure ShowStartupLog;
  end;

  TInjectorMock = class(TInterfacedObject, IInjectorImpl)
  public
    procedure InjectDependencies;
    procedure InjectTestDependencies;
    procedure ShowScenarioSelector;
    procedure ShowStartupLog;
  end;

var
  InjectorImpl: IInjectorImpl;

implementation

{ TInjectorMock }

procedure TInjectorMock.InjectDependencies;
begin

end;

procedure TInjectorMock.InjectTestDependencies;
begin

end;

procedure TInjectorMock.ShowScenarioSelector;
begin

end;

procedure TInjectorMock.ShowStartupLog;
begin

end;

end.
