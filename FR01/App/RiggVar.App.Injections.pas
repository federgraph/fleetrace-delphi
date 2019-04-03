unit RiggVar.App.Injections;

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
  RiggVar.App.Injector;

type
  TInjectorImpl = class(TInterfacedObject, IInjectorImpl)
  public
    procedure InjectDependencies;
    procedure InjectTestDependencies;
    procedure ShowScenarioSelector;
    procedure ShowStartupLog;
    procedure ShowMessageBox(s: string);
  end;

implementation

uses
  Winapi.Windows,
  Vcl.Forms,
  System.SysUtils,
  RiggVar.App.Main,
  RiggVar.EM.WorkspaceList,
  RiggVar.Util.AppUtils;

procedure TInjectorImpl.InjectDependencies;
begin
  { needed in Service (System32) and ISAPI (w3wp) }
  Main.StartupLogger.Add('Setting CurrentDir');
  SetCurrentDir(ExtractFilePath(TAppUtils.GetFullExeName));
end;

procedure TInjectorImpl.InjectTestDependencies;
begin
end;

procedure TInjectorImpl.ShowStartupLog;
begin
//  with TFormLogger.Create(nil) do
//  begin
//    Memo.Text := Main.StartupLogger.Text;
//    ShowModal;
//    Free;
//  end;
end;


procedure TInjectorImpl.ShowScenarioSelector;
begin
//  with TFormStartupScenario.Create(nil) do
//  begin
//    ShowModal;
//    Free;
//  end;
end;

procedure TInjectorImpl.ShowMessageBox(s: string);
begin
  MessageBox(0, PChar(s), PChar('Main.MessageBox'),
    mb_OK + mb_Topmost + mb_Service_Notification);
end;

end.
