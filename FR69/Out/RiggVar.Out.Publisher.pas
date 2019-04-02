unit RiggVar.Out.Publisher;

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
  RiggVar.DAL.Redirector;

type
  TPublisher = class
  private
    SL: TStringList;
    TL: TStringList;
    procedure WrapReport(report: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure PublishAll;
  end;

implementation

{ Publisher }

uses
  RiggVar.App.Main,
  RiggVar.BO.Def;

constructor TPublisher.Create;
begin
  SL := TDBStringList.Create;
  TL := TStringList.Create;
end;

destructor TPublisher.Destroy;
begin
  SL.Free;
  TL.Free;
  inherited;
end;

procedure TPublisher.PublishAll;
var
  s: string;
  fn: string;
  pp: string;
  en: string;
begin
  en := Main.DocManager.EventName;
  pp := Main.FolderInfo.PublishPath + en + '_';

  s := BO.Output.GetMsg('FR.*.Output.Report.FinishReport');
  WrapReport(s);
  fn := pp + 'FinishReport.htm';
  SL.SaveToFile(fn);

  s := BO.Output.GetMsg('FR.*.Output.Report.PointsReport');
  WrapReport(s);
  fn := pp + 'PointsReport.htm';
  SL.SaveToFile(fn);

//  s := Main.BO.Output.GetMsg('FR.*.Output.Report.TimePointReport');
//  SL.Text := s;
//  fn := pp + 'TimePointReport.xml';
//  SL.SaveToFile(fn);

  BO.RaceDataXML.GetXML(SL);
  fn := pp + 'RaceDataReport.xml';
  SL.SaveToFile(fn);

  SL.Clear;
  TL.Clear;
end;

procedure TPublisher.WrapReport(report: string);
var
  i: Integer;
begin
  TL.Text := report;

  SL.Clear;
  SL.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0');
  SL.Add('  Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
  SL.Add('<html xmlns="http://www.w3.org/1999/xhtml">');
  SL.Add('<head>');
  SL.Add('<title>rvts-DS2006</title>');

  SL.Add('<script type="text/javascript" src="javascripts/core.js"></script>');
  SL.Add('<script type="text/javascript" src="javascripts/rvts.js"></script>');
  SL.Add('<link type="text/css" href="stylesheets/fr42.css" rel="stylesheet" />');

  SL.Add('</head>');
  SL.Add('<body>');

  for i := 0 to TL.Count - 1 do
    SL.Add(TL[i]);

  SL.Add('</body>');
  SL.Add('</html>');
end;

end.
