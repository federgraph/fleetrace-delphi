unit RiggVar.EM.WorkspaceList01;

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
  RiggVar.EM.WorkspaceList;

type
  TWorkspaceList01 = class(TWorkspaceList)
  private
    LocalWorkspaceEntry: string;
    procedure InitAppData(EventCount: Integer);
    function GetAppDataDir: string;
  public
    procedure Init(FromMemo: Boolean); override;
  end;

implementation

uses
  SHFolder,
  RiggVar.App.Main,
  RiggVar.Util.AppUtils,
  RiggVar.BO.Def,
  RiggVar.Conn.Intern;

procedure TWorkspaceList01.Init(FromMemo: Boolean);
begin
  inherited;

  if not FromMemo then
  begin
    if LocalWorkspaceEntry = '' then
      InitAppData(5);
    ParseLine(LocalWorkspaceEntry);
    NL.Insert(0, FK);
    VL.Insert(0, FV.Value);
  end;

end;

procedure TWorkspaceList01.InitAppData(EventCount: Integer);
var
  SL: TStringList; //StringList
  ec: Integer; // EventCount
  dn: string; //DirectoryName
  wr: string; //WorkspaceRoot
  fn: string; //FileName
  ed: string; //EventData
  i: Integer;
  msg: string;
  TimingConnection: TConnection;
begin
  ec := EventCount;
  if ec < 3 then
    ec := 3;
  if ec > 12 then
    ec := 12;

  dn := GetAppDataDir;
  wr := GetAppDataRoot;

  Assert(DirectoryExists(dn), 'EventData directory in AppData does not exist.');

  if not DirectoryExists(dn) then
    Exit;

  SL := TStringList.Create;
  try
    TimingConnection := BO.InputServer.Server.Connect('Timing.Temp');
    { init EventData Files if not present }
    for i := 1 to ec do
    begin
      fn := dn + 'ED' + IntToStr(i) + '.txt';
      if not FileExists(fn) then
      begin
        { use Testdata, update EventName and FinishPositions for Bib 1}
        BO.EventProps.EventName := 'ED' + IntToStr(i);
        msg := 'FR.*.W1.Bib1.RV=' + IntToStr(i);
        TimingConnection.HandleMsg(msg);
        msg := 'FR.*.W2.Bib1.RV=' + IntToStr(i);
        TimingConnection.HandleMsg(msg);

        { then retrieve the data as txt, xml, or html  }
        //ed := BO.ToTxt;
        //ed := BO.ToXml
        ed := BO.ToHtml;

        { then save }
        SL.Text := ed;
        SL.SaveToFile(fn);
      end;
    end;
    TimingConnection.Free;

    { init EventMenuXml if not present }
    fn := dn + 'EM.xml';
    if not FileExists(fn) then
    begin
      SL.Clear;
      SL.Add('<?xml version="1.0" encoding="utf-8" ?>');
      SL.Add(Format('<EventMenu Root="%s">', [wr]));
      SL.Add('  <ComboEntry Caption="Local Workspace">');
      SL.Add('    <DataFolder Url="ED\">');
      SL.Add('      <ImgFolder Url="">');
      for i := 1 to ec do
      begin
        SL.Add(Format('        <Btn Data="ED%d.txt" Img="" Text="ED %d" />', [i, i]));
      end;
      SL.Add('      </ImgFolder>');
      SL.Add('    </DataFolder>');
      SL.Add('  </ComboEntry>');
      SL.Add('</EventMenu>');
      SL.SaveToFile(fn);
    end;
  finally
    SL.Free;
  end;

  LocalWorkspaceEntry := '* Local Workspace = ' + fn;
end;

function TWorkspaceList01.GetAppDataDir: string;
begin
  result := GetAppDataRoot + 'ED\';
  ForceDirectories(result);
end;

end.
