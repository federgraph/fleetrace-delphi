unit FrmReportNav;

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
  Winapi.Windows, Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  RiggVar.Col.Cache,
  RiggVar.BO.CacheMotor, Vcl.ToolWin;

type
  TFormReportNav = class(TForm)
    Memo: TMemo;
    ToolBar: TToolBar;
    edReportSpinner: TEdit;
    udReportSpinner: TUpDown;
    ReportBtn: TSpeedButton;
    procedure ReportBtnClick(Sender: TObject);
    procedure udReportSpinnerChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure FormCreate(Sender: TObject);
  private
    function GetCacheMotor: TCacheMotor;
    procedure UpdateReport(ReportID: Integer);
    property CacheMotor: TCacheMotor read GetCacheMotor;
  end;

var
  FormReportNav: TFormReportNav;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormReportNav.udReportSpinnerChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
var
  cl: TCacheRowCollection;
  MaxReportID: SmallInt;
begin
  cl := CacheMotor.Cache.Node.OutputCacheRowCollection;
  MaxReportID := cl.Count;

  if NewValue < 1 then
  begin
    AllowChange := False;
  end
  else if NewValue > MaxReportID then
  begin
    udReportSpinner.Max := MaxReportID;
  end
  else if udReportSpinner.Max < MaxReportID then
    udReportSpinner.Max := MaxReportID;
end;

procedure TFormReportNav.UpdateReport(ReportID: Integer);
var
  Data: string;
  cl: TCacheRowCollection;
  cr: TCacheRowCollectionItem;
  MaxReportID: SmallInt;
begin
  { ReportID is 1 based ! }

  Memo.Clear;

  cl := CacheMotor.Cache.Node.OutputCacheRowCollection;
  MaxReportID := cl.Count;

  if (ReportID > 0) and (ReportID <= MaxReportID) then
  begin
    cr := cl.Items[ReportID-1];
    if cr <> nil then
    begin
      Data := cr.Data;
      if (Data <> '') then
        Memo.Text := Data
      else
        Memo.Lines.Add('no data');
    end;
  end
  else
  begin
    if ReportID < 1 then
      udReportSpinner.Position := 1
    else if ReportID > MaxReportID then
    begin
      udReportSpinner.Position := 1;
      udReportSpinner.Max := MaxReportID;
      udReportSpinner.Position := MaxReportID;
      UpdateReport(ReportID);
    end;
  end;
end;

procedure TFormReportNav.FormCreate(Sender: TObject);
begin
  Memo.Align := alClient;
end;

function TFormReportNav.GetCacheMotor: TCacheMotor;
begin
  result := Main.GuiManager.CacheMotor;
 end;

procedure TFormReportNav.ReportBtnClick(Sender: TObject);
begin
  UpdateReport(udReportSpinner.Position);
end;

end.
