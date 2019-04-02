unit FrmHelp;

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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Menus,
  RiggVar.DAL.Redirector;

type
  TFormHelp = class(TForm)
    Memo: TMemo;
    MainMenu: TMainMenu;
    ActionsMenu: TMenuItem;
    LoadItem: TMenuItem;
    SaveItem: TMenuItem;
    EditItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LoadItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure ActionsMenuClick(Sender: TObject);
    procedure EditItemClick(Sender: TObject);
    procedure MemoKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
    FileName: string;
    function LoadFile: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;
  public
    { Public-Deklarationen }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

var
  FormHelp: TFormHelp;

implementation

uses
  RiggVar.App.Main;

{$R *.DFM}

procedure TFormHelp.FormCreate(Sender: TObject);
begin
  Memo.Align := alClient;
  if Main.IniImage.UseDoubleBuffer then
  begin
    Memo.DoubleBuffered := True;
  end;
  LoadFile;
end;

function TFormHelp.LoadFile: Boolean;
var
  fn: string; //filename
  found: Boolean;
  temp: string;
begin
  temp := Main.FolderInfo.HelpPath;
  fn := temp + '_docu.txt';
  found := DBFileExists(fn);
  if not found then
  begin
    fn := temp + '_doc.txt';
    found := DBFileExists(fn);
  end;
  if not found then
  begin
    fn := temp + '_Readme.txt';
    found := DBFileExists(fn);
  end;
  if not found then
  begin
    try
      Memo.Lines.Add('please update the file for this application:');
      Memo.Lines.Add(fn);
      Memo.Lines.SaveToFile(fn);
      found := DBFileExists(fn);
    except
    end;
  end;
  if found then
  begin
    Memo.Lines.LoadFromFile(fn);
    FileName := fn;
    ReadOnly := True;
  end
  else
  begin
    Memo.Clear;
    Memo.Lines.Add('cannot find ' + fn);
  end;
  result := found;
end;

procedure TFormHelp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TFormHelp.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Escape then
    Hide;
end;

procedure TFormHelp.LoadItemClick(Sender: TObject);
begin
  LoadFile;
end;

procedure TFormHelp.SaveItemClick(Sender: TObject);
begin
  Memo.Lines.SaveToFile(FileName);
  ReadOnly := True;
end;

procedure TFormHelp.ActionsMenuClick(Sender: TObject);
begin
  SaveItem.Enabled := (not ReadOnly) and (FileName <> '') and DBFileExists(FileName);
end;

procedure TFormHelp.EditItemClick(Sender: TObject);
begin
  ReadOnly := False;
end;

procedure TFormHelp.SetReadOnly(const Value: Boolean);
begin
  Memo.ReadOnly := Value;
  if Value then
  begin
    Memo.Color := clBtnFace;
  end
  else
  begin
    Memo.Color := clWindow;
  end;
end;

function TFormHelp.GetReadOnly: Boolean;
begin
  result := Memo.ReadOnly;
end;

procedure TFormHelp.MemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Escape then
    Close;
end;

end.
