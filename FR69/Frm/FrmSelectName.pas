unit FrmSelectName;

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
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TFormSelectName = class(TForm)
    ListBox: TListBox;
    PromptLabel: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    SelectedName: string;
  end;

implementation

{$R *.DFM}

{ TFormSelectName }

procedure TFormSelectName.OKBtnClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
    SelectedName := ListBox.Items[ListBox.ItemIndex]
  else
    SelectedName := '';
end;

procedure TFormSelectName.CancelBtnClick(Sender: TObject);
begin
  SelectedName := '';
end;

procedure TFormSelectName.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end;
end;

procedure TFormSelectName.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if ListBox.ItemIndex <> -1 then
    begin
      ModalResult := mrOK;
      Close;
    end;
  end;
end;

end.
