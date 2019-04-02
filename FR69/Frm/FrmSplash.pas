unit FrmSplash;

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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls;

type
  TFormSplash = class(TForm)
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  end;

var
  FormSplash: TFormSplash;

implementation

{$R *.dfm}

procedure TFormSplash.FormCreate(Sender: TObject);
var
  ML: TStrings;
begin
  Memo.Align := alClient;
  Memo.Clear;
  ML := Memo.Lines;

  //this is shown centered in the memo
  ML.Add('');
  ML.Add('Application is loading...');
  ML.Add('Please wait ca. 6 seconds,');
  ML.Add('while Application is checking ports.');
end;

end.
