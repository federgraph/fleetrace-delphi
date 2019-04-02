unit FrmConnectionProps;

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
  Vcl.StdCtrls,
  RiggVar.App.Config,
  RiggVar.BO.IniImage;

type
  TFormConnectionProps = class(TForm)
    edHost: TEdit;
    edPort: TEdit;
    HostLabel: TLabel;
    PortLabel: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure LoadModel(Section: TConfigSection);
    procedure SaveModel(Section: TConfigSection);
  end;

function EditConnectionProps(Section: TConfigSection): Boolean;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

var
  FormConnectionProps: TFormConnectionProps;

function EditConnectionProps(Section: TConfigSection): Boolean;
begin
  result := False;
  if not Assigned(FormConnectionProps) then
    FormConnectionProps := TFormConnectionProps.Create(Application);

  FormConnectionProps.LoadModel(Section);
  if FormConnectionProps.ShowModal = mrOK then
  begin
    FormConnectionProps.SaveModel(Section);
    result := True;
  end;
end;


{ TFormConnectionProps }

procedure TFormConnectionProps.LoadModel(Section: TConfigSection);
begin
  case Section of
    csBridge:
    begin
      edHost.Text := Main.IniImage.BridgeHost;
      edPort.Text := IntToStr(Main.IniImage.BridgePort);
    end;

    csOutput:
    begin
      edHost.Text := Main.IniImage.OutputHost;
      edPort.Text := IntToStr(Main.IniImage.OutputPort);
    end;
  end;
end;

procedure TFormConnectionProps.SaveModel(Section: TConfigSection);
begin
  case Section of
    csBridge:
    begin
      Main.IniImage.BridgeHost := edHost.Text;
      Main.IniImage.BridgePort := StrToIntDef(edPort.Text, Main.IniImage.BridgePort);
    end;

    csOutput:
    begin
      Main.IniImage.OutputHost := edHost.Text;
      Main.IniImage.OutputPort := StrToIntDef(edPort.Text, Main.IniImage.OutputPort);
    end;
  end;

end;

end.
