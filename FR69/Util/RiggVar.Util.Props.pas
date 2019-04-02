unit RiggVar.Util.Props;

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
  System.Classes;

type
  TProp = record
    Key: string;
    Value: string;
  end;

  TProps = class(TPersistent)
  private
    FProps: TStrings;
    function GetValue(Key: string): string;
    procedure SetValue(Key: string; const Value: string);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Value[Key: string]: string read GetValue write SetValue;
    property Props: TStrings read FProps;
    procedure GetProp(Index: Integer; var Prop: TProp);
    property Count: Integer read GetCount;
  end;

implementation

{ TProps }

procedure TProps.Assign(Source: TPersistent);
var
  o: TProps;
begin
  if Source is TProps then
  begin
    o := Source as TProps;
    FProps.Assign(o.Props);
  end
  else if Source is TStrings then
    FProps.Assign(Source)
  else
    inherited Assign(Source);
end;

constructor TProps.Create;
begin
  inherited Create;
  FProps := TStringList.Create;
end;

destructor TProps.Destroy;
begin
  FProps.Free;
  inherited;
end;

function TProps.GetCount: Integer;
begin
  result := FProps.Count;
end;

procedure TProps.GetProp(Index: Integer; var Prop: TProp);
begin
  Prop.Key := FProps.Names[Index];
  Prop.Value := FProps.ValueFromIndex[Index];
end;

function TProps.GetValue(Key: string): string;
begin
  result := FProps.Values[Key];
end;

procedure TProps.SetValue(Key: string; const Value: string);
begin
  FProps.Values[Key] := Value;
end;

end.

