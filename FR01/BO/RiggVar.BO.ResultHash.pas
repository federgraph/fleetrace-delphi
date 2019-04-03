unit RiggVar.BO.ResultHash;

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
  IdGlobal;

type
  THashAlgo = (
    haMD5,
    haSHA1
    );

  TResultHashBase = class
  private
    CL: TStringList;
    CompareMsg: string;
    function GetMD5: string;
    function GetSHA1: string;
  protected
    UseSHA1: Boolean;
    Algo: THashAlgo;
    TestMsg: string;
    RealMsg: string;
    function InsertDashes(s: string): string;
    function GetTestHash: string; virtual; abstract;
    function GetRealHash: string; virtual; abstract;
    function GetMessage: string;
    function GetMsgList(SL: TStrings): string;
    function GetMemoString: string;
    function CheckMsgList(SL: TStrings): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTextHash(data: string): string; virtual; abstract;
    procedure WriteCompareList(ML: TStrings);
    property MemoString: string read GetMemoString;
    property MD5: string read GetMD5;
    property SHA1: string read GetSHA1;
  end;

  TResultHash01 = class(TResultHashBase)
  private
    function GetHashMD5(data: TIdBytes): string;
  protected
    function GetTestHash: string; override;
    function GetRealHash: string; override;
  public
    function GetTextHash(data: string): string; override;
    function GetHash(HashAlgo: THashAlgo; data: TIdBytes): string; virtual;
  end;

  TResultHash02 = class(TResultHash01)
  private
    function GetHashSHA1(data: TIdBytes): string;
  public
    function GetHash(HashAlgo: THashAlgo; data: TIdBytes): string; override;
  end;


implementation

uses
  RiggVar.App.Main,
  RiggVar.BO.Def,
  RiggVar.Col.Event,
  RiggVar.BO.ExcelImport,
  IdHash,
  IdHashMessageDigest;

{ TResultHashBase }

function TResultHashBase.GetMessage: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    GetMsgList(SL);
    result := StringReplace(SL.Text, #13#10, '', [rfReplaceAll]);
  finally
    SL.Free;
  end;
end;

function TResultHashBase.GetMsgList(SL: TStrings): string;
var
  cl: TEventRowCollection;
  cr: TEventRowCollectionItem;
  i: Integer;
  s: string;
  v0, v1: Integer;
begin
  try
    cl := BO.EventNode.EventRowCollection;
    for i := 0 to cl.Count - 1 do
    begin
      cr := cl.Items[i];
      if (cr.PLZ >= 0) then
      begin
        cr := cl.Items[cr.PLZ];
        v0 := cr.Bib;
        v1 := cr.Race[0].CTimeAccess;
      end
      else
      begin
        //should never come into this branch
        v0 := i;
        v1 := 0;
      end;
      s := Format('%.3d=%.5d', [v0, v1]);
      SL.Add(s);
    end;
    result := SL.Text;
  except
    on e: Exception do
    begin
      result := e.Message;
      Exit;
    end;
  end;
end;

function TResultHashBase.GetSHA1: string;
begin
  Algo := haSHA1;
  result := GetRealHash;
end;

function TResultHashBase.CheckMsgList(SL: TStrings): Boolean;
var
  ML: TStrings;
  i: Integer;
  b1, b2, p1, p2: Integer;
begin
  result := true;
  CompareMsg := '';
  ML := BO.ExcelImporter.CompareList;
  if ML.Count = 0 then
  begin
    result := false;
    CompareMsg := 'Original CompareList (ML) is empty.';
    Exit;
  end
  else if ML.Count <> BO.BOParams.StartlistCount then
  begin
    result := false;
    CompareMsg := 'CompareList.Count does not match StartList.Count.';
    Exit;
  end
  else if ML.Count = SL.Count then
  begin
    for i := 0 to SL.Count-1 do
    begin
      b1 := StrToIntDef(SL.Names[i], -1);
      b2 := StrToIntDef(ML.Names[i], -1);
      if b1 = b2 then
      begin
        p1 := StrToIntDef(SL.ValueFromIndex[i], -1);
        p2 := StrToIntDef(ML.ValueFromIndex[i], -1);
        if p1 <> p2 then
        begin
          result := false;
          SL[i] := Format('//%d: %s : %s', [i, ML[i], SL[i]]);
          CompareMsg := Format('Points mismatch at Index %d', [i]);
          //break;
        end;
      end
      else
      begin
        result := false;
          SL[i] := Format('//%d: %s : %s', [i, ML[i], SL[i]]);
        CompareMsg := Format('Bib mismatch at Index %d', [i]);
        //break;
      end;
    end;
  end;
end;

constructor TResultHashBase.Create;
begin
  CL := TStringList.Create;
end;

destructor TResultHashBase.Destroy;
begin
  CL.Free;
  inherited;
end;

function TResultHashBase.GetMD5: string;
begin
  Algo := haMD5;
  result := GetRealHash;
 end;

function TResultHashBase.GetMemoString: string;
var
  m0, m1: string;
  s0, s1: string;
  SL: TStringList;
  i: Integer;
  b: boolean;
begin
  SL := TStringList.Create;
  try
    if UseSHA1 then
    begin
      Algo := haSHA1;
      s0 := GetTestHash;
      s1 := GetRealHash;
    end;
    Algo := haMD5;
    m0 := GetTestHash;
    m1 := GetRealHash;

    SL.Add(Format('<ResultHash algo="MD5" value="%s" />', [m1]));
    SL.Add('');
    SL.Add('Test-Msg: ' + TestMsg);
    SL.Add('Real-Msg: MsgList, w/o line-breaks');
    SL.Add('');
    SL.Add('Test-MD5 : ' + m0);
    SL.Add('Real-MD5 : ' + m1);
    if USESHA1 then
    begin
      SL.Add('');
      SL.Add('Test-SHA1: ' + s0);
      SL.Add('Real-SHA1: ' + s1);
    end;
    CL.Clear;
    GetMsgList(CL);
    SL.Add('');
    b := CheckMsgList(CL);
    if b then
      SL.Add('CompareList-Check ok')
    else
      SL.Add('CompareList-Check failed - ' + CompareMsg);

    for i := 0 to CL.Count-1 do
      SL.Add(CL[i]);

    result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TResultHashBase.InsertDashes(s: string): string;
var
  i: Integer;
begin
  if Length(s) > 2 then
  begin
    result := Copy(s, 0, 2);
    for i := 3 to Length(s) do
    begin
      if i mod 2 = 1 then
        result := result + '-';
      result := result + s[i];
    end;
  end;
end;

procedure TResultHashBase.WriteCompareList(ML: TStrings);
begin
  //ML.Add('CompareList.Begin');
  ML.Add(CompareListStartToken);
  GetMsgList(ML);
  ML.Add(CompareListEndToken);
  //ML.Add('CompareList.End');
end;

{ TResultHashMD5 }

function TResultHash01.GetTestHash: string;
begin
  TestMsg := 'abc';
  result := GetHash(Algo, TIdBytes(TEncoding.UTF8.GetBytes(TestMsg)));
end;

function TResultHash01.GetTextHash(data: string): string;
begin
  result := GetHash(haMD5, TIdBytes(TEncoding.UTF8.GetBytes(data)));
end;

function TResultHash01.GetRealHash: string;
begin
  RealMsg := GetMessage;
  result := GetHash(Algo, TIdBytes(TEncoding.UTF8.GetBytes(RealMsg)));
end;

function TResultHash01.GetHashMD5(data: TIdBytes): string;
var
  ha: TIdHash;
begin
  result := '';
  ha := TIdHashMessageDigest5.Create;
  try
    result := ha.HashBytesAsHex(data);
    result := InsertDashes(result);
  finally
    ha.Free;
  end;
end;

function TResultHash01.GetHash(HashAlgo: THashAlgo; data: TIdBytes): string;
begin
  case HashAlgo of
    haMD5: result := GetHashMD5(data);
    else result := '';
  end
end;

function TResultHash02.GetHash(HashAlgo: THashAlgo; data: TIdBytes): string;
begin
  case HashAlgo of
    haMD5: result := GetHashMD5(data);
    haSHA1: result := GetHashSHA1(data);
    else result := '';
  end
end;

function TResultHash02.GetHashSHA1(data: TIdBytes): string;
//var
//  ha: TIdHash;
begin
  result := 'not implemented';
//  ha := TIdHashSHA1.Create;
//  try
//    result := ha.HashBytesAsHex(data);
//    result := InsertDashes(result);
//  finally
//    ha.Free;
//  end;
end;

end.
