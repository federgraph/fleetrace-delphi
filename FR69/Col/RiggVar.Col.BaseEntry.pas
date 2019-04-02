unit RiggVar.Col.BaseEntry;

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

const
  SpaceChar = ' ';
  EmptyMark = '-';

type
  TOutputType = (otCSV, otHTM, otXML, otCSV_Header, otHTM_Header);

  TNewIDFunction = function: Integer of object;

  TBaseEntry = class(TPersistent)
  private
    FNewID: TNewIDFunction;
    FOutputType: TOutputType;
    procedure SLADD_CSV(aTagName, aTagValue: string);
    procedure SLADD_CSV_Header(aTagName, aTagValue: string);
    procedure SLADD_CSV_Header_LAST(aTagName, aTagValue: string);
    procedure SLADD_HTM(aTagName, aTagValue: string);
    procedure SLADD_HTM_Header(aTagName, aTagValue: string);
    procedure SLADD_XML(aTagName, aTagValue: string);
    procedure SLADD_CSV_LAST(aTagName, aTagValue: string);
    function Need_CSV_Escape(const aTagValue: string): Boolean;
  protected
    sep: string;
    procedure SLADD(aTagName, aTagValue: string);
    procedure SLADDLAST(aTagName, aTagValue: string);
    procedure GetOutput; virtual;
  public
    SO: Integer;
    sOutput: string;
    constructor Create;
    procedure GetFooter(SL: TStrings; ot: TOutputType; aName: string; XMLSection: Boolean);
    procedure GetHeader(SL: TStrings; ot: TOutputType; aName: string; XMLSection: Boolean);
    //
    function GetCommaText(SL: TStrings): string; virtual;
    procedure SetCommaText(SL: TStrings); virtual;
    //
    function GetCSV: string; virtual;
    function GetHTM: string; virtual;
    function GetXML(aTagName: string): string; virtual;
    //
    function GetCSV_Header: string; virtual;
    function GetHTM_Header: string; virtual;
    property OutputType: TOutputType read FOutputType write FOutputType;
    //
    property NewID: TNewIDFunction read FNewID write FNewID;
  end;

implementation

{ TBaseEntry }

procedure TBaseEntry.GetHeader(SL: TStrings; ot: TOutputType; aName: string; XMLSection: Boolean);
begin
  case ot of
    otCSV: SL.Add(GetCSV_Header);
    otHTM:
    begin
      //SL.Add('<H3>' + aName + '</H3>');
      SL.Add('<table border="1" cellspacing="0" cellpadding="1">');
      SL.Add('<caption>' + aName + '</caption>');
      SL.Add(GetHTM_Header);
    end;
    otXML:
      if XMLSection then
        SL.Add('<' + aName + '>');
        //SL.Add('<' + aName + ' xmlns=''http://tempuri.org/' + aName + '.xsd''>');
  end;
end;

procedure TBaseEntry.GetFooter(SL: TStrings; ot: TOutputType; aName: string; XMLSection: Boolean);
begin
  case ot of
    otCSV: ;
    otHTM: SL.Add('</table>');
    otXML: if XMLSection then SL.Add('</' + aName + '>');
  end;
end;

function TBaseEntry.GetCSV_Header: string;
begin
  //virtual
  result := '';
end;

function TBaseEntry.GetHTM_Header: string;
begin
  FOutputType := otHTM_Header;
  sOutput := '<tr align="left">';
  GetOutput;
  sOutput := sOutput + '</tr>';
  result := sOutput;
end;

function TBaseEntry.GetCSV: string;
begin
  FOutputType := otCSV;
  sOutput := '';
  GetOutput;
  result := sOutput;
end;

function TBaseEntry.GetHTM: string;
begin
  FOutputType := otHTM;
  sOutput := '<tr>';
  GetOutput;
  sOutput := sOutput + '</tr>';
  result := sOutput;
end;

function TBaseEntry.GetXML(aTagName: string): string;
begin
  FOutputType := otXML;
  sOutput := '';
  GetOutput;
  result := '<' + aTagName + ' ' + sOutput + '/>';
end;

constructor TBaseEntry.Create;
begin
  sep := ';';
end;

function TBaseEntry.GetCommaText(SL: TStrings): string;
begin
  //virtual
  result := '';
end;

procedure TBaseEntry.SetCommaText(SL: TStrings);
begin
  //virtual
end;

procedure TBaseEntry.SLADD_CSV_Header(aTagName, aTagValue: string);
begin
  sOutput := sOutput + aTagName + sep;
end;

procedure TBaseEntry.SLADD_CSV_Header_LAST(aTagName, aTagValue: string);
begin
  sOutput := sOutput + aTagName;
end;

procedure TBaseEntry.SLADD_CSV(aTagName, aTagValue: string);
begin
  if aTagValue = '' then
    sOutput := sOutput + aTagValue + sep
  else if Need_CSV_Escape(aTagValue) then
    sOutput := sOutput + '"' + aTagValue + '"' + sep
  else
    sOutput := sOutput + aTagValue + sep;
end;

function TBaseEntry.Need_CSV_Escape(const aTagValue: string): Boolean;
begin
  result :=  (Pos(' ', aTagValue) > 0) or
    ((sep = ',') and (Pos(sep, aTagValue) > 0));
end;

procedure TBaseEntry.SLADD_CSV_LAST(aTagName, aTagValue: string);
begin
  if aTagValue = '' then
    sOutput := sOutput + aTagValue
  else if Need_CSV_Escape(aTagValue) then
    sOutput := sOutput + '"' + aTagValue + '"'
  else
    sOutput := sOutput + aTagValue;
end;

procedure TBaseEntry.SLADD_HTM_Header(aTagName, aTagValue: string);
begin
  if aTagValue = '' then
    aTagValue := '&nbsp';
  sOutput := sOutput + '<th>' + aTagName + '</th>';
end;

procedure TBaseEntry.SLADD_HTM(aTagName, aTagValue: string);
begin
  if aTagValue = '' then
    aTagValue := '&nbsp';
  sOutput := sOutput + '<td>' + aTagValue + '</td>';
end;

procedure TBaseEntry.SLADD_XML(aTagName, aTagValue: string);
begin
  sOutput := sOutput + aTagName + '="' + aTagValue + '" ';
end;

procedure TBaseEntry.SLADD(aTagName, aTagValue: string);
begin
  case FOutputType of
    otCSV: SLADD_CSV(aTagName, aTagValue);
    otHTM: SLADD_HTM(aTagName, aTagValue);
    otXML: SLADD_XML(aTagName, aTagValue);
    otCSV_Header: SLADD_CSV_Header(aTagName, aTagValue);
    otHTM_Header: SLADD_HTM_Header(aTagName, aTagValue);
  end;
end;

procedure TBaseEntry.SLADDLAST(aTagName, aTagValue: string);
begin
  case FOutputType of
    otCSV: SLADD_CSV_LAST(aTagName, aTagValue);
    otHTM: SLADD_HTM(aTagName, aTagValue);
    otXML: SLADD_XML(aTagName, aTagValue);
    otCSV_Header: SLADD_CSV_Header_LAST(aTagName, aTagValue);
    otHTM_Header: SLADD_HTM_Header(aTagName, aTagValue);
  end;
end;

procedure TBaseEntry.GetOutput;
begin
  //virtual
  //overrides should make calls to SLADD here
end;

end.
