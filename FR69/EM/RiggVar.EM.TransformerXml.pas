unit RiggVar.EM.TransformerXml;

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
  RiggVar.EM.XmlVerySimple,
  RiggVar.EM.Transformer;

type
  TSimpleTransformer = class(TEventDataTransformer)
  private
    Xml: TSimpleXml;
    SL: TStrings;
    TL: TStrings;
    FieldCount: Integer;
    procedure ExtractNameList(xe: TXmlNode);
    procedure ExtractParams(xe: TXmlNode);
    procedure ExtractProperties(xe: TXmlNode);
    procedure ExtractStartList(xe: TXmlNode);
    procedure ExtractFleetList(xe: TXmlNode);
    procedure ExtractFinishList(xe: TXmlNode);
    procedure ExtractMsgList(xe: TXmlNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure TransformSL(SLXML, ATL, ASL: TStrings); override;
  end;

implementation

{ TSimpleTransformer }

constructor TSimpleTransformer.Create;
begin
  Xml := TSimpleXml.Create;
  Xml.Header.Attribute['encoding'] := 'utf-8';
  TL := TStringList.Create;
  TL.Delimiter := ';';
end;

destructor TSimpleTransformer.Destroy;
begin
  Xml.Free;
  TL.Free;
  inherited;
end;

procedure TSimpleTransformer.TransformSL(SLXML, ATL, ASL: TStrings);
begin
  FOK := false;
  if SLXML.Count > 0 then
  try
    SL := ASL;
    TL.Clear;
    SL.Clear;
    Xml.Text := SLXML.Text;

    ExtractParams(Xml.Root);
    ExtractProperties(Xml.Root);
    ExtractNameList(Xml.Root);
    ExtractStartList(Xml.Root);
    ExtractFleetList(Xml.Root);
    ExtractFinishList(Xml.Root);
    ExtractMsgList(Xml.Root);

    FOK := true;
  except
    on E: Exception do
    begin
      FOK := false;
      FError := E.Message
    end;
  end
  else
  begin
    FOK := false;
    FError := 'SL is empty';
  end;
end;

(* === FRXML20.xsl ===

<?xml version="1.0" encoding="UTF-8"?>
<!-- transforms FR data file from xml to txt -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="text" encoding="iso-8859-1"/>
	<xsl:template match="/">
		<xsl:apply-templates select="FR/Properties"/>
		<xsl:call-template name="Entries"/>
		<xsl:call-template name="StartList"/>
		<xsl:call-template name="FleetList"/>
		<xsl:call-template name="FinishList"/>
		<xsl:call-template name="TimeList"/>
		<xsl:call-template name="MsgList"/>
		<xsl:call-template name="ErrorList"/>
		<xsl:call-template name="InputMode"/>
	</xsl:template>

	<xsl:template match="Properties">
        <!-- #Params-->
		<xsl:text>#Params</xsl:text>
		<xsl:text>&#xA;&#xA;</xsl:text>
		<xsl:text>DP.StartlistCount=</xsl:text>
		<xsl:value-of select="@StartlistCount"/>
		<xsl:text>&#xA;</xsl:text>
		<xsl:text>DP.ITCount=</xsl:text>
		<xsl:value-of select="@ITCount"/>
		<xsl:text>&#xA;</xsl:text>
		<xsl:text>DP.RaceCount=</xsl:text>
		<xsl:value-of select="@RaceCount"/>
		<xsl:text>&#xA;&#xA;</xsl:text>
        <!-- #Event Properties-->
		<xsl:text>#Event Properties</xsl:text>
		<xsl:text>&#xA;&#xA;</xsl:text>
        <!-- DivisionName property is given out-of-order-->
		<xsl:text>EP.DivisionName=</xsl:text>
		<xsl:value-of select="@DivisionName"/>
		<xsl:text>&#xA;</xsl:text>
        <!-- all others are given as K/V child elements-->
		<xsl:for-each select="EP">
			<xsl:text>EP.</xsl:text>
			<xsl:value-of select="@K"/>
			<xsl:text>=</xsl:text>
			<xsl:value-of select="@V"/>
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="NameList">
	    <xsl:variable name="o" select="/FR/NameList/NL"/>
	    <xsl:if test="count($o)">
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>NameList.Begin&#xA;</xsl:text>
			<xsl:for-each select="$o">
				<xsl:value-of select="." />
				<xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
			<xsl:text>NameList.End&#xA;</xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template name="StartList">
		<xsl:text>&#xA;</xsl:text>
		<xsl:text>StartList.Begin&#xA;</xsl:text>
		<xsl:for-each select="/FR/StartList/Pos">
			<xsl:if test="position()=1">
				<xsl:text>Pos;SNR;Bib&#xA;</xsl:text>
				<xsl:text></xsl:text>
			</xsl:if>
			<!--<xsl:value-of select="@oID"/>-->
			<xsl:value-of select="position()"/>
			<xsl:text>;</xsl:text>
			<xsl:value-of select="@SNR" />
			<xsl:text>;</xsl:text>
			<xsl:value-of select="@Bib" />
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
		<xsl:text>StartList.End&#xA;</xsl:text>
	</xsl:template>

	<xsl:template name="FleetList">
	    <xsl:variable name="o" select="/FR/FleetList/FL"/>
	    <xsl:if test="count($o)">
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>FleetList.Begin&#xA;</xsl:text>
			<xsl:for-each select="$o">
				<xsl:value-of select="." />
				<xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
			<xsl:text>FleetList.End&#xA;</xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template name="FinishList">
	    <xsl:variable name="o" select="/FR/FinishList/FL"/>
	    <xsl:if test="count($o)">
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>FinishList.Begin&#xA;</xsl:text>
			<xsl:for-each select="$o">
				<xsl:value-of select="." />
				<xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
			<xsl:text>FinishList.End&#xA;</xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template name="TimeList">
		<xsl:for-each select="/FR/TimeList">
		    <xsl:variable name="r" select="@RaceID"/>
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>TimeList.Begin.</xsl:text>
			<xsl:value-of select="$r"/>
			<xsl:text>&#xA;</xsl:text>
			<xsl:for-each select="TL">
				<xsl:value-of select="." />
				<xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
			<xsl:text>TimeList.End</xsl:text>
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="MsgList">
	    <xsl:variable name="o" select="/FR/MsgList/ML"/>
	    <xsl:if test="count($o)">
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>#MsgList&#xA;&#xA;</xsl:text>
		</xsl:if>
		<xsl:for-each select="$o">
			<xsl:value-of select="." />
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="ErrorList">
	    <xsl:variable name="o" select="/FR/ErrorList/EL"/>
	    <xsl:if test="count($o)">
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>#ErrorList&#xA;&#xA;</xsl:text>
		</xsl:if>
		<xsl:for-each select="$o">
			<xsl:value-of select="." />
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="InputMode">
		<xsl:text>&#xA;</xsl:text>
		<xsl:choose>
			<xsl:when test="/FR/Properties/@InputMode">
				<xsl:text>EP.IM = </xsl:text>
				<xsl:value-of select="/FR/Properties/@InputMode"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>EP.IM = Strict</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>&#xA;</xsl:text>
	</xsl:template>

	<xsl:template name="Entries">
		<xsl:text>&#xA;</xsl:text>
		<xsl:text>NameList.Begin&#xA;</xsl:text>
		<xsl:for-each select="/FR/Entries/SNR">
			<xsl:if test="position()=1">
				<xsl:for-each select="@*">
					<xsl:if test="position()>1">
						<xsl:text>;</xsl:text>
					</xsl:if>
					<xsl:variable name="t" select="name()='oID'"/>
					<xsl:choose>
						<xsl:when test="$t">
							<xsl:text>SNR</xsl:text>
						</xsl:when>
						<xsl:otherwise>
							<xsl:value-of select="name()" />
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each>
				<xsl:text>&#xA;</xsl:text>
			</xsl:if>
			<xsl:for-each select="@*">
			    <xsl:if test="position()>1">
					<xsl:text>;</xsl:text>
			    </xsl:if>
				<xsl:value-of select="." />
			</xsl:for-each>
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
		<xsl:text>NameList.End&#xA;</xsl:text>
	</xsl:template>

<!--
	<xsl:template name="RaceDataList">
		<xsl:for-each select="/FR/W">
		    <xsl:variable name="r" select="@oID"/>
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>#W</xsl:text>
			<xsl:value-of select="$r"/>
			<xsl:text>&#xA;&#xA;</xsl:text>
			<xsl:variable name="dn" select="/FR/Properties/@DivisionName"/>
			<xsl:for-each select="Bib">
				<xsl:text>FR.</xsl:text>
				<xsl:value-of select="$dn"/>
				<xsl:text>.W</xsl:text>
				<xsl:value-of select="$r"/>
				<xsl:text>.Bib</xsl:text>
				<xsl:value-of select="@oID"/>
				<xsl:if test="@Rank">
					<xsl:text>.Rank=</xsl:text>
					<xsl:value-of select="@Rank"/>
				</xsl:if>
				<xsl:if test="@QU">
					<xsl:text>.QU=</xsl:text>
					<xsl:value-of select="@QU"/>
				</xsl:if>
				<xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="FinishList">
		<xsl:text>&#xA;</xsl:text>
		<xsl:text>FinishList.Begin&#xA;</xsl:text>
		<xsl:text>Bib</xsl:text>
		<xsl:for-each select="/FR/W">
			<xsl:text>;R</xsl:text>
			<xsl:value-of select="position()"/>
		</xsl:for-each>
		<xsl:text>&#xA;</xsl:text>
		<xsl:for-each select="/FR/Startlist/Pos">
		   <xsl:variable name="b" select="@oID"/>
           <xsl:value-of select="$b"/>
	        <xsl:for-each select="/FR/W/Bib[@oID=$b]">
				<xsl:text>;</xsl:text>
				<xsl:value-of select="@Rank"/>
				<xsl:if test="not(@Rank)">
					<xsl:text>0</xsl:text>
				</xsl:if>
			</xsl:for-each>
			<xsl:text>&#xA;</xsl:text>

		</xsl:for-each>
		<xsl:text>FinishList.End&#xA;</xsl:text>
	</xsl:template>

	<xsl:template name="QUList">
		<xsl:for-each select="/FR/W">
		    <xsl:variable name="r" select="@oID"/>
			<xsl:text>&#xA;</xsl:text>
			<xsl:text>#W</xsl:text>
			<xsl:value-of select="$r"/>
			<xsl:text>&#xA;&#xA;</xsl:text>
			<xsl:variable name="dn" select="/FR/EP/@DivisionName"/>
			<xsl:for-each select="Bib[@QU]">
				<xsl:text>FR.</xsl:text>
				<xsl:value-of select="$dn"/>
				<xsl:text>.W</xsl:text>
				<xsl:value-of select="$r"/>
				<xsl:text>.Bib</xsl:text>
				<xsl:value-of select="@oID"/>
				<xsl:text>.QU=</xsl:text>
				<xsl:value-of select="@QU"/>
				<xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
-->

</xsl:stylesheet>
*)

(* === NameTest.xml ===

<?xml version="1.0"?>
<FR>
  <Properties StartlistCount="8" ITCount="0" RaceCount="2" DivisionName="420" InputMode="Strict">
    <EP K="Name" V="NameTest"/>
    <EP K="ScoringSystem" V="Low Point System"/>
    <EP K="Throwouts" V="0"/>
    <EP K="DivisionName" V="420"/>
    <EP K="InputMode" V="Strict"/>
    <EP K="RaceLayout" V="Finish"/>
    <EP K="NameSchema" V="NX"/>
    <EP K="FieldMap" V="FN,_,LN"/>
    <EP K="FieldCaptions" V="FN,LN,SN,NAT,FN2,LN2,CPos"/>
    <EP K="FieldCount" V="7"/>
    <EP K="NameFieldCount" V="2"/>
    <EP K="NameFieldOrder" V="04"/>
    <EP K="UseFleets" V="False"/>
    <EP K="TargetFleetSize" V="8"/>
    <EP K="FirstFinalRace" V="20"/>
    <EP K="IsTimed" V="False"/>
    <EP K="UseCompactFormat" V="True"/>
  </Properties>
  <Entries>
    <SNR oID="1000" N1="FN1" N2="LN1" N3="SN1" N4="GER" N5="FN2-1" N6="LN2-1" N7="x"/>
    <SNR oID="1001" N1="FN2" N2="LN2" N3="SN2" N4="ITA" N5="FN2-2" N6="LN2-2" N7="y"/>
    <SNR oID="1002" N1="FN3" N2="LN3" N3="SN3" N4="FRA" N5="FN2-3" N6="LN2-3" N7="z"/>
  </Entries>
  <StartList>
    <Pos oID="1" Bib="1" SNR="1000"/>
    <Pos oID="2" Bib="2" SNR="1001"/>
    <Pos oID="3" Bib="3" SNR="1002"/>
    <Pos oID="4" Bib="4" SNR="1003"/>
    <Pos oID="5" Bib="5" SNR="1004"/>
    <Pos oID="6" Bib="6" SNR="1005"/>
    <Pos oID="7" Bib="7" SNR="1006"/>
    <Pos oID="8" Bib="8" SNR="1007"/>
  </StartList>
  <FinishList>
    <FL>SNR;Bib;R1;R2</FL>
    <FL>1000;1;2;3</FL>
    <FL>1001;2;7;4</FL>
    <FL>1002;3;5;8</FL>
    <FL>1003;4;1;7</FL>
    <FL>1004;5;6;5</FL>
    <FL>1005;6;8;6</FL>
    <FL>1006;7;4;2</FL>
    <FL>1007;8;3;1</FL>
  </FinishList>
  <MsgList/>
</FR>

*)

(* === FleetTest.xml ===

<?xml version="1.0"?>
<FR>
  <Properties StartlistCount="8" ITCount="0" RaceCount="2" DivisionName="420" InputMode="Strict">
    <EP K="Name" V="FleetTest"/>
    <EP K="ScoringSystem" V="Low Point System"/>
    <EP K="Throwouts" V="0"/>
    <EP K="DivisionName" V="420"/>
    <EP K="InputMode" V="Strict"/>
    <EP K="RaceLayout" V="Finish"/>
    <EP K="NameSchema" V=""/>
    <EP K="FieldMap" V="SN"/>
    <EP K="FieldCaptions" V=""/>
    <EP K="FieldCount" V="6"/>
    <EP K="NameFieldCount" V="2"/>
    <EP K="NameFieldOrder" V="041256"/>
    <EP K="ColorMode" V="Fleet"/>
    <EP K="UseFleets" V="True"/>
    <EP K="TargetFleetSize" V="8"/>
    <EP K="FirstFinalRace" V="20"/>
    <EP K="UseCompactFormat" V="True"/>
  </Properties>
  <Entries/>
  <StartList>
    <Pos oID="1" Bib="1" SNR="1000"/>
    <Pos oID="2" Bib="2" SNR="1001"/>
    <Pos oID="3" Bib="3" SNR="1002"/>
    <Pos oID="4" Bib="4" SNR="1003"/>
    <Pos oID="5" Bib="5" SNR="1004"/>
    <Pos oID="6" Bib="6" SNR="1005"/>
    <Pos oID="7" Bib="7" SNR="1006"/>
    <Pos oID="8" Bib="8" SNR="1007"/>
  </StartList>
  <FleetList>
    <FL>SNR;Bib;R1;R2</FL>
    <FL>1000;1;1;2</FL>
    <FL>1001;2;1;2</FL>
    <FL>1002;3;1;2</FL>
    <FL>1003;4;1;2</FL>
    <FL>1004;5;2;1</FL>
    <FL>1005;6;2;1</FL>
    <FL>1006;7;2;1</FL>
    <FL>1007;8;2;1</FL>
  </FleetList>
  <FinishList>
    <FL>SNR;Bib;R1;R2</FL>
    <FL>1000;1;1;1</FL>
    <FL>1001;2;3;2</FL>
    <FL>1002;3;2;4</FL>
    <FL>1003;4;4;3</FL>
    <FL>1004;5;4;4</FL>
    <FL>1005;6;3;3</FL>
    <FL>1006;7;2;2</FL>
    <FL>1007;8;1;1</FL>
  </FinishList>
  <MsgList/>
</FR>

*)

(* === NameTest.txt ===

#Params

DP.StartlistCount = 8
DP.ITCount = 0
DP.RaceCount = 2

#Event Properties

EP.Name = NameTest
EP.ScoringSystem = Low Point System
EP.Throwouts = 0
EP.DivisionName = 420
EP.InputMode = Strict
EP.RaceLayout = Finish
EP.NameSchema = NX
EP.FieldMap = FN,_,LN
EP.FieldCaptions = FN,LN,SN,NAT,FN2,LN2,CPos
EP.FieldCount = 7
EP.NameFieldCount = 2
EP.NameFieldOrder = 04
EP.UseFleets = False
EP.TargetFleetSize = 8
EP.FirstFinalRace = 20
EP.IsTimed = False
EP.UseCompactFormat = True

NameList.Begin
SNR;N1;N2;N3;N4;N5;N6;N7
1000;FN1;LN1;SN1;GER;FN2-1;LN2-1;x
1001;FN2;LN2;SN2;ITA;FN2-2;LN2-2;y
1002;FN3;LN3;SN3;FRA;FN2-3;LN2-3;z
NameList.End

StartList.Begin
Pos;SNR;Bib
1;1000;1
2;1001;2
3;1002;3
4;1003;4
5;1004;5
6;1005;6
7;1006;7
8;1007;8
StartList.End

FinishList.Begin
SNR;Bib;R1;R2
1000;1;2;3
1001;2;7;4
1002;3;5;8
1003;4;1;7
1004;5;6;5
1005;6;8;6
1006;7;4;2
1007;8;3;1
FinishList.End

#W1


#W2


EP.IM = Strict

*)

procedure TSimpleTransformer.ExtractParams(xe: TXmlNode);
var
  Properties: TXmlNode;
  K, V: string;
begin
  Properties := xe.Find('Properties');
  if Assigned(Properties) then
  begin
  SL.Add('#Params');
  SL.Add('');

  K := 'StartlistCount';
  V := Properties.Attribute[K];
  SL.Add(Format('DP.%s=%s', [K, V]));

  K := 'RaceCount';
  V := Properties.Attribute[K];
  SL.Add(Format('DP.%s=%s', [K, V]));

  K := 'ITCount';
  V := Properties.Attribute[K];
  SL.Add(Format('DP.%s=%s', [K, V]));
end;
end;

procedure TSimpleTransformer.ExtractProperties(xe: TXmlNode);
var
  EPs: TXmlNodeList;
  Properties, EP: TXmlNode;
  K, V: string;
begin
  Properties := xe.Find('Properties');
  if Assigned(Properties)  then
  begin
    SL.Add('');
    SL.Add('#Event Properties');
    SL.Add('');
    EPs := Properties.FindNodes('EP');
    for EP in EPs do
    begin
      K := EP.Attribute['K'];
      V := EP.Attribute['V'];
      SL.Add(Format('EP.%s=%s', [K, V]));
      if K = 'FieldCount' then
      begin
        FieldCount := StrToIntDef(V, 0);
      end;
    end;
    EPs.Free;
  end;
end;

procedure TSimpleTransformer.ExtractNameList(xe: TXmlNode);
var
  Entries: TXmlNode;
  Nodes: TXmlNodeList;
  SNR: TXmlNode;
  K, V: string;
  oID: Integer;
  i: Integer;
begin
(*
   FieldCount = 7

   <Entries>
    <SNR oID="1000" N1="FN1" N2="LN1" N3="SN1" N4="GER" N5="FN2-1" N6="LN2-1" N7="x"/>
    <SNR oID="1001" N1="FN2" N2="LN2" N3="SN2" N4="ITA" N5="FN2-2" N6="LN2-2" N7="y"/>
    <SNR oID="1002" N1="FN3" N2="LN3" N3="SN3" N4="FRA" N5="FN2-3" N6="LN2-3" N7="z"/>
  </Entries>
*)

  Entries := xe.Find('Entries');
  if Assigned(Entries) then
  begin
    Nodes := Entries.FindNodes('SNR');
    if Nodes.Count > 0 then
    begin
      SL.Add('');
      SL.Add('NameList.Begin');

      //add Header row for NameList table
      TL.Clear;
      TL.Add('SNR');
      for i := 1 to FieldCount do
      begin
        TL.Add(Format('N%d', [i]));
      end;
      SL.Add(TL.DelimitedText);
      TL.Clear;

      //add body rows for NameList table
      for SNR in Nodes do
      begin
        oID := StrToIntDef(SNR.Attribute['oID'], 0);
        TL.Add(IntToStr(oID));
        for i := 1 to FieldCount do
        begin
          K := Format('N%d', [i]);
          V := SNR.Attribute[K];
          TL.Add(V);
        end;
        SL.Add(TL.DelimitedText);
        TL.Clear;
      end;

      SL.Add('NameList.End');
    end;
    Nodes.Free;
  end;
end;

procedure TSimpleTransformer.ExtractStartList(xe: TXmlNode);
var
  ru: TXmlNode;
  cl: TXmlNodeList;
  cr: TXmlNode;
  av: Integer;
begin
  ru := xe.Find('StartList');
  if Assigned(ru) then
  begin
    cl := ru.FindNodes('Pos');
    if cl.Count > 0 then
    begin
      SL.Add('');
      SL.Add('StartList.Begin');
      SL.Add('Pos;SNR;Bib');
      TL.Clear;
      for cr in cl do
      begin
        av := StrToIntDef(cr.Attribute['oID'], 0);
        TL.Add(IntToStr(av));
        av := StrToIntDef(cr.Attribute['SNR'], 0);
        TL.Add(IntToStr(av));
        av := StrToIntDef(cr.Attribute['Bib'], 0);
        TL.Add(IntToStr(av));
        SL.Add(TL.DelimitedText);
        TL.Clear;
      end;
      SL.Add('StartList.End');
      TL.Clear;
    end;
    cl.Free;
  end;
end;

procedure TSimpleTransformer.ExtractFleetList(xe: TXmlNode);
var
  ru: TXmlNode;
  cl: TXmlNodeList;
  cr: TXmlNode;
  s: string;
begin
  ru := xe.Find('FleetList');
  if Assigned(ru) then
  begin
    cl := ru.FindNodes('FL');
    if cl.Count > 0 then
    begin
      SL.Add('');
      SL.Add('FleetList.Begin');
      for cr in cl do
begin
        s := Trim(cr.Text);
        SL.Add(s);
      end;
      SL.Add('FleetList.End');
    end;
    cl.Free;
  end;
end;

procedure TSimpleTransformer.ExtractFinishList(xe: TXmlNode);
var
  ru: TXmlNode;
  cl: TXmlNodeList;
  cr: TXmlNode;
  s: string;
begin
  ru := xe.Find('FinishList');
  if Assigned(ru) then
  begin
    cl := ru.FindNodes('FL');
    if cl.Count > 0 then
    begin
      SL.Add('');
      SL.Add('FinishList.Begin');
      for cr in cl do
begin
        s := Trim(cr.Text);
        SL.Add(s);
      end;
      SL.Add('FinishList.End');
    end;
    cl.Free;
  end;
end;

procedure TSimpleTransformer.ExtractMsgList(xe: TXmlNode);
var
  ru: TXmlNode;
  cl: TXmlNodeList;
  cr: TXmlNode;
  s: string;
begin
  ru := xe.Find('MsgList');
  if Assigned(ru) then
  begin
    cl := ru.FindNodes('ML');
    if cl.Count > 0 then
    begin
      SL.Add('');
      SL.Add('#QU Messages');
      for cr in cl do
      begin
        s := Trim(cr.Text);
        SL.Add(s);
      end;
    end;
    cl.Free;
  end;
end;

end.
