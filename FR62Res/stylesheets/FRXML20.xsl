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
