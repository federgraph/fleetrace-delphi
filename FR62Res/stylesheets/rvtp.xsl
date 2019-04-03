<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html"/>
	<xsl:template match="/">
		<xsl:variable name="legend-fn" select="'legend-data.xml'"/>
		<html>
			<head>
				<title>race-data-report</title>
				<link href="stylesheets/rvtp.css" type="text/css" rel="stylesheet" />
			</head>
			<body>
				<p><a href="index.htm">home</a></p>
				<h3>partial transform of RaceData.xml:</h3>
				<xsl:call-template name="timing-table">
				   <xsl:with-param name="current-race" select="/race-data/current-race" />
				</xsl:call-template>
			</body>
		</html>
	</xsl:template>
	<xsl:template name="timing-table">
	   <xsl:param name="current-race"/>
		<h2>Timing</h2>
		<!-- create output for single race if parameter is set,
			 all races if paramerter is 0 (default)-->
		<xsl:choose>
			<xsl:when test="$current-race &gt; 0">
				<xsl:variable name="temp-race" select="/race-data/race[@race-order=$current-race]"/>
				<xsl:if test="$temp-race/time-point">
					<xsl:apply-templates select="$temp-race" mode="timing"/>
				</xsl:if>
			</xsl:when>
			<xsl:otherwise>
				<xsl:for-each select="/race-data/race">
					<xsl:if test="time-point">
						<xsl:apply-templates select="." mode="timing"/>
					</xsl:if>
				</xsl:for-each>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="race" mode="timing">
		<h3>
			<xsl:value-of select="@race-name"/>
		</h3>
		<table border="1" cellpadding="1" cellspacing="1">
			<tr>
				<td>Pos</td>
				<xsl:for-each select="time-point[@mark-order>0]">
					<td>
						<xsl:value-of select="@mark-name"/>
					</td>
				</xsl:for-each>
			</tr>
			<xsl:variable name="r" select="@race-order"/>
			<xsl:variable name="it-count" select="count(time-point[@mark-order>0])"/>
			<xsl:for-each select="/race-data/startlist/entry">
				<tr>
					<xsl:variable name="e" select="position()"/>
					<td>
						<xsl:value-of select="$e"/>
					</td>
					<xsl:call-template name="timing-row-loop">
						<xsl:with-param name="e" select="$e"/>
						<xsl:with-param name="r" select="$r"/>
						<xsl:with-param name="i" select="1"/>
						<xsl:with-param name="mark-count" select="$it-count"/>
					</xsl:call-template>
				</tr>
			</xsl:for-each>
		</table>
	</xsl:template>
	<xsl:template name="timing-row-loop">
		<xsl:param name="e"/>
		<xsl:param name="r"/>
		<xsl:param name="i"/>
		<xsl:param name="mark-count"/>
		<xsl:variable name="test-ok">
			<xsl:if test="$i &lt;= $mark-count">
				<xsl:text>true</xsl:text>
			</xsl:if>
		</xsl:variable>
		<xsl:if test="$test-ok='true'">
			<!-- put your logic here -->
			<xsl:variable name="tp" select="/race-data/race[@race-order=$r]/time-point[@mark-order=$i]/tb[@pos=$e]"/>
			<td>
			   <xsl:variable name="vnoc" select="$tp/@noc" />
			   <xsl:variable name="vtp" select="$tp" />
			   <xsl:choose>
				   <xsl:when test="string-length($vnoc) &gt; 0" >
						<xsl:value-of select="$vnoc"/>
				   </xsl:when>
				   <xsl:otherwise>
						<xsl:text>-</xsl:text>
				   </xsl:otherwise>
			   </xsl:choose>
			   <br/>
			   <xsl:choose>
				   <xsl:when test="$vtp" >
						<xsl:value-of select="$vtp"/>
				   </xsl:when>
				   <xsl:otherwise>
						<xsl:text>-</xsl:text>
				   </xsl:otherwise>
			   </xsl:choose>
			</td>
			<!-- your logic should end here -->
			<xsl:call-template name="timing-row-loop">
				<xsl:with-param name="e" select="$e"/>
				<xsl:with-param name="r" select="$r"/>
				<xsl:with-param name="i" select="$i + 1"/>
				<xsl:with-param name="mark-count" select="$mark-count"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
