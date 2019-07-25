#!/usr/bin/ksh
#--------------------------------------------------
# web.sh 
# This script makes the HTML/JAVA files for the HWRF webpage
# Use for ATLANTIC or PACIFIC storms. 
# Written by Vijay Tallapragada 
# Usage:   sh web.sh STORMSTORMID  STORMID    STARTDATE   NO_OF_CASES
# Example: sh web.sh INVEST96L       96L      2007070312      20 
# Example: sh web.sh FOUR04L         04L      2007081318      20 
# Example: sh web.sh DEAN04L         04L      2007081418      20 
#--------------------------------------------------
# get the input variables 
if [ $# -eq 4 ]
then 
   storm=$1
   id=$2
   start_date=$3
   cases=$4

   echo 'storm      = ' $storm
   echo 'id         = ' $id
   echo 'start_date = ' $start_date
   echo 'cases      = ' $cases

   lstorm=`echo ${storm} | tr '[A-Z]' '[a-z]'`
   echo 'lower case storm = ' $lstorm
else
   echo "USAGE:   sh web.sh STORM     STORMID    STARTDATE  NO_OF_CASES"
   echo "EXAMPLE: sh web.sh INVEST96L    96L     2007070312    20      " 
   echo "EXAMPLE: sh web.sh FOUR04L      04L     2007081318    20      "
   echo "EXAMPLE: sh web.sh DEAN04L      04L     2007081418    20      "  
   echo "NEED FOUR ARGUMENTS"
   echo "SCRIPT WILL EXIT" 
   exit 1
fi 
#-------------------------------------------------
# make for cases every 6 hours
case=`expr $cases \* 6 - 6`
# make for cases every 12 hours
###case=`expr $cases \* 12 - 12`
echo $case
ndate='ndate'
count=0
yyyy=`echo $start_date | cut -c1-4`
#-------------------------------------------------
cat << eor >index.html
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML>
  <HEAD>
    <TITLE>2012 Operational HWRF Runs: $storm $yyyy </TITLE>
    <!-- All colors, font sizes, etc. are set in the CSS files loaded
         by these link tags: -->
    <link rel="stylesheet" href="../../site.css"
          type="text/css">
    <link rel="stylesheet" href="../model.css"
          type="text/css">

  </HEAD>
  <BODY class="sinv sinv_HWRF_${id}${yyyy}">

    <div class="who_are_we">
    <table>
      <tr>
        <td rowspan="2">
          <A id="EMC_IMG_LINK" HREF="http://www.emc.ncep.noaa.gov">
            <IMG SRC="http://www.ncep.noaa.gov/nwscwi/nwsright.jpg">
          </A>
        </td>
        <td>
          <A id="EMC_LINK" HREF="http://www.emc.ncep.noaa.gov">
            Environmental Modeling Center
          </A>
        </td>
        <td rowspan="2">
          <A id="NCEP_IMG_LINK" HREF="http://www.ncep.noaa.gov/">
            <IMG SRC="http://www.emc.ncep.noaa.gov/gmb/STATS/gifs/ncep_80.gif">
          </A>
        </td>
      </tr><tr>
        <td>
          <A id="NCEP_HFP_LINK" HREF="http://www.emc.ncep.noaa.gov/gc_wmb/vxt/index.html">
            NCEP Hurricane Forecast Project
          </A>
        </td>
      </tr>
    </table>
    </div>

    <div class="storm_inventory">
      <h2>HWRF Runs: $storm $yyyy </h2>

      <P><A id="back_to_model" href="../../index.html">Back to HWRF Main Page</A></P>

      <!--P id="model_perf_stat">
        Model performance <A id="stat_link" HREF="stats/stats.html">STATISTICS</A>
      </P-->

      <table id="storm_inv_content">
        <!-- The contents of this table were generated automatically by
             the mc_process_storm_html function in the HWRF Graphics
             Library. -->
        <tr><th id="storm_inv_cycname_head" bgcolor = DDEFFF>
          Experiment
        </th><th colspan="1" id="storm_inv_deck_head">
          Track/Intensity Forecast
        </th><th colspan="2" id="storm_inv_text_head">
          Text Forecast
        </th><th id="storm_inv_strliso_head" colspan="3" bgcolor = DDEFFF>
          850 hPa Streamlines, Isotachs &amp; swath
        </th><th id="storm_inv_ctr_head" colspan="5">
          Large scale diagnostics (outer domain) 
        </th><th id=storm_inv_goes_head" colspan="4" bgcolor = DDEFFF>
         Simulated GOES & Microwave Images
        </th><th id=storm_inv_emcmod_head" colspan="3">
         EMC Module Plots> 
        </th><th id=storm_inv_diags_head" colspan="2" bgcolor = DDEFFF>
         Add'l Diagnostics
        </th></tr>

eor
count=0
while [[ $count -le $case ]]
do
date=`$ndate ${count} $start_date`

echo "<!-- $storm $date -->" >>index.html

echo "<tr><th class=\"storm_inv_cycname\" bgcolor = DDEFFF>${storm}.${date} </th>" >>index.html
echo "<td class=\"storm_inv_track\" onMouseover=\"this.className='storm_inv_track hover'\"onMouseout=\"this.className='storm_inv_track'\"onclick=\"this.className='storm_inv_track';" >>index.html
echo "    window.location='${storm}.${date}/${storm}.${date}.fsct.png'\">" >>index.html
echo " <a href=\"${storm}.${date}/${storm}.${date}.fsct.png\">Track/Intensity</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_atcf\" onMouseover=\"this.className='storm_inv_atcf hover'\"onMouseout=\"this.className='storm_inv_atcf'\"onclick=\"this.className='storm_inv_atcf';" >>index.html
echo "    window.location='${storm}.${date}/${lstorm}.${date}.trak.hwrf.atcfunix'\">" >>index.html
echo " <a href=\"${storm}.${date}/${lstorm}.${date}.trak.hwrf.atcfunix\">ATCF</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_diag\" onMouseover=\"this.className='storm_inv_diag hover'\"onMouseout=\"this.className='storm_inv_diag'\"onclick=\"this.className='storm_inv_diag';" >>index.html
echo "    window.location='${storm}.${date}/${storm}.${date}.txt'\">" >>index.html
echo " <a href=\"${storm}.${date}/${storm}.${date}.txt\">Diag. Text File</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_combdom\"onMouseover=\"this.className='storm_inv_combdom hover'\"onMouseout=\"this.className='storm_inv_combdom'\"onclick=\"this.className='storm_inv_combdom';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_c.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_c.html\">Combined Domain</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_movgrid\"onMouseover=\"this.className='storm_inv_movgrid hover'\"onMouseout=\"this.className='storm_inv_movgrid'\"onclick=\"this.className='storm_inv_movgrid';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_x.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_x.html\">Vertical section</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_ctr\" onMouseover=\"this.className='storm_inv_ctr hover'\"onMouseout=\"this.className='storm_inv_ctr'\"onclick=\"this.className='storm_inv_ctr';" >>index.html
echo "    window.location='${storm}.${date}/swath.${storm}.${date}.png'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/swath.${storm}.${date}.png\">Rain/wind swath</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_ns\" onMouseover=\"this.className='storm_inv_ns hover'\"onMouseout=\"this.className='storm_inv_ns'\"onclick=\"this.className='storm_inv_ns';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_dlm.html'\">" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_dlm.html\">Deep layer mean</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_ns\" onMouseover=\"this.className='storm_inv_ns hover'\"onMouseout=\"this.className='storm_inv_ns'\"onclick=\"this.className='storm_inv_ns';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_shr.html'\">" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_shr.html\">Wind shear</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_ns\" onMouseover=\"this.className='storm_inv_ns hover'\"onMouseout=\"this.className='storm_inv_ns'\"onclick=\"this.className='storm_inv_ns';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_mht.html'\">" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_mht.html\">Mid-High trough</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_ns\" onMouseover=\"this.className='storm_inv_ns hover'\"onMouseout=\"this.className='storm_inv_ns'\"onclick=\"this.className='storm_inv_ns';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_hrv.html'\">" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_hrv.html\">500mb Rel Vort</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_ns\" onMouseover=\"this.className='storm_inv_ns hover'\"onMouseout=\"this.className='storm_inv_ns'\"onclick=\"this.className='storm_inv_ns';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_slp.html'\">" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_slp.html\">Sfc pressure</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_goesir\"onMouseover=\"this.className='storm_inv_goesir hover'\"onMouseout=\"this.className='storm_inv_goesir'\"onclick=\"this.className='storm_inv_goesir';" >>index.html
echo "    window.location='${storm}.${date}/par_GOESIR.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/par_GOESIR.html\">GOES IR</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_goeswv\"onMouseover=\"this.className='storm_inv_goeswv hover'\"onMouseout=\"this.className='storm_inv_goeswv'\"onclick=\"this.className='storm_inv_goeswv';" >>index.html
echo "    window.location='${storm}.${date}/par_GOESWV.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/par_GOESWV.html\">GOES WV</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_m37\"onMouseover=\"this.className='storm_inv_m37 hover'\"onMouseout=\"this.className='storm_inv_m37'\"onclick=\"this.className='storm_inv_m37';" >>index.html
echo "    window.location='${storm}.${date}/micro_37.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/micro_37.html\">37 GHz</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_m89\"onMouseover=\"this.className='storm_inv_m89 hover'\"onMouseout=\"this.className='storm_inv_m89'\"onclick=\"this.className='storm_inv_m89';" >>index.html
echo "    window.location='${storm}.${date}/micro_89.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/micro_89.html\">89 GHz</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_vxsec\"onMouseover=\"this.className='storm_inv_vxsec hover'\"onMouseout=\"this.className='storm_inv_vxsec'\"onclick=\"this.className='storm_inv_vxsec';" >>index.html
echo "    window.location='${storm}.${date}/${storm}.${date}.ns.png'\">" >>index.html
echo " <a href=\"${storm}.${date}/${storm}.${date}.ns.png\">Vertical X-Sect.</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_sfc\"onMouseover=\"this.className='storm_inv_sfc hover'\"onMouseout=\"this.className='storm_inv_sfc'\"onclick=\"this.className='storm_inv_sfc';" >>index.html
echo "    window.location='${storm}.${date}/${storm}.${date}.sfc.png'\">" >>index.html
echo " <a href=\"${storm}.${date}/${storm}.${date}.sfc.png\">Sfc Wind Field</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_tprime\"onMouseover=\"this.className='storm_inv_tprime hover'\"onMouseout=\"this.className='storm_inv_tprime'\"onclick=\"this.className='storm_inv_tprime';" >>index.html
echo "    window.location='${storm}.${date}/${storm}.${date}.tprime.png'\">" >>index.html
echo " <a href=\"${storm}.${date}/${storm}.${date}.tprime.png\">Moisture X-Sect.</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_wstruc\"onMouseover=\"this.className='storm_inv_wstruc hover'\"onMouseout=\"this.className='storm_inv_wstruc'\"onclick=\"this.className='storm_inv_wstruc';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_diag1.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_diag1.html\">Wind Structure</A>" >>index.html
echo "</td>" >>index.html
echo "<td class=\"storm_inv_tstruc\"onMouseover=\"this.className='storm_inv_tstruc hover'\"onMouseout=\"this.className='storm_inv_tstruc'\"onclick=\"this.className='storm_inv_tstruc';" >>index.html
echo "    window.location='${storm}.${date}/hwrf_diag2.html'\" bgcolor = DDEFFF>" >>index.html
echo " <a href=\"${storm}.${date}/hwrf_diag2.html\">Thermal Structure</A>" >>index.html
echo "</td>" >>index.html
echo "</tr>" >>index.html
echo "</tr>" >>index.html
#------------------------------------------------
# for cases every 12 hours
#count=`expr $count + 12`
# for cases every 6 hours 
count=`expr $count + 6`
echo $count
done

cat << eor1 >>index.html

<!--TH COLSPAN=13>No More Runs for $storm</TH></TR-->

      </table>

    </div>

    <div class="webmasters">
      <table><tr><td>
<script language="JavaScript">
document.write(Date()+"<br>");
</script>
      <p id="maintainers">Page Maintained By
        <a href="mailto:vijay.tallapragada@noaa.gov">Vijay Tallapragada,</a>
        <a href="mailto:samuel.trahan@noaa.gov">Samuel Trahan,</a>
        and
        <a href="mailto:Chanh.Kieu@noaa.gov">Chanh Kieu</a>
      </p>

      <p id="webmaster_email">
        Send mail to
        <a href="mailto:vijay.tallapragada@noaa.gov">vijay.tallapragada@noaa.gov</a>
        with questions or comments about this web site.
      </p>

      <p id="webpage_copyright">&copy; NCEP Hurricane Forecast Project, all rights reserved. </p>
      </td></tr></table>
    </div>


  </BODY>
</HTML>
eor1

##############################################################################################
# Statistics .html File
##############################################################################################
mkdir -p stats
cp -f e*gif stats
cd stats
cat <<eor2 >stats.html
<html>
<head>
<title>HWRF Coupled Model (2012 Operational Version) Forecast Statistics</title>
</head>
<body text="#000000" link="#0000ff" vlink="#551a8b" alink="#ff0000" bgcolor="#b0eeff">
<h1><FONT COLOR="ff0000"><P ALIGN="center"> HWRF forecasts for $storm $yyyy - Model Performance Statistics</FONT> </h1>

<td align=center><a href="../index.html" title="$storm Main Page" alt="$storm Main Page" onMouseOver="self.status=&quot; "$storm Main Page" &quot;;return true">Back to $storm Main Page</a>
<br><br>

<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="stats1.html" title="Track Forecast Statistics (HWRF, GFDL and AVNO)" alt="Track Forecast Statistics (HWRF, GFDL and AVNO)" onMouseOver="self.status=&quot; "Track Forecast Statistics (HWRF, GFDL and AVNO)" &quot;;return true">Track Forecast Statistics (HWRF, GFDL and AVNO)</a>
<td align=center><a href="stats2.html" title="Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS )" alt="Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS)" onMouseOver="self.status=&quot; "Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS)" &quot;;return true">Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS)</a>
<TR></TR>
</TABLE>

<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="${lstorm}_hg.t.txt" title="Track Forecast Statistics" alt="Track Forecast Statistics" onMouseOver="self.status=&quot; "Track Forecast Statistics" &quot;;return true">Track Forecast Statistics (Text File)</a>
<center>
<IMG SRC="${lstorm}_hg.t.png" >
<font size=3 >
<TR></TR>
</TABLE>
<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="${lstorm}.i.txt" title="Intensity Forecast Statistics" alt="Intensity Forecast Statistics" onMouseOver="self.status=&quot; "Intensity Forecast Statistics" &quot;;return true">Intensity Forecast Statistics (Text File)</a>
<center>
<IMG SRC="${lstorm}.i.png" >
<TR></TR>
</TABLE>

<TABLE ALIGN=bottom BORDER=0 CELLSPACING=2 CELLPADDING=2 BORDERCOLORDARK=ff3333>
<TH colspan=1>        </TH>
 <TR><TD></TD></TR>
 <TR></TR>


 <TR><TD></TD>

</TABLE>
<br><br><br<br><br><br<br><br><br><br><br><br>
<tr >
<td colspan=2 >
<HR>
<center>
<script language="JavaScript">
document.write(Date()+"<br>");
</script>
<center></i></font><font size=1 >Send mail to <a href="mailto:vijay.tallapragada@noaa.gov">vijay.tallapragada@noaa.gov</a> or <a href="mailto:janna.oconnor@noaa.gov">janna.oconnor@noaa.gov</a> with questions or comments about this web site.<br>
</font>
</center>
<P ALIGN=CENTER>
<FONT SIZE=2>&#169; NCEP Hurricane Forecast Project, all rights reserved. </FONT>
</td>
</tr>
</body>
</html>
eor2

##############################################################################################

cat <<eor3 >stats1.html
<html>
<head>
<title>HWRF Coupled Model (2009 Operational Version) Forecast Statistics</title>
</head>
<body text="#000000" link="#0000ff" vlink="#551a8b" alink="#ff0000" bgcolor="#b0eeff">
<h1><FONT COLOR="ff0000"><P ALIGN="center"> HWRF forecasts for $storm $yyyy - Model Performance Statistics</FONT> </h1>

<td align=center><a href="../index.html" title="$storm Main Page" alt="$storm Main Page" onMouseOver="self.status=&quot; "$storm Main Page" &quot;;return true">Back to $storm Main Page</a>
<br><br>

<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="stats.html" title="Track Forecast Statistics (HWRF and GFDL)" alt="Track Forecast Statistics (HWRF and GFDL)" onMouseOver="self.status=&quot; "Track Forecast Statistics (HWRF, and GFDL)" &quot;;return true">Track Forecast Statistics (HWRF and GFDL)</a>
<td align=center><a href="stats2.html" title="Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS )" alt="Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS)" onMouseOver="self.status=&quot; "Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS)" &quot;;return true">Track Forecast Statistics (HWRF, GFDL, AVNO, UKM and NOGAPS)</a>
<TR></TR>
</TABLE>

<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="${lstorm}_hga.t.txt" title="Track Forecast Statistics" alt="Track Forecast Statistics" onMouseOver="self.status=&quot; "Track Forecast Statistics" &quot;;return true">Track Forecast Statistics (Text File)</a>
<center>
<IMG SRC="${lstorm}_hga.t.png" >
<font size=3 >
<TR></TR>
</TABLE>
<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="${lstorm}.i.txt" title="Intensity Forecast Statistics" alt="Intensity Forecast Statistics" onMouseOver="self.status=&quot; "Intensity Forecast Statistics" &quot;;return true">Intensity Forecast Statistics (Text File)</a>
<center>
<IMG SRC="${lstorm}.i.png" >
<TR></TR>
</TABLE>

<TABLE ALIGN=bottom BORDER=0 CELLSPACING=2 CELLPADDING=2 BORDERCOLORDARK=ff3333>
<TH colspan=1>        </TH>
 <TR><TD></TD></TR>
 <TR></TR>


 <TR><TD></TD>

</TABLE>
<br><br><br<br><br><br<br><br><br><br><br><br>
<tr >
<td colspan=2 >
<HR>
<center>
<script language="JavaScript">
document.write(Date()+"<br>");
</script>
<center></i></font><font size=1 >Send mail to <a href="mailto:vijay.tallapragada@noaa.gov">vijay.tallapragada@noaa.gov</a> or <a href="mailto:janna.oconnor@noaa.gov">janna.oconnor@noaa.gov</a> with questions or comments about this web site.<br>
</font>
</center>
<P ALIGN=CENTER>
<FONT SIZE=2>&#169; NCEP Hurricane Forecast Project, all rights reserved. </FONT>
</td>
</tr>
</body>
</html>
eor3

##############################################################################################

cat <<eor4 >stats2.html
<html>
<head>
<title>HWRF Coupled Model (2009 Operational Version) Forecast Statistics</title>
</head>
<body text="#000000" link="#0000ff" vlink="#551a8b" alink="#ff0000" bgcolor="#b0eeff">
<h1><FONT COLOR="ff0000"><P ALIGN="center"> HWRF forecasts for $storm $yyyy - Model Performance Statistics</FONT> </h1>

<td align=center><a href="../index.html" title="$storm Main Page" alt="$storm Main Page" onMouseOver="self.status=&quot; "$storm Main Page" &quot;;return true">Back to $storm Main Page</a>
<br><br>

<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="stats1.html" title="Track Forecast Statistics (HWRF, GFDL, AVNO)" alt="Track Forecast Statistics (HWRF, GFDL, AVNO)" onMouseOver="self.status=&quot; "Track Forecast Statistics (HWRF, GFDL, AVNO)" &quot;;return true">Track Forecast Statistics (HWRF, GFDL, AVNO)</a>
<td align=center><a href="stats.html" title="Track Forecast Statistics (HWRF and GFDL)" alt="Track Forecast Statistics (HWRF and GFDL)" onMouseOver="self.status=&quot; "Track Forecast Statistics (HWRF, and GFDL)" &quot;;return true">Track Forecast Statistics (HWRF and GFDL)</a>
<TR></TR>
</TABLE>

<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="${lstorm}.t.txt" title="Track Forecast Statistics" alt="Track Forecast Statistics" onMouseOver="self.status=&quot; "Track Forecast Statistics" &quot;;return true">Track Forecast Statistics (Text File)</a>
<center>
<IMG SRC="${lstorm}.t.png" >
<font size=3 >
<TR></TR>
</TABLE>
<TABLE BORDER=3 colspan=4 align=center>
<td align=center><a href="${lstorm}.i.txt" title="Intensity Forecast Statistics" alt="Intensity Forecast Statistics" onMouseOver="self.status=&quot; "Intensity Forecast Statistics" &quot;;return true">Intensity Forecast Statistics (Text File)</a>
<center>
<IMG SRC="${lstorm}.i.png" >
<TR></TR>
</TABLE>

<TABLE ALIGN=bottom BORDER=0 CELLSPACING=2 CELLPADDING=2 BORDERCOLORDARK=ff3333>
<TH colspan=1>        </TH>
 <TR><TD></TD></TR>
 <TR></TR>


 <TR><TD></TD>

</TABLE>
<br><br><br<br><br><br<br><br><br><br><br><br>
<tr >
<td colspan=2 >
<HR>
<center>
<script language="JavaScript">
document.write(Date()+"<br>");
</script>
<center></i></font><font size=1 >Send mail to <a href="mailto:vijay.tallapragada@noaa.gov">vijay.tallapragada@noaa.gov</a> or <a href="mailto:janna.oconnor@noaa.gov">janna.oconnor@noaa.gov</a> with questions or comments about this web site.<br>
</font>
</center>
<P ALIGN=CENTER>
<FONT SIZE=2>&#169; NCEP Hurricane Forecast Project, all rights reserved. </FONT>
</td>
</tr>
</body>
</html>
eor4


