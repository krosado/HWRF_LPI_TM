SHELL=/bin/sh

include ../../../configure.hwrf

# OBJS = addate.o adn30.o \
# 	bfrini.o bort.o bort2.o \
# 	cadn30.o capit.o chekstab.o chrtrn.o chrtrna.o cktaba.o closbf.o closmg.o cmpmsg.o \
#         cmsgini.o cnved4.o conwin.o copybf.o copymg.o copysb.o cpbfdx.o cpymem.o cpyupd.o \
# 	datebf.o datelen.o digit.o drfini.o drstpl.o dumpbf.o dxdump.o dxinit.o dxmini.o \
# 	elemdx.o \
# 	getabdb.o getlens.o getntbe.o gets1loc.o gettbh.o getwin.o \
# 	i4dy.o ibfms.o ichkstr.o icopysb.o idn30.o ifbget.o ifxy.o igetdate.o igetfxy.o igetntbl.o \
# 	inctab.o invcon.o invmrg.o invtag.o invwin.o ipkm.o ireadmg.o ireadmm.o \
#         ireadns.o ireadsb.o irev.o istdesc.o iupb.o iupbs01.o iupbs1.o iupm.o iupvs01.o iupvs1.o \
# 	jstchr.o jstnum.o \
# 	ljust.o lmsg.o lstjpb.o lstrpc.o lstrps.o \
# 	makestab.o  maxout.o  mesgbc.o mesgbf.o minimg.o mrginv.o msgini.o msgupd.o msgwrt.o mvb.o \
# 	nemock.o nemtab.o nemtba.o nemtbax.o nemtbb.o nemtbd.o nenubd.o nevn.o newwin.o \
#         nmsub.o nmwrd.o numbck.o numtab.o numtbd.o nvnwin.o nwords.o nxtwin.o \
# 	openbf.o openbt.o openmb.o openmg.o \
# 	pad.o padmsg.o parseq.o parstr.o parusr.o parutg.o \
# 	pkb.o pkbs1.o pkc.o pkftbv.o pktdd.o pkvs01.o posapn.o posapx.o \
# 	rcstpl.o rdbfdx.o rdcmps.o rdmemm.o rdmems.o rdmgsb.o rdmsgb.o rdmsgw.o rdmtbb.o rdmtbd.o rdtree.o \
#         rdusdx.o readdx.o readerme.o readlc.o readmg.o readmm.o readns.o readsb.o rewnbf.o rjust.o rsvfvm.o \
# 	seqsdx.o sntbbe.o sntbde.o status.o stdmsg.o stndrd.o strcln.o string.o strnum.o strsuc.o subupd.o \
# 	tabent.o tabsub.o trybump.o \
# 	ufbcnt.o ufbcpy.o ufbcup.o ufbdmp.o ufbevn.o ufbget.o ufbin3.o ufbint.o ufbinx.o ufbmem.o ufbmms.o \
#         ufbmns.o ufbovr.o ufbpos.o ufbqcd.o ufbqcp.o ufbrep.o ufbrms.o ufbrp.o \
# 	ufbrw.o ufbseq.o ufbsp.o ufbstp.o ufbtab.o ufbtam.o ufdump.o upb.o upbb.o upc.o upds3.o upftbv.o uptdd.o usrtpl.o \
# 	valx.o \
# 	atrcpt.o bvers.o cpdxmm.o errwrt.o icmpdx.o idxmsg.o igetntbi.o igetsc.o igettdi.o \
# 	iok2cpy.o ishrdx.o isize.o iupbs3.o lcmgdf.o msgfull.o mtinfo.o \
# 	cmpia.o icvidx.o nummtb.o stseq.o \
# 	pad.o readmt.o rtrcpt.o stbfdx.o stntbia.o stntbi.o strcpt.o  reads3.o \
# 	wrcmps.o wrdlen.o wrdxtb.o writcp.o writdx.o writlc.o writsa.o writsb.o wrtree.o wtstat.o \
# 	bort_exit.o ccbfl.o cobfl.o crbmg.o cwbmg.o rbytes.o restd.o wrdesc.o 

OBJS= \
adn30.o atrcpt.o bfrini.o blocks.o bort2.o bort_exit.o bort.o bvers.o  \
cadn30.o capit.o ccbfl.o chekstab.o chrtrna.o chrtrn.o cktaba.o  \
closbf.o closmg.o cmpia.o cmpmsg.o cmsgini.o cnved4.o cobfl.o conwin.o  \
copybf.o copymg.o copysb.o cpbfdx.o cpdxmm.o cpymem.o cpyupd.o crbmg.o  \
cread.o cwbmg.o datebf.o datelen.o digit.o drfini.o drstpl.o dumpbf.o  \
dxdump.o dxinit.o dxmini.o elemdx.o errwrt.o getabdb.o getbmiss.o  \
getlens.o getntbe.o gets1loc.o gettagpr.o gettbh.o getvalnb.o getwin.o  \
i4dy.o ibfms.o icbfms.o ichkstr.o icmpdx.o icopysb.o icvidx.o idn30.o  \
idxmsg.o ifbget.o ifxy.o igetdate.o igetfxy.o igetntbi.o igetntbl.o  \
igetsc.o igettdi.o inctab.o invcon.o invmrg.o invtag.o invwin.o  \
iok2cpy.o ipkm.o ipks.o ireadmg.o ireadmm.o ireadns.o ireadsb.o irev.o  \
ishrdx.o isize.o istdesc.o iupb.o iupbs01.o iupbs3.o iupm.o iupvs01.o  \
jstchr.o jstnum.o lcmgdf.o lmsg.o lstjpb.o makestab.o maxout.o mesgbc.o  \
mesgbf.o minimg.o mrginv.o msgfull.o msgini.o msgupd.o msgwrt.o  \
mtinfo.o mvb.o nemock.o nemtab.o nemtba.o nemtbax.o nemtbb.o nemtbd.o  \
nenubd.o nevn.o newwin.o nmsub.o nmwrd.o numbck.o nummtb.o numtab.o  \
numtbd.o nvnwin.o nwords.o nxtwin.o openbf.o openbt.o openmb.o openmg.o  \
padmsg.o pad.o parstr.o parusr.o parutg.o pkb.o pkbs1.o pkc.o pkftbv.o  \
pktdd.o pkvs01.o posapx.o rbytes.o rcstpl.o rdbfdx.o rdcmps.o rdmemm.o  \
rdmems.o rdmgsb.o rdmsgb.o rdmsgw.o rdmtbb.o rdmtbd.o rdtree.o rdusdx.o  \
readdx.o readerme.o readlc.o readmg.o readmm.o readmt.o readns.o  \
reads3.o readsb.o restd.o rewnbf.o rjust.o rsvfvm.o rtrcpt.o seqsdx.o  \
setblock.o setbmiss.o sntbbe.o sntbde.o status.o stbfdx.o stdmsg.o  \
stndrd.o stntbia.o stntbi.o strcln.o strcpt.o string.o strnum.o  \
strsuc.o stseq.o tabent.o tabsub.o trybump.o ufbcnt.o ufbcpy.o ufbcup.o  \
ufbdmp.o ufbevn.o ufbget.o ufbin3.o ufbint.o ufbinx.o ufbmem.o ufbmex.o  \
ufbmms.o ufbmns.o ufbovr.o ufbpos.o ufbqcd.o ufbqcp.o ufbrep.o ufbrms.o  \
ufbrp.o ufbrw.o ufbseq.o ufbsp.o ufbstp.o ufbtab.o ufbtam.o ufdump.o  \
upbb.o upb.o upc.o upds3.o upftbv.o ups.o uptdd.o usrtpl.o valx.o  \
wrcmps.o wrdesc.o wrdlen.o wrdxtb.o writcp.o writdx.o writlc.o writsa.o  \
writsb.o wrtree.o wtstat.o

PRM = bufrlib.prm

FC=$(SFC)
CC=$(SCC)
CFLAGS=$(CFLAGS_BUFR) $(CFLAGS_BUFR_MORE)
FFLAGS=$(FFLAGS_BUFR) $(FFLAGS_BUFR_MORE)
LIB=$(OUTPUT_LIB)

all:
	OUTPUT_LIB=../../libbufr_i4r8.a \
	FFLAGS_BUFR_MORE="$(FFLAGS_DOUBLE)" \
	CFLAGS_BUFR_MORE="" \
	make -f Makefile clean lib
	FFLAGS_BUFR_MORE="$(FFLAGS_SINGLE)" \
	CFLAGS_BUFR_MORE="" \
	OUTPUT_LIB=../../libbufr_i4r4.a \
	make -f Makefile clean lib

lib:	$(PRM) $(LIB)

# Override the .F to be fixed-form pre-processed
.F.o:
	$(RM) $@ $*.mod
	$(CPP) $(FPPFLAGS) $(FDEFS)  $< > $*.f
	$(FC) $(FFLAGS) -c $*.f 
	$(RM) $*.f

$(LIB):	$(OBJS)
	$(AR) -ruv $(LIB) $(OBJS)

bufrlib.prm: bufrlib.PRM
	$(CPP) $(FPPFLAGS) $(FDEFS) bufrlib.PRM bufrlib.prm

clean:
	rm -f *.o *.mod $(LIB) bufrlib.prm

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

$(OBJS):	bufrlib.prm
bufrlib.prm:	bufrlib.PRM
