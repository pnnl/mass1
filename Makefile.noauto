# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  3, 2002 by William A. Perkins
# Last Change: Tue Mar 25 14:09:50 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------


DEBUG = -g
FLAGS = $(DEBUG)

F90 = f95
F90FLAGS = $(FLAGS) -trap=INVALID,DIVBYZERO,OVERFLOW  -YEXT_SFX=_ -YEXT_NAMES=LCS -YCFRL=1
LDLIBS = -lU77
MOD=mod

# F90 = ifc
# F90FLAGS = $(FLAGS) -static -Vaxlib
# LDLIBS = -lPEPCF90
# MOD=d


COMPILE.f90 = $(F90) $(F90FLAGS) -c $(DEBUG)
LINK.f90 = $(F90) $(F90FLAGS) $(LDFLAGS)
RANLIB = ranlib

INCLOC = 
FFLAGS = ${INCLOC} $(FLAGS)
LIBLOC = 
LDFLAGS = ${LIBLOC} $(FLAGS)

LIB = libts.a
SRCS = \
	time_series.f90 \
	julian.f90 \
	date_time_module.f90 \
	utility.f90

OBJS = $(SRCS:%.f90=%.o)
LIBOBJS = ${OBJS:%.o=$(LIB)(%.o)}

${LIB}: ${LIBOBJS}
	-${RANLIB} ${LIB}

clean::
	rm -f ${LIB}
	rm -f *.$(MOD)

test: tstest1 tstest2 datetest1

tstest1: tstest1.o ${LIB}
	${LINK.f90} -o $@ tstest1.o ${LIB} ${LDLIBS}

tstest2: tstest2.o ${LIB}
	${LINK.f90} -o $@ tstest2.o ${LIB} ${LDLIBS}

datetest1: datetest1.o ${LIB}
	${LINK.f90} -o $@ datetest1.o ${LIB} ${LDLIBS}

clean::
	rm -f tstest1.o tstest2.o datetest1.o
	rm -f tstest1 tstest2 datetest1

# dependancies for individual object files

tstest1.o: tstest1.f90 time_series.o
tstest2.o: tstest2.f90 time_series.o
datetime1.o: datetime1.f90 date_time_module.o utility.o
time_series.o: time_series.f90 date_time_module.o utility.o
date_time_module.o: date_time_module.f90 julian.o
julian.o: julian.f90
utility.o: utility.f90

clean:: 
	rm -f $(OBJS) 
	rm -f *.$(MOD)

%.o: %.f90
	${COMPILE.f90} $<


tags: TAGS
TAGS: $(SRCS)
	etags $(SRCS)
clean:: 
	rm -f TAGS

clean::
	rm -f *~ *% ,*

