# -------------------------------------------------------------
# file: Makefile
# Makefile for MASS1 on SGI
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 11, 1998 by William A. Perkins
# Last Change: Wed Sep 29 11:31:52 1999 by William A. Perkins <perk@erebus.pnl.gov>
# -------------------------------------------------------------
# RCSID: $Id$

F90 = f90
COMPILE.f90 = $(F90) $(F90FLAGS) -c $(DEBUG)
LINK.f90 = $(F90) $(LDFLAGS)
MOD=.mod
OBJ=.o

DEBUG = -O #  -g3 -O
FLAGS = $(DEBUG) -n32 -Ofast # -r10000 -TARG:platform=IP30
FLAGS = $(DEBUG) -n32 -r10000 -r8 -d8 -TARG:platform=IP30
F90FLAGS = $(FLAGS)
LIBLOC = 
LDFLAGS = ${LIBLOC} $(FLAGS)
LIBS = -lfpe

TARGET = mass1_v084
SRCS = \
    array_alloc.f90				\
    array_dealloc.f90				\
    coeff.f90					\
    energy_flux_module.f90			\
    file_manager.f90				\
    flow_sim.f90				\
    gage_output.f90				\
    gas_coeffs_module.f90			\
    gas_functions_module.f90			\
    gastrans.f90				\
    general_data.f90				\
    hydro_bc.f90				\
    initial_cond.f90				\
    julian.f90					\
    kick_off.f90				\
    latflow_bc.f90				\
    linear_interp.f90				\
    link_bc.f90					\
    link_data.f90				\
    mass1.f90					\
    met_data_module.f90				\
    modules.f90					\
    nonfluvial_coeff.f90			\
    point_data.f90				\
    print_output.f90				\
    profile_output.f90				\
    read_config.f90				\
    read_hotstart.f90				\
    scalars_module.f90				\
    section.f90					\
    section_data.f90				\
    section_table.f90				\
    svgrp.f90					\
    table_interp.f90				\
    tdg_equation_coeff.f90			\
    trans_bc.f90				\
    date_to_decimal.f90				\
    decimal_to_date.f90				\
    write_restart.f90

MODULES = 					\
    ENERGY_FLUX$(MOD)				\
    GAS_COEFFS$(MOD)				\
    GAS_FUNCTIONS$(MOD)				\
    MET_DATA_MODULE$(MOD)			\
    GENERAL_VARS$(MOD)				\
    DATE_VARS$(MOD)				\
    LOGICALS$(MOD)				\
    FILE_VARS$(MOD)				\
    LINKBC_VARS$(MOD)				\
    LINK_VARS$(MOD)				\
    POINT_VARS$(MOD)				\
    SECTION_VARS$(MOD)				\
    FLOW_COEFFS$(MOD)				\
    FLUVIAL_COEFFS$(MOD)			\
    TRANSPORT_VARS$(MOD)			\
    SCALARS$(MOD)				\
    JULIAN$(MOD)				\
    TDG_EQUATION_COEFF$(MOD)

OBJS = $(SRCS:%.f90=%$(OBJ))

${TARGET}: ${OBJS}
	${LINK.f90} ${OBJS} ${LIBS} -o ${TARGET}

clean::
	rm -f ${TARGET}

svgrp-test: svgrp-test.o svgrp.o
	${LINK.f90} svgrp-test.o svgrp.o ${LIBS} -o $@

clean:: 
	rm -f svgrp-test

# dependancies for individual object files

array_alloc$(OBJ): array_alloc.f90 				\
    GENERAL_VARS$(MOD) LINKBC_VARS$(MOD) LINK_VARS$(MOD) FLOW_COEFFS$(MOD) \
    POINT_VARS$(MOD) SECTION_VARS$(MOD) TRANSPORT_VARS$(MOD)

array_dealloc$(OBJ): array_dealloc.f90		\
    LINKBC_VARS$(MOD) LINK_VARS$(MOD) FLOW_COEFFS$(MOD) \
    POINT_VARS$(MOD) SECTION_VARS$(MOD) TRANSPORT_VARS$(MOD)

coeff$(OBJ): coeff.f90 FLUVIAL_COEFFS$(MOD) GENERAL_VARS$(MOD)

date_to_decimal$(OBJ): date_to_decimal.f90 JULIAN$(MOD)

decimal_to_date$(OBJ): decimal_to_date.f90 JULIAN$(MOD)

energy_flux_module$(OBJ): energy_flux_module.f90

file_manager$(OBJ): file_manager.f90 FILE_VARS$(MOD)

flow_sim$(OBJ): flow_sim.f90			\
    GENERAL_VARS$(MOD) LINK_VARS$(MOD) POINT_VARS$(MOD) \
    FLUVIAL_COEFFS$(MOD) FLOW_COEFFS$(MOD) LOGICALS$(MOD)

gage_output$(OBJ): gage_output.f90 			\
    LINK_VARS$(MOD)				\
    GENERAL_VARS$(MOD)				\
    POINT_VARS$(MOD)				\
    FILE_VARS$(MOD)				\
    TRANSPORT_VARS$(MOD)			\
    DATE_VARS$(MOD)				\
    SCALARS$(MOD)				\
    MET_DATA_MODULE$(MOD)			\
    GAS_FUNCTIONS$(MOD)

gas_coeffs_module$(OBJ): gas_coeffs_module.f90

gas_functions_module$(OBJ): gas_functions_module.f90 GAS_COEFFS$(MOD)

gas_sim$(OBJ): gas_sim.f90			\
    TRANSPORT_VARS$(MOD) GENERAL_VARS$(MOD) LINK_VARS$(MOD) \
    POINT_VARS$(MOD) LINKBC_VARS$(MOD) LOGICALS$(MOD)

gastrans$(OBJ): gastrans.f90

general_data$(OBJ): general_data.f90		\
    GENERAL_VARS$(MOD) SECTION_VARS$(MOD) FILE_VARS$(MOD)

hydro_bc$(OBJ): hydro_bc.f90			\
    LINKBC_VARS$(MOD) FILE_VARS$(MOD) GENERAL_VARS$(MOD) \
    DATE_VARS$(MOD)

initial_cond$(OBJ): initial_cond.f90		\
    POINT_VARS$(MOD)				\
    LINK_VARS$(MOD)				\
    GENERAL_VARS$(MOD)

julian$(OBJ): julian.f90

kick_off$(OBJ): kick_off.f90			\
    GENERAL_VARS$(MOD)				\
    LINK_VARS$(MOD)				\
    DATE_VARS$(MOD)

latflow_bc$(OBJ): latflow_bc.f90		\
    LINKBC_VARS$(MOD)				\
    FILE_VARS$(MOD)

linear_interp$(OBJ): linear_interp.f90

link_bc$(OBJ): link_bc.f90			\
    LINKBC_VARS$(MOD)				\
    FILE_VARS$(MOD)

link_data$(OBJ): link_data.f90			\
    GENERAL_VARS$(MOD)				\
    FILE_VARS$(MOD)

mass1$(OBJ): mass1.f90				\
    GENERAL_VARS$(MOD)				\
    DATE_VARS$(MOD)				\
    FILE_VARS$(MOD)				\
    LOGICALS$(MOD)				\
    SECTION_VARS$(MOD)				\
    SCALARS$(MOD)				\
    MET_DATA_MODULE$(MOD)

met_data_module$(OBJ): met_data_module.f90 DATE_VARS$(MOD)

modules$(OBJ): modules.f90

nonfluvial_coeff$(OBJ): nonfluvial_coeff.f90 LINK_VARS$(MOD)

point_data$(OBJ): point_data.f90		\
    LINK_VARS$(MOD)				\
    SECTION_VARS$(MOD)				\
    FILE_VARS$(MOD)				\
    GENERAL_VARS$(MOD)				\

print_output$(OBJ): print_output.f90		\
    LINK_VARS$(MOD)					\
    GENERAL_VARS$(MOD)				\
    POINT_VARS$(MOD)					\
    FILE_VARS$(MOD)					\
    TRANSPORT_VARS$(MOD)				\
    DATE_VARS$(MOD)					\
    SCALARS$(MOD)					\
    GAS_FUNCTIONS$(MOD)				\
    MET_DATA_MODULE$(MOD)

profile_output$(OBJ): profile_output.f90	\
    LINK_VARS$(MOD)					\
    GENERAL_VARS$(MOD)				\
    POINT_VARS$(MOD)					\
    FILE_VARS$(MOD)					\
    TRANSPORT_VARS$(MOD)				\
    DATE_VARS$(MOD)					\
    SCALARS$(MOD)					\
    GAS_FUNCTIONS$(MOD)				\
    MET_DATA_MODULE$(MOD)

read_config$(OBJ): read_config.f90		\
    FILE_VARS$(MOD)					\
    GENERAL_VARS$(MOD)				\
    SECTION_VARS$(MOD)				\
    POINT_VARS$(MOD)					\
    DATE_VARS$(MOD)					\
    LOGICALS$(MOD)

read_hotstart$(OBJ): read_hotstart.f90		\
    LINK_VARS$(MOD)				\
    GENERAL_VARS$(MOD)				\
    POINT_VARS$(MOD)				\
    FILE_VARS$(MOD)				\
    TRANSPORT_VARS$(MOD)			\
    SCALARS$(MOD)

scalars_module$(OBJ): scalars_module.f90	\
    TRANSPORT_VARS$(MOD)			\
    GENERAL_VARS$(MOD)				\
    LINK_VARS$(MOD)				\
    POINT_VARS$(MOD)				\
    LINKBC_VARS$(MOD)				\
    LOGICALS$(MOD)				\
    MET_DATA_MODULE$(MOD)			\
    ENERGY_FLUX$(MOD)				\
    TDG_EQUATION_COEFF$(MOD)			\
    GAS_FUNCTIONS$(MOD)

section$(OBJ): section.f90 SECTION_VARS$(MOD)

section_data$(OBJ): section_data.f90		\
    SECTION_VARS$(MOD)				\
    FILE_VARS$(MOD)

section_table$(OBJ): section_table.f90

svgrp$(OBJ): svgrp.f90

table_interp$(OBJ): table_interp.f90

tdg_equation_coeff$(OBJ): tdg_equation_coeff.f90

temp_sim$(OBJ): temp_sim.f90			\
    TRANSPORT_VARS$(MOD)				\
    GENERAL_VARS$(MOD)				\
    LINK_VARS$(MOD)					\
    POINT_VARS$(MOD)					\
    LINKBC_VARS$(MOD)					\
    LOGICALS$(MOD)

trans_bc$(OBJ): trans_bc.f90			\
    LINKBC_VARS$(MOD)				\
    FILE_VARS$(MOD)

write_restart$(OBJ): write_restart.f90		\
    LINK_VARS$(MOD)				\
    GENERAL_VARS$(MOD)				\
    POINT_VARS$(MOD)				\
    FILE_VARS$(MOD)				\
    TRANSPORT_VARS$(MOD)			\
    SCALARS$(MOD)

# dependances for individual modules:

ENERGY_FLUX$(MOD): energy_flux_module$(OBJ)

GAS_COEFFS$(MOD): gas_coeffs_module$(OBJ)

GAS_FUNCTIONS$(MOD): gas_functions_module$(OBJ)


GENERAL_VARS$(MOD) DATE_VARS$(MOD) LOGICALS$(MOD) FILE_VARS$(MOD) \
LINKBC_VARS$(MOD) LINK_VARS$(MOD) POINT_VARS$(MOD) SECTION_VARS$(MOD) \
FLOW_COEFFS$(MOD) FLUVIAL_COEFFS$(MOD) TRANSPORT_VARS$(MOD): modules$(OBJ)

MET_DATA_MODULE$(MOD): met_data_module$(OBJ)

SCALARS$(MOD): scalars_module$(OBJ)

TDG_EQUATION_COEFF$(MOD): tdg_equation_coeff$(OBJ)

JULIAN$(MOD): julian.o

svgrp-test.o: svgrp-test.f90 

clean::
	rm -f ${OBJS}
	rm -f ${MODULES}
	rm -f svgrp-test.o
	rm -f *~ *% ,*

%$(OBJ): %.f90
	${COMPILE.f90} $<

# -------------------------------------------------------------
# make tags for emacs
# -------------------------------------------------------------
tags: TAGS
TAGS: ${SRCS} 
	etags ${SRCS}

clean::
	rm -f TAGS

