# Microsoft Developer Studio Generated NMAKE File, Based on mass1_v082.dsp
!IF "$(CFG)" == ""
CFG=mass1_v082 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to mass1_v082 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "mass1_v082 - Win32 Release" && "$(CFG)" !=\
 "mass1_v082 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mass1_v082.mak" CFG="mass1_v082 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mass1_v082 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "mass1_v082 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mass1_v082.exe"

!ELSE 

ALL : "$(OUTDIR)\mass1_v082.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\accumulator.mod"
	-@erase "$(INTDIR)\accumulator.obj"
	-@erase "$(INTDIR)\array_alloc.obj"
	-@erase "$(INTDIR)\array_dealloc.obj"
	-@erase "$(INTDIR)\coeff.obj"
	-@erase "$(INTDIR)\date_to_decimal.obj"
	-@erase "$(INTDIR)\date_vars.mod"
	-@erase "$(INTDIR)\decimal_to_date.obj"
	-@erase "$(INTDIR)\energy_flux.mod"
	-@erase "$(INTDIR)\energy_flux_module.obj"
	-@erase "$(INTDIR)\file_manager.obj"
	-@erase "$(INTDIR)\file_vars.mod"
	-@erase "$(INTDIR)\flow_coeffs.mod"
	-@erase "$(INTDIR)\flow_sim.obj"
	-@erase "$(INTDIR)\fluvial_coeffs.mod"
	-@erase "$(INTDIR)\gage_output.obj"
	-@erase "$(INTDIR)\gas_coeffs.mod"
	-@erase "$(INTDIR)\gas_coeffs_module.obj"
	-@erase "$(INTDIR)\gas_functions.mod"
	-@erase "$(INTDIR)\gas_functions_module.obj"
	-@erase "$(INTDIR)\general_data.obj"
	-@erase "$(INTDIR)\general_vars.mod"
	-@erase "$(INTDIR)\hydro_bc.obj"
	-@erase "$(INTDIR)\hydro_output.obj"
	-@erase "$(INTDIR)\hydro_output_module.mod"
	-@erase "$(INTDIR)\initial_cond.obj"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\kick_off.obj"
	-@erase "$(INTDIR)\latflow_bc.obj"
	-@erase "$(INTDIR)\linear_interp.obj"
	-@erase "$(INTDIR)\link_bc.obj"
	-@erase "$(INTDIR)\link_data.obj"
	-@erase "$(INTDIR)\link_vars.mod"
	-@erase "$(INTDIR)\linkbc_vars.mod"
	-@erase "$(INTDIR)\logicals.mod"
	-@erase "$(INTDIR)\mass1.obj"
	-@erase "$(INTDIR)\met_data_module.mod"
	-@erase "$(INTDIR)\met_data_module.obj"
	-@erase "$(INTDIR)\modules.obj"
	-@erase "$(INTDIR)\nonfluvial_coeff.obj"
	-@erase "$(INTDIR)\point_data.obj"
	-@erase "$(INTDIR)\point_vars.mod"
	-@erase "$(INTDIR)\print_output.obj"
	-@erase "$(INTDIR)\profile_output.obj"
	-@erase "$(INTDIR)\profile_output_module.mod"
	-@erase "$(INTDIR)\read_config.obj"
	-@erase "$(INTDIR)\read_hotstart.obj"
	-@erase "$(INTDIR)\scalars.mod"
	-@erase "$(INTDIR)\scalars_module.obj"
	-@erase "$(INTDIR)\section.obj"
	-@erase "$(INTDIR)\section_data.obj"
	-@erase "$(INTDIR)\section_table.obj"
	-@erase "$(INTDIR)\section_vars.mod"
	-@erase "$(INTDIR)\svgrp.obj"
	-@erase "$(INTDIR)\table_interp.obj"
	-@erase "$(INTDIR)\tdg_equation_coeff.mod"
	-@erase "$(INTDIR)\tdg_equation_coeff.obj"
	-@erase "$(INTDIR)\trans_bc.obj"
	-@erase "$(INTDIR)\transport_vars.mod"
	-@erase "$(INTDIR)\write_restart.obj"
	-@erase "$(OUTDIR)\mass1_v082.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/real_size:64 /include:"$(INTDIR)\\" /compile_only /nologo\
 /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mass1_v082.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\mass1_v082.pdb" /machine:I386 /out:"$(OUTDIR)\mass1_v082.exe" 
LINK32_OBJS= \
	"$(INTDIR)\accumulator.obj" \
	"$(INTDIR)\array_alloc.obj" \
	"$(INTDIR)\array_dealloc.obj" \
	"$(INTDIR)\coeff.obj" \
	"$(INTDIR)\date_to_decimal.obj" \
	"$(INTDIR)\decimal_to_date.obj" \
	"$(INTDIR)\energy_flux_module.obj" \
	"$(INTDIR)\file_manager.obj" \
	"$(INTDIR)\flow_sim.obj" \
	"$(INTDIR)\gage_output.obj" \
	"$(INTDIR)\gas_coeffs_module.obj" \
	"$(INTDIR)\gas_functions_module.obj" \
	"$(INTDIR)\general_data.obj" \
	"$(INTDIR)\hydro_bc.obj" \
	"$(INTDIR)\hydro_output.obj" \
	"$(INTDIR)\initial_cond.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\kick_off.obj" \
	"$(INTDIR)\latflow_bc.obj" \
	"$(INTDIR)\linear_interp.obj" \
	"$(INTDIR)\link_bc.obj" \
	"$(INTDIR)\link_data.obj" \
	"$(INTDIR)\mass1.obj" \
	"$(INTDIR)\met_data_module.obj" \
	"$(INTDIR)\modules.obj" \
	"$(INTDIR)\nonfluvial_coeff.obj" \
	"$(INTDIR)\point_data.obj" \
	"$(INTDIR)\print_output.obj" \
	"$(INTDIR)\profile_output.obj" \
	"$(INTDIR)\read_config.obj" \
	"$(INTDIR)\read_hotstart.obj" \
	"$(INTDIR)\scalars_module.obj" \
	"$(INTDIR)\section.obj" \
	"$(INTDIR)\section_data.obj" \
	"$(INTDIR)\section_table.obj" \
	"$(INTDIR)\svgrp.obj" \
	"$(INTDIR)\table_interp.obj" \
	"$(INTDIR)\tdg_equation_coeff.obj" \
	"$(INTDIR)\trans_bc.obj" \
	"$(INTDIR)\write_restart.obj"

"$(OUTDIR)\mass1_v082.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mass1_v082.exe" "$(OUTDIR)\DF50.PDB"

!ELSE 

ALL : "$(OUTDIR)\mass1_v082.exe" "$(OUTDIR)\DF50.PDB"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\accumulator.mod"
	-@erase "$(INTDIR)\accumulator.obj"
	-@erase "$(INTDIR)\array_alloc.obj"
	-@erase "$(INTDIR)\array_dealloc.obj"
	-@erase "$(INTDIR)\coeff.obj"
	-@erase "$(INTDIR)\date_to_decimal.obj"
	-@erase "$(INTDIR)\date_vars.mod"
	-@erase "$(INTDIR)\decimal_to_date.obj"
	-@erase "$(INTDIR)\DF50.PDB"
	-@erase "$(INTDIR)\energy_flux.mod"
	-@erase "$(INTDIR)\energy_flux_module.obj"
	-@erase "$(INTDIR)\file_manager.obj"
	-@erase "$(INTDIR)\file_vars.mod"
	-@erase "$(INTDIR)\flow_coeffs.mod"
	-@erase "$(INTDIR)\flow_sim.obj"
	-@erase "$(INTDIR)\fluvial_coeffs.mod"
	-@erase "$(INTDIR)\gage_output.obj"
	-@erase "$(INTDIR)\gas_coeffs.mod"
	-@erase "$(INTDIR)\gas_coeffs_module.obj"
	-@erase "$(INTDIR)\gas_functions.mod"
	-@erase "$(INTDIR)\gas_functions_module.obj"
	-@erase "$(INTDIR)\general_data.obj"
	-@erase "$(INTDIR)\general_vars.mod"
	-@erase "$(INTDIR)\hydro_bc.obj"
	-@erase "$(INTDIR)\hydro_output.obj"
	-@erase "$(INTDIR)\hydro_output_module.mod"
	-@erase "$(INTDIR)\initial_cond.obj"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\kick_off.obj"
	-@erase "$(INTDIR)\latflow_bc.obj"
	-@erase "$(INTDIR)\linear_interp.obj"
	-@erase "$(INTDIR)\link_bc.obj"
	-@erase "$(INTDIR)\link_data.obj"
	-@erase "$(INTDIR)\link_vars.mod"
	-@erase "$(INTDIR)\linkbc_vars.mod"
	-@erase "$(INTDIR)\logicals.mod"
	-@erase "$(INTDIR)\mass1.obj"
	-@erase "$(INTDIR)\met_data_module.mod"
	-@erase "$(INTDIR)\met_data_module.obj"
	-@erase "$(INTDIR)\modules.obj"
	-@erase "$(INTDIR)\nonfluvial_coeff.obj"
	-@erase "$(INTDIR)\point_data.obj"
	-@erase "$(INTDIR)\point_vars.mod"
	-@erase "$(INTDIR)\print_output.obj"
	-@erase "$(INTDIR)\profile_output.obj"
	-@erase "$(INTDIR)\profile_output_module.mod"
	-@erase "$(INTDIR)\read_config.obj"
	-@erase "$(INTDIR)\read_hotstart.obj"
	-@erase "$(INTDIR)\scalars.mod"
	-@erase "$(INTDIR)\scalars_module.obj"
	-@erase "$(INTDIR)\section.obj"
	-@erase "$(INTDIR)\section_data.obj"
	-@erase "$(INTDIR)\section_table.obj"
	-@erase "$(INTDIR)\section_vars.mod"
	-@erase "$(INTDIR)\svgrp.obj"
	-@erase "$(INTDIR)\table_interp.obj"
	-@erase "$(INTDIR)\tdg_equation_coeff.mod"
	-@erase "$(INTDIR)\tdg_equation_coeff.obj"
	-@erase "$(INTDIR)\trans_bc.obj"
	-@erase "$(INTDIR)\transport_vars.mod"
	-@erase "$(INTDIR)\write_restart.obj"
	-@erase "$(OUTDIR)\mass1_v082.exe"
	-@erase "$(OUTDIR)\mass1_v082.ilk"
	-@erase "$(OUTDIR)\mass1_v082.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/real_size:64 /include:"$(INTDIR)\\" /compile_only /nologo /debug:full\
 /optimize:0 /warn:nofileopt /module:"Debug/" /object:"Debug/"\
 /pdbfile:"Debug/DF50.PDB" 
F90_OBJS=.\Debug/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mass1_v082.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\mass1_v082.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\mass1_v082.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\accumulator.obj" \
	"$(INTDIR)\array_alloc.obj" \
	"$(INTDIR)\array_dealloc.obj" \
	"$(INTDIR)\coeff.obj" \
	"$(INTDIR)\date_to_decimal.obj" \
	"$(INTDIR)\decimal_to_date.obj" \
	"$(INTDIR)\energy_flux_module.obj" \
	"$(INTDIR)\file_manager.obj" \
	"$(INTDIR)\flow_sim.obj" \
	"$(INTDIR)\gage_output.obj" \
	"$(INTDIR)\gas_coeffs_module.obj" \
	"$(INTDIR)\gas_functions_module.obj" \
	"$(INTDIR)\general_data.obj" \
	"$(INTDIR)\hydro_bc.obj" \
	"$(INTDIR)\hydro_output.obj" \
	"$(INTDIR)\initial_cond.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\kick_off.obj" \
	"$(INTDIR)\latflow_bc.obj" \
	"$(INTDIR)\linear_interp.obj" \
	"$(INTDIR)\link_bc.obj" \
	"$(INTDIR)\link_data.obj" \
	"$(INTDIR)\mass1.obj" \
	"$(INTDIR)\met_data_module.obj" \
	"$(INTDIR)\modules.obj" \
	"$(INTDIR)\nonfluvial_coeff.obj" \
	"$(INTDIR)\point_data.obj" \
	"$(INTDIR)\print_output.obj" \
	"$(INTDIR)\profile_output.obj" \
	"$(INTDIR)\read_config.obj" \
	"$(INTDIR)\read_hotstart.obj" \
	"$(INTDIR)\scalars_module.obj" \
	"$(INTDIR)\section.obj" \
	"$(INTDIR)\section_data.obj" \
	"$(INTDIR)\section_table.obj" \
	"$(INTDIR)\svgrp.obj" \
	"$(INTDIR)\table_interp.obj" \
	"$(INTDIR)\tdg_equation_coeff.obj" \
	"$(INTDIR)\trans_bc.obj" \
	"$(INTDIR)\write_restart.obj"

"$(OUTDIR)\mass1_v082.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(CFG)" == "mass1_v082 - Win32 Release" || "$(CFG)" ==\
 "mass1_v082 - Win32 Debug"
SOURCE=.\accumulator.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_ACCUM=\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	
F90_MODOUT=\
	"accumulator"


"$(INTDIR)\accumulator.obj"	"$(INTDIR)\accumulator.mod" : $(SOURCE)\
 $(DEP_F90_ACCUM) "$(INTDIR)" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\gas_functions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_ACCUM=\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	
F90_MODOUT=\
	"accumulator"


"$(INTDIR)\accumulator.obj"	"$(INTDIR)\accumulator.mod" : $(SOURCE)\
 $(DEP_F90_ACCUM) "$(INTDIR)" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\gas_functions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\array_alloc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_ARRAY=\
	".\Release\flow_coeffs.mod"\
	".\Release\general_vars.mod"\
	".\Release\hydro_output_module.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\array_alloc.obj" : $(SOURCE) $(DEP_F90_ARRAY) "$(INTDIR)"\
 "$(INTDIR)\flow_coeffs.mod" "$(INTDIR)\linkbc_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\hydro_output_module.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_ARRAY=\
	".\Debug\flow_coeffs.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\hydro_output_module.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\array_alloc.obj" : $(SOURCE) $(DEP_F90_ARRAY) "$(INTDIR)"\
 "$(INTDIR)\flow_coeffs.mod" "$(INTDIR)\linkbc_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\hydro_output_module.mod"


!ENDIF 

SOURCE=.\array_dealloc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_ARRAY_=\
	".\Release\flow_coeffs.mod"\
	".\Release\hydro_output_module.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\array_dealloc.obj" : $(SOURCE) $(DEP_F90_ARRAY_) "$(INTDIR)"\
 "$(INTDIR)\flow_coeffs.mod" "$(INTDIR)\linkbc_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\hydro_output_module.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_ARRAY_=\
	".\Debug\flow_coeffs.mod"\
	".\Debug\hydro_output_module.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\array_dealloc.obj" : $(SOURCE) $(DEP_F90_ARRAY_) "$(INTDIR)"\
 "$(INTDIR)\flow_coeffs.mod" "$(INTDIR)\linkbc_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\hydro_output_module.mod"


!ENDIF 

SOURCE=.\coeff.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_COEFF=\
	".\Release\fluvial_coeffs.mod"\
	".\Release\general_vars.mod"\
	

"$(INTDIR)\coeff.obj" : $(SOURCE) $(DEP_F90_COEFF) "$(INTDIR)"\
 "$(INTDIR)\fluvial_coeffs.mod" "$(INTDIR)\general_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_COEFF=\
	".\Debug\fluvial_coeffs.mod"\
	".\Debug\general_vars.mod"\
	

"$(INTDIR)\coeff.obj" : $(SOURCE) $(DEP_F90_COEFF) "$(INTDIR)"\
 "$(INTDIR)\fluvial_coeffs.mod" "$(INTDIR)\general_vars.mod"


!ENDIF 

SOURCE=.\date_to_decimal.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_DATE_=\
	".\Release\date_vars.mod"\
	".\Release\julian.mod"\
	

"$(INTDIR)\date_to_decimal.obj" : $(SOURCE) $(DEP_F90_DATE_) "$(INTDIR)"\
 "$(INTDIR)\julian.mod" "$(INTDIR)\date_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_DATE_=\
	".\Debug\date_vars.mod"\
	".\Debug\julian.mod"\
	

"$(INTDIR)\date_to_decimal.obj" : $(SOURCE) $(DEP_F90_DATE_) "$(INTDIR)"\
 "$(INTDIR)\julian.mod" "$(INTDIR)\date_vars.mod"


!ENDIF 

SOURCE=.\decimal_to_date.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_DECIM=\
	".\Release\date_vars.mod"\
	".\Release\julian.mod"\
	

"$(INTDIR)\decimal_to_date.obj" : $(SOURCE) $(DEP_F90_DECIM) "$(INTDIR)"\
 "$(INTDIR)\julian.mod" "$(INTDIR)\date_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_DECIM=\
	".\Debug\date_vars.mod"\
	".\Debug\julian.mod"\
	

"$(INTDIR)\decimal_to_date.obj" : $(SOURCE) $(DEP_F90_DECIM) "$(INTDIR)"\
 "$(INTDIR)\julian.mod" "$(INTDIR)\date_vars.mod"


!ENDIF 

SOURCE=.\energy_flux_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"energy_flux"


"$(INTDIR)\energy_flux_module.obj"	"$(INTDIR)\energy_flux.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"energy_flux"


"$(INTDIR)\energy_flux_module.obj"	"$(INTDIR)\energy_flux.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\file_manager.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_FILE_=\
	".\Release\file_vars.mod"\
	

"$(INTDIR)\file_manager.obj" : $(SOURCE) $(DEP_F90_FILE_) "$(INTDIR)"\
 "$(INTDIR)\file_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_FILE_=\
	".\Debug\file_vars.mod"\
	

"$(INTDIR)\file_manager.obj" : $(SOURCE) $(DEP_F90_FILE_) "$(INTDIR)"\
 "$(INTDIR)\file_vars.mod"


!ENDIF 

SOURCE=.\flow_sim.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_FLOW_=\
	".\Release\flow_coeffs.mod"\
	".\Release\fluvial_coeffs.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	

"$(INTDIR)\flow_sim.obj" : $(SOURCE) $(DEP_F90_FLOW_) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\fluvial_coeffs.mod"\
 "$(INTDIR)\flow_coeffs.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_FLOW_=\
	".\Debug\flow_coeffs.mod"\
	".\Debug\fluvial_coeffs.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	

"$(INTDIR)\flow_sim.obj" : $(SOURCE) $(DEP_F90_FLOW_) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\fluvial_coeffs.mod"\
 "$(INTDIR)\flow_coeffs.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\gage_output.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_GAGE_=\
	".\Release\accumulator.mod"\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\hydro_output_module.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\gage_output.obj" : $(SOURCE) $(DEP_F90_GAGE_) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\date_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\met_data_module.mod"\
 "$(INTDIR)\gas_functions.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\hydro_output_module.mod" "$(INTDIR)\accumulator.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_GAGE_=\
	".\Debug\accumulator.mod"\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\hydro_output_module.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\gage_output.obj" : $(SOURCE) $(DEP_F90_GAGE_) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\date_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\met_data_module.mod"\
 "$(INTDIR)\gas_functions.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\hydro_output_module.mod" "$(INTDIR)\accumulator.mod"


!ENDIF 

SOURCE=.\gas_coeffs_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"gas_coeffs"


"$(INTDIR)\gas_coeffs_module.obj"	"$(INTDIR)\gas_coeffs.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"gas_coeffs"


"$(INTDIR)\gas_coeffs_module.obj"	"$(INTDIR)\gas_coeffs.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\gas_functions_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_GAS_F=\
	".\Release\gas_coeffs.mod"\
	
F90_MODOUT=\
	"gas_functions"


"$(INTDIR)\gas_functions_module.obj"	"$(INTDIR)\gas_functions.mod" : $(SOURCE)\
 $(DEP_F90_GAS_F) "$(INTDIR)" "$(INTDIR)\gas_coeffs.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_GAS_F=\
	".\Debug\gas_coeffs.mod"\
	
F90_MODOUT=\
	"gas_functions"


"$(INTDIR)\gas_functions_module.obj"	"$(INTDIR)\gas_functions.mod" : $(SOURCE)\
 $(DEP_F90_GAS_F) "$(INTDIR)" "$(INTDIR)\gas_coeffs.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\general_data.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_GENER=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\section_vars.mod"\
	

"$(INTDIR)\general_data.obj" : $(SOURCE) $(DEP_F90_GENER) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\file_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_GENER=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\section_vars.mod"\
	

"$(INTDIR)\general_data.obj" : $(SOURCE) $(DEP_F90_GENER) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\file_vars.mod"


!ENDIF 

SOURCE=.\hydro_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_HYDRO=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	

"$(INTDIR)\hydro_bc.obj" : $(SOURCE) $(DEP_F90_HYDRO) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_HYDRO=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	

"$(INTDIR)\hydro_bc.obj" : $(SOURCE) $(DEP_F90_HYDRO) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\hydro_output.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_HYDRO_=\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	
F90_MODOUT=\
	"hydro_output_module"


"$(INTDIR)\hydro_output.obj"	"$(INTDIR)\hydro_output_module.mod" : $(SOURCE)\
 $(DEP_F90_HYDRO_) "$(INTDIR)" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\gas_functions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_HYDRO_=\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	
F90_MODOUT=\
	"hydro_output_module"


"$(INTDIR)\hydro_output.obj"	"$(INTDIR)\hydro_output_module.mod" : $(SOURCE)\
 $(DEP_F90_HYDRO_) "$(INTDIR)" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\gas_functions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\initial_cond.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_INITI=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\initial_cond.obj" : $(SOURCE) $(DEP_F90_INITI) "$(INTDIR)"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\general_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_INITI=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\initial_cond.obj" : $(SOURCE) $(DEP_F90_INITI) "$(INTDIR)"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\general_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\julian.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"julian"


"$(INTDIR)\julian.obj"	"$(INTDIR)\julian.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"julian"


"$(INTDIR)\julian.obj"	"$(INTDIR)\julian.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\kick_off.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_KICK_=\
	".\Release\date_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	

"$(INTDIR)\kick_off.obj" : $(SOURCE) $(DEP_F90_KICK_) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\date_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_KICK_=\
	".\Debug\date_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	

"$(INTDIR)\kick_off.obj" : $(SOURCE) $(DEP_F90_KICK_) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\date_vars.mod"


!ENDIF 

SOURCE=.\latflow_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_LATFL=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	

"$(INTDIR)\latflow_bc.obj" : $(SOURCE) $(DEP_F90_LATFL) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\link_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_LATFL=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	

"$(INTDIR)\latflow_bc.obj" : $(SOURCE) $(DEP_F90_LATFL) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\link_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\linear_interp.f90

"$(INTDIR)\linear_interp.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\link_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_LINK_=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	

"$(INTDIR)\link_bc.obj" : $(SOURCE) $(DEP_F90_LINK_) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\link_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_LINK_=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	

"$(INTDIR)\link_bc.obj" : $(SOURCE) $(DEP_F90_LINK_) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\link_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\link_data.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_LINK_D=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	

"$(INTDIR)\link_data.obj" : $(SOURCE) $(DEP_F90_LINK_D) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_LINK_D=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	

"$(INTDIR)\link_data.obj" : $(SOURCE) $(DEP_F90_LINK_D) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\mass1.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_MASS1=\
	".\Release\accumulator.mod"\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\profile_output_module.mod"\
	".\Release\scalars.mod"\
	".\Release\section_vars.mod"\
	".\Release\tdg_equation_coeff.mod"\
	

"$(INTDIR)\mass1.obj" : $(SOURCE) $(DEP_F90_MASS1) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\logicals.mod" "$(INTDIR)\section_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\tdg_equation_coeff.mod"\
 "$(INTDIR)\profile_output_module.mod" "$(INTDIR)\accumulator.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_MASS1=\
	".\Debug\accumulator.mod"\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\profile_output_module.mod"\
	".\Debug\scalars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\tdg_equation_coeff.mod"\
	

"$(INTDIR)\mass1.obj" : $(SOURCE) $(DEP_F90_MASS1) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\logicals.mod" "$(INTDIR)\section_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\tdg_equation_coeff.mod"\
 "$(INTDIR)\profile_output_module.mod" "$(INTDIR)\accumulator.mod"


!ENDIF 

SOURCE=.\met_data_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_MET_D=\
	".\Release\date_vars.mod"\
	".\Release\logicals.mod"\
	
F90_MODOUT=\
	"met_data_module"


"$(INTDIR)\met_data_module.obj"	"$(INTDIR)\met_data_module.mod" : $(SOURCE)\
 $(DEP_F90_MET_D) "$(INTDIR)" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_MET_D=\
	".\Debug\date_vars.mod"\
	".\Debug\logicals.mod"\
	
F90_MODOUT=\
	"met_data_module"


"$(INTDIR)\met_data_module.obj"	"$(INTDIR)\met_data_module.mod" : $(SOURCE)\
 $(DEP_F90_MET_D) "$(INTDIR)" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\modules.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"general_vars" \
	"date_vars" \
	"logicals" \
	"file_vars" \
	"linkbc_vars" \
	"link_vars" \
	"point_vars" \
	"section_vars" \
	"flow_coeffs" \
	"fluvial_coeffs" \
	"transport_vars"


"$(INTDIR)\modules.obj"	"$(INTDIR)\general_vars.mod"	"$(INTDIR)\date_vars.mod"\
	"$(INTDIR)\logicals.mod"	"$(INTDIR)\file_vars.mod"	"$(INTDIR)\linkbc_vars.mod"\
	"$(INTDIR)\link_vars.mod"	"$(INTDIR)\point_vars.mod"\
	"$(INTDIR)\section_vars.mod"	"$(INTDIR)\flow_coeffs.mod"\
	"$(INTDIR)\fluvial_coeffs.mod"	"$(INTDIR)\transport_vars.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"general_vars" \
	"date_vars" \
	"logicals" \
	"file_vars" \
	"linkbc_vars" \
	"link_vars" \
	"point_vars" \
	"section_vars" \
	"flow_coeffs" \
	"fluvial_coeffs" \
	"transport_vars"


"$(INTDIR)\modules.obj"	"$(INTDIR)\general_vars.mod"	"$(INTDIR)\date_vars.mod"\
	"$(INTDIR)\logicals.mod"	"$(INTDIR)\file_vars.mod"	"$(INTDIR)\linkbc_vars.mod"\
	"$(INTDIR)\link_vars.mod"	"$(INTDIR)\point_vars.mod"\
	"$(INTDIR)\section_vars.mod"	"$(INTDIR)\flow_coeffs.mod"\
	"$(INTDIR)\fluvial_coeffs.mod"	"$(INTDIR)\transport_vars.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\nonfluvial_coeff.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_NONFL=\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\point_vars.mod"\
	

"$(INTDIR)\nonfluvial_coeff.obj" : $(SOURCE) $(DEP_F90_NONFL) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\link_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_NONFL=\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\point_vars.mod"\
	

"$(INTDIR)\nonfluvial_coeff.obj" : $(SOURCE) $(DEP_F90_NONFL) "$(INTDIR)"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\link_vars.mod"


!ENDIF 

SOURCE=.\point_data.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_POINT=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\point_data.obj" : $(SOURCE) $(DEP_F90_POINT) "$(INTDIR)"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_POINT=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\point_data.obj" : $(SOURCE) $(DEP_F90_POINT) "$(INTDIR)"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\print_output.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_PRINT=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\section_vars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\print_output.obj" : $(SOURCE) $(DEP_F90_PRINT) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\date_vars.mod"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\section_vars.mod"\
 "$(INTDIR)\logicals.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\met_data_module.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_PRINT=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\print_output.obj" : $(SOURCE) $(DEP_F90_PRINT) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\date_vars.mod"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\section_vars.mod"\
 "$(INTDIR)\logicals.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\met_data_module.mod"


!ENDIF 

SOURCE=.\profile_output.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_PROFI=\
	".\Release\accumulator.mod"\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\transport_vars.mod"\
	
F90_MODOUT=\
	"profile_output_module"


"$(INTDIR)\profile_output.obj"	"$(INTDIR)\profile_output_module.mod" : \
$(SOURCE) $(DEP_F90_PROFI) "$(INTDIR)" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\date_vars.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\accumulator.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_PROFI=\
	".\Debug\accumulator.mod"\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	
F90_MODOUT=\
	"profile_output_module"


"$(INTDIR)\profile_output.obj"	"$(INTDIR)\profile_output_module.mod" : \
$(SOURCE) $(DEP_F90_PROFI) "$(INTDIR)" "$(INTDIR)\link_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\transport_vars.mod"\
 "$(INTDIR)\date_vars.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\accumulator.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\read_config.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_READ_=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	

"$(INTDIR)\read_config.obj" : $(SOURCE) $(DEP_F90_READ_) "$(INTDIR)"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_READ_=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	

"$(INTDIR)\read_config.obj" : $(SOURCE) $(DEP_F90_READ_) "$(INTDIR)"\
 "$(INTDIR)\file_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\read_hotstart.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_READ_H=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\read_hotstart.obj" : $(SOURCE) $(DEP_F90_READ_H) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_READ_H=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\read_hotstart.obj" : $(SOURCE) $(DEP_F90_READ_H) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\scalars_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_SCALA=\
	".\Release\energy_flux.mod"\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\hydro_output_module.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\point_vars.mod"\
	".\Release\tdg_equation_coeff.mod"\
	".\Release\transport_vars.mod"\
	
F90_MODOUT=\
	"scalars"


"$(INTDIR)\scalars_module.obj"	"$(INTDIR)\scalars.mod" : $(SOURCE)\
 $(DEP_F90_SCALA) "$(INTDIR)" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\linkbc_vars.mod"\
 "$(INTDIR)\logicals.mod" "$(INTDIR)\met_data_module.mod"\
 "$(INTDIR)\energy_flux.mod" "$(INTDIR)\tdg_equation_coeff.mod"\
 "$(INTDIR)\gas_functions.mod" "$(INTDIR)\hydro_output_module.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SCALA=\
	".\Debug\energy_flux.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\hydro_output_module.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\tdg_equation_coeff.mod"\
	".\Debug\transport_vars.mod"\
	
F90_MODOUT=\
	"scalars"


"$(INTDIR)\scalars_module.obj"	"$(INTDIR)\scalars.mod" : $(SOURCE)\
 $(DEP_F90_SCALA) "$(INTDIR)" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\point_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\linkbc_vars.mod"\
 "$(INTDIR)\logicals.mod" "$(INTDIR)\met_data_module.mod"\
 "$(INTDIR)\energy_flux.mod" "$(INTDIR)\tdg_equation_coeff.mod"\
 "$(INTDIR)\gas_functions.mod" "$(INTDIR)\hydro_output_module.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\section.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_SECTI=\
	".\Release\general_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	

"$(INTDIR)\section.obj" : $(SOURCE) $(DEP_F90_SECTI) "$(INTDIR)"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SECTI=\
	".\Debug\general_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	

"$(INTDIR)\section.obj" : $(SOURCE) $(DEP_F90_SECTI) "$(INTDIR)"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod"


!ENDIF 

SOURCE=.\section_data.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_SECTIO=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\section_vars.mod"\
	

"$(INTDIR)\section_data.obj" : $(SOURCE) $(DEP_F90_SECTIO) "$(INTDIR)"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\file_vars.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\general_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SECTIO=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\section_vars.mod"\
	

"$(INTDIR)\section_data.obj" : $(SOURCE) $(DEP_F90_SECTIO) "$(INTDIR)"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\file_vars.mod" "$(INTDIR)\logicals.mod"\
 "$(INTDIR)\general_vars.mod"


!ENDIF 

SOURCE=.\section_table.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_SECTION=\
	".\Release\logicals.mod"\
	".\Release\section_vars.mod"\
	

"$(INTDIR)\section_table.obj" : $(SOURCE) $(DEP_F90_SECTION) "$(INTDIR)"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SECTION=\
	".\Debug\logicals.mod"\
	".\Debug\section_vars.mod"\
	

"$(INTDIR)\section_table.obj" : $(SOURCE) $(DEP_F90_SECTION) "$(INTDIR)"\
 "$(INTDIR)\section_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\svgrp.f90

"$(INTDIR)\svgrp.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\table_interp.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_TABLE=\
	".\Release\general_vars.mod"\
	".\Release\linkbc_vars.mod"\
	

"$(INTDIR)\table_interp.obj" : $(SOURCE) $(DEP_F90_TABLE) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\general_vars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_TABLE=\
	".\Debug\general_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	

"$(INTDIR)\table_interp.obj" : $(SOURCE) $(DEP_F90_TABLE) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\general_vars.mod"


!ENDIF 

SOURCE=.\tdg_equation_coeff.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_TDG_E=\
	".\Release\file_vars.mod"\
	".\Release\logicals.mod"\
	
F90_MODOUT=\
	"tdg_equation_coeff"


"$(INTDIR)\tdg_equation_coeff.obj"	"$(INTDIR)\tdg_equation_coeff.mod" : \
$(SOURCE) $(DEP_F90_TDG_E) "$(INTDIR)" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\logicals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_TDG_E=\
	".\Debug\file_vars.mod"\
	".\Debug\logicals.mod"\
	
F90_MODOUT=\
	"tdg_equation_coeff"


"$(INTDIR)\tdg_equation_coeff.obj"	"$(INTDIR)\tdg_equation_coeff.mod" : \
$(SOURCE) $(DEP_F90_TDG_E) "$(INTDIR)" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\logicals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\trans_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_TRANS=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	

"$(INTDIR)\trans_bc.obj" : $(SOURCE) $(DEP_F90_TRANS) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_TRANS=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	

"$(INTDIR)\trans_bc.obj" : $(SOURCE) $(DEP_F90_TRANS) "$(INTDIR)"\
 "$(INTDIR)\linkbc_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\general_vars.mod" "$(INTDIR)\date_vars.mod" "$(INTDIR)\logicals.mod"


!ENDIF 

SOURCE=.\write_restart.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_WRITE=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\transport_vars.mod"\
	

"$(INTDIR)\write_restart.obj" : $(SOURCE) $(DEP_F90_WRITE) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\scalars.mod"


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_WRITE=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	

"$(INTDIR)\write_restart.obj" : $(SOURCE) $(DEP_F90_WRITE) "$(INTDIR)"\
 "$(INTDIR)\link_vars.mod" "$(INTDIR)\general_vars.mod"\
 "$(INTDIR)\point_vars.mod" "$(INTDIR)\file_vars.mod"\
 "$(INTDIR)\transport_vars.mod" "$(INTDIR)\scalars.mod"


!ENDIF 


!ENDIF 

