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

ALL : "$(OUTDIR)\mass1_v082.exe" "$(OUTDIR)\julian.mod"\
 "$(OUTDIR)\fluvial_coeffs.mod" "$(OUTDIR)\flow_coeffs.mod"\
 "$(OUTDIR)\gas_coeffs.mod" "$(OUTDIR)\met_data_module.mod"\
 "$(OUTDIR)\energy_flux.mod" "$(OUTDIR)\tdg_equation_coeff.mod"\
 "$(OUTDIR)\gas_functions.mod" "$(OUTDIR)\section_vars.mod"\
 "$(OUTDIR)\linkbc_vars.mod" "$(OUTDIR)\date_vars.mod" "$(OUTDIR)\logicals.mod"\
 "$(OUTDIR)\link_vars.mod" "$(OUTDIR)\general_vars.mod"\
 "$(OUTDIR)\point_vars.mod" "$(OUTDIR)\file_vars.mod"\
 "$(OUTDIR)\transport_vars.mod" "$(OUTDIR)\scalars.mod"

!ELSE 

ALL : "$(OUTDIR)\mass1_v082.exe" "$(OUTDIR)\julian.mod"\
 "$(OUTDIR)\fluvial_coeffs.mod" "$(OUTDIR)\flow_coeffs.mod"\
 "$(OUTDIR)\gas_coeffs.mod" "$(OUTDIR)\met_data_module.mod"\
 "$(OUTDIR)\energy_flux.mod" "$(OUTDIR)\tdg_equation_coeff.mod"\
 "$(OUTDIR)\gas_functions.mod" "$(OUTDIR)\section_vars.mod"\
 "$(OUTDIR)\linkbc_vars.mod" "$(OUTDIR)\date_vars.mod" "$(OUTDIR)\logicals.mod"\
 "$(OUTDIR)\link_vars.mod" "$(OUTDIR)\general_vars.mod"\
 "$(OUTDIR)\point_vars.mod" "$(OUTDIR)\file_vars.mod"\
 "$(OUTDIR)\transport_vars.mod" "$(OUTDIR)\scalars.mod"

!ENDIF 

CLEAN :
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

ALL : "$(OUTDIR)\mass1_v082.exe" "$(OUTDIR)\DF50.PDB" "$(OUTDIR)\julian.mod"\
 "$(OUTDIR)\fluvial_coeffs.mod" "$(OUTDIR)\flow_coeffs.mod"\
 "$(OUTDIR)\gas_coeffs.mod" "$(OUTDIR)\met_data_module.mod"\
 "$(OUTDIR)\energy_flux.mod" "$(OUTDIR)\tdg_equation_coeff.mod"\
 "$(OUTDIR)\gas_functions.mod" "$(OUTDIR)\section_vars.mod"\
 "$(OUTDIR)\linkbc_vars.mod" "$(OUTDIR)\date_vars.mod" "$(OUTDIR)\logicals.mod"\
 "$(OUTDIR)\link_vars.mod" "$(OUTDIR)\general_vars.mod"\
 "$(OUTDIR)\point_vars.mod" "$(OUTDIR)\file_vars.mod"\
 "$(OUTDIR)\transport_vars.mod" "$(OUTDIR)\scalars.mod"

!ELSE 

ALL : "$(OUTDIR)\mass1_v082.exe" "$(OUTDIR)\DF50.PDB" "$(OUTDIR)\julian.mod"\
 "$(OUTDIR)\fluvial_coeffs.mod" "$(OUTDIR)\flow_coeffs.mod"\
 "$(OUTDIR)\gas_coeffs.mod" "$(OUTDIR)\met_data_module.mod"\
 "$(OUTDIR)\energy_flux.mod" "$(OUTDIR)\tdg_equation_coeff.mod"\
 "$(OUTDIR)\gas_functions.mod" "$(OUTDIR)\section_vars.mod"\
 "$(OUTDIR)\linkbc_vars.mod" "$(OUTDIR)\date_vars.mod" "$(OUTDIR)\logicals.mod"\
 "$(OUTDIR)\link_vars.mod" "$(OUTDIR)\general_vars.mod"\
 "$(OUTDIR)\point_vars.mod" "$(OUTDIR)\file_vars.mod"\
 "$(OUTDIR)\transport_vars.mod" "$(OUTDIR)\scalars.mod"

!ENDIF 

CLEAN :
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
SOURCE=.\array_alloc.f90
DEP_F90_ARRAY=\
	".\flow_coeffs.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\linkbc_vars.mod"\
	".\point_vars.mod"\
	".\section_vars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\array_alloc.obj" : $(SOURCE) $(DEP_F90_ARRAY) "$(INTDIR)"


SOURCE=.\array_dealloc.f90
DEP_F90_ARRAY_=\
	".\flow_coeffs.mod"\
	".\link_vars.mod"\
	".\linkbc_vars.mod"\
	".\point_vars.mod"\
	".\section_vars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\array_dealloc.obj" : $(SOURCE) $(DEP_F90_ARRAY_) "$(INTDIR)"


SOURCE=.\coeff.f90
DEP_F90_COEFF=\
	".\fluvial_coeffs.mod"\
	".\general_vars.mod"\
	

"$(INTDIR)\coeff.obj" : $(SOURCE) $(DEP_F90_COEFF) "$(INTDIR)"


SOURCE=.\date_to_decimal.f90
DEP_F90_DATE_=\
	".\date_vars.mod"\
	".\julian.mod"\
	

"$(INTDIR)\date_to_decimal.obj" : $(SOURCE) $(DEP_F90_DATE_) "$(INTDIR)"


SOURCE=.\decimal_to_date.f90
DEP_F90_DECIM=\
	".\date_vars.mod"\
	".\general_vars.mod"\
	".\julian.mod"\
	

"$(INTDIR)\decimal_to_date.obj" : $(SOURCE) $(DEP_F90_DECIM) "$(INTDIR)"


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
DEP_F90_FILE_=\
	".\file_vars.mod"\
	

"$(INTDIR)\file_manager.obj" : $(SOURCE) $(DEP_F90_FILE_) "$(INTDIR)"


SOURCE=.\flow_sim.f90
DEP_F90_FLOW_=\
	".\flow_coeffs.mod"\
	".\fluvial_coeffs.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\point_vars.mod"\
	

"$(INTDIR)\flow_sim.obj" : $(SOURCE) $(DEP_F90_FLOW_) "$(INTDIR)"


SOURCE=.\gage_output.f90
DEP_F90_GAGE_=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\gas_functions.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\met_data_module.mod"\
	".\point_vars.mod"\
	".\scalars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\gage_output.obj" : $(SOURCE) $(DEP_F90_GAGE_) "$(INTDIR)"


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
DEP_F90_GAS_F=\
	".\gas_coeffs.mod"\
	

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"gas_functions"


"$(INTDIR)\gas_functions_module.obj"	"$(INTDIR)\gas_functions.mod" : $(SOURCE)\
 $(DEP_F90_GAS_F) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"gas_functions"


"$(INTDIR)\gas_functions_module.obj"	"$(INTDIR)\gas_functions.mod" : $(SOURCE)\
 $(DEP_F90_GAS_F) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\general_data.f90
DEP_F90_GENER=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\logicals.mod"\
	".\section_vars.mod"\
	

"$(INTDIR)\general_data.obj" : $(SOURCE) $(DEP_F90_GENER) "$(INTDIR)"


SOURCE=.\hydro_bc.f90
DEP_F90_HYDRO=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\linkbc_vars.mod"\
	".\logicals.mod"\
	

"$(INTDIR)\hydro_bc.obj" : $(SOURCE) $(DEP_F90_HYDRO) "$(INTDIR)"


SOURCE=.\initial_cond.f90
DEP_F90_INITI=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\point_vars.mod"\
	".\scalars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\initial_cond.obj" : $(SOURCE) $(DEP_F90_INITI) "$(INTDIR)"


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
DEP_F90_KICK_=\
	".\date_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	

"$(INTDIR)\kick_off.obj" : $(SOURCE) $(DEP_F90_KICK_) "$(INTDIR)"


SOURCE=.\latflow_bc.f90
DEP_F90_LATFL=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\linkbc_vars.mod"\
	".\logicals.mod"\
	

"$(INTDIR)\latflow_bc.obj" : $(SOURCE) $(DEP_F90_LATFL) "$(INTDIR)"


SOURCE=.\linear_interp.f90

"$(INTDIR)\linear_interp.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\link_bc.f90
DEP_F90_LINK_=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\linkbc_vars.mod"\
	".\logicals.mod"\
	

"$(INTDIR)\link_bc.obj" : $(SOURCE) $(DEP_F90_LINK_) "$(INTDIR)"


SOURCE=.\link_data.f90
DEP_F90_LINK_D=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	

"$(INTDIR)\link_data.obj" : $(SOURCE) $(DEP_F90_LINK_D) "$(INTDIR)"


SOURCE=.\mass1.f90
DEP_F90_MASS1=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\met_data_module.mod"\
	".\scalars.mod"\
	".\section_vars.mod"\
	".\tdg_equation_coeff.mod"\
	

"$(INTDIR)\mass1.obj" : $(SOURCE) $(DEP_F90_MASS1) "$(INTDIR)"


SOURCE=.\met_data_module.f90
DEP_F90_MET_D=\
	".\date_vars.mod"\
	".\logicals.mod"\
	

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"met_data_module"


"$(INTDIR)\met_data_module.obj"	"$(INTDIR)\met_data_module.mod" : $(SOURCE)\
 $(DEP_F90_MET_D) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"met_data_module"


"$(INTDIR)\met_data_module.obj"	"$(INTDIR)\met_data_module.mod" : $(SOURCE)\
 $(DEP_F90_MET_D) "$(INTDIR)"
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
DEP_F90_NONFL=\
	".\link_vars.mod"\
	".\point_vars.mod"\
	

"$(INTDIR)\nonfluvial_coeff.obj" : $(SOURCE) $(DEP_F90_NONFL) "$(INTDIR)"


SOURCE=.\point_data.f90
DEP_F90_POINT=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\point_vars.mod"\
	".\section_vars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\point_data.obj" : $(SOURCE) $(DEP_F90_POINT) "$(INTDIR)"


SOURCE=.\print_output.f90
DEP_F90_PRINT=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\gas_functions.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\linkbc_vars.mod"\
	".\logicals.mod"\
	".\met_data_module.mod"\
	".\point_vars.mod"\
	".\scalars.mod"\
	".\section_vars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\print_output.obj" : $(SOURCE) $(DEP_F90_PRINT) "$(INTDIR)"


SOURCE=.\profile_output.f90
DEP_F90_PROFI=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\gas_functions.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\met_data_module.mod"\
	".\point_vars.mod"\
	".\scalars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\profile_output.obj" : $(SOURCE) $(DEP_F90_PROFI) "$(INTDIR)"


SOURCE=.\read_config.f90
DEP_F90_READ_=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\logicals.mod"\
	".\point_vars.mod"\
	".\section_vars.mod"\
	

"$(INTDIR)\read_config.obj" : $(SOURCE) $(DEP_F90_READ_) "$(INTDIR)"


SOURCE=.\read_hotstart.f90
DEP_F90_READ_H=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\logicals.mod"\
	".\point_vars.mod"\
	".\scalars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\read_hotstart.obj" : $(SOURCE) $(DEP_F90_READ_H) "$(INTDIR)"


SOURCE=.\scalars_module.f90
DEP_F90_SCALA=\
	".\energy_flux.mod"\
	".\gas_functions.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\linkbc_vars.mod"\
	".\logicals.mod"\
	".\met_data_module.mod"\
	".\point_vars.mod"\
	".\tdg_equation_coeff.mod"\
	".\transport_vars.mod"\
	

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"scalars"


"$(INTDIR)\scalars_module.obj"	"$(INTDIR)\scalars.mod" : $(SOURCE)\
 $(DEP_F90_SCALA) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"scalars"


"$(INTDIR)\scalars_module.obj"	"$(INTDIR)\scalars.mod" : $(SOURCE)\
 $(DEP_F90_SCALA) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\section.f90
DEP_F90_SECTI=\
	".\general_vars.mod"\
	".\point_vars.mod"\
	".\section_vars.mod"\
	

"$(INTDIR)\section.obj" : $(SOURCE) $(DEP_F90_SECTI) "$(INTDIR)"


SOURCE=.\section_data.f90
DEP_F90_SECTIO=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\logicals.mod"\
	".\section_vars.mod"\
	

"$(INTDIR)\section_data.obj" : $(SOURCE) $(DEP_F90_SECTIO) "$(INTDIR)"


SOURCE=.\section_table.f90
DEP_F90_SECTION=\
	".\logicals.mod"\
	".\section_vars.mod"\
	

"$(INTDIR)\section_table.obj" : $(SOURCE) $(DEP_F90_SECTION) "$(INTDIR)"


SOURCE=.\svgrp.f90

"$(INTDIR)\svgrp.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\table_interp.f90
DEP_F90_TABLE=\
	".\general_vars.mod"\
	".\linkbc_vars.mod"\
	

"$(INTDIR)\table_interp.obj" : $(SOURCE) $(DEP_F90_TABLE) "$(INTDIR)"


SOURCE=.\tdg_equation_coeff.f90
DEP_F90_TDG_E=\
	".\file_vars.mod"\
	".\logicals.mod"\
	

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

F90_MODOUT=\
	"tdg_equation_coeff"


"$(INTDIR)\tdg_equation_coeff.obj"	"$(INTDIR)\tdg_equation_coeff.mod" : \
$(SOURCE) $(DEP_F90_TDG_E) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

F90_MODOUT=\
	"tdg_equation_coeff"


"$(INTDIR)\tdg_equation_coeff.obj"	"$(INTDIR)\tdg_equation_coeff.mod" : \
$(SOURCE) $(DEP_F90_TDG_E) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\trans_bc.f90
DEP_F90_TRANS=\
	".\date_vars.mod"\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\linkbc_vars.mod"\
	".\logicals.mod"\
	

"$(INTDIR)\trans_bc.obj" : $(SOURCE) $(DEP_F90_TRANS) "$(INTDIR)"


SOURCE=.\write_restart.f90
DEP_F90_WRITE=\
	".\file_vars.mod"\
	".\general_vars.mod"\
	".\link_vars.mod"\
	".\point_vars.mod"\
	".\scalars.mod"\
	".\transport_vars.mod"\
	

"$(INTDIR)\write_restart.obj" : $(SOURCE) $(DEP_F90_WRITE) "$(INTDIR)"



!ENDIF 

