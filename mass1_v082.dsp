# Microsoft Developer Studio Project File - Name="mass1_v082" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=mass1_v082 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mass1_v082.mak".
!MESSAGE 
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

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "mass1_v0"
# PROP BASE Intermediate_Dir "mass1_v0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /include:"mass1_v0/" /compile_only /nologo /warn:nofileopt
# ADD F90 /real_size:64 /include:"Release/" /include:"time_series/Release/" /compile_only /nologo /warn:nofileopt
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "mass1_v1"
# PROP BASE Intermediate_Dir "mass1_v1"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /include:"mass1_v1/" /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt
# ADD F90 /real_size:64 /include:"Debug/" /include:"time_series/Debug" /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "mass1_v082 - Win32 Release"
# Name "mass1_v082 - Win32 Debug"
# Begin Source File

SOURCE=.\accumulator.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_ACCUM=\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_ACCUM=\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	

!ENDIF 

F90_MODOUT=\
	"accumulator"

# End Source File
# Begin Source File

SOURCE=.\array_alloc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_ARRAY=\
	".\Release\flow_coeffs.mod"\
	".\Release\general_vars.mod"\
	".\Release\hydro_output_module.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	".\Release\transport_vars.mod"\
	

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
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\array_dealloc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_ARRAY_=\
	".\Release\flow_coeffs.mod"\
	".\Release\hydro_output_module.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	".\Release\transport_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_ARRAY_=\
	".\Debug\flow_coeffs.mod"\
	".\Debug\hydro_output_module.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\transport_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\coeff.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_COEFF=\
	".\Release\fluvial_coeffs.mod"\
	".\Release\general_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_COEFF=\
	".\Debug\fluvial_coeffs.mod"\
	".\Debug\general_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\energy_flux_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"energy_flux"

# End Source File
# Begin Source File

SOURCE=.\flow_sim.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_FLOW_=\
	".\Release\flow_coeffs.mod"\
	".\Release\fluvial_coeffs.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_FLOW_=\
	".\Debug\flow_coeffs.mod"\
	".\Debug\fluvial_coeffs.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

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
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

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
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gas_coeffs_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"gas_coeffs"

# End Source File
# Begin Source File

SOURCE=.\gas_functions_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_GAS_F=\
	".\Release\gas_coeffs.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_GAS_F=\
	".\Debug\gas_coeffs.mod"\
	

!ENDIF 

F90_MODOUT=\
	"gas_functions"

# End Source File
# Begin Source File

SOURCE=.\hydro_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_HYDRO=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_HYDRO=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hydro_output.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_HYDRO_=\
	".\Release\gas_functions.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_HYDRO_=\
	".\Debug\gas_functions.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	

!ENDIF 

F90_MODOUT=\
	"hydro_output_module"

# End Source File
# Begin Source File

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
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_INITI=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\kick_off.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_KICK_=\
	".\Release\date_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\pidlink.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_KICK_=\
	".\Debug\date_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\pidlink.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\latflow_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_LATFL=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_LATFL=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\linear_interp.f90
# End Source File
# Begin Source File

SOURCE=.\link_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_LINK_=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_LINK_=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\link_data.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_LINK_D=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_LINK_D=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

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
	".\Release\pidlink.mod"\
	".\Release\profile_output_module.mod"\
	".\Release\scalars.mod"\
	".\Release\section_vars.mod"\
	".\Release\tdg_equation_coeff.mod"\
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_MASS1=\
	".\Debug\accumulator.mod"\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\pidlink.mod"\
	".\Debug\profile_output_module.mod"\
	".\Debug\scalars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\tdg_equation_coeff.mod"\
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\met_data_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_MET_D=\
	".\Release\date_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_MET_D=\
	".\Debug\date_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

F90_MODOUT=\
	"met_data_module"

# End Source File
# Begin Source File

SOURCE=.\modules.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

!ENDIF 

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

# End Source File
# Begin Source File

SOURCE=.\nonfluvial_coeff.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_NONFL=\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\pidlink.mod"\
	".\Release\point_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_NONFL=\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\pidlink.mod"\
	".\Debug\point_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\pidlink.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_PIDLI=\
	".\Release\date_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\point_vars.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_PIDLI=\
	".\Debug\date_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\point_vars.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

F90_MODOUT=\
	"pidlink"

# End Source File
# Begin Source File

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
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_POINT=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	".\Debug\transport_vars.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

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
	".\time_series\Release\date_time.mod"\
	

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
	".\time_series\Debug\date_time.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

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
	".\time_series\Release\date_time.mod"\
	

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
	".\time_series\Debug\date_time.mod"\
	

!ENDIF 

F90_MODOUT=\
	"profile_output_module"

# End Source File
# Begin Source File

SOURCE=.\read_config.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_READ_=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_READ_=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

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
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_READ_H=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\scalars_module.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_SCALA=\
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
	

!ENDIF 

F90_MODOUT=\
	"scalars"

# End Source File
# Begin Source File

SOURCE=.\section.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_SECTI=\
	".\Release\general_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\section_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SECTI=\
	".\Debug\general_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\section_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\section_data.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_SECTIO=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\logicals.mod"\
	".\Release\section_vars.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SECTIO=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\logicals.mod"\
	".\Debug\section_vars.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\section_table.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_SECTION=\
	".\Release\logicals.mod"\
	".\Release\section_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_SECTION=\
	".\Debug\logicals.mod"\
	".\Debug\section_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\svgrp.f90
# End Source File
# Begin Source File

SOURCE=.\table_interp.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_TABLE=\
	".\Release\general_vars.mod"\
	".\Release\linkbc_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_TABLE=\
	".\Debug\general_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\tdg_equation_coeff.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_TDG_E=\
	".\Release\file_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_TDG_E=\
	".\Debug\file_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

F90_MODOUT=\
	"tdg_equation_coeff"

# End Source File
# Begin Source File

SOURCE=.\trans_bc.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

DEP_F90_TRANS=\
	".\Release\date_vars.mod"\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\linkbc_vars.mod"\
	".\Release\logicals.mod"\
	".\time_series\Release\date_time.mod"\
	".\time_series\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_TRANS=\
	".\Debug\date_vars.mod"\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\linkbc_vars.mod"\
	".\Debug\logicals.mod"\
	".\time_series\Debug\date_time.mod"\
	".\time_series\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\write_restart.f90

!IF  "$(CFG)" == "mass1_v082 - Win32 Release"

NODEP_F90_WRITE=\
	".\Release\file_vars.mod"\
	".\Release\general_vars.mod"\
	".\Release\link_vars.mod"\
	".\Release\point_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\transport_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass1_v082 - Win32 Debug"

DEP_F90_WRITE=\
	".\Debug\file_vars.mod"\
	".\Debug\general_vars.mod"\
	".\Debug\link_vars.mod"\
	".\Debug\point_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\transport_vars.mod"\
	

!ENDIF 

# End Source File
# End Target
# End Project
