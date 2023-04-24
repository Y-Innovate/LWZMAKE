//*-------------------------------------------------------------------*         
//* SAMPLE07 FIRST REAL MAKEFILE ASSEMBLING AND LINK-EDITING, $% VAR  *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=IKJEFT1B                                                     
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//SYSTSIN   DD *,SYMBOLS=EXECSYS                                                
CALL '&LWZMHLQ..LOAD(LWZMAKE)' '-t ALL' ASIS                                    
//LWZMINP   DD *                                                                
# Makefile for SAMPLE07                                                         
                                                                                
hlq         := @@SAMPLE07@@                                                     
srclib      := $(hlq).ASM                                                       
lkedlib     := $(hlq).LKED                                                      
objlib      := $(hlq).OBJECT                                                    
loadlib     := $(hlq).LOAD                                                      
syslib_asma := SYS1.MACLIB HLA.SASMMAC2   # <=== CHECK                          
syslib_lked := CEE.SCEELKED $(objlib)     # <=== CHECK                          
mods        := $(loadlib)(SHA1) $(loadlib)(TSHA1)                               
                                                                                
.PHONY ALL                                                                      
ALL : $(mods)                                                                   
                                                                                
$(mods) : $(loadlib) $(objlib) $(srclib)($%) $(lkedlib)($%)                     
- CALL ASMA SYSIN($(srclib)($%)) SYSLIN($(objlib)($%))\                         
-           SYSLIB($(syslib_asma))                                              
- CALL LKED SYSLIN($(lkedlib)($%)) SYSLMOD($(loadlib)($%))\                     
-           SYSLIB($(syslib_lked)) PARM(LIST,XREF,RENT,REUS)                    
                                                                                
$(objlib) :                                                                     
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B) LRECL(80)\                     
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
$(loadlib) :                                                                    
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(U) LRECL(0) BLKSIZE(32760)\         
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
