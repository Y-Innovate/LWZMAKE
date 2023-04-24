//*-------------------------------------------------------------------*         
//* SAMPLE10 PROFILING MEMBERS BEFORE SUBMITTING THEM                 *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=IKJEFT1B                                                     
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//SYSTSIN   DD *,SYMBOLS=EXECSYS                                                
CALL '&LWZMHLQ..LOAD(LWZMAKE)'                                                  
//LWZMINP   DD *                                                                
# Makefile for SAMPLE10                                                         
                                                                                
hlq        := @@SAMPLE10@@                                                      
template   := $(hlq).TEMPLATE(WHYJCL)                                           
resolves   := ${addpdsname $(hlq).JCL, \                                        
                 ${memberlist $(hlq).JCL, RESOL} }                              
temppds    := ${function TEMPPDS}                                               
                                                                                
.PHONY UPDATE                                                                   
UPDATE : $(resolves)                                                            
                                                                                
$(resolves) : 1                                                                 
- CALL UPDCOPY DSIN($(template)) DSOUT($(temppds)($%)) UPDWITH($@)              
- CALL SUBMIT $(temppds)($%)                                                    
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
