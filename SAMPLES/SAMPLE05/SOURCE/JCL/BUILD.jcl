//*-------------------------------------------------------------------*         
//* SAMPLE05 CONCATENATE TO VARIABLE, MULTIPLE TGTS AND $@            *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=LWZMAKE                                                      
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSEXEC   DD DISP=SHR,DSN=@@SAMPLE05@@.EXEC                                   
//LWZMINP   DD *                                                                
#* Makefile for SAMPLE05                                                        
                                                                                
hlq      := @@SAMPLE05@@                                                        
sometgts := $(hlq).NEWDS1                                                       
sometgts := $(sometgts) $(hlq).NEWDS2                                           
                                                                                
.PHONY ALL                                                                      
ALL : $(sometgts)                                                               
                                                                                
$(sometgts) :                                                                   
- CALL ALLOCDS $@                                                               
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
