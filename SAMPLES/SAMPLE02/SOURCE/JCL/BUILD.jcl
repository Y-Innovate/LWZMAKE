//*-------------------------------------------------------------------*         
//* SAMPLE02 REXX WITH PARM                                           *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=LWZMAKE                                                      
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//LWZMINP   DD *                                                                
# Makefile for SAMPLE02                                                         
                                                                                
.PHONY JUST_ECHO                                                                
JUST_ECHO :                                                                     
- CALL JUSTECHO Anything after the REXX name is parameter data                  
- CALL JUSTECHO Any line can be continued with a continuation\                  
- character                                                                     
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
