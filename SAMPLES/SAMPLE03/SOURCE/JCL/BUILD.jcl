//*-------------------------------------------------------------------*         
//* SAMPLE03 USE OF A VARIABLE                                        *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=LWZMAKE                                                      
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//LWZMINP   DD *                                                                
# Makefile for SAMPLE03                                                         
                                                                                
somevar = Hello world!                                                          
                                                                                
.PHONY JUST_ECHO                                                                
JUST_ECHO :                                                                     
- CALL JUSTECHO $(somevar)                                                      
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
