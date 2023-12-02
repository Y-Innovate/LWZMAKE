//*-------------------------------------------------------------------*         
//* SAMPLE01 HELLO WORLD                                              *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=LWZMAKE                                                      
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSEXEC   DD DISP=SHR,DSN=@@SAMPLE01@@.EXEC                                   
//LWZMINP   DD *                                                                
#* Makefile for SAMPLE01                                                        
                                                                                
.PHONY HELLO_WORLD                                                              
HELLO_WORLD :                                                                   
- CALL HELLO                                                                    
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
