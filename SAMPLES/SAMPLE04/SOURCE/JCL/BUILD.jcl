//*-------------------------------------------------------------------*         
//* SAMPLE04 VARIABLES IN VARIABLES AND REAL (NON-PHONY) TARGET       *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=LWZMAKE                                                      
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSEXEC   DD DISP=SHR,DSN=@@SAMPLE04@@.EXEC                                   
//LWZMINP   DD *                                                                
#* Makefile for SAMPLE04                                                        
                                                                                
hlq     = @@SAMPLE04@@                                                          
sometgt = $(hlq).NEWDS                                                          
                                                                                
$(sometgt) :                                                                    
- CALL ALLOCDS $(sometgt)                                                       
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
