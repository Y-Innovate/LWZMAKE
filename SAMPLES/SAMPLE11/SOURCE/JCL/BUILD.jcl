//*-------------------------------------------------------------------*         
//* SAMPLE11 CUSTOM WRITTEN FUNCTION FOR DETERMINING NEXT DS NAME     *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=IKJEFT1B                                                     
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//          DD DISP=SHR,DSN=@@SAMPLE11@@.EXEC                                   
//SYSTSIN   DD *,SYMBOLS=EXECSYS                                                
CALL '&LWZMHLQ..LOAD(LWZMAKE)'                                                  
//LWZMINP   DD *                                                                
#* Makefile for SAMPLE11                                                        
                                                                                
hlq        := @@SAMPLE11@@                                                      
mask       := $(hlq).N*                                                         
nextnr     := ${function NEXTNR,$(mask)}                                        
                                                                                
.PHONY ALLOC                                                                    
ALLOC :                                                                         
- nextds := $(hlq).N$(nextnr)                                                   
- CALL TSOCMD ALLOC DATASET('$(nextds)') NEW RECFM(F,B) LRECL(80)+              
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$(nextds)')                                         
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
