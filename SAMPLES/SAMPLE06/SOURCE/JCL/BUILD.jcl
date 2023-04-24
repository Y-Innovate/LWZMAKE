//*-------------------------------------------------------------------*         
//* SAMPLE06 RUNNING LWZMAKE FROM TSO AND USING TSOCMD                *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=IKJEFT1B                                                     
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//SYSTSIN   DD *,SYMBOLS=EXECSYS                                                
CALL '&LWZMHLQ..LOAD(LWZMAKE)' '-t ALL' ASIS                                    
//LWZMINP   DD *                                                                
# Makefile for SAMPLE06                                                         
                                                                                
hlq      := @@SAMPLE06@@                                                        
objlib   := $(hlq).OBJECT                                                       
loadlib  := $(hlq).LOAD                                                         
sometgts := $(objlib) $(loadlib)                                                
                                                                                
.PHONY ALL                                                                      
ALL : $(sometgts)                                                               
                                                                                
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
