//*-------------------------------------------------------------------*         
//* SAMPLE08 RUN LWZMAKE FROM ISPF, TRY CLEAN_ALL AND ALL TARGETS     *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PROC=ISPFMAKE,                                                   
//             LWZMHLQ=&LWZMHLQ,                                                
//             MAKEPARM='-T CLEAN_ALL',                                         
//             EXECLIB=&LWZMHLQ..EXEC                                           
//ZMAKE.LWZMINP DD *                                                            
#* Makefile for SAMPLE08                                                        
                                                                                
hlq         := @@SAMPLE08@@                                                     
srclib      := $(hlq).COB                                                       
lkedlib     := $(hlq).LKED                                                      
objlib      := $(hlq).OBJECT                                                    
loadlib     := $(hlq).LOAD                                                      
syslib_cob  := CEE.SCEESAMP               #* <=== CHECK                         
syslib_lked := CEE.SCEELKED $(objlib)     #* <=== CHECK                         
mods        := $(memberlist $(srclib))                                          
objects     := $(addpdsname $(objlib),  $(mods))                                
loads       := $(addpdsname $(loadlib), $(mods))                                
                                                                                
.PHONY CLEAN_ALL                                                                
CLEAN_ALL : CLEAN ALL                                                           
                                                                                
.PHONY ALL                                                                      
ALL : $(loads)                                                                  
                                                                                
$(loads) : $(loadlib) $(objlib)($%) $(lkedlib)($%)                              
- CALL LKED SYSLIN($(lkedlib)($%)) SYSLMOD($(loadlib)($%))+                     
-           SYSLIB($(syslib_lked)) PARM(LIST,XREF,RENT,REUS)                    
                                                                                
$(objects) : $(objlib) $(srclib)($%)                                            
- CALL COBOL6 SYSIN($(srclib)($%)) SYSLIN($(objlib)($%))+                       
-             SYSLIB($(syslib_cob))                                             
- CALL TOUCHMEM DATASET($(objlib)($%))                                          
                                                                                
$(objlib) :                                                                     
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B) LRECL(80)+                     
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
$(loadlib) :                                                                    
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(U) LRECL(0) BLKSIZE(32760)+         
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
.PHONY CLEAN                                                                    
CLEAN :                                                                         
- CALL SAFEDEL $(loads) $(objects)                                              
/*                                                                              
