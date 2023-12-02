//*-------------------------------------------------------------------*         
//* SAMPLE09 DEPLOY SAMPLE USING IEBCOPY                              *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PGM=LWZMAKE                                                      
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC                                      
//LWZMINP   DD *                                                                
#* Makefile for SAMPLE09                                                        
                                                                                
hlq     := @@SAMPLE09@@                                                         
srcpds  := $(hlq).SRCPDS                                                        
tgtpds  := $(hlq).TGTPDS                                                        
newmems := $(memberlist $(srcpds))                                              
tgts    := $(addpdsname $(tgtpds), $(newmems))                                  
                                                                                
.PHONY DEPLOY                                                                   
DEPLOY : $(tgts)                                                                
                                                                                
$(tgts) : $(addpdsname $(srcpds), $%)                                           
- CALL IEBCOPY PDSIN($(srcpds)) PDSOUT($(tgtpds)) MEMBER($%)                    
/*                                                                              
//LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)                                
//SYSTSPRT  DD SYSOUT=*                                                         
