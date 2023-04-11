//YBTKSA   JOB 'BUILD LWZMAKE',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID                
//*                                                                             
//      EXPORT SYMLIST=*                                                        
//         SET LWZMHLQ=LWZM020                                                  
//      JCLLIB ORDER=(&LWZMHLQ..CNTL)                                           
//*                                                                             
//ZMAKE   EXEC PROC=ISPFMAKE,                                                   
//             LWZMHLQ=&LWZMHLQ,                                                
//             MAKEPARM='-t BUILD_ALL',                                         
//*            MAKEFILE=&YDMOHLQ..MAKEFILE(BUILD),                              
//             EXECLIB=&LWZMHLQ..EXEC,                                          
//             DYNAMNBR.ZMAKE=100                                               
//ZMAKE.LWZMINP DD *                                                            
# build LWZMAKE using LWZMAKE                                                   
                                                                                
.USSHOME = /u/yin/ybtks                                                         
                                                                                
gitdir          := /u/yin/ybtks/LWZMAKE                                         
                                                                                
feature         := ${sh cd $(gitdir);git branch | \                             
                     grep -E "^\\* (.*)$" | cut -d' ' -f2 }                     
feature_upper   := ${sh echo "$(feature)"|tr 'a-z' 'A-Z'}                       
                                                                                
hlq             := LWZM020.$(feature_upper)                                     
                                                                                
asmlib          := $(hlq).ASM                                                   
asmlstlib       := $(asmlib).LISTING                                            
jcllib          := $(hlq).JCL                                                   
objlib          := $(hlq).OBJECT                                                
sysadatalib     := $(hlq).SYSADATA                                              
eqalangxlib     := $(hlq).EQALANGX                                              
lkedlib         := $(hlq).LKED                                                  
loadlib         := $(hlq).LOAD                                                  
syslib_asma     := SYS1.MACLIB SYS1.MODGEN CEE.SCEEMAC HLA.SASMMAC2\            
                   $(asmlib)                                                    
syslib_lked     := CEE.SCEELKED $(objlib)                                       
                                                                                
recfmFB80       := $(asmlib) $(jcllib) $(objlib) $(lkedlib)                     
recfmFBA133     := $(asmlstlib)                                                 
recfmVB32756    := $(sysadatalib)                                               
recfmVB1562     := $(eqalangxlib)                                               
recfmU          := $(loadlib)                                                   
                                                                                
asmdir          := $(gitdir)/SOURCE/ASM                                         
jcldir          := $(gitdir)/SOURCE/JCL                                         
lkeddir         := $(gitdir)/SOURCE/LKED                                        
                                                                                
asmfiles        := ${sh cd $(asmdir);find * -prune -type f}                     
asmmems         := ${stripext $(asmfiles)}                                      
                                                                                
jclfiles        := ${sh cd $(jcldir);find * -prune -type f}                     
jclmems         := ${stripext $(jclfiles)}                                      
                                                                                
lkedfiles       := ${sh cd $(lkeddir);find * -prune -type f}                    
lkedmems        := ${stripext $(lkedfiles)}                                     
                                                                                
asmsrcs         := CEEUOPT LWZMAKE LWZMAVL LWZMFMG LWZMINP LWZMLOG \            
                   LWZMPRS LWZMREX LWZMSTM LWZMSTR LWZMTOK LWZMUSS \            
                   LWZMVCP                                                      
                                                                                
asmtgts         := ${addpdsname $(asmlib),$(asmmems)}                           
jcltgts         := ${addpdsname $(jcllib),$(jclmems)}                           
lkedtgts        := ${addpdsname $(lkedlib),$(lkedmems)}                         
objtgts         := ${addpdsname $(objlib),$(asmsrcs)}                           
loadtgts        := ${addpdsname $(loadlib),$(lkedmems)}                         
                                                                                
.PHONY BUILD_ALL                                                                
BUILD_ALL : $(recfmFB80) $(recfmFBA133) $(recfmVB32756) $(recfmVB1562)\         
            $(recfmU)\                                                          
            $(asmtgts) $(jcltgts) $(lkedtgts)\                                  
            LIST_CPY\                                                           
            $(loadtgts)                                                         
                                                                                
$(asmtgts) : $(asmdir)/$%.asm                                                   
- CALL OGET '$(asmdir)/$%.asm' '$@' TEXT CONVERT(YES)                           
                                                                                
$(jcltgts) : $(jcldir)/$%.jcl                                                   
- CALL OGET '$(jcldir)/$%.jcl' '$@' TEXT CONVERT(YES)                           
                                                                                
$(lkedtgts) : $(lkeddir)/$%.lked                                                
- CALL OGET '$(lkeddir)/$%.lked' '$@' TEXT CONVERT(YES)                         
                                                                                
.PHONY LIST_CPY                                                                 
LIST_CPY :                                                                      
- cpysrcs := ${function NOTIN,SET1(${memberlist $(asmlib)})\                    
-                             SET2($(asmsrcs))}                                 
- cpysrcs := ${addpdsname $(asmlib),$(cpysrcs)}                                 
                                                                                
$(objtgts) : $(asmlib)($%) $(cpysrcs)                                           
- CALL ASMA SYSIN($(asmlib)($%)) SYSLIN($(objlib)($%))\                         
-           SYSLIB($(syslib_asma)) SYSADATA($(sysadatalib)($%))\                
-           SYSPRINT($(asmlstlib)($%)) PARM(ADATA,GOFF,LIST(133))\              
-           PRINTSUCCESS(NO)                                                    
- CALL TOUCHMEM DATASET($(objlib)($%))                                          
- CALL TOUCHMEM DATASET($(asmlstlib)($%))                                       
- CALL TOUCHMEM DATASET($(sysadatalib)($%))                                     
                                                                                
$(loadtgts) : $(objtgts) $(lkedlib)($%)                                         
- CALL LKED SYSLIN($(lkedlib)($%)) SYSLMOD($(loadlib)($%))\                     
-           SYSLIB($(syslib_lked)) PARM(LIST,XREF,RENT,REUS)\                   
-           PRINTSUCCESS(NO)                                                    
                                                                                
$(recfmFB80) :                                                                  
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B) LRECL(80)\                     
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
$(recfmFBA133) :                                                                
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B,A) LRECL(133)\                  
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
$(recfmVB32756) :                                                               
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(V,B) LRECL(32756)\                  
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
$(recfmVB1562) :                                                                
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(V,B) LRECL(1562)\                   
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
                                                                                
$(recfmU) :                                                                     
- CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(U) LRECL(0) BLKSIZE(32760)\         
-             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)                   
- CALL TSOCMD FREE DATASET('$@')                                                
/*                                                                              
