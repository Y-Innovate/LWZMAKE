//*-------------------------------------------------------------------*         
//* JCL to invoke LWZMAKE in ISPF to build LWZMAKE                    *         
//*                                                                   *         
//* Use <LWZMAKE_gitdir>/SOURCE/build.sh to run the build. It updates *         
//* this JCL by filling in @@HOMEDIR@@ and @@GITDIR@@ and prepends    *         
//* JOBSTMT.jcl before submitting it.                                 *         
//*                                                                   *         
//* Make sure you copy JOBSTMT.jcl.template to JOBSTMT.jcl and change *         
//* it according to your needs.                                       *         
//*-------------------------------------------------------------------*         
//ZMAKE   EXEC PROC=ISPFMAKE,                                                   
//             LWZMHLQ=&LWZMHLQ,                                                
//             MAKEPARM='-t BUILD_ALL',                                         
//*            MAKEFILE=&YDMOHLQ..MAKEFILE(BUILD),                              
//             EXECLIB=&LWZMHLQ..EXEC,                                          
//             DYNAMNBR.ZMAKE=100                                               
//ZMAKE.LWZMINP DD *,SYMBOLS=EXECSYS                                            
# build LWZMAKE using LWZMAKE                                                   
                                                                                
.USSHOME = @@HOMEDIR@@  # <== change if you want a different home dir           
                                                                                
CEEHLQ          := CEE  # <== set to your LE data set HLQ                       
HLAHLQ          := HLA  # <== set to your HLASM data set HLQ                    
                                                                                
gitdir          := @@GITDIR@@                                                   
                                                                                
feature         := ${sh cd $(gitdir);git branch | \                             
                     grep -E "^\\* (.*)$" | cut -d' ' -f2 }                     
feature_upper   := ${sh echo "$(feature)"|tr 'a-z' 'A-Z'}                       
                                                                                
hlq             := &LWZMHLQ..$(feature_upper)  # <== choose a HLQ               
                                                                                
execlib         := $(hlq).EXEC                                                  
asmlib          := $(hlq).ASM                                                   
asmlstlib       := $(asmlib).LISTING                                            
cpylib          := $(hlq).COPY                                                  
jcllib          := $(hlq).JCL                                                   
objlib          := $(hlq).OBJECT                                                
sysadatalib     := $(hlq).SYSADATA                                              
eqalangxlib     := $(hlq).EQALANGX                                              
lkedlib         := $(hlq).LKED                                                  
loadlib         := $(hlq).LOAD                                                  
syslib_asma     := SYS1.MACLIB SYS1.MODGEN $(CEEHLQ).SCEEMAC\                   
                   $(HLAHLQ).SASMMAC2 $(cpylib)                                 
syslib_lked     := $(CEEHLQ).SCEELKED $(objlib)                                 
                                                                                
recfmFB80       := $(execlib) $(asmlib) $(cpylib) $(jcllib) $(objlib)\          
                   $(lkedlib)                                                   
recfmFBA133     := $(asmlstlib)                                                 
recfmVB32756    := $(sysadatalib)                                               
recfmVB1562     := $(eqalangxlib)                                               
recfmU          := $(loadlib)                                                   
                                                                                
execdir         := $(gitdir)/EXEC                                               
asmdir          := $(gitdir)/ASM                                                
cpydir          := $(gitdir)/COPY                                               
jcldir          := $(gitdir)/JCL                                                
lkeddir         := $(gitdir)/LKED                                               
                                                                                
execfiles       := ${sh cd $(execdir);find *.rexx -prune -type f}               
execmems        := ${stripext $(execfiles)}                                     
                                                                                
asmfiles        := ${sh cd $(asmdir);find *.asm -prune -type f}                 
asmmems         := ${stripext $(asmfiles)}                                      
                                                                                
cpyfiles        := ${sh cd $(cpydir);find *.asm -prune -type f}                 
cpymems         := ${stripext $(cpyfiles)}                                      
                                                                                
jclfiles        := ${sh cd $(jcldir);find *.jcl -prune -type f}                 
jclmems         := ${stripext $(jclfiles)}                                      
                                                                                
lkedfiles       := ${sh cd $(lkeddir);find *.lked -prune -type f}               
lkedmems        := ${stripext $(lkedfiles)}                                     
                                                                                
exectgts        := ${addpdsname $(execlib),$(execmems)}                         
asmtgts         := ${addpdsname $(asmlib),$(asmmems)}                           
cpytgts         := ${addpdsname $(cpylib),$(cpymems)}                           
jcltgts         := ${addpdsname $(jcllib),$(jclmems)}                           
lkedtgts        := ${addpdsname $(lkedlib),$(lkedmems)}                         
objtgts         := ${addpdsname $(objlib),$(asmmems)}                           
loadtgts        := ${addpdsname $(loadlib),$(lkedmems)}                         
                                                                                
.PHONY BUILD_ALL                                                                
BUILD_ALL : $(recfmFB80) $(recfmFBA133) $(recfmVB32756) $(recfmVB1562)\         
            $(recfmU)\                                                          
            $(exectgts) $(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)\           
            $(loadtgts)                                                         
                                                                                
$(exectgts) : $(execdir)/$%.rexx                                                
- CALL OGET '$(execdir)/$%.rexx' '$@' TEXT CONVERT(YES)                         
                                                                                
$(cpytgts) : $(cpydir)/$%.asm                                                   
- CALL OGET '$(cpydir)/$%.asm' '$@' TEXT CONVERT(YES)                           
- SH cd $(asmdir);touch $(asmfiles)                                             
                                                                                
$(asmtgts) : $(asmdir)/$%.asm                                                   
- CALL OGET '$(asmdir)/$%.asm' '$@' TEXT CONVERT(YES)                           
                                                                                
$(jcltgts) : $(jcldir)/$%.jcl                                                   
- CALL OGET '$(jcldir)/$%.jcl' '$@' TEXT CONVERT(YES)                           
                                                                                
$(lkedtgts) : $(lkeddir)/$%.lked                                                
- CALL OGET '$(lkeddir)/$%.lked' '$@' TEXT CONVERT(YES)                         
                                                                                
$(objtgts) : $(asmlib)($%)                                                      
- CALL ASMA SYSIN($(asmlib)($%)) SYSLIN($(objlib)($%))\                         
-           SYSLIB($(syslib_asma)) SYSADATA($(sysadatalib)($%))\                
-           SYSPRINT($(asmlstlib)($%)) PARM(ADATA,GOFF,LIST(133))\              
-           PRINTSUCCESS(NO)                                                    
- CALL TOUCHMEM DATASET($(objlib)($%))                                          
- CALL TOUCHMEM DATASET($(asmlstlib)($%))                                       
- CALL TOUCHMEM DATASET($(sysadatalib)($%))                                     
- CALL EQALANGX SYSADATA($(sysadatalib)($%))\                                   
-               IDILANGX($(eqalangxlib)($%)) PARM(ASM ERROR)                    
- CALL TOUCHMEM DATASET($(eqalangxlib)($%))                                     
                                                                                
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
