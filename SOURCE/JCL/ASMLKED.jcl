//<JOBCARD> <=== FILL IN                                                        
//*-------------------------------------------------------------------*         
//* Thit job assembles and link-edits LWZMAKE without making use of   *         
//* LWZMAKE (there's another job to do just that, but it requires a   *         
//* load module LWZMAKE)                                              *         
//*-------------------------------------------------------------------*         
//      EXPORT SYMLIST=*                                                        
//         SET LWZMHLQ=<LWZMAKE_HLQ> <=== FILL IN                               
//         SET CEEHLQ=CEE            <=== CHECK                                 
//         SET HLAHLQ=HLA            <=== CHECK                                 
//         SET IPVHLQ=ISM332         <=== CHECK                                 
//*-------------------------------------------------------------------*         
//* Instream procedure to assemble a source and generate EQALANGX file          
//*                                                                             
//ASMPROC PROC MEMBER=                                                          
//*                                                                             
//* Assemble a source                                                           
//*                                                                             
//ASM     EXEC PGM=ASMA90,PARM='ADATA,GOFF,LIST(133)'                           
//SYSLIB    DD DISP=SHR,DSN=SYS1.MACLIB                                         
//          DD DISP=SHR,DSN=SYS1.MODGEN                                         
//          DD DISP=SHR,DSN=&CEEHLQ..SCEEMAC                                    
//          DD DISP=SHR,DSN=&HLAHLQ..SASMMAC2                                   
//          DD DISP=SHR,DSN=&LWZMHLQ..ASM                                       
//SYSIN     DD DISP=SHR,DSN=&LWZMHLQ..ASM(&MEMBER)                              
//SYSLIN    DD DISP=SHR,DSN=&LWZMHLQ..OBJECT(&MEMBER)                           
//SYSADATA  DD DISP=SHR,DSN=&LWZMHLQ..SYSADATA(&MEMBER)                         
//SYSUT1    DD DSN=&&SYSUT1,SPACE=(4096,(120,120),,,ROUND),                     
//             UNIT=SYSALLDA,DCB=BUFNO=1                                        
//SYSPRINT  DD DISP=SHR,DSN=&LWZMHLQ..ASM.LISTING(&MEMBER)                      
//*                                                                             
//* Copy assembler listing to SYSOUT                                            
//*                                                                             
//ASMLST  EXEC PGM=IEBGENER                                                     
//SYSUT1    DD DISP=SHR,DSN=&LWZMHLQ..ASM.LISTING(&MEMBER)                      
//SYSIN     DD DUMMY                                                            
//SYSUT2    DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//*                                                                             
//* Generate EQALANGX file                                                      
//*                                                                             
//EQAXTR  EXEC PGM=EQALANGX,REGION=32M,COND=(0,NE,ASM),                         
//             PARM='(ASM ERROR'                                                
//STEPLIB   DD DISP=SHR,DSN=&IPVHLQ..SIPVMODA                                   
//SYSADATA  DD DISP=SHR,DSN=&LWZMHLQ..SYSADATA(&MEMBER)                         
//IDILANGX  DD DISP=SHR,DSN=&LWZMHLQ..EQALANGX(&MEMBER)                         
//SYSPRINT  DD SYSOUT=*                                                         
//*                                                                             
//        PEND                                                                  
//*-------------------------------------------------------------------*         
//*                                                                             
//* Assemble all the object sources                                             
//*                                                                             
//CEEUOPT EXEC ASMPROC,MEMBER=CEEUOPT  * LE runtime options                     
//AVL     EXEC ASMPROC,MEMBER=LWZMAVL  * AVL-tree object                        
//FMG     EXEC ASMPROC,MEMBER=LWZMFMG  * File manager object                    
//INP     EXEC ASMPROC,MEMBER=LWZMINP  * Makefile input objects                 
//LOG     EXEC ASMPROC,MEMBER=LWZMLOG  * Logging object                         
//PRS     EXEC ASMPROC,MEMBER=LWZMPRS  * Parser objects                         
//REX     EXEC ASMPROC,MEMBER=LWZMREX  * REXX caller object                     
//STM     EXEC ASMPROC,MEMBER=LWZMSTM  * Statement objects                      
//STR     EXEC ASMPROC,MEMBER=LWZMSTR  * String and StringBuilder objs          
//TOK     EXEC ASMPROC,MEMBER=LWZMTOK  * Tokenizer objects                      
//USS     EXEC ASMPROC,MEMBER=LWZMUSS  * USS services caller object             
//VCP     EXEC ASMPROC,MEMBER=LWZMVCP  * VARIANT copy routine                   
//MAKE    EXEC ASMPROC,MEMBER=LWZMAKE  * LWZMAKE mainline                       
//*                                                                             
//* Link-edit into LWZMAKE load module                                          
//*                                                                             
//LKED    EXEC PGM=IEWL,COND=(0,NE),                                            
//             PARM='LIST,XREF,RENT,REUS'                                       
//SYSLIB    DD DISP=SHR,DSN=&CEEHLQ..SCEELKED                                   
//          DD DISP=SHR,DSN=&CEEHLQ..SCEELKEX                                   
//          DD DISP=SHR,DSN=&LWZMHLQ..OBJECT                                    
//SYSLIN    DD *                                                                
  MODE AMODE(31),RMODE(ANY)                                                     
  INCLUDE SYSLIB(LWZMAKE)                                                       
  INCLUDE SYSLIB(LWZMAVL)                                                       
  INCLUDE SYSLIB(LWZMFMG)                                                       
  INCLUDE SYSLIB(LWZMINP)                                                       
  INCLUDE SYSLIB(LWZMLOG)                                                       
  INCLUDE SYSLIB(LWZMPRS)                                                       
  INCLUDE SYSLIB(LWZMREX)                                                       
  INCLUDE SYSLIB(LWZMSTM)                                                       
  INCLUDE SYSLIB(LWZMSTR)                                                       
  INCLUDE SYSLIB(LWZMTOK)                                                       
  INCLUDE SYSLIB(LWZMUSS)                                                       
  INCLUDE SYSLIB(LWZMVCP)                                                       
  INCLUDE SYSLIB(CEEUOPT)                                                       
  NAME LWZMAKE(R)                                                               
/*                                                                              
//SYSLMOD   DD DISP=SHR,DSN=&LWZMHLQ..LOAD                                      
//SYSUT1    DD UNIT=SYSDA,DCB=BLKSIZE=1024,                                     
//             SPACE=(1024,(200,20))                                            
//SYSPRINT  DD SYSOUT=*                                                         
