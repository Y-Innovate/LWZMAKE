//<JOBCARD> <=== FILL IN                                                        
//*-------------------------------------------------------------------*         
//* This job allocates the necessary PDS's and copies USS files into  *         
//* them for an initial build of LWZMAKE with LWZMAKE itself.         *         
//*-------------------------------------------------------------------*         
//      EXPORT SYMLIST=*                                                        
//         SET LWZMHLQ=<LWZMAKE_HLQ>           <=== FILL IN                     
//         SET BRANCH='MASTER.'                <=== CHECK                       
//         SET GITDIR='<your Git LWZMAKE dir>' <=== FILL IN                     
//*-------------------------------------------------------------------*         
//*                                                                             
//* Allocate minimum needed PDS's                                               
//*                                                                             
//ALLOC   EXEC PGM=IEFBR14                                                      
//DD1       DD DISP=(MOD,CATLG),DSN=&LWZMHLQ..&BRANCH.EXEC,                     
//             DCB=(RECFM=FB,LRECL=80,DSORG=PO),DSNTYPE=LIBRARY,                
//             UNIT=SYSALLDA,SPACE=(CYL,(1,1))                                  
//DD2       DD DISP=(MOD,CATLG),DSN=&LWZMHLQ..&BRANCH.JCL,                      
//             DCB=(RECFM=FB,LRECL=80,DSORG=PO),DSNTYPE=LIBRARY,                
//             UNIT=SYSALLDA,SPACE=(CYL,(1,1))                                  
//DD3       DD DISP=(MOD,CATLG),DSN=&LWZMHLQ..&BRANCH.LOAD,                     
//             DCB=(RECFM=U,LRECL=0,BLKSIZE=32760,DSORG=PO),                    
//             DSNTYPE=LIBRARY,UNIT=SYSALLDA,SPACE=(CYL,(1,1))                  
//*-------------------------------------------------------------------*         
//*                                                                             
//* Copy files                                                                  
//*                                                                             
//COPY    EXEC PGM=IKJEFT1B                                                     
//SYSPROC   DD DISP=SHR,DSN=SYS1.SBPXEXEC                                       
//SYSTSIN   DD *,SYMBOLS=EXECSYS                                                
OGET '&GITDIR/SOURCE/EXEC/ASMA.rexx' -                                          
     '&LWZMHLQ..&BRANCH.EXEC(ASMA)' -                                           
     TEXT CONVERT(YES)                                                          
OGET '&GITDIR/SOURCE/EXEC/EQALANGX.rexx' -                                      
     '&LWZMHLQ..&BRANCH.EXEC(EQALANGX)' -                                       
     TEXT CONVERT(YES)                                                          
OGET '&GITDIR/SOURCE/EXEC/LKED.rexx' -                                          
     '&LWZMHLQ..&BRANCH.EXEC(LKED)' -                                           
     TEXT CONVERT(YES)                                                          
OGET '&GITDIR/SOURCE/EXEC/TOUCHMEM.rexx' -                                      
     '&LWZMHLQ..&BRANCH.EXEC(TOUCHMEM)' -                                       
     TEXT CONVERT(YES)                                                          
OGET '&GITDIR/SOURCE/EXEC/TSOCMD.rexx' -                                        
     '&LWZMHLQ..&BRANCH.EXEC(TSOCMD)' -                                         
     TEXT CONVERT(YES)                                                          
OSHELL cp '&GITDIR/BINARY/LOAD/LWZMAKE.load' -                                  
          "//'&LWZMHLQ..&BRANCH.LOAD(LWZMAKE)'"                                 
/*                                                                              
//SYSTSPRT  DD SYSOUT=*                                                         
