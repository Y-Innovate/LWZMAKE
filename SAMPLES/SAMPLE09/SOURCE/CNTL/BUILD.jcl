//(JOBCARD)   <=== FILL IN
//*
//      EXPORT SYMLIST=*
//         SET LWZMHLQ=(HLQ FOR LWZMAKE LOAD MOD)  <=== FILL IN
//         SET SAMPHLQ=(HLQ FOR SAMPLE09)          <=== FILL IN
//      JCLLIB ORDER=(&SAMPHLQ..CNTL)
//*
//ZMAKE   EXEC PGM=LWZMAKE
//STEPLIB   DD DISP=SHR,DSN=&LWZMHLQ..LOADLIB
//SYSEXEC   DD DISP=SHR,DSN=&LWZMHLQ..EXEC
//MAKEFILE  DD DISP=SHR,DSN=&SAMPHLQ..CNTL(MAKEFILE)
//LWZMTRC   DD SYSOUT=*
//LWZMRPT   DD SYSOUT=*
//SYSTSPRT  DD SYSOUT=*
