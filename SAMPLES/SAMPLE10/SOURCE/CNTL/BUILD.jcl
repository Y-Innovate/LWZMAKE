//(JOBCARD)   <=== FILL IN
//*
//      EXPORT SYMLIST=*
//         SET LWZMHLQ=(HLQ FOR LWZMAKE LOAD MOD)  <=== FILL IN
//         SET SAMPHLQ=(HLQ FOR SAMPLE10)          <=== FILL IN
//      JCLLIB ORDER=(&SAMPHLQ..CNTL)
//*
//ZMAKE   EXEC PGM=IKJEFT1B
//SYSEXEC   DD DISP=SHR,DSN=&SAMPHLQ..EXEC
//SYSTSIN   DD *,SYMBOLS=EXECSYS
CALL '&LWZMHLQ..LOADLIB(LWZMAKE)'
//MAKEFILE  DD DISP=SHR,DSN=&SAMPHLQ..CNTL(MAKEFILE)
//LWZMTRC   DD SYSOUT=*
//LWZMRPT   DD SYSOUT=*
//SYSTSPRT  DD SYSOUT=*
