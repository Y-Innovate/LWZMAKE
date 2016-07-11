//(JOBCARD)                                                             00010000
//*                                                                     00020000
//      EXPORT SYMLIST=*                                                00021000
//         SET LWZMHLQ=(HIGH LEVEL QUALIFIER(S))                        00022000
//         SET MEMBER=LWZMAKE                                           00023000
//*                                                                     00024000
//ASM     EXEC PGM=ASMA90,PARM='ADATA,GOFF,LIST(133)'                   00025001
//SYSLIB    DD DISP=SHR,DSN=SYS1.MACLIB                                 00026000
//          DD DISP=SHR,DSN=SYS1.MODGEN                                 00027000
//          DD DISP=SHR,DSN=SYS1.SCEEMAC                                00028000
//          DD DISP=SHR,DSN=SYS1.SASMMAC2                               00029000
//          DD DISP=SHR,DSN=&LWZMHLQ..ASM                               00030000
//SYSIN     DD DISP=SHR,DSN=&LWZMHLQ..ASM(&MEMBER)                      00040000
//SYSLIN    DD DISP=SHR,DSN=&LWZMHLQ..OBJECT(&MEMBER)                   00050000
//SYSADATA  DD DISP=SHR,DSN=&LWZMHLQ..SYSADATA(&MEMBER)                 00060000
//SYSUT1    DD DSN=&&SYSUT1,SPACE=(4096,(120,120),,,ROUND),             00070000
//             UNIT=SYSALLDA,DCB=BUFNO=1                                00080000
//SYSPRINT  DD DISP=SHR,DSN=&LWZMHLQ..ASM.LISTING(&MEMBER)              00090000
//*                                                                     00100000
//ASMLST  EXEC PGM=IEBGENER                                             00110000
//SYSUT1    DD DISP=SHR,DSN=&LWZMHLQ..ASM.LISTING(&MEMBER)              00120000
//SYSIN     DD DUMMY                                                    00130000
//SYSUT2    DD SYSOUT=*                                                 00140000
//SYSPRINT  DD SYSOUT=*                                                 00150000
//*                                                                     00150100
//*EQAXTR EXEC PGM=EQALANGX,REGION=32M,COND=(0,NE,ASM),                 00150200
//*            PARM='(ASM ERROR'                                        00150300
//*SYSADATA DD DISP=SHR,DSN=&LWZMHLQ..SYSADATA(&MEMBER)                 00150400
//*IDILANGX DD DISP=SHR,DSN=&LWZMHLQ..EQALANGX(&MEMBER)                 00150500
//*SYSPRINT DD SYSOUT=*                                                 00150600
//*                                                                     00150700
//LKED    EXEC PGM=IEWL,COND=(0,NE),                                    00150800
//             PARM='LIST,XREF,RENT,REUS'                               00150900
//SYSLIB    DD DISP=SHR,DSN=SYS1.SCEELKED                               00151000
//          DD DISP=SHR,DSN=SYS1.SCEELKEX                               00152000
//          DD DISP=SHR,DSN=&LWZMHLQ..OBJECT                            00153000
//SYSLIN    DD *,SYMBOLS=EXECSYS                                        00154000
  MODE AMODE(31),RMODE(ANY)                                             00155000
  INCLUDE SYSLIB(&MEMBER)                                               00156000
  NAME &MEMBER(R)                                                       00157000
/*                                                                      00158000
//SYSLMOD   DD DISP=SHR,DSN=&LWZMHLQ..LOADLIB                           00159000
//SYSUT1    DD UNIT=SYSDA,DCB=BLKSIZE=1024,                             00160000
//             SPACE=(1024,(200,20))                                    00170000
//SYSPRINT  DD SYSOUT=*                                                 00180000