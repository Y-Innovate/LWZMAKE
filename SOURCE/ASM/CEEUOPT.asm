*/****************************************************************/
*/* LICENSED MATERIALS - PROPERTY OF IBM                         */
*/*                                                              */
*/* 5650-ZOS                                                     */
*/*                                                              */
*/*     COPYRIGHT IBM CORP. 1991, 2012                           */
*/*                                                              */
*/* US GOVERNMENT USERS RESTRICTED RIGHTS - USE,                 */
*/* DUPLICATION OR DISCLOSURE RESTRICTED BY GSA ADP              */
*/* SCHEDULE CONTRACT WITH IBM CORP.                             */
*/*                                                              */
*/* STATUS = HLE7790                                             */
*/****************************************************************/
CEEUOPT  CSECT
CEEUOPT  AMODE ANY
CEEUOPT  RMODE ANY
         CEEXOPT ABPERC=(NONE),                                        X
               ABTERMENC=(ABEND),                                      X
               AIXBLD=(OFF),                                           X
               ALL31=(ON),                                             X
               ANYHEAP=(16K,8K,ANYWHERE,FREE),                         X
               BELOWHEAP=(8K,4K,FREE),                                 X
               CBLOPTS=(ON),                                           X
               CBLPSHPOP=(ON),                                         X
               CBLQDA=(OFF),                                           X
               CEEDUMP=(60,SYSOUT=*,FREE=END,SPIN=UNALLOC),            X
               CHECK=(ON),                                             X
               COUNTRY=(US),                                           X
               DEBUG=(OFF),                                            X
               DEPTHCONDLMT=(10),                                      X
               DYNDUMP=(*USERID,NODYNAMIC,TDUMP),                      X
               ENVAR=('_BPX_SHAREAS=MUST'),                            X
               ERRCOUNT=(0),                                           X
               ERRUNIT=(6),                                            X
               FILEHIST=(ON),                                          X
               FILETAG=(NOAUTOCVT,NOAUTOTAG),                          X
               HEAP=(32K,32K,ANYWHERE,KEEP,8K,4K),                     X
               HEAPCHK=(OFF,1,0,0,0,1024,0,1024,0),                    X
               HEAPPOOLS=(OFF,8,10,32,10,128,10,256,10,1024,10,2048,   X
               10,0,10,0,10,0,10,0,10,0,10,0,10),                      X
               HEAPZONES=(0,ABEND,0,ABEND),                            X
               INFOMSGFILTER=(OFF,,,,),                                X
               INQPCOPN=(ON),                                          X
               INTERRUPT=(OFF),                                        X
               LIBSTACK=(4K,4K,FREE),                                  X
               MSGFILE=(SYSOUT,FBA,121,0,NOENQ),                       X
               MSGQ=(15),                                              X
               NATLANG=(ENU),                                          X
               NOAUTOTASK=,                                            X
               NOTEST=(ALL,*,PROMPT,INSPPREF),                         X
               NOUSRHDLR=(''),                                         X
               OCSTATUS=(ON),                                          X
               PAGEFRAMESIZE=(4K,4K,4K),                               X
               PC=(OFF),                                               X
               PLITASKCOUNT=(20),                                      X
               POSIX=(ON),                                             X
               PROFILE=(OFF,''),                                       X
               PRTUNIT=(6),                                            X
               PUNUNIT=(7),                                            X
               RDRUNIT=(5),                                            X
               RECPAD=(OFF),                                           X
               RPTOPTS=(OFF),                                          X
               RPTSTG=(OFF),                                           X
               RTEREUS=(OFF),                                          X
               SIMVRD=(OFF),                                           X
               STACK=(128K,128K,ANYWHERE,KEEP,512K,128K),              X
               STORAGE=(NONE,NONE,NONE,0K),                            X
               TERMTHDACT=(TRACE,,96),                                 X
               THREADHEAP=(4K,4K,ANYWHERE,KEEP),                       X
               THREADSTACK=(OFF,4K,4K,ANYWHERE,KEEP,128K,128K),        X
               TRACE=(OFF,4K,DUMP,LE=0),                               X
               TRAP=(ON,SPIE),                                         X
               UPSI=(00000000),                                        X
               VCTRSAVE=(OFF),                                         X
               XPLINK=(OFF),                                           X
               XUFLOW=(AUTO)
         END
