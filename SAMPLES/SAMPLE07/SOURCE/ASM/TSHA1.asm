*********************************************************************** 00010001
* Program    : TSHA1                                                  * 00020001
*                                                                     * 00030001
* Description: Test SHA1 routine                                      * 00040001
*********************************************************************** 00110001
         TITLE 'TSHA1'                                                  00120001
*                                                                       00130001
         COPY  ASMMSP                                                   00140001
*                                                                       00150001
TSHA1    AMODE 31                                                       00160001
TSHA1    RMODE ANY                                                      00170001
TSHA1    CSECT                                                          00180001
         STM   R14,R12,12(R13)   * Save callers registers               00190001
         BASR  R10,R0            * Set R11 to next instruction          00200001
TSHA1#   LA    R11,4095(,R10)    * Setup R12 as second using            00210001
         LA    R11,1(,R11)       *   base register                      00220001
         USING TSHA1#,R10,R11    * Establish addressing                 00230001
         GETMAIN RU,LV=WALEN                                            00250001
         ST    R13,4(R1)         * Backward chain callers SA            00260001
         ST    R1,8(R13)         * Forward chain my SA                  00270001
         LR    R13,R1            * Point R13 to my SA                   00280001
         USING WORKAREA,R13                                             00290001
*                                                                       00310001
         BAL   R8,INIT                                                  00390001
*                                                                       00391002
         MVI   PAR_FUNC,PAR_FUNC_INIT                                   00392002
         LA    R1,PAR_SHA1                                              00392104
         ST    R1,PAR_SHA1_PTR                                          00392204
         LA    R1,PAR_SHA1_PTR                                          00392304
         L     R15,=V(SHA1)                                             00393002
         BASR  R14,R15                                                  00394002
*                                                                       00400002
         LA    R7,MYSTRING                                              00410007
         ST    R7,PAR_BUFFER_PTR                                        00420002
         MVC   PAR_BYTE_COUNT,=F'100'                                   00421010
*                                                                       00430001
         MVI   PAR_FUNC,PAR_FUNC_UPDATE                                 00430202
         LA    R1,PAR_SHA1                                              00430304
         ST    R1,PAR_SHA1_PTR                                          00430404
         LA    R1,PAR_SHA1_PTR                                          00430504
         L     R15,=V(SHA1)                                             00430602
         BASR  R14,R15                                                  00430702
*                                                                       00430802
         MVI   PAR_FUNC,PAR_FUNC_FINAL                                  00431002
         LA    R1,PAR_SHA1                                              00431104
         ST    R1,PAR_SHA1_PTR                                          00431204
         LA    R1,PAR_SHA1_PTR                                          00431304
         L     R15,=V(SHA1)                                             00431402
         BASR  R14,R15                                                  00431502
*                                                                       00431602
         MVC   WTOLEN,=H'24'                                            00431708
         MVC   WTOFIL,=H'0'                                             00431802
         MVC   WTOTEXT,PAR_DIGEST                                       00431902
         WTO   MF=(E,WTOBLOCK),ROUTCDE=11,DESC=7                        00432002
*                                                                       00433002
*         BAL   R8,OPENOUT                                              00440001
*                                                                       00450001
*         CLI   PAR_RETCODE,PAR_RETCODE_OK                              00460001
*         BNE   RET                                                     00470001
*                                                                       00480001
*         MVI   PUTTEXT,C' '                                            00490001
*         MVC   PUTTEXT+1(L'PUTTEXT-1),PUTTEXT                          00500001
*         MVC   PUTTEXT+1(L'WDATASET),WDATASET                          00510001
*         L     R7,DCBOUTA                                              00520001
*         PUT   (R7),PUTTEXT                                            00530001
*                                                                       00700001
*         BAL   R8,CLOSEOUT                                             00710001
*                                                                       00720001
RET      DS    0H                                                       00730001
         XR    R4,R4                                                    00740001
         IC    R4,PAR_RETCODE    * Save returncode before freemain      00750001
RET2     L     R3,MYSA+4         * Restore address of callers SA        00760001
         FREEMAIN RU,LV=WALEN,A=(R13)                                   00770001
         LR    R15,R4            * Set returncode                       00780001
         LR    R13,R3            * Address of callers SA                00790001
         L     R14,12(R13)       * Restore callers R14                  00800001
         LM    R0,R12,20(R13)    * Restore callers registers 0-12       00810001
         BR    R14               * Return                               00820001
*                                                                       00830001
* Initializations                                                       00840001
*                                                                       00850001
INIT     DS    0H                                                       00860001
*         MVC   DCBOUTA,=A(0)                                           00880001
*                                                                       00890001
INITDONE XR    R15,R15                                                  00900001
         BR    R8                                                       00910001
*                                                                       03380001
* Open OUT file                                                         03390001
*                                                                       03400001
*OPENOUT  DS    0H                                                      03410001
*         GETMAIN RU,LV=DCBOUTSIZ,LOC=24                                03420001
*                                                                       03430001
*         ST    R1,DCBOUTA                                              03440001
*         LR    R7,R1                                                   03450001
*         USING IHADCB,R7                                               03460001
*                                                                       03470001
*         MVC   0(DCBOUTSIZ,R7),DCBOUT                                  03480001
*         MVC   OPENW,OPENL                                             03490001
*                                                                       03500001
*         OPEN  ((R7),OUTPUT),MODE=31,MF=(E,OPENW)                      03510001
*                                                                       03520001
*         LTR   R15,R15                                                 03530001
*         BZ    OPENOK                                                  03540001
*         TM    DCBOFLGS,DCBOFOPN                                       03550001
*         BO    OPENOK                                                  03560001
*         WTO   'SHA1: OPEN OUT FAILED',ROUTCDE=11                      03570001
*         MVC   RETCODE,=X'000C0004'                                    03580001
*         BR    R8                                                      03590001
*OPENOK   DS    0H                                                      03600001
*                                                                       03610001
*         DROP  R7                                                      03620001
*                                                                       03630001
*         XR    R15,R15                                                 03640001
*         BR    R8                                                      03650001
*                                                                       03660001
* Close OUT file                                                        03670001
*                                                                       03680001
*CLOSEOUT DS    0H                                                      03690001
*         L     R7,DCBOUTA                                              03700001
*                                                                       03710001
*         MVC   CLOSEW,CLOSEL                                           03720001
*                                                                       03730001
*         CLOSE ((R7)),MODE=31,MF=(E,CLOSEW)                            03740001
*                                                                       03750001
*         XR    R15,R15                                                 03760001
*         BR    R8                                                      03770001
*                                                                       03780001
SHS_DATASIZE   EQU 64                                                   03790001
SHS_DIGESTSIZE EQU 20                                                   03800001
*                                                                       03810001
         LTORG                                                          03820001
*                                                                       03850001
MYSTRING DC    XL20'4141414141414141414141414141414141414141'           03850110
         DC    XL20'4141414141414141414141414141414141414141'           03850210
         DC    XL20'4141414141414141414141414141414141414141'           03850410
         DC    XL20'4141414141414141414141414141414141414141'           03850510
         DC    XL20'4141414141414141414141414141414141414141'           03850610
*                                                                       03851007
*CBOUT   DCB   DDNAME=OUT,DEVD=DA,DSORG=PS,LRECL=132,MACRF=PM,RECFM=FBA 03860001
*DCBOUTSIZ EQU  *-DCBOUT                                                03870001
*                                                                       03880001
*OPENL    OPEN  (,),MODE=31,MF=L                                        03890001
*OPENLSIZ EQU   *-OPENL                                                 03900001
*                                                                       03910001
*CLOSEL   CLOSE (),MODE=31,MF=L                                         03920001
*CLOSELSIZ EQU  *-CLOSEL                                                03930001
*                                                                       03940001
*         DCBD  DSORG=(PS),DEVD=(DA)                                    03950001
*                                                                       03960001
*                                                                       04120001
WORKAREA  DSECT                                                         04130001
MYSA      DS    18F               * My savearea                         04140001
*                                                                       04140101
PAR_SHA1_PTR      DS    A                                               04141004
PAR_SHA1          DS    0F                                              04141104
PAR_FUNC          DS    C                                               04142001
PAR_FUNC_INIT     EQU   X'01'                                           04143001
PAR_FUNC_UPDATE   EQU   X'02'                                           04144001
PAR_FUNC_FINAL    EQU   X'03'                                           04145001
PAR_RETCODE       DS    C                                               04146001
PAR_RETCODE_OK    EQU   X'00'                                           04147001
PAR_RETCODE_ERROR EQU   X'12'                                           04148001
                  DS    CL2                                             04149001
PAR_DIGEST        DS    CL20                                            04149101
PAR_COUNTLO       DS    CL4                                             04149201
PAR_COUNTHI       DS    CL4                                             04149301
PAR_DATA          DS    CL64                                            04149401
PAR_BUFFER_PTR    DS    A                                               04149501
PAR_BYTE_COUNT    DS    F                                               04149601
*                                                                       04149701
*DCBOUTA  DS    A                 * DCB address                         04240001
*         DS    0F                                                      04250001
*OPENW    DS    CL(OPENLSIZ)      * Work area for OPEN exec form        04260001
*         DS    0F                                                      04270001
*CLOSEW   DS    CL(CLOSELSIZ)     * Work area for CLOSE exec form       04280001
*         DS    0F                                                      04290001
*PUTTEXT  DS    CL132                                                   04300001
*                                                                       04301002
WTOBLOCK DS    0F                                                       04302002
WTOLEN   DS    H                                                        04303002
WTOFIL   DS    H                                                        04304002
WTOTEXT  DS    CL80                                                     04305002
*                                                                       04306002
WALEN    EQU   *-WORKAREA                                               04310001
*                                                                       04320001
R0       EQU   0                                                        04330001
R1       EQU   1                                                        04340001
R2       EQU   2                                                        04350001
R3       EQU   3                                                        04360001
R4       EQU   4                                                        04370001
R5       EQU   5                                                        04380001
R6       EQU   6                                                        04390001
R7       EQU   7                                                        04400001
R8       EQU   8                                                        04410001
R9       EQU   9                                                        04420001
R10      EQU   10                                                       04430001
R11      EQU   11                                                       04440001
R12      EQU   12                                                       04450001
R13      EQU   13                                                       04460001
R14      EQU   14                                                       04470001
R15      EQU   15                                                       04480001
*                                                                       04490001
         END   TSHA1                                                    04500001
