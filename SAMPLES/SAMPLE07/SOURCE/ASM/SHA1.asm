*********************************************************************** 00010000
* Program    : SHA1                                                   * 00020000
*                                                                     * 00030000
* Description: Hash a 512 bit chunk with SHA1 routine.                * 00040000
*                                                                     * 00050000
* Parameters : 1) CL20     5 fullword digest                          * 00060000
*                 CL8      doubleword bit count                       * 00070000
*                 CL64     512 bit data buffer                        * 00080000
*                 A        Data pointer                               * 00090000
*                 F        Length in bytes                            * 00100000
*********************************************************************** 00110000
         TITLE 'SHA1'                                                   00120000
*                                                                       00130001
         COPY  ASMMSP                                                   00140001
*                                                                       00150000
SHA1     AMODE 31                                                       00160000
SHA1     RMODE ANY                                                      00170000
SHA1     CSECT                                                          00180000
         STM   R14,R12,12(R13)   * Save callers registers               00190000
         BASR  R10,R0            * Set R11 to next instruction          00200000
SHA1#    LA    R11,4095(,R10)    * Setup R12 as second using            00210000
         LA    R11,1(,R11)       *   base register                      00220000
         USING SHA1#,R10,R11     * Establish addressing                 00230000
         LR    R8,R1             * Save possible parameter ptr          00240000
         GETMAIN RU,LV=WALEN                                            00250000
         ST    R13,4(R1)         * Backward chain callers SA            00260000
         ST    R1,8(R13)         * Forward chain my SA                  00270000
         LR    R13,R1            * Point R13 to my SA                   00280000
         USING WORKAREA,R13                                             00290000
         ST    R8,PARMS          * Save address of parameter ptrs       00300000
*                                                                       00310000
         LT    R1,PARMS                                                 00320000
         BNZ   PARMSOK                                                  00330000
         L     R4,12                                                    00340000
         B     RET2                                                     00350000
PARMSOK  L     R9,0(,R1)                                                00360000
         USING PAR_SHA1,R9                                              00370000
*                                                                       00380000
         BAL   R8,INIT                                                  00390042
*                                                                       00400000
         CLI   PAR_RETCODE,PAR_RETCODE_OK                               00410042
         BNE   RET                                                      00420042
*                                                                       00430000
*        BAL   R8,OPENOUT                                               00440042
*                                                                       00450000
*        CLI   PAR_RETCODE,PAR_RETCODE_OK                               00460042
*        BNE   RET                                                      00470042
*                                                                       00480000
         CLI   PAR_FUNC,PAR_FUNC_INIT                                   00550000
         BNE   CHECK_UPDATE                                             00560000
         BAL   R8,SHAINIT                                               00570000
         B     RET                                                      00580000
*                                                                       00590000
CHECK_UPDATE DS 0H                                                      00600000
         CLI   PAR_FUNC,PAR_FUNC_UPDATE                                 00610000
         BNE   CHECK_FINAL                                              00620000
         BAL   R8,SHAUPDATE                                             00630000
         B     RET                                                      00640000
*                                                                       00650000
CHECK_FINAL DS 0H                                                       00660000
         CLI   PAR_FUNC,PAR_FUNC_FINAL                                  00670020
         BNE   RET                                                      00680014
         BAL   R8,SHAFINAL                                              00690014
*                                                                       00700000
*        BAL   R8,CLOSEOUT                                              00710042
*                                                                       00720000
RET      DS    0H                                                       00730000
         XR    R4,R4                                                    00740000
         IC    R4,PAR_RETCODE    * Save returncode before freemain      00750000
RET2     L     R3,MYSA+4         * Restore address of callers SA        00760000
         FREEMAIN RU,LV=WALEN,A=(R13)                                   00770000
         LR    R15,R4            * Set returncode                       00780000
         LR    R13,R3            * Address of callers SA                00790000
         L     R14,12(R13)       * Restore callers R14                  00800000
         LM    R0,R12,20(R13)    * Restore callers registers 0-12       00810000
         BR    R14               * Return                               00820000
*                                                                       00830000
* Initializations                                                       00840000
*                                                                       00850000
INIT     DS    0H                                                       00860000
         MVI   PAR_RETCODE,PAR_RETCODE_OK                               00870000
*        MVC   DCBOUTA,=A(0)                                            00880042
*                                                                       00890000
INITDONE XR    R15,R15                                                  00900000
         BR    R8                                                       00910000
*                                                                       00920000
* SHAINIT                                                               00930000
*                                                                       00940000
SHAINIT  DS    0H                                                       00950000
         MVC   PAR_DIGEST,=X'67452301'                                  00960000
         MVC   PAR_DIGEST+4,=X'EFCDAB89'                                00970000
         MVC   PAR_DIGEST+8,=X'98BADCFE'                                00980000
         MVC   PAR_DIGEST+12,=X'10325476'                               00990000
         MVC   PAR_DIGEST+16,=X'C3D2E1F0'                               01000000
         MVC   PAR_COUNTLO,=A(0)                                        01010000
         MVC   PAR_COUNTHI,=A(0)                                        01020000
*                                                                       01030000
SHAINIT_DONE DS 0H                                                      01040000
         XR    R15,R15                                                  01050000
         BR    R8                                                       01060000
*                                                                       01070000
* SHAUPDATE                                                             01080000
*                                                                       01090000
SHAUPDATE DS   0H                                                       01100000
         MVC   BUFFERU,PAR_BUFFER_PTR                                   01110004
         MVC   COUNTU,PAR_BYTE_COUNT                                    01120004
         MVC   TMP,PAR_COUNTLO                                          01130000
         L     R7,COUNTU                                                01140004
         SLL   R7,3                                                     01150000
         A     R7,TMP                                                   01160000
         ST    R7,PAR_COUNTLO                                           01170000
         L     R6,PAR_COUNTHI                                           01180000
         IF (O) THEN                                                    01190000
            LA    R6,1(,R6)                                             01200000
         ENDIF                                                          01210000
         L     R7,COUNTU                                                01220004
         SRL   R7,29                                                    01230000
         AR    R6,R7                                                    01240000
         ST    R6,PAR_COUNTHI                                           01250000
*                                                                       01260000
         L     R7,TMP                                                   01270002
         SRL   R7,3                                                     01280002
         N     R7,=X'0000003F'                                          01290002
         ST    R7,DATACOUNT                                             01300002
         IF (NZ) THEN                                                   01310002
            LA    R6,PAR_DATA                                           01320002
            AR    R6,R7                                                 01330002
            ST    R6,P                                                  01340002
            LA    R5,SHS_DATASIZE                                       01350002
            SR    R5,R7                                                 01360002
            ST    R5,DATACOUNT                                          01370002
            C     R5,COUNTU                                             01380005
            L     R0,P                                                  01390004
            L     R2,BUFFERU                                            01400004
            IF (GT) THEN                                                01410002
               L     R1,COUNTU                                          01420005
               LR    R3,R1                                              01430002
               MVCL  R0,R2                                              01440002
               B     SHAUPDATE_DONE                                     01450002
            ENDIF                                                       01460002
            L     R1,DATACOUNT                                          01470004
            LR    R3,R1                                                 01480004
            MVCL  R0,R2                                                 01490004
            BAL   R7,SHSTRANSFORM                                       01510004
            L     R6,DATACOUNT                                          01520004
            L     R7,BUFFERU                                            01530004
            AR    R7,R6                                                 01540004
            ST    R7,BUFFERU                                            01550004
            L     R7,COUNTU                                             01560004
            SR    R7,R6                                                 01570006
            ST    R7,COUNTU                                             01580004
         ENDIF                                                          01590002
*                                                                       01600000
SHAUPDATE_10 DS 0H                                                      01610006
         LA    R7,SHS_DATASIZE                                          01620006
         C     R7,COUNTU                                                01630006
         BH    SHAUPDATE_20                                             01640006
         LA    R0,PAR_DATA                                              01650006
         L     R2,BUFFERU                                               01660006
         LA    R1,SHS_DATASIZE                                          01670006
         LR    R3,R1                                                    01680006
         MVCL  R0,R2                                                    01690006
         BAL   R7,SHSTRANSFORM                                          01710006
         LA    R6,SHS_DATASIZE                                          01720006
         L     R7,BUFFERU                                               01730006
         AR    R7,R6                                                    01740006
         ST    R7,BUFFERU                                               01750006
         L     R7,COUNTU                                                01760006
         SR    R7,R6                                                    01770006
         ST    R7,COUNTU                                                01780006
SHAUPDATE_20 DS 0H                                                      01790006
*                                                                       01800006
         LA    R0,PAR_DATA                                              01810006
         L     R2,BUFFERU                                               01820006
         L     R1,COUNTU                                                01830006
         LR    R3,R1                                                    01840006
         MVCL  R0,R2                                                    01850006
*                                                                       01860006
SHAUPDATE_DONE DS 0H                                                    01870000
         XR    R15,R15                                                  01880000
         BR    R8                                                       01890000
*                                                                       01891014
* SHAFINAL                                                              01892014
*                                                                       01893014
SHAFINAL DS    0H                                                       01894014
         L     R5,PAR_COUNTLO                                           01894814
         SRL   R5,3                                                     01894914
         N     R5,=X'0000003F'                                          01895014
*                                                                       01895114
         LA    R6,PAR_DATA                                              01895214
         AR    R6,R5                                                    01895314
         MVI   0(R6),X'80'                                              01895416
         LA    R6,1(,R6)                                                01895514
*                                                                       01895614
         LA    R4,SHS_DATASIZE                                          01895714
         BCTR  R4,R0                                                    01895814
         SR    R4,R5                                                    01895914
         ST    R4,COUNTF                                                01896014
*                                                                       01896114
         C     R4,=F'8'                                                 01896214
         BNL   SHAFINAL_10                                              01896314
         BCTR  R4,R0                                                    01896414
         LA    R3,=XL56'00'                                             01896514
         EX    R4,MVCR6R3                                               01896614
         BAL   R7,SHSTRANSFORM                                          01896814
*                                                                       01896914
         LA    R6,PAR_DATA                                              01897014
         LA    R3,=XL56'00'                                             01897114
         LA    R4,SHS_DATASIZE-8-1                                      01897214
         EX    R4,MVCR6R3                                               01897314
         LA    R6,SHS_DATASIZE-8(,R6)                                   01897414
         B     SHAFINAL_20                                              01897514
SHAFINAL_10 DS 0H                                                       01897614
         BE    SHAFINAL_20                                              01897714
         S     R4,=F'9'                                                 01897814
         LA    R3,=XL56'00'                                             01897914
         EX    R4,MVCR6R3                                               01898014
*                                                                       01898114
SHAFINAL_20 DS 0H                                                       01898214
         MVC   PAR_DATA+56,PAR_COUNTHI                                  01898314
         MVC   PAR_DATA+60,PAR_COUNTLO                                  01898414
*                                                                       01898514
         BAL   R7,SHSTRANSFORM                                          01898714
*                                                                       01898814
SHAFINAL_DONE DS 0H                                                     01898914
         XR    R15,R15                                                  01899114
         BR    R8                                                       01899214
*                                                                       02190004
* SHSTRANSFORM                                                          02200004
*                                                                       02210004
SHSTRANSFORM DS 0H                                                      02220004
         MVC   EDATA(64),PAR_DATA                                       02230008
         LA    R6,EDATA+64                                              02240008
         LA    R5,EDATA                                                 02250008
         LA    R4,EDATA+8                                               02260008
         LA    R3,EDATA+32                                              02270008
         LA    R2,EDATA+52                                              02280008
         LA    R1,64                                                    02290008
SHSTRANSFORM_10 DS 0H                                                   02300008
         L     R14,0(,R2)                                               02310036
         X     R14,0(,R3)                                               02320036
         X     R14,0(,R4)                                               02330036
         X     R14,0(,R5)                                               02340036
         RLL   R14,R14,1                                                02350009
         ST    R14,0(,R6)                                               02360009
         LA    R6,4(,R6)                                                02370008
         LA    R5,4(,R5)                                                02380008
         LA    R4,4(,R4)                                                02390008
         LA    R3,4(,R3)                                                02400008
         LA    R2,4(,R2)                                                02410008
         BCT   R1,SHSTRANSFORM_10                                       02420008
*                                                                       02430008
         LM    R0,R4,PAR_DIGEST                                         02440011
*                                                                       02450013
         DROP  R9                                                       02460013
*                                                                       02470013
         LA    R5,20                                                    02480010
         LA    R6,EDATA                                                 02490010
SHATRANSFORM_20 DS 0H                                                   02500010
         LR    R14,R2              * f = c                              02510010
         XR    R14,R3              * f = c xor d                        02520010
         NR    R14,R1              * f = b and (c xor d)                02530010
         XR    R14,R3              * f = c xor (b and (c xor d)         02540010
         LR    R15,R0              * temp = a                           02550010
         RLL   R15,R15,5           * a leftrotate 5                     02560010
         AR    R15,R14             * + f                                02570010
         AR    R15,R4              * + e                                02580010
         A     R15,=X'5A827999'    * + k                                02591021
         A     R15,0(,R6)          * + edata                            02600010
         LR    R4,R3               * e = d                              02610010
         LR    R3,R2               * d = c                              02620010
         LR    R2,R1               * c = b                              02630010
         RLL   R2,R2,30            * c = b leftrotate 30                02640010
         LR    R1,R0               * b = a                              02650010
         LR    R0,R15              * a = temp                           02660010
         LA    R6,4(,R6)                                                02670010
         BCT   R5,SHATRANSFORM_20                                       02680010
         LA    R5,20                                                    02690011
SHATRANSFORM_40 DS 0H                                                   02700011
         LR    R14,R1              * f = b                              02710011
         XR    R14,R2              * f = b xor c                        02720011
         XR    R14,R3              * f = b xor c xor d                  02730011
         LR    R15,R0              * temp = a                           02740011
         RLL   R15,R15,5           * a leftrotate 5                     02750011
         AR    R15,R14             * + f                                02760011
         AR    R15,R4              * + e                                02770011
         A     R15,=X'6ED9EBA1'    * + k                                02781021
         A     R15,0(,R6)          * + edata                            02790011
         LR    R4,R3               * e = d                              02800011
         LR    R3,R2               * d = c                              02810011
         LR    R2,R1               * c = b                              02820011
         RLL   R2,R2,30            * c = b leftrotate 30                02830011
         LR    R1,R0               * b = a                              02840011
         LR    R0,R15              * a = temp                           02850011
         LA    R6,4(,R6)                                                02860011
         BCT   R5,SHATRANSFORM_40                                       02870011
         LA    R5,20                                                    02880011
SHATRANSFORM_60 DS 0H                                                   02890011
         LR    R14,R1              * f = b                              02900011
         OR    R14,R2              * f = b or c                         02910011
         NR    R14,R3              * f = d and (b or c)                 02920011
         LR    R9,R1               * f2 = b                             02930011
         NR    R9,R2               * f2 = b and c                       02940011
         OR    R14,R9              * f = (b and c) or (d and (b or c))  02950011
         LR    R15,R0              * temp = a                           02960011
         RLL   R15,R15,5           * a leftrotate 5                     02970011
         AR    R15,R14             * + f                                02980011
         AR    R15,R4              * + e                                02990011
         A     R15,=X'8F1BBCDC'    * + k                                03001021
         A     R15,0(,R6)          * + edata                            03010011
         LR    R4,R3               * e = d                              03020011
         LR    R3,R2               * d = c                              03030011
         LR    R2,R1               * c = b                              03040011
         RLL   R2,R2,30            * c = b leftrotate 30                03050011
         LR    R1,R0               * b = a                              03060011
         LR    R0,R15              * a = temp                           03070011
         LA    R6,4(,R6)                                                03080011
         BCT   R5,SHATRANSFORM_60                                       03090011
         LA    R5,20                                                    03100011
SHATRANSFORM_80 DS 0H                                                   03110011
         LR    R14,R1              * f = b                              03120011
         XR    R14,R2              * f = b xor c                        03130011
         XR    R14,R3              * f = b xor c xor d                  03140011
         LR    R15,R0              * temp = a                           03150011
         RLL   R15,R15,5           * a leftrotate 5                     03160011
         AR    R15,R14             * + f                                03170011
         AR    R15,R4              * + e                                03180011
         A     R15,=X'CA62C1D6'    * + k                                03191021
         A     R15,0(,R6)          * + edata                            03200011
         LR    R4,R3               * e = d                              03210011
         LR    R3,R2               * d = c                              03220011
         LR    R2,R1               * c = b                              03230011
         RLL   R2,R2,30            * c = b leftrotate 30                03240011
         LR    R1,R0               * b = a                              03250011
         LR    R0,R15              * a = temp                           03260011
         LA    R6,4(,R6)                                                03270011
         BCT   R5,SHATRANSFORM_80                                       03280011
*                                                                       03290011
         L     R5,PARMS                                                 03300012
         L     R9,0(,R5)                                                03310012
         USING PAR_SHA1,R9                                              03320012
*                                                                       03330012
         A     R0,PAR_DIGEST                                            03340011
         A     R1,PAR_DIGEST+4                                          03350011
         A     R2,PAR_DIGEST+8                                          03360011
         A     R3,PAR_DIGEST+12                                         03370011
         A     R4,PAR_DIGEST+16                                         03380011
         STM   R0,R4,PAR_DIGEST                                         03390011
*                                                                       03400011
SHSTRANSFORM_DONE DS 0H                                                 03410011
         XR    R15,R15                                                  03420011
         BR    R7                                                       03430011
*                                                                       03440011
* Open OUT file                                                         03450000
*                                                                       03460000
*OPENOUT  DS    0H                                                      03470042
*        GETMAIN RU,LV=DCBOUTSIZ,LOC=24                                 03480042
*                                                                       03490038
*        ST    R1,DCBOUTA                                               03500042
*        LR    R7,R1                                                    03510042
*        USING IHADCB,R7                                                03520042
*                                                                       03530038
*        MVC   0(DCBOUTSIZ,R7),DCBOUT                                   03540042
*        MVC   OPENW,OPENL                                              03550042
*                                                                       03560038
*        OPEN  ((R7),OUTPUT),MODE=31,MF=(E,OPENW)                       03570042
*                                                                       03580038
*        LTR   R15,R15                                                  03590042
*        BZ    OPENOK                                                   03600042
*        TM    DCBOFLGS,DCBOFOPN                                        03610042
*        BO    OPENOK                                                   03620042
*        WTO   'SHA1: OPEN OUT FAILED',ROUTCDE=11                       03630042
*        MVI   PAR_RETCODE,PAR_RETCODE_ERROR                            03640042
*        BR    R8                                                       03650042
*OPENOK   DS    0H                                                      03660042
*                                                                       03670000
*        DROP  R7                                                       03680042
*                                                                       03690000
*        XR    R15,R15                                                  03700042
*        BR    R8                                                       03710042
*                                                                       03720000
* Close OUT file                                                        03730000
*                                                                       03740000
*CLOSEOUT DS    0H                                                      03750042
*        L     R7,DCBOUTA                                               03760042
*                                                                       03770000
*        MVC   CLOSEW,CLOSEL                                            03780042
*                                                                       03790000
*        CLOSE ((R7)),MODE=31,MF=(E,CLOSEW)                             03800042
*                                                                       03810000
*        XR    R15,R15                                                  03820042
*        BR    R8                                                       03830042
*                                                                       03840000
SHS_DATASIZE   EQU 64                                                   03850003
SHS_DIGESTSIZE EQU 20                                                   03860003
*                                                                       03870003
         LTORG                                                          03880000
*                                                                       03880114
MVCR6R3  MVC   0(1,R6),0(R3)                                            03881014
*                                                                       03890000
*DCBOUT  DCB   DDNAME=OUT,DEVD=DA,DSORG=PS,LRECL=132,MACRF=PM,RECFM=FBA 03900042
*DCBOUTSIZ EQU  *-DCBOUT                                                03910042
*                                                                       03920000
*OPENL    OPEN  (,),MODE=31,MF=L                                        03930042
*OPENLSIZ EQU   *-OPENL                                                 03940042
*                                                                       03950000
*CLOSEL   CLOSE (),MODE=31,MF=L                                         03960042
*CLOSELSIZ EQU  *-CLOSEL                                                03970042
*                                                                       03980000
*        DCBD  DSORG=(PS),DEVD=(DA)                                     03990042
*                                                                       04000000
PAR_SHA1          DSECT                                                 04010000
PAR_FUNC          DS    C                                               04020000
PAR_FUNC_INIT     EQU   X'01'                                           04030000
PAR_FUNC_UPDATE   EQU   X'02'                                           04040000
PAR_FUNC_FINAL    EQU   X'03'                                           04050000
PAR_RETCODE       DS    C                                               04060000
PAR_RETCODE_OK    EQU   X'00'                                           04070000
PAR_RETCODE_ERROR EQU   X'12'                                           04080000
                  DS    CL2                                             04090000
PAR_DIGEST        DS    CL20                                            04100000
PAR_COUNTLO       DS    CL4                                             04110000
PAR_COUNTHI       DS    CL4                                             04120000
PAR_DATA          DS    CL64                                            04130000
PAR_BUFFER_PTR    DS    A                                               04140000
PAR_BYTE_COUNT    DS    F                                               04150000
*                                                                       04160000
WORKAREA  DSECT                                                         04170002
MYSA      DS    18F               * My savearea                         04180002
*MYSA2     DS    18F               * My savearea 2                      04181042
PARMS     DS    F                 * Address of parameter ptrs           04190002
BUFFERU   DS    A                                                       04200004
COUNTU    DS    F                                                       04210004
COUNTF    DS    F                                                       04211014
TMP       DS    F                                                       04220002
DATACOUNT DS    F                                                       04230002
P         DS    A                                                       04240002
ABCDE     DS    CL20                                                    04250010
EDATA     DS    CL320                                                   04260008
*DCBOUTA   DS    A                 * DCB address                        04270042
*          DS    0F                                                     04280042
*OPENW     DS    CL(OPENLSIZ)      * Work area for OPEN exec form       04290042
*          DS    0F                                                     04300042
*CLOSEW    DS    CL(CLOSELSIZ)     * Work area for CLOSE exec form      04310042
*          DS    0F                                                     04320042
*PUTTEXT   DS    CL132                                                  04330042
WALEN    EQU   *-WORKAREA                                               04340000
*                                                                       04350000
R0       EQU   0                                                        04360000
R1       EQU   1                                                        04370000
R2       EQU   2                                                        04380000
R3       EQU   3                                                        04390000
R4       EQU   4                                                        04400000
R5       EQU   5                                                        04410000
R6       EQU   6                                                        04420000
R7       EQU   7                                                        04430000
R8       EQU   8                                                        04440000
R9       EQU   9                                                        04450000
R10      EQU   10                                                       04460000
R11      EQU   11                                                       04470000
R12      EQU   12                                                       04480000
R13      EQU   13                                                       04490000
R14      EQU   14                                                       04500000
R15      EQU   15                                                       04510000
*                                                                       04520000
         END   SHA1                                                     04530000
