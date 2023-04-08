*---------------------------------------------------------------------*
* Program    : LWZMLOG                                                *
* Description: COM object for LOG handling                            *
*---------------------------------------------------------------------*
         TITLE 'LWZMLOG'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates a new LOG object
*
LWZMLOG  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSA,R13       * Address DSA and extra stg for vars
*
         L     R6,0(,R1)         * Pointer to GUID
         L     R5,4(,R1)         * Pointer to pointer object
         ST    R5,RETPTR         * Save return object ptr ptr
         MVC   0(4,R5),=A(0)     * By default return NULL object
*
         USING GLOBAL,R9         * Address global area DSECT
*
*        Was a new LOG object requested?
         IF (CLC,0(16,R6),EQ,G_ILOG_GUID) THEN
            MNEWOBJ OBJTYPE=LOG,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING LOG_obj,R14
            MVI   bOpen,C'N'
            MVI   cLogLevel,LOG_LEVEL_DEBUG
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
         ELSE
            CALL  CEE3ABD,(=A(1003),=A(3)),MF=(E,WORK)
         ENDIF
*
LWZMLOG_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
LOG#01A                      DC    A(LOG#01)   * QueryInterface
LOG#02A                      DC    A(LOG#02)   * AddRef
LOG#03A                      DC    A(LOG#03)   * Release
LOG#04A                      DC    A(LOG#04)   * Open
LOG#05A                      DC    A(LOG#05)   * Close
LOG#06A                      DC    A(LOG#06)   * Write
LOG#07A                      DC    A(LOG#07)   * SetLogLevel
*
PPA      CEEPPA
*
         CEECAA
*
         CEEDSA
*
WORKDSA                      DSECT
*
                             ORG   *+CEEDSASZ
*
FC                           DS    3A  * Feedback code
WORK                         DS    4A  * Work area
RETPTR                       DS    A   * Address of return ptr
OBJPTR                       DS    A   * Address of new obj stg
*
WORKDSA_SIZ                  EQU   *-WORKDSA
*
         MGLOBAL                 * Global area DSECT
*
         COPY  DSCOMOBJ          * Generic COM obj DSECT
*
         COPY  DSLOG             * ILOG obj DSECT
*
         DROP
*
LWZMLOG  CSECT
*
* ILOG QueryInterface
*
LOG#01   MQRYIFCE SUF=L01,IFACE=ILOG
*
* ILOG AddRef
*
LOG#02   MADDREF
*
* ILOG Release
*
LOG#03   MRELEASE SUF=L03,OBJ=LOG
*
* ILOG Open
*
LOG#04   CEEENTRY AUTO=WORKDSAL04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAL04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING LOG_obj,R8        * Address object DSECT
*
         CLI   bOpen,C'Y'        * If already open
         BE    LOG#04_RET        * Skip to end
*
         IF (CLC,lpDCB,EQ,=A(0)) THEN * Is DCB memory allocated yet?
            GETMAIN RU,LV=DCB_DSECT_SIZ,LOC=24 * Get stg below line
            ST    R1,lpDCB       * Save DCB ptr
*
*           Initialize DCB area with constants
            MVC   DCBLOG-DCB_DSECT(DCBLOG_SIZ,R1),CDCBLOG
         ENDIF
*
         L     R14,lpDCB         * Point R14 to DCB DSECT
         LA    R2,DCBLOG-DCB_DSECT(,R14)   * Point R2 to DCB
         MVC   G_OPEND,G_OPENL   * Init OPEN execute mem
*
*        Open DCBLOG DCB
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,G_OPEND)
*
         LTR   R15,R15           * Test return code
         IF (NZ) THEN
            CVD   R15,G_DEC8       * Turn OPEN return code
            UNPK  G_ZONED8,G_DEC8  * into zoned
            OI    G_ZONED8+7,X'F0' * and get rid of sign
*
            MVC   G_WTOTEXT(20),=CL20'LWZMLOG OPEN FAILED '
            MVC   G_WTOTEXT+20(2),G_ZONED8+6
            MVC   G_WTOLEN,=AL2(26)
            MVC   G_WTOFIL,=AL2(0)
            WTO   MF=(E,G_WTOBLOCK),ROUTCDE=11,DESC=7
*
            CALL  CEE3ABD,(=A(1004),=A(3)),MF=(E,WORKL04)
         ENDIF
*
         MVI   bOpen,C'Y'        * Set flag that DCB is now open
*
LOG#04_RET EQU   *
         CEETERM
*
         LTORG
*
* Constant initialize DCB area
CDCBLOG                      DCB   DDNAME=LWZMLOG,MACRF=(PM),DSORG=PS, X
               RECFM=FB,LRECL=160
DCBLOG_SIZ                   EQU   *-CDCBLOG
*
WORKDSAL04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKL04                      DS    2A
*
WORKDSAL04_SIZ               EQU   *-WORKDSAL04
*
* DSECT with DCBs
DCB_DSECT                    DSECT
*
DCBLOG                       DS    0F
                             ORG   *+DCBLOG_SIZ
*
DCB_DSECT_SIZ                EQU   *-DCB_DSECT
*
* DCECT for addressing a DCB
         DCBD  DSORG=PS,DEVD=DA
*
LWZMLOG  CSECT
*
         DROP
*
* ILOG Close
*
LOG#05   CEEENTRY AUTO=WORKDSAL05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAL05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING LOG_obj,R8        * Address object DSECT
*
         CLI   bOpen,C'Y'        * If not open
         BNE   LOG#05_RET        * Skip to end
*
         L     R14,lpDCB         * Point R14 to DCB DSECT
         LA    R2,DCBLOG-DCB_DSECT(,14) * Point R2 to DCB
         MVC   G_CLOSED,G_CLOSEL * Init CLOSE execute mem
*
*        Close DCBLOG DCB
         CLOSE ((R2)),MODE=31,MF=(E,G_CLOSED)
*
         LTR   R15,R15           * Test return code
         IF (NZ) THEN
            CVD   R15,G_DEC8       * Turn CLOSE return code
            UNPK  G_ZONED8,G_DEC8  * into zoned
            OI    G_ZONED8+7,X'F0' * and get rid of sign
*
            MVC   G_WTOTEXT(21),=CL21'LWZMLOG CLOSE FAILED '
            MVC   G_WTOTEXT+21(2),G_ZONED8+6
            MVC   G_WTOLEN,=AL2(27)
            MVC   G_WTOFIL,=AL2(0)
            WTO   MF=(E,G_WTOBLOCK),ROUTCDE=11,DESC=7
*
            CALL  CEE3ABD,(=A(1004),=A(3)),MF=(E,WORKL05)
         ENDIF
*
         MVI   bOpen,C'N'        * Set flag that DCB is now closed
*
LOG#05_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAL05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKL05                      DS    2A
*
WORKDSAL05_SIZ               EQU   *-WORKDSAL05
*
LWZMLOG  CSECT
*
         DROP
*
* ILOG Write
*
LOG#06   CEEENTRY AUTO=WORKDSAL06_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAL06,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING LOG_obj,R8        * Address object DSECT
*
         L     R14,8(,R1)        * Parm 3 is msg log level
         XR    R15,R15           * Clear R15
         IC    R15,cLogLevel     * Put log level in R15
         CR    R14,R15           * Compare msg log level with LOG's
         BH    LOG#06_RET        * If msg log level higher, skip log
*
         CLI   bOpen,C'Y'        * If not open
         BNE   LOG#06_RET        * Skip to end
*
         L     R7,4(,R1)         * Parm 2 is line to write
*
*        Get the current date+time
         TIME  DEC,G_TIMEDATE,ZONE=LT,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD
*        Convert time part to zoned
         UNPK  G_TIMEDATEZ+10(10),G_TIMEDATE(5)
*        Convert date part to zoned
         UNPK  G_TIMEDATEZ(10),G_TIMEDATE+8(5)
*
         XR    R0,R0             * Search for terminating zero
         LR    R1,R7             * Point R1 to incoming zero str
         LA    R2,4095(,R1)      * Search for max 4K
         SRST  R2,R1             * Scan for zero terminator
         BRC   1,*-4             * Scan was incomplete, try again
         BRC   2,*-12            * Not found, try another 4K
*
         LR    R5,R7             * Point R5 to incoming zero str
         SR    R2,R5             * Calculate length
*
         L     R14,lpDCB         * Get DCB memory pointer
         LA    R6,DCBLOG-DCB_DSECT(,R14) * Get addr of LWZMLOG DCB
*
         DO WHILE=(C,R2,GT,=A(0))
            IF (CR,R5,EQ,R7) THEN
               MVI   cLine,C' '     * Clear cLine
               MVC   cLine+1(L'cLine-1),cLine
               MVC   cLine(19),=C'XXXX-XX-XX-XX:XX:XX'
               MVC   cLine(4),G_TIMEDATEZ+1
               MVC   cLine+5(2),G_TIMEDATEZ+5
               MVC   cLine+8(2),G_TIMEDATEZ+7
               MVC   cLine+11(2),G_TIMEDATEZ+11
               MVC   cLine+14(2),G_TIMEDATEZ+13
               MVC   cLine+17(2),G_TIMEDATEZ+15
*
               IF (C,R2,GT,=A(L'cLine-20)) THEN
                  L     R3,=A(L'cLine-20)
               ELSE
                  LR    R3,R2
               ENDIF
               BCTR  R3,R0
               B     *+10
               MVC   cLine+20(1),0(R5)
               EX    R3,*-6
            ELSE
               MVI   cLine+27,C' '
               MVC   cLine+28(L'cLine-28),cLine+27
               MVI   cLine+27,C'+'
*
               IF (C,R2,GT,=A(L'cLine-28)) THEN
                  L     R3,=A(L'cLine-28)
               ELSE
                  LR    R3,R2
               ENDIF
               BCTR  R3,R0
               B     *+10
               MVC   cLine+28(1),0(R5)
               EX    R3,*-6
            ENDIF
*
            PUT   (R6),cLine
*
            LA    R3,1(,R3)
            AR    R5,R3
            SR    R2,R3
         ENDDO
*
LOG#06_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAL06                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKDSAL06_SIZ               EQU   *-WORKDSAL06
*
LWZMLOG  CSECT
*
         DROP
*
* ILOG SetLogLevel
*
LOG#07   MSETPB OBJ=LOG,PROP=cLogLevel
*
         DROP
*
         COPY  REGS              * Register equates
*
         END   LWZMLOG
