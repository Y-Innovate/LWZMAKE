*---------------------------------------------------------------------*
* Program    : LWZMINP                                                *
* Description: COM object for Input (from File or String)             *
*---------------------------------------------------------------------*
         TITLE 'LWZMINP'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates a new IND (input from file) or INS (input from
*   string) object
*
LWZMINP  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
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
*        Was a new IND object requested?
         IF (CLC,0(16,R6),EQ,G_IIND_GUID) THEN
            MNEWOBJ OBJTYPE=IND,WORK=WORK * Alloc new object
*
*           Init obj attributes
            LR    R7,R14
            USING IND_obj,R7
            MVI   IND_bOpen,C'N'
            MVI   IND_bEOF,C'N'
            MVI   IND_bEOL,C'Y'
*
*           Instantiate a new ISTR object
            MINSTANT GUID=G_ISTR_GUID,WORK=WORK,                       X
               OBJPTR=IND_ISTR_currLine
*
            DROP  R7
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R7,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_IND
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new INF object requested?
         ELSEIF (CLC,0(16,R6),EQ,G_IINF_GUID) THEN
            MNEWOBJ OBJTYPE=INF,WORK=WORK * Alloc new object
*
*           Init obj attributes
            LR    R7,R14
            USING INF_obj,R7
            MVI   INF_bOpen,C'N'
            MVI   INF_bEOF,C'N'
            MVI   INF_bEOL,C'Y'
*
*           Instantiate a new ISTB object
            MINSTANT GUID=G_ISTB_GUID,WORK=WORK,                       X
               OBJPTR=INF_ISTB_currLine
*
            DROP  R7
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R7,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_INF
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new IINS object requested?
         ELSEIF (CLC,0(16,R6),EQ,G_IINS_GUID) THEN
            MNEWOBJ OBJTYPE=INS,WORK=WORK * Alloc new object
*
*           Init obj attributes
            LR    R7,R14
            USING INS_obj,R7
            MVI   INS_bEOF,C'N'
            MVI   INS_bEOL,C'N'
*
*           Instantiate a new ISTR object
            MINSTANT GUID=G_ISTR_GUID,WORK=WORK,                       X
               OBJPTR=INS_ISTR_currLine
*
            DROP  R7
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R7,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_INS
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new IIFO object requested?
         ELSEIF (CLC,0(16,R6),EQ,G_IIFO_GUID) THEN
            MNEWOBJ OBJTYPE=IFO,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING IFO_obj,R14
            MVI   IFO_bEOF,C'N'
            MVI   IFO_bEOL,C'Y'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R7,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_IFO
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
         ELSE
            CALL  CEE3ABD,(=A(1003),=A(3)),MF=(E,WORK)
         ENDIF
*
LWZMINP_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
IND#01A                      DC    A(IND#01)   * QueryInterface
IND#02A                      DC    A(IND#02)   * AddRef
IND#03A                      DC    A(IND#03)   * Release
IND#04A                      DC    A(IND#04)   * GetNextChar
IND#05A                      DC    A(IND#05)   * Open
IND#06A                      DC    A(IND#06)   * Close
*
                             DS    0F
INF#01A                      DC    A(INF#01)   * QueryInterface
INF#02A                      DC    A(INF#02)   * AddRef
INF#03A                      DC    A(INF#03)   * Release
INF#04A                      DC    A(INF#04)   * GetNextChar
INF#05A                      DC    A(INF#05)   * Open
INF#06A                      DC    A(INF#06)   * Close
*
                             DS    0F
INS#01A                      DC    A(INS#01)   * QueryInterface
INS#02A                      DC    A(INS#02)   * AddRef
INS#03A                      DC    A(INS#03)   * Release
INS#04A                      DC    A(INS#04)   * GetNextChar
INS#05A                      DC    A(INS#05)   * Init
*
                             DS    0F
IFO#01A                      DC    A(IFO#01)   * QueryInterface
IFO#02A                      DC    A(IFO#02)   * AddRef
IFO#03A                      DC    A(IFO#03)   * Release
*
                             DS    0F
MAK501D_IND                  DC    C'MAK501D Created IIND object '
                             DC    X'00'
                             DS    0F
MAK501D_INF                  DC    C'MAK501D Created IINF object '
                             DC    X'00'
                             DS    0F
MAK501D_INS                  DC    C'MAK501D Created IINS object '
                             DC    X'00'
                             DS    0F
MAK501D_IFO                  DC    C'MAK501D Created IIFO object '
                             DC    X'00'
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
         COPY  DSIND             * IIND obj DSECT
         COPY  DSINF             * IINF obj DSECT
         COPY  DSINS             * IINS obj DSECT
         COPY  DSIFO             * IIFO obj DSECT
         COPY  DSSTR             * ISTR obj DSECT
         COPY  DSSTB             * ISTB obj DSECT
*
LWZMINP  CSECT
*
         DROP
*
* IIND QueryInterface
*
IND#01   MQRYIFCE SUF=D01,IFACE=IIND
*
* IIND AddRef
*
IND#02   MADDREF
*
* IIND Release
*
IND#03   CEEENTRY AUTO=WORKDSAD03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAD03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    IND#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING IND_obj,R6
*
            ISTR_Release OBJECT=IND_ISTR_currLine,WORK=WORKD03
            MVC   IND_ISTR_currLine,=A(0)
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRD03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRD03,FCD03),MF=(E,WORKD03)
*
            L     R15,G_OBJCOUNT
            BCTR  R15,R0
            ST    R15,G_OBJCOUNT
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R6,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKD03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD03,      X
               ZSTR=MAK502D_IND
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKD03,LINE=0(,R2),    , X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
IND#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_IND                  DC    C'MAK502D Deleted IIND object '
                             DC    X'00'
*
WORKDSAD03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRD03                    DS    A
WORKD03                      DS    3A
*
WORKDSAD03_SIZ               EQU   *-WORKDSAD03
*
LWZMINP  CSECT
*
         DROP
*
* IIND GetNextChar
*
IND#04   CEEENTRY AUTO=WORKDSAD04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAD04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING IND_obj,R8        * Address object DSECT
*
         L     R7,4(,R1)         * Parm 2 is IIFO ptr
         ST    R7,IIFO_ii_D04    * Save as local var
         USING IFO_obj,R7        * Addressability for ii
*
         MVI   IFO_cCurrChar,X'00'
         MVI   IFO_cPeekChar,X'00'
         MVI   IFO_cPeekChar2,X'00'
         MVI   IFO_bEOL,C'N'
         MVI   IFO_bEOF,C'N'
         MVC   IFO_pos,IND_pos
         MVC   IFO_nLine,IND_nLine
*
         IF (CLI,IND_bEOL,EQ,C'Y') THEN
            L     R15,=A(IND#99) * Read next line
            BASR  R14,R15
*
            MVC   IFO_nLine,IND_nLine
         ENDIF
*
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKD04
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD04,            X
               ZSTR=MAK601D_IND
*
         IF (CLI,IND_bEOF,EQ,C'Y') THEN
            MVI   IFO_bEOF,C'Y'
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD04,         X
               ZSTR==X'C5D6C600' * EOF
         ELSE
            IF (CLC,IND_pos,LT,IND_nCurrLineLength) THEN
               ISTR_CharAt OBJECT=IND_ISTR_currLine,WORK=WORKD04,      X
               POS=IND_pos,CHAROUT=IND_cCurrChar
               MVC   IFO_cCurrChar,IND_cCurrChar
               ASI   IND_pos,1
               MVC   IFO_pos,IND_pos
*
               ISTB_AppendChar OBJECT=G_ISTB_tmp,WORK=WORKD04,         X
               CHAR=IFO_cCurrChar
*
               IF (CLC,IND_pos,LT,IND_nCurrLineLength) THEN
                  ISTR_CharAt OBJECT=IND_ISTR_currLine,WORK=WORKD04,   X
               POS=IND_pos,CHAROUT=IND_cPeekChar
                  MVC   IFO_cPeekChar,IND_cPeekChar
*
                  MVC   POSD04,IND_pos
                  ASI   POSD04,1
*
                  IF (CLC,POSD04,LT,IND_nCurrLineLength) THEN
                     ISTR_CharAt OBJECT=IND_ISTR_currLine,WORK=WORKD04,X
               POS=POSD04,CHAROUT=IND_cPeekChar2
                     MVC   IFO_cPeekChar2,IND_cPeekChar
                  ENDIF
               ENDIF
            ELSE
               MVI   IND_bEOL,C'Y'
               MVI   IFO_bEOL,C'Y'
*
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD04,      X
               ZSTR==X'C5D6D300' * EOL
            ENDIF
         ENDIF
*
         L     R2,G_ISTB_tmp
         L     R2,STB_lpBuf-STB_obj(,R2)
         ILOG_Write OBJECT=G_ILOG,WORK=WORKD04,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_DEBUG3
*
IND#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK601D_IND                  DC    C'MAK601D Got next character ',X'00'
*
WORKDSAD04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKD04                      DS    3A
*
IIFO_ii_D04                  DS    A
*
POSD04                       DS    F
*
WORKDSAD04_SIZ               EQU   *-WORKDSAD04
*
LWZMINP  CSECT
*
         DROP
*
* IIND Open
*
IND#05   CEEENTRY AUTO=WORKDSAD05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAD05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING IND_obj,R8        * Address object DSECT
*
         CLI   IND_bOpen,C'Y'    * If already open
         BE    IND#05_RET        * Skip to end
*
         L     R2,4(,R1)         * Parm 2 is DD name ptr
         MVC   IND_cDDName,0(R2) * Copy DD name
*
         IF (CLC,IND_lpDCB,EQ,=A(0)) THEN * Is DCB mem allocated yet?
            GETMAIN RU,LV=DCB_DSECT_SIZ,LOC=24 * Get stg below line
            ST    R1,IND_lpDCB   * Save DCB ptr
*
*           Initialize DCB area with constants
            MVC   DCBIND-DCB_DSECT(DCBIND_SIZ,R1),CDCBIND
            MVC   DCBEIND-DCB_DSECT(DCBEIND_SIZ,R1),CDCBEIND
*
            LA    R7,DCBIND-DCB_DSECT(,R1)  * Point R7 to makefile DCB
            LA    R6,DCBEIND-DCB_DSECT(,R1) * Point R6 to makefile DCBE
            ST    R6,DCBDCBE-IHADCB(,R7)    * Store ptr to DCBE in DCB
            LA    R5,IND#05_EOF          * Get address of EODAD routine
            ST    R5,DCBEEODA-DCBE(,R6)  * Store address in DCBE
*
            LA    R14,DCBIND-DCB_DSECT(,R1)
            USING IHADCB,R14
            MVC   DCBDDNAM,IND_cDDName
            DROP  R14
         ENDIF
*
         L     R14,IND_lpDCB     * Point R14 to DCB DSECT
         LA    R2,DCBIND-DCB_DSECT(,R14)   * Point R2 to DCB
         MVC   G_OPEND,G_OPENL
*
*        Open DCBIND DCB
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)
*
         LTR   R15,R15           * Test return code
         IF (NZ) THEN
            CVD   R15,G_DEC8       * Turn OPEN return code
            UNPK  G_ZONED8,G_DEC8  * into zoned
            OI    G_ZONED8+7,X'F0' * and get rid of sign
*
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKD05
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD05,         X
               ZSTR=MAK101E_D05
            MVC   WDDNAMED05,IND_cDDName
            MVI   WDDNAMED05+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD05,         X
               ZSTR=WDDNAMED05
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD05,         X
               ZSTR==X'4000'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD05,         X
               ZSTR=G_ZONED8+6
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKD05,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_ERROR
*
            CALL  CEE3ABD,(=A(1004),=A(3)),MF=(E,WORKD05)
         ENDIF
*
         MVI   IND_bOpen,C'Y'    * Set flag that DCB is now open
*
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKD05
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD05,            X
               ZSTR=MAK303I_D05
         MVC   WDDNAMED05,IND_cDDName
         MVI   WDDNAMED05+8,X'00'
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD05,            X
               ZSTR=WDDNAMED05
*
         L     R2,G_ISTB_tmp
         L     R2,STB_lpBuf-STB_obj(,R2)
         ILOG_Write OBJECT=G_ILOG,WORK=WORKD05,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_INFO
*
IND#05_RET EQU   *
         CEETERM
*
* IND file is EOF (EODAD routine)
*
IND#05_EOF DS    0H
         MVI   IND_bEOF,C'Y'
         BR    R7
*
         LTORG
*
                             DS    0F
*                                                          DDNAME__ RC
MAK101E_D05                  DC    C'MAK101E Error opening ',X'00'
                             DS    0F
MAK303I_D05                  DC    C'MAK303I Opened input data set withX
                DD name ',X'00'
*
* Constant initialize DCB area
CDCBIND                      DCB   MACRF=(GM),DSORG=PS,RECFM=FB,       X
               LRECL=80,DCBE=CDCBEIND
DCBIND_SIZ                   EQU   *-CDCBIND
*
* Constant initialize DCBE area
CDCBEIND                     DCBE  EODAD=0,RMODE31=BUFF
DCBEIND_SIZ                  EQU   *-CDCBEIND
*
WORKDSAD05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKD05                      DS    3A
*
WDDNAMED05                   DS    CL8,C
*
WORKDSAD05_SIZ               EQU   *-WORKDSAD05
*
* DSECT with DCBs
DCB_DSECT                    DSECT
*
DCBIND                       DS    0F
                             ORG   *+DCBIND_SIZ
*
DCBEIND                      DS    0F
                             ORG   *+DCBEIND_SIZ
*
DCB_DSECT_SIZ                EQU   *-DCB_DSECT
*
* DCECT for addressing a DCB
         DCBD  DSORG=PS,DEVD=DA
*
* DSECT for addressing a DCBE
         IHADCBE
*
LWZMINP  CSECT
*
         DROP
*
* IIND Close
*
IND#06   CEEENTRY AUTO=WORKDSAD06_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAD06,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING IND_obj,R8        * Address object DSECT
*
         CLI   IND_bOpen,C'N'    * If not open
         BE    IND#06_RET        * Skip to end
*
         L     R14,IND_lpDCB     * Point R14 to DCB DSECT
         LA    R2,DCBIND-DCB_DSECT(,14) * Point R2 to DCB
         MVC   G_CLOSED,G_CLOSEL * Init CLOSE execute mem
*
*        Close DCBIND DCB
         CLOSE ((R2)),MODE=31,MF=(E,G_CLOSED)
*
         LTR   R15,R15           * Test return code
         IF (NZ) THEN
            CVD   R15,G_DEC8       * Turn CLOSE return code
            UNPK  G_ZONED8,G_DEC8  * into zoned
            OI    G_ZONED8+7,X'F0' * and get rid of sign
            MVI   G_ZONED8+8,X'00' * Zero terminate
*
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKD06
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD06,         X
               ZSTR=MAK102E_D06
            MVC   WDDNAMED06,IND_cDDName
            MVI   WDDNAMED06+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD06,         X
               ZSTR=WDDNAMED06
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD06,         X
               ZSTR==X'4000'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD06,         X
               ZSTR=G_ZONED8+6
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKD06,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_ERROR
*
            CALL  CEE3ABD,(=A(1004),=A(3)),MF=(E,WORKD06)
         ENDIF
*
         MVI   IND_bOpen,C'N'    * Set flag that DCB is now closed
*
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKD06
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD06,            X
               ZSTR=MAK304I_D06
         MVC   WDDNAMED06,IND_cDDName
         MVI   WDDNAMED06+8,X'00'
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD06,            X
               ZSTR=WDDNAMED06
*
         L     R2,G_ISTB_tmp
         L     R2,STB_lpBuf-STB_obj(,R2)
         ILOG_Write OBJECT=G_ILOG,WORK=WORKD06,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_INFO
*
IND#06_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
*                                                          DDNAME__ RC
MAK102E_D06                  DC    C'MAK101E Error closing ',X'00'
                             DS    0F
MAK304I_D06                  DC    C'MAK304I Closed input data set withX
                DD name ',X'00'
*
WORKDSAD06                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKD06                      DS    3A
*
WDDNAMED06                   DS    CL8,C
*
WORKDSAD06_SIZ               EQU   *-WORKDSAD06
*
LWZMINP  CSECT
*
         DROP
*
* IIND Read a line
*
IND#99   CEEENTRY AUTO=WORKDSAD99_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAD99,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING IND_obj,R8        * Address object DSECT
*
         MVI   IND_bEOL,C'N'     * Reset End-Of-Line to false
*
         L     R14,IND_lpDCB     * Point R14 to DCB DSECT
         LA    R4,DCBIND-DCB_DSECT(,R14) * Point R4 to DCB
         LA    R7,IND#99_EOF_RET * At EOF just return
*
         GET   (R4),LINED99      * Read a record
*
IND#99_EOF_RET EQU   *
         IF (CLI,IND_bEOF,NE,C'Y') THEN * Not EOF yet?
            LA    R2,MAK504D_D99
         ELSE                    * Or EOF
            LA    R2,MAK505D_D99
         ENDIF
         ILOG_Write OBJECT=G_ILOG,WORK=WORKD99,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
         IF (CLI,IND_bEOF,NE,C'Y') THEN * Not EOF yet?
            MVI   LINED99+L'LINED99,X'00' * Append zero terminator
*
*           Set ISTR string value to line just read
            ISTR_Set OBJECT=IND_ISTR_currLine,WORK=WORKD99,STR=LINED99
*
            MVC   IND_nCurrLineLength,=A(72) * Line length always 72
            MVC   IND_pos,=A(0)  * Reset pos
            ASI   IND_nLine,1    * Increase line number
         ENDIF
*
IND#99_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK504D_D99                  DC    C'MAK504D Read an input line',X'00'
                             DS    0F
MAK505D_D99                  DC    C'MAK505D Input file is EOF',X'00'
*
WORKDSAD99                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKD99                      DS    3A
*
LINED99                      DS    CL80
                             DS    C
*
WORKDSAD99_SIZ               EQU   *-WORKDSAD99
*
LWZMINP  CSECT
*
         DROP
*
* IINF QueryInterface
*
INF#01   MQRYIFCE SUF=F01,IFACE=IINF
*
* IINF AddRef
*
INF#02   MADDREF
*
* IINF Release
*
INF#03   CEEENTRY AUTO=WORKDSAF03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAF03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    INF#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING INF_obj,R6
*
            ISTR_Release OBJECT=INF_ISTB_currLine,WORK=WORKF03
            MVC   INF_ISTB_currLine,=A(0)
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRF03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRF03,FCF03),MF=(E,WORKF03)
*
            L     R15,G_OBJCOUNT
            BCTR  R15,R0
            ST    R15,G_OBJCOUNT
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R6,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF03,      X
               ZSTR=MAK502D_INF
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKF03,LINE=0(,R2),    , X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
INF#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_INF                  DC    C'MAK502D Deleted IINF object '
                             DC    X'00'
*
WORKDSAF03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRF03                    DS    A
WORKF03                      DS    3A
*
WORKDSAF03_SIZ               EQU   *-WORKDSAF03
*
LWZMINP  CSECT
*
         DROP
*
* IINF GetNextChar
*
INF#04   CEEENTRY AUTO=WORKDSAF04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAF04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING INF_obj,R8        * Address object DSECT
*
         L     R7,4(,R1)         * Parm 2 is IIFO ptr
         ST    R7,IIFO_ii_F04    * Save as local var
         USING IFO_obj,R7        * Addressability for ii
*
         MVI   IFO_cCurrChar,X'00'
         MVI   IFO_cPeekChar,X'00'
         MVI   IFO_cPeekChar2,X'00'
         MVI   IFO_bEOL,C'N'
         MVI   IFO_bEOF,C'N'
         MVC   IFO_pos,INF_pos
         MVC   IFO_nLine,INF_nLine
*
         IF (CLI,INF_bEOL,EQ,C'Y') THEN
            L     R15,=A(INF#99) * Read next line
            BASR  R14,R15
*
            MVC   IFO_nLine,INF_nLine
         ENDIF
*
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF04
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,            X
               ZSTR=MAK601D_INF
*
         IF (CLI,INF_bEOF,EQ,C'Y') THEN
            MVI   IFO_bEOF,C'Y'
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,         X
               ZSTR==X'C5D6C600' * EOF
         ELSE
            IF (CLC,INF_pos,LT,INF_nCurrLineLength) THEN
               ISTB_CharAt OBJECT=INF_ISTB_currLine,WORK=WORKF04,      X
               POS=INF_pos,CHAROUT=INF_cCurrChar
               MVC   IFO_cCurrChar,INF_cCurrChar
               ASI   INF_pos,1
               MVC   IFO_pos,INF_pos
*
               ISTB_AppendChar OBJECT=G_ISTB_tmp,WORK=WORKF04,         X
               CHAR=IFO_cCurrChar
*
               IF (CLC,INF_pos,LT,INF_nCurrLineLength) THEN
                  ISTB_CharAt OBJECT=INF_ISTB_currLine,WORK=WORKF04,   X
               POS=INF_pos,CHAROUT=INF_cPeekChar
                  MVC   IFO_cPeekChar,INF_cPeekChar
*
                  MVC   POSF04,INF_pos
                  ASI   POSF04,1
*
                  IF (CLC,POSF04,LT,INF_nCurrLineLength) THEN
                     ISTB_CharAt OBJECT=INF_ISTB_currLine,WORK=WORKF04,X
               POS=POSF04,CHAROUT=INF_cPeekChar2
                     MVC   IFO_cPeekChar2,INF_cPeekChar
                  ENDIF
               ENDIF
            ELSE
               MVI   INF_bEOL,C'Y'
               MVI   IFO_bEOL,C'Y'
*
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,      X
               ZSTR==X'C5D6D300' * EOL
            ENDIF
         ENDIF
*
         L     R2,G_ISTB_tmp
         L     R2,STB_lpBuf-STB_obj(,R2)
         ILOG_Write OBJECT=G_ILOG,WORK=WORKF04,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_DEBUG3
*
INF#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK601D_INF                  DC    C'MAK601D Got next character ',X'00'
*
WORKDSAF04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKF04                      DS    3A
*
IIFO_ii_F04                  DS    A
*
POSF04                       DS    F
*
WORKDSAF04_SIZ               EQU   *-WORKDSAF04
*
LWZMINP  CSECT
*
         DROP
*
* IINF Open
*
INF#05   CEEENTRY AUTO=WORKDSAF05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAF05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING INF_obj,R8        * Address object DSECT
*
         CLI   INF_bOpen,C'Y'    * If already open
         BE    INF#05_RET        * Skip to end
*
         L     R2,4(,R1)         * Parm 2 is DD name ptr
         MVC   INF_cDDName,0(R2) * Copy DD name
*
         IFMG_DDNameToPath OBJECT=G_IFMG,WORK=WORKF05,                 X
               DDNAME=INF_cDDName,PATH=WPATHF05
*
         IUSS_BPX1OPN OBJECT=G_IUSS,WORK=WORKF05,PATH=WPATHF05,        X
               FDESC_RETVAL=INF_FD
*
         IF (CLC,G_RETCODE,EQ,=A(0)) THEN
            MVI   INF_bOpen,C'Y'  * Set flag that DCB is now open
*
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF05
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF05,         X
               ZSTR=MAK303I_F05
            MVC   WDDNAMEF05,INF_cDDName
            MVI   WDDNAMEF05+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF05,         X
               ZSTR=WDDNAMEF05
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF05,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_INFO
         ENDIF
*
INF#05_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
*                                                          DDNAME__ RC
MAK101E_F05                  DC    C'MAK101E Error opening ',X'00'
                             DS    0F
MAK303I_F05                  DC    C'MAK303I Opened input data set withX
                DD name ',X'00'
*
WORKDSAF05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKF05                      DS    4A
*
WPATHF05                     DS    A
WDDNAMEF05                   DS    CL8,C
*
WORKDSAF05_SIZ               EQU   *-WORKDSAF05
*
LWZMINP  CSECT
*
         DROP
*
* IINF Close
*
INF#06   CEEENTRY AUTO=WORKDSAF06_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAF06,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING INF_obj,R8        * Address object DSECT
*
         CLI   INF_bOpen,C'N'    * If not open
         BE    INF#06_RET        * Skip to end
*
         IUSS_BPX1CLO OBJECT=G_IUSS,WORK=WORKF06,FDESC=INF_FD
*
         IF (CLC,G_RETCODE,EQ,=A(0)) THEN
            MVI   INF_bOpen,C'N' * Set flag that DCB is now closed
*
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF06
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF06,         X
               ZSTR=MAK304I_F06
            MVC   WDDNAMEF06,INF_cDDName
            MVI   WDDNAMEF06+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF06,         X
               ZSTR=WDDNAMEF06
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF06,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_INFO
         ENDIF
*
INF#06_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
*                                                          DDNAME__ RC
MAK102E_F06                  DC    C'MAK101E Error closing ',X'00'
                             DS    0F
MAK304I_F06                  DC    C'MAK304I Closed input data set withX
                DD name ',X'00'
*
WORKDSAF06                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKF06                      DS    3A
*
WDDNAMEF06                   DS    CL8,C
*
WORKDSAF06_SIZ               EQU   *-WORKDSAF06
*
LWZMINP  CSECT
*
         DROP
*
* IINF Read a line
*
INF#99   CEEENTRY AUTO=WORKDSAF99_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAF99,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING INF_obj,R8        * Address object DSECT
*
         MVI   INF_bEOL,C'N'     * Reset End-Of-Line to false
*
         ISTB_Init OBJECT=INF_ISTB_currLine,WORK=WORKF99
*
INF#99_RED EQU   *
         IUSS_BPX1RED OBJECT=G_IUSS,WORK=WORKF99,FDESC=INF_FD,         X
               RETCHAR=CHARF99,RETEOF=EOFF99
*
         IF (CLC,G_RETCODE,EQ,=A(0)) THEN
            IF (CLI,EOFF99,EQ,X'00') THEN
               IF (CLI,CHARF99,NE,X'15') THEN
                  ISTB_AppendChar OBJECT=INF_ISTB_currLine,            X
               WORK=WORKF99,CHAR=CHARF99
*
                  B     INF#99_RED
               ELSE
                  L     R14,INF_ISTB_currLine
                  L     R14,STB_nStrLen-STB_Obj(,R14)
                  ST    R14,INF_nCurrLineLength
                  MVC   INF_pos,=A(0)  * Reset pos
                  ASI   INF_nLine,1    * Increase line number
               ENDIF
            ELSE
               MVI   INF_bEOF,C'Y'
            ENDIF
         ENDIF
*
INF#99_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK504D_F99                  DC    C'MAK504D Read an input line',X'00'
                             DS    0F
MAK505D_F99                  DC    C'MAK505D Input file is EOF',X'00'
*
WORKDSAF99                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKF99                      DS    4A
*
CHARF99                      DS    C
EOFF99                       DS    C
*
WORKDSAF99_SIZ               EQU   *-WORKDSAF99
*
LWZMINP  CSECT
*
         DROP
*
* IINS QueryInterface
*
INS#01   MQRYIFCE SUF=S01,IFACE=IINS
*
* IINS AddRef
*
INS#02   MADDREF
*
* IINS Release
*
INS#03   CEEENTRY AUTO=WORKDSAS03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAS03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    INS#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING INS_obj,R6
*
            ISTR_Release OBJECT=INS_ISTR_currLine,WORK=WORKS03
            MVC   INS_ISTR_currLine,=A(0)
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRS03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRS03,FCS03),MF=(E,WORKS03)
*
            L     R15,G_OBJCOUNT
            BCTR  R15,R0
            ST    R15,G_OBJCOUNT
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R6,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS03,      X
               ZSTR=MAK502D_INS
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKS03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
INS#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_INS                  DC    C'MAK502D Deleted IINS object '
                             DC    X'00'
*
WORKDSAS03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRS03                    DS    A
WORKS03                      DS    3A
*
WORKDSAS03_SIZ               EQU   *-WORKDSAS03
*
LWZMINP  CSECT
*
         DROP
*
* IINS GetNextChar
*
INS#04   CEEENTRY AUTO=WORKDSAS04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAS04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING INS_obj,R8        * Address object DSECT
*
         L     R7,4(,R1)         * Parm 2 is IIFO ptr
         ST    R7,IIFO_ii_S04    * Save as local var
         USING IFO_obj,R7        * Addressability for ii
*
         MVI   IFO_cCurrChar,X'00'
         MVI   IFO_cPeekChar,X'00'
         MVI   IFO_cPeekChar2,X'00'
         MVI   IFO_bEOL,C'N'
         MVI   IFO_bEOF,C'N'
         MVC   IFO_pos,INS_pos
         MVC   IFO_nLine,INS_nLine
*
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS04
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS04,            X
               ZSTR=MAK601D_INS
*
         IF (CLI,INS_bEOL,EQ,C'Y'),OR,                                 X
               (CLI,INS_bEOF,EQ,C'Y') THEN
            MVI   IFO_bEOL,C'Y'
            MVI   IFO_bEOF,C'Y'
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS04,         X
               ZSTR==X'C5D6C600' * EOF
         ELSE
            IF (CLC,INS_pos,LT,INS_nCurrLineLength) THEN
               ISTR_CharAt OBJECT=INS_ISTR_currLine,WORK=WORKS04,      X
               POS=INS_pos,CHAROUT=INS_cCurrChar
               MVC   IFO_cCurrChar,INS_cCurrChar
               ASI   INS_pos,1
               MVC   IFO_pos,INS_pos
*
               ISTB_AppendChar OBJECT=G_ISTB_tmp,WORK=WORKS04,         X
               CHAR=IFO_cCurrChar
*
               IF (CLC,INS_pos,LT,INS_nCurrLineLength) THEN
                  ISTR_CharAt OBJECT=INS_ISTR_currLine,WORK=WORKS04,   X
               POS=INS_pos,CHAROUT=INS_cPeekChar
                  MVC   IFO_cPeekChar,INS_cPeekChar
*
                  MVC   POSS04,INS_pos
                  ASI   POSS04,1
*
                  IF (CLC,POSS04,LT,INS_nCurrLineLength) THEN
                     ISTR_CharAt OBJECT=INS_ISTR_currLine,WORK=WORKS04,X
               POS=POSS04,CHAROUT=INS_cPeekChar2
                     MVC   IFO_cPeekChar2,INS_cPeekChar
                  ENDIF
               ENDIF
            ELSE
               MVI   INS_bEOL,C'Y'
               MVI   IFO_bEOL,C'Y'
*
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS04,      X
               ZSTR==X'C5D6D300' * EOL
            ENDIF
         ENDIF
*
         L     R2,G_ISTB_tmp
         L     R2,STB_lpBuf-STB_obj(,R2)
         ILOG_Write OBJECT=G_ILOG,WORK=WORKS04,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_DEBUG3
*
INS#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK601D_INS                  DC    C'MAK601D Got next character ',X'00'
*
WORKDSAS04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKS04                      DS    3A
*
IIFO_ii_S04                  DS    A
*
POSS04                       DS    F
*
WORKDSAS04_SIZ               EQU   *-WORKDSAS04
*
LWZMINP  CSECT
*
         DROP
*
* IINS Init
*
INS#05   CEEENTRY AUTO=WORKDSAS05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAS05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING INS_obj,R8        * Address object DSECT
*
         L     R7,4(,R1)         * Parm 2 is ptr to zero term string
*
         MVC   INS_nLine,8(R1)   * Parm 3 is line number
*
         ISTR_Set OBJECT=INS_ISTR_currLine,WORK=WORKS05,STR=0(,R7)
*
         L     R2,INS_ISTR_currLine
         MVC   INS_nCurrLineLength,STR_nStrLen-STR_obj(R2)
*
         MVC   INS_pos,=F'0'
*
INS#05_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAS05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKS05                      DS    2A
*
WORKDSAS05_SIZ               EQU   *-WORKDSAS05
*
LWZMINP  CSECT
*
         DROP
*
* IIFO QueryInterface
*
IFO#01   MQRYIFCE SUF=I01,IFACE=IIFO
*
* IIFO AddRef
*
IFO#02   MADDREF
*
* IIFO Release
*
IFO#03   MRELEASE SUF=I03,OBJ=IFO
*
         COPY  REGS              * Register equates
*
         END   LWZMINP
