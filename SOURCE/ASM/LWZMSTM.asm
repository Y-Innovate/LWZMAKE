*---------------------------------------------------------------------*
* Program    : LWZMSTM                                                *
* Description: COM object for Make Statements                         *
*---------------------------------------------------------------------*
         TITLE 'LWZMSTM'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates new Statement objects
*
LWZMSTM  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
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
*        Was a new ST1 object requested? (Assignment)
         IF (CLC,0(16,R6),EQ,G_IST1_GUID) THEN
            MNEWOBJ OBJTYPE=ST1,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST1_obj,R14
            MVI   ST1_statementType,C'A'
            MVI   ST1_recipeStatement,C'N'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST1
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST2 object requested? (Call)
         ELSEIF (CLC,0(16,R6),EQ,G_IST2_GUID) THEN
            MNEWOBJ OBJTYPE=ST2,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST2_obj,R14
            MVI   ST2_statementType,C'C'
            MVI   ST2_recipeStatement,C'N'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST2
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST3 object requested? (Phony)
         ELSEIF (CLC,0(16,R6),EQ,G_IST3_GUID) THEN
            MNEWOBJ OBJTYPE=ST3,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST3_obj,R14
            MVI   ST3_statementType,C'P'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST3
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST4 object requested? (RecipePrefix)
         ELSEIF (CLC,0(16,R6),EQ,G_IST4_GUID) THEN
            MNEWOBJ OBJTYPE=ST4,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST4_obj,R14
            MVI   ST4_statementType,C'-'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST4
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST5 object requested? (Rule)
         ELSEIF (CLC,0(16,R6),EQ,G_IST5_GUID) THEN
            MNEWOBJ OBJTYPE=ST5,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST5_obj,R14
            MVI   ST5_statementType,C'R'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST5
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST6 object requested? (Sh)
         ELSEIF (CLC,0(16,R6),EQ,G_IST6_GUID) THEN
            MNEWOBJ OBJTYPE=ST6,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST6_obj,R14
            MVI   ST6_statementType,C'B'
            MVI   ST6_recipeStatement,C'N'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST6
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST7 object requested? (UssHome)
         ELSEIF (CLC,0(16,R6),EQ,G_IST7_GUID) THEN
            MNEWOBJ OBJTYPE=ST7,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST7_obj,R14
            MVI   ST7_statementType,C'U'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST7
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new ST8 object requested? (BuildWhen)
         ELSEIF (CLC,0(16,R6),EQ,G_IST8_GUID) THEN
            MNEWOBJ OBJTYPE=ST8,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING ST8_obj,R14
            MVI   ST8_statementType,C'B'
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_ST8
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
LWZMSTM_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
ST1#01A                      DC    A(ST1#01)   * QueryInterface
ST1#02A                      DC    A(ST1#02)   * AddRef
ST1#03A                      DC    A(ST1#03)   * Release
ST1#04A                      DC    A(ST1#04)   * Init
*
                             DS    0F
ST2#01A                      DC    A(ST2#01)   * QueryInterface
ST2#02A                      DC    A(ST2#02)   * AddRef
ST2#03A                      DC    A(ST2#03)   * Release
ST2#04A                      DC    A(ST2#04)   * Init
*
                             DS    0F
ST3#01A                      DC    A(ST3#01)   * QueryInterface
ST3#02A                      DC    A(ST3#02)   * AddRef
ST3#03A                      DC    A(ST3#03)   * Release
ST3#04A                      DC    A(ST3#04)   * Init
*
                             DS    0F
ST4#01A                      DC    A(ST4#01)   * QueryInterface
ST4#02A                      DC    A(ST4#02)   * AddRef
ST4#03A                      DC    A(ST4#03)   * Release
ST4#04A                      DC    A(ST4#04)   * Init
*
                             DS    0F
ST5#01A                      DC    A(ST5#01)   * QueryInterface
ST5#02A                      DC    A(ST5#02)   * AddRef
ST5#03A                      DC    A(ST5#03)   * Release
ST5#04A                      DC    A(ST5#04)   * Init
*
                             DS    0F
ST6#01A                      DC    A(ST6#01)   * QueryInterface
ST6#02A                      DC    A(ST6#02)   * AddRef
ST6#03A                      DC    A(ST6#03)   * Release
ST6#04A                      DC    A(ST6#04)   * Init
*
                             DS    0F
ST7#01A                      DC    A(ST7#01)   * QueryInterface
ST7#02A                      DC    A(ST7#02)   * AddRef
ST7#03A                      DC    A(ST7#03)   * Release
ST7#04A                      DC    A(ST7#04)   * Init
*
                             DS    0F
ST8#01A                      DC    A(ST8#01)   * QueryInterface
ST8#02A                      DC    A(ST8#02)   * AddRef
ST8#03A                      DC    A(ST8#03)   * Release
ST8#04A                      DC    A(ST8#04)   * Init
*
                             DS    0F
MAK501D_ST1                  DC    C'MAK501D Created IST1 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST2                  DC    C'MAK501D Created IST2 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST3                  DC    C'MAK501D Created IST3 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST4                  DC    C'MAK501D Created IST4 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST5                  DC    C'MAK501D Created IST5 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST6                  DC    C'MAK501D Created IST6 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST7                  DC    C'MAK501D Created IST7 object '
                             DC    X'00'
                             DS    0F
MAK501D_ST8                  DC    C'MAK501D Created IST8 object '
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
         COPY  DSST1             * IST1 obj DSECT
         COPY  DSST2             * IST2 obj DSECT
         COPY  DSST3             * IST3 obj DSECT
         COPY  DSST4             * IST4 obj DSECT
         COPY  DSST5             * IST5 obj DSECT
         COPY  DSST6             * IST6 obj DSECT
         COPY  DSST7             * IST7 obj DSECT
         COPY  DSST8             * IST8 obj DSECT
         COPY  DSSTB             * ISTB obj DSECT
*
         DROP
*
LWZMSTM  CSECT
*
* IST1 QueryInterface
*
ST1#01   MQRYIFCE SUF=A01,IFACE=IST1
*
* IST1 AddRef
*
ST1#02   MADDREF
*
* IST1 Release
*
ST1#03   CEEENTRY AUTO=WORKDSAA03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAA03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST1#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST1_obj,R6
*
            IF (CLC,ST1_ITFO_tiName,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST1_ITFO_tiName,WORK=WORKA03
               MVC   ST1_ITFO_tiName,=A(0)
            ENDIF
*
            IF (CLC,ST1_ITFO_tiOperator,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST1_ITFO_tiOperator,WORK=WORKA03
               MVC   ST1_ITFO_tiOperator,=A(0)
            ENDIF
*
            IF (CLC,ST1_ITFO_tiValue,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST1_ITFO_tiValue,WORK=WORKA03
               MVC   ST1_ITFO_tiValue,=A(0)
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRA03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRA03,FCA03),MF=(E,WORKA03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKA03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKA03,      X
               ZSTR=MAK502D_ST1
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKA03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKA03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST1#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST1                  DC    C'MAK502D Deleted IST1 object '
                             DC    X'00'
*
WORKDSAA03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRA03                    DS    A
WORKA03                      DS    3A
*
WORKDSAA03_SIZ               EQU   *-WORKDSAA03
*
LWZMSTM  CSECT
*
         DROP
*
* IST1 Init
*
ST1#04   CEEENTRY AUTO=WORKDSAA04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST1_obj,R8        * Address object DSECT
*
         MVC   ST1_ITFO_tiName,4(R1) * Parm 2 is ITFO Name token
*
         MVC   ST1_ITFO_tiOperator,8(R1) * Parm 3 is ITFO Operator tkn
*
         MVC   ST1_ITFO_tiValue,12(R1) * Parm 4 is ITFO Value token
*
         L     R14,16(,R1)       * Parm 5 is flag recipeStatement
         STC   R14,ST1_recipeStatement
*
         ITFO_AddRef OBJECT=ST1_ITFO_tiName,WORK=WORKA04
*
         ITFO_AddRef OBJECT=ST1_ITFO_tiOperator,WORK=WORKA04
*
         ITFO_AddRef OBJECT=ST1_ITFO_tiValue,WORK=WORKA04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKA04,LINE=MAK503D_A04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST1#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_A04                  DC    C'MAK503D Initialized IST1 object',XX
               '00'
*
WORKDSAA04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA04                      DS    3A
*
WORKDSAA04_SIZ               EQU   *-WORKDSAA04
*
LWZMSTM  CSECT
*
         DROP
*
* IST2 QueryInterface
*
ST2#01   MQRYIFCE SUF=B01,IFACE=IST2
*
* IST2 AddRef
*
ST2#02   MADDREF
*
* IST2 Release
*
ST2#03   CEEENTRY AUTO=WORKDSAB03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAB03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST2#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST2_obj,R6
*
            IF (CLC,ST2_ITFO_tiCall,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST2_ITFO_tiCall,WORK=WORKB03
               MVC   ST2_ITFO_tiCall,=A(0)
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRB03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRB03,FCB03),MF=(E,WORKB03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKB03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKB03,      X
               ZSTR=MAK502D_ST2
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKB03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKB03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST2#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST2                  DC    C'MAK502D Deleted IST2 object '
                             DC    X'00'
*
WORKDSAB03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRB03                    DS    A
WORKB03                      DS    3A
*
WORKDSAB03_SIZ               EQU   *-WORKDSAB03
*
LWZMSTM  CSECT
*
         DROP
*
* IST2 Init
*
ST2#04   CEEENTRY AUTO=WORKDSAB04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST2_obj,R8        * Address object DSECT
*
         MVC   ST2_ITFO_tiCall,4(R1) * Parm 2 is ITFO Call token
*
         L     R14,8(,R1)        * Parm 3 is flag recipeStatement
         STC   R14,ST2_recipeStatement
*
         ITFO_AddRef OBJECT=ST2_ITFO_tiCall,WORK=WORKB04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKB04,LINE=MAK503D_B04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST2#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_B04                  DC    C'MAK503D Initialized IST2 object',XX
               '00'
*
WORKDSAB04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB04                      DS    3A
*
WORKDSAB04_SIZ               EQU   *-WORKDSAB04
*
LWZMSTM  CSECT
*
         DROP
*
* IST3 QueryInterface
*
ST3#01   MQRYIFCE SUF=C01,IFACE=IST3
*
* IST3 AddRef
*
ST3#02   MADDREF
*
* IST3 Release
*
ST3#03   CEEENTRY AUTO=WORKDSAC03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAC03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST3#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST3_obj,R6
*
            IF (CLC,ST3_ITFO_tiPhony,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST3_ITFO_tiPhony,WORK=WORKC03
               MVC   ST3_ITFO_tiPhony,=A(0)
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRC03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRC03,FCC03),MF=(E,WORKC03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKC03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKC03,      X
               ZSTR=MAK502D_ST3
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKC03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKC03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST3#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST3                  DC    C'MAK502D Deleted IST3 object '
                             DC    X'00'
*
WORKDSAC03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRC03                    DS    A
WORKC03                      DS    3A
*
WORKDSAC03_SIZ               EQU   *-WORKDSAC03
*
LWZMSTM  CSECT
*
         DROP
*
* IST3 Init
*
ST3#04   CEEENTRY AUTO=WORKDSAC04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAC04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST3_obj,R8        * Address object DSECT
*
         MVC   ST3_ITFO_tiPhony,4(R1) * Parm 2 is ITFO Phony token
*
         ITFO_AddRef OBJECT=ST3_ITFO_tiPhony,WORK=WORKC04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKC04,LINE=MAK503D_C04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST3#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_C04                  DC    C'MAK503D Initialized IST3 object',XX
               '00'
*
WORKDSAC04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKC04                      DS    3A
*
WORKDSAC04_SIZ               EQU   *-WORKDSAC04
*
LWZMSTM  CSECT
*
         DROP
*
* IST4 QueryInterface
*
ST4#01   MQRYIFCE SUF=D01,IFACE=IST4
*
* IST4 AddRef
*
ST4#02   MADDREF
*
* IST4 Release
*
ST4#03   CEEENTRY AUTO=WORKDSAD03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAD03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST4#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST4_obj,R6
*
            IF (CLC,ST4_ITFO_tiRecipePrefix,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST4_ITFO_tiRecipePrefix,WORK=WORKD03
               MVC   ST4_ITFO_tiRecipePrefix,=A(0)
            ENDIF
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
               ZSTR=MAK502D_ST4
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKD03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKD03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST4#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST4                  DC    C'MAK502D Deleted IST4 object '
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
LWZMSTM  CSECT
*
         DROP
*
* IST4 Init
*
ST4#04   CEEENTRY AUTO=WORKDSAD04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAD04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST4_obj,R8        * Address object DSECT
*
         MVC   ST4_ITFO_tiRecipePrefix,4(R1) * Parm 2 is ITFO Rec.p.tkn
*
         ITFO_AddRef OBJECT=ST4_ITFO_tiRecipePrefix,WORK=WORKD04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKD04,LINE=MAK503D_D04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST4#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_D04                  DC    C'MAK503D Initialized IST4 object',XX
               '00'
*
WORKDSAD04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKD04                      DS    3A
*
WORKDSAD04_SIZ               EQU   *-WORKDSAD04
*
LWZMSTM  CSECT
*
         DROP
*
* IST5 QueryInterface
*
ST5#01   MQRYIFCE SUF=E01,IFACE=IST5
*
* IST5 AddRef
*
ST5#02   MADDREF
*
* IST5 Release
*
ST5#03   CEEENTRY AUTO=WORKDSAE03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAE03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST5#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST5_obj,R6
*
            IF (CLC,ST5_ITFO_tiTargets,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST5_ITFO_tiTargets,WORK=WORKE03
               MVC   ST5_ITFO_tiTargets,=A(0)
            ENDIF
*
            IF (CLC,ST5_ITFO_tiPrereqs,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST5_ITFO_tiPrereqs,WORK=WORKE03
               MVC   ST5_ITFO_tiPrereqs,=A(0)
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRE03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRE03,FCE03),MF=(E,WORKE03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKE03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKE03,      X
               ZSTR=MAK502D_ST5
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKE03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKE03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST5#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST5                  DC    C'MAK502D Deleted IST5 object '
                             DC    X'00'
*
WORKDSAE03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRE03                    DS    A
WORKE03                      DS    3A
*
WORKDSAE03_SIZ               EQU   *-WORKDSAE03
*
LWZMSTM  CSECT
*
         DROP
*
* IST5 Init
*
ST5#04   CEEENTRY AUTO=WORKDSAE04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAE04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST5_obj,R8        * Address object DSECT
*
         MVC   ST5_ITFO_tiTargets,4(R1) * Parm 2 is ITFO targets
*
         MVC   ST5_ITFO_tiPrereqs,8(R1) * Parm 3 is ITFO prereqs
*
         ITFO_AddRef OBJECT=ST5_ITFO_tiTargets,WORK=WORKE04
*
         ITFO_AddRef OBJECT=ST5_ITFO_tiPrereqs,WORK=WORKE04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKE04,LINE=MAK503D_E04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST5#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_E04                  DC    C'MAK503D Initialized IST5 object',XX
               '00'
*
WORKDSAE04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKE04                      DS    3A
*
WORKDSAE04_SIZ               EQU   *-WORKDSAE04
*
LWZMSTM  CSECT
*
         DROP
*
* IST6 QueryInterface
*
ST6#01   MQRYIFCE SUF=F01,IFACE=IST6
*
* IST6 AddRef
*
ST6#02   MADDREF
*
* IST6 Release
*
ST6#03   CEEENTRY AUTO=WORKDSAF03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAF03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST6#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST6_obj,R6
*
            IF (CLC,ST6_ITFO_tiSh,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST6_ITFO_tiSh,WORK=WORKF03
               MVC   ST6_ITFO_tiSh,=A(0)
            ENDIF
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
               ZSTR=MAK502D_ST6
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKF03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST6#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST6                  DC    C'MAK502D Deleted IST6 object '
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
LWZMSTM  CSECT
*
         DROP
*
* IST6 Init
*
ST6#04   CEEENTRY AUTO=WORKDSAF04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAF04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST6_obj,R8        * Address object DSECT
*
         MVC   ST6_ITFO_tiSh,4(R1) * Parm 2 is ITFO Sh token
*
         L     R14,8(,R1)        * Parm 3 is flag recipeStatement
         STC   R14,ST6_recipeStatement
*
         ITFO_AddRef OBJECT=ST6_ITFO_tiSh,WORK=WORKF04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKF04,LINE=MAK503D_F04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST6#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_F04                  DC    C'MAK503D Initialized IST6 object',XX
               '00'
*
WORKDSAF04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKF04                      DS    3A
*
WORKDSAF04_SIZ               EQU   *-WORKDSAF04
*
LWZMSTM  CSECT
*
         DROP
*
* IST7 QueryInterface
*
ST7#01   MQRYIFCE SUF=G01,IFACE=IST7
*
* IST7 AddRef
*
ST7#02   MADDREF
*
* IST7 Release
*
ST7#03   CEEENTRY AUTO=WORKDSAG03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAG03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST7#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST7_obj,R6
*
            IF (CLC,ST7_ITFO_tiHome,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST7_ITFO_tiHome,WORK=WORKG03
               MVC   ST7_ITFO_tiHome,=A(0)
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRG03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRG03,FCG03),MF=(E,WORKG03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKG03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKG03,      X
               ZSTR=MAK502D_ST7
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKG03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKG03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST7#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST7                  DC    C'MAK502D Deleted IST7 object '
                             DC    X'00'
*
WORKDSAG03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRG03                    DS    A
WORKG03                      DS    3A
*
WORKDSAG03_SIZ               EQU   *-WORKDSAG03
*
LWZMSTM  CSECT
*
         DROP
*
* IST7 Init
*
ST7#04   CEEENTRY AUTO=WORKDSAG04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAG04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST7_obj,R8        * Address object DSECT
*
         MVC   ST7_ITFO_tiHome,4(R1) * Parm 2 is ITFO usshome token
*
         ITFO_AddRef OBJECT=ST7_ITFO_tiHome,WORK=WORKG04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKG04,LINE=MAK503D_G04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST7#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_G04                  DC    C'MAK503D Initialized IST7 object',XX
               '00'
*
WORKDSAG04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKG04                      DS    3A
*
WORKDSAG04_SIZ               EQU   *-WORKDSAG04
*
LWZMSTM  CSECT
*
         DROP
*
* IST8 QueryInterface
*
ST8#01   MQRYIFCE SUF=H01,IFACE=IST8
*
* IST8 AddRef
*
ST8#02   MADDREF
*
* IST8 Release
*
ST8#03   CEEENTRY AUTO=WORKDSAH03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAH03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    ST8#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING ST8_obj,R6
*
            IF (CLC,ST8_ITFO_tiBuildWhen,NE,=A(0)) THEN
               ITFO_Release OBJECT=ST8_ITFO_tiBuildWhen,WORK=WORKH03
               MVC   ST8_ITFO_tiBuildWhen,=A(0)
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRH03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRH03,FCH03),MF=(E,WORKH03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKH03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKH03,      X
               ZSTR=MAK502D_ST8
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKH03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKH03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
ST8#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_ST8                  DC    C'MAK502D Deleted IST8 object '
                             DC    X'00'
*
WORKDSAH03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRH03                    DS    A
WORKH03                      DS    3A
*
WORKDSAH03_SIZ               EQU   *-WORKDSAH03
*
LWZMSTM  CSECT
*
         DROP
*
* IST8 Init
*
ST8#04   CEEENTRY AUTO=WORKDSAH04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAH04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING ST8_obj,R8        * Address object DSECT
*
         MVC   ST8_ITFO_tiBuildWhen,4(R1) * Parm 2 is ITFO BUILDWHEN
*
         ITFO_AddRef OBJECT=ST8_ITFO_tiBuildWhen,WORK=WORKH04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKH04,LINE=MAK503D_H04,       X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
ST8#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D_H04                  DC    C'MAK503D Initialized IST8 object',XX
               '00'
*
WORKDSAH04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKH04                      DS    3A
*
WORKDSAH04_SIZ               EQU   *-WORKDSAH04
*
LWZMSTM  CSECT
*
         DROP
*
         COPY  REGS              * Register equates
*
         END   LWZMSTM
