*---------------------------------------------------------------------*
* Program    : LWZMTOK                                                *
* Description: COM object for Tokenizer and TokenizerInfo             *
*---------------------------------------------------------------------*
         TITLE 'LWZMTOK'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates a new TOK or TFO object
*
LWZMTOK  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
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
*        Was a new TOK object requested?
         IF (CLC,0(16,R6),EQ,G_ITOK_GUID) THEN
            MNEWOBJ OBJTYPE=TOK,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING TOK_obj,R14
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
               ZSTR=MAK501D_TOK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new TFO object requested?
         ELSEIF (CLC,0(16,R6),EQ,G_ITFO_GUID) THEN
            MNEWOBJ OBJTYPE=TFO,WORK=WORK * Alloc new object
*
*           Init obj attributes
            LR    R4,R14
            USING TFO_obj,R4
*
            LA    R1,WORK
            ST    R4,0(,R1)
            L     R15,TFO#04A    * Call Init during instantiate
            BASR  R14,R15
*
            DROP  R4
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R4,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_TFO
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
LWZMTOK_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
TOK#01A                      DC    A(TOK#01)   * QueryInterface
TOK#02A                      DC    A(TOK#02)   * AddRef
TOK#03A                      DC    A(TOK#03)   * Release
TOK#04A                      DC    A(TOK#04)   * Init
TOK#05A                      DC    A(TOK#05)   * GetNextToken
*
                             DS    0F
TFO#01A                      DC    A(TFO#01)   * QueryInterface
TFO#02A                      DC    A(TFO#02)   * AddRef
TFO#03A                      DC    A(TFO#03)   * Release
TFO#04A                      DC    A(TFO#04)   * Init
*
                             DS    0F
MAK501D_TOK                  DC    C'MAK501D Created ITOK object '
                             DC    X'00'
                             DS    0F
MAK501D_TFO                  DC    C'MAK501D Created ITFO object '
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
         COPY  DSTOK             * ITOK obj DSECT
         COPY  DSTFO             * ITFO obj DSECT
         COPY  DSIFO             * IIFO obj DSECT
         COPY  DSPSS             * IDSS obj DSECT
         COPY  DSSTR             * ISTR obj DSECT
         COPY  DSSTB             * ISTB obj DSECT
*
         DROP
*
LWZMTOK  CSECT
*
* ITOK QueryInterface
*
TOK#01   MQRYIFCE SUF=T01,IFACE=ITOK
*
* ITOK AddRef
*
TOK#02   MADDREF
*
* ITOK Release
*
TOK#03   CEEENTRY AUTO=WORKDSAT03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAT03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    TOK#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING TOK_obj,R6
*
            IIND_Release OBJECT=IIN_in,WORK=WORKT03
            MVC   IIN_in,=A(0)
*
            IIFO_Release OBJECT=IIFO_ii,WORK=WORKT03
            MVC   IIFO_ii,=A(0)
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRT03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRT03,FCT03),MF=(E,WORKT03)
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
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKT03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKT03,      X
               ZSTR=MAK502D_TOK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKT03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKT03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
TOK#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_TOK                  DC    C'MAK502D Deleted ITOK object '
                             DC    X'00'
*
WORKDSAT03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRT03                    DS    A
WORKT03                      DS    3A
*
WORKDSAT03_SIZ               EQU   *-WORKDSAT03
*
LWZMTOK  CSECT
*
         DROP
*
* ITOK Init
*
TOK#04   CEEENTRY AUTO=WORKDSAT04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAT04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING TOK_obj,R8        * Address object DSECT
*
         MVC   IIN_in,4(R1)      * Parm 2 is generic input (IIN*)
*
         MVC   IIFO_ii,8(R1)     * Parm 3 is IIFO
*
         IIND_AddRef OBJECT=IIN_in,WORK=WORKT04
*
         IIFO_AddRef OBJECT=IIFO_ii,WORK=WORKT04
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKT04,LINE=MAK503D,           X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
TOK#04_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK503D                      DC    C'MAK503D Initialized ITOK object',XX
               '00'
*
WORKDSAT04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKT04                      DS    3A
*
WORKDSAT04_SIZ               EQU   *-WORKDSAT04
*
LWZMTOK  CSECT
*
         DROP
*
* ITOK GetNextToken
*
TOK#05   CEEENTRY AUTO=WORKDSAT05_SIZ,MAIN=NO,BASE=(R10,R11)
*
         USING WORKDSAT05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING TOK_obj,R8        * Address object DSECT
*
         MVC   IPSS_ps_T05,4(R1) * Parm 2 is ParserState
*
         L     R7,8(,R1)         * Parm 3 is return TokenizerInfo ptr
         ST    R7,ITFO_return_ptr_T05 * Save as local var
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKT05,LINE=MAK506D,           X
               LOGLEVEL=LOG_LEVEL_DEBUG2
*
         IF (CLC,0(4,R7),EQ,=A(0)) THEN
*           Instantiate a new ITFO object
            MINSTANT GUID=G_ITFO_GUID,WORK=WORKT05,OBJPTR=ITFO_ti_T05
*
            MVC   0(4,R7),ITFO_ti_T05
         ELSE
            MVC   ITFO_ti_T05,0(R7)
*
            ITFO_Init OBJECT=ITFO_ti_T05,WORK=WORKT05
         ENDIF
*
         L     R7,ITFO_ti_T05    * Point R7 to ti
         USING TFO_obj,R7        * Addressability for ti
*
         L     R6,IIFO_ii        * Point R7 to ii
         USING IFO_obj,R6        * Addressability for ii
*
         IF (CLI,IFO_bEOF,NE,C'Y') THEN
*
*           Use IIND's GetNextChar, method should be in same place
            IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,IIFO=IIFO_ii
*
            DO WHILE=(CLI,IFO_bEOF,NE,C'Y',AND,                        X
               CLI,IFO_cCurrChar,EQ,C' ')
               ASI   TFO_nSpaces,1
*
               IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,IIFO=IIFO_ii
            ENDDO
*
            IF (CLI,IFO_bEOF,NE,C'Y') THEN
               IF (CLI,IFO_bEOL,NE,C'Y') THEN
                  ISTB_AppendChar OBJECT=TFO_ISTB_token,WORK=WORKT05,  X
               CHAR=IFO_cCurrChar
*
                  IF (CLC,IFO_cCurrChar(2),EQ,=C'#*') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_COMMENT)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        DO WHILE=(CLI,IFO_bEOF,NE,C'Y',AND,            X
               CLI,IFO_cPeekChar,GT,X'00')
                           IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,X
               IIFO=IIFO_ii
*
                           ISTB_AppendChar OBJECT=TFO_ISTB_token,      X
               WORK=WORKT05,CHAR=IFO_cCurrChar
                        ENDDO
*
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'.') THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_UNDETERMIX
               NED)),OR,(CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_RECIPEX
               )) THEN
                        MVC   TFO_tokenType,=A(TOKEN_TYPE_SPECIAL)
                        MVC   TFO_nLine,IFO_nLine
                        MVC   TFO_pos,IFO_pos

                        IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,   X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                        IF (CLI,isExpected_T05,EQ,C'Y') THEN
                           DO WHILE=(CLI,IFO_bEOF,NE,C'Y',AND,         X
               CLI,IFO_cPeekChar,GT,X'00',AND,                         X
               CLI,IFO_cPeekChar,NE,C' ',AND,                          X
               CLI,IFO_cPeekChar,NE,C'+',AND,                          X
               CLC,IFO_cPeekChar(2),NE,=C'#*')
                              IIND_GetNextChar OBJECT=IIN_in,          X
               WORK=WORKT05,IIFO=IIFO_ii
*
                              LARL  R15,TRT_CHARACTER_CAT1
                              TRT   IFO_cCurrChar,0(R15)
*
                              IF (Z) THEN
                                 ISTB_AppendChar OBJECT=TFO_ISTB_token,X
               WORK=WORKT05,CHAR=IFO_cCurrChar
                              ELSE
                                 B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCX
               EPTION
                              ENDIF
                           ENDDO
*
                           B     TOK#05_TOKEN
                        ELSE
                           B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                        ENDIF
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'=') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_EQUALS)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C':'),AND,                  X
               (CLI,IFO_cPeekChar,EQ,C'=') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_IMMEDEQUALS)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        ISTB_AppendChar OBJECT=TFO_ISTB_token,         X
               WORK=WORKT05,CHAR=IFO_cCurrChar
*
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'?'),AND,                  X
               (CLI,IFO_cPeekChar,EQ,C'=') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_CONDEQUALS)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        ISTB_AppendChar OBJECT=TFO_ISTB_token,         X
               WORK=WORKT05,CHAR=IFO_cCurrChar
*
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C':') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_RULE)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'$'),ANDIF,                X
               (CLI,IFO_cPeekChar,EQ,C'('),OR,                         X
               (CLI,IFO_cPeekChar,EQ,C'{') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_VARIABLE)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        ISTB_AppendChar OBJECT=TFO_ISTB_token,         X
               WORK=WORKT05,CHAR=IFO_cCurrChar
*
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'$'),AND,                  X
               (CLI,IFO_cPeekChar,EQ,C'@') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_TARGETVAR)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        ISTB_AppendChar OBJECT=TFO_ISTB_token,         X
               WORK=WORKT05,CHAR=IFO_cCurrChar
*
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'$'),AND,                  X
               (CLI,IFO_cPeekChar,EQ,C'%') THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_MEMBERVAR)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        ISTB_AppendChar OBJECT=TFO_ISTB_token,         X
               WORK=WORKT05,CHAR=IFO_cCurrChar
*
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C')') THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_VARIABX
               LE2A)),OR,                                              X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST4A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION4A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_SHFUNCTION2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND2A)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND2A)),   x
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_STRIPEXT2A))
                        MVC   TFO_tokenType,=A(TOKEN_TYPE_CLOSEBRACKET)
                        MVC   TFO_nLine,IFO_nLine
                        MVC   TFO_pos,IFO_pos
*
                        B     TOK#05_TOKEN
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'}') THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_VARIABX
               LE2B)),OR,                                              X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST4B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2B)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION4B)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_SHFUNCTION2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND2B)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND2B)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_STRIPEXT2B))
                        MVC   TFO_tokenType,=A(TOKEN_TYPE_CLOSECURLY)
                        MVC   TFO_nLine,IFO_nLine
                        MVC   TFO_pos,IFO_pos
*
                        B     TOK#05_TOKEN
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C',') THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSX
               NAME1)),OR,                                             X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND1)),     X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND2A)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND2B)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND1)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND2A)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND2B)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2B))
                        MVC   TFO_tokenType,=A(TOKEN_TYPE_COMMA)
                        MVC   TFO_nLine,IFO_nLine
                        MVC   TFO_pos,IFO_pos
*
                        B     TOK#05_TOKEN
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'+') THEN
                     IF (CLI,IFO_cPeekChar,NE,C'+') THEN
                        L     R14,IPSS_ps_T05
                        IF (CLI,PSS_cPhase-PSS_obj(R14),NE,C'0') THEN
                           MVC   TFO_tokenType,=A(TOKEN_TYPE_CONTINUATIX
               ON)
                           MVC   TFO_nLine,IFO_nLine
                           MVC   TFO_pos,IFO_pos
*
                           IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                           IF (CLI,isExpected_T05,EQ,C'Y') THEN
                              B     TOK#05_TOKEN
                           ELSE
                              B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTX
               ION
                           ENDIF
                        ENDIF
                     ELSE
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        B     TOK#05_NORMAL
                     ENDIF
                  ENDIF
*
                  IF (CLI,IFO_cCurrChar,EQ,C'-'),ANDIF,                X
               (CLI,IFO_cPeekChar,EQ,C'v'),OR,                         X
               (CLI,IFO_cPeekChar,EQ,C'V'),OR,                         X
               (CLI,IFO_cPeekChar,EQ,C't'),OR,                         X
               (CLI,IFO_cPeekChar,EQ,C'T') THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PARAMEX
               TER)) THEN
                        IF (CLI,IFO_cPeekChar,EQ,C'v'),OR,             X
               (CLI,IFO_cPeekChar,EQ,C'V') THEN
                           MVC   TFO_tokenType,=A(TOKEN_TYPE_LOGSWITCH)
                        ELSE
                           MVC   TFO_tokenType,=A(TOKEN_TYPE_TARGETSWITX
               CH)
                        ENDIF
                        MVC   TFO_nLine,IFO_nLine
                        MVC   TFO_pos,IFO_pos
*
                        IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,   X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                        IF (CLI,isExpected_T05,EQ,C'Y') THEN
                           IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,X
               IIFO=IIFO_ii
*
                           ISTB_AppendChar OBJECT=TFO_ISTB_token,      X
               WORK=WORKT05,CHAR=IFO_cCurrChar
                           B     TOK#05_TOKEN
                        ELSE
                           B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                        ENDIF
                     ENDIF
                  ENDIF
*
                  L     R15,IPSS_ps_T05
                  IF (CLC,IFO_cCurrChar,EQ,PSS_cRecipePrefix-PSS_obj(R1X
               5)),AND,(CLC,IFO_pos,EQ,=A(1)) THEN
                     MVC   TFO_tokenType,=A(TOKEN_TYPE_RECIPEPREFIX)
                     MVC   TFO_nLine,IFO_nLine
                     MVC   TFO_pos,IFO_pos
*
                     IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,      X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                     IF (CLI,isExpected_T05,EQ,C'Y') THEN
                        B     TOK#05_TOKEN
                     ELSE
                        B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                     ENDIF
                  ENDIF
*
TOK#05_NORMAL     EQU   *
*
                  MVC   TFO_tokenType,=A(TOKEN_TYPE_NORMAL)
                  MVC   TFO_nLine,IFO_nLine
                  MVC   TFO_pos,IFO_pos
*
                  IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,         X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                  IF (CLI,isExpected_T05,EQ,C'Y') THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     DO WHILE=(CLI,IFO_bEOF,NE,C'Y',AND,               X
               CLI,IFO_cPeekChar,GT,X'00',AND,                         X
               CLI,IFO_cPeekChar,NE,C' ',AND,                          X
               CLI,IFO_cPeekChar,NE,C'+',AND,                          X
               CLI,IFO_cPeekChar,NE,C'$',AND,                          X
               CLC,IFO_cPeekChar,NE,=C'#*')
                        IF (CLI,IFO_cPeekChar,EQ,C')'),OR,             X
               (CLI,IFO_cPeekChar,EQ,C'}') THEN
                           IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_X
               VARIABLE1)),OR,                                         X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST1)), X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST3)), X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST4A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST4B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION1)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2B)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION3)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION4A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION4B)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND2A)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND2B)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND2A)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND2B)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_STRIPEXT2A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_STRIPEXT2B)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_SHFUNCTION1)), X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_SHFUNCTION2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_SHFUNCTION2B))
                              ASMLEAVE
                           ENDIF
                        ELSEIF (CLI,IFO_cPeekChar,EQ,C',') THEN
                           IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_X
               ADDPDSNAME1)),OR,                                       X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_APPEND1)),     X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_PREPEND1)),    X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST1)), X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2A)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_MEMBERLIST2B)),X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION1)),   X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2A)),  X
               OR,                                                     X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_FUNCTION2B))
                              ASMLEAVE
                           ENDIF
                        ELSEIF (CLI,IFO_cPeekChar,EQ,C':') THEN
                           IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_X
               RULE1)),OR,                                             X
               (CLC,lastState_T05,EQ,=A(PARSER_STATE_UNDETERMINED))
                              ASMLEAVE
                           ENDIF
                        ENDIF
*
                        IIND_GetNextChar OBJECT=IIN_in,WORK=WORKT05,   X
               IIFO=IIFO_ii
*
                        ISTB_AppendChar OBJECT=TFO_ISTB_token,         X
               WORK=WORKT05,CHAR=IFO_cCurrChar
                     ENDDO
*
                     B     TOK#05_TOKEN
                  ELSE
                     B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                  ENDIF
*
TOK#05_TOKEN      EQU   *
*
                  IF (CLC,TFO_tokenType,EQ,=A(TOKEN_TYPE_NORMAL)) THEN
                     IPSS_GetLastState OBJECT=IPSS_ps_T05,WORK=WORKT05,X
               STATE_OUT=lastState_T05
*
                     IF (CLC,lastState_T05,EQ,=A(PARSER_STATE_IN_RECIPEX
               )) THEN
                        MVC   ISTR_upper_T05,=A(0)
*
                        ISTB_ToUpperCase OBJECT=TFO_ISTB_token,        X
               WORK=WORKT05,ISTRPTR=ISTR_upper_T05
*
                        ISTR_EqualsZStr OBJECT=ISTR_upper_T05,         X
               WORK=WORKT05,ZSTR=CALL_T05
*
                        IF (C,R15,EQ,=A(1)) THEN
                           MVC   TFO_tokenType,=A(TOKEN_TYPE_CALL)
*
                           IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                           IF (CLI,isExpected_T05,NE,C'Y') THEN
                              B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTX
               ION
                           ENDIF
                        ELSE
                           ISTR_EqualsZStr OBJECT=ISTR_upper_T05,      X
               WORK=WORKT05,ZSTR=SH_T05
*
                           IF (C,R15,EQ,=A(1)) THEN
                              MVC   TFO_tokenType,=A(TOKEN_TYPE_SH)
*
                              IPSS_IsExpectedTokenType                 X
               OBJECT=IPSS_ps_T05,WORK=WORKT05,ITFO=ITFO_ti_T05,       X
               RESULT=isExpected_T05
*
                              IF (CLI,isExpected_T05,NE,C'Y') THEN
                                 B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCX
               EPTION
                              ENDIF
                           ENDIF
                        ENDIF
*
                        ISTR_Release OBJECT=ISTR_upper_T05,WORK=WORKT05
                     ENDIF
                  ENDIF
*
               ELSE * IF (CLI,IFO_bEOL,NE,C'Y')
                  MVI   TFO_bEOL,C'Y'
*
                  MVC   TFO_tokenType,=A(TOKEN_TYPE_EOL)
                  MVC   TFO_nLine,IFO_nLine
                  MVC   TFO_pos,IFO_pos
*
                  IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,         X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
                  IF (CLI,isExpected_T05,NE,C'Y') THEN
                     B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
                  ENDIF
               ENDIF
            ELSE * IF (CLI,IFO_bEOF,NE,C'Y')
               MVI   TFO_bEOF,C'Y'
*
               MVC   TFO_tokenType,=A(TOKEN_TYPE_EOF)
               MVC   TFO_nLine,IFO_nLine
               MVC   TFO_pos,IFO_pos
*
               IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,            X
               WORK=WORKT05,ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
               IF (CLI,isExpected_T05,NE,C'Y') THEN
                  B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
               ENDIF
            ENDIF
         ELSE * IF (CLI,IFO_bEOF,NE,C'Y')
            MVI   TFO_bEOF,C'Y'
*
            MVC   TFO_tokenType,=A(TOKEN_TYPE_EOF)
            MVC   TFO_nLine,IFO_nLine
            MVC   TFO_pos,IFO_pos
*
            IPSS_IsExpectedTokenType OBJECT=IPSS_ps_T05,WORK=WORKT05,  X
               ITFO=ITFO_ti_T05,RESULT=isExpected_T05
*
            IF (CLI,isExpected_T05,NE,C'Y') THEN
               B     TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION
            ENDIF
         ENDIF
*
         L     R15,G_ILOG
         IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG) THEN
            LA    R1,WORKT05
            MVC   0(4,R1),ITFO_ti_T05
            L     R15,TOK#98A_T05
            BASR  R14,R15
         ENDIF
*
         L     R15,ITFO_return_ptr_T05
         MVC   0(4,R15),ITFO_ti_T05
*
TOK#05_RET EQU   *
         CEETERM
*
* Jump here to log unexpected token and return
TOK#05_UNEXPECTED_TOKEN_TYPE_EXCEPTION EQU   *
         LA    R1,WORKT05
         MVC   0(4,R1),ITFO_ti_T05
         L     R15,TOK#99A_T05
         BASR  R14,R15
*
         B     TOK#05_RET
*
         LTORG
*
                             DS    0F
MAK506D                      DC    C'MAK506D Getting next token',X'00'
*
                             DS    0F
CALL_T05                     DC    C'CALL',X'00'
                             DS    0F
SH_T05                       DC    C'SH',X'00'
*
                             DS    0F
TOK#98A_T05                  DC    A(TOK#98)
TOK#99A_T05                  DC    A(TOK#99)
*
WORKDSAT05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKT05                      DS    3A
*
IPSS_ps_T05                  DS    A
ITFO_return_ptr_T05          DS    A
ITFO_ti_T05                  DS    A
ISTR_upper_T05               DS    A
*
lastState_T05                DS    F
isExpected_T05               DS    C
*
WORKDSAT05_SIZ               EQU   *-WORKDSAT05
*
LWZMTOK  CSECT
*
         DROP
*
* ITOK WriteDebugLineNextToken
*
TOK#98   CEEENTRY AUTO=WORKDSAT98_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAT98,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is ITFO object
         USING TFO_obj,R8        * Addressability of ITFO
*
         XR    R0,R0
         LA    R6,LINET98
         LA    R7,LIT01#98
         MVST  R6,R7
         BC    1,*-4
*
         SELECT CLC,TFO_tokenType,EQ
         WHEN =A(TOKEN_TYPE_EOF)
            LARL  R7,TOKEN_NAME_EOF
         WHEN =A(TOKEN_TYPE_EOL)
            LARL  R7,TOKEN_NAME_EOL
         WHEN =A(TOKEN_TYPE_COMMENT)
            LARL  R7,TOKEN_NAME_COMMENT
         WHEN =A(TOKEN_TYPE_SPECIAL)
            LARL  R7,TOKEN_NAME_SPECIAL
         WHEN =A(TOKEN_TYPE_EQUALS)
            LARL  R7,TOKEN_NAME_EQUALS
         WHEN =A(TOKEN_TYPE_IMMEDEQUALS)
            LARL  R7,TOKEN_NAME_IMMEDEQUALS
         WHEN =A(TOKEN_TYPE_CONDEQUALS)
            LARL  R7,TOKEN_NAME_CONDEQUALS
         WHEN =A(TOKEN_TYPE_RULE)
            LARL  R7,TOKEN_NAME_RULE
         WHEN =A(TOKEN_TYPE_CONTINUATION)
            LARL  R7,TOKEN_NAME_CONTINUATION
         WHEN =A(TOKEN_TYPE_VARIABLE)
            LARL  R7,TOKEN_NAME_VARIABLE
         WHEN =A(TOKEN_TYPE_TARGETVAR)
            LARL  R7,TOKEN_NAME_TARGETVAR
         WHEN =A(TOKEN_TYPE_MEMBERVAR)
            LARL  R7,TOKEN_NAME_MEMBERVAR
         WHEN =A(TOKEN_TYPE_CLOSEBRACKET)
            LARL  R7,TOKEN_NAME_CLOSEBRACKET
         WHEN =A(TOKEN_TYPE_CLOSECURLY)
            LARL  R7,TOKEN_NAME_CLOSECURLY
         WHEN =A(TOKEN_TYPE_RECIPEPREFIX)
            LARL  R7,TOKEN_NAME_RECIPEPREFIX
         WHEN =A(TOKEN_TYPE_CALL)
            LARL  R7,TOKEN_NAME_CALL
         WHEN =A(TOKEN_TYPE_SH)
            LARL  R7,TOKEN_NAME_SH
         WHEN =A(TOKEN_TYPE_COMMA)
            LARL  R7,TOKEN_NAME_COMMA
         WHEN =A(TOKEN_TYPE_LOGSWITCH)
            LARL  R7,TOKEN_NAME_LOGSWITCH
         WHEN =A(TOKEN_TYPE_TARGETSWITCH)
            LARL  R7,TOKEN_NAME_TARGETSWITCH
         OTHRWISE
            B     TOK#98_NORMAL
         ENDSEL
*
         MVST  R6,R7
         BC    1,*-4
*
         B     TOK#98_WRITE
*
TOK#98_NORMAL EQU   *
*
         L     R7,TFO_ISTB_token
         LA    R1,LINET98+80
         SR    R1,R6
         IF (C,R1,GE,STB_nStrLen-STB_obj(R7)) THEN
            L     R7,STB_lpBuf-STB_obj(,R7)
            MVST  R6,R7
            BC    1,*-4
         ELSE
         ENDIF
*
TOK#98_WRITE EQU   *
         MVI   0(R6),X'00'
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKT98,LINE=LINET98,           X
               LOGLEVEL=LOG_LEVEL_DEBUG
*
TOK#98_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
LIT01#98                     DC    C'MAK411D Got next token ',X'00'
*
WORKDSAT98                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKT98                      DS    3A
*
LINET98                      DS    CL80
                             DS    C
*
WORKDSAT98_SIZ               EQU   *-WORKDSAT98
*
LWZMTOK  CSECT
*
         DROP
*
* ITOK UnexpectedTokenTypeException
*
TOK#99   CEEENTRY AUTO=WORKDSAT99_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAT99,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is ITFO object
         USING TFO_obj,R8        * Addressability of ITFO
*
         XR    R0,R0
         LA    R6,LINET99
         LA    R7,LIT01#99
         MVST  R6,R7
         BC    1,*-4
*
         SELECT CLC,TFO_tokenType,EQ
         WHEN =A(TOKEN_TYPE_EOF)
            LARL  R7,TOKEN_NAME_EOF
         WHEN =A(TOKEN_TYPE_EOL)
            LARL  R7,TOKEN_NAME_EOL
         WHEN =A(TOKEN_TYPE_COMMENT)
            LARL  R7,TOKEN_NAME_COMMENT
         WHEN =A(TOKEN_TYPE_SPECIAL)
            LARL  R7,TOKEN_NAME_SPECIAL
         WHEN =A(TOKEN_TYPE_EQUALS)
            LARL  R7,TOKEN_NAME_EQUALS
         WHEN =A(TOKEN_TYPE_IMMEDEQUALS)
            LARL  R7,TOKEN_NAME_IMMEDEQUALS
         WHEN =A(TOKEN_TYPE_RULE)
            LARL  R7,TOKEN_NAME_RULE
         WHEN =A(TOKEN_TYPE_CONTINUATION)
            LARL  R7,TOKEN_NAME_CONTINUATION
         WHEN =A(TOKEN_TYPE_VARIABLE)
            LARL  R7,TOKEN_NAME_VARIABLE
         WHEN =A(TOKEN_TYPE_TARGETVAR)
            LARL  R7,TOKEN_NAME_TARGETVAR
         WHEN =A(TOKEN_TYPE_MEMBERVAR)
            LARL  R7,TOKEN_NAME_MEMBERVAR
         WHEN =A(TOKEN_TYPE_CLOSEBRACKET)
            LARL  R7,TOKEN_NAME_CLOSEBRACKET
         WHEN =A(TOKEN_TYPE_CLOSECURLY)
            LARL  R7,TOKEN_NAME_CLOSECURLY
         WHEN =A(TOKEN_TYPE_RECIPEPREFIX)
            LARL  R7,TOKEN_NAME_RECIPEPREFIX
         WHEN =A(TOKEN_TYPE_CALL)
            LARL  R7,TOKEN_NAME_CALL
         WHEN =A(TOKEN_TYPE_COMMA)
            LARL  R7,TOKEN_NAME_COMMA
         OTHRWISE
            LARL  R7,TOKEN_NAME_NORMAL
         ENDSEL
*
         MVST  R6,R7
         BC    1,*-4
*
         LA    R7,LIT02#99
         MVST  R6,R7
         BC    1,*-4
*
         L     R15,TFO_nLine
         CVD   R15,G_DEC8
         UNPK  G_ZONED8,G_DEC8
         OI    G_ZONED8+7,X'F0'
         MVI   G_ZONED8+8,X'00'
         L     R14,G_TRT_ONLY_ZEROS
         TRT   G_ZONED8(7),0(R14)
         BC    7,*+8
         LA    R1,G_ZONED8+7
         LR    R7,R1
         MVST  R6,R7
         BC    1,*-4
*
         LA    R7,LIT03#99
         MVST  R6,R7
         BC    1,*-4
*
         L     R15,TFO_pos
         CVD   R15,G_DEC8
         UNPK  G_ZONED8,G_DEC8
         OI    G_ZONED8+7,X'F0'
         MVI   G_ZONED8+8,X'00'
         L     R14,G_TRT_ONLY_ZEROS
         TRT   G_ZONED8(7),0(R14)
         BC    7,*+8
         LA    R1,G_ZONED8+7
         LR    R7,R1
         MVST  R6,R7
         BC    1,*-4
*
TOK#99_WRITE EQU   *
         MVI   0(R6),X'00'
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORKT99,LINE=LINET99,           X
               LOGLEVEL=LOG_LEVEL_ERROR
*
         MVC   G_RETCODE,=A(8)
*
TOK#99_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
LIT01#99                     DC    C'MAK103E Unexpected ',X'00'
                             DS    0F
LIT02#99                     DC    C' at line ',X'00'
                             DS    0F
LIT03#99                     DC    C' pos ',X'00'
*
WORKDSAT99                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKT99                      DS    3A
*
LINET99                      DS    CL80
                             DS    C
*
WORKDSAT99_SIZ               EQU   *-WORKDSAT99
*
LWZMTOK  CSECT
*
         DROP
*
*
* ITFO QueryInterface
*
TFO#01   MQRYIFCE SUF=F01,IFACE=ITFO
*
* ITFO AddRef
*
TFO#02   MADDREF
*
* ITFO Release
*
TFO#03   CEEENTRY AUTO=WORKDSAF03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAF03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    TFO#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING TFO_obj,R6
*
            IF (CLC,TFO_ISTB_token,NE,=A(0)) THEN
               ISTB_Release OBJECT=TFO_ISTB_token,WORK=WORKF03
               MVC   TFO_ISTB_token,=A(0)
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
               ZSTR=MAK502D_TFO
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
TFO#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK502D_TFO                  DC    C'MAK502D Deleted ITFO object '
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
LWZMTOK  CSECT
*
         DROP
*
* ITFO Init
*
TFO#04   CEEENTRY AUTO=WORKDSAF04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAF04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING TFO_obj,R8        * Address object DSECT
*
         MVI   TFO_bEOF,C'N'
         MVI   TFO_bEOL,C'N'
         IF (CLC,TFO_ISTB_token,EQ,=A(0)) THEN
            MINSTANT GUID=G_ISTB_GUID,WORK=WORKF04,                    X
               OBJPTR=TFO_ISTB_token
         ELSE
            ISTB_Init OBJECT=TFO_ISTB_token,WORK=WORKF04
         ENDIF
         MVC   TFO_tokenType,=A(TOKEN_TYPE_UNDETERMINED)
         MVC   TFO_nLine,=A(0)
         MVC   TFO_pos,=A(0)
         MVC   TFO_nSpaces,=A(0)
*
TFO#04_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAF04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKF04                      DS    A
*
WORKDSAF04_SIZ               EQU   *-WORKDSAF04
*
LWZMTOK  CSECT
*
         DROP
*
* Translate table for ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_
*
TRT_CHARACTER_CAT1 DS    0AD
*                           0 1 2 3 4 5 6 7 8 9 A B C D E F
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 5
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 7
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A
                   DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B
                   DC    X'FF000000000000000000FFFFFFFFFFFF' C
                   DC    X'FF000000000000000000FFFFFFFFFFFF' D
                   DC    X'FFFF0000000000000000FFFFFFFFFFFF' E
                   DC    X'00000000000000000000FFFFFFFFFFFF' F
*
                             DS    0F
TOKEN_NAME_EOF               DC    C'end of file',X'00'
                             DS    0F
TOKEN_NAME_EOL               DC    C'end of line',X'00'
                             DS    0F
TOKEN_NAME_COMMENT           DC    C'comment',X'00'
                             DS    0F
TOKEN_NAME_SPECIAL           DC    C'special variable',X'00'
                             DS    0F
TOKEN_NAME_EQUALS            DC    C'=',X'00'
                             DS    0F
TOKEN_NAME_IMMEDEQUALS       DC    C':=',X'00'
                             DS    0F
TOKEN_NAME_CONDEQUALS        DC    C'?=',X'00'
                             DS    0F
TOKEN_NAME_RULE              DC    C':',X'00'
                             DS    0F
TOKEN_NAME_CONTINUATION      DC    C'+',X'00'
                             DS    0F
TOKEN_NAME_VARIABLE          DC    C'variable',X'00'
                             DS    0F
TOKEN_NAME_TARGETVAR         DC    C'target variable',X'00'
                             DS    0F
TOKEN_NAME_MEMBERVAR         DC    C'member variable',X'00'
                             DS    0F
TOKEN_NAME_CLOSEBRACKET      DC    C')',X'00'
                             DS    0F
TOKEN_NAME_CLOSECURLY        DC    C'}',X'00'
                             DS    0F
TOKEN_NAME_RECIPEPREFIX      DC    C'recipeprefix',X'00'
                             DS    0F
TOKEN_NAME_CALL              DC    C'CALL',X'00'
                             DS    0F
TOKEN_NAME_SH                DC    C'SH',X'00'
                             DS    0F
TOKEN_NAME_COMMA             DC    C',',X'00'
                             DS    0F
TOKEN_NAME_LOGSWITCH         DC    C'-v',X'00'
                             DS    0F
TOKEN_NAME_TARGETSWITCH      DC    C'-t',X'00'
                             DS    0F
TOKEN_NAME_NORMAL            DC    C'token',X'00'
*
         COPY  REGS              * Register equates
*
         END   LWZMTOK
