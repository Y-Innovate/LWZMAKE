*---------------------------------------------------------------------*
* Program    : LWZMUSS                                                *
* Description: COM object for executing USS commands                  *
*---------------------------------------------------------------------*
         TITLE 'LWZMUSS'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates a new USS object
*
LWZMUSS  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
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
*        Was a new USS object requested?
         IF (CLC,0(16,R6),EQ,G_IUSS_GUID) THEN
            MNEWOBJ OBJTYPE=USS,WORK=WORK * Alloc new object
*
*           Init obj attributes
            LR    R7,R14
            USING USS_obj,R7
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
               ZSTR=MAK501D_USS
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
LWZMUSS_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
USS#01A                      DC    A(USS#01)   * QueryInterface
USS#02A                      DC    A(USS#02)   * AddRef
USS#03A                      DC    A(USS#03)   * Release
USS#04A                      DC    A(USS#04)   * BPX1STA
USS#05A                      DC    A(USS#05)   * Run command
USS#06A                      DC    A(USS#06)   * BPX1OPN
USS#07A                      DC    A(USS#07)   * BPX1CLO
USS#08A                      DC    A(USS#08)   * BPX1RED
*
                             DS    0F
MAK501D_USS                  DC    C'MAK501D Created IUSS object '
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
         COPY  DSUSS             * IUSS obj DSECT
         COPY  DSFFO             * IFFO obj DSECT
         COPY  DSSTR             * ISTR obj DSECT
         COPY  DSSTB             * ISTB obj DSECT
*
         IHAPSA   DSECT=YES
*
         CVT      DSECT=YES
*
         BPXYCONS
*
         BPXYOPNF
*
         BPXYFTYP
*
         BPXYFCTL
*
         DROP
*
LWZMUSS  CSECT
*
* IUSS QueryInterface
*
USS#01   MQRYIFCE SUF=U01,IFACE=IUSS
*
* IUSS AddRef
*
USS#02   MADDREF
*
* IUSS Release
*
USS#03   MRELEASE SUF=U03,OBJ=USS
*
LWZMUSS  CSECT
*
         DROP
*
* IUSS BPX1STA to get USS file last modified date
*
USS#04   CEEENTRY AUTO=WORKDSAU04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAU04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING USS_obj,R8        * Address object DSECT
*
         L     R7,4(,R1)         * Parm 2 is IFFO object
         ST    R7,IFFO_U04       * Save in local var
         USING FFO_obj,R7        * Address FFO object
*
         LT    R15,G_BPX1STAA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1STA,SF=(E,G_LOADD) * entry point BPX1STA
*
            ST    R0,G_BPX1STAA * and store in global var
         ENDIF
*
         MVC   USS_PATH_LEN,FFO_fnameLen
         MVC   USS_PATH_PTR,FFO_fname
         MVC   USS_BPX1STA_STAT_LEN,=A(ST#LEN)
         LA    R1,USS_BPX1STA_STATAREA
         ST    R1,USS_BPX1STA_STAT_PTR
*
         LA    R1,USS_PAR15A
         LA    R15,USS_PATH_LEN
         ST    R15,0(,R1)
         MVC   4(4,R1),USS_PATH_PTR
         LA    R15,USS_BPX1STA_STAT_LEN
         ST    R15,8(,R1)
         MVC   12(4,R1),USS_BPX1STA_STAT_PTR
         LA    R15,USS_RETVAL
         ST    R15,16(,R1)
         LA    R15,USS_RETCODE
         ST    R15,20(,R1)
         LA    R15,USS_REASON
         ST    R15,24(,R1)
         OI    24(R1),X'80'
*
         L     R15,G_BPX1STAA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'0') THEN
            MVI   FFO_exists,C'Y'
*
            L     R15,USS_BPX1STA_STATAREA+(ST_MTIME-STAT)
*
            USING PSA,0
*
HFSFMTTM    M     R14,=F'1000000'     Convert to microseconds
            LA    R0,1                Get one
            AL    R15,SEC70YRS+4      Change origin from 1970 to 1900
            BC    12,HFSFMTL1
            ALR   R14,R0              Carry one from overflow
HFSFMTL1    AL    R14,SEC70YRS
            SLDL  R14,12              Convert to STCK format
            L     R1,FLCCVT           CVT
            L     R1,CVTEXT2-CVT(,R1) OS/VS2 common extension
            USING CVTXTNT2,R1
            AL    R15,CVTLDTOR        Add CVTLDTO right word
            BC    12,HFSFMTL2         CVTLDTO = Local Date/Time Offset
            ALR   R14,R0              Carry one from overflow
HFSFMTL2    AL    R14,CVTLDTOL        Add CVTLDTO left word
            SL    R15,CVTLSOL         Subtract CVTLSO low word
            BC    3,HFSFMTL3          CVTLSO = Leap Second Offset
            SR    R14,R0              Borrow one from overflow
HFSFMTL3    SL    R14,CVTLSOH         Subtract CVTLSO high word
            DROP  R1                  CVTXTNT2
            STM   R14,R15,G_CONVTOD_OUTAREA Save the STCK value
            MVI   G_STCKCONV_OUTAREA,X'00'
            MVC   G_STCKCONV_OUTAREA+1(15),G_STCKCONV_OUTAREA
            MVC   G_STCKCONVD,G_STCKCONVL
            STCKCONV STCKVAL=G_CONVTOD_OUTAREA, Point to input STCK val+
               CONVVAL=G_STCKCONV_OUTAREA, Point to output four words  +
               TIMETYPE=DEC,         Get time decimal digits (default) +
               DATETYPE=YYYYMMDD,    Specify date format               +
               MF=(E,G_STCKCONVD)    Specify parameter list
            MVC   G_DATEWORK_DEC_1(4),G_STCKCONV_OUTAREA+8
            MVC   G_DATEWORK_DEC_1+4(3),G_STCKCONV_OUTAREA
            MVO   G_DATEWORK_DEC_2,G_DATEWORK_DEC_1(7)
            MVN   G_DATEWORK_DEC_2+7(1),=X'0F'
            UNPK  G_DATEWORK_ZON,G_DATEWORK_DEC_2
            MVC   FFO_alterDate(14),G_DATEWORK_ZON+2
            MVI   FFO_alterDate+14,X'00'
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG) THEN
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU04
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR=MAK415D_U04
               L     R2,FFO_fname
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR=0(,R2)
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR==X'4089A24000' * ' is '
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR=FFO_alterDate
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKU04,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG
            ENDIF
         ELSE
            IF (CLC,USS_RETCODE,NE,=F'129') THEN
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU04
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR=MAK119E_U04
*
               MVC   G_DEC8(4),USS_RETCODE
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR=G_ZONED8
*
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR==X'4000'
*
               MVC   G_DEC8(4),USS_REASON
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU04,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKU04,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_ERROR
*
               MVC   G_RETCODE,=A(12)
               B     USS#04_RET
            ENDIF
         ENDIF
*
USS#04_RET EQU   *
         CEETERM
*
         LTORG
*
*        MICROSECONDS FROM THE START OF 1900 TO THE START OF 1970
*        =  ((70*365)+17)*24*3600*1000000  =  2208988800 * 1000000
SEC70YRS                     DC    0D'0',FL8'2208988800E6'
*
                             DS    0F
MAK119E_U04                  DC    C'MAK119E BPX1STA error ',X'00'
                             DS    0F
MAK415D_U04                  DC    C'MAK415D Last modified date of ',X'X
               00'
*
WORKDSAU04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKU04                      DS    4A
*
IFFO_U04                     DS    A
*
WORKDSAU04_SIZ               EQU   *-WORKDSAU04
*
         BPXYSTAT
*
LWZMUSS  CSECT
*
         DROP
*
* IUSS Execute USS command
*
USS#05   CEEENTRY AUTO=WORKDSAU05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAU05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING USS_obj,R8        * Address object DSECT
*
         MVC   ISTB_sh_U05,4(R1) * Parm 2 is USS sh ISTB
*
         L     R2,8(,R1)         * Parm 3 is ISTR return ptr
         ST    R2,ISTBPTR_retval_U05 * Save in local var
*
         IF (CLC,0(4,R2),EQ,=A(0)) THEN
            MINSTANT GUID=G_ISTB_GUID,WORK=WORKU05,OBJPTR=0(,R2)
         ELSE
            ISTB_Init OBJECT=0(,R2),WORK=WORKU05
         ENDIF
*
         L     R15,G_ILOG
         IF (CLI,13(R15),GE,LOG_LEVEL_INFO) THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=MAK316I_U05
*
            L     R7,ISTB_sh_U05
            L     R7,STB_lpBuf-STB_obj(,R7)
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=0(,R7)
*
            L     R7,G_ISTB_tmp
            L     R7,STB_lpBuf-STB_obj(,R7)
*
            ILOG_Write OBJECT=G_ILOG,WORK=WORKU05,LINE=0(,R7),         X
               LOGLEVEL=LOG_LEVEL_INFO
         ENDIF
*
         LT    R15,G_BPX1PIPA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1PIP,SF=(E,G_LOADD) * entry point BPX1PIP
*
            ST    R0,G_BPX1PIPA * and store in global var
         ENDIF
*
         LT    R15,G_BPX1SPNA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1SPN,SF=(E,G_LOADD) * entry point BPX1SPN
*
            ST    R0,G_BPX1SPNA * and store in global var
         ENDIF
*
         LT    R15,G_BPX1OPNA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1OPN,SF=(E,G_LOADD) * entry point BPX1OPN
*
            ST    R0,G_BPX1OPNA * and store in global var
         ENDIF
*
         LT    R15,G_BPX1CLOA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1CLO,SF=(E,G_LOADD) * entry point BPX1CLO
*
            ST    R0,G_BPX1CLOA * and store in global var
         ENDIF
*
         LT    R15,G_BPX1REDA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1RED,SF=(E,G_LOADD) * entry point BPX1RED
*
            ST    R0,G_BPX1REDA * and store in global var
         ENDIF
*
         LT    R15,G_BPX1WATA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1WAT,SF=(E,G_LOADD) * entry point BPX1WAT
*
            ST    R0,G_BPX1WATA * and store in global var
         ENDIF
*
*        Open /dev/null for stdin
         LA    R1,USS_PAR15A
         LA    R15,=A(9)
         ST    R15,0(,R1)
         LA    R15,=C'/dev/null'
         ST    R15,4(,R1)
         LA    R15,=X'02000082'
         ST    R15,8(,R1)
         LA    R15,=A(0)
         ST    R15,12(,R1)
         LA    R15,USS_RETVAL
         ST    R15,16(,R1)
         LA    R15,USS_RETCODE
         ST    R15,20(,R1)
         LA    R15,USS_REASON
         ST    R15,24(,R1)
         OI    24(R1),X'80'
*
         L     R15,G_BPX1OPNA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1OPN error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
         MVC   USS_FD,USS_RETVAL
*
*        Create pipe for stdout
         LA    R1,USS_PAR15A
         LA    R15,USS_BPX1PIP_FDREAD1
         ST    R15,0(,R1)
         LA    R15,USS_BPX1PIP_FDWRITE1
         ST    R15,4(,R1)
         LA    R15,USS_RETVAL
         ST    R15,8(,R1)
         LA    R15,USS_RETCODE
         ST    R15,12(,R1)
         LA    R15,USS_REASON
         ST    R15,16(,R1)
         OI    16(R1),X'80'
*
         L     R15,G_BPX1PIPA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1PIP error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
*        Spawn /bin/sh with commandline
         MVC   USS_PATH_LEN,=A(L'EXECSTR_U05)
*
         MVC   USS_BPX1SPN_ARGCNT,=F'3'
*
         L     R5,ISTB_sh_U05
*
         LA    R15,ARG0LEN_U05
         ST    R15,USS_BPX1SPN_ARGLLST
         LA    R15,ARG1LEN_U05
         ST    R15,USS_BPX1SPN_ARGLLST+4
*        LA    R15,ARG2LEN_U05
*        ST    R15,USS_BPX1SPN_ARGLLST+8
         LA    R15,STB_nStrLen-STB_obj(,R5)
         ST    R15,USS_BPX1SPN_ARGLLST+8
         LA    R15,ARG0STR_U05
         ST    R15,USS_BPX1SPN_ARGSLST
         LA    R15,ARG1STR_U05
         ST    R15,USS_BPX1SPN_ARGSLST+4
*        LA    R15,ARG2STR_U05
*        ST    R15,USS_BPX1SPN_ARGSLST+8
         MVC   USS_BPX1SPN_ARGSLST+8(4),STB_lpBuf-STB_obj(R5)
*
         MVC   USS_BPX1SPN_ENVCNT,=A(1)
*
*        LA    R15,ENV0LEN_U05
*        ST    R15,USS_BPX1SPN_ENVLENS
         LA    R15,G_HOMELEN
         ST    R15,USS_BPX1SPN_ENVLENS
*        LA    R15,ENV0STR_U05
*        ST    R15,USS_BPX1SPN_ENVPARMS
         MVC   USS_BPX1SPN_ENVPARMS(4),G_HOME
*
         MVC   USS_BPX1SPN_FDCNT,=A(3)
         MVC   USS_BPX1SPN_FDLST(4),USS_FD
         MVC   USS_BPX1SPN_FDLST+4(4),USS_BPX1PIP_FDWRITE1
         MVC   USS_BPX1SPN_FDLST+8(4),USS_BPX1PIP_FDWRITE1
*
         MVI   USS_INHE,X'00'
         MVC   USS_INHE+1(INHE#LENGTH-1),USS_INHE
         LA    R15,USS_INHE
         MVC   INHEEYE-INHE(4,R15),=C'INHE'
         MVC   INHELENGTH-INHE(2,R15),=AL2(INHE#LENGTH)
         MVC   INHEVERSION-INHE(2,R15),=AL2(INHE#VER)
         OI    INHEFLAGS1-INHE(R15),INHEMUSTBELOCAL
*
         LA    R1,USS_PAR15A
         LA    R14,EXECLEN_U05
         ST    R14,0(,R1)
         LA    R14,EXECSTR_U05
         ST    R14,4(,R1)
         LA    R14,USS_BPX1SPN_ARGCNT
         ST    R14,8(,R1)
         LA    R14,USS_BPX1SPN_ARGLLST
         ST    R14,12(,R1)
         LA    R14,USS_BPX1SPN_ARGSLST
         ST    R14,16(,R1)
         LA    R14,USS_BPX1SPN_ENVCNT
         ST    R14,20(,R1)
         LA    R14,USS_BPX1SPN_ENVLENS
         ST    R14,24(,R1)
         LA    R14,USS_BPX1SPN_ENVPARMS
         ST    R14,28(,R1)
         LA    R14,USS_BPX1SPN_FDCNT
         ST    R14,32(,R1)
         LA    R14,USS_BPX1SPN_FDLST
         ST    R14,36(,R1)
         LA    R14,=A(INHE#LENGTH)
         ST    R14,40(,R1)
         LA    R14,USS_INHE
         ST    R14,44(,R1)
         LA    R15,USS_RETVAL
         ST    R15,48(,R1)
         LA    R15,USS_RETCODE
         ST    R15,52(,R1)
         LA    R15,USS_REASON
         ST    R15,56(,R1)
         OI    56(R1),X'80'
*
         L     R15,G_BPX1SPNA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1SPN error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
         MVC   processId_U05,USS_RETVAL
*
         LA    R1,USS_PAR15A
         LA    R15,USS_BPX1PIP_FDWRITE1
         ST    R15,0(,R1)
         LA    R15,USS_RETVAL
         ST    R15,4(,R1)
         LA    R15,USS_RETCODE
         ST    R15,8(,R1)
         LA    R15,USS_REASON
         ST    R15,12(,R1)
         OI    12(R1),X'80'
*
         L     R15,G_BPX1CLOA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1CLO error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
         L     R5,ISTBPTR_retval_U05
*
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
*
         MVI   newLine_U05,C'N'
*
USS#05_RED EQU   *
         LA    R1,USS_PAR15A
         LA    R15,USS_BPX1PIP_FDREAD1
         ST    R15,0(,R1)
         LA    R15,USS_BUFFER
         ST    R15,USS_BUFFERA
         LA    R15,USS_BUFFERA
         ST    R15,4(,R1)
         MVC   USS_ALET,=A(0)
         LA    R15,USS_ALET
         ST    R15,8(,R1)
         MVC   USS_COUNT,=A(1)
         LA    R15,USS_COUNT
         ST    R15,12(,R1)
         LA    R15,USS_RETVAL
         ST    R15,16(,R1)
         LA    R15,USS_RETCODE
         ST    R15,20(,R1)
         LA    R15,USS_REASON
         ST    R15,24(,R1)
         OI    24(R1),X'80'
*
         L     R15,G_BPX1REDA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1RED error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
         IF (CLC,USS_RETVAL,NE,=A(0)) THEN
            IF (CLI,USS_BUFFER,NE,X'15') THEN
               IF (CLI,newLine_U05,EQ,C'Y') THEN
                  ISTB_AppendChar OBJECT=0(,R5),WORK=WORKU05,          X
               CHAR==X'40'
                  MVI   newLine_U05,C'N'
               ENDIF
               ISTB_AppendChar OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               CHAR=USS_BUFFER
               ISTB_AppendChar OBJECT=0(,R5),WORK=WORKU05,             X
               CHAR=USS_BUFFER
            ELSE
               L     R2,G_ISTB_tmp
               L     R3,STB_lpBuf-STB_obj(,R2)
               S     R3,=A(2)
               MVC   0(2,R3),STB_nStrLen-STB_obj+2(R2)
               CALL  CEEMOUT,(0(,R3),=A(2),FCU05),VL,MF=(E,WORKU05)
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
*
               MVI   newLine_U05,C'Y'
            ENDIF
*
            B     USS#05_RED
         ENDIF
*
         L     R2,G_ISTB_tmp
         IF (CLC,STB_nStrLen-STB_obj(4,R2),GT,=A(0)) THEN
            L     R3,STB_lpBuf-STB_obj(,R2)
            S     R3,=A(2)
            MVC   0(2,R3),STB_nStrLen-STB_obj+2(R2)
            CALL  CEEMOUT,(0(,R3),=A(2),FCU05),VL,MF=(E,WORKU05)
         ENDIF
*
         LA    R1,USS_PAR15A
         LA    R15,USS_BPX1PIP_FDREAD1
         ST    R15,0(,R1)
         LA    R15,USS_RETVAL
         ST    R15,4(,R1)
         LA    R15,USS_RETCODE
         ST    R15,8(,R1)
         LA    R15,USS_REASON
         ST    R15,12(,R1)
         OI    12(R1),X'80'
*
         L     R15,G_BPX1CLOA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1CLO error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
         LA    R1,USS_PAR15A
         LA    R15,=F'-1'
         ST    R15,0(,R1)
         LA    R15,=A(0)
         ST    R15,4(,R1)
         LA    R15,USS_WAST
         ST    R15,USS_WASTA
         LA    R15,USS_WASTA
         ST    R15,8(,R1)
         LA    R15,USS_RETVAL
         ST    R15,12(,R1)
         LA    R15,USS_RETCODE
         ST    R15,16(,R1)
         LA    R15,USS_REASON
         ST    R15,20(,R1)
         OI    20(R1),X'80'
*
         L     R15,G_BPX1WATA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU05
            MVC   USS_MAK119E(22),=C'MAK119E BPX1WAT error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,         X
               ZSTR=USS_MAK119E
*
            BAL   R6,USS#05_MAK119E
*
            B     USS#05_RET
         ENDIF
*
USS#05_RET EQU   *
         CEETERM
*
* Construct MAK119E message
*
USS#05_MAK119E DS    0H
         MVC   G_DEC8(4),USS_RETCODE
         UNPK  G_ZONED8(9),G_DEC8(5)
         L     R15,G_HEXTAB
         TR    G_ZONED8(8),0(R15)
         MVI   G_ZONED8+8,X'00'
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,            X
               ZSTR=G_ZONED8
*
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,            X
               ZSTR==X'4000'
*
         MVC   G_DEC8,USS_REASON
         UNPK  G_ZONED8(9),G_DEC8(5)
         L     R15,G_HEXTAB
         TR    G_ZONED8(8),0(R15)
         MVI   G_ZONED8+8,X'00'
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU05,            X
               ZSTR=G_ZONED8
*
         L     R2,G_ISTB_tmp
         L     R2,STB_lpBuf-STB_obj(,R2)
         ILOG_Write OBJECT=G_ILOG,WORK=WORKU05,LINE=0(,R2),            X
               LOGLEVEL=LOG_LEVEL_ERROR
*
         MVC   G_RETCODE,=A(12)
*
         BR    R6
*
         LTORG
*
                             DS    0F
STDOUT_U05                   DC    C'//dd:STDOUT',X'00'
                             DS    0F
EXECSTR_U05                  DC    C'/bin/sh',X'00'
EXECLEN_U05                  DC    A(L'EXECSTR_U05)
ARG0STR_U05                  DC    C'-sh',X'00'
ARG0LEN_U05                  DC    A(L'ARG0STR_U05)
*ARG1STR_U05                  DC    C'-L',X'00'
*ARG1LEN_U05                  DC    A(L'ARG1STR_U05)
ARG1STR_U05                  DC    C'-c',X'00'
ARG1LEN_U05                  DC    A(L'ARG1STR_U05)
*ENV0STR_U05                  DC    C'_BPX_SHAREAS=MUST',X'00'
*ENV0LEN_U05                  DC    A(L'ENV0STR_U05)
*
                             DS    0F
MAK316I_U05                  DC    C'MAK316I SH ',X'00'
*
WORKDSAU05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKU05                      DS    4A
FCU05                        DS    3A
*
ISTB_sh_U05                  DS    A
ISTBPTR_retval_U05           DS    A
*
processId_U05                DS    F
newLine_U05                  DS    C
*
WORKDSAU05_SIZ               EQU   *-WORKDSAU05
*
         BPXYINHE
*
LWZMUSS  CSECT
*
         DROP
*
* IUSS BPX1OPN Open file
*
USS#06   CEEENTRY AUTO=WORKDSAU06_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAU06,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING USS_obj,R8        * Address object DSECT
*
         MVC   PARMPATHU06,4(R1) * Parm 2 is len prefixed path
*
         L     R2,8(,R1)         * Parm 3 is FD return ptr
         ST    R2,PARMFDRETVALU06
         MVC   0(4,R2),=A(0)
*
         LT    R15,G_BPX1OPNA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1OPN,SF=(E,G_LOADD) * entry point BPX1OPN
*
            ST    R0,G_BPX1OPNA * and store in global var
         ENDIF
*
         LA    R1,USS_PAR15A
         L     R14,PARMPATHU06
         XR    R15,R15
         LH    R15,0(,R14)
         ST    R15,PATHLENU06
         LA    R15,PATHLENU06
         ST    R15,0(,R1)
         LA    R15,2(,R14)
         ST    R15,4(,R1)
         LA    R15,=X'02000082'
         ST    R15,8(,R1)
         LA    R15,=A(0)
         ST    R15,12(,R1)
         LA    R15,USS_RETVAL
         ST    R15,16(,R1)
         LA    R15,USS_RETCODE
         ST    R15,20(,R1)
         LA    R15,USS_REASON
         ST    R15,24(,R1)
         OI    24(R1),X'80'
*
         L     R15,G_BPX1OPNA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU06
            MVC   USS_MAK119E(22),=C'MAK119E BPX1OPN error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU06,         X
               ZSTR=USS_MAK119E
*
            MVC   G_DEC8(4),USS_RETCODE
            UNPK  G_ZONED8(9),G_DEC8(5)
            L     R15,G_HEXTAB
            TR    G_ZONED8(8),0(R15)
            MVI   G_ZONED8+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU06,         X
               ZSTR=G_ZONED8
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU06,         X
               ZSTR==X'4000'
*
            MVC   G_DEC8,USS_REASON
            UNPK  G_ZONED8(9),G_DEC8(5)
            L     R15,G_HEXTAB
            TR    G_ZONED8(8),0(R15)
            MVI   G_ZONED8+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU06,         X
               ZSTR=G_ZONED8
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKU06,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_ERROR
*
            MVC   G_RETCODE,=A(12)
*
            B     USS#06_RET
         ENDIF
*
         L     R2,PARMFDRETVALU06
         MVC   0(4,R2),USS_RETVAL
*
USS#06_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAU06                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKU06                      DS    4A
PARMPATHU06                  DS    A
PARMFDRETVALU06              DS    A
PATHLENU06                   DS    F
*
WORKDSAU06_SIZ               EQU   *-WORKDSAU06
*
LWZMUSS  CSECT
*
         DROP
*
* IUSS BPX1CLO Close file
*
USS#07   CEEENTRY AUTO=WORKDSAU07_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAU07,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING USS_obj,R8        * Address object DSECT
*
         MVC   PARMFDU07,4(R1)   * Parm 2 is FD
*
         LT    R15,G_BPX1CLOA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1CLO,SF=(E,G_LOADD) * entry point BPX1CLO
*
            ST    R0,G_BPX1CLOA * and store in global var
         ENDIF
*
         LA    R1,USS_PAR15A
         LA    R15,PARMFDU07
         ST    R15,0(,R1)
         LA    R15,USS_RETVAL
         ST    R15,4(,R1)
         LA    R15,USS_RETCODE
         ST    R15,8(,R1)
         LA    R15,USS_REASON
         ST    R15,12(,R1)
         OI    12(R1),X'80'
*
         L     R15,G_BPX1CLOA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU07
            MVC   USS_MAK119E(22),=C'MAK119E BPX1CLO error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU07,         X
               ZSTR=USS_MAK119E
*
            MVC   G_DEC8(4),USS_RETCODE
            UNPK  G_ZONED8(9),G_DEC8(5)
            L     R15,G_HEXTAB
            TR    G_ZONED8(8),0(R15)
            MVI   G_ZONED8+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU07,         X
               ZSTR=G_ZONED8
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU07,         X
               ZSTR==X'4000'
*
            MVC   G_DEC8,USS_REASON
            UNPK  G_ZONED8(9),G_DEC8(5)
            L     R15,G_HEXTAB
            TR    G_ZONED8(8),0(R15)
            MVI   G_ZONED8+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU07,         X
               ZSTR=G_ZONED8
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKU07,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_ERROR
*
            MVC   G_RETCODE,=A(12)
*
            B     USS#07_RET
         ENDIF
*
USS#07_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAU07                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKU07                      DS    3A
PARMFDU07                    DS    A
*
WORKDSAU07_SIZ               EQU   *-WORKDSAU07
*
LWZMUSS  CSECT
*
         DROP
*
* IUSS BPX1RED Read character from file
*
USS#08   CEEENTRY AUTO=WORKDSAU08_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAU08,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING USS_obj,R8        * Address object DSECT
*
         MVC   PARMFDU08,4(R1)   * Parm 2 is FD
*
         MVC   PARMRETCHARU08,8(R1) * Parm 3 is return character
*
         L     R15,12(,R1)       * Parm 4 is return EOF
         ST    R15,PARMRETEOFU08
         MVI   0(R15),X'00'
*
         LT    R15,G_BPX1REDA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=BPX1RED,SF=(E,G_LOADD) * entry point BPX1RED
*
            ST    R0,G_BPX1REDA * and store in global var
         ENDIF
*
         LA    R1,USS_PAR15A
         LA    R15,PARMFDU08
         ST    R15,0(,R1)
         LA    R15,USS_BUFFER
         ST    R15,USS_BUFFERA
         LA    R15,USS_BUFFERA
         ST    R15,4(,R1)
         MVC   USS_ALET,=A(0)
         LA    R15,USS_ALET
         ST    R15,8(,R1)
         MVC   USS_COUNT,=A(1)
         LA    R15,USS_COUNT
         ST    R15,12(,R1)
         LA    R15,USS_RETVAL
         ST    R15,16(,R1)
         LA    R15,USS_RETCODE
         ST    R15,20(,R1)
         LA    R15,USS_REASON
         ST    R15,24(,R1)
         OI    24(R1),X'80'
*
         L     R15,G_BPX1REDA
         BASR  R14,R15
*
         IF (CLC,USS_RETVAL,EQ,=F'-1') THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKU08
            MVC   USS_MAK119E(22),=C'MAK119E BPX1RED error '
            MVI   USS_MAK119E+23,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU08,         X
               ZSTR=USS_MAK119E
*
            MVC   G_DEC8(4),USS_RETCODE
            UNPK  G_ZONED8(9),G_DEC8(5)
            L     R15,G_HEXTAB
            TR    G_ZONED8(8),0(R15)
            MVI   G_ZONED8+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU08,         X
               ZSTR=G_ZONED8
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU08,         X
               ZSTR==X'4000'
*
            MVC   G_DEC8,USS_REASON
            UNPK  G_ZONED8(9),G_DEC8(5)
            L     R15,G_HEXTAB
            TR    G_ZONED8(8),0(R15)
            MVI   G_ZONED8+8,X'00'
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKU08,         X
               ZSTR=G_ZONED8
*
            L     R2,G_ISTB_tmp
            L     R2,STB_lpBuf-STB_obj(,R2)
            ILOG_Write OBJECT=G_ILOG,WORK=WORKU08,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_ERROR
*
            MVC   G_RETCODE,=A(12)
*
            B     USS#08_RET
         ENDIF
*
         IF (CLC,USS_RETVAL,NE,=A(0)) THEN
            L     R2,PARMRETCHARU08
            MVC   0(1,R2),USS_BUFFER
         ELSE
            L     R2,PARMRETEOFU08
            MVI   0(R2),X'01'
         ENDIF
*
USS#08_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAU08                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKU08                      DS    3A
PARMFDU08                    DS    A
PARMRETCHARU08               DS    A
PARMRETEOFU08                DS    A
*
WORKDSAU08_SIZ               EQU   *-WORKDSAU08
*
LWZMUSS  CSECT
*
         DROP
*
         COPY  REGS              * Register equates
*
         END   LWZMUSS
