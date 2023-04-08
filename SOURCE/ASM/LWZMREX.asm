*---------------------------------------------------------------------*
* Program    : LWZMREX                                                *
* Description: COM object for executing REXX scripts                  *
*---------------------------------------------------------------------*
         TITLE 'LWZMREX'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates a new REX object
*
LWZMREX  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
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
*        Was a new REX object requested?
         IF (CLC,0(16,R6),EQ,G_IREX_GUID) THEN
            MNEWOBJ OBJTYPE=REX,WORK=WORK * Alloc new object
*
*           Init obj attributes
            LR    R7,R14
            USING REX_obj,R7
            MVI   REX_inTSO,C'N'
            MVI   REX_inISPF,C'N'
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
               ZSTR=MAK501D_REX
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
LWZMREX_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
REX#01A                      DC    A(REX#01)   * QueryInterface
REX#02A                      DC    A(REX#02)   * AddRef
REX#03A                      DC    A(REX#03)   * Release
REX#04A                      DC    A(REX#04)   * Exec
*
                             DS    0F
MAK501D_REX                  DC    C'MAK501D Created IREX object '
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
         COPY  DSREX             * IREX obj DSECT
         COPY  DSSTR             * ISTR obj DSECT
         COPY  DSSTB             * ISTB obj DSECT
*
* DSECT for addressing REXX ENV  block
         IRXENVB
*
* DSECT for addressing REXX PARM block
         IRXPARMB
*
* DSECT for addressing REXX host command environment table
         IRXSUBCT
*
* DSECT for addressing REXX EXEC block
         IRXEXECB
*
* DSECT for addressing REXX EVAL block
         IRXEVALB
*
         DROP
*
LWZMREX  CSECT
*
* IREX QueryInterface
*
REX#01   MQRYIFCE SUF=R01,IFACE=IREX
*
* IREX AddRef
*
REX#02   MADDREF
*
* IREX Release
*
REX#03   MRELEASE SUF=R03,OBJ=REX
*
LWZMREX  CSECT
*
         DROP
*
* IREX Exec
*
REX#04   CEEENTRY AUTO=WORKDSAR04_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAR04,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING REX_obj,R8        * Address object DSECT
*
         MVC   PARMISTBEXECR04,4(R1) * Parm 2 is EXEC cmd ISTB
*
         LT    R2,8(,R1)         * Parm 3 is ISTR return ptr
         ST    R2,ISTBPTR_retval_R04 * Save in local var
*
         IF (NZ) THEN
            IF (CLC,0(4,R2),EQ,=A(0)) THEN
               MINSTANT GUID=G_ISTB_GUID,WORK=WORKR04,OBJPTR=0(,R2)
            ELSE
               ISTB_Init OBJECT=0(,R2),WORK=WORKR04
            ENDIF
         ENDIF
*
         MVC   RETCODER04,=A(0)
*
         L     R15,G_ILOG
         IF (CLI,13(R15),GE,LOG_LEVEL_INFO) THEN
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKR04
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKR04,         X
               ZSTR=MAK308I_R04
*
            L     R7,PARMISTBEXECR04
            L     R7,STB_lpBuf-STB_obj(,R7)
*
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKR04,         X
               ZSTR=0(,R7)
*
            L     R7,G_ISTB_tmp
            L     R7,STB_lpBuf-STB_obj(,R7)
*
            ILOG_Write OBJECT=G_ILOG,WORK=WORKR04,LINE=0(,R7),         X
               LOGLEVEL=LOG_LEVEL_INFO
         ENDIF
*
         IF (CLC,REX_ENVBLOCK_Ptr,EQ,=A(0)) THEN
            L     R15,REX#99A_R04
            BASR  R14,R15
         ENDIF
*
         IF (CLI,REX_inISPF,EQ,C'Y'),AND,                              X
               (CLC,ISTBPTR_retval_R04,EQ,=A(0)) THEN
            MINSTANT GUID=G_ISTB_GUID,WORK=WORKR04,OBJPTR=ISTB_exec_R04
*
            ISTB_AppendZString OBJECT=ISTB_exec_R04,WORK=WORKR04,      X
               ZSTR=SELECT_R04
*
            L     R5,PARMISTBEXECR04
            L     R6,STB_lpBuf-STB_obj(,R5)
*
            ISTB_AppendZString OBJECT=ISTB_exec_R04,WORK=WORKR04,      X
               ZSTR=0(,R6)
*
            ISTB_AppendZString OBJECT=ISTB_exec_R04,WORK=WORKR04,      X
               ZSTR==X'5D00'
*
            LA    R1,WORKR04
            L     R15,ISTB_exec_R04
            LA    R14,STB_nStrLen-STB_obj(R15)
            ST    R14,0(,R1)
            MVC   4(4,R1),STB_lpBuf-STB_obj(R15)
            OI    4(R1),X'80'
*
            MVC   G_LINKD,G_LINKL
*
            LINK  EP=ISPEXEC,SF=(E,G_LINKD)
*
            IF (C,R15,GT,=A(8)) THEN
               CVD   R15,G_DEC8
               UNPK  G_ZONED8,G_DEC8
               OI    G_ZONED8+7,X'F0'
               MVI   G_ZONED8+8,X'00'
               L     R14,G_TRT_ONLY_ZEROS
               TRT   G_ZONED8(7),0(R14)
               BC    7,*+8
               LA    R1,G_ZONED8+7
               LR    R7,R1
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKR04
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKR04,      X
               ZSTR=MAK110E_R04
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKR04,      X
               ZSTR=0(,R7)
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKR04,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_ERROR
*
               CALL  CEE3ABD,(=A(1005),=A(3)),MF=(E,WORKR04)
            ENDIF
*
            ST    R15,RETCODER04
*
            ISTB_Release OBJECT=ISTB_exec_R04,WORK=WORKR04
            MVC   ISTB_exec_R04,=A(0)
*
         ELSE
            MVI   callExec_R04,C' '
            MVC   callExec_R04+1(L'callExec_R04-2),callExec_R04
            MVI   callExec_R04+L'callExec_R04-1,X'00'
*
            MVC   callParmPtr_R04,=A(0)
            MVC   callParmLen_R04,=A(0)
*
            L     R5,PARMISTBEXECR04
            L     R6,STB_lpBuf-STB_obj(,R5)
            L     R7,STB_nStrLen-STB_obj(,R5)
*
            L     R1,G_TRT_ANY_BUT_SPACE
*
            LR    R4,R6
*
            TRTE  R6,R3,0
            BC    1,*-4
*
            IF (12) THEN
               LR    R5,R6
               SR    R5,R4
               BCTR  R5,R0
               B     *+10
               MVC   callExec_R04(1),0(R4)
               EX    R5,*-6
*
               IF (C,R7,GT,=A(0)) THEN
                  L     R1,G_TRT_ONLY_SPACE
*
                  TRTE  R6,R3,0
                  BC    1,*-4
*
                  IF (4) THEN
                     ST    R6,callParmPtr_R04
                     ST    R7,callParmLen_R04
                  ENDIF
               ENDIF
            ELSE
               BCTR  R7,R0
               B     *+10
               MVC   callExec_R04(1),0(R6)
               EX    R7,*-6
            ENDIF
*
            LA    R6,REX_EXECBLK
            USING EXECBLK,R6
            MVC   EXEC_BLK_ACRYN,=CL8'IRXEXECB'
            MVC   EXEC_BLK_LENGTH,=A(EXECBLK_V2_LEN)
            MVC   EXEC_MEMBER,callExec_R04
            MVC   EXEC_DDNAME,=CL8' '
            MVC   EXEC_SUBCOM,=CL8' '
            MVC   EXEC_DSNPTR,=A(0)
            MVC   EXEC_DSNLEN,=A(0)
            MVC   EXEC_EXTNAME_PTR,=A(0)
            MVC   EXEC_EXTNAME_LEN,=A(0)
            DROP  R6
*
            LA    R6,REX_EVALBLK
            IF (CLC,REX_EVALBLK_Ptr,EQ,=A(0)) THEN
               ST    R6,REX_EVALBLK_Ptr
*
               MVI   0(R6),X'00'
               MVC   1(255,R6),0(R6)
               MVC   256(16,R6),255(R6)
               USING EVALBLOCK,R6
               MVC   EVALBLOCK_EVSIZE,=A(34) * 272 / 8
               DROP  R6
            ELSE
               MVC   EVALBLOCK_EVLEN-EVALBLOCK(4,R6),=A(0)
            ENDIF
*
            LA    R15,REX_EXECBLK
            ST    R15,REX_EXECBLK_Ptr
*
            IF (CLC,callParmLen_R04,EQ,=A(0)) THEN
               MVC   REX_ARGS(8),=X'FFFFFFFFFFFFFFFF'
            ELSE
               MVC   REX_ARGS(4),callParmPtr_R04
               MVC   REX_ARGS+4(4),callParmLen_R04
               MVC   REX_ARGS+8(8),=X'FFFFFFFFFFFFFFFF'
            ENDIF
            LA    R15,REX_ARGS
            ST    R15,REX_ARGS_Ptr
*
            MVC   REX_FLAGS,=X'50000000'
            MVC   REX_INSTBLK_Ptr,=A(0)
            MVC   REX_CPPL_Ptr,=A(0)
*
            MVC   REX_WORKAREA_Ptr,=A(0)
            MVC   REX_USRFIELD_Ptr,=A(0)
*
            LT    R15,G_IRXEXECA
            IF (Z) THEN
               MVC   G_LOADD,G_LOADL
*
               LOAD  EP=IRXEXEC,SF=(E,G_LOADD) * entry point IRXEXEC
*
               ST    R0,G_IRXEXECA * and store in global var
            ENDIF
*
            LA    R1,WORKR04
            LA    R15,REX_EXECBLK_Ptr
            ST    R15,0(,R1)
            LA    R15,REX_ARGS_Ptr
            ST    R15,4(,R1)
            LA    R15,REX_FLAGS
            ST    R15,8(,R1)
            LA    R15,REX_INSTBLK_Ptr
            ST    R15,12(,R1)
            LA    R15,REX_CPPL_Ptr
            ST    R15,16(,R1)
            LA    R15,REX_EVALBLK_Ptr
            ST    R15,20(,R1)
            LA    R15,REX_WORKAREA_Ptr
            ST    R15,24(,R1)
            LA    R15,REX_USRFIELD_Ptr
            ST    R15,28(,R1)
            LA    R15,REX_ENVBLOCK_Ptr
            ST    R15,32(,R1)
            LA    R15,RETCODER04
            ST    R15,36(,R1)
            OI    36(R1),X'80'
*
            XR    R0,R0
*
            L     R15,G_IRXEXECA
            BASR  R14,R15
*
            IF (C,R15,GE,=A(20000)) THEN
               CVD   R15,G_DEC8
               UNPK  G_ZONED8,G_DEC8
               OI    G_ZONED8+7,X'F0'
               MVI   G_ZONED8+8,X'00'
               L     R14,G_TRT_ONLY_ZEROS
               TRT   G_ZONED8(7),0(R14)
               BC    7,*+8
               LA    R1,G_ZONED8+7
               LR    R7,R1
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKR04
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKR04,      X
               ZSTR=MAK109E_R04
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKR04,      X
               ZSTR=0(,R7)
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKR04,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_ERROR
*
               MVC   G_RETCODE,=A(8)
               B     REX#04_RET
            ENDIF
*
            ST    R15,RETCODER04
*
            LA    R5,REX_EVALBLK
            USING EVALBLOCK,R5
            LT    R7,EVALBLOCK_EVLEN
            IF (2) THEN
               IF (CLC,ISTBPTR_retval_R04,NE,=A(0)) THEN
                  LA    R1,WORKR04
                  LA    R15,1(,R7)
                  ST    R15,0(,R1)
                  LA    R15,retvalZstr_R04
                  ST    R15,4(,R1)
                  L     R15,G_GTST
                  BASR  R14,R15
*
                  LA    R6,EVALBLOCK_EVDATA
                  L     R14,retvalZstr_R04
                  LR    R15,R7
                  MVCL  R14,R6
                  MVI   0(R14),X'00'
*
                  L     R2,ISTBPTR_retval_R04
                  L     R3,retvalZstr_R04
                  ISTB_AppendZString OBJECT=0(,R2),WORK=WORKR04,       X
               ZSTR=0(,R3)
               ENDIF
            ELSE
               X     R7,=X'FFFFFFFF'
               LA    R7,26(,R7)  * Add 1 plus room for 4A + 8 + 1 zero
               LA    R7,7(,R7)   * Add 7 more to round up
               N     R7,=X'FFFFFFF8'
               ST    R7,G_GTSTSIZ
*
               LA    R1,WORKR04
               ST    R7,0(,R1)
               LA    R15,REX_EVALBLK_Ptr
               ST    R15,4(,R1)
               L     R15,G_GTST
               BASR  R14,R15
*
               L     R6,REX_EVALBLK_Ptr
               L     R7,G_GTSTSIZ
               XR    R14,R14
               XR    R15,R15
               MVCL  R6,R14
               L     R6,REX_EVALBLK_Ptr
               L     R5,G_GTSTSIZ
               SRA   R5,3
               ST    R5,EVALBLOCK_EVSIZE-EVALBLOCK(R6)
*
               LT    R15,G_IRXRLTA
               IF (Z) THEN
                  MVC   G_LOADD,G_LOADL
*
                  LOAD  EP=IRXRLT,SF=(E,G_LOADD) * entry point IRXRLT
*
                  ST    R0,G_IRXRLTA * and store in global var
               ENDIF
*
               LA    R1,WORKR04
               LA    R15,REX_IRXRLT_FUNCTION
               MVC   0(8,R15),=CL8'GETRLTE'
               ST    R15,0(,R1)
               LA    R15,REX_EVALBLK_Ptr
               ST    R15,4(,R1)
               MVC   8(4,R1),=A(0)
               OI    8(R1),X'80'
*
               L     R0,REX_ENVBLOCK_Ptr
*
               L     R15,G_IRXRLTA
               BASR  R14,R15
*
               IF (C,R15,EQ,=A(0)),OR,(C,R15,EQ,=A(4)) THEN
                  L     R2,ISTBPTR_retval_R04
                  L     R3,REX_EVALBLK_Ptr
                  LA    R3,EVALBLOCK_EVDATA-EVALBLOCK(,R3)
                  ISTB_AppendZString OBJECT=0(,R2),WORK=WORKR04,       X
               ZSTR=0(,R3)
               ELSE
                  CALL  CEE3ABD,(=A(1005),=A(3)),MF=(E,WORKR04)
               ENDIF
            ENDIF
            DROP  R5
         ENDIF
*
REX#04_RET EQU   *
         CEETERM RC=RETCODER04
*
         LTORG
*
                             DS    0F
SELECT_R04                   DC    C'SELECT CMD(%',X'00'
*
                             DS    0F
MAK308I_R04                  DC    C'MAK308I CALL ',X'00'
*
                             DS    0F
MAK109E_R04                  DC    C'MAK109E Error invoking IRXEXEC ',XX
               '00'
                             DS    0F
MAK110E_R04                  DC    C'MAK110E Error invoking ISPEXEC ',XX
               '00'
*
                             DS    0F
REX#99A_R04                  DC    A(REX#99)
*
WORKDSAR04                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKR04                      DS    10A
RETCODER04                   DS    F
*
PARMISTBEXECR04              DS    A
ISTBPTR_retval_R04           DS    A
ISTB_exec_R04                DS    A
ISTB_info_R04                DS    A
*
callParmPtr_R04              DS    A
callParmLen_R04              DS    A
callExec_R04                 DS    CL9
*
retvalZstr_R04               DS    A
*
WORKDSAR04_SIZ               EQU   *-WORKDSAR04
*
LWZMREX  CSECT
*
         DROP
*
* IREX Init REX_ENVBLOCK_Ptr
*
REX#99   CEEENTRY AUTO=WORKDSAR99_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAR99,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING REX_obj,R8        * Addressability of IREX
*
         MVC   functionR99,=C'FINDENVB'
         MVC   parmModR99,=CL8' '
         MVC   inStoreParmListR99,=A(0)
         MVC   userFieldR99,=X'80000000'
         MVC   reservedR99,=A(0)
         MVC   reasonR99,=A(0)
*
         LT    R15,G_IRXINITA
         IF (Z) THEN
            MVC   G_LOADD,G_LOADL
*
            LOAD  EP=IRXINIT,SF=(E,G_LOADD) * entry point IRXINIT
*
            ST    R0,G_IRXINITA * and store in global var
         ENDIF
*
         LA    R1,WORKR99
         LA    R15,functionR99
         ST    R15,0(,R1)
         LA    R15,parmModR99
         ST    R15,4(,R1)
         LA    R15,inStoreParmListR99
         ST    R15,8(,R1)
         LA    R15,userFieldR99
         ST    R15,12(,R1)
         LA    R15,reservedR99
         ST    R15,16(,R1)
         LA    R15,REX_ENVBLOCK_Ptr
         ST    R15,20(,R1)
         LA    R15,reasonR99
         ST    R15,24(,R1)
         OI    24(R1),X'80'
*
         L     R15,G_IRXINITA
         BASR  R14,R15
*
         IF (C,R15,EQ,=A(0)),OR,(C,R15,EQ,=A(4)) THEN
            L     R2,REX_ENVBLOCK_Ptr
            L     R2,ENVBLOCK_PARMBLOCK-ENVBLOCK(,R2)
            TM    PARMBLOCK_FLAGS1-PARMBLOCK(R2),TSOFL
            IF (NZ) THEN
               MVI   REX_inTSO,C'Y'
*
               L     R3,PARMBLOCK_SUBCOMTB-PARMBLOCK(,R2)
               L     R4,8(,R3)   * Used entries
               L     R5,12(,R3)  * Length of each entry
               L     R3,0(,R3)   * Address of first entry
               DO FROM=(R4)
                  IF (CLC,0(8,R3),EQ,=CL8'ISPEXEC') THEN
                     MVI   REX_inISPF,C'Y'
                     ASMLEAVE
                  ENDIF
                  AR    R3,R5
               ENDDO
            ENDIF
         ELSEIF (C,R15,EQ,=A(28)) THEN
            MVC   functionR99,=C'INITENVB'
            MVC   parmModR99,=CL8' '
            MVC   inStoreParmListR99,=A(0)
            MVC   userFieldR99,=X'80000000'
            MVC   reservedR99,=A(0)
            MVC   reasonR99,=A(0)
            MVC   stgWorkareaR99,=A(0)
*
            LA    R1,WORKR99
            LA    R15,functionR99
            ST    R15,0(,R1)
            LA    R15,parmModR99
            ST    R15,4(,R1)
            LA    R15,inStoreParmListR99
            ST    R15,8(,R1)
            LA    R15,userFieldR99
            ST    R15,12(,R1)
            LA    R15,reservedR99
            ST    R15,16(,R1)
            LA    R15,REX_ENVBLOCK_Ptr
            ST    R15,20(,R1)
            LA    R15,reasonR99
            ST    R15,24(,R1)
            LA    R15,stgWorkareaR99
            ST    R15,28(,R1)
            OI    28(R1),X'80'
*
            L     R15,G_IRXINITA
            BASR  R14,R15
*
            IF (C,R15,NE,=A(0)),AND,(C,R15,NE,=A(4)) THEN
               CALL  CEE3ABD,(=A(1005),=A(3)),MF=(E,WORKR99)
            ENDIF
         ELSE
            CALL  CEE3ABD,(=A(1005),=A(3)),MF=(E,WORKR99)
         ENDIF
REX#99_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAR99                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKR99                      DS    8A
*
functionR99                  DS    CL8
parmModR99                   DS    CL8
inStoreParmListR99           DS    A
userFieldR99                 DS    A
reservedR99                  DS    A
reasonR99                    DS    A
stgWorkareaR99               DS    A
*
WORKDSAR99_SIZ               EQU   *-WORKDSAR99
*
LWZMREX  CSECT
*
         DROP
*
         COPY  REGS              * Register equates
*
         END   LWZMREX
