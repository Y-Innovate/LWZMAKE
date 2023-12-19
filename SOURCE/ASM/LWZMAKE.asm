***********************************************************************
* LL       WW      WW   ZZZZZZ   MM    MM    AAAAA    KK  KK   EEEEEE *
* LL       WW      WW      ZZ    MMMMMMMM   AA   AA   KK KK    EE     *
* LL        WW WW WW      ZZ     MM MM MM   AAAAAAA   KKKK     EEEE   *
* LL        WWWWWWWW     ZZ      MM    MM   AA   AA   KK KK    EE     *
* LLLLLL     WW  WW     ZZZZZZ   MM    MM   AA   AA   KK  KK   EEEEEE *
*                                                                     *
* =================================================================== *
* Description:                                                        *
* ------------------------------------------------------------------- *
* LWZMAKE is a build tool for z/OS that executes a zmake makefile.    *
* A zmake makefile is a script that defines targets (build output),   *
* their dependencies and the recipes (small script functions within   *
* the zmake makefile) to build those targets, very similar to make.   *
* The two major differences with make are 1) that LWZMAKE can handle  *
* both MVS and USS files, and 2) that LWZMAKE doesn't start USS       *
* commandline commands, but instead allows you to start a REXX with   *
* a parameter string (which could in turn start a USS command).       *
* ------------------------------------------------------------------- *
* Error codes:                                                        *
* MAK101E Error opening <ddname>                                      *
* MAK102E Error closing <ddname>                                      *
* MAK103E Unexpected token                                            *
* MAK104E Duplicate entry not allowed                                 *
* MAK105E .RECIPEPREFIX should be a single character                  *
* MAK106E Target not found: <target name>                             *
* MAK107E REXX <name> returned <return value>                         *
* MAK108E IGGCSI00 Catalog search error <error codes>                 *
* MAK109E Error invoking IRXEXEC <error code>                         *
* MAK110E Error invoking ISPEXEC <error code>                         *
* MAK111E CAMLST OBTAIN error <error code>                            *
* MAK112E Member specified on non-PDS: <data set name>                *
* MAK113E Dynamic allocation failed <error code> <data set name>      *
* MAK114E Dynamic deallocation failed <error code> <data set name>    *
* MAK115E Binder error <func> <error code> <data set name>            *
* MAK116E Prerequisite does not exist <data set name>                 *
* MAK117E Invalid log level <log level>                               *
* MAK118E Undefined variable <variable name>                          *
* MAK119E BPX1xxx error <retcode> <reason>                            *
* MAK120E Invalid BUILDWHEN value <value>                             *
* MAK201W Member without statistics <data set name>                   *
* MAK301I Start                                                       *
* MAK302I Done                                                        *
* MAK303I Opened input data set with DD name                          *
* MAK304I Closed input data set with DD name                          *
* MAK305I Variable <name> = <value>                                   *
* MAK306I Building target: <target name>                              *
* MAK307I Target is phony: <target name>                              *
* MAK308I <REXX name> <parameters>                                    *
* MAK309I Checking target <target> prereqs <prereqs>                  *
* MAK310I Data set is cataloged: <data set name>                      *
* MAK311I Member not found: <data set name>                           *
* MAK312I Target does not exist <data set name>                       *
* MAK313I Pre-existing target <data set name> was last updated on <da *
*         tetime>                                                     *
* MAK313I Pre-existing target <data set name>                         *
* MAK314I Prereq is newer <data set name> <datetime>                  *
* MAK315I Target should be built unconditionally <data set name>      *
* MAK316I <USS cmd> <parameters>                                      *
* MAK317I Prereq is unequal <data set name> <datetime>                *
* MAK401D Parsing statement                                           *
* MAK402D Parsing assignment statement                                *
* MAK403D Parsing rule statement                                      *
* MAK404D Parsing CALL statement                                      *
* MAK405D Parsing special                                             *
* MAK406D Parsing continuation                                        *
* MAK407D Parsing variable                                            *
* MAK408D Added last state of <parser state>                          *
* MAK409D Altered last state of <parser state>                        *
* MAK410D Reverted to previous state of <parser state>                *
* MAK411D Got next token <token type> <token>                         *
* MAK412D Last altered date of <data set> is <date>                   *
* MAK413D Load module date of <data set> is <date>                    *
* MAK414D Parsing parameter                                           *
* MAK415D Last modified date of <uss file> is <date>                  *
* MAK416D Parsing SH statement                                        *
* MAK501D Created I<obj> object                                       *
* MAK502D Deleted I<obj> object                                       *
* MAK503D Initialized I<obj> object                                   *
* MAK504D Read an input line                                          *
* MAK505D Input file is EOF                                           *
* MAK506D Getting next token                                          *
* MAK601D Got next character <character>                              *
* ------------------------------------------------------------------- *
* Abend codes:                                                        *
* 1000 Unable to allocate global memory area                          *
* 1001 Unable to allocate initial LWZMAKE managed heap                *
* 1002 Unable to allocate extended LWZMAKE managed heap               *
* 1003 Unknown class GUID requested                                   *
* 1004 Error opening/closing DD                                       *
* 1005 Error invoking IRXEXEC/IRXINIT                                 *
* 1006 Error invoking ISPEXEC                                         *
* 1007 Error invoking IGGCSI00                                        *
* 1008 Duplicate entry not allowed in AVL/AV2                         *
***********************************************************************
         TITLE 'LWZMAKE'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
* LWZMAKE mainline
*
LWZMAKE  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=YES,BASE=(R10,R11)
*
         USING WORKDSA,R13       * Address DSA and extra stg for vars
*
         ST    R1,PARML_PTR      * Save address of parameter ptrs
*
*        Allocate global area shared across all objects
         CALL  CEEGTST,(=A(0),=A(GLOBAL_SIZ),GLOBAL_PTR,FC),           X
               MF=(E,WORK)
*
         IF (CLC,FC(8),NE,=XL8'0000000000000000') THEN
            CALL  CEE3ABD,(=A(1000),=A(3)),MF=(E,WORK)
         ENDIF
*
         L     R9,GLOBAL_PTR     * R9 points to global area
         USING GLOBAL,R9         * Address global DSECT
*
         BAL   R7,INIT           * Initializations
*
         BAL   R7,PARMS          * Read parameters
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   LWZMAKE_RET       * skip the rest
*
         ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=MAK301I,              X
               LOGLEVEL=LOG_LEVEL_INFO
*
         BAL   R7,FILETYPE       * Determine makefile file type
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   LWZMAKE_RET       * skip the rest
*
*        If makefile is data set, use IND
         IF (CLI,datasetType,EQ,C'D') THEN DO
*
*           Instantiate a new IIND object
            MINSTANT GUID=G_IIND_GUID,WORK=WORK,OBJPTR=IIND_MAIN
*
*        If makefile is unix file, use INF
         ELSE
*
*           Instantiate a new IINF object
            MINSTANT GUID=G_IINF_GUID,WORK=WORK,OBJPTR=IINF_MAIN
*
         ENDIF
*
*        Open makefile input data set
         IIND_Open OBJECT=IIND_MAIN,WORK=WORK,DDNAME==CL8'LWZMINP'
*
*        Instantiate a new IIFO object
         MINSTANT GUID=G_IIFO_GUID,WORK=WORK,OBJPTR=IIFO_MAIN
*
*        Instantiate a new IPRS object
         MINSTANT GUID=G_IPRS_GUID,WORK=WORK,OBJPTR=IPRS_MAIN
*
*        Instantiate a new IPSS object
         MINSTANT GUID=G_IPSS_GUID,WORK=WORK,OBJPTR=IPSS_MAIN
*
*        Instantiate a new ITOK object
         MINSTANT GUID=G_ITOK_GUID,WORK=WORK,OBJPTR=ITOK_MAIN
*
         ITOK_Init OBJECT=ITOK_MAIN,WORK=WORK,IIN=IIND_MAIN,           X
               IIFO=IIFO_MAIN
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   LWZMAKE_RET       * skip the rest
*
         L     R6,IIFO_MAIN
         DO UNTIL=(CLC,G_RETCODE,NE,=A(0),OR,                          X
               CLI,IFO_bEOF-IFO_obj(R6),EQ,C'Y')
*
            IPRS_ParseStatement OBJECT=IPRS_MAIN,WORK=WORK,            X
               IPSS=IPSS_MAIN,ITOK=ITOK_MAIN
*
         ENDDO
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   LWZMAKE_RET       * skip the rest
*
         IIND_Close OBJECT=IIND_MAIN,WORK=WORK
*
*        Instantiate a new IAV2 object for targets to build
         MINSTANT GUID=G_IAV2_GUID,WORK=WORK,OBJPTR=IAV2_buildTargets
*
         IPRS_ResolveParameter OBJECT=IPRS_MAIN,WORK=WORK,             X
               PARMS=G_ISTB_buildTargets,IAV2TGTS=IAV2_buildTargets
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   LWZMAKE_RET       * skip the rest
*
         IPSS_SetPhase OBJECT=IPSS_MAIN,WORK=WORK,PHASE==C'2'
*
         IPRS_BuildTargets OBJECT=IPRS_MAIN,WORK=WORK,                 X
               IAV2TGTS=IAV2_buildTargets
*
LWZMAKE_RET EQU   *              * Skip to here to finish
*
         BAL   R7,WRAPUP         * Wrap things up
*
         L     R2,G_RETCODE      * Set returncode
*
*        Free global area
         CALL  CEEFRST,(GLOBAL_PTR,FC),MF=(E,WORK)
*
         CEETERM RC=(R2),MF=(E,WORK)
*
* Initializations
*
INIT     DS    0H
         LA    R2,GLOBAL         * Initialize global area
         L     R3,=A(GLOBAL_SIZ) *   to all
         XR    R0,R0             *     zeros
         XR    R1,R1
         MVCL  R2,R0
*
         MVC   G_LINKL,LINKL
*
         MVC   G_LOADL,LOADL
*
         MVC   G_OPENL,OPENL
*
         MVC   G_CLOSEL,CLOSEL
*
         MVC   G_CONVTODL,CONVTODL
*
         MVC   G_STCKCONVL,STCKCONVL
*
         MVC   G_SWAREQL,SWAREQL
*
         MVC   G_GETDSABL,GETDSABL
*
         CALL  CEEGTST,(=A(0),=A(4096),G_HEAP,FC),MF=(E,WORK)
*
         IF (CLC,FC(8),NE,=XL8'0000000000000000') THEN
            CALL  CEE3ABD,(=A(1001),=A(3)),MF=(E,WORK)
         ENDIF
*
         XR    R0,R0
         XR    R1,R1
         L     R2,G_HEAP
         L     R3,=A(4096)
         MVCL  R2,R0
*
         L     R2,G_HEAP
         USING HEAPSEG,R2
         MVC   HEAPSEG_SIZ,=A(4096)
         MVC   HEAPSEG_LEFT,=A(4096-HEAPSEGDS_SIZ)
         DROP  R2
*
         MVC   G_GTST,LWZMGTSTA
*
         MVC   G_IAVL_GUID,=XL16'DB7A760CAF594E5B8AE56AB96CB45E45'
         MVC   G_IAV2_GUID,=XL16'48C91686AB184D43BC7D460B5ACD92B0'
         MVC   G_IFFO_GUID,=XL16'AAB19835736849058A1AE9BA9690472C'
         MVC   G_IFMG_GUID,=XL16'FBAB8F58290646D4B25212007D9ADF5B'
         MVC   G_IIFO_GUID,=XL16'E7C15027869045AAB6B59095A150031A'
         MVC   G_IIND_GUID,=XL16'36BD513C7B064E7989594890E873DE69'
         MVC   G_IINF_GUID,=XL16'E44AC6BDC9A3491E96AF326979C366EE'
         MVC   G_IINS_GUID,=XL16'E1C8B43FBFC54BF3BCC39628052F8771'
         MVC   G_ILOG_GUID,=XL16'3CAFEC54D6FC461C8F68F55CC6030554'
         MVC   G_IPRS_GUID,=XL16'73D05C4DBA6D4285B3CF39B3B2E29F8B'
         MVC   G_IPSS_GUID,=XL16'16DC67F5C8B949309D54B8AEA83F9281'
         MVC   G_IREX_GUID,=XL16'866BC126D13847BCB8B9F9B644F8A0DD'
         MVC   G_ISTB_GUID,=XL16'866BBAD1400E444E987DD23B8575F8DC'
         MVC   G_ISTR_GUID,=XL16'71CEBE514C98488786311FDC982AFF70'
         MVC   G_IST1_GUID,=XL16'99FF343EB0574E8E94B4C1C7DAD475F7'
         MVC   G_IST2_GUID,=XL16'C2D9EF6CDF964A7BBE4C71CB6165F827'
         MVC   G_IST3_GUID,=XL16'08B03EA5EAC94A04BBB2DDCC26FABB4C'
         MVC   G_IST4_GUID,=XL16'CB74556DCB0A4061BBA57C43B9C0D766'
         MVC   G_IST5_GUID,=XL16'D416AD49706949DBA3B3DF415DF63E0F'
         MVC   G_IST6_GUID,=XL16'7897F500AAB4410381C1A4B4B3FFE200'
         MVC   G_IST7_GUID,=XL16'7A000760334A4FB1847765E2A3CF16C6'
         MVC   G_IST8_GUID,=XL16'5DD95BAC9C554672A2657E70DA626DF1'
         MVC   G_ITFO_GUID,=XL16'636548D956D74AEBB65B07032B1FA868'
         MVC   G_ITOK_GUID,=XL16'A90CC60AC47042589BE8D7B09B55353E'
         MVC   G_ITOK_GUID,=XL16'A1F14400723E4ADBA13EE0BA41E5FDB4'
*
         MVC   G_BUILDWHEN(3),=C'TOM'
         MVI   G_BUILDWHEN+3,X'00'
*
         LARL  R15,TRT_ANY_BUT_SPACE
         ST    R15,G_TRT_ANY_BUT_SPACE
*
         LARL  R15,TRT_ONLY_SPACE
         ST    R15,G_TRT_ONLY_SPACE
*
         LARL  R15,TRT_ONLY_ZEROS
         ST    R15,G_TRT_ONLY_ZEROS
*
         LARL  R15,HEXTAB
         ST    R15,G_HEXTAB
*
         MVC   IINS_PARMS,=A(0)
         MVC   IIFO_PARMS,=A(0)
         MVC   IPRS_PARMS,=A(0)
         MVC   IPSS_PARMS,=A(0)
         MVC   ITOK_PARMS,=A(0)
*
         MVC   IIND_MAIN,=A(0)
         MVC   IIFO_MAIN,=A(0)
         MVC   IPRS_MAIN,=A(0)
         MVC   IPSS_MAIN,=A(0)
         MVC   ITOK_MAIN,=A(0)
*
         MVC   IAV2_buildTargets,=A(0)
*
*        Instantiate a new ILOG object
         MINSTANT GUID=G_ILOG_GUID,WORK=WORK,OBJPTR=G_ILOG
*
*        Open log data set
         ILOG_Open OBJECT=G_ILOG,WORK=WORK
*
         ILOG_SetLogLevel OBJECT=G_ILOG,WORK=WORK,                     X
               LOGLEVEL==A(LOG_LEVEL_INFO)
*
*        Instantiate a new IFMG object
         MINSTANT GUID=G_IFMG_GUID,WORK=WORK,OBJPTR=G_IFMG
*
*        Instantiate a new ISTB object
         MINSTANT GUID=G_ISTB_GUID,WORK=WORK,OBJPTR=G_ISTB_tmp
*
*        Instantiate a new IREX object for calling REXX execs
         MINSTANT GUID=G_IREX_GUID,WORK=WORK,OBJPTR=G_IREX
*
*        Instantiate a new IUSS object for calling USS commands
         MINSTANT GUID=G_IUSS_GUID,WORK=WORK,OBJPTR=G_IUSS
*
INIT_RET EQU   *
         BR    R7                * Return to mainline
*
* Wrap things up
*
WRAPUP   DS    0H
         IF (CLC,ITOK_MAIN,NE,=A(0)) THEN
            ITOK_Release OBJECT=ITOK_MAIN,WORK=WORK
            MVC   ITOK_MAIN,=A(0)
         ENDIF
*
         IF (CLC,IPSS_MAIN,NE,=A(0)) THEN
            IPSS_Release OBJECT=IPSS_MAIN,WORK=WORK
            MVC   IPSS_MAIN,=A(0)
         ENDIF
*
         IF (CLC,IPRS_MAIN,NE,=A(0)) THEN
            IPRS_Release OBJECT=IPRS_MAIN,WORK=WORK
            MVC   IPRS_MAIN,=A(0)
         ENDIF
*
         IF (CLC,IIFO_MAIN,NE,=A(0)) THEN
            IIFO_Release OBJECT=IIFO_MAIN,WORK=WORK
            MVC   IIFO_MAIN,=A(0)
         ENDIF
*
         IF (CLC,IIND_MAIN,NE,=A(0)) THEN
            IIND_Close OBJECT=IIND_MAIN,WORK=WORK
*
            IIND_Release OBJECT=IIND_MAIN,WORK=WORK
            MVC   IIND_MAIN,=A(0)
         ENDIF
*
         IF (CLC,G_IUSS,NE,=A(0)) THEN
            IUSS_Release OBJECT=G_IUSS,WORK=WORK
            MVC   G_IUSS,=A(0)
         ENDIF
*
         IF (CLC,G_IREX,NE,=A(0)) THEN
            IREX_Release OBJECT=G_IREX,WORK=WORK
            MVC   G_IREX,=A(0)
         ENDIF
*
         IF (CLC,G_IFMG,NE,=A(0)) THEN
            IFMG_Release OBJECT=G_IFMG,WORK=WORK
            MVC   G_IFMG,=A(0)
         ENDIF
*
         IF (CLC,G_ISTB_tmp,NE,=A(0)) THEN
            ISTB_Release OBJECT=G_ISTB_tmp,WORK=WORK
            MVC   G_ISTB_tmp,=A(0)
         ENDIF
*
         IF (CLC,G_ILOG,NE,=A(0)) THEN
            ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=MAK302I,           X
               LOGLEVEL=LOG_LEVEL_INFO
*
            ILOG_Close OBJECT=G_ILOG,WORK=WORK
*
            ILOG_Release OBJECT=G_ILOG,WORK=WORK
            MVC   G_ILOG,=A(0)
         ENDIF
*
WRAPUP_RET EQU   *
         BR    R7                * Return to mainline
*
* Read parameters
*
PARMS    DS    0H
         L     R1,PARML_PTR       * Get pointer to parameter list
         L     R1,0(,R1)          * Get pointer to first parameter
         N     R1,=X'7FFFFFFF'    * Get rid of high order bit
*
         XR    R3,R3              * Clear R3
         LH    R3,0(,R1)          * Get JCL parameter length
         LTR   R3,R3              * Check for zero
         BZ    PARMS_RET          * Zero length parameter
         LA    R2,2(,R1)          * Point R2 to parameter
*
         LA    R4,1(,R3)          * Add 1 to length for zero term
         ST    R4,G_GTSTSIZ       * Save as variable
*
         LA    R1,WORK
         MVC   0(4,R1),G_GTSTSIZ
         LA    R15,parameterCopy
         ST    R15,4(,R1)
         L     R15,G_GTST
         BASR  R14,R15
*
         LR    R4,R2
         LR    R5,R3
         L     R14,parameterCopy
         LR    R15,R5
         MVCL  R14,R4
         MVI   0(R14),X'00'
*
*        Instantiate a new IINS object
         MINSTANT GUID=G_IINS_GUID,WORK=WORK,OBJPTR=IINS_PARMS
*
*        Assign parameter string as input
         IINS_Init OBJECT=IINS_PARMS,WORK=WORK,ZSTR=parameterCopy,     X
               LINENO==A(1)
*
*        Instantiate a new IIFO object
         MINSTANT GUID=G_IIFO_GUID,WORK=WORK,OBJPTR=IIFO_PARMS
*
*        Instantiate a new IPRS object
         MINSTANT GUID=G_IPRS_GUID,WORK=WORK,OBJPTR=IPRS_PARMS
*
*        Instantiate a new IPSS object
         MINSTANT GUID=G_IPSS_GUID,WORK=WORK,OBJPTR=IPSS_PARMS
*
*        Instantiate a new ITOK object
         MINSTANT GUID=G_ITOK_GUID,WORK=WORK,OBJPTR=ITOK_PARMS
*
         ITOK_Init OBJECT=ITOK_PARMS,WORK=WORK,IIN=IINS_PARMS,         X
               IIFO=IIFO_PARMS
*
         IPSS_Push OBJECT=IPSS_PARMS,WORK=WORK,                        X
               STATE_IN==A(PARSER_STATE_IN_PARAMETER)
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   PARMS_RET         * skip the rest
*
         L     R6,IIFO_PARMS
         DO UNTIL=(CLC,G_RETCODE,NE,=A(0),OR,                          X
               CLI,IFO_bEOF-IFO_obj(R6),EQ,C'Y')
*
            IPRS_ParseParameter OBJECT=IPRS_PARMS,WORK=WORK,           X
               IPSS=IPSS_PARMS,ITOK=ITOK_PARMS
*
         ENDDO
*
         CLC   G_RETCODE,=A(0)   * If something wrong
         BNE   PARMS_RET         * skip the rest
*
         ITOK_Release OBJECT=ITOK_PARMS,WORK=WORK
         MVC   ITOK_PARMS,=A(0)
*
         IPSS_Release OBJECT=IPSS_PARMS,WORK=WORK
         MVC   IPSS_PARMS,=A(0)
*
         IPRS_Release OBJECT=IPRS_PARMS,WORK=WORK
         MVC   IPRS_PARMS,=A(0)
*
         IIFO_Release OBJECT=IIFO_PARMS,WORK=WORK
         MVC   IIFO_PARMS,=A(0)
*
         IINS_Release OBJECT=IINS_PARMS,WORK=WORK
         MVC   IINS_PARMS,=A(0)
*
PARMS_RET EQU   *
         BR    R7
*
* Determine makefile file type
*
FILETYPE DS    0H
         MVI   datasetType,C'D'
*
         IFMG_DDNameToDSName OBJECT=G_IFMG,WORK=WORK,                  X
               DDNAME==CL8'LWZMINP',DSNAME=datasetName
*
         IF (CLC,datasetName(3),EQ,=C'...') THEN
            MVI   datasetType,C'F'
         ENDIF
*
FILETYPE_RET EQU   *
         BR    R7
*
* End of code
*
         LTORG
*
                             DS    0F
MAK301I                      DC    C'MAK301I Start',X'00'
                             DS    0F
MAK302I                      DC    C'MAK302I Done',X'00'
*
                             DS    0F
LWZMGTSTA                    DC    A(LWZMGTST)
*
* List form of LINK macro area
                             DS    0F
LINKL                        LINK  SF=L
LINK_SIZ                     EQU   *-LINKL
*
* List form of LOAD macro area
                             DS    0F
LOADL                        LOAD  SF=L
LOAD_SIZ                     EQU   *-LOADL
*
* List form of OPEN macro area
                             DS    0F
OPENL                        OPEN  (,),MODE=31,MF=L
OPEN_SIZ                     EQU   *-OPENL
*
* List form of CLOSE macro area
                             DS    0F
CLOSEL                       CLOSE (),MODE=31,MF=L
CLOSE_SIZ                    EQU   *-CLOSEL
*
* List form of CONVTOD macro area
CONVTODL                     CONVTOD MF=L
CONVTOD_SIZ                  EQU   *-CONVTODL
*
STCKCONVL                    STCKCONV MF=L
STCKCONV_SIZ                 EQU   *-STCKCONVL
*
SWAREQL                      SWAREQ MF=L
SWAREQ_SIZ                   EQU   *-SWAREQL
*
GETDSABL                     GETDSAB MF=(L,GETDSABL)
GETDSAB_SIZ                  EQU   *-GETDSABL
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
PARML_PTR                    DS    A   * Ptr to parm list
GLOBAL_PTR                   DS    A   * Ptr to global area
FC                           DS    3A  * Feedback code
WORK                         DS    4A  * Work area
*
IINS_PARMS                   DS    A   * Input from parameter string
IIFO_PARMS                   DS    A   * InputInfo for IINS_PARMS
IPRS_PARMS                   DS    A   * Parser for parameters
IPSS_PARMS                   DS    A   * ParserState for parameters
ITOK_PARMS                   DS    A   * Tokenizer for makefile
*
IIND_MAIN                    DS    A   * InputFromDataSet
                             ORG   IIND_MAIN
IINF_MAIN                    DS    A   * InputFromFileSet
                             ORG   *
IIFO_MAIN                    DS    A   * InputInfo for IIN%_MAIN
IPRS_MAIN                    DS    A   * Parser for makefile
IPSS_MAIN                    DS    A   * ParserState for makefile
ITOK_MAIN                    DS    A   * Tokenizer for makefile
*
IAV2_buildTargets            DS    A   * ArrayList of target names
*
parameterCopy                DS    A
*
datasetType                  DS    C
datasetName                  DS    CL44
*
WORKDSA_SIZ                  EQU   *-WORKDSA
*
HEAPSEG                      DSECT
*
HEAPSEG_SIZ                  DS    F
HEAPSEG_LEFT                 DS    F
HEAPSEG_PREV                 DS    F
HEAPSEG_NEXT                 DS    F
*
HEAPSEGDS_SIZ                EQU   *-HEAPSEG
*
         COPY  DSCOMOBJ          * Generic COM obj DSECT
*
         COPY  DSIFO             * IIFO obj DSECT
*
         COPY  VARIANT           * DSECT for typed value
*
         MGLOBAL                 * Global area DSECT
*
*        CVT DSECT=YES,LIST=YES
*
*        IEFJESCT
*
*        IEFZB505 LOCEPAX=YES
*
*        IHADSAB
*
LWZMAKE  CSECT
*
         DROP
*
LWZMGTST CEEENTRY AUTO=WORKDSAGTST_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAGTST,R13   * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R8,0(,R1)         * Parm 1 is requested size
*
         L     R7,4(,R1)         * Parm 2 is ptr to output ptr
         MVC   0(4,R7),=A(0)     * Initialize to zeros
*
         LA    R8,3(,R8)
         NILL  R8,X'FFFC'
*
         L     R6,G_HEAP
         USING HEAPSEG,R6
*
         DO WHILE=(C,R8,GT,HEAPSEG_LEFT)
            IF (CLC,HEAPSEG_NEXT,NE,=A(0)) THEN
               L     R6,HEAPSEG_NEXT
            ELSE
               LR    R5,R8
               AFI   R5,4095+HEAPSEGDS_SIZ
               NILL  R5,X'F000'
               ST    R5,GTSTSIZ
*
               CALL  CEEGTST,(=A(0),GTSTSIZ,HEAPSEG_NEXT,FCGTST),      X
               MF=(E,WORKGTST)
*
               IF (CLC,FCGTST(8),NE,=XL8'0000000000000000') THEN
                  CALL  CEE3ABD,(=A(1002),=A(3)),MF=(E,WORKGTST)
               ENDIF
*
               L     R4,HEAPSEG_NEXT
               L     R5,GTSTSIZ
               XR    R0,R0
               XR    R1,R1
               MVCL  R4,R0
*
               L     R4,HEAPSEG_NEXT
               L     R5,GTSTSIZ
               ST    R5,HEAPSEG_SIZ-HEAPSEG(,R4)
               S     R5,=A(HEAPSEGDS_SIZ)
               ST    R5,HEAPSEG_LEFT-HEAPSEG(,R4)
               ST    R6,HEAPSEG_PREV-HEAPSEG(,R4)
*
               L     R6,HEAPSEG_NEXT
            ENDIF
         ENDDO
*
         LR    R5,R6
         L     R4,HEAPSEG_LEFT
*
         A     R5,HEAPSEG_SIZ
         SR    R5,R4
         ST    R5,0(,R7)
*
         SR    R4,R8
         ST    R4,HEAPSEG_LEFT
*
LWZMGTST_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAGTST                  DSECT
*
                             ORG   *+CEEDSASZ
*
FCGTST                       DS    3A
WORKGTST                     DS    4A
GTSTSIZ                      DS    F
*
WORKDSAGTST_SIZ              EQU   *-WORKDSAGTST
*
LWZMAKE  CSECT
*
         DROP
*
TRT_ANY_BUT_SPACE            DS    0AD
                             DC    (C' ')X'00'
                             DC    X'FF'
                             DC    (255-C' '-1)X'00'
*
TRT_ONLY_SPACE               DS    0AD
                             DC    (C' ')X'FF'
                             DC    X'00'
                             DC    (255-C' '-1)X'FF'
*
TRT_ONLY_ZEROS               DS    0AD
                             DC    (C'0')X'FF'
                             DC    X'00'
                             DC    (255-C'0'-1)X'FF'
*
HEXTAB                       DS    0AD
                             DC    (C'0')X'00'
                             DC    C'0123456789ABCDEF'
*
         COPY  REGS              * Register equates
*
         END   LWZMAKE
