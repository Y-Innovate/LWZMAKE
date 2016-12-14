         TITLE 'LWZMAKE'
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
*                                                                     *
* A zmake makefile is processed in 2 phases.                          *
* Phase 1 parses the input and creates a linked list of statements    *
* in an internal format. While doing so, variable assignments with := *
* are resolved then, assignments with = are left to be resolved in    *
* phase 2. Similar for targets, whatever's left of : in a rule is     *
* resolved then, the rest is left to be resolved in phase 2.          *
* Phase 2 starts with the first target and for each dependency it     *
* recursively goes through any target it finds for those dependencies.*
* When all dependency targets are processed and LWZMAKE determines a  *
* target should be built, the recipe is executed.                     *
*                                                                     *
* As stated above, during both phases variable substitution takes     *
* place. This is also done recursively, in other words when a variable*
* is resolved to a value that also contains a variable, LWZMAKE keeps *
* resolving until no variables are found.                             *
*                                                                     *
* For remembering variables and their values an ordered linked list   *
* is used.                                                            *
*                                                                     *
* Parsing is done with a scanner section that reads the makefile      *
* character by character. This section is invoked by a tokenizer or   *
* lexer section that strings the scanned characters into tokens. This *
* section in turn is invoked by a statement parsing section that      *
* analyzes tokens that form statements, converts the statements into  *
* an in-memory internal format and saves the statements in a linked   *
* list.                                                               *
* For the tokenizer and parser to 'know' what is allowed at any given *
* point a 'state machine' is used. This is basically a stack of state *
* bytes. Every state byte corresponds with a fullword with bit        *
* positions that indicate whether a token is allowed or not.          *
*                                                                     *
* The input to the scanner (and thus also to the tokenizer and parser)*
* is also stacked. Initially this stack has one entry which represents*
* the makefile which was taken from the JCL. Next entries are pushed  *
* onto the input stack when for example the next characters need to   *
* temporarily be taken from a variable's value. After the variable's  *
* value is exhausted, the stack is popped and the scanner continues   *
* with the makefile input.                                            *
*                                                                     *
* =================================================================== *
* Module specifications:                                              *
* ------------------------------------------------------------------- *
* - Fully re-entrant                                                  *
* - AMODE 31                                                          *
* - RMODE any                                                         *
*                                                                     *
* =================================================================== *
* Required DD names:                                                  *
* ------------------------------------------------------------------- *
* - SYSEXEC   for finding REXX members                                *
* - SYSTSPRT  for REXX SAY output                                     *
* - LWZMRPT   for execution report                                    *
* - LWZMTRC   for trace output                                        *
* - MAKEFILE  input makefile script                                   *
*                                                                     *
* =================================================================== *
* Source conventions:                                                 *
* ------------------------------------------------------------------- *
* Basically everything's in this single source.                       *
* Major sections use standard MVS savearea chaining, within such      *
* sections, smaller subroutines are BAL'd to with R8.                 *
* Code is not LE-conforming (so no CEEENTRY - CEETERM), but does      *
* adhere to LE-enabled register conventions.                          *
*                                                                     *
* =================================================================== *
* Register conventions:                                               *
* ------------------------------------------------------------------- *
* R0   work reg                                                       *
* R1   work reg, parameter list ptr on entry, used as parameter list  *
*        ptr to call external routines                                *
* R2-7 work reg (sometimes R7 is also return address in BAL)          *
* R8   return address in BAL to subroutine                            *
* R9   USING GLOBAL_DATA_DSECT for 'global' data                      *
* R10  first base register                                            *
* R11  second base register                                           *
* R12  not used in order to be LE enabled                             *
* R13  points to save area and working storage                        *
* R14  standard linkage convention return address                     *
* R15  standard linkage convention entry address + return value       *
*                                                                     *
***********************************************************************
*
         COPY  ASMMSP             * Enable HLASM struct.prog.macro's
*
* Macro MLWZMTRC for generating a snippet to write a trace record
*
         MACRO
         MLWZMTRC &LEVEL=,&MSGNR=,&CONST=,&DATA
         LCLA  &CONST_LEN
         IF (CLI,G_LWZMAKE_TRACE,NL,&LEVEL) THEN
.* Check for constant data provided
         AIF   ('&CONST' EQ '').NOCONST
            LA    R15,=&CONST
            ST    R15,G_LWZMTRC_DATA_PTR
&CONST_LEN  SETA  K'&CONST-3
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(&CONST_LEN)
         AGO   .WITHDATA
.NOCONST ANOP
.* Check for presence of DATA keyword, if not present clear data size
         AIF   ('&DATA' NE '').WITHDATA
            XC    G_LWZMTRC_DATA_SIZ,G_LWZMTRC_DATA_SIZ * data siz = 0
.WITHDATA ANOP
            MVC   G_LWZMTRC_MSGNR,=&MSGNR * Copy msg number to glb var
            L     R15,G_LWZMAKE_TRACEA  * Get address of trace section
            BASR  R14,R15               * Link to trace section
         ENDIF
         MEND
*
* Macro MLWZMRPT for generating a snippet to write a report line
*
         MACRO
         MLWZMRPT &RPTLINE=,&APND_LC=
         MVC   G_LWZMRPT_LINE,=&RPTLINE * Copy report line to glb var
         AIF   ('&APND_LC' EQ '').NO_APND_LC
         MVI   G_LWZMRPT_APND_LC,&APND_LC
.NO_APND_LC ANOP
         L     R15,G_LWZMAKE_RPTA * Get address of report section
         BASR  R14,R15            * Link to report section
         MEND
*
* Macro MLWZSAVE for generating simple start of section
*
         MACRO
&NAME    MLWZSAVE
*
* Make sure we start without any usings
         DROP
*
&NAME    DS    0F
         STM   R14,R12,12(R13)    * Save callers registers
         LR    R10,R15            * Setup R11 as base register
         LA    R11,4095(,R10)     * Setup R10 as second
         LA    R11,1(,R11)        *   base register
         USING &NAME,R10,R11      * Establish addressing
         LR    R2,R1              * Save possible parameter ptr
         GETMAIN RU,LV=72         * Get storage for SA
         XC    0(2,R1),0(R1)      * Clear first 2 bytes of SA
         ST    R13,4(R1)          * Backward chain callers SA
         ST    R1,8(R13)          * Forward chain my SA
         LR    R13,R1             * Point R13 to my SA
         LR    R1,R2              * Restore parameter list ptr in R1
         USING GLOBAL_DATA_DSECT,R9 * Establish addressing of glb data
         MEND
*
* Macro MLWZTERM for generating simple end of section
*
         MACRO
         MLWZTERM
         L     R3,4(,R13)         * Save address of callers SA
         FREEMAIN RU,LV=72,A=(R13) * Free storage for SA
         LR    R13,R3             * Restore address of callers SA
         LM    R14,R12,12(R13)    * Restore reg 14 through 12
         BR    R14                * Return to caller
         MEND
***********************************************************************
* Section: LWZMAKE                                                    *
* Purpose: Main section                                               *
***********************************************************************
LWZMAKE  AMODE 31
LWZMAKE  RMODE ANY
LWZMAKE  CSECT
         J     LWZMAKE_ENTRY      * Jump past name/date/time/version
         DC    CL8'LWZMAKE'       * Module name
         DC    CL8'&SYSDATE'      * Assembly date
         DC    CL8'&SYSTIME'      * Assembly time
LWZMAKE_ENTRY DS    0H
         STM   R14,R12,12(R13)    * Save callers registers
         LARL  R10,LWZMAKE_ENTRY  * Set R10 to entry point
         LA    R11,4095(,R10)     * Setup R11 as second using
         LA    R11,1(,R11)        *   base register
         USING LWZMAKE_ENTRY,R10,R11 * Establish addressing
         LR    R2,R1              * Save possible parameter ptr
         GETMAIN RU,LV=WORKAREA_SIZ * Get storage for SA+work stg+glb
         XC    0(2,R1),0(R1)      * Clear first 2 bytes of SA
         ST    R13,4(R1)          * Backward chain callers SA
         ST    R1,8(R13)          * Forward chain my SA
         LR    R13,R1             * Point R13 to my SA
         USING WORKAREA,R13       * Establish addressing of workarea
         LR    R1,R2              * Restore parameter list ptr in R1

         ST    R1,PARML_PTR       * Save address of parameter ptrs
*
         LA    R9,GLOBAL_DATA     * Global storage was GM'd along with
*                                 * SA and working storage,
         USING GLOBAL_DATA_DSECT,R9 * Establish address of global data
*
         BAL   R8,INIT            * Perform initializations, LOAD
*                                 * external modules, OPEN DCB's
*
         LT    R15,G_RETCODE      * Check for error
         BNZ   RET                * If error then skip to end
*
         BAL   R8,PARMS           * Perform parsing parameters
*
         LT    R15,G_RETCODE      * Check for error
         BNZ   RET                * If error then skip to end
*
         L     R15,LWZMAKE_PHASE1A * Get address of phase 1 section
         BASR  R14,R15            * Link to phase 1 section
*
         LT    R15,G_RETCODE      * Check for error
         BNZ   RET                * If error thenk skip to end
*
         L     R15,LWZMAKE_PHASE2A * Get address of phase 2 section
         BASR  R14,R15            * Link to phase 2 section
*
RET      EQU   *
*
         BAL   R8,WRAPUP          * Perform wrapup
*
         L     R2,G_RETCODE       * Save returncode
         L     R3,4(,R13)         * Save address of callers SA
         FREEMAIN RU,LV=WORKAREA_SIZ,A=(R13) * Free workarea
         L     R0,=A(0)           * Load modifier (is an LE thing)
         LR    R13,R3             * Restore address of callers SA
         LR    R15,R2             * Set returncode
         L     R14,12(,R13)       * Reload return address
         LM    R1,R12,24(R13)     * Restore reg 1 through 12
         BR    R14                * Return to caller
*
* Initializations
*
INIT     EQU   *
*        Set trace level, for a different trace level change here
         MVI   G_LWZMAKE_TRACE,LWZMAKE_TRACE_WARNING
*
         MVC   G_RETCODE,=F'0'    * Initial return code 0
*
         MVI   TRCOPEN,C'N'       * Flags for open DCB's trace,
         MVI   RPTOPEN,C'N'       *                      report
         MVI   MKFOPEN,C'N'       *                  and makefile
*
         MVI   G_MKFEOF,C'N'      * Preset EOF for makefile to N
*
         MVI   G_USE_ISPEXEC,C' ' * Preset use ISPEXEC to undetermined
*
         MVI   G_RECIPEPREFIX,X'05' * Default recipe prefix is tab
*
         MVI   G_PREV_STMT_TYPE,X'00' * Initial no prev statement type
*
         MVI   G_SCAN_CLOSE_BRACKET,X'00' * Set no close bracket check
*
         MVC   G_DCB_MEM_PTR,=A(0) * Clear DCB memory pointer
*
         MVC   G_STMT_LIST_PTR,=A(0) * Clear linked list pointers
         MVC   G_FIRST_VAR_PTR,=A(0)
         MVC   G_FIRST_TGT_PTR,=A(0)
         MVC   G_FIRST_PNY_PTR,=A(0)
*
         MVI   G_DEFAULT_TARGET,C' ' * Clear default target
         MVC   G_DEFAULT_TARGET+1(L'G_DEFAULT_TARGET-1),G_DEFAULT_TARGEX
               T
*
         MVC   G_SCAN_CURRCOL,=F'0'   '
         MVC   G_SCAN_CURRLINE,=F'0'     * so current line goes 0 > 1
         MVI   G_SCAN_CONTINUED_LINE,C'N' * Initial no continued line
         MVI   G_SCAN_APPEND_TO,X'00'    * Initial append to scratch
*                                        * token (1)
*
         MVI   G_SCAN_STATE_STACK,X'00'
         MVC   G_SCAN_STATE_STACK+1(L'G_SCAN_STATE_STACK-1),G_SCAN_STATX
               E_STACK
*                                 * Clear scan state stack index
         MVI   G_SCAN_STATE_STACK_IDX,X'00'
*
*                                 * Clear scan input stack
         MVI   G_SCAN_INPUT_STACK,X'00'
         MVC   G_SCAN_INPUT_STACK+1(L'G_SCAN_INPUT_STACK-1),G_SCAN_INPUX
               T_STACK
*                                 * Set initial inputstack
         MVI   G_SCAN_INPUT_STACK_IDX,X'01'
*
         MVC   G_LWZMRPT_CURRLINE,=H'999' * Make sure first report line
*                                         * causes next line
         MVC   G_LWZMRPT_CURRPAGE,=F'0'   * so current page goes 0 > 1
         MVC   G_LWZMRPT_APND_LC,=C'N'    * Initial no append line/col
*
*                                         * Initialize page hader in
         MVC   G_PAGE_HEADER,CPAGE_HEADER * global var
*
         MVC   LOADD,LOADL        * Copy list form of LOAD macro
*
         MVC   G_LINKD,LINKL      * Copy list form of LINK macro
*
         MVC   G_OPEND,OPENL      * Copy list form of LOAD macro
*
*        Save addresses of sections in global vars
         MVC   G_LWZMAKE_TRACEA,LWZMAKE_TRACEA
         MVC   G_LWZMAKE_RPTA,LWZMAKE_RPTA
*
*        Clear REXX environment block pointer
         MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)
*
*                                 * GM DCB storage below the line
         GETMAIN RU,LV=DCB_DSECT_SIZ,LOC=24
         ST    R1,G_DCB_MEM_PTR   * and save in global var
*
*        Overwrite DCB's with hard coded ones (for re-entrancy)
         MVC   DCBTRC-DCB_DSECT(LEN_DCBTRC,R1),CDCBTRC
         MVC   DCBRPT-DCB_DSECT(LEN_DCBRPT,R1),CDCBRPT
         MVC   DCBMKF-DCB_DSECT(LEN_DCBMKF,R1),CDCBMKF
         MVC   DCBEMKF-DCB_DSECT(LEN_DCBEMKF,R1),CDCBEMKF
*
         LA    R7,DCBMKF-DCB_DSECT(,R1)  * Point R7 to makefile DCB
         LA    R6,DCBEMKF-DCB_DSECT(,R1) * Point R6 to makefile DCBE
         ST    R6,DCBDCBE-IHADCB(,R7)    * Store ptr to DCBE in DCB
         LA    R1,MAKEFILE_IS_EOF        * Get address of EODAD routine
         ST    R1,DCBEEODA-DCBE(,R6)     * Store address in DCBE
*
*                                       * Load catalog search interface
         LOAD  EP=IGGCSI00,SF=(E,LOADD) * entry point IGGCSI00
*        No error handling, missing module will cause S806 abend
*
         ST    R0,G_IGGCSI00A     * and store in global var
*
*                                       * Load binder fast data access
         LOAD  EP=IEWBFDAT,SF=(E,LOADD) * entry point IEWBFDAT
*        No error handling, missing module will cause S806 abend
*
         ST    R0,G_IEWBFDATA     * and store in global var
*
*        OPEN the DCB's
*
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer
         LA    R2,DCBTRC-DCB_DSECT(,R14) * Get address of LWZMTRC DCB
*                                 * and open for output
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,G_OPEND)
         LTR   R15,R15            * Check for returned 0
         IF (NZ) THEN             * non-zero means open failed
            CVD   R15,G_DEC8      * convert return value to packed
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned
            OI    G_ZONED8+7,X'F0' * get rid of sign
*
*           No other way of informing about error than doing a WTO
            MVC   G_WTOLEN,=H'26'
            MVC   G_WTOFIL,=H'0'
            MVC   G_WTOTEXT(20),=C'OPEN LWZMTRC FAILED '
            MVC   G_WTOTEXT+20(2),G_ZONED8+6
            WTO   MF=(E,G_WTOBLOCK),ROUTCDE=11,DESC=7
*
            MVC   G_RETCODE,=A(12) * Set return code to 12
            B     INIT_RET        * and skip rest of INIT
         ENDIF
         MVI   TRCOPEN,C'Y'       * Remember to close trace DCB
*        Trace record LWZMAKE start
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'601'
*
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer
         LA    R2,DCBRPT-DCB_DSECT(,R14) * Get address of LWZMRPT DCB
*                                 * and open for output
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,G_OPEND)
         LTR   R15,R15            * Check for returned 0
         IF (NZ) THEN             * non-zero means open failed
            CVD   R15,G_DEC8      * convert return value to packed
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned
            OI    G_ZONED8+7,X'F0' * get rid of sign
            MVC   G_HELPER_DATA(2),G_ZONED8+6 * Copy to helper data
            LA    R14,G_HELPER_DATA  * Get ptr to helper data
            ST    R14,G_LWZMTRC_DATA_PTR * Save trace data ptr
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(2) * Set trace data size
*           Trace record LWZMRPT DCB OPEN failed
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'001',DATA
            MVC   G_RETCODE,=F'12' * Set return code to 12
            B     INIT_RET        * and skip rest of INIT
         ENDIF
         MVI   RPTOPEN,C'Y'       * Remember to close report DCB
*        Trace record LWZMRPT DCB open
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'602',CONST=C'LWZMRPTX
               '
*        Report line LWZMAKE start
         MLWZMRPT RPTLINE=CL133' LWZMAKE start'
*
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer
         LA    R2,DCBMKF-DCB_DSECT(,R14) * Get address of MAKEFILE DCB
*                                 * and open for input
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)
         LTR   R15,R15            * Check for returned 0
         IF (NZ) THEN             * non-zero means open failed
            CVD   R15,G_DEC8      * convert return value to packed
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned
            OI    G_ZONED8+7,X'F0' * get rid of sign
            MVC   G_HELPER_DATA(2),G_ZONED8+6 * Copy to helper data
            LA    R14,G_HELPER_DATA  * Get ptr to helper data
            ST    R14,G_LWZMTRC_DATA_PTR * Save trace data ptr
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(2) * Set trace data size
*           Trace record MAKEFILE DCB OPEN failed
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'002',DATA
            MVC   G_LWZMRPT_LINE,=CL133'0Error opening MAKEFILE'
            MVC   G_LWZMRPT_LINE+24(2),G_ZONED8+6
            L     R15,G_LWZMAKE_RPTA * Get address of report section
            BASR  R14,R15          * Link to report section
            MVC   G_RETCODE,=F'12' * Set return code to 12
            B     INIT_RET        * and skip rest of INIT
         ENDIF
         MVI   MKFOPEN,C'Y'       * Remember to close MAKEFILE DCB
*        Put DCB ptr in first entry of INPUT_STACK
         LA    R3,G_SCAN_INPUT_STACK
         ST    R2,INPUTPTR-INPUT_DSECT(,R3)
*        Trace record MAKEFILE DCB open
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'602',CONST=C'MAKEFILX
               E'
*        Report line MAKEFILE opened
         MLWZMRPT RPTLINE=CL133' MAKEFILE DD opened'
*
*        Retrieve submitter user id
         IAZXJSAB READ,USERID=G_USERID
*
*        Allocate multi-purpose tokens
         MVC   G_SCAN_TOKEN_MAXLEN,=A(SCAN_TOKEN_MAXLEN)
         MVC   G_STGOR_LEN,G_SCAN_TOKEN_MAXLEN
         MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
         L     R15,LWZMAKE_STG_OBTAINA
         BASR  R14,R15
         MVC   G_SCAN_TOKENA,G_STGOR_PTR
*
         MVC   G_SCAN_TOKEN2_MAXLEN,=A(SCAN_TOKEN_MAXLEN)
         MVC   G_STGOR_LEN,G_SCAN_TOKEN2_MAXLEN
         MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
         L     R15,LWZMAKE_STG_OBTAINA
         BASR  R14,R15
         MVC   G_SCAN_TOKEN2A,G_STGOR_PTR
*
         MVC   G_SCAN_TOKEN3_MAXLEN,=A(SCAN_TOKEN_MAXLEN)
         MVC   G_STGOR_LEN,G_SCAN_TOKEN3_MAXLEN
         MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
         L     R15,LWZMAKE_STG_OBTAINA
         BASR  R14,R15
         MVC   G_SCAN_TOKEN3A,G_STGOR_PTR
*
*        Allocate evaluation block
         LA    R3,272
         ST    R3,G_EVALBLOCK_MAXLEN
         ST    R3,G_STGOR_LEN
         MVI   G_STGOR_TYPE,STGOR_TYPE_EVAL
         L     R15,LWZMAKE_STG_OBTAINA
         BASR  R14,R15
         MVC   G_EVALBLOCK_PTR,G_STGOR_PTR
*
INIT_RET EQU   *                  * INIT done
         BR    R8                 * Return
*
* Parse parameters
*
PARMS    EQU   *
         LT    R1,PARML_PTR       * Retrieve parameter list pointer
         IF (Z) THEN              * Empty pointer?
NO_PARAMETER EQU *
            MLWZMRPT RPTLINE=CL133' No parameter received'
            B     PARMS_DONE
         ENDIF
*
         L     R1,0(,R1)          * Get pointer to first parameter
         N     R1,=X'7FFFFFFF'    * Get rid of high order bit
*
         XR    R3,R3              * Clear R3
         LH    R3,0(,R1)          * Get JCL parameter length
         LTR   R3,R3              * Check for zero
         BZ    NO_PARAMETER       * Zero length parameter
         LA    R2,2(,R1)          * Point R2 to parameter
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_INFO) THEN
            LR    R4,R3
            C     R4,=A(L'G_HELPER_DATA)
            IF (H) THEN
               L     R4,=A(L'G_HELPER_DATA)
            ENDIF
            BCTR  R4,R0
            B     *+10
            MVC   G_HELPER_DATA(1),0(R2)
            EX    R4,*-6
            LA    R4,1(,R4)
            ST    R2,G_LWZMTRC_DATA_PTR
            STH   R4,G_LWZMTRC_DATA_SIZ
            MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'606',DATA
         ENDIF
*
*        Split up parts of the parameter
*
*        Start by putting parameter in input stack
         XR    R4,R4              * Clear R4
         XR    R5,R5              * Clear R5
         IC    R5,G_SCAN_INPUT_STACK_IDX * Get current stack index
         C     R5,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra
*                                 * entry fit?
         IF (NL) THEN             * If not write error
            MLWZMRPT RPTLINE=CL133'0Internal error, state stack overfloX
               w'
            MVC   G_RETCODE,=F'12' * Set return code 12
            B     PARMS_RET        * and return
         ENDIF
         LA    R5,1(,R5)          * Add 1 to stack size
         STC   R5,G_SCAN_INPUT_STACK_IDX * And store it
         BCTR  R5,R0              * Subtract 1 to calculate offset
         M     R4,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry
         LA    R4,G_SCAN_INPUT_STACK * Point R4 to input stack
         AR    R4,R5              * Add calculated offset
*
         USING INPUT_DSECT,R4     * Address with INPUT DSECT
*
         MVC   INPUTLEAD,=H'0'    * Clear leading spaces
         MVI   INPUTTYPE,INPUTTYPE_STRPTR_EOF * Set type of input
         STH   R3,INPUTLEN        * Copy value length
         ST    R2,INPUTPTR        * Copy value pointer
         MVC   INPUTPOS,=H'0'     * Set initial scan position to start
*
         MVI   G_LWZMRPT_LINE,C' '
         MVC   G_LWZMRPT_LINE+1(L'G_LWZMRPT_LINE-1),G_LWZMRPT_LINE
         MVC   G_LWZMRPT_LINE(22),=C' Parameter received...'
         IF (C,R3,GT,=F'110') THEN
            L     R3,=F'110'
         ENDIF
         BCTR  R3,R0
         B     *+10
         MVC   G_LWZMRPT_LINE+23(1),0(R2)
         EX    R3,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         DROP  R4
*
         L     R15,LWZMAKE_SCAN_TOKENA * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
NEXT_PARMS_ROUND EQU *
         MVI   G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT
*
         IF (CLI,G_MKFEOF,EQ,C'Y') THEN
            MVI   G_MKFEOF,C'N'
            B     PARMS_DONE
         ENDIF
*
         CLC   G_SCAN_TOKEN_LEN,=F'1' * Any valid parameter starts with
         BNE   PARMS_ERROR        * a '-' switch which has to be 1 long
*
         L     R14,G_SCAN_TOKENA  * Point R14 to token 1
         IF (CLI,0(R14),EQ,C'-') THEN * Check if it's a '-'
            L     R15,LWZMAKE_SCAN_TOKENA * Get address of SCAN_TOKEN
            BASR  R14,R15         * Link to SCAN_TOKEN section
*
            CLC   G_SCAN_TOKEN_LEN,=F'1' * Switch has to be 1 char
            BNE   PARMS_ERROR     * If not, then error
            CLC   G_SCAN_SPACE_COUNT,=F'0' * No space after '-'
            BNE   PARMS_ERROR     * If not, then error
*
            L     R14,G_SCAN_TOKENA * Point R14 to token 1
            IF (CLI,0(R14),EQ,C't') THEN * Check for target sw?
               OI    G_SCAN_STATE,SCAN_STATE_IN_PARMTARGET
*
               L     R15,LWZMAKE_SCAN_TOKENA * Get address SCAN_TOKEN
               BASR  R14,R15      * Link to SCAN_TOKEN section
*
               CLC   G_SCAN_TOKEN_LEN,=F'0'
               BE    PARMS_ERROR
*
               CLC   G_SCAN_SPACE_COUNT,=F'0'
               BNH   PARMS_ERROR
*
               MVC   G_SCAN_SPACE_COUNT,=F'0'
*
               MVI   G_SCAN_APPEND_TO,X'01'
               L     R15,LWZMAKE_APPEND_TOKENA
               BASR  R14,R15
*
NEXT_PARMS_TARGET_TOKEN EQU *
               L     R15,LWZMAKE_SCAN_TOKENA * Get address SCAN_TOKEN
               BASR  R14,R15      * Link to SCAN_TOKEN section
*
               CLC   G_SCAN_TOKEN_LEN,=F'0'
               BE    PARMS_TARGET_SET
*
               CLC   G_SCAN_SPACE_COUNT,=F'0'
               BH    PARMS_TARGET_SET
*
               MVI   G_SCAN_APPEND_TO,X'01'
               L     R15,LWZMAKE_APPEND_TOKENA
               BASR  R14,R15
*
               B     NEXT_PARMS_TARGET_TOKEN
*
PARMS_TARGET_SET EQU *
               LA    R2,G_DEFAULT_TARGET * Point R2 to default tgt
               L     R3,G_SCAN_TOKEN2A   * Point R3 to token 2
               L     R4,G_SCAN_TOKEN2_LEN * Get length of target in 2
               BCTR  R4,R0         * R4 = R4 - 1 for EX
               B     *+10          * Skip MVC constant for EX
               MVC   0(1,R2),0(R3) * MVC constant for EX
               EX    R4,*-6        * EX previous MVC stmt with R4
*
               B     NEXT_PARMS_ROUND
            ELSE                  * Else, it's not a -t switch
               B     PARMS_ERROR  * means error
            ENDIF
         ELSE                     * Else, it's not a '-' switch char
            B     PARMS_ERROR     * means error
         ENDIF
*
PARMS_DONE EQU  *
         MVC   G_SCAN_CURRCOL,=F'999'    * Make sure first scanned char
*                                        * causes read record
         MVC   G_SCAN_INPUT_STACK+(INPUTPOS-INPUT_DSECT)(2),=H'999'
PARMS_RET EQU  *
         BR    R8                 * Return
*
PARMS_ERROR EQU *
         MLWZMRPT RPTLINE=CL133'0Wrong parameter received'
         MVC   G_RETCODE,=F'8'
         BR    R8
*
* Wrap things up
*
WRAPUP   EQU   *
         LT    R3,G_STMT_LIST_PTR * Get first stmt in list
         DO WHILE=(NZ)
            L     R4,STMT_NEXT_PTR-STMT_DSECT(,R3) * Save ptr next stmt
            ST    R3,G_STGOR_PTR
            MVC   G_STGOR_LEN,STMT_LEN-STMT_DSECT(R3)
            MVI   G_STGOR_TYPE,STGOR_TYPE_STMT
            L     R15,LWZMAKE_STG_RELEASEA
            BASR  R14,R15
            LTR   R3,R4           * Test pointer to next statement
         ENDDO
         MVC   G_STMT_LIST_PTR,=A(0) * Clear first block ptr
*
         LT    R1,G_FIRST_VAR_PTR
         IF (NZ) THEN
            MVI   G_STGOR_TYPE,STGOR_TYPE_VAR
            L     R15,LWZMAKE_FREEBSTA
            BASR  R14,R15
         ENDIF
         MVC   G_FIRST_VAR_PTR,=A(0)
*
         LT    R1,G_FIRST_TGT_PTR
         IF (NZ) THEN
            MVI   G_STGOR_TYPE,STGOR_TYPE_TGT
            L     R15,LWZMAKE_FREEBSTA
            BASR  R14,R15
         ENDIF
         MVC   G_FIRST_TGT_PTR,=A(0)
*
         LT    R1,G_FIRST_PNY_PTR
         IF (NZ) THEN
            MVI   G_STGOR_TYPE,STGOR_TYPE_PNY
            L     R15,LWZMAKE_FREEBSTA
            BASR  R14,R15
         ENDIF
         MVC   G_FIRST_PNY_PTR,=A(0)
*
*        Free multi-purpose tokens
         MVC   G_STGOR_LEN,G_SCAN_TOKEN_MAXLEN
         MVC   G_STGOR_PTR,G_SCAN_TOKENA
         MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
         L     R15,LWZMAKE_STG_RELEASEA
         BASR  R14,R15
*
         MVC   G_STGOR_LEN,G_SCAN_TOKEN2_MAXLEN
         MVC   G_STGOR_PTR,G_SCAN_TOKEN2A
         MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
         L     R15,LWZMAKE_STG_RELEASEA
         BASR  R14,R15
*
         MVC   G_STGOR_LEN,G_SCAN_TOKEN3_MAXLEN
         MVC   G_STGOR_PTR,G_SCAN_TOKEN3A
         MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
         L     R15,LWZMAKE_STG_RELEASEA
         BASR  R14,R15
*
*        Free evaluation block
         MVC   G_STGOR_LEN,G_EVALBLOCK_MAXLEN
         MVC   G_STGOR_PTR,G_EVALBLOCK_PTR
         MVI   G_STGOR_TYPE,STGOR_TYPE_EVAL
         L     R15,LWZMAKE_STG_RELEASEA
         BASR  R14,R15
*
         IF (CLI,MKFOPEN,EQ,C'Y') THEN      * Was MAKEFILE opened?
            L     R14,G_DCB_MEM_PTR         * Get DCB memory pointer
            LA    R2,DCBMKF-DCB_DSECT(,R14) * Get addr of MAKEFILE DCB
            CLOSE ((R2)),MODE=31            * and close it
*           Trace record MAKEFILE DCB closed
            MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'603',CONST=C'MAKEX
               FILE'
*           Report line MAKEFILE closed
            MLWZMRPT RPTLINE=CL133' MAKEFILE DD closed'
         ENDIF
*
         IF (CLI,RPTOPEN,EQ,C'Y') THEN * Was LWZMRPT opened?
            IF (CLC,G_RETCODE,NE,=F'0') THEN * Did an error occur?
               MLWZMRPT RPTLINE=CL133'0Error occurred, see LWZMTRC for X
               more details.'
            ENDIF
*           Write last report line
            MLWZMRPT RPTLINE=CL133'0LWZMAKE end'
            L     R14,G_DCB_MEM_PTR         * Get DCB memory pointer
            LA    R2,DCBRPT-DCB_DSECT(,R14) * Get addr of LWZMRPT DCB
            CLOSE ((R2)),MODE=31            * and close it
*           Trace record LWZMRPT DCB closed
            MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'603',CONST=C'LWZMX
               RPT'
         ENDIF
*
         IF (CLI,TRCOPEN,EQ,C'Y') THEN * Was LWZMTRC opened?
            L     R14,G_DCB_MEM_PTR         * Get DCB memory pointer
            LA    R2,DCBTRC-DCB_DSECT(,R14) * Get addr of LWZMTRC DCB
            CLOSE ((R2)),MODE=31            * and close it
         ENDIF
*
         IF (CLC,G_DCB_MEM_PTR,NE,=A(0)) THEN * Was DCB memory alloc'd?
            L     R1,G_DCB_MEM_PTR    * Get DCB memory pointer
            FREEMAIN RU,LV=DCB_DSECT_SIZ,A=(R1) * and free it
            MVC   G_DCB_MEM_PTR,=A(0) * Clear DCB memory pointer
         ENDIF
*
         BR    R8                 * Return
*
* EODAD for MAKEFILE
*
MAKEFILE_IS_EOF EQU *
         MVI   G_MKFEOF,C'Y'      * Set MAKEFILE EOF to true
*        Pop input stack
         XR    R14,R14
         IC    R14,G_SCAN_INPUT_STACK_IDX
         BCTR  R14,R0
         STC   R14,G_SCAN_INPUT_STACK_IDX
*        R7 was set to instruction following GET macro
         BR    R7
*
* End of code of main section
*
         LTORG
*
* Report header line template
CPAGE_HEADER DC CL133'1Light Weight LWZMAKE utility                    X
                                    Date DD-MM-YYYY    Time HH:MM:SS   X
                Page 00000000'
*
* Translate table for conversion to hex
                            DS    0F
MAIN_HEXTAB                 EQU   *-C'0'
                            DC    C'0123456789ABCDEF'
*
* Table of scan state full words of flag bits.
* Every scan state is an index of this table, every entry in this
* table contains up to 32 flag bits indicating valid token types
* for that given scan state.
SCAN_STATE_TABLE            DS    0F
                            DC    AL4(SCAN_EXPECTED_NEWSTMT)
                            DC    AL4(SCAN_EXPECTED_NEWSTMT2)
                            DC    AL4(SCAN_EXPECTED_ASSIGN)
                            DC    AL4(SCAN_EXPECTED_ASSIGN2)
                            DC    AL4(SCAN_EXPECTED_VARIABLE)
                            DC    AL4(SCAN_EXPECTED_VARIABLER)
                            DC    AL4(SCAN_EXPECTED_VARIABLE2)
                            DC    AL4(SCAN_EXPECTED_RULE)
                            DC    AL4(SCAN_EXPECTED_RULE2)
                            DC    AL4(SCAN_EXPECTED_RULE3)
                            DC    AL4(SCAN_EXPECTED_CALL)
                            DC    AL4(SCAN_EXPECTED_CALL2)
                            DC    AL4(SCAN_EXPECTED_EXPAND)
                            DC    AL4(SCAN_EXPECTED_PHONY)
                            DC    AL4(SCAN_EXPECTED_PHONY2)
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME)
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME2)
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME3)
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME4)
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST)
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST2)
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST3)
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST4)
                            DC    AL4(SCAN_EXPECTED_FUNCTION)
                            DC    AL4(SCAN_EXPECTED_FUNCTION2)
                            DC    AL4(SCAN_EXPECTED_FUNCTION3)
                            DC    AL4(SCAN_EXPECTED_FUNCTION4)
                            DC    AL4(SCAN_EXPECTED_INCLUDE)
                            DC    AL4(SCAN_EXPECTED_INCLUDE2)
                            DC    AL4(SCAN_EXPECTED_PARMTARGET)
*
* Local constant pointers to section addresses
LWZMAKE_TRACEA              DC    A(LWZMAKE_TRACE)
LWZMAKE_RPTA                DC    A(LWZMAKE_RPT)
LWZMAKE_STG_OBTAINA         DC    A(LWZMAKE_STG_OBTAIN)
LWZMAKE_STG_RELEASEA        DC    A(LWZMAKE_STG_RELEASE)
LWZMAKE_APPEND_TOKENA       DC    A(LWZMAKE_APPEND_TOKEN)
LWZMAKE_SCAN_TOKENA         DC    A(LWZMAKE_SCAN_TOKEN)
LWZMAKE_PHASE1A             DC    A(LWZMAKE_PHASE1)
LWZMAKE_PHASE2A             DC    A(LWZMAKE_PHASE2)
LWZMAKE_FREEBSTA            DC    A(LWZMAKE_FREEBST)
*
* Constant list form of LOAD macro
                            DS    0F
LOADL                       LOAD  SF=L
LOAD_SIZ                    EQU   *-LOADL
*
* Constant list form of LINK macro
                            DS    0F
LINKL                       LINK  SF=L
LINK_SIZ                    EQU   *-LINKL
*
* Constant list form of OPEN macro
                            DS    0F
OPENL                       OPEN  (,),MODE=31,MF=L
OPEN_SIZ                    EQU   *-OPENL
*
* Constant DCB for LWZMTRC
CDCBTRC                     DCB   DDNAME=LWZMTRC,LRECL=133,MACRF=(PM),RX
               ECFM=FBA,DSORG=PS
LEN_DCBTRC                  EQU   *-CDCBTRC
*
* Constant DCB for LWZMRPT
CDCBRPT                     DCB   DDNAME=LWZMRPT,LRECL=133,MACRF=(PM),RX
               ECFM=FBA,DSORG=PS
LEN_DCBRPT                  EQU   *-CDCBRPT
*
* Constant DCB for MAKEFILE
CDCBMKF                     DCB   DDNAME=MAKEFILE,LRECL=80,MACRF=(GM),RX
               ECFM=FB,DSORG=PS,DCBE=CDCBEMKF
LEN_DCBMKF                  EQU   *-CDCBMKF
*
* Constant DCBE for MAKEFILE
CDCBEMKF                    DCBE  EODAD=0,RMODE31=BUFF
LEN_DCBEMKF                 EQU   *-CDCBEMKF
*
* DSECT for SA, main section working storage, which also contains the
* area for global data. This DSECT is pointed to by R13, global data
* is pointed to by R9.
*
WORKAREA                    DSECT
                            DS    18F * My savearea
PARML_PTR                   DS    A   * Contents of R1 on entry
*
* Working storage list form of LOAD macro
                            DS    0F
LOADD                       DS    CL(LOAD_SIZ)
*
* Flags indicating DCB's open or not
                            DS    0F
TRCOPEN                     DS    C   * LWZMTRC  open flag
RPTOPEN                     DS    C   * LWZMRPT  open flag
MKFOPEN                     DS    C   * MAKEFILE open flag
*
* Area for global data, pointed to by R9
                            DS    0F
GLOBAL_DATA                 DS    CL(GLOBAL_DATA_SIZ)
*
WORKAREA_SIZ                EQU   *-WORKAREA
*
* DSECT for global data pointed to by R9, global vars start with G_
* for visibility
*
GLOBAL_DATA_DSECT           DSECT
*
* LWZMAKE return code
G_RETCODE                   DS    F
*
* Pointers to section address
                            DS    0F
G_LWZMAKE_TRACEA            DS    A
G_LWZMAKE_RPTA              DS    A
*
* Working storage list form of OPEN macro
                            DS    0F
G_OPEND                     DS    CL(OPEN_SIZ)
*
* Working storage list form of LINK macro
                            DS    0F
G_LINKD                     DS    CL(LINK_SIZ)
*
* Pointer to 24-bit block of storage for DCB's, DCB_DSECT for overlay
                            DS    0F
G_DCB_MEM_PTR               DS    A
*
* Userid executing current task
G_USERID                    DS    CL8
*
* Helper for converting CALL to uppercase
G_CALL4                     DS    CL4
*
* EOF flag for MAKEFILE
G_MKFEOF                    DS    C
*
* Flag to indicate whether to expand or not
G_DO_EXPAND                 DS    C
*
* Flag to indicate whether to assign variable value or not
G_DO_ASSIGN                 DS    C
*
* RECIPEPREFIX, initialized to X'05', can be set in MAKEFILE script
G_RECIPEPREFIX              DS    C
*
* LWZMAKE trace level
G_LWZMAKE_TRACE             DS    C
LWZMAKE_TRACE_NONE          EQU   C'0'
LWZMAKE_TRACE_ERROR         EQU   C'1'
LWZMAKE_TRACE_WARNING       EQU   C'2'
LWZMAKE_TRACE_INFO          EQU   C'3'
LWZMAKE_TRACE_DEBUG         EQU   C'4'
LWZMAKE_TRACE_DEEBUG        EQU   C'6'
LWZMAKE_TRACE_DEEEBUG       EQU   C'8'
*
* Trace message number '000' - '999'
G_LWZMTRC_MSGNR             DS    CL3
*
* Trace record
G_LWZMTRC_RECORD            DS    CL133
*
* Trace record data pointer and size
                            DS    0F
G_LWZMTRC_DATA_PTR          DS    A
G_LWZMTRC_DATA_SIZ          DS    H
*
* STORAGE OBTAIN / RELEASE vars
                            DS    0F
G_STGOR_LEN                 DS    F
G_STGOR_PTR                 DS    A
G_STGOR_TYPE                DS    C
STGOR_TYPE_TOKEN            EQU   C'N'
STGOR_TYPE_STMT             EQU   C'S'
STGOR_TYPE_VAR              EQU   C'V'
STGOR_TYPE_VARVAL           EQU   C'='
STGOR_TYPE_TGT              EQU   C'T'
STGOR_TYPE_PNY              EQU   C'P'
STGOR_TYPE_EVAL             EQU   C'E'
*
* Catalog search interface IGGCSI00 external function address
                            DS    0F
G_IGGCSI00A                 DS    A
*
* Input and output of CHECK_MVSDS section
                            DS    0F
G_CHECK_MVSDS_TOKEN         DS    C
G_CHECK_MVSDS_MVSDS         DS    C
                            DS    CL2
*
* Pointer and length of member name in MVS data set string
G_MVSDS_MEMBER_PTR          DS    A
G_MVSDS_MEMBER_LEN          DS    F
*
* DYNALLOC section input and output
                            DS    0F
G_DYNALLOC_FUNC             DS    C
                            DS    0F
G_DYNALLOC_DSNAME           DS    CL44
G_DYNALLOC_MEMBER           DS    CL8
G_DYNALLOC_DDNAME           DS    CL8
*
* Binder fast data access external function address
                            DS    0F
G_IEWBFDATA                 DS    A
*
* CALL_REXX parameters
G_CALL_REXX_PAR2A           DS    2A
*
* REXX execute IRXINIT parameters
G_IRXINIT_PAR7A             DS    10A
G_IRXINIT_FUNCTION          DS    CL8
G_IRXINIT_PARMMOD           DS    CL8
G_IRXINIT_INSTORPARM_PTR    DS    CL4
G_IRXINIT_USRFIELD_PTR      DS    CL4
G_IRXINIT_RESERVED_PTR      DS    CL4
G_IRXINIT_ENVBLOCK_PTR      DS    CL4
G_IRXINIT_REASON            DS    CL4
*
* ISPF ISPLINK parameters
G_ISPLINK_PAR5A             DS    5A
G_ISPLINK_FUNCTION          DS    CL8
G_ISPLINK_VARNAME           DS    CL8
G_ISPLINK_VARPTR            DS    A
G_ISPLINK_FORMAT            DS    CL8
G_ISPLINK_VARLEN            DS    F
G_ISPLINK_VAR               DS    CL250
                            DS    CL2
*
* REXX execute ISPEXEC parameters
G_USE_ISPEXEC               DS    C
                            DS    0F
G_ISPEXEC_PAR2A             DS    2A
*
* REXX execute IRXEXEC parameters
G_IRXEXEC_PAR10A            DS    10A
G_IRXEXEC_EXECBLK_PTR       DS    CL4
G_IRXEXEC_ARGS_PTR          DS    CL4
G_IRXEXEC_FLAGS             DS    CL4
G_IRXEXEC_INSTBLK_PTR       DS    CL4
G_IRXEXEC_CPPL_PTR          DS    CL4
G_IRXEXEC_EVALBLK_PTR       DS    CL4
G_IRXEXEC_WORKAREA_PTR      DS    CL4
G_IRXEXEC_USRFIELD_PTR      DS    CL4
G_IRXEXEC_ENVBLOCK_PTR      DS    CL4
G_IRXEXEC_REASON_PTR        DS    CL4
*
* REXX exec block
                            DS    0F
G_IRXEXEC_EXECBLK           DS    CL(EXECBLEN)
*
* REXX exec arguments, 4 byte pointer + 4 byte length + 8 byte X'FF'
                            DS    0F
G_IRXEXEC_ARGS              DS    CL16
*
* REXX evaluation block
G_EVALBLOCK_PTR             DS    A
G_EVALBLOCK_MAXLEN          DS    F
*
* REXX execute IRXEXEC reason code
                            DS    0F
G_IRXEXEC_REASON            DS    CL4
*
* REXX get result routine
G_IRXRLT_PAR5A              DS    5A
G_IRXRLT_FUNCTION           DS    CL8
G_IRXRLT_EVALBLK_PTR        DS    CL4
G_IRXRLT_EVALDATA_LEN       DS    CL4
G_IRXRLT_ENVBLOCK_PTR       DS    CL4
G_IRXRLT_REASON             DS    CL4
*
* Starting address of statement pointer block linked list
* This linked list links 4K blocks, each containing (possibly) 1022
* pointers to any STMT_*_DSECT area, first pointer is a backward chain,
* 1024th pointer is a forward chain
                            DS    0F
G_STMT_LIST_PTR             DS    A
*
* Pointer to previous statement used to chain stmts in linked list
G_STMT_SAVE_PTR             DS    A
*
* Statement operator ('=', ':=' or '?=' in assigment, ':' in rule)
G_STMT_SAVE_OP              DS    CL2
*
* Previous statement type
G_PREV_STMT_TYPE            DS    C
*
* Switch (Y/N) indicating previous statement was in recipe
G_PREV_STMT_IN_RECIPE       DS    C
*
* Parameters and return pointer for LWZMAKE_ALLOC_STMT section
G_STMT_ALLOC_LEN            DS    F    * size of memory to allocate
G_STMT_ALLOC_TYPE           DS    C    * statement type
                            DS    CL3  * reserved
G_STMT_ALLOC_RETURN_PTR     DS    A    * returned ptr to alloc'd memory
*
* Scanner/tokenizer/parser variables
G_SCAN_CURRLINE             DS    F    * Current line
G_SCAN_CURRCOL              DS    F    * Current column within line
G_SCAN_SPACE_COUNT          DS    F    * Spaces since last token
G_SAVE_SPACE_COUNT          DS    F    * Place to remember space count
G_SCAN_CURRCHAR             DS    C    * Scanner current character
G_SCAN_PEEKCHAR             DS    C    * Scanner next character
G_SCAN_NEWLINE              DS    C    * Switch ind new line
G_SCAN_CONTINUED_LINE       DS    C    * Switch ind continued line (\)
G_SCAN_TOKENTYPE            DS    C    * Token type for G_SCAN_TOKEN
G_SCAN_TOKENTYPE2           DS    C    * Token type for G_SCAN_TOKEN2
G_SCAN_TOKENTYPE3           DS    C    * Token type for G_SCAN_TOKEN3
SCAN_TOKENTYPE_IGNORE       EQU   C'I' * Anything beyond pos 72
SCAN_TOKENTYPE_COMMENT      EQU   C'#' * Anything after #
SCAN_TOKENTYPE_NORMAL       EQU   C'A' * Any word pos 1 A-Z a-z
SCAN_TOKENTYPE_NUMBER       EQU   C'N' * Any work containing only 0-9
SCAN_TOKENTYPE_OPERATOR     EQU   C'=' * Any operator (:= ?= =)
SCAN_TOKENTYPE_CONTINUATION EQU   C'\' * Line continuation char \
SCAN_TOKENTYPE_RULE         EQU   C':' * Rule separator :
SCAN_TOKENTYPE_SPECIAL      EQU   C'.' * Any special variable
SCAN_TOKENTYPE_VARIABLE     EQU   C'$' * The word $(
SCAN_TOKENTYPE_CLOSEBRACKET EQU   C')' * Closing bracket char )
SCAN_TOKENTYPE_CALL         EQU   C'C' * The word CALL
SCAN_TOKENTYPE_ACRO         EQU   C'@' * The word $@
SCAN_TOKENTYPE_PERCENT      EQU   C'%' * The word $%
SCAN_TOKENTYPE_RECIPEPREFIX EQU   X'05' * Pos 1 if it's equal to RPREF
SCAN_TOKENTYPE_COMMA        EQU   C',' * Comma character ,
SCAN_TOKENTYPE_SLASH        EQU   C'/' * Slash character /
G_SCAN_APPEND_TO            DS    C    * Which G_SCAN_TOKEN* to append
*                                      * to when scanning $() variable
G_SCAN_CLOSE_BRACKET        DS    C    * Save ) or } to check matching
*                                      * with ( or {
G_SCAN_VAR_PRESERVE_SPACES  DS    C    * Preserve all spaces or just 1
G_SCAN_TOKEN_72             DS    C    * Check for 72 bounds
*
* Stack of scan state bytes, highest entry is the last state before
* the one in G_SCAN_STATE
                            DS    0F
G_SCAN_STATE_STACK          DS    CL128
*
* The current scan state byte which is an index on SCAN_STATE_TABLE
G_SCAN_STATE                DS    C    1
SCAN_STATE_NOT_IN_STMT      EQU   X'00'
SCAN_STATE_IN_STMT          EQU   X'01'
SCAN_STATE_IN_ASSIGN        EQU   X'02'
SCAN_STATE_IN_ASSIGN2       EQU   X'03'
SCAN_STATE_IN_VARIABLE      EQU   X'04'
SCAN_STATE_IN_VARIABLER     EQU   X'05'
SCAN_STATE_IN_VARIABLE2     EQU   X'06'
SCAN_STATE_IN_RULE          EQU   X'07'
SCAN_STATE_IN_RULE2         EQU   X'08'
SCAN_STATE_IN_RULE3         EQU   X'09'
SCAN_STATE_IN_CALL          EQU   X'0A'
SCAN_STATE_IN_CALL2         EQU   X'0B'
SCAN_STATE_IN_EXPAND        EQU   X'0C'
SCAN_STATE_IN_PHONY         EQU   X'0D'
SCAN_STATE_IN_PHONY2        EQU   X'0E'
SCAN_STATE_IN_ADDPDSNAME    EQU   X'0F'
SCAN_STATE_IN_ADDPDSNAME2   EQU   X'10'
SCAN_STATE_IN_ADDPDSNAME3   EQU   X'11'
SCAN_STATE_IN_ADDPDSNAME4   EQU   X'12'
SCAN_STATE_IN_MEMBERLIST    EQU   X'13'
SCAN_STATE_IN_MEMBERLIST2   EQU   X'14'
SCAN_STATE_IN_MEMBERLIST3   EQU   X'15'
SCAN_STATE_IN_MEMBERLIST4   EQU   X'16'
SCAN_STATE_IN_FUNCTION      EQU   X'17'
SCAN_STATE_IN_FUNCTION2     EQU   X'18'
SCAN_STATE_IN_FUNCTION3     EQU   X'19'
SCAN_STATE_IN_FUNCTION4     EQU   X'1A'
SCAN_STATE_IN_INCLUDE       EQU   X'1B'
SCAN_STATE_IN_INCLUDE2      EQU   X'1C'
SCAN_STATE_IN_PARMTARGET    EQU   X'1D'
SCAN_STATE_IN_RECIPE        EQU   X'80'
                            DS    C    2
*
* Current stack size, current scan state in G_SCAN_STATE, last scan
* state pushed on stack at G_SCAN_STATE_STACK_IDX
G_SCAN_STATE_STACK_IDX      DS    C    3
                            DS    C    4
*
* Working field to contain the entry in SCAN_STATE_TABLE indexed by
* G_SCAN_STATE
G_SCAN_EXPECTED             DS    CL4
*
* Flags for using TM byte by byte in G_SCAN_EXPECTED
SCAN_EXPECTED1_EOF          EQU   X'80'  1
SCAN_EXPECTED1_NEWLINE      EQU   X'40'  2
SCAN_EXPECTED1_COMMENT      EQU   X'20'  3
SCAN_EXPECTED1_IGNORE       EQU   X'10'  4
SCAN_EXPECTED1_NORMAL       EQU   X'08'  5
SCAN_EXPECTED1_OPENVAR      EQU   X'04'  6
SCAN_EXPECTED1_OPENBRC      EQU   X'02'  7
SCAN_EXPECTED1_CLOSEBRC     EQU   X'01'  8
SCAN_EXPECTED2_NUMBER       EQU   X'80'  9
SCAN_EXPECTED2_OPERATOR     EQU   X'40' 10
SCAN_EXPECTED2_RULE         EQU   X'20' 11
SCAN_EXPECTED2_SPECIAL      EQU   X'10' 12
SCAN_EXPECTED2_CONTINUA     EQU   X'08' 13
SCAN_EXPECTED2_CALL         EQU   X'04' 14
SCAN_EXPECTED2_ACRO         EQU   X'02' 15
SCAN_EXPECTED2_PERCENT      EQU   X'01' 16
SCAN_EXPECTED3_RECIPREF     EQU   X'80' 17
SCAN_EXPECTED3_COMMA        EQU   X'40' 18
SCAN_EXPECTED3_SLASH        EQU   X'20' 19
*
* Combinations of the flags above, used in SCAN_STATE_TABLE
*                                            1         2         3
*                                   12345678901234567890123456789012
SCAN_EXPECTED_NEWSTMT       EQU   B'11111100000101001010000000000000'
SCAN_EXPECTED_NEWSTMT2      EQU   B'00001100011010001010000000000000'
SCAN_EXPECTED_ASSIGN        EQU   B'01111111100010111110000000000000'
SCAN_EXPECTED_ASSIGN2       EQU   B'01111111100010111110000000000000'
SCAN_EXPECTED_VARIABLE      EQU   B'00001000000010111000000000000000'
SCAN_EXPECTED_VARIABLER     EQU   B'00001001000010111000000000000000'
SCAN_EXPECTED_VARIABLE2     EQU   B'00000001000010111000000000000000'
SCAN_EXPECTED_RULE          EQU   B'00001111001010001010000000000000'
SCAN_EXPECTED_RULE2         EQU   B'01111111100010111010000000000000'
SCAN_EXPECTED_RULE3         EQU   B'01111111100010111010000000000000'
SCAN_EXPECTED_CALL          EQU   B'00001100000010111000000000000000'
SCAN_EXPECTED_CALL2         EQU   B'01111111111010111110000000000000'
SCAN_EXPECTED_EXPAND        EQU   B'10001111111000110110000000000000'
SCAN_EXPECTED_PHONY         EQU   B'00001100000010000000000000000000'
SCAN_EXPECTED_PHONY2        EQU   B'01111100000010000000000000000000'
SCAN_EXPECTED_ADDPDSNAME    EQU   B'00001100000010110000000000000000'
SCAN_EXPECTED_ADDPDSNAME2   EQU   B'00001100000010110100000000000000'
SCAN_EXPECTED_ADDPDSNAME3   EQU   B'00001100100010110000000000000000'
SCAN_EXPECTED_ADDPDSNAME4   EQU   B'00001101100010110000000000000000'
SCAN_EXPECTED_MEMBERLIST    EQU   B'00001100000010100000000000000000'
SCAN_EXPECTED_MEMBERLIST2   EQU   B'00001101000010100100000000000000'
SCAN_EXPECTED_MEMBERLIST3   EQU   B'00001100000010110000000000000000'
SCAN_EXPECTED_MEMBERLIST4   EQU   B'00001101100010110100000000000000'
SCAN_EXPECTED_FUNCTION      EQU   B'00001100000010100000000000000000'
SCAN_EXPECTED_FUNCTION2     EQU   B'00001101000010100100000000000000'
SCAN_EXPECTED_FUNCTION3     EQU   B'00001100000010110000000000000000'
SCAN_EXPECTED_FUNCTION4     EQU   B'00001101100010110110000000000000'
SCAN_EXPECTED_INCLUDE       EQU   B'00001110100010000000000000000000'
SCAN_EXPECTED_INCLUDE2      EQU   B'01111111100010000000000000000000'
SCAN_EXPECTED_PARMTARGET    EQU   B'10001011000000000000000000000000'
SCAN_EXPECTED_IGNORE        EQU   B'01010000000000000000000000000000'
SCAN_EXPECTED_NEWLINE       EQU   B'01000000000000000000000000000000'
SCAN_EXPECTED_COMMENT       EQU   B'01110000000000000000000000000000'
*
* Stack of INPUT_DSECT structures, highest stack entry is the one
* LWZMAKE_SCAN_CHAR reads from. Initial size is 1, entry 0 filled
* with all zeros, indicating input from MAKEFILE DD
                            DS    0F
MAX_SCAN_INPUT_STACK_ENTRY  EQU   16
G_SCAN_INPUT_STACK          DS    CL(MAX_SCAN_INPUT_STACK_ENTRY*INPUT_DX
               SECT_SIZ)
G_SCAN_INPUT_STACK_IDX      DS    C
*
* Starting address of binary search tree of variables, each variable
* addressed with VAR_DSECT
                            DS    0F
G_FIRST_VAR_PTR             DS    A
*
* Parameters and return value for LWZMAKE_FINDVAR
G_SRCH_VAR_LEN              DS    H
G_SRCH_VAR                  DS    CL72
G_FOUND_VAR_PTR             DS    A
*
* Starting address of binary search tree of targets, each target
* addressed with TARGET_DSECT
                            DS    0F
G_FIRST_TGT_PTR             DS    A
*
* Default target to build starting phase 2
G_DEFAULT_TARGET            DS    CL72
*
* Return value for LWZMAKE_FINDTGT (takes input from G_SCAN_TOKEN)
G_FOUND_TGT_PTR             DS    A
*
* Parameter area ptr + area for first call to LWZMAKE_EXEC_TGT
G_EXEC_TGT_PAR1A            DS    A
G_EXEC_TGT_PAR1             DS    CL(EXEC_TGT_PAR_LEN)
*
* Starting address of binary search tree of PHONIES, each PHONY
* addressed with PHONY_DSECT
                            DS    0F
G_FIRST_PNY_PTR             DS    A
*
* Return value for LWZMAKE_FINDPNY (takes input from G_SCAN_TOKEN)
G_FOUND_PNY_PTR             DS    A
*
* Returned altered date+time from LWZMAKE_GET_DATE
                            DS    0F
G_SAVE_ALTER_DATE           DS    CL16
G_DSFOUND                   DS    C
*
* Current MAKEFILE record being scanned, read by LWZMAKE_SCAN_CHAR
                            DS    0F
G_MAKEFILE_REC              DS    CL80
*
* Progress report current page and line
                            DS    0F
G_LWZMRPT_CURRPAGE          DS    F
G_LWZMRPT_CURRLINE          DS    H
*
* Switch indicating whether line n column m should be added to rpt line
G_LWZMRPT_APND_LC           DS    C
*
* Line to write to LWZMRPT next
                            DS    0F
G_LWZMRPT_LINE              DS    CL133
*
* Room for formatting data to put in G_LWZMRPT_LINE
                            DS    0F
G_LWZMRPT_HELPER            DS    CL80
*
LINES_PER_PAGE              EQU   40
*
                            DS    0F
G_PAGE_HEADER               DS    CL133
                            ORG   G_PAGE_HEADER+75
G_PAGE_HEADER_DAY           DS    CL2
                            DS    C
G_PAGE_HEADER_MONTH         DS    CL2
                            DS    C
G_PAGE_HEADER_YEAR          DS    CL4
                            ORG   G_PAGE_HEADER+94
G_PAGE_HEADER_HOUR          DS    CL2
                            DS    C
G_PAGE_HEADER_MINUTE        DS    CL2
                            DS    C
G_PAGE_HEADER_SECOND        DS    CL2
                            ORG   G_PAGE_HEADER+111
G_PAGE_HEADER_PAGENR        DS    CL8
                            ORG
*
* Generic variables
                            DS    0F
G_DEC8                      DS    PL8  * for CVB / CVD
G_ZONED8                    DS    CL8  * for PACK / UNPK
                            DS    CL4  * Extra bytes for hex conversion
G_TIMEDATE                  DS    PL16 * for TIME macro
                            DS    0F
G_TIMEDATEZ                 DS    CL32 * for formatting time
G_HELPER_DATA               DS    CL133 * for formatting anything
*                                       * mostly trace data
*
* WTOBLOCK for WTO execute form
G_WTOBLOCK                  DS    0F
G_WTOLEN                    DS    H
G_WTOFIL                    DS    H
G_WTOTEXT                   DS    CL133
*
* 3 multi-purpose 4K token area's
                            DS    0F
G_SCAN_TOKEN_LEN            DS    F
G_SCAN_TOKEN2_LEN           DS    F
G_SCAN_TOKEN3_LEN           DS    F
G_SCAN_TOKEN_MAXLEN         DS    F
G_SCAN_TOKEN2_MAXLEN        DS    F
G_SCAN_TOKEN3_MAXLEN        DS    F
G_SCAN_TOKENA               DS    A
G_SCAN_TOKEN2A              DS    A
G_SCAN_TOKEN3A              DS    A
*
SCAN_TOKEN_MAXLEN           EQU   4096
*
GLOBAL_DATA_SIZ             EQU   *-GLOBAL_DATA_DSECT
*
* DSECT for DCB memory below the line
*
DCB_DSECT                   DSECT
*
* DCB for LWZMTRC, constant CDCBTRC copied into here
DCBTRC                      DS    0F
                            ORG   *+LEN_DCBTRC
*
* DCB for LWZMRPT, constant CDCBRPT copied into here
DCBRPT                      DS    0F
                            ORG   *+LEN_DCBRPT
*
* DCB for MAKEFILE, constant CDCBMKF copied into here
DCBMKF                      DS    0F
                            ORG   *+LEN_DCBMKF
*
* DCBE for MAKEFILE, constant CDCBEMKF copied into here
DCBEMKF                     EQU   *
                            ORG   *+LEN_DCBEMKF
*
* DCB for any PDS dynamically allocated for reading directory
DCBPDS_DIR                  DS    0F
                            ORG   *+LEN_DCBPDS_DIR_GD
*
* DCBE for any PDS dynamically allocated for reading directory
DCBEPDS_DIR                 EQU   *
                            ORG   *+LEN_DCBEPDS_DIR_GD
*
* DCB for any PDS dynamically allocated for binder access
DCBPDS_BDR                  DS    0F
                            ORG   *+LEN_DCBPDS_BDR
*
DCB_DSECT_SIZ               EQU   *-DCB_DSECT
*
* DSECT for included makefile DCB memory below the line
*
DCB_INCLUDE_DSECT           DSECT
*
* DCB for included MAKEFILE, constant CDCBMKF copied into here
DCBMKFI                     DS    0F
                            ORG   *+LEN_DCBMKF
*
* DCBE for included MAKEFILE, constant CDCBEMKF copied into here
DCBEMKFI                    EQU   *
                            ORG   *+LEN_DCBEMKF
*
DCB_INCLUDE_DSECT_SIZ       EQU   *-DCB_INCLUDE_DSECT
*
* Every parsed statement is converted to internal format, being one of
* the following DSECTs. Each DSECT is dynamically allocated, pointer is
* stored in the statement pointer block linked list (starts at
* G_STMT_LIST_PTR). Every statement area starts with a generic part
* addressed with STMT_DSECT. Every STMT_*_DSECT starts with this
* generic part, followed by the specifics of the type of statement.
*
STMT_DSECT                  DSECT
STMT_LEN                    DS    F    * length of stmt area (generic +
*                                      * STMT_*_DSECT part)
STMT_TYPE                   DS    C    * byte for type of statement
STMT_TYPE_ASSIGNMENT        EQU   C'A' * assignment, use STMT_A_DSECT
STMT_TYPE_RULE              EQU   C'R' * rule, use STMT_R_DSECT
STMT_TYPE_CALL              EQU   C'C' * call, use STMT_C_DSECT
STMT_TYPE_PHONY             EQU   C'P' * PHONY, use STMT_P_DSECT
STMT_TYPE_INCLUDE           EQU   C'I' * INCLUDE, use STMT_I_DSECT
STMT_IN_RECIPE              DS    C    * switch (Y/N) indicating stmt
*                                      * found in recipe
                            DS    CL2  * reserved
STMT_NEXT_PTR               DS    A    * forward chain to next stmt
STMT_PREV_PTR               DS    A    * backward chain to prev stmt
STMT_DSECT_LEN              EQU   *-STMT_DSECT
*
* Statement area for assignment type statement
*
STMT_A_DSECT                DSECT
*                                 * generic part of area
                            DS    CL(STMT_DSECT_LEN)
*
STMT_A_DESTLEN              DS    H    * length of variable name
STMT_A_DEST                 DS    CL72 * variable name (destination)
STMT_A_OPERATOR             DS    CL2  * type of assignment
STMT_A_SRCLEN               DS    H    * length of value
STMT_A_SRC                  DS    0C   * value (source) starts here
*                                      * length can vary
STMT_A_DSECT_LEN            EQU   *-STMT_A_DSECT
*
* Statement area for rule type statement
*
STMT_R_DSECT                DSECT
*                                 * generic part of area
                            DS    CL(STMT_DSECT_LEN)
*
* STMT_R_TGT is the starting address of 3 strings: target name,
* requisite name and requisite for sequence only name. Each string
* directly follows the previous one, so the lengths are also the
* offsets to the next string.
STMT_R_TGTLEN               DS    H   * target name length
STMT_R_REQLEN               DS    H   * requisite name length
STMT_R_REQOLEN              DS    H   * requisite for sequence only len
                            DS    CL2 * reserved
STMT_R_TGT                  DS    0C  * target name starts here
STMT_R_DSECT_LEN            EQU   *-STMT_R_DSECT
*
* Statement area for call type statement
*
STMT_C_DSECT                DSECT
*                                 * generic part of area
                            DS    CL(STMT_DSECT_LEN)
*
* STMT_C_EXEC is the starting address of 2 strings: exec name and
* parameter string. Parameter string directly follows the exec name,
* so the exec length is also the offset to the parameter string.
STMT_C_EXECLEN              DS    H   * exec name length
STMT_C_PARMLEN              DS    H   * parameters string length
STMT_C_EXEC                 DS    0C  * exec name starts here
STMT_C_DSECT_LEN            EQU   *-STMT_C_DSECT
*
* Statement area for PHONY type statement
*
STMT_P_DSECT                DSECT
*                                 * generic part of area
                            DS    CL(STMT_DSECT_LEN)
*
* STMT_P_PNY is the starting address of a phony target name.
STMT_P_PNYLEN               DS    H   * PHONY name length
                            DS    CL2 * reserved
STMT_P_PNY                  DS    0C  * PHONY name starts here
STMT_P_DSECT_LEN            EQU   *-STMT_P_DSECT
*
* Statement area for INCLUDE type statement
*
STMT_I_DSECT                DSECT
*                                 * generic part of area
                            DS    CL(STMT_DSECT_LEN)
*
* STMT_I_INC is the starting address of an include ds name.
STMT_I_INCLEN               DS    H   * INCLUDE name length
                            DS    CL2 * reserved
STMT_I_INC                  DS    0C  * INCLUDE name starts here
STMT_I_DSECT_LEN            EQU   *-STMT_I_DSECT
*
* Variable area, first one pointed to by G_FIRST_VAR_PTR, each VARLOW
* and VARHIGH (possibly) point to variables with a name lower or higher
*
VAR_DSECT                   DSECT
VARLEN                      DS    F    * length of the whole block
VARLOW                      DS    A    * pointer to variable with name
*                                      * lower than this one
VARHIGH                     DS    A    * pointer to variable with name
*                                      * higher than this one
VARNAMELEN                  DS    H    * length of variable name
VARNAME                     DS    CL72 * variable name
VALLEN                      DS    H    * length of variable value
VALPTR                      DS    A    * pointer to value (getmain'd)
VAR_DSECT_LEN               EQU   *-VAR_DSECT
*
* Target area, first one pointed to by G_FIRST_TGT_PTR, each TGTLOW
* and TGTHIGH (possibly) point to target with a name lower or higher
*
TARGET_DSECT                DSECT
TGTLEN                      DS    F    * length of the whole block
TGTLOW                      DS    A    * pointer to target with name
*                                      * lower than this one
TGTHIGH                     DS    A    * pointer to target with name
*                                      * higher than this one
TGTSTMT                     DS    A    * pointer to stmt that resulted
*                                      * in this target
TGTNAMELEN                  DS    H    * length of target name
TGTNAMEMEMLEN               DS    H    * length of target member name
TGTNAME                     DS    0C   * target name starts here
TARGET_DSECT_LEN            EQU   *-TARGET_DSECT
*
* PHONY area, first one pointed to by G_FIRST_PNY_PTR, each PNYLOW
* and PNYHIGH (possibly) point to PHONY with a name lower or higher
*
PHONY_DSECT                 DSECT
PNYLEN                      DS    F    * length of the whole block
PNYLOW                      DS    A    * pointer to PHONY with name
*                                      * lower than this one
PNYHIGH                     DS    A    * pointer to PHONY with name
*                                      * higher than this one
PNYSTMT                     DS    A    * pointer to stmt that resulted
*                                      * in this PHONY
PNYNAMELEN                  DS    H    * length of PHONY name
PNYNAME                     DS    0C   * PHONY name starts here
PHONY_DSECT_LEN             EQU   *-PHONY_DSECT
*
* Input descriptor area, G_SCAN_INPUT_STACK consists of blocks with
* this layout. A block with all zeros means input is taken from the
* MAKEFILE DD. The value of a variable is another source of input.
*
INPUT_DSECT                 DSECT
INPUTTYPE                   DS    C    * type of input, X'00' means
*                                      * input from MAKEFILE DD
INPUTTYPE_MAKEFILE          EQU   X'00'
INPUTTYPE_STRPTR_NEOF       EQU   X'01'
INPUTTYPE_STRPTR_EOF        EQU   X'02'
INPUTTYPE_MAKEFILE_INC      EQU   X'03'
INPUTTYPE_STRPTR_NEOF_FREE  EQU   X'04'
                            DS    C    * reserved
*
INPUTLEAD                   DS    H    * leading spaces count
*
INPUTPTR                    DS    A    * for type != X'00' ptr to input
*                                      * string (e.g. variable value)
INPUTXPTR                   DS    A    * extra pointer
INPUTLEN                    DS    H    * length of string pointed to by
*                                      * INPUTPTR
INPUTPOS                    DS    H    * current position in string
*                                      * pointed to by INPUTPTR
INPUT_DSECT_SIZ             EQU   *-INPUT_DSECT
*
* DSECT for IGGCSI00 catalog search interface
CSIFIELD_DSECT              DSECT
CSIFILTK                    DS    CL44    FILTER   KEY
CSICATNM                    DS    CL44    CATALOG NAME OR BLANKS
CSIRESNM                    DS    CL44    RESUME NAME OR BLANKS
CSIDTYPD                    DS    0CL16   ENTRY TYPES
CSIDTYPS                    DS    CL16
CSIOPTS                     DS    0CL4    CSI OPTIONS
CSICLDI                     DS    CL1     RETURN DATA OR INDEX
CSIRESUM                    DS    CL1     RESUME FLAG
CSIS1CAT                    DS    CL1     SEARCH CATALOG
CSIRESRV                    DS    XL1     RESERVED
CSINUMEN                    DS    H       NUMBER OF ENTRIES FOLLOWING
CSIENTS                     DS    0CL8    VARIABLE NUMBER OF ENTRIES
CSIFLDNM                    DS    CL8     FIELD NAME
CSIFIELD_DSECT_SIZ          EQU   *-CSIFIELD_DSECT
*
* DSECT for OBTAIN
OBTAIN_DSECT                DSECT
                            IECSDSL1 1
OBTAIN_DSECT_SIZ            EQU   *-OBTAIN_DSECT
*
* DSECT for addressing a DCB
                            DCBD  DSORG=PS,DEVD=DA
*
* DSECT for addresssing a DCBE
                            IHADCBE
*
* The following macro's are all needed to use IAZXJSAB for determining
* the submitter user id.
                            IHAPSA   DSECT=YES,LIST=NO
                            IAZJSAB  DSECT=YES,LIST=NO
                            IHAASCB  DSECT=YES,LIST=NO
                            IHAASSB  LIST=NO
                            IHASTCB  LIST=NO
                            IKJTCB   DSECT=YES,LIST=NO
*
* DSECT for addressing REXX EXEC block
                            IRXEXECB
*
* DSECT for addressing REXX EVAL block
                            IRXEVALB
*
                            IKJTSVT
*
                            CVT      DSECT=YES,LIST=NO
*
* Continue with code
LWZMAKE  CSECT
*
***********************************************************************
* Section: LWZMAKE_TRACE                                              *
* Purpose: This section writes a trace record to the LWZMTRC DD.      *
*          Mostly invoked using the macro MLWZMTRC at the top of this *
*          source file.                                               *
*          On entry, G_LWZMTRC_MSGNR should be filled with a 3 digit  *
*          message number (e.g. C'001'), optionally G_LWZMTRC_DATA_PTR*
*          and G_LWZMTRC_DATA_SIZ can also be provided. When DATA_SIZ *
*          is not zero, whatever DATA_PTR points to is appended to the*
*          trace record after the last non-space + 1.                 *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_TRACE MLWZSAVE
*
*        Make sure G_LWZMTRC_MSGNR is composed of 3 digits
         TRT   G_LWZMTRC_MSGNR,XLATENUM
         BNZ   RET_TRACE          * if not, skip to end
*
*        Setup R6,R7 and R8 dependent on the hundreds digit
         SELECT CLI,G_LWZMTRC_MSGNR,EQ
            WHEN C'0'
               LA    R8,LWZ000    * Start of 000-099 messages
               LA    R7,LWZ000T   * Table with 100 single byte offsets
               LA    R6,LWZ000X   * 2x2 byte offset+length table
            WHEN C'6'
               LA    R8,LWZ600    * Start of 600-699 messages
               LA    R7,LWZ600T   * Table with 100 single byte offsets
               LA    R6,LWZ600X   * 2x2 byte offset+length table
            OTHRWISE
               B     RET_TRACE    * if none of the above, skip to end
         ENDSEL
*
*        Convert last 2 digits of message number to binary in R2
         MVC   G_ZONED8,=CL8'0'   * initialize to 8 zeros
         MVC   G_ZONED8+6(2),G_LWZMTRC_MSGNR+1 * put last 2 digits
*                                              * at the end
         PACK  G_DEC8,G_ZONED8    * convert to packed decimal
         CVB   R2,G_DEC8          * convert to binary
*
*        Use binary value of last 2 digits as offset to LWZn00T
         AR    R2,R7              * offset LWZn00T by message number
         CLI   0(R2),X'FF'        * check if constant is present
         BE    RET_TRACE          * if not, skip to end
*
*        Use byte in LWZn00T as index to LWZn00X
         XR    R3,R3              * Clear R3
         IC    R3,0(,R2)          * Put index byte in R3
         SLL   R3,2               * Multiply by 4
         AR    R3,R6              * offset LWZn00X by index
*
*        Use offset in entry of LWZn00X to address message constant
         XR    R4,R4              * Clear R4
         LH    R4,0(,R3)          * Load halfword offset to LWZn00
         AR    R4,R8              * offset LWZn00
*
*        Put length in entry of LWZn00X in R5
         XR    R5,R5              * Clear R5
         LH    R5,2(,R3)          * Load length of message constant
         BCTR  R5,R0              * Minus 1 for EX of MVC
*
         LA    R1,G_LWZMTRC_RECORD * Put address of trace record in R1
         MVI   0(R1),C' '         * Initialize trace record to blanks
         MVC   1(L'G_LWZMTRC_RECORD-1,R1),0(R1)
         MVC   1(3,R1),=C'LWZ'    * Put LWZ in pos 2-4
         MVC   4(3,R1),G_LWZMTRC_MSGNR * Put message number in pos 5-7
*
*        Suffix message number with severity level
         SELECT CLI,G_LWZMTRC_MSGNR,EQ
            WHEN C'0'             * Trace messages LWZ000E-LWZ099E
               MVI   7(R1),C'E'   * are error messages
            WHEN C'6'             * Trace messages LWZ600I-LWZ699I
               MVI   7(R1),C'I'   * are informatory messages
         ENDSEL
*
         B     *+10               * Skip MVC constant for EX
         MVC   9(1,R1),0(R4)      * MVC constant for EX
         EX    R5,*-6             * EX previous MVC statement with R5
*
         BAL   R8,TRACE_APPEND    * Perform append of G_LWZMTRC_DATA
*
         L     R5,G_DCB_MEM_PTR   * Get DCB memory pointer
         LA    R5,DCBTRC-DCB_DSECT(,R5) * Get addr of LWZMTRC DCB
         PUT   (R5),G_LWZMTRC_RECORD * Write a trace record to LWZMTRC
*
RET_TRACE EQU  *
         MLWZTERM                 * Return back to caller
*
* Append trace data to trace record after last non space + 1.
* Trace data is optionally pointed to by G_LWZMTRC_DATA_PTR, but only
* if length set in G_LWZMTRC_DATA_SIZ is non zero.
*
TRACE_APPEND EQU *
         CLC   G_LWZMTRC_DATA_SIZ,=H'0' * Check for zero length
         BE    TRACE_APPEND_RET   * If zero, skip append routine
*
*        Trim trailing spaces in trace record
         LA    R6,G_LWZMTRC_RECORD * Point R6 to trace record
         LA    R5,132(,R6)        * Point R5 to last byte of record
         LA    R4,132             * Put trace record length - 1 in R4
TRACE_APPEND_TRIM EQU *
         CLI   0(R5),C' '         * Check for space
         BNE   TRACE_APPEND_TRIM_DONE * Non space means done trimming
         BCTR  R5,R0              * R5 = R5 - 1
         BCT   R4,TRACE_APPEND_TRIM * R4 = R4 - 1 until R4 = 0
TRACE_APPEND_TRIM_DONE EQU *
         LA    R5,2(,R5)          * Go forward to last space + 1
         LA    R4,2(,R4)          * R4 = R4 + 2
         C     R4,=F'133'         * Check if any space left in record
         BNL   TRACE_APPEND_RET   * If not, skip to end of routine
         LA    R3,133             * Put length of trace record in R3
         SR    R3,R4              * Subtract length of text + 2
         CH    R3,G_LWZMTRC_DATA_SIZ * Check if trace data length
         IF (H) THEN              * is less than remaining room in rcd
            LH    R3,G_LWZMTRC_DATA_SIZ * If so, replace with trace
         ENDIF                    * data length
         BCTR  R3,R0              * Minus 1 for EX of MVC
         L     R2,G_LWZMTRC_DATA_PTR * Put trace data pointer in R2
         B     *+10               * Skip MVC constant for EX
         MVC   0(1,R5),0(R2)      * MVC constant for EX
         EX    R3,*-6             * EX previous MVC statement with R3
*
TRACE_APPEND_RET EQU *
         BR    R8                 * Return
*
         LTORG
*
* Translate table to check for digits C'0' - C'9'
XLATENUM DS    0F
         DC    256X'FF'
         ORG   XLATENUM+C'0'
         DC    10X'00'
         ORG
*
* The following macro is used to generate constant tables used for
* addressing messages. It's used for a range of 100 message numbers,
* e.g. from LWZ000E to LWZ099E, and it's only used in this section.
* It expects a series of message constants to be defined, each one
* directly following the other, starting with a label like LWZ000.
* It goes through 100 message numbers in three passes.
* Pass 1: populate the macro's local character variable array MSGS
* Pass 2: generate a table LWZn00T of 100 single byte entries, one for
*         each message number. A message number for which there's no
*         constant defined in the assembler source, a byte X'FF' is
*         generated, for message numbers with a constant defined a
*         byte with a sequence number is generated. That sequence
*         number is used as an index to the table generated in pass 3.
* Pass 3: generate a table LWZn00X with an entry for each defined
*         message. Each entry is 2x2 bytes, the first 2 are an offset
*         of the message constant to the starting label, the second 2
*         are the message constant length.
*
                  MACRO
.* MSGPREF   = first 4 characers of messages, e.g. LWZ0
.* MSGSUFFIX = last character of message indicating severy level I/E
                  MTRCNTRS &MSGPREF=,&MSGSUFFIX=
                  LCLA  &COUNTER   * for looping 100 times
                  LCLC  &MSGNR     * complete msg const name LWZnnna
                  LCLC  &MSGS(100) * table with 100 index values or FF
                  LCLA  &INDEX     * current index
.*
.* start loop with 0
&COUNTER          SETA  0
.* stop looping when counter > 99
.CHECK_LOOP1      AIF   ('&COUNTER' GT '99').STOP_LOOP1
.* construct complete message constant name LWZnnna
.* if counter is single digit, add a '0'
                  AIF   (K'&COUNTER EQ 2).DOUBLE_DIGIT
&MSGNR            SETC  '&MSGPREF'.'0'.'&COUNTER'.'&MSGSUFFIX'
                  AGO   .MSGNR_SET
.DOUBLE_DIGIT     ANOP
&MSGNR            SETC  '&MSGPREF'.'&COUNTER'.'&MSGSUFFIX'
.MSGNR_SET        ANOP
.* preset current message entry in table to empty
&MSGS(&COUNTER+1) SETC ''
.* if assembler source contains defined constant with MSGNR as name
                  AIF   (NOT D'&MSGNR).INCREMENT_LOOP1
.* put message name in table entry
&MSGS(&COUNTER+1) SETC '&MSGNR'
.* increase counter and loop around
.INCREMENT_LOOP1  ANOP
&COUNTER          SETA  &COUNTER+1   * increase counter
                  AGO   .CHECK_LOOP1 * and loop around
.* done with loop 1
.STOP_LOOP1       ANOP
.*
.* define start of table with single byte offsets
&MSGPREF.00T      DS    0F
.* initialize index (sequence number)
&INDEX            SETA  0
.* start loop with 0
&COUNTER          SETA  0
.* stop looping when counter > 99
.CHECK_LOOP2      AIF   ('&COUNTER' GT '99').STOP_LOOP2
.* if there was a constant defined for this message, the entry in
.* MSGS is non blank
                  AIF   ('&MSGS(&COUNTER+1)' EQ '').EMPTY_MSG
.* in that case define a byte with the index in LWZn00T table
                  DC    AL1(&INDEX)
&INDEX            SETA  &INDEX+1  * and increase the index
                  AGO   .INCREMENT_LOOP2
.* when entry in MSGS is blank define a byte X'FF' in LWZn00T table
.EMPTY_MSG        ANOP
                  DC    X'FF'
.* increase counter and loop around
.INCREMENT_LOOP2  ANOP
&COUNTER          SETA  &COUNTER+1   * increase counter
                  AGO   .CHECK_LOOP2 * and loop around
.* done with loop 2
.STOP_LOOP2       ANOP
.*
.* define start of table with 2x2 byte (offset+length) entries
&MSGPREF.00X      DS    0F
.* start loop with 0
&COUNTER          SETA  0
.* stop looping when counter > 99
.CHECK_LOOP3      AIF   ('&COUNTER' GT '99').STOP_LOOP3
.* if there was a constant defined for this message, the entry in
.* MSGS is non blank
                  AIF   ('&MSGS(&COUNTER+1)' EQ '').INCREMENT_LOOP3
.* if so, copy message constant name to MSGNR
&MSGNR            SETC  '&MSGS(&COUNTER+1)'
.* and define an entry in LWZn00X table
                  DC    AL2(&MSGNR-&MSGPREF.00),AL2(L'&MSGNR)
.* increase counter and loop around
.INCREMENT_LOOP3  ANOP
&COUNTER          SETA  &COUNTER+1   * increase counter
                  AGO   .CHECK_LOOP3 * and loop around
.* done with loop 3
.STOP_LOOP3       ANOP
.* done with macro
                  MEND
*
* Messages LWZ000E-LWZ099E
*
LWZ000   DS    0F
LWZ001E  DC    C'LWZMRPT OPEN FAILED'
LWZ002E  DC    C'MAKEFILE OPEN FAILED'
LWZ003E  DC    C'PARSE ERROR'
LWZ010E  DC    C'BINDER ERROR'
LWZ011E  DC    C'BPX1STA ERROR'
*
* Generate LWZ000T and LWZ000X
*
         MTRCNTRS MSGPREF=LWZ0,MSGSUFFIX=E
*
* Messages LWZ600I-LWZ699I
*
LWZ600   DS    0F
LWZ601I  DC    C'LWZMAKE TRACE STARTED'
LWZ602I  DC    C'DCB OPENED'
LWZ603I  DC    C'DCB CLOSED'
LWZ604I  DC    C'SECTION STARTED'
LWZ605I  DC    C'SECTION ENDED'
LWZ606I  DC    C'PARAMETER RECEIVED'
LWZ608I  DC    C'START PARSE STATEMENT TYPE'
LWZ609I  DC    C'FINISH PARSE STATEMENT'
LWZ610I  DC    C'PARSED TOKEN'
LWZ611I  DC    C'PARSED CHAR '
LWZ612I  DC    C'START PARSE TOKEN TYPE'
LWZ620I  DC    C'STATEMENT BLOCK CONTENTS'
LWZ622I  DC    C'VARIABLE BLOCK CONTENTS'
LWZ638I  DC    C'TOKEN BLOCK ALLOCATE'
LWZ639I  DC    C'TOKEN BLOCK FREE'
LWZ640I  DC    C'STATEMENT BLOCK ALLOCATE'
LWZ641I  DC    C'STATEMENT BLOCK FREE'
LWZ642I  DC    C'VARIABLE BLOCK ALLOCATE'
LWZ643I  DC    C'VARIABLE BLOCK FREE'
LWZ644I  DC    C'TARGET BLOCK ALLOCATE'
LWZ645I  DC    C'TARGET BLOCK FREE'
LWZ646I  DC    C'PHONY BLOCK ALLOCATE'
LWZ647I  DC    C'PHONY BLOCK FREE'
LWZ648I  DC    C'EVAL BLOCK ALLOCATE'
LWZ649I  DC    C'EVAL BLOCK FREE'
LWZ650I  DC    C'VARIABLE VALUE ALLOCATE'
LWZ651I  DC    C'VARIABLE VALUE FREE'
LWZ660I  DC    C'DYNAMIC ALLOCATION'
LWZ661I  DC    C'DYNAMIC DEALLOCATION'
LWZ699I  DC    C'LWZMAKE TRACE ENDED'
*
* Generate LWZ600T and LWZ600X
*
         MTRCNTRS MSGPREF=LWZ6,MSGSUFFIX=I
*
***********************************************************************
* Section: LWZMAKE_RPT                                                *
* Purpose: This section writes a report line to the LWZMRPT DD.       *
*          Mostly invoked using the macro MLWZMRPT at the top of this *
*          source file.                                               *
*          On entry, G_LWZMRPT_LINE contains the line to be written.  *
*          If G_LWZMRPT_APND_LC is set to 'Y', G_SCAN_CURRLINE and    *
*          G_SCAN_CURRCOL are appended to the report line.            *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_RPT MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_RPT'
*
*        If G_LWZMRPT_APND_LC = 'Y', append line and column nr to line
         IF (CLI,G_LWZMRPT_APND_LC,EQ,C'Y') THEN
            BAL   R8,RPT_APPEND_LC  * Perform append line and column nr
            MVI   G_LWZMRPT_APND_LC,C'N' * Reset switch to 'N'
         ENDIF
*
WRITE    EQU   *
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer
         LA    R2,DCBRPT-DCB_DSECT(,R14) * Get addr of LWZMRPT DCB
*
         XR    R3,R3              * Clear R3
         LH    R3,G_LWZMRPT_CURRLINE * Load current line number
         IF (CLI,G_LWZMRPT_LINE,EQ,C' ') THEN * Check ASA ' ' char
            LA    R3,1(,R3)       * If found, advance 1 line number
            B     CHECK_CURRLINE  * Skip other ASA checks
         ENDIF
         IF (CLI,G_LWZMRPT_LINE,EQ,C'0') THEN * Check ASA '0' char
            LA    R3,2(,R3)       * If found, advance 2 line numbers
            B     CHECK_CURRLINE  * Skip other ASA checks
         ENDIF
         IF (CLI,G_LWZMRPT_LINE,EQ,C'-') THEN * Check ASA '-' char
            LA    R3,3(,R3)       * If found, advance 3 line numbers
            B     CHECK_CURRLINE  * Skip other ASA checks
         ENDIF
         CLI   G_LWZMRPT_LINE,C'1' * Check ASA '1' char
         BE    PAGE_SKIP          * If found, jump to page skip
CHECK_CURRLINE EQU *
         STH   R3,G_LWZMRPT_CURRLINE * Store the new line number
         CH    R3,=AL2(LINES_PER_PAGE) * Check if we crossed page bndry
         BNH   NO_PAGE_SKIP       * If not, don't do page skip
PAGE_SKIP DS   0H
         L     R4,G_LWZMRPT_CURRPAGE * Get current page number
         LA    R4,1(,R4)          * page number = page number + 1
         ST    R4,G_LWZMRPT_CURRPAGE * and put it back
         CVD   R4,G_DEC8          * convert to packed decimal
         UNPK  G_ZONED8,G_DEC8    * convert to zoned
         OI    G_ZONED8+7,X'F0'   * get rid of sign nibble
         MVC   G_PAGE_HEADER_PAGENR,G_ZONED8 * put page nr in header
*        Get the current date+time
         TIME  DEC,G_TIMEDATE,ZONE=LT,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD
*        Convert time part to zoned
         UNPK  G_TIMEDATEZ+10(10),G_TIMEDATE(5)
*        Convert date part to zoned
         UNPK  G_TIMEDATEZ(10),G_TIMEDATE+8(5)
*        Put date and time in header
         MVC   G_PAGE_HEADER_DAY,G_TIMEDATEZ+7
         MVC   G_PAGE_HEADER_MONTH,G_TIMEDATEZ+5
         MVC   G_PAGE_HEADER_YEAR,G_TIMEDATEZ+1
         MVC   G_PAGE_HEADER_HOUR,G_TIMEDATEZ+11
         MVC   G_PAGE_HEADER_MINUTE,G_TIMEDATEZ+13
         MVC   G_PAGE_HEADER_SECOND,G_TIMEDATEZ+15
         PUT   (R2),G_PAGE_HEADER * Write a report line to LWZMRPT
         MVI   G_LWZMRPT_LINE,C'0' * Overwrite ASA char, skip 2 lines
         LH    R3,=H'1'           * Set R3 to 1
         STH   R3,G_LWZMRPT_CURRLINE * and store it as current line nr
         B     WRITE              * Jump back to write the actual line
NO_PAGE_SKIP DS 0H
         PUT   (R2),G_LWZMRPT_LINE * Write a report line to LWZMRPT
*
RET_RPT  EQU   *
         MLWZTERM                 * Return back to caller
*
* Append ' at line x column y' to print line, only performed when
* G_LWZMRPT_APND_LC is set to 'Y'
*
RPT_APPEND_LC EQU *
*        Initialize helper var
         MVC   G_LWZMRPT_HELPER,=CL80' at line'
         LA    R7,G_LWZMRPT_HELPER+9 * Point R7 to where line nr starts
*
         L     R15,G_SCAN_CURRLINE * Get the current line number
         CVD   R15,G_DEC8         * Convert it to packed decimal
         UNPK  G_ZONED8,G_DEC8    * Convert it to zoned
         OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble
*
*        Left trim the line number of leading zeros
         LA    R2,G_ZONED8        * Point R2 to line number
         LA    R3,L'G_ZONED8-1    * Put byte counter - 1 in R3, so
*                                 * we're always left with min. 1 byte
*                                 * and R3 is ready to use with EX
RPT_TRIM_NR1 EQU *
         CLI   0(R2),C'0'         * Is this a zero?
         BNE   RPT_TRIM_NR1_END   * Nope, stop trimming
         LA    R2,1(,R2)          * R2 = R2 + 1
         BCT   R3,RPT_TRIM_NR1    * R3 = R3 - 1 until R3 = 0
RPT_TRIM_NR1_END EQU *
         B     *+10               * Skip MVC constant for EX
         MVC   0(1,R7),0(R2)      * MVC constant for EX
         EX    R3,*-6             * EX previous MVC statement with R3
         LA    R3,1(,R3)          * R3 = R3 + 1
         AR    R7,R3              * Advance R7 past line number
*
         MVC   0(8,R7),=C' column ' * Append constant
         LA    R7,8(,R7)          * Advance R7 to point after constant
*
         L     R15,G_SCAN_CURRCOL * Get the current column number
         CVD   R15,G_DEC8         * Convert it to packed decimal
         UNPK  G_ZONED8,G_DEC8    * Convert it to zoned
         OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble
*
*        Left trim the column number of leading zeros
         LA    R2,G_ZONED8        * Point R2 to column number
         LA    R3,L'G_ZONED8-1    * Put byte counter - 1 in R3, so
*                                 * we're always left with min. 1 byte
*                                 * and R3 is ready to use with EX
RPT_TRIM_NR2 EQU *
         CLI   0(R2),C'0'         * Is this a zero?
         BNE   RPT_TRIM_NR2_END   * Nope, stop trimming
         LA    R2,1(,R2)          * R2 = R2 + 1
         BCT   R3,RPT_TRIM_NR2    * R3 = R3 - 1 until R3 = 0
RPT_TRIM_NR2_END EQU *
         B     *+10               * Skip MVC constant for EX
         MVC   0(1,R7),0(R2)      * MVC constant for EX
         EX    R3,*-6             * EX previous MVC statement with R3
         LA    R3,1(,R3)          * R3 = R3 + 1
         AR    R7,R3              * Advance R7 past column number
*
*        Calculate actual length of helper var
         LA    R6,G_LWZMRPT_HELPER * Point R6 to helper var
         SR    R7,R6              * Subtract start from R7
*
*        Trim the report line of trailing spaces
         LA    R2,G_LWZMRPT_LINE+L'G_LWZMRPT_LINE-1 * Point R2 to last
*                                 * byte of report line
         LA    R3,L'G_LWZMRPT_LINE-1 * R3 = length of line - 1, so
*                                 * we're always left with min. 1 byte
RPT_TRIM_LINE EQU *
         CLI   0(R2),C' '         * Is this a space?
         BNE   RPT_TRIM_LINE_DONE * Nope, stop trimming
         BCTR  R2,R0              * R2 = R2 - 1
         BCT   R3,RPT_TRIM_LINE   * R3 = R3 - 1 until R3 = 0
RPT_TRIM_LINE_DONE EQU *
         LA    R2,1(,R2)          * Point R2 past last non space
         LA    R5,G_LWZMRPT_LINE+L'G_LWZMRPT_LINE * Point R5 past end
*                                 * of report line
         SR    R5,R2              * Calculate room left
         BO    RPT_APPEND_LC_END  * No room left, skip rest of append
         CR    R5,R7              * Check if room left is more than
         IF (H) THEN              * the room needed for append string
            LR    R5,R7           * If so, use length of append string
         ENDIF
         BCTR  R5,R0              * R5 = R5 - 1 because of EX
         B     *+10               * Skip MVC constant for EX
         MVC   0(1,R2),G_LWZMRPT_HELPER * MVC constant for EX
         EX    R5,*-6             * EX previous MVC statement with R5
*
RPT_APPEND_LC_END EQU *
         BR    R8                 * Return
*
         LTORG
*
***********************************************************************
* Section: LWZMAKE_APPEND_TOKEN                                       *
* Purpose: This section appends token 1 to either token 2 or 3 and    *
*          takes care of allocating a larger memory block if there's  *
*          not enough room.                                           *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_APPEND_TOKEN MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_APPEND_TOKEN'
*
*        If APPEND_TO != X'00' then skip to end
         CLI   G_SCAN_APPEND_TO,X'00'
         BE    APPEND_RET
*
*        If token 1 length = 0 then skip to end
         LT    R2,G_SCAN_TOKEN_LEN
         BZ    APPEND_RET
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'01') THEN * Append to token 2
            L     R2,G_SCAN_TOKEN2_LEN * Get length of token 2
            A     R2,G_SCAN_SPACE_COUNT * Add number of spaces
            A     R2,G_SCAN_TOKEN_LEN  * Add length of token 1
            C     R2,G_SCAN_TOKEN2_MAXLEN * Will it fit?
            IF (H) THEN
               L     R3,G_SCAN_TOKEN2_MAXLEN * Get current max length
               LR    R6,R3            * Save it for storage release
               SLL   R3,1             * Multiply max length by 2
               ST    R3,G_SCAN_TOKEN2_MAXLEN * Make it new max length
               MVC   G_STGOR_LEN,G_SCAN_TOKEN2_MAXLEN
               MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
               L     R15,LWZMAKE_STG_OBTAINA_APPEND
               BASR  R14,R15
               L     R0,G_STGOR_PTR   * Have R0 point to new block
               L     R1,G_SCAN_TOKEN2_LEN * Get length of token 2
               L     R2,G_SCAN_TOKEN2A * Have R2 point to old block
               LR    R5,R2            * Save it for storage release
               LR    R3,R1            * Make sure no cropping/filling
               ST    R0,G_SCAN_TOKEN2A * Save ptr to new block
               MVCL  R0,R2            * Copy old to new block
               ST    R6,G_STGOR_LEN
               ST    R5,G_STGOR_PTR
               MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
               L     R15,LWZMAKE_STG_RELEASEA_APPEND
               BASR  R14,R15
            ENDIF
            L     R2,G_SCAN_TOKEN2A    * Point R2 to token 2
            A     R2,G_SCAN_TOKEN2_LEN * Add length of token 2
            L     R3,G_SCAN_TOKEN2_LEN * Get length of token 2
            A     R3,G_SCAN_SPACE_COUNT * Add number of spaces
            A     R3,G_SCAN_TOKEN_LEN  * Add length of token 1
            ST    R3,G_SCAN_TOKEN2_LEN * Put it back as new len
         ELSE
            IF (CLI,G_SCAN_APPEND_TO,EQ,X'02') THEN * Append token 3
               L     R2,G_SCAN_TOKEN3_LEN * Get length of token 3
               A     R2,G_SCAN_SPACE_COUNT * Add number of spaces
               A     R2,G_SCAN_TOKEN_LEN  * Add length of token 1
               C     R2,G_SCAN_TOKEN3_MAXLEN * Will it fit?
               IF (H) THEN
                  L     R3,G_SCAN_TOKEN3_MAXLEN * Get current max len
                  LR    R6,R3         * Save it for storage release
                  SLL   R3,1          * Multiply max length by 2
                  ST    R3,G_SCAN_TOKEN3_MAXLEN * Make it new max len
                  MVC   G_STGOR_LEN,G_SCAN_TOKEN3_MAXLEN
                  MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
                  L     R15,LWZMAKE_STG_OBTAINA_APPEND
                  BASR  R14,R15
                  L     R0,G_STGOR_PTR * Have R0 point to new block
                  L     R1,G_SCAN_TOKEN3_LEN * Get length of token 3
                  L     R2,G_SCAN_TOKEN3A * Have R2 point to old block
                  LR    R5,R2         * Save it for storage release
                  LR    R3,R1         * Make sure no cropping/filling
                  ST    R0,G_SCAN_TOKEN3A * Save ptr to new block
                  MVCL  R0,R2            * Copy old to new block
                  ST    R6,G_STGOR_LEN
                  ST    R5,G_STGOR_PTR
                  MVI   G_STGOR_TYPE,STGOR_TYPE_TOKEN
                  L     R15,LWZMAKE_STG_RELEASEA_APPEND
                  BASR  R14,R15
               ENDIF
               L     R2,G_SCAN_TOKEN3A    * Point R0 to token 3
               A     R2,G_SCAN_TOKEN3_LEN * Add length of token 3
               L     R3,G_SCAN_TOKEN3_LEN * Get length of token 3
               A     R3,G_SCAN_SPACE_COUNT * Add number of spaces
               A     R3,G_SCAN_TOKEN_LEN  * Add length of token 1
               ST    R3,G_SCAN_TOKEN3_LEN * Put it back as new len
            ENDIF
         ENDIF
         LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?
         IF (NZ) THEN               * Yep...
            MVI   0(R2),C' '        * Fill in first space
            L     R1,G_SCAN_SPACE_COUNT * Put number of spaces in R1
            S     R1,=F'2'          * Minus 1 for first space and minus
*                                   * another one for EX
            IF CC=10 THEN           * If R1 >= 0
               B     *+10           * Skip MVC constant for EX
               MVC   1(1,R2),0(R2)  * MVC constant for EX
               EX    R1,*-6         * EX previous MVC statement with R1
            ENDIF
            A     R2,G_SCAN_SPACE_COUNT
         ENDIF
         L     R4,G_SCAN_TOKENA * Point R2 to token 1
         L     R5,G_SCAN_TOKEN_LEN * Get length of token 1
         LR    R3,R5           * Make sure no cropping/filling
         MVCL  R2,R4           * Append to either token 2 or 3
*
APPEND_RET EQU   *
         MLWZTERM                 * Return back to caller
*
         LTORG
*
LWZMAKE_STG_OBTAINA_APPEND  DC    A(LWZMAKE_STG_OBTAIN)
LWZMAKE_STG_RELEASEA_APPEND DC    A(LWZMAKE_STG_RELEASE)
*
***********************************************************************
* Section: LWZMAKE_STG_OBTAIN                                         *
* Purpose: This section does a STORAGE OBTAIN and optionally writes a *
*          trace record.                                              *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_STG_OBTAIN MLWZSAVE
*
         L     R2,G_STGOR_LEN
         STORAGE OBTAIN,LENGTH=(R2)
         ST    R1,G_STGOR_PTR
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEBUG) THEN
            ST    R1,G_DEC8    * Put ptr in area of at least 5 bytes
            UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex
            TR    G_ZONED8,STG_OBTAIN_HEXTAB * Turn into hex
            MVC   G_HELPER_DATA(8),G_ZONED8 * Copy hex to helper dat
            LA    R14,G_HELPER_DATA       * Get ptr to helper data
            ST    R14,G_LWZMTRC_DATA_PTR  * Save it for trace data
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace data length 8
            SELECT CLI,G_STGOR_TYPE,EQ
            WHEN STGOR_TYPE_TOKEN
               MVC   G_LWZMTRC_MSGNR,=C'638'
            WHEN STGOR_TYPE_STMT
               MVC   G_LWZMTRC_MSGNR,=C'640'
            WHEN STGOR_TYPE_VAR
               MVC   G_LWZMTRC_MSGNR,=C'642'
            WHEN STGOR_TYPE_TGT
               MVC   G_LWZMTRC_MSGNR,=C'644'
            WHEN STGOR_TYPE_PNY
               MVC   G_LWZMTRC_MSGNR,=C'646'
            WHEN STGOR_TYPE_EVAL
               MVC   G_LWZMTRC_MSGNR,=C'648'
            WHEN STGOR_TYPE_VARVAL
               MVC   G_LWZMTRC_MSGNR,=C'650'
            ENDSEL
*
            L     R15,G_LWZMAKE_TRACEA  * Get address of trace section
            BASR  R14,R15               * Link to trace section
*
            L     R1,G_STGOR_PTR
         ENDIF
*
STG_OBTAIN_RET EQU   *
         MLWZTERM                 * Return back to caller
*
         LTORG
*
* Translate table for conversion to hex
                            DS    0F
STG_OBTAIN_HEXTAB           EQU   *-C'0'
                            DC    C'0123456789ABCDEF'
*
***********************************************************************
* Section: LWZMAKE_STG_RELEASE                                        *
* Purpose: This section does a STORAGE RELEASE and optionally writes  *
*          a trace record.                                            *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_STG_RELEASE MLWZSAVE
*
         L     R3,G_STGOR_PTR
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEBUG) THEN
            ST    R3,G_DEC8    * Put ptr in area of at least 5 bytes
            UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex
            TR    G_ZONED8,STG_RELEASE_HEXTAB * Turn into hex
            MVC   G_HELPER_DATA(8),G_ZONED8 * Copy hex to helper dat
            LA    R14,G_HELPER_DATA       * Get ptr to helper data
            ST    R14,G_LWZMTRC_DATA_PTR  * Save it for trace data
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace data length 8
            SELECT CLI,G_STGOR_TYPE,EQ
            WHEN STGOR_TYPE_TOKEN
               MVC   G_LWZMTRC_MSGNR,=C'639'
            WHEN STGOR_TYPE_STMT
               MVC   G_LWZMTRC_MSGNR,=C'641'
            WHEN STGOR_TYPE_VAR
               MVC   G_LWZMTRC_MSGNR,=C'643'
            WHEN STGOR_TYPE_TGT
               MVC   G_LWZMTRC_MSGNR,=C'645'
            WHEN STGOR_TYPE_PNY
               MVC   G_LWZMTRC_MSGNR,=C'647'
            WHEN STGOR_TYPE_EVAL
               MVC   G_LWZMTRC_MSGNR,=C'649'
            WHEN STGOR_TYPE_VARVAL
               MVC   G_LWZMTRC_MSGNR,=C'651'
            ENDSEL
*
            L     R15,G_LWZMAKE_TRACEA  * Get address of trace section
            BASR  R14,R15               * Link to trace section
         ENDIF
*
         L     R2,G_STGOR_LEN
         STORAGE RELEASE,LENGTH=(R2),ADDR=(R3)
*
STG_RELEASE_RET EQU   *
         MLWZTERM                 * Return back to caller
*
         LTORG
*
* Translate table for conversion to hex
                            DS    0F
STG_RELEASE_HEXTAB          EQU   *-C'0'
                            DC    C'0123456789ABCDEF'
*
***********************************************************************
* Section: LWZMAKE_PHASE1                                             *
* Purpose: This section performs phase 1 of executing a makefile.     *
*          During this phase the makefile source is parsed, a linked  *
*          list of statements is created and a binary search tree of  *
*          variables.                                                 *
*          Parsing is done by invoking section LWZMAKE_SCAN_STMT      *
*          statement by statement. It invokes section                 *
*          LWZMAKE_SCAN_TOKEN which in turn invokes LWZMAKE_SCAN_CHAR.*
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_PHASE1 MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_PHASE1'
*
*        Write report line to start parsing
         MLWZMRPT RPTLINE=CL133' Phase 1 parsing .....'
*
NEXTSTMT L     R15,LWZMAKE_SCAN_STMTA_PHASE1 * Get address of scan stmt
*                                            * section
         BASR  R14,R15            * Link to scan stmt section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   BREAK_STMT_LOOP    * Yes, stop looping
         CLI   G_SCAN_INPUT_STACK_IDX,X'00' * No more input?
         BE    BREAK_STMT_LOOP    * Stop looping
         B     NEXTSTMT           * In all other cases loop around
*
BREAK_STMT_LOOP EQU *
*
PHASE1_RET EQU *
         MLWZTERM                 * Return back to caller
*
         LTORG
*
* Local constant pointers to section addresses
LWZMAKE_SCAN_STMTA_PHASE1   DC    A(LWZMAKE_SCAN_STMT)
*
***********************************************************************
* Section: LWZMAKE_PHASE2                                             *
* Purpose: This section performs phase 2 of executing a makefile,     *
*          starting with the executing of the first target and        *
*          recursively going through any prerequisite targets.        *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_PHASE2 MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_PHASE2'
*
*        Write report line to start executing
         MLWZMRPT RPTLINE=CL133' Phase 2 executing ...'
*
*        Check if a default target is filled
         IF (CLC,G_DEFAULT_TARGET,NE,=CL72' ') THEN
            L     R2,G_SCAN_TOKENA
            MVC   0(L'G_DEFAULT_TARGET,R2),G_DEFAULT_TARGET
            LA    R2,71(,R2)
            L     R3,=A(71)
TRIM_DEFAULT_TARGET_CHAR EQU *
            IF (CLI,0(R2),EQ,C' ') THEN
               BCTR  R2,R0
               BCT   R3,TRIM_DEFAULT_TARGET_CHAR
            ENDIF
            LA    R3,1(,R3)
            ST    R3,G_SCAN_TOKEN_LEN
*
            L     R15,LWZMAKE_FINDTGTA_PHASE2
            BASR  R14,R15
*
            CLC   G_RETCODE,=F'0'
            BNE   PHASE2_RET
*
            IF (CLC,G_FOUND_TGT_PTR,EQ,=A(0)) THEN
               MLWZMRPT RPTLINE=CL133'0Default target not found'
               MVC   G_RETCODE,=F'8'
               B     PHASE2_RET
            ENDIF
         ELSE
            IF (CLC,G_FIRST_TGT_PTR,NE,=A(0)) THEN
               MVC   G_FOUND_TGT_PTR,G_FIRST_TGT_PTR
            ELSE
               MLWZMRPT RPTLINE=CL133'0No targets found'
               B     PHASE2_RET
            ENDIF
         ENDIF
*
*        Fill execute target parameter block for first target
         LA    R1,G_EXEC_TGT_PAR1 * Point R1 to parameter block
*        Put target pointer in parameter block
         MVC   EXEC_TGT_PTR-EXEC_TGT_PAR(4,R1),G_FOUND_TGT_PTR
         ST    R1,G_EXEC_TGT_PAR1A * Store address of parameter block
         LA    R1,G_EXEC_TGT_PAR1A * Load address of param block ptr
         L     R15,LWZMAKE_EXEC_TGTA_PHASE2 * Get address of EXEC_TGT
         BASR  R14,R15             * Link to EXEC_TGT section
*
PHASE2_RET EQU *
         MLWZTERM                 * Return back to caller
*
         LTORG
*
* Local constant pointers to section addresses
LWZMAKE_FINDTGTA_PHASE2     DC    A(LWZMAKE_FINDTGT)
LWZMAKE_EXEC_TGTA_PHASE2    DC    A(LWZMAKE_EXEC_TGT)
*
***********************************************************************
* Section: LWZMAKE_SCAN_STMT                                          *
* Purpose: This section performs the parsing of statements. As long   *
*          as the statement is not finished it keeps calling          *
*          LWZMAKE_SCAN_TOKEN for the next keyword. In most cases the *
*          first 2 tokens are needed to determine the type of state-  *
*          ment. 2 tokens are needed to determine the type of state-  *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_SCAN_STMT MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_SCAN_STMT'
*
*        Before resetting scan state, remember if previous statement
*        was in recipe
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN
            MVI   G_PREV_STMT_IN_RECIPE,C'Y'
         ELSE
            MVI   G_PREV_STMT_IN_RECIPE,C'N'
         ENDIF
*        Reset scan state to initial state NOT_IN_STMT
         MVI   G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT
*        Reset token lengths to 0
         MVC   G_SCAN_TOKEN_LEN,=F'0'
         MVC   G_SCAN_TOKEN2_LEN,=F'0'
         MVC   G_SCAN_TOKEN3_LEN,=F'0'
*
*        Get the first token
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_STMT_RET      * Yes, stop parsing statement
         CLI   G_MKFEOF,C'Y'      * Are we at EOF makefile?
         BE    SCAN_STMT_RET      * If so, stop parsing statement
*
*        Only a rule type statement can start with a $ variable
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_VARIABLE) THEN
*           So the second token is not needed
            BAL   R8,STMT_RULE    * Perform parsing of rule statement
            B     SCAN_STMT_RET   * Statement parsed in subroutine
*                                 * so stop parsing
         ENDIF
*
*        Only a rule type statement can start with a /
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_SLASH) THEN
            OI    G_SCAN_STATE,SCAN_STATE_IN_RULE
*           So the second token is not needed
            BAL   R8,STMT_RULE    * Perform parsing of rule statement
            B     SCAN_STMT_RET   * Statement parsed in subroutine
*                                 * so stop parsing
         ENDIF
*
*        Only a call type statement can start with the CALL keyword
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_CALL) THEN
*           So the second token is not needed
            BAL   R8,STMT_CALL    * Perform parsing of call statement
            B     SCAN_STMT_RET   * Statement parsed in subroutine
*                                 * so stop parsing
         ENDIF
*
*        If first token is .PHONY the statement is a PHONY
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_SPECIAL) THEN
            CLC   G_SCAN_TOKEN_LEN,=A(6)
            IF (EQ) THEN
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1
               CLC   0(6,R14),=C'.PHONY'  * Is it .PHONY?
               IF (EQ) THEN               * If so...
                  BAL   R8,STMT_PHONY     * Perform parsing PHONY stmt
                  B     SCAN_STMT_RET     * Statement parsed in
*                                         * subroutine so stop parsing
               ENDIF
            ENDIF
         ENDIF
*
*        If first token is .INCLUDE the statement is an INCLUDE
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_SPECIAL) THEN
            CLC   G_SCAN_TOKEN_LEN,=A(8)
            IF (EQ) THEN
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1
               CLC   0(6,R14),=C'.INCLUDE' * Is it .INCLUDE?
               IF (EQ) THEN               * If so...
                  IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN
                     MLWZMRPT RPTLINE=CL133'0.INCLUDE not allowed in reX
               cipe'
                     MVC   G_RETCODE,=F'8'
                     B     SCAN_STMT_RET
                  ENDIF
                  BAL   R8,STMT_INCLUDE   * Perform parsing INCLUDE
                  B     SCAN_STMT_RET     * Statement parsed in
*                                         * subroutine so stop parsing
               ENDIF
            ENDIF
         ENDIF
*
*        Copy token 1 to token 2 so it can be used for next SCAN_TOKEN
         MVI   G_SCAN_APPEND_TO,X'01'
         MVC   G_SCAN_SPACE_COUNT,=A(0)
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_STMT
         OI    G_SCAN_STATE,SCAN_STATE_IN_STMT
*
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_STMT_RET      * Yes, stop parsing statement
*
*        If second token is an operator the statement is an assignment
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_OPERATOR) THEN
            BAL   R8,STMT_ASSIGNMENT * Perform parsing of assignment
            B     SCAN_STMT_RET   * Statement parsed in subroutine
*                                 * so stop parsing
         ENDIF
*
*        If second token is a rule separator the statement is a rule
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN
            BAL   R8,STMT_RULE    * Perform parsing of rule statement
            B     SCAN_STMT_RET   * Statement parsed in subroutine
*                                 * so stop parsing
         ENDIF
*
*        If first and second token are both regular keywords the
*        statement is a rule
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN
            IF (CLI,G_SCAN_TOKENTYPE2,EQ,SCAN_TOKENTYPE_NORMAL) THEN
               BAL   R8,STMT_RULE  * Perform parsing of rule statement
               B     SCAN_STMT_RET * Statement parsed in subroutine
*                                  * so stop parsing
            ENDIF
         ENDIF
*
*        If first token is normal and second token is a decimal point
*        it's probably a constant data set name, so it's a rule
         IF (CLI,G_SCAN_TOKENTYPE,EQ,X'00'),AND,                       X
               (CLI,G_SCAN_TOKENTYPE2,EQ,SCAN_TOKENTYPE_NORMAL) THEN
            IF (CLC,G_SCAN_TOKEN_LEN,EQ,=F'1') THEN
               L     R14,G_SCAN_TOKENA
               IF (CLI,0(R14),EQ,C'.') THEN
                  OI    G_SCAN_STATE,SCAN_STATE_IN_RULE
*
                  BAL   R8,STMT_RULE  * Perform parsing of rule stmt
                  B     SCAN_STMT_RET * Statement parsed in subroutine
*                                     * so stop parsing
               ENDIF
            ENDIF
         ENDIF
*
*        No valid combination of keywords found, so report syntax error
*        and give off return code 8
         MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'
         MVC   G_RETCODE,=F'8'
*
SCAN_STMT_RET EQU *
         MLWZTERM                 * Return back to caller
*
* STMT assignment (e.g. 'foo = bar')
* At this point 2 tokens have been scanned, the assignment destination,
* which has been copied to token 2 and the operator still in token 1.
* From here on the source text (right side of the assignment) is parsed
* into token 3. When the statement is finished it's converted to
* internal memory format and added to the statement linked list. Also
* the variable and it's current value are added/updated to the variable
* binary search tree.
*
STMT_ASSIGNMENT EQU *
*        Write a trace record for statement type assignment
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'ASSIGNX
               MENT'
*
*        Save operator so token 1 can be reused
         L     R5,G_SCAN_TOKENA   * Point R5 to token 1
         L     R6,G_SCAN_TOKEN_LEN * Put token 1 length in R6
         C     R6,=F'2'           * Was the operator 2 bytes?
         IF (EQ) THEN             * If so...
            MVC   G_STMT_SAVE_OP(2),0(R5) * copy 2 bytes
         ELSE                     * otherwise...
            MVC   G_STMT_SAVE_OP(1),0(R5) * copy 1 byte
            MVI   G_STMT_SAVE_OP+1,X'00'  * and add a null char
         ENDIF
*
         L     R6,G_SCAN_TOKEN2_LEN * Get length of variable name
         C     R6,=A(L'STMT_A_DEST) * Check if it fits
         IF (H) THEN                * If not, write error and stop
            MLWZMRPT RPTLINE=CL133'0Internal error, variable name longeX
               r than 72',APND_LC=C'Y'
            MVC   G_RETCODE,=F'12'  * Set return code 12
            BR    R8                * and return
         ENDIF
*
         MVI   G_DO_EXPAND,C'Y'
         MVI   G_DO_ASSIGN,C'Y'
*
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN
            MVI   G_DO_EXPAND,C'N'
            MVI   G_DO_ASSIGN,C'N'
            B     STMT_A_START
         ENDIF
*
         IF (CLI,G_STMT_SAVE_OP,EQ,C'=') THEN
            MVI   G_DO_EXPAND,C'N'
            B     STMT_A_START
         ENDIF
*
         IF (CLC,G_STMT_SAVE_OP,EQ,=C'?=') THEN
*           Copy variable name to FINDVAR name
            L     R1,G_SCAN_TOKEN2_LEN * Get length of variable name
            STH   R1,G_SRCH_VAR_LEN * Put length in FINDVAR search len
            LA    R0,G_SRCH_VAR   * Point R0 to FINDVAR search name
            L     R2,G_SCAN_TOKEN2A * Point R2 to token 2
            LR    R3,R1           * Make sure no cropping/filling
            MVCL  R0,R2           * Copy variable name to FINDVAR name
*
            L     R15,LWZMAKE_FINDVARA_STMT * Get address to FINDVAR
            BASR  R14,R15         * Link to FINDVAR section
*
            LT    R4,G_FOUND_VAR_PTR * Check if a pointer was returned
            IF (NZ) THEN
               MVI   G_DO_EXPAND,C'N'
               MVI   G_DO_ASSIGN,C'N'
               B     STMT_A_START
            ENDIF
         ENDIF
*
STMT_A_START EQU *
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_ASSIGN
         OI    G_SCAN_STATE,SCAN_STATE_IN_ASSIGN
*        Clear token 3 length, which will receive the assignment source
         MVC   G_SCAN_TOKEN3_LEN,=F'0'
*        Append to token 3
         MVI   G_SCAN_APPEND_TO,X'02'
*
STMT_A_NEXT_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement
*
*        Check if scan state was reset to NOT_IN_STMT, meaning this
*        statement was finished
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt
         BE    STMT_A_FINISH      * If so, statement done
*
*        Check if we've hit a $() variable and we're in a simply
*        expanded variable assignment (:=). In that case the variable
*        is immediately resolved.
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()
         IF (NE) THEN
            C     R14,=A(SCAN_STATE_IN_VARIABLER)
         ENDIF
         IF (EQ) THEN             * If so...
            IF (CLI,G_DO_EXPAND,EQ,C'Y') THEN
               MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1
            ENDIF
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR
            BASR  R14,R15      * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement
*
            B     STMT_A_NEXT_TOKEN * Loop around to get next token
         ENDIF
*
*        Append token 1 to token 3
         MVI   G_SCAN_APPEND_TO,X'02'
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3
         IF (Z) THEN                * Is this the first part of token 3
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces
         ENDIF
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_ASSIGN2
         OI    G_SCAN_STATE,SCAN_STATE_IN_ASSIGN2
*
         B     STMT_A_NEXT_TOKEN  * Loop around to get next token
*
STMT_A_FINISH EQU *
*        Write trace record that statement is finished
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'
*
*        Allocate a new memory block for this assignment
         L     R1,=A(STMT_A_DSECT_LEN) * Size of block without token
         A     R1,G_SCAN_TOKEN3_LEN    * Add token length
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_ASSIGNMENT * New block is
*                                 * for type assignment
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT
         BASR  R14,R15            * Link to ALLOC_STMT section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement
*
*        Get returned pointer to new block of memory and put it in R7
*        It should stay in R7 for the LWZMAKE_STORE_VAR section
         LT    R7,G_STMT_ALLOC_RETURN_PTR
         BZ    STMT_ASSIGNMENT_RET * If it was zero, stop parsing
*                                 * (failsafe, shouldn't happen)
*
         USING STMT_A_DSECT,R7    * Address with assignment DSECT
*
*        Fill in the operator in assignment block
         MVC   STMT_A_OPERATOR,G_STMT_SAVE_OP * Copy 2 bytes operator
*
*        Fill in destination variable name (token 2) in assignment blk
         L     R6,G_SCAN_TOKEN2_LEN * Get length of variable name
         STH   R6,STMT_A_DESTLEN  * Put variable name length in block
         L     R5,G_SCAN_TOKEN2A  * Point R5 to token 2
         LA    R4,STMT_A_DEST     * Point R4 to var name in block
         BCTR  R6,R0              * Length minus 1 for EX
         B     *+10               * Skip MVC constant for EX
         MVC   0(1,R4),0(R5)      * MVC constant for EX
         EX    R6,*-6             * EX previous MVC statement with R6
*
*        Fill in source text (token 3) in assignment block
         LA    R0,STMT_A_SRC        * Point R0 to source in block
         L     R1,G_SCAN_TOKEN3_LEN * Get length of source
         STH   R1,STMT_A_SRCLEN     * Store length in block
         L     R2,G_SCAN_TOKEN3A    * Point R2 to token 3
         LR    R3,R1                * Make sure no cropping/filling
         MVCL  R0,R2                * Copy source text
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEBUG) THEN
            ST    R7,G_LWZMTRC_DATA_PTR
            L     R14,0(,R7)
            STH   R14,G_LWZMTRC_DATA_SIZ
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'620',DATA
         ENDIF
*
*        Check for assignment of special var
         L     R14,G_SCAN_TOKEN2A * Point R14 to token 2
         IF (CLI,0(R14),EQ,C'.') THEN * If token 2 starts with .
            CLC   G_SCAN_TOKEN2_LEN,=F'13' * Is length 13?
            IF (EQ) THEN              * Yes, so it can be recipepref
               CLC   0(13,R14),=C'.RECIPEPREFIX' * Is it?
               BNE   STMT_ASSIGNMENT_UNKNOWN_SPECIAL * No, error
               CLC   G_SCAN_TOKEN3_LEN,=F'1' * Was source text 1 pos?
               BNE   STMT_ASSIGNMENT_WRONG_REPPREFLEN * No, error
               L     R14,G_SCAN_TOKEN3A    * Point R14 to token 3
               MVC   G_RECIPEPREFIX,0(R14) * Copy recipeprefix
               B     STMT_ASSIGNMENT_RET   * Skip the rest
            ELSE                      * Length is not 13
               B     STMT_ASSIGNMENT_UNKNOWN_SPECIAL * so error
            ENDIF
         ENDIF
*
*        Add/update the variable to binary search tree for vars
         IF (CLI,G_DO_ASSIGN,EQ,C'Y') THEN
*           R7 points to the assignment statement
            L     R15,LWZMAKE_STORE_VARA_STMT * Get address STORE_VAR
            BASR  R14,R15              * Link to STORE_VAR section
         ENDIF
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement
*
*        Remember this was an assignment for next statement's previous
*        statement type
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_ASSIGNMENT
*
STMT_ASSIGNMENT_RET EQU *
         BR    R8
*
STMT_ASSIGNMENT_UNKNOWN_SPECIAL EQU *
         MLWZMRPT RPTLINE=CL133'0Unknown special variable',APND_LC=C'Y'
         MVC   G_RETCODE,=F'8'
         BR    R8
*
STMT_ASSIGNMENT_WRONG_REPPREFLEN EQU *
         MLWZMRPT RPTLINE=CL133'0Recipeprefix can only be 1 character',X
               APND_LC=C'Y'
         MVC   G_RETCODE,=F'8'
         BR    R8
*
         DROP  R7
*
* STMT rule 'bla : jodel'
* At this point 1 or 2 tokens have been scanned, 1 in the case the
* statement starts with a $() variable, 2 in either the case of a
* token followed by a rule separator (:), or in the case of 2 normal
* tokens.
* From here on keywords are parsed and appended to token 2 until a rule
* separator is encountered, so that token 2 contains the target name(s)
* After that keywords are parsed and appended to token 3 until end of
* statement, so that token 3 contains the requisite(s).
* Any variables encountered in the target name(s) are resolved here,
* but variables in the requisites are left intact and are resolved in
* phase 2.
* When the statement is finished it's converted to internal memory
* format and added to the statement linked list.
*
STMT_RULE EQU  *
*        Write a trace record for statement type rule
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'RULE'
*
*        Check if we already have a rule separator, if so save it as
*        the operator and skip to target token complete
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN
            MVI   G_STMT_SAVE_OP,C':' * Save : as operator
            MVI   G_STMT_SAVE_OP+1,X'00' * Append null char
            B     STMT_R_TGT_TOKEN_COMPLETE * Skip parsing target name
         ELSE
            MVC   G_STMT_SAVE_OP,=X'0000' * Initialize operator
*           Check if we ended up here because statement started with
*           a $() variable, in that case do a nasty jump right into
*           the keyword scanning loop below to expand this variable
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_VARIABLE
            BE    STMT_R_SCAN_VAR
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_SLASH
            BE    STMT_R_TGT_TOKEN_APPEND
            CLI   G_SCAN_TOKENTYPE,X'00'
            BE    STMT_R_TGT_TOKEN_APPEND
         ENDIF
*
STMT_R_NEXT_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_RULE_RET      * Yes, stop parsing statement
*
*        Check if we have a rule separator, if so save it as the
*        operator and skip to target token complete
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN
            MVI   G_STMT_SAVE_OP,C':' * Save : as operator
            MVI   G_STMT_SAVE_OP+1,X'00' * Append null char
            B     STMT_R_TGT_TOKEN_COMPLETE * Skip parsing target name
         ENDIF
*
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR
*        section to expand it and loop around for the next keyword
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable
         IF (EQ) THEN             * If so...
STMT_R_SCAN_VAR EQU *
            MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1' * Preserve 1 space
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address of SCAN_VAR
            BASR  R14,R15         * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_RULE_RET   * Yes, stop parsing statement
*
            B     STMT_R_NEXT_TOKEN * Loop around to get next token
         ENDIF
*
STMT_R_TGT_TOKEN_APPEND EQU *
*        Append token 1 to token 2
         MVI   G_SCAN_APPEND_TO,X'01'
         LT    R1,G_SCAN_TOKEN2_LEN  * Get current length token 2
         IF (Z) THEN                * Is this the first part of token 2
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces
         ELSE
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?
            IF (NZ) THEN               * Yep...
               MVC   G_SCAN_SPACE_COUNT,=F'1'
            ENDIF
         ENDIF
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
*
         B     STMT_R_NEXT_TOKEN  * Loop around to get next token
*
STMT_R_TGT_TOKEN_COMPLETE EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_RULE2, meaning target name complete
         OI    G_SCAN_STATE,SCAN_STATE_IN_RULE2
*        Clear token 3 length, which will receive the requisites
         MVC   G_SCAN_TOKEN3_LEN,=F'0'
*
STMT_R_NEXT_TOKEN2 EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_RULE_RET      * Yes, stop parsing statement
*
*        Check if scan state was reset to NOT_IN_STMT, meaning this
*        statement was finished
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt
         BE    STMT_R_FINISH      * If so, statement done
*
*        Check if we've hit a $() variable, if so parse that with the
*        same section that resolves them. Setting append to token 3
*        causes it not to resolve but simple parse and append.
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()
         IF (EQ) THEN             * If so...
            MVI   G_SCAN_APPEND_TO,X'02' * Set append to token 3
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1' * Preserve 1 space
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address of SCAN_VAR
            BASR  R14,R15         * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_RULE_RET   *  Yes, stop parsing statement
*
            B     STMT_R_NEXT_TOKEN2 * Loop around to get next token
         ENDIF
*
*        Append token 1 to token 3
         MVI   G_SCAN_APPEND_TO,X'02'
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3
         IF (Z) THEN                * Is this the first part of token 3
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces
         ELSE
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?
            IF (NZ) THEN               * Yep...
               MVC   G_SCAN_SPACE_COUNT,=F'1'
            ENDIF
         ENDIF
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
*
*        Set scan scate tot RULE3
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_IN_RULE2) * Check for RULE2
         IF (EQ) THEN             * Only RULE2 can change to RULE3
*           Clear scan state except for left most bit for in recipe
            NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*           Set scan state bits to IN_RULE3, meaning in requisites
            OI    G_SCAN_STATE,SCAN_STATE_IN_RULE3
         ENDIF
*
         B     STMT_R_NEXT_TOKEN2 * Loop around for the next token
*
STMT_R_FINISH EQU *
*        Write trace record that statement is finished
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'
*
*        Allocate a new memory block for this rule
         L     R1,=A(STMT_R_DSECT_LEN) * Size of block without tokens
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of target name(s)
         A     R1,G_SCAN_TOKEN3_LEN    * Add length of requisites
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_RULE * New block is for type
*                                               * rule
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT
         BASR  R14,R15            * Link to ALLOC_STMT section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_RULE_RET      * Yes, stop parsing statement
*
*        Get returned pointer to new block of memory and put it in R7
*        It should stay in R7 for the LWZMAKE_STORE_TGT section
         LT    R7,G_STMT_ALLOC_RETURN_PTR
         BZ    STMT_RULE_RET      * If it was zero, stop parsing
*                                 * (failsafe, shouldn't happen)
*
         USING STMT_R_DSECT,R7    * Address with rule DSECT
*
*        Copy target name(s) to memory block of rule statement
         LA    R0,STMT_R_TGT      * Point R0 to start of target
         L     R1,G_SCAN_TOKEN2_LEN * Get target length
         STH   R1,STMT_R_TGTLEN   * Put target length in block
         L     R2,G_SCAN_TOKEN2A  * Point R2 to token 2
         LR    R3,R1              * Make sure no cropping/filling
         MVCL  R0,R2              * Copy target name(s) to block
*
*        Copy requisite name(s) to memory block of rule statement
         LA    R0,STMT_R_TGT      * Point R0 to start of target
         AH    R0,STMT_R_TGTLEN   * Add target length, so now points to
*                                 * start of requisite
         L     R1,G_SCAN_TOKEN3_LEN * Get requisite length
         STH   R1,STMT_R_REQLEN   * Put requisite length in block
         L     R2,G_SCAN_TOKEN3A  * Point R2 to token 3
         LR    R3,R1              * Make sure no cropping/filling
         MVCL  R0,R2              * Copy requisite name(s) to block
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEBUG) THEN
            ST    R7,G_LWZMTRC_DATA_PTR
            L     R14,0(,R7)
            STH   R14,G_LWZMTRC_DATA_SIZ
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'620',DATA
         ENDIF
*
*        Split up space delimited target name(s) and for each name link
*        to STORE_TGT to allocate a target block and add it to the
*        target binary search tree.
         LA    R2,STMT_R_TGT      * Point R2 to target name(s)
         XR    R3,R3              * Clear R3
         LH    R3,STMT_R_TGTLEN   * Put length of target name(s) in R3
STMT_R_SCAN_NEXT_TGT EQU *
         L     R4,G_SCAN_TOKENA   * Point R4 to token 1
         XR    R5,R5              * Clear R5
         ST    R5,G_SCAN_TOKEN_LEN * And clear length token 1
STMT_R_TGT_BLANK EQU *
         IF (CLI,0(R2),EQ,C' ') THEN * Current pos a space?
            LA    R2,1(,R2)       * Skip space char
            BCT   R3,STMT_R_TGT_BLANK * R3 = R3 - 1 until R3 = 0
            B     STMT_R_STORE_DONE * No more chars, done storing TGTs
         ENDIF
STMT_R_TGT_NONBLANK EQU *
         MVC    0(1,R4),0(R2)     * Copy char to token 1
         LA     R4,1(,R4)         * Advance current pos token 1
         LA     R5,1(,R5)         * R5 = R5 - 1
         BCTR   R3,R0             * R3 = R3 - 1
         C      R3,=F'0'          * At end of target name(s)?
         IF (H) THEN              * If not...
            LA     R2,1(,R2)      * Advance current pos target name(s)
            CLI    0(R2),C' '     * Current pos a space?
            BNE    STMT_R_TGT_NONBLANK * Loop around to copy next char
         ENDIF
*        Either a space was found or we've reached the end of targets
         ST     R5,G_SCAN_TOKEN_LEN * Store target length
*
*        Add the target to binary search tree for targets
*        R7 points to the rule statement
*        Token 1 contains one target name
         L     R15,LWZMAKE_STORE_TGTA_STMT * Get address of STORE_TGT
         BASR  R14,R15            * Link to STORE_TGT section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_RULE_RET      * Yes, stop storing targets
*
         C      R3,=F'0'          * More chars left in target name(s)?
         BH     STMT_R_SCAN_NEXT_TGT * If so, look for next target
*
         DROP  R7
*
STMT_R_STORE_DONE EQU *
*        Remember this was a rule for next statement's previous
*        statement type
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_RULE
*
STMT_RULE_RET EQU *
         BR    R8                 * Return
*
* STMT call 'CALL routine'
* At this point 1 token has been scanned, which is the CALL keyword.
* From here on the first next token will be the REXX exec called, which
* is stored in token 2. Any tokens after that are concatenated in token
* 3 which becomes the (optional) parameter to the REXX exec.
* When the statement is finished it's converted to internal memory
* format and added to the statement linked list.
*
STMT_CALL EQU  *
*        Write a trace record for statement type call
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'CALL'
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_CALL
         OI    G_SCAN_STATE,SCAN_STATE_IN_CALL
*        Clear token 2 length, which will receive the REXX exec name
         MVC   G_SCAN_TOKEN2_LEN,=F'0'
*
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_CALL_RET      * Yes, stop parsing statement
*
         L     R2,G_SCAN_TOKEN_LEN * Get length of token 1
         C     R2,=A(8)           * Longer than 8 positions?
         IF (H) THEN              * If so...
            MLWZMRPT RPTLINE=CL133'0REXX exec cannot be longer than 8 cX
               haracters',APND_LC=C'Y'
            MVC   G_RETCODE,=F'8' * Set return code 8
            BR    R8              * and return
         ENDIF
*
*        Copy token 1 to token 2
         MVI   G_SCAN_APPEND_TO,X'01'
         MVC   G_SCAN_SPACE_COUNT,=A(0)
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_CALL2
         OI    G_SCAN_STATE,SCAN_STATE_IN_CALL2
*        Clear token 2 length, which will receive the REXX exec parm
         MVC   G_SCAN_TOKEN3_LEN,=F'0'
*
STMT_C_NEXT_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_CALL_RET      * Yes, stop parsing statement
*
*        Check if scan state was reset to NOT_IN_STMT, meaning this
*        statement was finished
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt
         BE    STMT_C_FINISH      * If so, statement done
*
*        Check if we've hit a $() variable
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()
         IF (NE) THEN
            C     R14,=A(SCAN_STATE_IN_VARIABLER)
         ENDIF
         IF (EQ) THEN             * If so...
            MVI   G_SCAN_APPEND_TO,X'02' * Set append to token 3
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR
            BASR  R14,R15         * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_CALL_RET   * Yes, stop parsing statement
*
            B     STMT_C_NEXT_TOKEN * Loop around to get next token
         ENDIF
*
*        Append token 1 to token 3, leading spaces since last token are
*        preserved (tokenizer counts them in G_SCAN_SPACE_COUNT)
         MVI   G_SCAN_APPEND_TO,X'02'
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3
         IF (Z) THEN                * Is this the first part of token 3
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces
         ELSE
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?
            IF (NZ) THEN               * Yep...
               MVC   G_SCAN_SPACE_COUNT,=F'1'
            ENDIF
         ENDIF
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
*
         B     STMT_C_NEXT_TOKEN  * Loop around for the next token
*
STMT_C_FINISH EQU *
*        Write trace record that statement is finished
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'
*
*        Allocate a new memory block for this call
         L     R1,=A(STMT_C_DSECT_LEN) * Size of block without tokens
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of REXX exec name
         A     R1,G_SCAN_TOKEN3_LEN    * Add length of REXX exec parm
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_CALL * New block is for type
*                                               * call
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT
         BASR  R14,R15            * Link to ALLOC_STMT section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_CALL_RET      * Yes, stop parsing statement
*
         LT    R7,G_STMT_ALLOC_RETURN_PTR * Get returned ptr to new
*                                         * block of memory
         BZ    STMT_CALL_RET      * If it was zero, stop parsing
*                                 * (failsafe, shouldn't happen)
*
         USING STMT_C_DSECT,R7    * Address with call DSECT
*
*        Copy REXX exec name
         LA    R2,STMT_C_EXEC     * Point R2 to start of exec
         L     R3,G_SCAN_TOKEN2A  * Point R3 to token 1
         L     R4,G_SCAN_TOKEN2_LEN * Get length of token 2
         STH   R4,STMT_C_EXECLEN  * Store exec length in block
         BCTR  R4,R0              * Minus 1 for EX
         B     *+10               * Skip MVC constant for EX
         MVC   0(1,R2),0(R3)      * MVC constant for EX
         EX    R4,*-6             * EX previous MVC statement with R4
*
*        Copy REXX exec parameter
         LA    R0,STMT_C_EXEC     * Point R0 to start of exec in block
         AH    R0,STMT_C_EXECLEN  * Advance to start of parm in block
         L     R1,G_SCAN_TOKEN3_LEN * Get length of exec parm
         STH   R1,STMT_C_PARMLEN  * Store length of exec parm in block
         L     R2,G_SCAN_TOKEN3A  * Point R2 to token 3
         LR    R3,R1              * Make sure no cropping/filling
         MVCL  R0,R2              * Copy REXX exec parm to block
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEBUG) THEN
            ST    R7,G_LWZMTRC_DATA_PTR
            L     R14,0(,R7)
            STH   R14,G_LWZMTRC_DATA_SIZ
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'620',DATA
         ENDIF
*
*        Call the REXX, but not if we're in a recipe
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN
            MVC   G_CALL_REXX_PAR2A(4),=A(0)
            ST    R7,G_CALL_REXX_PAR2A+4
            LA    R1,G_CALL_REXX_PAR2A
*
            L     R15,LWZMAKE_CALL_REXXA_STMT * Get address STORE_VAR
            BASR  R14,R15              * Link to STORE_VAR section
         ENDIF
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement
*
*        Remember this was a call for next statement's previous
*        statement type
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_CALL
*
STMT_CALL_RET EQU *
         BR    R8                 * Return
*
         DROP  R7
*
* STMT PHONY '.PHONY targetname'
* At this point 1 token has been scanned, which is the .PHONY keyword.
* From here on the first next token will be the PHONY target name.
* When the statement is finished it's converted to internal memory
* format and added to the statement linked list.
* The PHONY target name is added to the binary search tree for phonies.
*
STMT_PHONY EQU  *
*        Write a trace record for statement type call
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'PHONY'
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_PHONY
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY
*
STMT_P_FIRST_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement
*
*        Check if we've hit a $() variable
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()
         IF (EQ) THEN             * If so...
            MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR
            BASR  R14,R15      * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_PHONY_RET  * Yes, stop parsing statement
            B     STMT_P_FIRST_TOKEN
         ENDIF
*
         IF (CLI,G_SCAN_TOKENTYPE,NE,SCAN_TOKENTYPE_NORMAL) THEN
            MLWZMRPT RPTLINE=CL133'0.PHONY must be followed by a constaX
               nt target name',APND_LC=C'Y'
            MVC   G_RETCODE,=F'8' * Set return code 8
            BR    R8              * and return
         ENDIF
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_PHONY2
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY2
*
         B     STMT_P_CREATE
*
STMT_P_NEXT_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement
*
*        Check if scan state was reset to NOT_IN_STMT, meaning this
*        statement was finished
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt
         BE    STMT_P_FINISH      * If so, finished
*
*        Check if we've hit a $() variable
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()
         IF (EQ) THEN             * If so...
            MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR
            BASR  R14,R15      * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_PHONY_RET  * Yes, stop parsing statement
            B     STMT_P_NEXT_TOKEN * Loop around to get next token
         ENDIF
*
STMT_P_CREATE EQU *
*        Copy token 1 to token 2
         MVC   G_SCAN_TOKEN2_LEN,=F'0'
         MVC   G_SCAN_SPACE_COUNT,=A(0)
         MVI   G_SCAN_APPEND_TO,X'01'
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type
*
*        Allocate a new memory block for this PHONY
         L     R1,=A(STMT_P_DSECT_LEN) * Size of block without tokens
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of PHONY name
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_PHONY * New block is for
*                                                * type PHONY
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT
         BASR  R14,R15            * Link to ALLOC_STMT section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement
*
*        Get returned pointer to new block of memory and put it in R7
*        It should stay in R7 for the LWZMAKE_STORE_PNY section
         LT    R7,G_STMT_ALLOC_RETURN_PTR
         BZ    STMT_PHONY_RET     * If it was zero, stop parsing
*                                 * (failsafe, shouldn't happen)
*
         USING STMT_P_DSECT,R7    * Address with PHONY DSECT
*
*        Copy PHONY name to memory block of PHONY statement
         LA    R0,STMT_P_PNY      * Point R0 to start of PHONY
         L     R1,G_SCAN_TOKEN2_LEN * Get PHONY length
         STH   R1,STMT_P_PNYLEN   * Put PHONY length in block
         L     R2,G_SCAN_TOKEN2A  * Point R2 to token 2
         LR    R3,R1              * Make sure no cropping/filling
         MVCL  R0,R2              * Copy PHONY name to block
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEBUG) THEN
            ST    R7,G_LWZMTRC_DATA_PTR
            L     R14,0(,R7)
            STH   R14,G_LWZMTRC_DATA_SIZ
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'620',DATA
         ENDIF
*
*        R7 points to the PHONY statement
         L     R15,LWZMAKE_STORE_PNYA_STMT * Get address STORE_PNY
         BASR  R14,R15            * Link to STORE_PNY section
*
         DROP  R7
*
         B     STMT_P_NEXT_TOKEN * Loop around to get next token
*
STMT_P_FINISH EQU *
*        Write trace record that statement is finished
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'
*
*        Remember this was a PHONY for next statement's previous
*        statement type
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_PHONY
*
STMT_PHONY_RET EQU *
         BR    R8                 * Return
*
* STMT INCLUDE '.INCLUDE data_set_name'
* At this point 1 token has been scanned, which is the .INCLUDE keyword
* From here on the first next token will be the INCLUDE data set name.
* When the statement is finished it's converted to internal memory
* format and added to the statement linked list.
* The INCLUDE data set is opened and added to the INPUT STACK.
*
STMT_INCLUDE EQU *
*        Write a trace record for statement type call
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'INCLUDX
               E'
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_INCLUDE
         OI    G_SCAN_STATE,SCAN_STATE_IN_INCLUDE
*        Clear token 2 length, which will receive the INCLUDE ds name
         MVC   G_SCAN_TOKEN2_LEN,=F'0'
*
STMT_I_NEXT_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_INCLUDE_RET   * Yes, stop parsing statement
*
*        Check if scan state was reset to NOT_IN_STMT, meaning this
*        statement was finished
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt
         BE    STMT_I_FINISH      * If so, statement doned
*
*        Check if we've hit a $() variable
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()
         IF (NE) THEN
            C     R14,=A(SCAN_STATE_IN_VARIABLER)
         ENDIF
         IF (EQ) THEN             * If so...
            MVI   G_SCAN_APPEND_TO,X'00' * Set to expand now
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR
            BASR  R14,R15         * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0' * Did an error occur?
            BNE   STMT_INCLUDE_RET   * Yes, stop parsing statement
*
            B     STMT_I_NEXT_TOKEN * Loop around to get next token
         ENDIF
*
         LT    R14,G_SCAN_TOKEN2_LEN
         IF (NZ) THEN
            LT    R14,G_SCAN_SPACE_COUNT
            IF (NZ) THEN
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as data setX
                name in INCLUDE',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     STMT_INCLUDE_RET * and return
            ENDIF
         ENDIF
*
*        Copy token 1 to token 2
         MVI   G_SCAN_APPEND_TO,X'01'
         MVC   G_SCAN_SPACE_COUNT,=A(0)
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_INCLUDE2
         OI    G_SCAN_STATE,SCAN_STATE_IN_INCLUDE2
*
         B     STMT_I_NEXT_TOKEN  * Loop around for the next token
*
STMT_I_FINISH EQU *
*        Write trace record that statement is finished
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'
*
         MVI   G_CHECK_MVSDS_TOKEN,X'01'
*
         L     R15,LWZMAKE_CHECK_MVSDSA_STMT
         BASR  R14,R15
*
         IF (CLI,G_CHECK_MVSDS_MVSDS,NE,C'Y') THEN
            MLWZMRPT RPTLINE=CL133'0INCLUDE parameter is not an mvs datX
               a set name',APND_LC=C'Y'
            MVC   G_RETCODE,=F'8'  * Set return code 8
            B     STMT_INCLUDE_RET * and return
         ENDIF
*
*        Allocate a new memory block for this INCLUDE
         L     R1,=A(STMT_I_DSECT_LEN) * Size of block without tokens
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of INCLUDE ds name
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_INCLUDE * New block is for
*                                                  * type INCLUDE
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT
         BASR  R14,R15            * Link to ALLOC_STMT section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_INCLUDE_RET   * Yes, stop parsing statement
*
         L     R7,G_STMT_ALLOC_RETURN_PTR
*
         L     R4,G_SCAN_TOKEN2_LEN
         STH   R4,STMT_I_INCLEN-STMT_I_DSECT(,R7)
         LA    R2,STMT_I_INC-STMT_I_DSECT(,R7)
         L     R3,G_SCAN_TOKEN2A
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
*
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEBUG) THEN
            ST    R7,G_LWZMTRC_DATA_PTR
            L     R14,0(,R7)
            STH   R14,G_LWZMTRC_DATA_SIZ
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'620',DATA
         ENDIF
*
         MVI   G_DYNALLOC_DSNAME,C' '
         MVC   G_DYNALLOC_DSNAME+1(L'G_DYNALLOC_DSNAME-1),G_DYNALLOC_DSX
               NAME
         L     R4,G_SCAN_TOKEN2_LEN
         C     R4,=A(L'G_DYNALLOC_DSNAME)
         IF (H) THEN
            L     R4,=A(L'G_DYNALLOC_DSNAME)
         ENDIF
         LA    R2,G_DYNALLOC_DSNAME
         L     R3,G_SCAN_TOKEN2A
STMT_I_NEXT_DSNAME_CHAR EQU *
         IF (CLI,0(R3),NE,C'(') THEN
            MVC   0(1,R2),0(R3)
            LA    R2,1(,R2)
            LA    R3,1(,R3)
            BCT   R4,STMT_I_NEXT_DSNAME_CHAR
         ENDIF
*
         MVC   G_DYNALLOC_MEMBER,=CL8' '
         IF (CLC,G_MVSDS_MEMBER_LEN,NE,=F'0') THEN
            L     R4,G_MVSDS_MEMBER_LEN
            LA    R2,G_DYNALLOC_MEMBER
            L     R3,G_MVSDS_MEMBER_PTR
            BCTR  R4,R0
            B     *+10
            MVC   0(1,R2),0(R3)
            EX    R4,*-6
         ENDIF
*
         MVI   G_DYNALLOC_FUNC,C'A'
*
         L     R15,LWZMAKE_DYNALLOCA_STMT
         BASR  R14,R15
*
         CLC   G_RETCODE,=F'0'
         BNE   STMT_INCLUDE_RET
*
*        Start by putting parameter in input stack
         XR    R4,R4              * Clear R4
         XR    R5,R5              * Clear R5
         IC    R5,G_SCAN_INPUT_STACK_IDX * Get current stack index
         C     R5,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra
*                                 * entry fit?
         IF (NL) THEN             * If not write error
            MLWZMRPT RPTLINE=CL133'0Internal error, state stack overfloX
               w'
            MVC   G_RETCODE,=F'12' * Set return code 12
            B     STMT_INCLUDE_RET * and return
         ENDIF
         LA    R5,1(,R5)          * Add 1 to stack size
         STC   R5,G_SCAN_INPUT_STACK_IDX * And store it
         BCTR  R5,R0              * Subtract 1 to calculate offset
         M     R4,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry
         LA    R4,G_SCAN_INPUT_STACK * Point R4 to input stack
         AR    R4,R5              * Add calculated offset
*
         USING INPUT_DSECT,R4     * Address with INPUT DSECT
*
         MVC   INPUTLEAD,=H'0'    * Clear leading spaces
         MVI   INPUTTYPE,INPUTTYPE_MAKEFILE_INC * Set type of input
         MVC   INPUTLEN,=H'0'     * Clear value length
         MVC   INPUTPOS,=H'999'   * Force a read of next record
*
*                                 * GM DCB storage below the line
         GETMAIN RU,LV=DCB_INCLUDE_DSECT_SIZ,LOC=24
         ST    R1,INPUTPTR        * and save in input stack entry ptr
*
         L     R2,CDCBMKFA_STMT
         MVC   DCBMKFI-DCB_INCLUDE_DSECT(LEN_DCBMKF,R1),0(R2)
         L     R2,CDCBEMKFA_STMT
         MVC   DCBEMKFI-DCB_INCLUDE_DSECT(LEN_DCBEMKF,R1),0(R2)
         LA    R6,DCBMKFI-DCB_INCLUDE_DSECT(,R1)
         MVC   DCBDDNAM-IHADCB(8,R6),G_DYNALLOC_DDNAME
         LA    R5,DCBEMKFI-DCB_INCLUDE_DSECT(,R1)
         ST    R5,DCBDCBE-IHADCB(,R6)    * Store ptr to DCBE in DCB
         L     R1,MAKEFILE_IS_EOFA_STMT  * Get address of EODAD routine
         ST    R1,DCBEEODA-DCBE(,R5)     * Store address in DCBE
*
         ST    R7,INPUTXPTR       * Save statement block in extra ptr
*
         L     R2,INPUTPTR        * Get DCB memory pointer
*
         DROP  R4
*                                 * and open for input
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)
         LTR   R15,R15            * Check for returned 0
         IF (NZ) THEN             * non-zero means open failed
            CVD   R15,G_DEC8      * convert return value to packed
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned
            OI    G_ZONED8+7,X'F0' * get rid of sign
            MVC   G_HELPER_DATA(2),G_ZONED8+6 * Copy to helper data
            LA    R14,G_HELPER_DATA  * Get ptr to helper data
            ST    R14,G_LWZMTRC_DATA_PTR * Save trace data ptr
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(2) * Set trace data size
*           Trace record MAKEFILE DCB OPEN failed
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'002',DATA
            MVC   G_LWZMRPT_LINE,=CL133'0Error opening included MAKEFILX
               E'
            MVC   G_LWZMRPT_LINE+33(2),G_ZONED8+6
            L     R15,G_LWZMAKE_RPTA * Get address of report section
            BASR  R14,R15          * Link to report section
            MVC   G_RETCODE,=F'8'  * Set return code to 8
            B     STMT_INCLUDE_RET * and skip rest
         ENDIF
*
*        Remember this was an INCLUDE for next statement's previous
*        statement type
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_INCLUDE
*
STMT_INCLUDE_RET EQU *
         BR    R8                 * Return
*
         LTORG
*
* Local constant pointers to section addresses
LWZMAKE_SCAN_TOKENA_STMT   DC    A(LWZMAKE_SCAN_TOKEN)
LWZMAKE_APPEND_TOKENA_STMT DC    A(LWZMAKE_APPEND_TOKEN)
LWZMAKE_SCAN_VARA_STMT     DC    A(LWZMAKE_SCAN_VAR)
LWZMAKE_ALLOC_STMTA_STMT   DC    A(LWZMAKE_ALLOC_STMT)
LWZMAKE_STORE_VARA_STMT    DC    A(LWZMAKE_STORE_VAR)
LWZMAKE_STORE_TGTA_STMT    DC    A(LWZMAKE_STORE_TGT)
LWZMAKE_STORE_PNYA_STMT    DC    A(LWZMAKE_STORE_PNY)
LWZMAKE_CALL_REXXA_STMT    DC    A(LWZMAKE_CALL_REXX)
LWZMAKE_FINDVARA_STMT      DC    A(LWZMAKE_FINDVAR)
LWZMAKE_CHECK_MVSDSA_STMT  DC    A(LWZMAKE_CHECK_MVSDS)
LWZMAKE_DYNALLOCA_STMT     DC    A(LWZMAKE_DYNALLOC)
*
CDCBMKFA_STMT              DC    A(CDCBMKF)
CDCBEMKFA_STMT             DC    A(CDCBEMKF)
MAKEFILE_IS_EOFA_STMT      DC    A(MAKEFILE_IS_EOF)
*
         DROP
*
***********************************************************************
* Section: LWZMAKE_SCAN_VAR                                           *
* Purpose: Parse a $() variable. At this point $( was parsed into     *
*          token 1. This section parses the variable and closing ).   *
*          If G_SCAN_APPEND_TO equals X'00' the variable is looked up *
*          in the variable binary search tree and its value is added  *
*          to the input stack so parsing first read from the variable *
*          value before continuing with the original input.           *
*          If G_SCAN_APPEND_TO is not X'00' $(var) is appended to     *
*          token 2 or 3.                                              *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_SCAN_VAR DS    0F
         STM   R14,R12,12(R13)   * Save callers registers
         LR    R10,R15
         LA    R11,4095(,R10)
         LA    R11,1(,R11)
         USING LWZMAKE_SCAN_VAR,R10,R11
         GETMAIN RU,LV=SCAN_VAR_DSECT_SIZ
         ST    R13,4(R1)         * Backward chain callers SA
         ST    R1,8(R13)         * Forward chain my SA
         LR    R13,R1            * Point R13 to my SA
         USING SCAN_VAR_DSECT,R13 * Establish addressing of workarea
         USING GLOBAL_DATA_DSECT,R9
*
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_SCAN_VAR'
*
         MVC   SCAN_VAR_SAVE_SPACE_COUNT,G_SAVE_SPACE_COUNT
         MVC   G_SAVE_SPACE_COUNT,=F'0'
         MVC   SCAN_VAR_SAVE_CLOSE_BRACKET,G_SCAN_CLOSE_BRACKET
*
*        In any case save the spaces
         LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?
         IF (NZ) THEN             * Yep...
            IF (CLI,G_SCAN_VAR_PRESERVE_SPACES,NE,C'A') THEN
               MVC   G_SCAN_SPACE_COUNT,=F'1'
            ENDIF
            SELECT CLI,G_SCAN_APPEND_TO,EQ
            WHEN X'00'
               MVC   G_SAVE_SPACE_COUNT,G_SCAN_SPACE_COUNT
            WHEN X'01'
               LT    R14,G_SCAN_TOKEN2_LEN    * Get token 2 len
               IF (Z) THEN                    * Currently empty?
                  MVC   G_SCAN_SPACE_COUNT,=F'0'
               ENDIF
            WHEN X'02'
               LT    R14,G_SCAN_TOKEN3_LEN    * Get token 3 len
               IF (Z) THEN                    * Currently empty?
                  MVC   G_SCAN_SPACE_COUNT,=F'0'
               ENDIF
            ENDSEL
         ENDIF
*
         L     R15,LWZMAKE_APPEND_TOKENA_VAR
         BASR  R14,R15
*
*        Get next token, which should be the variable name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_VAR_RET       * Yes, stop parsing variable
*
*        Check if scan state changed to anything other than IN_VARIABLE
*        which shouldn't happen if a valid variable name was found
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable
         IF (NE) THEN             * If not, write error and stop
            C     R14,=A(SCAN_STATE_IN_VARIABLER)
         ENDIF
         IF (NE) THEN
            MLWZMRPT RPTLINE=CL133'0Empty $()',APND_LC=C'Y'
            MVC   G_RETCODE,=F'8' * Set return code 8
            B     SCAN_VAR_RET    * and return
         ENDIF
*
         L     R1,G_SCAN_TOKEN_LEN * Get length of variable name
         C     R1,=A(10)           * addpdsname is 10 long
         IF (EQ) THEN
            L     R2,G_SCAN_TOKENA * Point R2 to token 1
            MVC   G_HELPER_DATA(10),0(R2) * Copy token to helper
            OC    G_HELPER_DATA(10),=10X'40' * Convert to uppercase
            CLC   G_HELPER_DATA(10),=C'ADDPDSNAME'
            IF (EQ) THEN           * If it's the addpdsname function
               BAL   R8,SCAN_ADDPDSNAME * Scan the function
               MVC   G_SCAN_SPACE_COUNT,G_SAVE_SPACE_COUNT
               B     SCAN_VAR_RET  * And skip the rest of SCAN_VAR
            ENDIF
            CLC   G_HELPER_DATA(10),=C'MEMBERLIST'
            IF (EQ) THEN           * If it's the memberlist function
               BAL   R8,SCAN_MEMBERLIST * Scan the function
               MVC   G_SCAN_SPACE_COUNT,G_SAVE_SPACE_COUNT
               B     SCAN_VAR_RET  * And skip the rest of SCAN_VAR
            ENDIF
         ENDIF
         L     R1,G_SCAN_TOKEN_LEN
         C     R1,=A(8)
         IF (EQ) THEN
            L     R2,G_SCAN_TOKENA * Point R2 to token 1
            MVC   G_HELPER_DATA(8),0(R2) * Copy token to helper
            OC    G_HELPER_DATA(8),=10X'40' * Convert to uppercase
            CLC   G_HELPER_DATA(8),=C'FUNCTION'
            IF (EQ) THEN
               BAL   R8,SCAN_FUNCTION
               MVC   G_SCAN_SPACE_COUNT,G_SAVE_SPACE_COUNT
               B     SCAN_VAR_RET
            ENDIF
         ENDIF
*
*        Ending up here means it's not a function
*        If APPEND_TO is X'00', copy variable name to input to FINDVAR
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            L     R1,G_SCAN_TOKEN_LEN * Get length of variable name
            C     R1,=A(L'G_SRCH_VAR) * If should always fit, but just
            IF (H) THEN               * in case check anyway
               MLWZMRPT RPTLINE=CL133'0Internal error, variable name loX
               nger than 72',APND_LC=C'Y'
               MVC   G_RETCODE,=F'12' * Set return code 12
               B     SCAN_VAR_RET     * and return
            ENDIF
*           Copy variable name to FINDVAR name
            STH   R1,G_SRCH_VAR_LEN * Put length in FINDVAR search len
            LA    R0,G_SRCH_VAR   * Point R0 to FINDVAR search name
            L     R2,G_SCAN_TOKENA * Point R2 to token 1
            LR    R3,R1           * Make sure no cropping/filling
            MVCL  R0,R2           * Copy variable name to FINDVAR name
         ELSE
*           Else if APPEND_TO is not X'00'
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
         ENDIF
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_VARIABLE2
         OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLE2
*
*        Get the next token which should be )
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_VAR_RET       * Yes, stop parsing variable
*
*        If APPEND_TO != X'00' then copy ) from token 1 to token n
         IF (CLI,G_SCAN_APPEND_TO,NE,X'00') THEN
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
         ELSE
*           Else if APPEND_TO is X'00'
            L     R15,LWZMAKE_FINDVARA_VAR * Get address to FINDVAR
            BASR  R14,R15         * Link to FINDVAR section
*
            LT    R4,G_FOUND_VAR_PTR * Check if a pointer was returned
            IF (Z) THEN           * If not write error and stop
               MLWZMRPT RPTLINE=CL133'0Variable not found',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_VAR_RET    * and return
            ENDIF
*
            USING VAR_DSECT,R4    * Address with VAR DSECT
*
            CLC   VALLEN,=H'0'    * Check empty variable value
            BE    SCAN_VAR_RET    * If empty skip rest of section
*
*           Push variable value on to input stack
            XR    R2,R2           * Clear R2
            XR    R3,R3           * Clear R3
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index
            C     R3,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra
*                                 * entry fit?
            IF (NL) THEN          * If not write error
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX
               flow',APND_LC=C'Y'
               MVC   G_RETCODE,=F'12' * Set return code 12
               B     SCAN_VAR_RET     * and return
            ENDIF
            LA    R3,1(,R3)       * Add 1 to stack size
            STC   R3,G_SCAN_INPUT_STACK_IDX * And store it
            BCTR  R3,R0           * Subtract 1 to calculate offset
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack
            AR    R2,R3           * Add calculated offset
*
            USING INPUT_DSECT,R2  * Address with INPUT DSECT
*
            MVI   INPUTTYPE,INPUTTYPE_STRPTR_NEOF * Set type of input
            MVC   INPUTLEAD,G_SAVE_SPACE_COUNT+2
            MVC   INPUTLEN,VALLEN * Copy value length
            MVC   INPUTPTR,VALPTR * Copy value pointer
            MVC   INPUTPOS,=H'0'  * Set initial scan position to start
*
            DROP  R2
            DROP  R4
         ENDIF
*
SCAN_VAR_RET EQU *
         MVC   G_SAVE_SPACE_COUNT,SCAN_VAR_SAVE_SPACE_COUNT
*
         L     R3,4(,R13)        * Restore address of callers SA
         FREEMAIN RU,LV=SCAN_VAR_DSECT_SIZ,A=(R13)
         LR    R13,R3
         LM    R14,R12,12(R13)
         BR    R14                    Return to caller
*
* Scan function ADDPDSNAME
*
SCAN_ADDPDSNAME EQU *
         L     R15,LWZMAKE_APPEND_TOKENA_VAR
         BASR  R14,R15
*
         BAL   R7,SCAN_VAR_SAVE
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_ADDPDSNAME
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME
*
*        Get next token, which should be the start of the PDS name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function
*
         B     ADDPDSNAME_PDSNAME_CHECK_VAR
*
ADDPDSNAME_PDSNAME_NEXT_TOKEN EQU *
*        Get next token, which should be the PDS name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function
*
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA
         BE    ADDPDSNAME3
*
         LT    R14,G_SCAN_TOKEN2_LEN
         IF (NZ) THEN
            LT    R14,G_SCAN_SPACE_COUNT
            IF (NZ) THEN
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as pds nameX
                in addpdsname function',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               BR    R8             * and return
            ENDIF
         ENDIF
*
ADDPDSNAME_PDSNAME_CHECK_VAR EQU *
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR
*        section to expand it and loop around for the next keyword
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable
         IF (NE) THEN
            C     R14,=A(SCAN_STATE_IN_VARIABLER)
         ENDIF
         IF (EQ) THEN             * If so...
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR
            BASR  R14,R15            * Link to SCAN_VAR section
*
            CLC   G_RETCODE,=F'0'    * Did an error occur?
            BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing statement
*
            MVC   G_SCAN_CLOSE_BRACKET,SCAN_VAR_SAVE_CLOSE_BRACKET
*
            B     ADDPDSNAME2
         ENDIF
*
         MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
*           Append token 1 to token 2
            MVI   G_SCAN_APPEND_TO,X'01'
            LT    R1,G_SCAN_TOKEN2_LEN
            IF (Z) THEN
               MVC   G_SCAN_SPACE_COUNT,=A(0)
            ENDIF
         ENDIF
         L     R15,LWZMAKE_APPEND_TOKENA_VAR
         BASR  R14,R15
         MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO
*
ADDPDSNAME2 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_ADDPDSNAME2
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME2
*
         B     ADDPDSNAME_PDSNAME_NEXT_TOKEN
*
ADDPDSNAME3 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_ADDPDSNAME3
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME3
*        If we're expanding
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
*           Clear token 3 length, which will receive the member(s)
            MVC   G_SCAN_TOKEN3_LEN,=F'0'
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
         ENDIF
*
ADDPDSNAME_NEXT_MEMBER EQU *
*        Get next token, which should be the a member name or variable
