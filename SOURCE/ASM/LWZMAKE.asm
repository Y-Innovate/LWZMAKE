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
         GETMAIN RU,LV=72         * Get storage for SA
         XC    0(2,R1),0(R1)      * Clear first 2 bytes of SA
         ST    R13,4(R1)          * Backward chain callers SA
         ST    R1,8(R13)          * Forward chain my SA
         LR    R13,R1             * Point R13 to my SA
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
*        Allocate multi-purpose tokens
         GETMAIN RU,LV=SCAN_TOKEN_MAXLEN
         ST    R1,G_SCAN_TOKENA
         MVC   G_SCAN_TOKEN_MAXLEN,=A(SCAN_TOKEN_MAXLEN)
         GETMAIN RU,LV=SCAN_TOKEN_MAXLEN
         ST    R1,G_SCAN_TOKEN2A
         MVC   G_SCAN_TOKEN2_MAXLEN,=A(SCAN_TOKEN_MAXLEN)
         GETMAIN RU,LV=SCAN_TOKEN_MAXLEN
         ST    R1,G_SCAN_TOKEN3A
         MVC   G_SCAN_TOKEN3_MAXLEN,=A(SCAN_TOKEN_MAXLEN)
*
*        Allocate evaluation block
         LA    R3,272
         ST    R3,G_EVALBLOCK_MAXLEN
         STORAGE OBTAIN,LENGTH=(R3)
         ST    R1,G_EVALBLOCK_PTR
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
            L     R15,G_LWZMAKE_RPTA * Get address of report section
            BASR  R14,R15          * Link to report section
            MVC   G_RETCODE,=F'12' * Set return code to 12
            B     INIT_RET        * and skip rest of INIT
         ENDIF
         MVI   MKFOPEN,C'Y'       * Remember to close MAKEFILE DCB
*        Trace record MAKEFILE DCB open
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'602',CONST=C'MAKEFILX
               E'
*        Report line MAKEFILE opened
         MLWZMRPT RPTLINE=CL133' MAKEFILE DD opened'
*
*        Retrieve submitter user id
         IAZXJSAB READ,USERID=G_USERID
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
         MVI   INPUTTYPE,X'02'    * Set type of input to ptr to string
         STH   R3,INPUTLEN        * Copy value length
         ST    R2,INPUTPTR        * Copy value pointer
         MVC   INPUTPOS,=H'0'     * Set initial scan position to start
*
         DROP  R4
*
NEXT_PARMS_ROUND EQU *
         L     R15,LWZMAKE_SCAN_TOKENA * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         L     R14,G_SCAN_TOKENA  * Point R14 to token 1
         CLI   0(R14),X'00'       * Check if empty token was returned
         BE    PARMS_DONE         * Because that means done
*
         CLC   G_SCAN_TOKEN_LEN,=F'1' * Any valid parameter starts with
         BNE   PARMS_ERROR        * a '-' switch which has to be 1 long
*
         IF (CLI,0(R14),EQ,C'-') THEN * Check if it's a '-'
            L     R15,LWZMAKE_SCAN_TOKENA * Get address of SCAN_TOKEN
            BASR  R14,R15         * Link to SCAN_TOKEN section
*
            L     R14,G_SCAN_TOKENA * Point R14 to token 1
            CLI   0(R14),X'00'    * Check if empty token returned
            BE    PARMS_ERROR     * If so, then error
*
            CLC   G_SCAN_TOKEN_LEN,=F'1' * Switch has to be 1 char
            BNE   PARMS_ERROR     * If not, then error
*
            IF (CLI,0(R14),EQ,C't') THEN * Check for target sw?
               L     R15,LWZMAKE_SCAN_TOKENA * Get address SCAN_TOKEN
               BASR  R14,R15      * Link to SCAN_TOKEN section
*
               L     R14,G_SCAN_TOKENA * Point R14 to token 1
               CLI   0(R14),X'00' * Check if empty token returned
               BE    PARMS_ERROR  * If so, then error
*
*              Double check length is between 1 and 72, if not error
               CLC   G_SCAN_TOKEN_LEN,=F'0'
               BE    PARMS_ERROR
               CLC   G_SCAN_TOKEN_LEN,=F'72'
               BH    PARMS_ERROR
*
*              * Check if normal token was returned
               IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN
*                 Everything checks out, so copy to default target
                  LA    R2,G_DEFAULT_TARGET * Point R2 to default tgt
                  L     R3,G_SCAN_TOKENA    * Point R3 to token 1
                  L     R4,G_SCAN_TOKEN_LEN * Get length of target in 1
                  BCTR  R4,R0         * R4 = R4 - 1 for EX
                  B     *+10          * Skip MVC constant for EX
                  MVC   0(1,R2),0(R3) * MVC constant for EX
                  EX    R4,*-6        * EX previous MVC stmt with R4
*
                  CLI   G_SCAN_INPUT_STACK_IDX,X'01' * Check if we're
*                                        * still in parameter input
                  BNE   NEXT_PARMS_ROUND * If so, go for next parm
               ELSE                 * Else, not token type normal
                  B     PARMS_ERROR * means error
               ENDIF
            ELSE                  * Else, it's not a -t switch
               B     PARMS_ERROR  * means error
            ENDIF
         ELSE                     * Else, it's not a '-' switch char
            B     PARMS_ERROR     * means error
         ENDIF
*
PARMS_DONE EQU  *
         MVC   G_SCAN_CURRCOL,=F'99999'  * Make sure first scanned char
*                                        * causes read record
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
            ST    R3,G_DEC8       * Put ptr in area of at least 5 bytes
            UNPK  G_ZONED8(9),G_DEC8(5)      * Turn into almost hex
            TR    G_ZONED8,MAIN_HEXTAB       * Turn into hex
            MVC   G_HELPER_DATA(8),G_ZONED8  * Copy hex to helper data
            LA    R14,G_HELPER_DATA          * Get ptr to helper data
            ST    R14,G_LWZMTRC_DATA_PTR     * Save it for trace data
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace data length 8
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'641',DATA
            L     R4,STMT_NEXT_PTR-STMT_DSECT(,R3) * Save ptr next stmt
            L     R2,STMT_LEN-STMT_DSECT(,R3)     * Get length of block
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * and free it
            LTR   R3,R4           * Test pointer to next statement
         ENDDO
         MVC   G_STMT_LIST_PTR,=A(0) * Clear first block ptr
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
*
* Local constant pointers to section addresses
LWZMAKE_TRACEA              DC    A(LWZMAKE_TRACE)
LWZMAKE_RPTA                DC    A(LWZMAKE_RPT)
LWZMAKE_SCAN_TOKENA         DC    A(LWZMAKE_SCAN_TOKEN)
LWZMAKE_PHASE1A             DC    A(LWZMAKE_PHASE1)
LWZMAKE_PHASE2A             DC    A(LWZMAKE_PHASE2)
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
* Catalog search interface IGGCSI00 external function address
                            DS    0F
G_IGGCSI00A                 DS    A
*
* Pointer and length of member name in MVS data set string
G_MVSDS_MEMBER_PTR          DS    A
G_MVSDS_MEMBER_LEN          DS    F
*
* Binder fast data access external function address
                            DS    0F
G_IEWBFDATA                 DS    A
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
* Statement operator ('=' or ':=' in assigment, ':' in rule)
G_STMT_SAVE_OP              DS    CL2
*
* Previous statement type ('A'ssignment, 'R'ule or 'C'all)
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
SCAN_TOKENTYPE_OPERATOR     EQU   C'=' * Any operator (:= =)
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
G_SCAN_APPEND_TO            DS    C    * Which G_SCAN_TOKEN* to append
*                                      * to when scanning $() variable
G_SCAN_CLOSE_BRACKET        DS    C    * Save ) or } to check matching
*                                      * with ( or {
G_SCAN_VAR_PRESERVE_SPACES  DS    C    * Preserve all spaces or just 1
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
SCAN_EXPECTED1_EOF          EQU   X'80'
SCAN_EXPECTED1_NEWLINE      EQU   X'40'
SCAN_EXPECTED1_COMMENT      EQU   X'20'
SCAN_EXPECTED1_IGNORE       EQU   X'10'
SCAN_EXPECTED1_NORMAL       EQU   X'08'
SCAN_EXPECTED1_OPENVAR      EQU   X'04'
SCAN_EXPECTED1_OPENBRC      EQU   X'02'
SCAN_EXPECTED1_CLOSEBRC     EQU   X'01'
SCAN_EXPECTED2_NUMBER       EQU   X'80'
SCAN_EXPECTED2_OPERATOR     EQU   X'40'
SCAN_EXPECTED2_RULE         EQU   X'20'
SCAN_EXPECTED2_SPECIAL      EQU   X'10'
SCAN_EXPECTED2_CONTINUA     EQU   X'08'
SCAN_EXPECTED2_CALL         EQU   X'04'
SCAN_EXPECTED2_ACRO         EQU   X'02'
SCAN_EXPECTED2_PERCENT      EQU   X'01'
SCAN_EXPECTED3_RECIPREF     EQU   X'80'
SCAN_EXPECTED3_COMMA        EQU   X'40'
*
* Combinations of the flags above, used in SCAN_STATE_TABLE
SCAN_EXPECTED_NEWSTMT       EQU   B'11111100000101001000000000000000'
SCAN_EXPECTED_NEWSTMT2      EQU   B'00001100011010001000000000000000'
SCAN_EXPECTED_ASSIGN        EQU   B'01111111100010111100000000000000'
SCAN_EXPECTED_ASSIGN2       EQU   B'01111111100010111100000000000000'
SCAN_EXPECTED_VARIABLE      EQU   B'00001000000010111000000000000000'
SCAN_EXPECTED_VARIABLER     EQU   B'00001001000010111000000000000000'
SCAN_EXPECTED_VARIABLE2     EQU   B'00000001000010111000000000000000'
SCAN_EXPECTED_RULE          EQU   B'00001111001010001000000000000000'
SCAN_EXPECTED_RULE2         EQU   B'01111111000010111000000000000000'
SCAN_EXPECTED_RULE3         EQU   B'01111111000010111000000000000000'
SCAN_EXPECTED_CALL          EQU   B'00001100000010111000000000000000'
SCAN_EXPECTED_CALL2         EQU   B'01111111100010111100000000000000'
SCAN_EXPECTED_EXPAND        EQU   B'10001111100000110100000000000000'
SCAN_EXPECTED_PHONY         EQU   B'00001000000010000000000000000000'
SCAN_EXPECTED_PHONY2        EQU   B'01110000000010000000000000000000'
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
SCAN_EXPECTED_FUNCTION4     EQU   B'00001101100010110100000000000000'
SCAN_EXPECTED_IGNORE        EQU   B'01010000000000000000000000000000'
SCAN_EXPECTED_NEWLINE       EQU   B'01000000000000000000000000000000'
SCAN_EXPECTED_COMMENT       EQU   B'01110000000000000000000000000000'
*
* Stack of INPUT_DSECT structures, highest stack entry is the one
* LWZMAKE_SCAN_CHAR reads from. Initial size is 1, entry 0 filled
* with all zeros, indicating input from MAKEFILE DD
                            DS    0F
MAX_SCAN_INPUT_STACK_ENTRY  EQU   20
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
STMT_A_OPERATOR             DS    CL2  * type of assignment (= or :=)
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
* Variable area, first one pointed to by G_FIRST_VAR_PTR, each VARLOW
* and VARHIGH (possibly) point to variables with a name lower or higher
*
VAR_DSECT                   DSECT
VARLEN                      DS    H    * length of variable name
VARNAME                     DS    CL72 * variable name
VALLEN                      DS    H    * length of variable value
VALPTR                      DS    A    * pointer to value (getmain'd)
VARLOW                      DS    A    * pointer to variable with name
*                                      * lower than this one
VARHIGH                     DS    A    * pointer to variable with name
*                                      * higher than this one
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
                            DS    C    * reserved
*
INPUTLEAD                   DS    H    * leading spaces count
*
INPUTPTR                    DS    A    * for type != X'00' ptr to input
*                                      * string (e.g. variable value)
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
                            IHAPSA   DSECT=YES,LIST=YES
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
LWZ640I  DC    C'STATEMENT BLOCK ALLOCATE'
LWZ641I  DC    C'STATEMENT BLOCK FREE'
LWZ642I  DC    C'VARIABLE BLOCK ALLOCATE'
LWZ643I  DC    C'VARIABLE BLOCK FREE'
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
               STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block
               LR    R0,R1            * Have R0 point to new block
               L     R1,G_SCAN_TOKEN2_LEN * Get length of token 2
               L     R2,G_SCAN_TOKEN2A * Have R2 point to old block
               LR    R5,R2            * Save it for storage release
               LR    R3,R1            * Make sure no cropping/filling
               ST    R0,G_SCAN_TOKEN2A * Save ptr to new block
               MVCL  R0,R2            * Copy old to new block
               STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)
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
                  STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block
                  LR    R0,R1         * Have R0 point to new block
                  L     R1,G_SCAN_TOKEN3_LEN * Get length of token 3
                  L     R2,G_SCAN_TOKEN3A * Have R2 point to old block
                  LR    R5,R2         * Save it for storage release
                  LR    R3,R1         * Make sure no cropping/filling
                  ST    R0,G_SCAN_TOKEN3A * Save ptr to new block
                  MVCL  R0,R2            * Copy old to new block
                  STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)
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
         CLI   G_MKFEOF,C'Y'      * Are we at the end of makefile?
         BE    BREAK_STMT_LOOP    * Yes, stop looping
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
         CLI   G_MKFEOF,C'Y'      * Are we at the end of makefile?
         BE    SCAN_STMT_RET      * Yes, stop parsing statement
*
*        Only a rule type statement can start with a $ variable
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_VARIABLE) THEN
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
            CLC   G_STMT_SAVE_OP,=C':=' * Check for simply expanded
            IF (EQ) THEN          * If so...
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
*        Get returned pointer to new block of memory and put in in R7
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
         C     R6,=A(L'STMT_A_DEST) * Check if it fits
         IF (H) THEN                * If not, write error and stop
            MLWZMRPT RPTLINE=CL133'0Internal error, variable name longeX
               r than 72',APND_LC=C'Y'
            MVC   G_RETCODE,=F'12'  * Set return code 12
            BR    R8                * and return
         ENDIF
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
*        Add/update the variable to binary search tree for vars, but
*        not if we're in a recipe
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN
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
*        Get returned pointer to new block of memory and put in in R7
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
*        Clear token 2 length, which will receive the PHONY target name
         MVC   G_SCAN_TOKEN2_LEN,=F'0'
*
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement
*
         IF (CLI,G_SCAN_TOKENTYPE,NE,SCAN_TOKENTYPE_NORMAL) THEN
            MLWZMRPT RPTLINE=CL133'0.PHONY must be followed by a constaX
               nt target name',APND_LC=C'Y'
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
*        Set scan state bits to IN_PHONY2
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY2
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
         BNE   STMT_P_NEXT_TOKEN  * If not so, loop around
*
STMT_P_FINISH EQU *
*        Write trace record that statement is finished
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'
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
*        Get returned pointer to new block of memory and put in in R7
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
*        R7 points to the PHONY statement
         L     R15,LWZMAKE_STORE_PNYA_STMT * Get address STORE_PNY
         BASR  R14,R15            * Link to STORE_PNY section
*
STMT_PHONY_RET EQU *
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
            MVI   INPUTTYPE,X'01' * Set type of input to ptr to string
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
         MVC   G_SCAN_CLOSE_BRACKET,SCAN_VAR_SAVE_CLOSE_BRACKET
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
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function
*
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
            CLI   G_SCAN_APPEND_TO,X'00'
            BE    ADDPDSNAME_NEXT_MEMBER * Loop around for next token
            B     ADDPDSNAME4
         ENDIF
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    ADDPDSNAME_FINISH
*
*           Append member to token 3
            L     R0,G_SCAN_TOKEN2A
            A     R0,G_SCAN_TOKEN2_LEN
            L     R1,=F'1'
            LA    R2,=C'('
            LR    R3,R1
            MVCL  R0,R2
*
            L     R2,G_SCAN_TOKENA  * Point R2 to token 1
            L     R1,G_SCAN_TOKEN_LEN * Get length of token to append
            LR    R3,R1             * Make sure no cropping/filling
            MVCL  R0,R2             * Append to token 3
*
            L     R1,=F'1'
            LA    R2,=C')'
            LR    R3,R1
            MVCL  R0,R2
*
            L     R0,G_SCAN_TOKENA
            L     R1,G_SCAN_TOKEN2_LEN
            A     R1,G_SCAN_TOKEN_LEN
            LA    R1,2(,R1)
            ST    R1,G_SCAN_TOKEN_LEN
            L     R2,G_SCAN_TOKEN2A
            LR    R3,R1
            MVCL  R0,R2
*
            MVI   G_SCAN_APPEND_TO,X'02'
            LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3
            IF (Z) THEN             * Is this the first part of token 3
               MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of lead spaces
            ENDIF
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            MVI   G_SCAN_APPEND_TO,X'00'
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    ADDPDSNAME_FINISH
         ENDIF
*
ADDPDSNAME4 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_ADDPDSNAME4
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME4
*
         B     ADDPDSNAME_NEXT_MEMBER * Loop around to get next token
*
ADDPDSNAME_FINISH EQU *
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack
         XR    R2,R2                 * Clear R2
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index
         BCTR  R2,R0                 * Subtract 1 from index
         IC    R15,0(R2,R1)          * Get stack state in that idx
         STC   R15,G_SCAN_STATE      * And save it as current state
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx
*
         BAL   R7,SCAN_VAR_RESTORE
*
SCAN_ADDPDSNAME_RET EQU *
         BR    R8
*
* Scan function MEMBERLIST
*
SCAN_MEMBERLIST EQU *
         L     R15,LWZMAKE_APPEND_TOKENA_VAR
         BASR  R14,R15
*
         BAL   R7,SCAN_VAR_SAVE
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            MVC   G_SCAN_TOKEN3_LEN,=F'0'
         ENDIF
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_MEMBERLIST
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST
*
*        Get next token, which should be the start of the PDS name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function
*
         B     MEMBERLIST_PDSNAME_CHECK_VAR
*
MEMBERLIST_PDSNAME_NEXT_TOKEN EQU *
*        Get next token, which should be the PDS name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function
*
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA
         BE    MEMBERLIST3
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
         BE    MEMBERLIST_FINISH
*
         LT    R14,G_SCAN_TOKEN2_LEN
         IF (NZ) THEN
            LT    R14,G_SCAN_SPACE_COUNT
            IF (NZ) THEN
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as pds nameX
                in memberlist function',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               BR    R8             * and return
            ENDIF
         ENDIF
*
MEMBERLIST_PDSNAME_CHECK_VAR EQU *
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
            BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing statement
*
            MVC   G_SCAN_CLOSE_BRACKET,SCAN_VAR_SAVE_CLOSE_BRACKET
*
            B     MEMBERLIST2
         ENDIF
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    MEMBERLIST_FINISH
*
*           Append token 1 to token 2
            MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO
            MVI   G_SCAN_APPEND_TO,X'01'
            LT    R1,G_SCAN_TOKEN2_LEN
            IF (Z) THEN
               MVC   G_SCAN_SPACE_COUNT,=A(0)
            ENDIF
*
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    MEMBERLIST_FINISH
         ENDIF
*
MEMBERLIST2 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_MEMBERLIST2
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST2
*
         B     MEMBERLIST_PDSNAME_NEXT_TOKEN
*
MEMBERLIST3 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_MEMBERLIST3
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST3
*        If we're expanding
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
*           Clear token 3 length, which will receive the member(s)
            MVC   G_SCAN_TOKEN3_LEN,=F'0'
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
         ENDIF
*
MEMBERLIST_MEMFILTER EQU *
*        Get next token, which should be the a member name or variable
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function
*
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
            BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing statement
*
            MVC   G_SCAN_CLOSE_BRACKET,SCAN_VAR_SAVE_CLOSE_BRACKET
*
            CLI   G_SCAN_APPEND_TO,X'00'
            BE    MEMBERLIST_MEMFILTER * Loop around for next token
            B     MEMBERLIST4
         ENDIF
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    MEMBERLIST_FINISH
*
            MVI   G_SCAN_APPEND_TO,X'02'
            LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3
            IF (Z) THEN             * Is this the first part of token 3
               MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of lead spaces
            ENDIF
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            MVI   G_SCAN_APPEND_TO,X'00'
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    MEMBERLIST_FINISH
         ENDIF
*
MEMBERLIST4 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_MEMBERLIST4
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST4
*
         B     MEMBERLIST_MEMFILTER * Loop around to get next token
*
MEMBERLIST_FINISH EQU *
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack
         XR    R2,R2                 * Clear R2
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index
         BCTR  R2,R0                 * Subtract 1 from index
         IC    R15,0(R2,R1)          * Get stack state in that idx
         STC   R15,G_SCAN_STATE      * And save it as current state
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
*           Get the member list
            L     R15,LWZMAKE_GET_MEMLISTA_VAR * Get addr GET_MEMLIST
            BASR  R14,R15         * Link to GET_MEMLIST section
         ENDIF
*
         BAL   R7,SCAN_VAR_RESTORE
*
SCAN_MEMBERLIST_RET EQU *
         BR    R8
*
* Scan function FUNCTION
*
SCAN_FUNCTION EQU *
         L     R15,LWZMAKE_APPEND_TOKENA_VAR
         BASR  R14,R15
*
         BAL   R7,SCAN_VAR_SAVE
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            MVC   G_SCAN_TOKEN3_LEN,=F'0'
         ENDIF
*
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_FUNCTION
         OI    G_SCAN_STATE,SCAN_STATE_IN_FUNCTION
*
*        Get next token, which should be the start of a REXX name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_FUNCTION_RET  * Yes, stop parsing function
*
         B     FUNCTION_REXXNAME_CHECK_VAR
*
FUNCTION_REXXNAME_NEXT_TOKEN EQU *
*        Get next token, which should be the REXX name
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_FUNCTION_RET  * Yes, stop parsing function
*
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA
         BE    FUNCTION3
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
         BE    FUNCTION_FINISH
*
         LT    R14,G_SCAN_TOKEN2_LEN
         IF (NZ) THEN
            LT    R14,G_SCAN_SPACE_COUNT
            IF (NZ) THEN
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as REXX nameX
                in function',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               BR    R8             * and return
            ENDIF
         ENDIF
*
FUNCTION_REXXNAME_CHECK_VAR EQU *
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
            BNE   SCAN_FUNCTION_RET * Yes, stop parsing statement
*
            MVC   G_SCAN_CLOSE_BRACKET,SCAN_VAR_SAVE_CLOSE_BRACKET
*
            B     FUNCTION2
         ENDIF
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    FUNCTION_FINISH
*
*           Append token 1 to token 2
            MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO
            MVI   G_SCAN_APPEND_TO,X'01'
            LT    R1,G_SCAN_TOKEN2_LEN
            IF (Z) THEN
               MVC   G_SCAN_SPACE_COUNT,=A(0)
            ENDIF
*
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    FUNCTION_FINISH
         ENDIF
*
FUNCTION2 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_FUNCTION2
         OI    G_SCAN_STATE,SCAN_STATE_IN_FUNCTION2
*
         B     FUNCTION_REXXNAME_NEXT_TOKEN
*
FUNCTION3 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_FUNCTION3
         OI    G_SCAN_STATE,SCAN_STATE_IN_FUNCTION3
*        If we're expanding
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
*           Clear token 3 length, which will receive the member(s)
            MVC   G_SCAN_TOKEN3_LEN,=F'0'
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
         ENDIF
*
FUNCTION_PARAMETER_NEXT_TOKEN EQU *
*        Get next token, which should be the a member name or variable
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN
         BASR  R14,R15            * Link to SCAN_TOKEN section
*
         CLC   G_RETCODE,=F'0'    * Did an error occur?
         BNE   SCAN_FUNCTION_RET * Yes, stop parsing function
*
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
            BNE   SCAN_FUNCTION_RET * Yes, stop parsing statement
*
            MVC   G_SCAN_CLOSE_BRACKET,SCAN_VAR_SAVE_CLOSE_BRACKET
*
            CLI   G_SCAN_APPEND_TO,X'00'
            BE    FUNCTION_PARAMETER_NEXT_TOKEN * Loop around
            B     FUNCTION4
         ENDIF
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    FUNCTION_FINISH
*
            MVI   G_SCAN_APPEND_TO,X'02'
            LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3
            IF (Z) THEN             * Is this the first part of token 3
               MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of lead spaces
            ENDIF
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            MVI   G_SCAN_APPEND_TO,X'00'
         ELSE
            L     R15,LWZMAKE_APPEND_TOKENA_VAR
            BASR  R14,R15
*
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
            BE    FUNCTION_FINISH
         ENDIF
*
FUNCTION4 EQU *
*        Clear scan state except for left most bit indicating in recipe
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*        Set scan state bits to IN_FUNCTION4
         OI    G_SCAN_STATE,SCAN_STATE_IN_FUNCTION4
*
         B     FUNCTION_PARAMETER_NEXT_TOKEN * Loop around
*
FUNCTION_FINISH EQU *
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack
         XR    R2,R2                 * Clear R2
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index
         BCTR  R2,R0                 * Subtract 1 from index
         IC    R15,0(R2,R1)          * Get stack state in that idx
         STC   R15,G_SCAN_STATE      * And save it as current state
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx
*
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
*           Get the member list
            L     R15,LWZMAKE_CALL_FUNCA_VAR * Get addr CALL_FUNC
            BASR  R14,R15         * Link to CALL_FUNC section
         ENDIF
*
         BAL   R7,SCAN_VAR_RESTORE
*
SCAN_FUNCTION_RET EQU *
         BR    R8
*
* Save tokens
*
SCAN_VAR_SAVE EQU *
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            MVC   SCAN_VAR_SAVE_TOKENA,G_SCAN_TOKENA
            MVC   SCAN_VAR_SAVE_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN
            MVC   SCAN_VAR_SAVE_TOKEN_LEN,G_SCAN_TOKEN_LEN
            L     R4,G_SCAN_TOKEN_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKENA
            MVC   G_SCAN_TOKEN_LEN,=A(0)
*
            MVC   SCAN_VAR_SAVE_TOKEN2A,G_SCAN_TOKEN2A
            MVC   SCAN_VAR_SAVE_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN
            MVC   SCAN_VAR_SAVE_TOKEN2_LEN,G_SCAN_TOKEN2_LEN
            L     R4,G_SCAN_TOKEN2_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKEN2A
            MVC   G_SCAN_TOKEN2_LEN,=A(0)
*
            MVC   SCAN_VAR_SAVE_TOKEN3A,G_SCAN_TOKEN3A
            MVC   SCAN_VAR_SAVE_TOKEN3_MAXLEN,G_SCAN_TOKEN3_MAXLEN
            MVC   SCAN_VAR_SAVE_TOKEN3_LEN,G_SCAN_TOKEN3_LEN
            L     R4,G_SCAN_TOKEN3_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKEN3A
            MVC   G_SCAN_TOKEN3_LEN,=A(0)
         ENDIF
*
         BR    R7
*
* Restore tokens
*
SCAN_VAR_RESTORE EQU *
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN
            L     R2,G_SCAN_TOKEN_MAXLEN
            L     R3,G_SCAN_TOKENA
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
*
            MVC   G_SCAN_TOKENA,SCAN_VAR_SAVE_TOKENA
            MVC   G_SCAN_TOKEN_MAXLEN,SCAN_VAR_SAVE_TOKEN_MAXLEN
            MVC   G_SCAN_TOKEN_LEN,SCAN_VAR_SAVE_TOKEN_LEN
*
            L     R2,G_SCAN_TOKEN2_MAXLEN
            L     R3,G_SCAN_TOKEN2A
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
            MVC   G_SCAN_TOKEN2A,SCAN_VAR_SAVE_TOKEN2A
            MVC   G_SCAN_TOKEN2_MAXLEN,SCAN_VAR_SAVE_TOKEN2_MAXLEN
            MVC   G_SCAN_TOKEN2_LEN,SCAN_VAR_SAVE_TOKEN2_LEN
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
            MVI   INPUTTYPE,X'01' * Set type of input to ptr to string
            MVC   INPUTLEAD,G_SAVE_SPACE_COUNT+2
            MVC   INPUTLEN,G_SCAN_TOKEN3_LEN+2 * Copy value length
            MVC   INPUTPTR,G_SCAN_TOKEN3A * Copy value pointer
            MVC   INPUTPOS,=H'0'  * Set initial scan position to start
*
            DROP  R2
*
            MVC   G_SCAN_TOKEN3A,SCAN_VAR_SAVE_TOKEN3A
            MVC   G_SCAN_TOKEN3_MAXLEN,SCAN_VAR_SAVE_TOKEN3_MAXLEN
            MVC   G_SCAN_TOKEN3_LEN,SCAN_VAR_SAVE_TOKEN3_LEN
         ENDIF
*
         BR    R7
*
         LTORG
*
* Local constant pointers to section addresses
LWZMAKE_APPEND_TOKENA_VAR DC    A(LWZMAKE_APPEND_TOKEN)
LWZMAKE_SCAN_TOKENA_VAR   DC    A(LWZMAKE_SCAN_TOKEN)
LWZMAKE_FINDVARA_VAR      DC    A(LWZMAKE_FINDVAR)
LWZMAKE_SCAN_VARA_VAR     DC    A(LWZMAKE_SCAN_VAR)
LWZMAKE_GET_MEMLISTA_VAR  DC    A(LWZMAKE_GET_MEMLIST)
LWZMAKE_CALL_FUNCA_VAR    DC    A(LWZMAKE_CALL_FUNC)
*
SCAN_VAR_DSECT              DSECT
                            DS    18F * My savearea
*
SCAN_VAR_SAVE_APPEND_TO     DS    C
SCAN_VAR_SAVE_CLOSE_BRACKET DS    C
*
                            DS    0F
SCAN_VAR_SAVE_TOKEN_LEN     DS    F
SCAN_VAR_SAVE_TOKEN2_LEN    DS    F
SCAN_VAR_SAVE_TOKEN3_LEN    DS    F
SCAN_VAR_SAVE_TOKEN_MAXLEN  DS    F
SCAN_VAR_SAVE_TOKEN2_MAXLEN DS    F
SCAN_VAR_SAVE_TOKEN3_MAXLEN DS    F
SCAN_VAR_SAVE_TOKENA        DS    A
SCAN_VAR_SAVE_TOKEN2A       DS    A
SCAN_VAR_SAVE_TOKEN3A       DS    A
*
SCAN_VAR_DSECT_SIZ          EQU   *-SCAN_VAR_DSECT
*
LWZMAKE  CSECT
*
***********************************************************************
* Section: LWZMAKE_SCAN_TOKEN                                         *
* Purpose: Parsing lexer. This section invokes LWZMAKE_SCAN_CHAR to   *
*          get characetrs in order to return the next keyword. It     *
*          gets rid of whitespace, but counts the spaces leading the  *
*          next keyword should those need to be preserved.            *
*          Based on the current scan state, checks are performed      *
*          whether a type of token is allowed where it is found, in   *
*          other words most of the syntax checking is done here.      *
*          R7 is used as the index register to address the next       *
*          character position in G_SCAN_TOKEN.
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_SCAN_TOKEN MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_SCAN_TOKEN'
*
         MVC   G_SCAN_TOKEN_LEN,=F'0' * Initialize token 1 length
         MVI   G_SCAN_TOKENTYPE,X'00' * Initialize token 1 type
         MVC   G_SCAN_SPACE_COUNT,=F'0' * Initialize space count
         XR    R7,R7                  * Initialize index reg to token
*
STATE_AND_SCAN_FOR_WHITESPACE EQU *
*        Translate scan state to bitstring of allowed token types
         IC    R2,G_SCAN_STATE    * Get the scan state
         N     R2,=X'0000007F'    * Clear out bits 0-56, so including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         SLL   R2,2               * Multiply by 4 (scan state table
*                                 * contains full words)
         L     R1,SCAN_STATE_TABLEA_TOKEN * Point R1 to scan state tab
         AR    R1,R2              * Add offset for current scan stata
         MVC   G_SCAN_EXPECTED,0(R1) * Get the corresponding bitstring
*
SCAN_FOR_WHITESPACE EQU *
         L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address to SCAN_CHAR
         BASR  R14,R15            * Link to SCAN_CHAR section
*
*        Check for end of file and whether it's expected
         IF (CLI,G_MKFEOF,EQ,C'Y') THEN * Is EOF switch on?
*                                 * And was it expected?
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_EOF,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected end of file',APND_LC=X
               C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
            ENDIF
            B     SCAN_TOKEN_RET  * Skip rest of tokenizer
         ENDIF
*
*        Check for beginning a new line, when allowed and not in a
*        continued statement, this resets the scan state to not in stmt
         IF (CLI,G_SCAN_NEWLINE,EQ,C'Y') THEN * Is new line switch on?
*                                 * And was it expected?
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_NEWLINE,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected new line',APND_LC=C'YX
               '
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*           Check if we're not in a continued line
            IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN
               IC    R14,G_SCAN_STATE * Get the scan state
               N     R14,=X'0000007F' * Clear out bits 0-56, including
*                                     * the high order bit in the scan
*                                     * state for 'in recipe'
               C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in stmt
               IF (NE) THEN           * If not so...
*                 Clear scan state except for left most bit 'in recipe'
                  NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*                 Set scan state bits to NOT_IN_STMT
                  OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT
*                 Skip to finishing valid token
                  B     SCAN_TOKEN_VALID
               ENDIF
            ENDIF
            B     STATE_AND_SCAN_FOR_WHITESPACE * Loop for next char
         ENDIF
*
*        Check space, if so add to leading space char count
         IF (CLI,G_SCAN_CURRCHAR,EQ,C' ') THEN
            L     R2,G_SCAN_SPACE_COUNT * Get current space count
            LA    R2,1(,R2)             * Add 1
            ST    R2,G_SCAN_SPACE_COUNT * And put it back
            B     SCAN_FOR_WHITESPACE   * Loop for next char
         ENDIF
*
*        Anything beyond column 72 is ignored and considered the end
*        of a line
         L     R6,G_SCAN_CURRCOL  * Get current column
         C     R6,=F'72'          * Check if we're beyond col 72
         IF (GE) THEN             * If so (currcol starts with 0)
*           Set token type to ignore
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_IGNORE
*           Was it expected?
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_IGNORE,Z) THEN
*              Prepare helper data for parse error trace record
               MVC   G_HELPER_DATA(23),=C'UNEXPECTED END OF LINE '
               MVC   G_HELPER_DATA+23(1),G_SCAN_STATE
               MVI   G_HELPER_DATA+24,C' '
               MVC   G_HELPER_DATA+25(4),G_SCAN_EXPECTED
               LA    R14,G_HELPER_DATA * Get address to helper data
               ST    R14,G_LWZMTRC_DATA_PTR * Set it as trace data ptr
               MVC   G_LWZMTRC_DATA_SIZ,=AL2(29) * Set trace data len
               MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'003',DATA
               MLWZMRPT RPTLINE=CL133'0Unexpected end of line',APND_LC=X
               C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*
CHECK_NEXT_IGNORE_CHAR EQU *
*           Reaching column 80 in combination with not a continued line
*           resets scan state to not in stmt
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'79'         * Check column 80
            IF (GE) THEN            * If we're there
*                                   * And it's not a continued line
               IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN
                  IC    R14,G_SCAN_STATE * Get the scan state
                  N     R14,=X'0000007F' * Clear out bits 0-56,
*                                    * including the high order bit in
*                                    * the scan state for 'in recipe'
                  C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check
                  IF (NE) THEN       * not in stmt?
*                    Clear scan state except left most bit 'in recipe'
                     NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*                    Set scan state bits to NOT_IN_STMT
                     OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT
*                    Skip to finishing valid token
                     B     SCAN_TOKEN_VALID
                  ENDIF
               ENDIF
*              Overrule bitstring with that for a new line
               MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_NEWLINE)
*              Jump to scan for whitespace, which for a new line after
*              continuation jumps to STATE_AND_SCAN_FOR_WHITESPACE and
*              reset to the correct bitstring
               B     SCAN_FOR_WHITESPACE
            ENDIF
*           We end up here for chars in columns 72-80
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR
            BASR  R14,R15          * Link to SCAN_CHAR section
            CLI   G_MKFEOF,C'Y'    * At this point EOF should
            BE    SCAN_TOKEN_VALID * return valid ignore token
            CLI   G_SCAN_NEWLINE,C'Y' * Same goes for new line
            BE    SCAN_TOKEN_VALID * returns valid ignore token
            B     CHECK_NEXT_IGNORE_CHAR * None of the above, so loop
*                                  * around checking next ignore char
         ENDIF
*
*        At this point we've ruled out EOF, new line and pos 72-80
*        so we can start checking token types.
*        Point R5 to current char to start checking.
         LA    R5,G_SCAN_CURRCHAR
*
*        Check for comments
         IF (CLI,0(R5),EQ,C'#') THEN
*           Set token type to comment
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMENT
*           Prepare helper data for start parse token trace record
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'612',CONST=C'COX
               MMENT'
*           Was comment expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_COMMENT,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected comment',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
CHECK_NEXT_COMMENT_CHAR EQU *
*           Anything up to column 72 is considered part of the comments
*           after that, if not in continued line we can reset scan
*           state to not in stmt
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'71'         * Check if we're at 72 yet
            IF (NL) THEN            * If so...
*              And it's not a continued line
               IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN
                  IC    R14,G_SCAN_STATE * Get the scan state
                  N     R14,=X'0000007F' * Clear out bits 0-56,
*                                    * including the high order bit in
*                                    * the scan state for 'in recipe'
                  C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check
                  IF (NE) THEN       * not in stmt?
*                    Clear scan state except left most bit 'in recipe'
                     NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*                    Set scan state bits to NOT_IN_STMT
                     OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT
*                    Skip to finishing valid token
                     B     SCAN_TOKEN_VALID
                  ENDIF
               ENDIF
*              Overrule bitstring with that for ignore chars
               MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_IGNORE)
*              Jump to scan for whitespace, to check for ignore chars
*              and newline / EOF
               B     SCAN_FOR_WHITESPACE
            ENDIF
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR
            BASR  R14,R15                 * Link to SCAN_CHAR section
            B     CHECK_NEXT_COMMENT_CHAR * Loop for next comment char
         ENDIF
*
*        If we ended up here and we've already had line continuation
*        character, anything other than comment is a syntax error
         IF (CLI,G_SCAN_CONTINUED_LINE,EQ,C'Y') THEN
            MLWZMRPT RPTLINE=CL133'0Syntax error, only comments allowedX
                after continuation character',APND_LC=C'Y'
            MVC   G_RETCODE,=F'8' * Set return code 8
            B     SCAN_TOKEN_RET  * Skip the rest of tokenizer
         ENDIF
*
*        Check for continuation character
         IF (CLI,0(R5),EQ,C'\') THEN
*           Set token type to continuation
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CONTINUATION
*           Set switch for continuation to true
            MVI   G_SCAN_CONTINUED_LINE,C'Y'
*           Overrule bitstring with that for comments
            MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_COMMENT)
*           Jump to scan for whitespace, to look for comments, ignore
*           chars and newline / EOF
            B     SCAN_FOR_WHITESPACE
         ENDIF
*
*        Only on column 1 check for recipe prefix
         L     R6,G_SCAN_CURRCOL  * Get current column
         C     R6,=F'0'           * Check for column 1
         IF (EQ) THEN             * If so...
            CLC   0(1,R5),G_RECIPEPREFIX * Check for recipe prefix char
            IF (EQ) THEN          * If that's the case...
*              Set token type to recipe prefix
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_RECIPEPREFIX
*              Was it expected? If not, write error and stop
               IF (TM,G_SCAN_EXPECTED+2,SCAN_EXPECTED3_RECIPREF,Z) THEN
UNEXPECTED_RECIPE EQU *
                  MLWZMRPT RPTLINE=CL133'0Unexpected recipe',APND_LC=C'X
               Y'
                  MVC   G_RETCODE,=F'8' * Set return code 8
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer
               ENDIF
*              In rare cases the expected bitstring doesn't cover it,
*              e.g. a recipe line followed by an empty line and
*              another recipe line. That's why this elaborate check.
*              If the previous statement wasn't a rule
               IF (CLI,G_PREV_STMT_TYPE,NE,STMT_TYPE_RULE) THEN
*                 and the scan state isn't in recipe
                  IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN
*                    and the previous scan state wasn't either
                     IF (CLI,G_PREV_STMT_IN_RECIPE,NE,C'Y') THEN
*                       then also write error and stop
                        B     UNEXPECTED_RECIPE
                     ENDIF
                  ENDIF
               ENDIF
*              If we end up here the recipe prefix was valid, so set
*              the bit in the scan state
               OI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*              Loop around to continue scanning for whitespace
               B     SCAN_FOR_WHITESPACE
            ENDIF
         ENDIF
*
*        Check for assignment operator
         IF (CLI,0(R5),EQ,C'=') THEN
*           Set token type to operator
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_OPERATOR
*           Was it expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_OPERATOR,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected operator',APND_LC=C'YX
               '
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
            B     SCAN_TOKEN_VALID   * Skip to finishing valid token
         ENDIF
*
*        Check for colon, which could be a rule or byte 1 of :=
         IF (CLI,0(R5),EQ,C':') THEN
*           Set token type to rule (check for := follows)
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_RULE
*           If the next char will return =
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'=') THEN
*              Check if an operator is expected, if not error and stop
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_OPERATOR,Z) THEN
                  MLWZMRPT RPTLINE=CL133'0Unexpected operator',APND_LC=X
               C'Y'
                  MVC   G_RETCODE,=F'8' * Set return code 8
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer
               ENDIF
            ELSE
*              If next char is not =, so it's a rule
*              Check if a rule is expected, if not error and stop
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_RULE,Z) THEN
                  MLWZMRPT RPTLINE=CL133'0Unexpected colon',APND_LC=C'YX
               '
                  MVC   G_RETCODE,=F'8' * Set return code 8
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer
               ENDIF
            ENDIF
*           If we end up here the rule or operator was valid
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*           Check for := on pos 71 (making = an ignore char)
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'71'         * Check for pos 72
            BNL   SCAN_TOKEN_VALID  * Skip to finishing valid token
*           Check again for next char =
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'=') THEN
*              Set token type to operator
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_OPERATOR
               L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get addr SCAN_CHAR
               BASR  R14,R15             * Link to SCAN_CHAR section
               BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
            ENDIF
            B     SCAN_TOKEN_VALID * Skip to finishing valid token
         ENDIF
*
*        Check for a variable
         IF (CLI,0(R5),EQ,C'$') THEN
*           Set token type to variable (check for $@ or $% follows)
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_VARIABLE
*           Was it expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_OPENVAR,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected variable or function'X
               ,APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*
*           Check if next char will be @
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'@') THEN
*              Set token type to target variable
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_ACRO
*              Was it expected? If so, continue checking because the
*              expected bitstring isn't complete
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_ACRO,O) THEN
*                 Check if in recipe
                  TM    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
                  IF (Z) THEN     * Not in recipe, keep on checking
*                    Check if in expand (resolving variables in rule
*                    requisites during phase 2)
                     TM    G_SCAN_STATE,SCAN_STATE_IN_EXPAND
                  ENDIF
*                 If either of the tests above gives CC ones
                  IF (O) THEN
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
                     L     R15,LWZMAKE_SCAN_CHARA_TOKEN
                     BASR  R14,R15             * Link to SCAN_CHAR
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
                     B     SCAN_TOKEN_VALID    * Skip to finish token
                  ENDIF
               ENDIF
*              If not expected or the other tests above fail, write
*              error and stop
               MLWZMRPT RPTLINE=CL133'0Unexpected target variable',APNDX
               _LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*
*           Check if next char will be %
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'%') THEN
*              Set token type to target member variable
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_PERCENT
*              Was it expected? If so, continue checking because the
*              expected bitstring isn't complete
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_PERCENT,O) THEN
*                 Check if in recipe
                  TM    G_SCAN_STATE,SCAN_STATE_IN_RULE2
                  IF (Z) THEN     * Not in rule2, keep on checking
                     TM    G_SCAN_STATE,SCAN_STATE_IN_RULE3
                  ENDIF
                  IF (Z) THEN     * Not in rule3, keep op checking
*                    Check if in expand (resolving variables in rule
*                    requisites during phase 2)
                     TM    G_SCAN_STATE,SCAN_STATE_IN_EXPAND
                  ENDIF
*                 If either of the tests above gives CC ones
                  IF (NO) THEN
                     MVI   G_SCAN_TOKENTYPE,X'00'
                  ENDIF
                  BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
                  L     R15,LWZMAKE_SCAN_CHARA_TOKEN
                  BASR  R14,R15             * Link to SCAN_CHAR
                  BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
                  B     SCAN_TOKEN_VALID    * Skip to finish token
               ELSE
                  MLWZMRPT RPTLINE=CL133'0Unexpected target member variX
               able',APND_LC=C'Y'
                  MVC   G_RETCODE,=F'8' * Set return code 8
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer
               ENDIF
            ENDIF
*
*           So it's not $@ or $%, continue checking normal variable
*           If we're at pos 71 or more, no sense checking the rest
*           because there's not enough room for a variable
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'70'         * Check for pos 71 or above
            IF (NL) THEN            * If so write error and stop
               MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*
*           Check if next char will be either ( or {
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'(') THEN
*              Save matching close bracket to check later
               MVI   G_SCAN_CLOSE_BRACKET,C')'
            ELSE
               IF (CLI,G_SCAN_PEEKCHAR,EQ,C'{') THEN
*                 Save matching close bracket to check later
                  MVI   G_SCAN_CLOSE_BRACKET,C'}'
               ELSE
*                 Neither ( nor { is a syntax error
                  MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'
                  MVC   G_RETCODE,=F'8' * Set return code 8
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer
               ENDIF
            ENDIF
*
*           Valid variable start, so store in token 1
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR
            BASR  R14,R15             * Link to SCAN_CHAR section
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*
*           The next bit of code is a dirty trick because determining
*           if the scan state should be set to rule should really only
*           be done in the statement parser section.
*           The thing is, when a variable is the first token the scan
*           state should be IN_RULE before pushing it on the stack and
*           setting it to IN_VARIABLE(R)
            IC    R14,G_SCAN_STATE * Get the scan state
            N     R14,=X'0000007F' * Clear out bits 0-56, including
*                                  * the high order bit in the scan
*                                  * state for 'in recipe'
            C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in stmt
            IF (EQ) THEN           * If so...
*              Clear scan state except for left most bit 'in recipe'
               NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
*              Set scan state bits to IN_RULE
               OI    G_SCAN_STATE,SCAN_STATE_IN_RULE
            ENDIF
*
*           Push current scan state on the stack before setting
*           it to IN_VARIABLE(R)
            LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack
            XR    R2,R2                 * Clear R2
            IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index
            IC    R15,G_SCAN_STATE      * Get current scan state
            STC   R15,0(R2,R1)          * and store it in the stack
            LA    R2,1(,R2)             * Add 1 to stack index
            C     R2,=A(L'G_SCAN_STATE_STACK) * Is stack full?
            IF (H) THEN                 * Yep, write error and stop
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX
               flow',APND_LC=C'Y'
               MVC   G_RETCODE,=F'12'   * Set return code 12
               B     SCAN_TOKEN_RET     * Skip rest of tokenizer
            ENDIF
            STC   R2,G_SCAN_STATE_STACK_IDX * Store new stack size
*
*           Clear scan state except left most bit 'in recipe', which
*           sets CC
            NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE
            IF (Z) THEN           * If in recipe bit is off
*              Set scan state bits to IN_VARIABLE
               OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLE
            ELSE                  * Else if recipe bit is on
*              Set scan state bits to IN_VARIABLER
               OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLER
            ENDIF
            B     SCAN_TOKEN_VALID * Skip to finishing valid token
         ENDIF
*
*        Check for a closing bracket
         CLI   0(R5),C')'          * Is it a )
         IF (NE) THEN              * If not
            CLI   0(R5),C'}'       * Is it a }
         ENDIF
         IF (EQ) THEN              * If it was ) or }
*           Set token type to closing bracket
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET
*           Was if expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_CLOSEBRC,Z) THEN
UNEXPECTED_CLOSE_BRACKET EQU *
               MLWZMRPT RPTLINE=CL133'0Unexpected close bracket',APND_LX
               C=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*
*           Check if close bracket matches previous open bracket
            CLC   0(1,R5),G_SCAN_CLOSE_BRACKET
            BNE UNEXPECTED_CLOSE_BRACKET
*
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*
*           Check if scan state is form of IN_VARIABLE*, if not it's
*           just a closing bracket, meaning no pop of scan state stack
            IC    R14,G_SCAN_STATE * Get the scan state
            N     R14,=X'0000007F' * Clear out bits 0-56, including
*                                  * the high order bit in the scan
*                                  * state for 'in recipe'
            C     R14,=A(SCAN_STATE_IN_VARIABLE2) * Check in variable2
            IF (NE) THEN           * If not, check in variableR
               C     R14,=A(SCAN_STATE_IN_VARIABLER)
               BNE   SCAN_TOKEN_VALID * Neither, so skip to finish
            ENDIF
            LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack
            XR    R2,R2                 * Clear R2
            IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index
            BCTR  R2,R0                 * Subtract 1 from index
            IC    R15,0(R2,R1)          * Get stack state in that idx
            STC   R15,G_SCAN_STATE      * And save it as current state
            STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx
            B     SCAN_TOKEN_VALID      * Skip to finishing valid token
         ENDIF
*
*        Check for a comma
         CLI   0(R5),C','          * Is it a ,
         IF (EQ) THEN              * If it was ) or }
*           Set token type to comma
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA
*           Was if expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED+2,SCAN_EXPECTED3_COMMA,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected comma',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer
            ENDIF
*
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
            B     SCAN_TOKEN_VALID      * Skip to finishing valid token
         ENDIF
*
*        Special variables are the first token in a new statement, so
*        if we're already in one, skip the special variable check
         IC    R14,G_SCAN_STATE   * Get the scan state
         N     R14,=X'0000007F'   * Clear out bits 0-56, including
*                                 * the high order bit in the scan
*                                 * state for 'in recipe'
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in statement
         BNE   SKIP_SCAN_SPECIAL  * If not skip special var check
*
*        Check for special variable. This snippet only checks for a
*        variable that starts with . and consists of valid special var
*        characters. Whether it's semantically valid is checked in the
*        statement parser.
         IF (CLI,0(R5),EQ,C'.') THEN
*           Set token type to special variable
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_SPECIAL
*           Was it expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_SPECIAL,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected special variable',APNX
               D_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer
            ENDIF
*
STORE_NEXT_SPECIAL_TOKEN_CHAR EQU *
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*
*           Positions up to 72 count as part of the special var name
*           We check peek char so when the next token is parsed,
*           scanning continues with the char directly after the special
*           variable name
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'71'         * Check pos 72 and if we're there
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token
*
            LA    R2,G_SCAN_PEEKCHAR * Point R2 to peek char
            TRT   0(1,R2),SPECIAL_TOKEN_NEXTCHAR * Check for valid char
            BNZ   SCAN_TOKEN_VALID  * If not, skip to finish token
*           Ending up here means the char is valid, to scan it and
*           loop around to store it and check the next
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR
            BASR  R14,R15           * Link to SCAN_CHAR section
            B     STORE_NEXT_SPECIAL_TOKEN_CHAR * Loop around
         ENDIF
*
SKIP_SCAN_SPECIAL EQU *
*
*        Check for normal token
         TRT   0(1,R5),NORMAL_TOKEN_STARTCHAR
         IF (Z) THEN
*           Set token type to normal
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_NORMAL
*           Was it expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_NORMAL,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected token',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer
            ENDIF
*
STORE_NEXT_NORMAL_TOKEN_CHAR EQU *
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*
*           Positions up to 72 count as part of the normal token name
*           We check peek char so when the next token is parsed,
*           scanning continues with the char directly after the normal
*           token name
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'71'         * Check pos 72 and if we're there
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token
*
            LA    R2,G_SCAN_PEEKCHAR * Point R2 to peek char
            TRT   0(1,R2),NORMAL_TOKEN_NEXTCHAR * Check for valid char
            BNZ   SCAN_TOKEN_VALID   * If not, skip to finish token
*           Ending up here means the char is valid, to scan it and
*           loop around to store it and check the next
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR
            BASR  R14,R15         * Link to SCAN_CHAR section
            B     STORE_NEXT_NORMAL_TOKEN_CHAR * Loop around
         ENDIF
*
*        Check for number token
         TRT   0(1,R5),NUMBER_TOKEN_CHAR
         IF (Z) THEN
*           Set token type to number
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_NUMBER
*           Was it expected? If not, write error and stop
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_NUMBER,Z) THEN
               MLWZMRPT RPTLINE=CL133'0Unexpected number',APND_LC=C'Y'
               MVC   G_RETCODE,=F'8' * Set return code 8
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer
            ENDIF
*
STORE_NEXT_NUMBER_TOKEN_CHAR EQU *
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*
*           Positions up to 72 count as part of the number token name
*           We check peek char so when the next token is parsed,
*           scanning continues with the char directly after the number
*           token name
            L     R6,G_SCAN_CURRCOL * Get current column
            C     R6,=F'71'         * Check pos 72 and if we're there
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token
*
            LA    R5,G_SCAN_PEEKCHAR * Point R2 to peek char
            TRT   0(1,R5),NUMBER_TOKEN_CHAR * Check for valid char
            BNZ   SCAN_TOKEN_VALID   * If not, skip to finish token
*           Ending up here means the char is valid, to scan it and
*           loop around to store it and check the next
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR
            BASR  R14,R15         * Link to SCAN_CHAR section
            B     STORE_NEXT_NUMBER_TOKEN_CHAR * Loop around
         ENDIF
*
*        If the scanned character doesn't start any of the previous
*        types, just store the character, don't set a type and just let
*        the parser take care of it
         BAL   R8,STORE_TOKEN_CHAR * Add char to token 1
*
*        Previous code jumps here when a valid done is completely
*        scanned
SCAN_TOKEN_VALID EQU *
*        If normal token, it could be a call keyword
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN
            CLC   G_SCAN_TOKEN_LEN,=F'4' * Is the token 4 bytes long?
            IF (EQ) THEN
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1
               MVC   G_CALL4,0(R14)       * Copy to helper var
               OC    G_CALL4,=X'40404040' * Convert to uppercase
               CLC   G_CALL4,=C'CALL'     * Is it CALL?
               IF (EQ) THEN               * If so...
*                 Set token type to call
                  MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CALL
               ENDIF
            ENDIF
         ENDIF
*
*        Write a trace record for the token we just scanned
*        Put token type char and scanned token in helper data
         MVC   G_HELPER_DATA(1),G_SCAN_TOKENTYPE * Store token type
         MVI   G_HELPER_DATA+1,C' '  * followed by space
         LA    R2,G_HELPER_DATA+2    * Point R2 to where token comes
         L     R3,G_SCAN_TOKENA      * Point R3 to scanned token
         L     R4,G_SCAN_TOKEN_LEN   * Get length of scanned token
         C     R4,=A(L'G_HELPER_DATA-2) * Compare it to leftover space
         IF (H) THEN                 * If too long
            L     R4,=A(L'G_HELPER_DATA-2) * Replace len with what fits
         ENDIF
         LA    R5,2(,R4)             * Put the correct length in R5
         BCTR  R4,R0                 * R4 = R4 - 1 for EX
         B     *+10                  * Skip MVC constant for EX
         MVC   0(1,R2),0(R3)         * MVC constant for EX
         EX    R4,*-6                * EX previous MVC stmt with R4
         LA    R2,G_HELPER_DATA      * Get address of helper data
         ST    R2,G_LWZMTRC_DATA_PTR * And store it as trace data ptr
         STH   R5,G_LWZMTRC_DATA_SIZ * And store length as data length
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'610',DATA
*
SCAN_TOKEN_RET EQU *
         MLWZTERM                 * Return back to caller
*
* Store token char routine, appends a scanned char to token 1
* R7 is expected to contain the position within token 1 where the next
* character should be stored (used as an index register).
*
STORE_TOKEN_CHAR EQU *
         C     R7,=A(SCAN_TOKEN_MAXLEN) * Have we exhausted token 1?
         IF (GT) THEN             * If so, write error and stop
            MLWZMRPT RPTLINE=CL133'0Internal error, no more room left iX
               n 4K token space',APND_LC=C'Y'
            MVC   G_RETCODE,=F'12' * Set return code 12
            B     SCAN_TOKEN_RET   * Dirty jump to end of tokenizer
         ENDIF                     * but quicker than check retcode
*                                  * everytime after this routine
         L     R4,G_SCAN_TOKENA   * Point R4 to token 1
         IC    R5,G_SCAN_CURRCHAR * Get the last scanned character
         STC   R5,0(R7,R4)        * And store it at index R7
         LA    R7,1(,R7)          * Increase index R7 with 1
         ST    R7,G_SCAN_TOKEN_LEN * and store it as token 1 length
         BR    R8                 * Return
*
         LTORG
*
* Local constant pointers to section addresses
LWZMAKE_SCAN_CHARA_TOKEN     DC    A(LWZMAKE_SCAN_CHAR)
*
* Local constant pointers to previously defined constants, but too far
* away for local addressing
SCAN_STATE_TABLEA_TOKEN      DC    A(SCAN_STATE_TABLE)
*
* Translate table for starting character for a normal token
* Can be [a-zA-Z]
NORMAL_TOKEN_STARTCHAR DS 0F
         DC    256X'FF'
         ORG   NORMAL_TOKEN_STARTCHAR+C'a'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_STARTCHAR+C'j'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_STARTCHAR+C's'
         DC    X'0000000000000000'
         ORG   NORMAL_TOKEN_STARTCHAR+C'A'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_STARTCHAR+C'J'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_STARTCHAR+C'S'
         DC    X'0000000000000000'
         ORG
*
* Translate table for any character for a normal token except the first
* Can be [$#@_a-zA-Z0-9]
NORMAL_TOKEN_NEXTCHAR DS 0F
         DC    256X'FF'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'$'
         DC    X'00'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'#'
         DC    X'00'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'@'
         DC    X'00'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'_'
         DC    X'00'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'a'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'j'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_NEXTCHAR+C's'
         DC    X'0000000000000000'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'A'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'J'
         DC    X'000000000000000000'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'S'
         DC    X'0000000000000000'
         ORG   NORMAL_TOKEN_NEXTCHAR+C'0'
         DC    X'00000000000000000000'
         ORG
*
* Translate table for any character for a special token
* Can be [_A-Z]
SPECIAL_TOKEN_NEXTCHAR DS 0F
         DC    256X'FF'
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'_'
         DC    X'00'
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'A'
         DC    X'000000000000000000'
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'J'
         DC    X'000000000000000000'
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'S'
         DC    X'0000000000000000'
         ORG
*
* Translate table for any character for a number token
* Can be [0-9]
NUMBER_TOKEN_CHAR DS 0F
         DC    256X'FF'
         ORG   NUMBER_TOKEN_CHAR+C'0'
         DC    X'00000000000000000000'
         ORG
*
***********************************************************************
* Section: LWZMAKE_SCAN_CHAR                                          *
* Purpose: Parsing scanner. This section gets the next character from *
*          whichever input is at the top of the input stack. For a    *
*          makefile, after column 80 a next record is read, any other *
*          type of input is simply read until its last char. If an    *
*          input is exhausted it is popped from the input stack.      *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_SCAN_CHAR MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEEBUG,MSGNR=C'604',CONST=C'LWZMX
               AKE_SCAN_CHAR'
*
         MVI   G_SCAN_NEWLINE,C'N' * Initialize newline
         MVC   G_SCAN_CURRCHAR(2),=X'0000' * Init CURRCHAR + PEEKCHAR
*
SCAN_CHAR_CHECK_INPUT_STACK EQU *
*        Check for empty input stack, of so set EOF and skip the rest
         IF (CLI,G_SCAN_INPUT_STACK_IDX,EQ,X'00') THEN
            MVI   G_MKFEOF,C'Y'   * Set EOF switch to Y
            B     SCAN_CHAR_RET   * Skip rest of scanner
         ELSE
*           Get the top entry in the input stack
            XR    R2,R2           * Clear R2
            XR    R3,R3           *   and R3
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index
            BCTR  R3,R0           * Subtract 1 for calculating offset
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to entry
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack
            AR    R2,R3           * Add calculated offset
*
            USING INPUT_DSECT,R2  * Address with INPUT DSECT
*
            CLI   INPUTTYPE,X'00' * Check for input from makefile
            BE    SCAN_CHAR_READ_FROM_MAKEFILE * If so jump ahead
*
            XR    R3,R3
            LH    R3,INPUTLEAD
            CH    R3,=H'0'
            IF (NE) THEN
               MVI   G_SCAN_CURRCHAR,C' '
               BCTR  R3,R0
               STH   R3,INPUTLEAD
               B     SCAN_CHAR_RET
            ENDIF
*
            XR    R3,R3           * Clear R3
            LH    R3,INPUTPOS     * Get next position in input
            CH    R3,INPUTLEN     * Has input been exhausted?
            IF (L) THEN           * If not...
               L     R4,INPUTPTR  * Point R4 to input
               IC    R5,0(R3,R4)  * Get the next character from input
               STC   R5,G_SCAN_CURRCHAR * and put it in CURRCHAR
               LA    R3,1(,R3)    * Advance next position
               STH   R3,INPUTPOS  * and store it in input block
               CH    R3,INPUTLEN  * Was this the last char?
               IF (L) THEN        * If not, also get PEEKCHAR
                  IC    R5,0(R3,R4) * Get the next char + 1 from input
                  STC   R5,G_SCAN_PEEKCHAR * and put it in PEEKCHAR
               ENDIF
               B     SCAN_CHAR_RET * Skip rest of scanner
            ELSE                  * Else, input exhausted
               XR    R3,R3        * Clear R3
               IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack idx
               BCTR  R3,R0        * Subtract 1
               STC   R3,G_SCAN_INPUT_STACK_IDX * and put back in stack
               CLI   INPUTTYPE,X'01' * Input type string that continues
*                                 * with popped stack entry?
               BNE   SCAN_CHAR_RET * If not, skip rest of scan char
               B     SCAN_CHAR_CHECK_INPUT_STACK * Loop around to
*                                 * check the input stack again
            ENDIF
         ENDIF
         DROP  R2
*
SCAN_CHAR_READ_FROM_MAKEFILE EQU *
*        A makefile has LRECL 80 but in order to return a line change
*        as a separate token, a fictitious 81st column is used for that
*        When we go beyond column 81 that triggers a next record to be
*        read from the makefile.
         L     R3,G_SCAN_CURRCOL  * Get the current column
         LA    R3,1(,R3)          * Advance 1 position
         ST    R3,G_SCAN_CURRCOL  * and put it back
         C     R3,=F'80'          * Check if we're past 81 yet
         IF (GT) THEN             * If so...
            MVI   G_SCAN_CONTINUED_LINE,C'N' * Reset continued line
            BAL   R8,READNEXT     * Read another input record
         ELSE                     * Not past 81
            IF (EQ) THEN          * Buf if exactly at 81
               MVI   G_SCAN_NEWLINE,C'Y' * Set new line switch to Y
               B     SCAN_CHAR_RET       * Skip rest of scanner
            ENDIF
         ENDIF
*
*        Check for EOF
         CLI   G_MKFEOF,C'Y'      * Is the EOF switch set?
         BE    SCAN_CHAR_RET      * If so skip rest of scanner
*
*        If we're here, we're scanning a column between 1 and 80
         LA    R4,G_MAKEFILE_REC  * Point R4 to the last read record
         L     R3,G_SCAN_CURRCOL  * Get the current column
         IC    R5,0(R3,R4)        * Get the next character from input
         STC   R5,G_SCAN_CURRCHAR * and put it in CURRCHAR
         C     R3,=F'79'          * Check pos = 80
         IF (LT) THEN             * Only pos < 80 there's a PEEKCHAR
            IC    R5,1(R3,R4)     * Get the next char + 1 from input
            STC   R5,G_SCAN_PEEKCHAR * and put it in PEEKCHAR
         ENDIF
*
*        The rest of this scanner code is for writing a deeebug trace
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEEBUG) THEN
            LA    R2,G_HELPER_DATA   * Point R2 to helper data
            MVC   0(5,R2),=C'LINE '  * Start with line constant
            LA    R2,5(,R2)          * Advance R2 to right after
            LA    R6,5               * Use R6 as length
            L     R3,G_SCAN_CURRLINE * Get the current line number
            CVD   R3,G_DEC8          * Convert it to packed decimal
            UNPK  G_ZONED8,G_DEC8    * Convert it to zoned
            OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble
            MVC   0(8,R2),G_ZONED8   * Append line number to trc data
            LA    R2,8(,R2)          * Advance R2 to right after
            LA    R6,8(,R6)          * Add 8 to length
            MVC   0(8,R2),=C' COLUMN ' * Append column constant
            LA    R2,8(,R2)          * Advance R2 to right after
            LA    R6,8(,R6)          * Add 8 to length
            L     R3,G_SCAN_CURRCOL  * Get the current column numer
            LA    R3,1(,R3)          * Add 1 because it's zero based
            CVD   R3,G_DEC8          * Convert it to packed decimal
            UNPK  G_ZONED8,G_DEC8    * Convert it to zoned
            OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble
            MVC   0(8,R2),G_ZONED8   * Append column number to trc data
            LA    R2,8(,R2)          * Advance R2 to right after
            LA    R6,8(,R6)          * Add 8 to length
            MVI   0(R2),C' '         * Append a space
            MVC   1(1,R2),G_SCAN_CURRCHAR * Append current char
            MVI   2(R2),C' '         * Append another space
            MVC   3(1,R2),G_SCAN_PEEKCHAR * Append peek char
            LA    R6,4(,R6)          * Add 4 to length
            LA    R2,G_HELPER_DATA   * Get address of helper data
            ST    R2,G_LWZMTRC_DATA_PTR * Set is as trace data pointer
            STH   R6,G_LWZMTRC_DATA_SIZ * Store R6 as trace data length
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEEBUG,MSGNR=C'611',DATA
         ENDIF
*
SCAN_CHAR_RET EQU *
         MLWZTERM                 * Return back to caller
*
* Read next MAKEFILE record
*
READNEXT EQU   *
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer
         LA    R3,DCBMKF-DCB_DSECT(,R14) * Get addr of MAKEFILE DCB
         LA    R7,READNEXT_10     * Set R7 to statement EODAD routine
*                                 * branches to at EOF
         GET   (R3),G_MAKEFILE_REC * Get a record from makefile
READNEXT_10 EQU *
         IF (CLI,G_MKFEOF,NE,C'Y') THEN * Did we hit EOF? If not
            MVC   G_SCAN_CURRCOL,=F'0' * Reset current column to 0
            L     R4,G_SCAN_CURRLINE * Get the current line
            LA    R4,1(,R4)          * Advance 1 line count
            ST    R4,G_SCAN_CURRLINE * And put it back as current line
         ENDIF
*
         BR    R8                 * Return
*
         LTORG
***********************************************************************
* Section: LWZMAKE_ALLOC_STMT                                         *
* Purpose: Allocate a memory block for an internal representation of  *
*          a statement. This section also fills in the generic part   *
*          of the allocated statement block, including chaining into  *
*          the linked list of statements.                             *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_ALLOC_STMT MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_ALLOC_STMT'
*
         MVC   G_STMT_ALLOC_RETURN_PTR,=A(0) * Initialize return ptr
*
         MVC   G_STMT_SAVE_PTR,=A(0) * Initialize helper pointer
*
         LA    R7,G_STMT_LIST_PTR    * Get start of statement linked
*                                    * list
FIND_STMT_SLOT EQU *
         CLC   0(4,R7),=A(0)         * Have we reached an empty ptr?
         IF (NZ) THEN                * If not, go down the chain
            L     R6,0(,R7)          * Get the actual pointer
            ST    R6,G_STMT_SAVE_PTR * Save for back chain
            LA    R7,STMT_NEXT_PTR-STMT_DSECT(,R6) * Get next stmt ptr
            B     FIND_STMT_SLOT     * and loop around to check it
         ENDIF
*
*        Allocate a statement block
         L     R4,G_STMT_ALLOC_LEN  * Get length to allocate
         STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
         ST    R1,0(,R7)            * Save it at forward chain address
         ST    R1,G_STMT_ALLOC_RETURN_PTR * Also save it as return ptr
*
*        Write a trace record for allocated block
         ST    R1,G_DEC8             * Put in var with at least 5 bytes
         UNPK  G_ZONED8(9),G_DEC8(5) * Turn into almost hex
         TR    G_ZONED8,STMT_HEXTAB  * Turn into hex
         MVC   G_HELPER_DATA(8),G_ZONED8 * Copy 8 hex chars to helper
         LA    R2,G_HELPER_DATA      * Get address of helper data
         ST    R2,G_LWZMTRC_DATA_PTR * put in in trace record data ptr
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace record data length 8
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'640',DATA
*
*        Initialize block
         L     R2,G_STMT_ALLOC_RETURN_PTR * Point R2 to memory block
         L     R3,G_STMT_ALLOC_LEN  * Get length of block
         XR    R0,R0                * Clear R0
         XR    R1,R1                *   and R1
         MVCL  R2,R0                * Zero out memory block
*
*        Fill in generic stuff
         L     R1,G_STMT_ALLOC_RETURN_PTR
*        Fill in block length and statement type
         MVC   STMT_LEN-STMT_DSECT(4,R1),G_STMT_ALLOC_LEN
         MVC   STMT_TYPE-STMT_DSECT(1,R1),G_STMT_ALLOC_TYPE
*        Fill in switch for statement in recipe
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN
            MVI   STMT_IN_RECIPE-STMT_DSECT(R1),C'Y'
         ELSE
            MVI   STMT_IN_RECIPE-STMT_DSECT(R1),C'N'
         ENDIF
*        Fill in back chain statement pointer
         MVC   STMT_PREV_PTR-STMT_DSECT(4,R1),G_STMT_SAVE_PTR
*
ALLOC_STMT_RET EQU *
         MLWZTERM                 * Return back to caller
*
         LTORG
*
* Translate table for hex conversion
STMT_HEXTAB EQU   *-C'0'
            DC    C'0123456789ABCDEF'
***********************************************************************
* Section: LWZMAKE_STORE_VAR                                          *
* Purpose: Assign a value to a variable. The binary search tree is    *
*          searched, if the variable is found the new value is        *
*          assigned. If not found, a new variable memory block is     *
*          allocated, added to the binary search tree and then the    *
*          value is assigned.                                         *
*          R7 should point to a statement block for an assignment     *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_STORE_VAR MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_STORE_VAR'
*
         USING STMT_A_DSECT,R7    * Use R7 for addressing statement
         USING VAR_DSECT,R6       * Use R6 for addressing variable
*
         LT    R6,G_FIRST_VAR_PTR * Get the start of the search tree
         IF (Z) THEN              * If this is the first var
            BAL   R8,ALLOC_VAR    * Perform allocate variable routine
            LR    R6,R1           * Copy returned pointer to R6
            ST    R6,G_FIRST_VAR_PTR * And set it as start of tree
         ELSE                     * Else, search tree not empty
TEST_VARS   EQU   *               * Test this var for matching name
            XR    R3,R3           * Clear R3
            LH    R3,STMT_A_DESTLEN * and get length of stmt var name
            CH    R3,VARLEN       * Compare it to current var name len
            IF (H) THEN           * Compare length of shortest of the 2
               LH    R3,VARLEN    * If current var name len is less
            ENDIF                 * then use that
            BCTR  R3,R0           * Subtract 1 for EX
            LA    R2,STMT_A_DEST  * Point R2 to statement var name
            LA    R4,VARNAME      * Point R4 to current var name
            B     *+10            * Skip CLC constant for EX
            CLC   0(1,R2),0(R4)   * CLC constant for EX
            EX    R3,*-6          * EX previous CLC stmt with R3
            IF (L) THEN           * If statement var name is lower
               CLC   VARLOW,=A(0) * Check if current var's low ptr set
               IF (EQ) THEN       * If not
                  BAL   R8,ALLOC_VAR * Perform allocate var routine
                  ST    R1,VARLOW * Store returned pointer as low ptr
                  LR    R6,R1     * And set it as the current var ptr
                  B     FILL_VAR  * Skip to filling variable value
               ELSE               * Else, current var's low ptr is set
                  L     R6,VARLOW * Replace current var by low var
                  B     TEST_VARS * Loop around to test that one
               ENDIF
            ELSE                  * Else, statement var name is >=
               IF (EQ) THEN       * If the compared part is equal
                  CLC   STMT_A_DESTLEN,VARLEN * Check if they're also
                  IF (EQ) THEN    * of equal length
                     B     FILL_VAR * because then no alloc needed,
                  ENDIF           * just replacing the value
               ENDIF
*              If we end up here, it's not a match
               CLC   VARHIGH,=A(0) * Check if curr var's high ptr set
               IF (EQ) THEN       * If not
                  BAL   R8,ALLOC_VAR * Perform allocate var routine
                  ST    R1,VARHIGH * Store returned pointer as high ptr
                  LR    R6,R1     * And set it as the current var ptr
                  B     FILL_VAR  * Skip to filling variable value
               ELSE               * Else, current var's high ptr is set
                  L     R6,VARHIGH * Replace current var by high var
                  B     TEST_VARS * Loop around to test that one
               ENDIF
            ENDIF
         ENDIF
*
* Set a value, either in a new variable just allocated, or replacing a
* value of an existing variable
FILL_VAR EQU   *
         CLC   VALLEN,=AL2(0)     * Check if there's an existing value
         IF (NE) THEN             * If so...
            XR    R2,R2           * Clear R2
            LH    R2,VALLEN       * and put old value length in
            L     R3,VALPTR       * Get old value pointer
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
         ENDIF
         XR    R2,R2              * Clear R2
         LH    R2,STMT_A_SRCLEN   * Get new value length
         STH   R2,VALLEN          * Put it in variable block
         STORAGE OBTAIN,LENGTH=(R2) * Allocate value storage
         ST    R1,VALPTR          * Put new memory ptr in var block
         LR    R0,R1              * Copy new value ptr to R0
         LR    R1,R2              * Copy new value length to R1
         LR    R3,R1              * Make sure no cropping/filling
         LA    R2,STMT_A_SRC      * Point R2 to new value
         MVCL  R0,R2              * Copy new value to variable value
*
*        Write a report line with variable assignment
         MVC   G_LWZMRPT_LINE,=CL133' .....................'
         LA    R2,G_LWZMRPT_LINE+1
         LA    R3,21
         LA    R4,VARNAME
         LR    R5,R3
         CH    R5,VARLEN
         IF (H) THEN
            LH    R5,VARLEN
         ENDIF
         BCTR  R5,R0
         B     *+10
         MVC   0(1,R2),0(R4)
         EX    R5,*-6
         LA    R2,G_LWZMRPT_LINE+23
         LA    R3,110
         L     R4,VALPTR
         LR    R5,R3
         CH    R5,VALLEN
         IF (H) THEN
            LH    R5,VALLEN
         ENDIF
         LTR   R5,R5
         BZ    STORE_VAR_WRITE_RPT
         BCTR  R5,R0
         B     *+10
         MVC   0(1,R2),0(R4)
         EX    R5,*-6
STORE_VAR_WRITE_RPT EQU *
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         DROP  R6
*
STORE_VAR_RET EQU *
         MLWZTERM                 * Return back to caller
*
* Allocate a variable block. Value for variable is allocated separately
* The using for R7 in previous code is left intact to keep addressing
* of the assignment statement possible.
*
ALLOC_VAR EQU  *
         GETMAIN RU,LV=VAR_DSECT_LEN * Allocate memory for var block
*
         LR    R5,R1              * Copy allocated memory ptr to R5
         USING VAR_DSECT,R5       * Addressing of new variable block
*
         MVC   VARLEN,STMT_A_DESTLEN * Copy variable name length
         MVC   VARNAME,STMT_A_DEST * Copy variable name (both 72 long)
*        Set the rest of the variable block to zero's
         MVI   VALLEN,X'00'
         MVC   VALLEN+1(VAR_DSECT_LEN-L'VARLEN-L'VARNAME-1),VALLEN
*
*        Write a trace record for allocated block
         ST    R5,G_DEC8             * Put in var with at least 5 bytes
         UNPK  G_ZONED8(9),G_DEC8(5) * Turn into almost hex
         TR    G_ZONED8,VAR_HEXTAB   * Turn into hex
         MVC   G_HELPER_DATA(8),G_ZONED8 * Copy 8 hex chars to helper
         LA    R2,G_HELPER_DATA      * Get address of helper data
         ST    R2,G_LWZMTRC_DATA_PTR * put in in trace record data ptr
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace record data length 8
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'642',DATA
*
         LR    R1,R5              * Put ptr of allocated block in R1
*
         DROP  R5
*
         BR    R8                 * Return
*
         DROP  R7
*
         LTORG
*
* Translate table for hex conversion
VAR_HEXTAB EQU   *-C'0'
           DC    C'0123456789ABCDEF'
*
***********************************************************************
* Section: LWZMAKE_STORE_TGT                                          *
* Purpose: Store a target in binary search tree. The binary search    *
*          tree is searched, but an existing entry won't be replaced, *
*          the tree can contain duplicate entries. At the correct     *
*          place a new allocated memory block is inserted and filled  *
*          with the target data.                                      *
*          R7 should point to a rule statement block                  *
*          R9 should point to global data.                            *
*          Token 1 contains a single space delimited target name      *
***********************************************************************
LWZMAKE_STORE_TGT MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_STORE_TGT'
*
         USING TARGET_DSECT,R6    * Use R6 for addressing variable
*
         LT    R6,G_FIRST_TGT_PTR * Get the start of the search tree
         IF (Z) THEN              * If this is the first target
            BAL   R8,ALLOC_TGT    * Perform allocate target routine
            ST    R1,G_FIRST_TGT_PTR * Store return ptr start of tree
            B     STORE_TGT_RET   * Skip rest of store target
         ELSE                     * Else, search tree not empty
TEST_TGTS   EQU   *               * Test this target for matching name
            L     R3,G_SCAN_TOKEN_LEN * Get new target name length
            CH    R3,TGTNAMELEN   * Compare it to current tgt name len
            IF (H) THEN           * Compare length of shortest of the 2
               LH    R3,TGTNAMELEN * If current tgt name len is less
            ENDIF                 * then use that
            BCTR  R3,R0           * Subtract 1 for EX
            L     R2,G_SCAN_TOKENA * Point R2 to new target name
            LA    R4,TGTNAME      * Point R4 to current target name
            B     *+10            * Skip CLC constant for EX
            CLC   0(1,R2),0(R4)   * CLC constant for EX
            EX    R3,*-6          * EX previous CLC stmt with R3
            IF (L) THEN           * If new target name is lower
               CLC   TGTLOW,=A(0) * Check if current tgt's low ptr set
               IF (EQ) THEN       * If not
                  BAL   R8,ALLOC_TGT * Perform allocate tgt routine
                  ST    R1,TGTLOW * Store returned poitner as low ptr
                  B     STORE_TGT_RET * Skip rest of store target
               ELSE               * Else, current tgt's low ptr is set
                  L     R6,TGTLOW * Replace current tgt by low tgt
                  B     TEST_TGTS * Loop around to test that one
               ENDIF
            ELSE                  * Else, new target name is >=
               CLC   TGTHIGH,=A(0) * Check if curr tgt's high ptr set
               IF (EQ) THEN       * If not
                  BAL   R8,ALLOC_TGT * Perform allocate tgt routine
                  ST    R1,TGTHIGH * Store returned pointer as high ptr
                  B     STORE_TGT_RET * Skip rest of store target
               ELSE               * Else, current tgt's high ptr is set
                  L     R6,TGTHIGH * Replace current tgt by high tgt
                  B     TEST_TGTS * Loop around to test that one
               ENDIF
            ENDIF
         ENDIF
*
         DROP  R6
*
STORE_TGT_RET EQU *
         MLWZTERM                 * Return back to caller
*
* Allocate a target block
* R7 is left intact pointing to the rule statement where this target
* originated
*
ALLOC_TGT EQU  *
         L     R3,=A(TARGET_DSECT_LEN) * Get size of fixed part of tgt
         A     R3,G_SCAN_TOKEN_LEN  * Add target name length
         A     R3,=A(8)           * Add length for optional member name
*
         STORAGE OBTAIN,LENGTH=(R3) *Allocate memory for tgt block
*
         USING TARGET_DSECT,R1    * Addressing of new target block
*
*        Clear memory block
         LR    R2,R1              * Copy ptr to target block to R2
         XR    R4,R4              * Clear R4
         XR    R5,R5              *   and R5
         MVCL  R2,R4              * Zero out memory
*
*        Fill in new target block
         L     R3,=A(TARGET_DSECT_LEN) * Get size of fixed part of tgt
         A     R3,G_SCAN_TOKEN_LEN  * Add target name length
         A     R3,=A(8)           * Add length for optional member name
         ST    R3,TGTLEN          * Store total block length
         L     R2,G_SCAN_TOKEN_LEN * Get length of target name
         STH   R2,TGTNAMELEN      * Store target name length in block
         ST    R7,TGTSTMT         * Store target stmt ptr in block
         LA    R2,TGTNAME         * Point R2 to target name in block
         L     R3,G_SCAN_TOKEN_LEN * Get target name length
         L     R4,G_SCAN_TOKENA   * Point R4 to target name in token 1
         LR    R5,R3              * Make sure no cropping/filling
         MVCL  R2,R4              * Copy target name to block
*
         DROP  R1
*
         BR    R8                 * Return
*
***********************************************************************
* Section: LWZMAKE_STORE_PNY                                          *
* Purpose: Store a PHONY in the binary search tree. Duplicates are    *
*          simply ignored. Every new phony is allocated a new phony   *
*          memory block, which is added to the binary search tree.    *
*          R7 should point to a statement block for a PHONY           *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_STORE_PNY MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_STORE_PNY'
*
         USING STMT_P_DSECT,R7    * Use R7 for addressing PHONY stmt
         USING PHONY_DSECT,R6     * Use R6 for addressing PHONY
*
         LT    R6,G_FIRST_PNY_PTR * Get the start of the search tree
         IF (Z) THEN              * If this is the first PHONY
            BAL   R8,ALLOC_PNY    * Perform allocate PHONY routine
            LR    R6,R1           * Copy returned pointer to R6
            ST    R6,G_FIRST_PNY_PTR * And set it as start of tree
         ELSE                     * Else, search tree not empty
TEST_PNYS   EQU   *               * Test this PHONY for matching name
            XR    R3,R3           * Clear R3
            LH    R3,STMT_P_PNYLEN * and get length of stmt PNONY name
            CH    R3,PNYNAMELEN   * Compare it to current PHONY len
            IF (H) THEN           * Compare length of shortest of the 2
               LH    R3,PNYNAMELEN * If current PHONY name len is less
            ENDIF                 * then use that
            BCTR  R3,R0           * Subtract 1 for EX
            LA    R2,STMT_P_PNY   * Point R2 to statement PHONY name
            LA    R4,PNYNAME      * Point R4 to current PHONY name
            B     *+10            * Skip CLC constant for EX
            CLC   0(1,R2),0(R4)   * CLC constant for EX
            EX    R3,*-6          * EX previous CLC stmt with R3
            IF (L) THEN           * If statement PHONY name is lower
               CLC   PNYLOW,=A(0) * Check if current PHONY low ptr set
               IF (EQ) THEN       * If not
                  BAL   R8,ALLOC_PNY * Perform allocate PHONY routine
                  ST    R1,PNYLOW * Store returned pointer as low ptr
                  LR    R6,R1     * And set it as the current PHONY ptr
                  B     FILL_PNY  * Skip to writing report line
               ELSE               * Else, current PHONY low ptr is set
                  L     R6,PNYLOW * Replace current PHONY by low PHONY
                  B     TEST_PNYS * Loop around to test that one
               ENDIF
            ELSE                  * Else, statement PHONY name is >=
               IF (EQ) THEN       * If the compared part is equal
                  CLC   STMT_P_PNYLEN,PNYNAMELEN * Check if they're
                  IF (EQ) THEN    * also of equal length
                     B     STORE_PNY_RET * we're done
                  ENDIF
               ENDIF
*              If we end up here, it's not a match
               CLC   PNYHIGH,=A(0) * Check if curr PHONY high ptr set
               IF (EQ) THEN       * If not
                  BAL   R8,ALLOC_PNY * Perform allocate PHONY routine
                  ST    R1,PNYHIGH * Store returned pointer as high ptr
                  LR    R6,R1     * And set it as the current PHONY ptr
                  B     FILL_PNY  * Skip to writing report line
               ELSE               * Else, current PHONY high ptr is set
                  L     R6,PNYHIGH * Replace current PHONY by high
                  B     TEST_PNYS * Loop around to test that one
               ENDIF
            ENDIF
         ENDIF
*
* Fill a new PHONY name
FILL_PNY EQU   *
*
*        Write a report line with PHONY
         MVC   G_LWZMRPT_LINE,=CL133' Phony target ........'
         LA    R2,G_LWZMRPT_LINE+23
         LA    R3,110
         LA    R4,PNYNAME
         LR    R5,R3
         CH    R5,PNYNAMELEN
         IF (H) THEN
            LH    R5,PNYNAMELEN
         ENDIF
         BCTR  R5,R0
         B     *+10
         MVC   0(1,R2),0(R4)
         EX    R5,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         DROP  R6
*
STORE_PNY_RET EQU *
         MLWZTERM                 * Return back to caller
*
* Allocate a PHONY block.
* The using for R7 in previous code is left intact to keep addressing
* of the PHONY statement possible.
*
ALLOC_PNY EQU  *
         L     R3,=A(PHONY_DSECT_LEN) * Get size of fixed part of PHONY
         AH    R3,STMT_P_PNYLEN   * Add PHONY name length
*
         STORAGE OBTAIN,LENGTH=(R3) * Allocate memory for PHONY block
*
         USING PHONY_DSECT,R1     * Addressing of new PHONY block
*
*        Clear memory block
         LR    R2,R1              * Copy ptr to target block to R2
         XR    R4,R4              * Clear R4
         XR    R5,R5              *   and R5
         MVCL  R2,R4              * Zero out memory
*
*        Fill in new target block
         L     R3,=A(PHONY_DSECT_LEN) * Get size of fixed part of PHONY
         AH    R3,STMT_P_PNYLEN   * Add PHONY name length
         ST    R3,PNYLEN          * Store total block length
         XR    R3,R3              * Clear R2
         LH    R3,STMT_P_PNYLEN   * Get length of PHONY name
         STH   R3,PNYNAMELEN      * Store PHONY name length in block
         ST    R7,PNYSTMT         * Store PHONY stmt ptr in block
         LA    R2,PNYNAME         * Point R2 to PHONY name in block
         LA    R4,STMT_P_PNY      * Point R4 to PHONY name in stmt
         LR    R5,R3              * Make sure no cropping/filling
         MVCL  R2,R4              * Copy PHONY name to block
*
         DROP  R1
*
         BR    R8                 * Return
*
***********************************************************************
* Section: LWZMAKE_FINDVAR                                            *
* Purpose: Find a variable in variable binary search tree             *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_FINDVAR MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_FINDVAR'
*
         MVC   G_FOUND_VAR_PTR,=A(0) * Initialize return pointer
*
         USING VAR_DSECT,R6       * Use R6 for addressing variable
*
         LT    R6,G_FIRST_VAR_PTR * Get start of search tree for vars
         IF (Z) THEN              * If no vars in tree
            B     FINDVAR_RET     * Skip rest of find var
         ENDIF
TEST_VARF   EQU   *
         XR    R3,R3              * Clear R3
         LH    R3,G_SRCH_VAR_LEN  * Get var to find's name length
         CH    R3,VARLEN          * Compare it to current var name len
         IF (H) THEN              * Compare length of shortest of the 2
            LH    R3,VARLEN       * If current var name len is less
         ENDIF                    * then use that
         BCTR  R3,R0              * Subtract 1 for EX
         LA    R2,G_SRCH_VAR      * Point R2 to var to find's name
         LA    R4,VARNAME         * Point R4 to current var name
         B     *+10               * Skip CLC constant for EX
         CLC   0(1,R2),0(R4)      * CLC constant for EX
         EX    R3,*-6             * EX previous CLC stmt with R3
         IF (L) THEN              * If find var name is lower
            CLC   VARLOW,=A(0)    * Check if current var's low ptr set
            IF (EQ) THEN          * If not
               B     FINDVAR_RET  * Var not found, skip rest of findvar
            ELSE                  * Else, current var's low ptr set
               L     R6,VARLOW    * Replace current var by low var
               B     TEST_VARF    * Loop around to test that one
            ENDIF
         ELSE                     * Else, find var name is >=
            IF (EQ) THEN          * If the compared part is equal
               CLC   G_SRCH_VAR_LEN,VARLEN * Check if they're also of
               IF (EQ) THEN       * equal length
                  ST    R6,G_FOUND_VAR_PTR * Return this var's ptr
                  B     FINDVAR_RET * Skip rest of find var
               ENDIF
            ENDIF
*           If we end up here, it's not a match
            CLC   VARHIGH,=A(0)   * Check if curr var's high ptr set
            IF (EQ) THEN          * If not
               B     FINDVAR_RET  * Var not found, skip rest of findvar
            ELSE                  * Else, current var's high ptr set
               L     R6,VARHIGH   * Replace current var by high var
               B     TEST_VARF    * Loop around to test that one
            ENDIF
         ENDIF
*
         DROP  R6
*
FINDVAR_RET EQU *
         MLWZTERM                 * Return back to caller
*
         LTORG
***********************************************************************
* Section: LWZMAKE_FINDTGT                                            *
* Purpose: Find a target in target binary search tree                 *
*          The name of the target to find is in token 1.              *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_FINDTGT MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_FINDTGT'
*
         MVC   G_FOUND_TGT_PTR,=A(0) * Initialize return pointer
*
         USING TARGET_DSECT,R6    * Use R6 for addressing target
*
         LT    R6,G_FIRST_TGT_PTR * Get start of search tree for tgts
         IF (Z) THEN              * If no tgts in tree
            B     FINDTGT_RET     * Skip rest of find tgt
         ENDIF
TEST_TGTF EQU  *
         L     R3,G_SCAN_TOKEN_LEN * Get tgt to find's name length
         CH    R3,TGTNAMELEN      * Compare it to current tgt name len
         IF (H) THEN              * Compare length of shortest of the 2
            LH    R3,TGTNAMELEN   * If current tgt name len is less
         ENDIF                    * then use that
         BCTR  R3,R0              * Subtract 1 for EX
         L     R2,G_SCAN_TOKENA   * Point R2 to tgt to find's name
         LA    R4,TGTNAME         * Point R4 to current tgt name
         B     *+10               * Skip CLC constant for EX
         CLC   0(1,R2),0(R4)      * CLC constant for EX
         EX    R3,*-6             * EX previous CLC stmt with R3
         IF (L) THEN              * If find tgt name is lower
            CLC   TGTLOW,=A(0)    * Check if current tgt's low ptr set
            IF (EQ) THEN          * If not
               B     FINDTGT_RET  * Tgt not found, skip rest of findtgt
            ELSE                  * Else, current tgt's low ptr set
               L     R6,TGTLOW    * Replace current tgt by low tgt
               B     TEST_TGTF    * Loop around to test that one
            ENDIF
         ELSE                     * Else, find tgt name is >=
            IF (EQ) THEN          * If the compared part is equal
               CLC   G_SCAN_TOKEN_LEN+2(2),TGTNAMELEN * Check if also
               IF (EQ) THEN       * of equal length
                  ST    R6,G_FOUND_TGT_PTR * Return this tgt's ptr
                  B     FINDTGT_RET * Skip rest of find tgt
               ENDIF
            ENDIF
*           If we end up here, it's not a match
            CLC   TGTHIGH,=A(0)   * Check if curr tgt's high ptr set
            IF (EQ) THEN          * If not
               B     FINDTGT_RET  * Tgt not found, skip rest of findtgt
            ELSE                  * Else, current tgt's high ptr set
               L     R6,TGTHIGH   * Replace current tgt by high tgt
               B     TEST_TGTF    * Loop around to test that one
            ENDIF
         ENDIF
*
FINDTGT_RET EQU *
*        Write result of find var to report
         CLC   G_FOUND_TGT_PTR,=A(0) * Was a tgt found?
         IF (EQ) THEN                * Nope
            MVC   G_LWZMRPT_LINE,=CL133' ..................... No targeX
               t found'
         ELSE                        * Tgt was found
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target fX
               ound'
         ENDIF
         L     R15,G_LWZMAKE_RPTA * Get address of LWZMAKE_RPT section
         BASR  R14,R15            * Link to LWZMAKE_RPT section
*
         DROP  R6
*
         MLWZTERM                 * Return back to caller
*
         LTORG
*
***********************************************************************
* Section: LWZMAKE_FINDPNY                                            *
* Purpose: Find a PHONY in PHONY binary search tree                   *
*          The name of the PHONY to find is in token 1.               *
*          R9 should point to global data.                            *
***********************************************************************
LWZMAKE_FINDPNY MLWZSAVE
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_FINDPNY'
*
         MVC   G_FOUND_PNY_PTR,=A(0) * Initialize return pointer
*
         USING PHONY_DSECT,R6     * Use R6 for addressing PHONY
*
         LT    R6,G_FIRST_PNY_PTR * Get start of search tree for PNYs
         IF (Z) THEN              * If no PNYs in tree
            B     FINDPNY_RET     * Skip rest of find PHONY
         ENDIF
TEST_PNYF EQU  *
         L     R3,G_SCAN_TOKEN_LEN * Get PNY to find's name length
         CH    R3,PNYNAMELEN      * Compare it to current PNY name len
         IF (H) THEN              * Compare length of shortest of the 2
            LH    R3,PNYNAMELEN   * If current PNY name len is less
         ENDIF                    * then use that
         BCTR  R3,R0              * Subtract 1 for EX
         L     R2,G_SCAN_TOKENA   * Point R2 to PNY to find's name
         LA    R4,PNYNAME         * Point R4 to current PNY name
         B     *+10               * Skip CLC constant for EX
         CLC   0(1,R2),0(R4)      * CLC constant for EX
         EX    R3,*-6             * EX previous CLC stmt with R3
         IF (L) THEN              * If find PNY name is lower
            CLC   PNYLOW,=A(0)    * Check if current PNY's low ptr set
            IF (EQ) THEN          * If not
               B     FINDPNY_RET  * PNY not found, skip rest of findpny
            ELSE                  * Else, current PNY's low ptr set
               L     R6,PNYLOW    * Replace current PNY by low PNY
               B     TEST_PNYF    * Loop around to test that one
            ENDIF
         ELSE                     * Else, find PNY name is >=
            IF (EQ) THEN          * If the compared part is equal
               CLC   G_SCAN_TOKEN_LEN+2(2),PNYNAMELEN * Check if also
               IF (EQ) THEN       * of equal length
                  ST    R6,G_FOUND_PNY_PTR * Return this PNY's ptr
                  B     FINDPNY_RET * Skip rest of find PNY
               ENDIF
            ENDIF
*           If we end up here, it's not a match
            CLC   PNYHIGH,=A(0)   * Check if curr PNY's high ptr set
            IF (EQ) THEN          * If not
               B     FINDPNY_RET  * PNY not found, skip rest of findpny
            ELSE                  * Else, current PNY's high ptr set
               L     R6,PNYHIGH   * Replace current PNY by high PNY
               B     TEST_PNYF    * Loop around to test that one
            ENDIF
         ENDIF
*
FINDPNY_RET EQU *
*        Write result of find var to report
         CLC   G_FOUND_PNY_PTR,=A(0) * Was a PNY found?
         IF (EQ) THEN                * Nope
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target iX
               s a file'
         ELSE                        * Tgt was found
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target iX
               s PHONY'
         ENDIF
         L     R15,G_LWZMAKE_RPTA * Get address of LWZMAKE_RPT section
         BASR  R14,R15            * Link to LWZMAKE_RPT section
*
         DROP  R6
*
         MLWZTERM                 * Return back to caller
*
         LTORG
*
***********************************************************************
* Section: LWZMAKE_EXEC_TGT                                           *
* Purpose: Execute a target. This is a recursive section, in other    *
*          words, it calls itself.                                    *
*          First, the target last altered date is acquired. Then the  *
*          requisites are 'expanded', meaning any variables in that   *
*          string are resolved. Then, for each requisite this section *
*          is invoked. If, after all requisites have been processed,  *
*          any of the requisites had a laster altered date more       *
*          recent than the target's, that target should be built.     *
*          If so, the recipe following the rult statement for this    *
*          target is executed.                                        *
*          R9 should point to global data.                            *
***********************************************************************
*
LWZMAKE_EXEC_TGT DS    0F
         STM   R14,R12,12(R13)   * Save callers registers
         LR    R10,R15           * Set R10 to entry point
         LA    R11,4095(,R10)    * Setup R11 as second using
         LA    R11,1(,R11)       *   base register
         USING LWZMAKE_EXEC_TGT,R10,R11 * Establish addressing
         LR    R2,R1             * Save possible parameter ptr
         GETMAIN RU,LV=WORKAREA_EXEC_TGT_LEN
         ST    R13,4(R1)         * Backward chain callers SA
         ST    R1,8(R13)         * Forward chain my SA
         LR    R13,R1            * Point R13 to my SA
         USING WORKAREA_EXEC_TGT,R13 * Establish addressing
         USING GLOBAL_DATA_DSECT,R9
         LR    R1,R2             * Restore parameter list ptr in R1
         XR    R15,R15
         ST    R15,RETCODE_EXEC_TGT
*
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_EXEC_TGT'
*
*        Reset scanning variables
         MVC   G_SCAN_CURRCOL,=F'0' * Not really used, but it shouldn't
*                                 * be whatever value (like 80) left
*                                 * over from scanning makefile
         MVI   G_MKFEOF,C'N'      * Reset EOF switch
*
*        This section is invoked with a parameter of EXEC_TGT_PAR. It
*        holds a pointer to a TARGET_DSECT, which in turn points to a
*        STMT_R_DSECT.
         L     R1,0(,R1)          * Get pointer to first parameter
         ST    R1,EXEC_TGT_PARA   * Save it
         L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1) * Get TARGET_DSECT ptr
         USING TARGET_DSECT,R6    * Address it with R6
         L     R7,TGTSTMT         * Get originating rule statement ptr
         USING STMT_R_DSECT,R7    * Address it with R7
*
*
         MVC   G_LWZMRPT_LINE,=CL133' Processing target ...'
         LA    R2,G_LWZMRPT_LINE+23
         LA    R3,TGTNAME
         L     R4,=F'110'
         CH    R4,TGTNAMELEN
         IF (H) THEN
            LH    R4,TGTNAMELEN
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         L     R2,G_SCAN_TOKENA
         LA    R3,TGTNAME
         XR    R4,R4
         LH    R4,TGTNAMELEN
         ST    R4,G_SCAN_TOKEN_LEN
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         L     R15,LWZMAKE_FINDPNYA_EXEC
         BASR  R14,R15
*
         CLC   G_FOUND_PNY_PTR,=A(0)
         IF (NE) THEN
            XC    TARGET_ALTER_DATE,TARGET_ALTER_DATE
            B     EXEC_TGT_PREREQ
         ENDIF
*
         L     R2,G_SCAN_TOKENA
         LA    R3,TGTNAME
         XR    R4,R4
         LH    R4,TGTNAMELEN
         ST    R4,G_SCAN_TOKEN_LEN
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         L     R15,LWZMAKE_GET_DATEA_EXEC
         BASR  R14,R15
*
         CLC   G_RETCODE,=F'0'
         BNE   EXEC_TGT_RET
*
         LT    R3,G_MVSDS_MEMBER_LEN
         IF (NZ) THEN
            STH   R3,TGTNAMEMEMLEN
            LA    R2,TGTNAME
            AH    R2,TGTNAMELEN
            L     R4,G_MVSDS_MEMBER_PTR
            BCTR  R3,R0
            B     *+10
            MVC   0(1,R2),0(R4)
            EX    R3,*-6
         ENDIF
*
         MVC   TARGET_ALTER_DATE,G_SAVE_ALTER_DATE
         CLC   TARGET_ALTER_DATE,=16X'FF'
         IF (EQ) THEN
            MLWZMRPT RPTLINE=CL133' ..................... Target has noX
                last altered date, build required'
         ENDIF
*
EXEC_TGT_PREREQ EQU *
         CLC   STMT_R_REQLEN,=H'0'
         BE    EXEC_TGT_PREREQ_DONE
*
         XR    R2,R2
         XR    R3,R3
         IC    R3,G_SCAN_INPUT_STACK_IDX
         C     R3,=F'20'
         IF (NL) THEN
            MLWZMRPT RPTLINE=CL133'0Internal error, state stack overfloX
               w'
            MVC   G_RETCODE,=F'12'
            B     EXEC_TGT_RET
         ENDIF
         LA    R3,1(,R3)
         STC   R3,G_SCAN_INPUT_STACK_IDX
         BCTR  R3,R0
         M     R2,=F'12'
         LA    R2,G_SCAN_INPUT_STACK
         AR    R2,R3
         USING INPUT_DSECT,R2
         MVI   INPUTTYPE,X'01'
         MVC   INPUTLEN,STMT_R_REQLEN
         LA    R4,STMT_R_TGT
         AH    R4,STMT_R_TGTLEN
         ST    R4,INPUTPTR
         MVC   INPUTPOS,=H'0'
         DROP  R2
*
         MVI   G_SCAN_STATE,SCAN_STATE_IN_EXPAND
         MVC   G_SCAN_TOKEN2_LEN,=F'0'
*
EXEC_TGT_PREREQ_NEXT_TOKEN EQU *
         L     R15,LWZMAKE_SCAN_TOKENA_EXEC
         BASR  R14,R15
*
         CLC   G_RETCODE,=F'0'
         BNE   EXEC_TGT_RET
*
         CLI   G_MKFEOF,C'Y'
         BE    EXEC_TGT_PREREQ_EXPANDED
*
         IC    R14,G_SCAN_STATE
         N     R14,=X'0000007F'
         C     R14,=A(SCAN_STATE_IN_VARIABLE)
         IF (EQ) THEN
            MVI   G_SCAN_APPEND_TO,X'00'
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1'
            L     R15,LWZMAKE_SCAN_VARA_EXEC
            BASR  R14,R15
*
            CLC   G_RETCODE,=F'0'
            BNE   EXEC_TGT_RET
*
            B     EXEC_TGT_PREREQ_NEXT_TOKEN
         ENDIF
*
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_PERCENT) THEN
            L     R2,G_SCAN_TOKENA
            LA    R4,TGTNAME
            AH    R4,TGTNAMELEN
            XR    R3,R3
            LH    R3,TGTNAMEMEMLEN
            ST    R3,G_SCAN_TOKEN_LEN
            BCTR  R3,R0
            B     *+10
            MVC   0(1,R2),0(R4)
            EX    R3,*-6
         ENDIF
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_ACRO) THEN
            L     R2,G_SCAN_TOKENA
            LA    R4,TGTNAME
            XR    R3,R3
            LH    R3,TGTNAMELEN
            ST    R3,G_SCAN_TOKEN_LEN
            BCTR  R3,R0
            B     *+10
            MVC   0(1,R2),0(R4)
            EX    R3,*-6
         ENDIF
*
*        Append token 1 to token 2
         MVI   G_SCAN_APPEND_TO,X'01'
         L     R15,LWZMAKE_APPEND_TOKENA_EXEC * Get addr APPEND_TOKEN
         BASR  R14,R15            * Link to APPEND_TOKEN section
*
         B     EXEC_TGT_PREREQ_NEXT_TOKEN
*
EXEC_TGT_PREREQ_EXPANDED EQU *
         MVC   G_LWZMRPT_LINE,=CL133' Prerequisites........'
         LA    R2,G_LWZMRPT_LINE+23
         L     R3,G_SCAN_TOKEN2_LEN
         L     R4,G_SCAN_TOKEN2A
         C     R3,=A(110)
         IF (H) THEN
            L     R3,=A(110)
         ENDIF
         BCTR  R3,R0
         B     *+10
         MVC   0(1,R2),0(R4)
         EX    R3,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         MVC   EXEC_WORD_SPLIT_PTR,G_SCAN_TOKEN2A
         MVC   EXEC_WORD_SPLIT_LEN,G_SCAN_TOKEN2_LEN
EXEC_TGT_SCAN_NEXT_PREREQ EQU *
         L     R2,EXEC_WORD_SPLIT_PTR
         L     R3,EXEC_WORD_SPLIT_LEN
         L     R4,G_SCAN_TOKENA
         XR    R5,R5
         ST    R5,G_SCAN_TOKEN_LEN
EXEC_TGT_PREREQ_BLANK EQU *
         IF (CLI,0(R2),EQ,C' ') THEN
            LA    R2,1(,R2)
            BCT   R3,EXEC_TGT_PREREQ_BLANK
            B     EXEC_TGT_RET
         ENDIF
EXEC_TGT_PREREQ_NONBLANK EQU *
         MVC   0(1,R4),0(R2)
         LA    R4,1(,R4)
         LA    R5,1(,R5)
         BCTR  R3,R0
         C     R3,=F'0'
         IF (H) THEN
            LA    R2,1(,R2)
            CLI    0(R2),C' '     * Current pos a space?
            BNE   EXEC_TGT_PREREQ_NONBLANK
         ENDIF
         ST    R5,G_SCAN_TOKEN_LEN
         ST    R2,EXEC_WORD_SPLIT_PTR
         ST    R3,EXEC_WORD_SPLIT_LEN
*
         MVC   G_LWZMRPT_LINE,=CL133' Next prereq for tgt .'
         LA    R2,G_LWZMRPT_LINE+23
         LA    R3,TGTNAME
         L     R4,=F'110'
         CH    R4,TGTNAMELEN
         IF (H) THEN
            LH    R4,TGTNAMELEN
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         MVC   G_LWZMRPT_LINE,=CL133' Checking prereq .....'
         LA    R2,G_LWZMRPT_LINE+23
         L     R3,G_SCAN_TOKENA
         L     R4,=F'110'
         C     R4,G_SCAN_TOKEN_LEN
         IF (H) THEN
            L     R4,G_SCAN_TOKEN_LEN
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         L     R15,LWZMAKE_FINDTGTA_EXEC
         BASR  R14,R15
*
         CLC   G_RETCODE,=F'0'
         BNE   EXEC_TGT_RET
*
         CLC   G_FOUND_TGT_PTR,=A(0)
         IF (NE) THEN
            MVC   EXEC_SAVE_SCAN_TOKENA,G_SCAN_TOKENA
            MVC   EXEC_SAVE_SCAN_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN
            MVC   EXEC_SAVE_SCAN_TOKEN_LEN,G_SCAN_TOKEN_LEN
            L     R4,G_SCAN_TOKEN_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKENA
            MVC   G_SCAN_TOKEN_LEN,=A(0)
*
            MVC   EXEC_SAVE_SCAN_TOKEN2A,G_SCAN_TOKEN2A
            MVC   EXEC_SAVE_SCAN_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN
            MVC   EXEC_SAVE_SCAN_TOKEN2_LEN,G_SCAN_TOKEN2_LEN
            L     R4,G_SCAN_TOKEN2_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKEN2A
            MVC   G_SCAN_TOKEN2_LEN,=A(0)
*
            LA    R1,EXEC_NEXTTGT_PAR
            MVC   EXEC_TGT_PTR-EXEC_TGT_PAR(4,R1),G_FOUND_TGT_PTR
            ST    R1,EXEC_NEXTTGT_PARA
            LA    R1,EXEC_NEXTTGT_PARA
            L     R15,LWZMAKE_EXEC_TGTA_EXEC
            BASR  R14,R15
*
            CLC   G_RETCODE,=F'0'
            BNE   EXEC_TGT_RET
*
            L     R2,G_SCAN_TOKEN_MAXLEN
            L     R3,G_SCAN_TOKENA
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
            MVC   G_SCAN_TOKENA,EXEC_SAVE_SCAN_TOKENA
            MVC   G_SCAN_TOKEN_MAXLEN,EXEC_SAVE_SCAN_TOKEN_MAXLEN
            MVC   G_SCAN_TOKEN_LEN,EXEC_SAVE_SCAN_TOKEN_LEN
*
            L     R2,G_SCAN_TOKEN2_MAXLEN
            L     R3,G_SCAN_TOKEN2A
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
            MVC   G_SCAN_TOKEN2A,EXEC_SAVE_SCAN_TOKEN2A
            MVC   G_SCAN_TOKEN2_MAXLEN,EXEC_SAVE_SCAN_TOKEN2_MAXLEN
            MVC   G_SCAN_TOKEN2_LEN,EXEC_SAVE_SCAN_TOKEN2_LEN
*
            MVC   G_LWZMRPT_LINE,=CL133' Continuing target ...'
            LA    R2,G_LWZMRPT_LINE+23
            LA    R3,TGTNAME
            L     R4,=F'110'
            CH    R4,TGTNAMELEN
            IF (H) THEN
               LH    R4,TGTNAMELEN
            ENDIF
            BCTR  R4,R0
            B     *+10
            MVC   0(1,R2),0(R3)
            EX    R4,*-6
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
*
            MVC   G_LWZMRPT_LINE,=CL133' Continuing prereq ...'
            LA    R2,G_LWZMRPT_LINE+23
            L     R3,G_SCAN_TOKENA
            L     R4,=F'110'
            C     R4,G_SCAN_TOKEN_LEN
            IF (H) THEN
               L     R4,G_SCAN_TOKEN_LEN
            ENDIF
            BCTR  R4,R0
            B     *+10
            MVC   0(1,R2),0(R3)
            EX    R4,*-6
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
         ENDIF
*
         L     R15,LWZMAKE_FINDPNYA_EXEC
         BASR  R14,R15
*
         CLC   G_FOUND_PNY_PTR,=A(0)
         BNE   EXEC_TGT_PREREQ_CHECK_LOOP
*
         L     R15,LWZMAKE_GET_DATEA_EXEC
         BASR  R14,R15
*
         CLC   G_RETCODE,=F'0'
         BNE   EXEC_TGT_RET
*
         IF (CLI,G_DSFOUND,NE,C'Y') THEN
            MLWZMRPT RPTLINE=CL133' ..................... Prerequisite X
               not found, build stopped'
            MVC   G_RETCODE,=F'8'
            B     EXEC_TGT_RET
         ENDIF
*
         CLC   TARGET_ALTER_DATE,=16X'00'
         BE    EXEC_TGT_PREREQ_CHECK_LOOP
*
         CLC   TARGET_ALTER_DATE,=16X'FF'
         IF (NE) THEN
            CLC   TARGET_ALTER_DATE,G_SAVE_ALTER_DATE
            IF (LT) THEN
               MLWZMRPT RPTLINE=CL133' ..................... Target is X
               older than prereq, build required'
               MVC   TARGET_ALTER_DATE,=16X'FF'
            ENDIF
         ENDIF
*
EXEC_TGT_PREREQ_CHECK_LOOP EQU *
         L     R3,EXEC_WORD_SPLIT_LEN
         C     R3,=F'0'
         BH    EXEC_TGT_SCAN_NEXT_PREREQ
*
EXEC_TGT_PREREQ_DONE EQU *
         CLC   TARGET_ALTER_DATE,=16X'FF'
         IF (NE) THEN
            CLC   TARGET_ALTER_DATE,=16X'00'
         ENDIF
         IF (EQ) THEN
            BAL   R8,EXEC_TGT_BUILD
         ELSE
            MLWZMRPT RPTLINE=CL133' ..................... No need to buX
               ild target'
            B     EXEC_TGT_RET
         ENDIF
*
         DROP  R6
         DROP  R7
*
EXEC_TGT_RET EQU *
         L     R2,RETCODE_EXEC_TGT * Save return value
         L     R3,4(,R13)        * Restore address of callers SA
         FREEMAIN RU,LV=WORKAREA_EXEC_TGT_LEN,A=(R13)
         LR    R15,R2            * Restore return value
         LR    R13,R3            * Address of callers SA
         L     R14,12(R13)       * Restore callers R14
         LM    R0,R12,20(R13)    * Restore callers registers 0-12
         BR    R14               * Return
*
* Execute the recipe
*
EXEC_TGT_BUILD EQU *
         L     R1,EXEC_TGT_PARA
         L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)
         USING TARGET_DSECT,R6
         L     R7,TGTSTMT
         USING STMT_DSECT,R7
*
         MVC   G_LWZMRPT_LINE,=CL133' Building target .....'
         LA    R2,G_LWZMRPT_LINE+23
         LA    R3,TGTNAME
         L     R4,=F'110'
         CH    R4,TGTNAMELEN
         IF (H) THEN
            LH    R4,TGTNAMELEN
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
         DROP  R6
*
         LT    R7,STMT_NEXT_PTR
         BZ    EXEC_TGT_BUILD_NO_RECIPE
*
         CLI   STMT_IN_RECIPE,C'Y'
         BNE   EXEC_TGT_BUILD_NO_RECIPE
*
NEXT_RECIPE_STMT EQU *
         IF (CLI,STMT_TYPE,EQ,STMT_TYPE_CALL) THEN
            DROP  R7
            USING STMT_C_DSECT,R7
*
            MVC   EXEC_SAVE_SCAN_TOKENA,G_SCAN_TOKENA
            MVC   EXEC_SAVE_SCAN_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN
            MVC   EXEC_SAVE_SCAN_TOKEN_LEN,G_SCAN_TOKEN_LEN
            L     R4,G_SCAN_TOKEN_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKENA
            MVC   G_SCAN_TOKEN_LEN,=A(0)
*
            MVC   EXEC_SAVE_SCAN_TOKEN2A,G_SCAN_TOKEN2A
            MVC   EXEC_SAVE_SCAN_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN
            MVC   EXEC_SAVE_SCAN_TOKEN2_LEN,G_SCAN_TOKEN2_LEN
            L     R4,G_SCAN_TOKEN2_MAXLEN
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block
            ST    R1,G_SCAN_TOKEN2A
            MVC   G_SCAN_TOKEN2_LEN,=A(0)
*
            XR    R2,R2
            XR    R3,R3
            IC    R3,G_SCAN_INPUT_STACK_IDX
            C     R3,=F'20'
            IF (NL) THEN
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX
               flow'
               MVC   G_RETCODE,=F'12'
               B     EXEC_CALL_END
            ENDIF
            LA    R3,1(,R3)
            STC   R3,G_SCAN_INPUT_STACK_IDX
            BCTR  R3,R0
            M     R2,=F'12'
            LA    R2,G_SCAN_INPUT_STACK
            AR    R2,R3
            USING INPUT_DSECT,R2
            MVI   INPUTTYPE,X'01'
            MVC   INPUTLEN,STMT_C_PARMLEN
            LA    R6,STMT_C_EXEC
            AH    R6,STMT_C_EXECLEN
            ST    R6,INPUTPTR
            MVC   INPUTPOS,=H'0'
            MVI   G_MKFEOF,C'N'
*
            DROP  R2
*
            MVI   G_SCAN_STATE,SCAN_STATE_IN_EXPAND
            MVC   G_SCAN_TOKEN2_LEN,=F'0'
*
EXEC_TGT_CALL_NEXT_TOKEN EQU *
            L     R15,LWZMAKE_SCAN_TOKENA_EXEC
            BASR  R14,R15
*
            CLC   G_RETCODE,=F'0'
            BNE   EXEC_CALL_END
*
            CLI   G_MKFEOF,C'Y'
            BE    EXEC_TGT_CALL_EXPANDED
*
            IC    R14,G_SCAN_STATE
            N     R14,=X'0000007F'
            C     R14,=A(SCAN_STATE_IN_VARIABLE)
            IF (EQ) THEN
               MVI   G_SCAN_APPEND_TO,X'00'
               MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1'
               L     R15,LWZMAKE_SCAN_VARA_EXEC
               BASR  R14,R15
*
               CLC   G_RETCODE,=F'0'
               BNE   EXEC_CALL_END
*
               B     EXEC_TGT_CALL_NEXT_TOKEN
            ENDIF
*
            L     R1,G_SCAN_TOKEN_LEN
            C     R1,=F'2'
            IF (EQ) THEN
               L     R2,G_SCAN_TOKENA
               CLC   0(2,R2),=C'$@'
               IF (EQ) THEN
                  L     R1,EXEC_TGT_PARA
                  L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)
                  LA    R3,TGTNAME-TARGET_DSECT(,R6)
                  XR    R4,R4
                  LH    R4,TGTNAMELEN-TARGET_DSECT(,R6)
                  ST    R4,G_SCAN_TOKEN_LEN
                  BCTR  R4,R0
                  B     *+10
                  MVC   0(1,R2),0(R3)
                  EX    R4,*-6
               ENDIF
               CLC   0(2,R2),=C'$%'
               IF (EQ) THEN
                  L     R1,EXEC_TGT_PARA
                  L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)
                  LA    R3,TGTNAME-TARGET_DSECT(,R6)
                  XR    R4,R4
                  LH    R4,TGTNAMELEN-TARGET_DSECT(,R6)
                  AR    R3,R4
                  LH    R4,TGTNAMEMEMLEN-TARGET_DSECT(,R6)
                  ST    R4,G_SCAN_TOKEN_LEN
                  BCTR  R4,R0
                  B     *+10
                  MVC   0(1,R2),0(R3)
                  EX    R4,*-6
               ENDIF
            ENDIF
*
            MVI   G_SCAN_APPEND_TO,X'01'
            L     R15,LWZMAKE_APPEND_TOKENA_EXEC
            BASR  R14,R15         * Link to APPEND_TOKEN section
*
            B     EXEC_TGT_CALL_NEXT_TOKEN
*
EXEC_TGT_CALL_EXPANDED EQU *
*
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Calling X
               REXX'
            LA    R2,G_LWZMRPT_LINE+36
            LA    R3,STMT_C_EXEC
            XR    R4,R4
            LH    R4,STMT_C_EXECLEN
            BCTR  R4,R0
            B     *+10
            MVC   0(1,R2),0(R3)
            EX    R4,*-6
            LT    R4,G_SCAN_TOKEN2_LEN
            IF (NZ) THEN
               AH    R2,STMT_C_EXECLEN
               LA    R2,1(,R2)
               LA    R3,G_LWZMRPT_LINE+133
               SR    R3,R2
               CR    R4,R3
               IF (H) THEN
                  LR    R4,R3
               ENDIF
               BCTR  R4,R0
               L     R3,G_SCAN_TOKEN2A
               B     *+10
               MVC   0(1,R2),0(R3)
               EX    R4,*-6
            ENDIF
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
*
            IF (CLI,G_USE_ISPEXEC,EQ,C' ') THEN
               L     R15,LWZMAKE_IRXINITA_EXEC
               BASR  R14,R15
*
*              MVI   G_USE_ISPEXEC,C'N'
*
*              MVC   G_IRXINIT_FUNCTION,=CL8'FINDENVB'
*              MVC   G_IRXINIT_PARMMOD,=CL8' '
*              MVC   G_IRXINIT_INSTORPARM_PTR,=A(0)
*              MVC   G_IRXINIT_USRFIELD_PTR,=X'80000000'
*              MVC   G_IRXINIT_RESERVED_PTR,=A(0)
*              MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)
*              MVC   G_IRXINIT_REASON,=A(0)
*
*              XR    R0,R0
*              LA    R1,G_IRXINIT_FUNCTION
*              ST    R1,G_IRXINIT_PAR7A
*              LA    R1,G_IRXINIT_PARMMOD
*              ST    R1,G_IRXINIT_PAR7A+4
*              LA    R1,G_IRXINIT_INSTORPARM_PTR
*              ST    R1,G_IRXINIT_PAR7A+8
*              LA    R1,G_IRXINIT_USRFIELD_PTR
*              ST    R1,G_IRXINIT_PAR7A+12
*              LA    R1,G_IRXINIT_RESERVED_PTR
*              ST    R1,G_IRXINIT_PAR7A+16
*              LA    R1,G_IRXINIT_ENVBLOCK_PTR
*              ST    R1,G_IRXINIT_PAR7A+20
*              LA    R1,G_IRXINIT_REASON
*              O     R1,=X'80000000'
*              ST    R1,G_IRXINIT_PAR7A+24
*              LA    R1,G_IRXINIT_PAR7A
*
*              LINK  EP=IRXINIT,SF=(E,G_LINKD)
*
*              C     R15,=A(0)
*              BE    IRXINIT_OK
*              C     R15,=A(4)
*              BE    IRXINIT_OK
*              C     R15,=A(28)
*              BE    IRXINIT_OK
*              MLWZMRPT RPTLINE=CL133'0Error finding REXX environment'
*              MVC   G_RETCODE,=F'12'
*              BR    R8
*IRXINIT_OK     EQU   *
*
*              C     R15,=A(28)
*              IF (NE) THEN
*                 L     R2,G_IRXINIT_ENVBLOCK_PTR
*                 L     R2,16(,R2)
*                 L     R2,20(,R2)
*                 L     R3,8(,R2)
*                 L     R4,12(,R2)
*                 L     R2,0(,R2)
*FIND_ISPEXEC      EQU   *
*                 CLC   0(8,R2),=CL8'ISPEXEC'
*                 BE    ISPEXEC_FOUND
*                 AR    R2,R4
*                 BCT   R3,FIND_ISPEXEC
*                 B     ISPEXEC_NOT_FOUND
*ISPEXEC_FOUND     EQU   *
*                 MVI   G_USE_ISPEXEC,C'Y'
*ISPEXEC_NOT_FOUND EQU   *
*              ENDIF
            ENDIF
*
            IF (CLI,G_USE_ISPEXEC,EQ,C'Y') THEN
               L     R6,G_SCAN_TOKENA
               MVC   0(12,R6),=C'SELECT CMD(%'
               LA    R5,12
               LA    R6,12(,R6)
               LA    R3,STMT_C_EXEC
               XR    R4,R4
               LH    R4,STMT_C_EXECLEN
               CH    R4,=H'8'
               IF (H) THEN
                  LH    R4,=H'8'
               ENDIF
               BCTR  R4,R0
               B     *+10
               MVC   0(1,R6),0(R3)
               EX    R4,*-6
               LA    R4,1(,R4)
               AR    R6,R4
               AR    R5,R4
               ST    R5,G_SCAN_TOKEN_LEN
               CLC   G_SCAN_TOKEN2_LEN,=F'0'
               IF (NE) THEN
                  LA    R1,1(,R5)
                  A     R1,G_SCAN_TOKEN2_LEN
                  C     R1,G_SCAN_TOKEN_MAXLEN
                  IF (H) THEN
                     L     R3,G_SCAN_TOKEN_MAXLEN * Get current max len
                     LR    R4,R3      * Save it for storage release
ISPEXEC_ENLARGE_TOKEN EQU *
                     SLL   R3,1       * Multiply max length by 2
                     CR    R1,R3
                     BH    ISPEXEC_ENLARGE_TOKEN
                     ST    R3,G_SCAN_TOKEN_MAXLEN * Make it new max len
                     STORAGE OBTAIN,LENGTH=(R3) * Allocate a mem block
                     LR    R0,R1      * Have R0 point to new block
                     L     R1,G_SCAN_TOKEN_LEN * Get length of token
                     L     R2,G_SCAN_TOKENA * Have R2 point to old blk
                     LR    R14,R2     * Save it for storage release
                     LR    R3,R1      * Make sure no cropping/filling
                     ST    R0,G_SCAN_TOKENA * Save ptr to new block
                     MVCL  R0,R2      * Copy old to new block
                     LR    R2,R14
                     STORAGE RELEASE,LENGTH=(R4),ADDR=(R2)
                     L     R6,G_SCAN_TOKENA
                     AR    R6,R5
                  ENDIF
                  MVI   0(R6),C' '
                  LA    R6,1(,R6)
                  LA    R5,1(,R5)
                  LR    R0,R6
                  L     R2,G_SCAN_TOKEN2A
                  L     R1,G_SCAN_TOKEN2_LEN
                  LR    R3,R1
                  MVCL  R0,R2
                  A     R5,G_SCAN_TOKEN2_LEN
                  A     R6,G_SCAN_TOKEN2_LEN
               ENDIF
               MVI   0(R6),C')'
               LA    R5,1(,R5)
               ST    R5,G_SCAN_TOKEN_LEN
               LA    R1,G_SCAN_TOKEN_LEN
               ST    R1,G_ISPEXEC_PAR2A
               L     R1,G_SCAN_TOKENA
               O     R1,=X'80000000'
               ST    R1,G_ISPEXEC_PAR2A+4
               LA    R1,G_ISPEXEC_PAR2A
*
               LINK  EP=ISPEXEC,SF=(E,G_LINKD)
*
               LTR   R15,R15
               IF (NZ) THEN
                  MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'
                  MVC   G_RETCODE,=F'12'
                  BR    R8
               ENDIF
*
               B     EXEC_CALL_END
            ENDIF
*
            LA    R6,G_IRXEXEC_EXECBLK
            USING EXECBLK,R6
            MVC   EXEC_BLK_ACRYN,=CL8'IRXEXECB'
            LA    R5,EXECBLEN
            ST    R5,EXEC_BLK_LENGTH
            MVC   EXEC_MEMBER,=CL8' '
            LA    R2,EXEC_MEMBER
            LA    R3,STMT_C_EXEC
            XR    R4,R4
            LH    R4,STMT_C_EXECLEN
            CH    R4,=H'8'
            IF (H) THEN
               LH    R4,=H'8'
            ENDIF
            BCTR  R4,R0
            B     *+10
            MVC   0(1,R2),0(R3)
            EX    R4,*-6
            MVC   EXEC_DDNAME,=CL8' '
            MVC   EXEC_SUBCOM,=CL8' '
            XR    R5,R5
            ST    R5,EXEC_BLK_LENGTH+4
            ST    R5,EXEC_DSNPTR
            ST    R5,EXEC_DSNLEN
            DROP  R6
*
            L     R6,G_EVALBLOCK_PTR
            USING EVALBLOCK,R6
            XR    R5,R5
            ST    R5,EVALBLOCK_EVPAD1
            ST    R5,EVALBLOCK_EVPAD2
            L     R5,G_EVALBLOCK_MAXLEN
            SRA   R5,3
            ST    R5,EVALBLOCK_EVSIZE
            DROP  R6
*
            LA    R1,G_IRXEXEC_EXECBLK
            ST    R1,G_IRXEXEC_EXECBLK_PTR
            CLC   G_SCAN_TOKEN2_LEN,=F'0'
            IF (NE) THEN
               MVC   G_IRXEXEC_ARGS(4),G_SCAN_TOKEN2A
               MVC   G_IRXEXEC_ARGS+4(4),G_SCAN_TOKEN2_LEN
               MVC   G_IRXEXEC_ARGS+8(8),=X'FFFFFFFFFFFFFFFF'
            ELSE
               MVC   G_IRXEXEC_ARGS,=X'FFFFFFFFFFFFFFFF'
            ENDIF
            LA    R1,G_IRXEXEC_ARGS
            ST    R1,G_IRXEXEC_ARGS_PTR
            MVC   G_IRXEXEC_FLAGS,=X'40000000'
            MVC   G_IRXEXEC_INSTBLK_PTR,=A(0)
            MVC   G_IRXEXEC_CPPL_PTR,=A(0)
            MVC   G_IRXEXEC_EVALBLK_PTR,G_EVALBLOCK_PTR
            MVC   G_IRXEXEC_WORKAREA_PTR,=A(0)
            MVC   G_IRXEXEC_USRFIELD_PTR,=X'8000000'
            MVC   G_IRXEXEC_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR
            LA    R1,G_IRXEXEC_REASON
            ST    R1,G_IRXEXEC_REASON_PTR
            XR    R0,R0
            LA    R1,G_IRXEXEC_EXECBLK_PTR
            ST    R1,G_IRXEXEC_PAR10A
            LA    R1,G_IRXEXEC_ARGS_PTR
            ST    R1,G_IRXEXEC_PAR10A+4
            LA    R1,G_IRXEXEC_FLAGS
            ST    R1,G_IRXEXEC_PAR10A+8
            LA    R1,G_IRXEXEC_INSTBLK_PTR
            ST    R1,G_IRXEXEC_PAR10A+12
            LA    R1,G_IRXEXEC_CPPL_PTR
            ST    R1,G_IRXEXEC_PAR10A+16
            LA    R1,G_IRXEXEC_EVALBLK_PTR
            ST    R1,G_IRXEXEC_PAR10A+20
            LA    R1,G_IRXEXEC_WORKAREA_PTR
            ST    R1,G_IRXEXEC_PAR10A+24
            LA    R1,G_IRXEXEC_USRFIELD_PTR
            ST    R1,G_IRXEXEC_PAR10A+28
            LA    R1,G_IRXEXEC_ENVBLOCK_PTR
            ST    R1,G_IRXEXEC_PAR10A+32
            LA    R1,G_IRXEXEC_REASON_PTR
            O     R1,=X'80000000'
            ST    R1,G_IRXEXEC_PAR10A+36
            LA    R1,G_IRXEXEC_PAR10A
*
            LINK  EP=IRXEXEC,SF=(E,G_LINKD)
*
            LTR   R15,R15
            IF (NZ) THEN
               MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'
               MVC   G_RETCODE,=F'12'
               BR    R8
            ENDIF
*
            MVC   G_IRXINIT_ENVBLOCK_PTR,G_IRXEXEC_ENVBLOCK_PTR
*
            L     R5,G_EVALBLOCK_PTR
            USING EVALBLOCK,R5
*
            CLC   EVALBLOCK_EVLEN,=F'1'
            BNE   EXEC_REXX_ERROR
            CLI   EVALBLOCK_EVDATA,C'0'
            BE    EXEC_REXX_NO_ERROR
EXEC_REXX_ERROR EQU *
            MVC   G_LWZMRPT_LINE,=CL133'0REXX exec returned'
            LA    R2,G_LWZMRPT_LINE+20
            LA    R3,EVALBLOCK_EVDATA
            L     R4,EVALBLOCK_EVLEN
            C     R4,=F'113'
            IF (H) THEN
               L     R4,=F'113'
            ENDIF
            BCTR  R4,R0
            B     *+10
            MVC   0(1,R2),0(R3)
            EX    R4,*-6
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
            MVC   G_RETCODE,=F'8'
            BR    R8
EXEC_REXX_NO_ERROR EQU *
*
            DROP  R5
*
EXEC_CALL_END EQU *
            L     R2,G_SCAN_TOKEN_MAXLEN
            L     R3,G_SCAN_TOKENA
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
            MVC   G_SCAN_TOKENA,EXEC_SAVE_SCAN_TOKENA
            MVC   G_SCAN_TOKEN_MAXLEN,EXEC_SAVE_SCAN_TOKEN_MAXLEN
            MVC   G_SCAN_TOKEN_LEN,EXEC_SAVE_SCAN_TOKEN_LEN
*
            L     R2,G_SCAN_TOKEN2_MAXLEN
            L     R3,G_SCAN_TOKEN2A
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage
            MVC   G_SCAN_TOKEN2A,EXEC_SAVE_SCAN_TOKEN2A
            MVC   G_SCAN_TOKEN2_MAXLEN,EXEC_SAVE_SCAN_TOKEN2_MAXLEN
            MVC   G_SCAN_TOKEN2_LEN,EXEC_SAVE_SCAN_TOKEN2_LEN
         ENDIF
*
         DROP  R7
         USING STMT_DSECT,R7
*
         LT    R7,STMT_NEXT_PTR
         BZ    EXEC_TGT_BUILD_RET
*
         CLI   STMT_IN_RECIPE,C'Y'
         BE    NEXT_RECIPE_STMT
*
         DROP  R7
*
EXEC_TGT_BUILD_RET EQU *
         BR    R8
*
EXEC_TGT_BUILD_NO_RECIPE EQU *
         MLWZMRPT RPTLINE=CL133' ..................... No recipe'
         BR    R8
*
         LTORG
*
LWZMAKE_SCAN_TOKENA_EXEC    DC    A(LWZMAKE_SCAN_TOKEN)
LWZMAKE_SCAN_VARA_EXEC      DC    A(LWZMAKE_SCAN_VAR)
LWZMAKE_FINDPNYA_EXEC       DC    A(LWZMAKE_FINDPNY)
LWZMAKE_FINDTGTA_EXEC       DC    A(LWZMAKE_FINDTGT)
LWZMAKE_EXEC_TGTA_EXEC      DC    A(LWZMAKE_EXEC_TGT)
LWZMAKE_GET_DATEA_EXEC      DC    A(LWZMAKE_GET_DATE)
LWZMAKE_APPEND_TOKENA_EXEC  DC    A(LWZMAKE_APPEND_TOKEN)
LWZMAKE_IRXINITA_EXEC       DC    A(LWZMAKE_IRXINIT)
*
WORKAREA_EXEC_TGT           DSECT
EXEC_TGT_SA                 DS    18F
RETCODE_EXEC_TGT            DS    F
EXEC_TGT_PARA               DS    A
EXEC_NEXTTGT_PARA           DS    A
EXEC_NEXTTGT_PAR            DS    CL(EXEC_TGT_PAR_LEN)
*
                            DS    0F
TARGET_ALTER_DATE           DS    CL16
*
                            DS    0F
EXEC_WORD_SPLIT_PTR         DS    A
EXEC_WORD_SPLIT_LEN         DS    F
*
                            DS    0F
EXEC_IRXEXECB               DS    CL(EXECBLK_V2_LEN)
*
                            DS    0F
EXEC_SAVE_SCAN_TOKEN_LEN    DS    F
EXEC_SAVE_SCAN_TOKEN2_LEN   DS    F
EXEC_SAVE_SCAN_TOKEN_MAXLEN DS    F
EXEC_SAVE_SCAN_TOKEN2_MAXLEN DS    F
EXEC_SAVE_SCAN_TOKENA       DS    A
EXEC_SAVE_SCAN_TOKEN2A      DS    A
WORKAREA_EXEC_TGT_LEN       EQU *-WORKAREA_EXEC_TGT
*
EXEC_TGT_PAR                DSECT
EXEC_TGT_PTR                DS    A
EXEC_TGT_PAR_LEN            EQU   *-EXEC_TGT_PAR
*
SHVBLOCK_DSECT              DSECT
SHVBLOCK_SHVNEXT            DS    A
SHVBLOCK_SHVUSER            DS    A
SHVBLOCK_SHVCODE            DS    C
SHVBLOCK_SHVRET             DS    C
SHVBLOCK_RESERVED           DS    H
SHVBLOCK_SHVBUFL            DS    F
SHVBLOCK_SHVNAMA            DS    A
SHVBLOCK_SHVNAML            DS    F
SHVBLOCK_SHVVALA            DS    A
SHVBLOCK_SHVVALL            DS    F
SHVBLOCK_SHVBLEN            EQU   *-SHVBLOCK_DSECT
*
LWZMAKE  CSECT
*
* Get the date of a file
*
         DROP
*
LWZMAKE_GET_DATE DS    0F
         STM   R14,R12,12(R13)   * Save callers registers
         LR    R10,R15
         LA    R11,4095(,R10)
         LA    R11,1(,R11)
         USING LWZMAKE_GET_DATE,R10,R11
         GETMAIN RU,LV=GET_DATE_DSECT_SIZ
         ST    R13,4(R1)         * Backward chain callers SA
         ST    R1,8(R13)         * Forward chain my SA
         LR    R13,R1            * Point R13 to my SA
         USING GET_DATE_DSECT,R13 * Establish addressing of workarea
         USING GLOBAL_DATA_DSECT,R9
*
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_GET_DATE'
*
         MVC   G_SAVE_ALTER_DATE,=CL16' '
*
         L     R3,G_SCAN_TOKENA
         L     R4,G_SCAN_TOKEN_LEN
         XR    R6,R6
         XR    R7,R7
*
GET_DATE_TEST_QUAL1 EQU *
         LTR   R4,R4
         BZ    GET_DATE_NOT_MVSDS
         TRT   0(1,R3),TRT_ALPHANAT
         BNZ   GET_DATE_NOT_MVSDS
         LA    R3,1(,R3)
         BCT   R4,*+8
         B     GET_DATE_MVSDS
         LR    R5,R4
         C     R5,=F'7'
         IF (H) THEN
            L     R5,=F'7'
         ENDIF
         BCTR  R5,R0
         B     *+10
         TRT   0(1,R3),TRT_ALPHANUMNATDASH
         EX    R5,*-6
         IF (Z) THEN
            LA    R5,1(,R5)
         ELSE
            LR    R5,R1
            SR    R5,R3
         ENDIF
         AR    R3,R5
         SR    R4,R5
         BZ    GET_DATE_MVSDS
         IF (CLI,0(R3),EQ,C'.') THEN
            LA    R3,1(,R3)
            BCTR  R4,R0
            B     GET_DATE_TEST_QUAL1
         ENDIF
         CLI   0(R3),C'('
         BNE   GET_DATE_NOT_MVSDS
         LR    R6,R3
         LA    R3,1(,R3)
         BCT   R4,*+8
         B     GET_DATE_NOT_MVSDS
         TRT   0(1,R3),TRT_ALPHANAT
         BNZ   GET_DATE_NOT_MVSDS
         LR    R6,R3
         LA    R3,1(,R3)
         BCT   R4,*+8
         B     GET_DATE_NOT_MVSDS
         LR    R5,R4
         C     R5,=F'7'
         IF (H) THEN
            L     R5,=F'7'
         ENDIF
         BCTR  R5,R0
         B     *+10
         TRT   0(1,R3),TRT_ALPHANUMNATDASH
         EX    R5,*-6
         IF (Z) THEN
            LA    R5,1(,R5)
         ELSE
            LR    R5,R1
            SR    R5,R3
         ENDIF
         AR    R3,R5
         SR    R4,R5
         BZ    GET_DATE_NOT_MVSDS
         CLI   0(R3),C')'
         BNE   GET_DATE_NOT_MVSDS
         LR    R7,R3
         SR    R7,R6
         LA    R3,1(,R3)
         BCT   R4,GET_DATE_NOT_MVSDS
*
GET_DATE_MVSDS EQU *
         ST    R6,G_MVSDS_MEMBER_PTR
         ST    R7,G_MVSDS_MEMBER_LEN
*
         MLWZMRPT RPTLINE=CL133' ..................... Name is MVS dataX
                set name'
*
         BAL   R8,GET_DATE_IGGCSI00
*
         CLC   G_RETCODE,=A(0)
         BNE   GET_DATE_RET
*
         CLI   G_DSFOUND,C'Y'
         BNE   GET_DATE_RET
*
         CLC   G_MVSDS_MEMBER_PTR,=A(0)
         BE    GET_DATE_RET
*
         BAL   R8,GET_DATE_OBTAIN
*
         CLC   G_RETCODE,=A(0)
         BNE   GET_DATE_RET
*
         IF (TM,OBTAIN_GD+(DS1RECFM-OBTAIN_DSECT),DS1RECFU,O) THEN
            BAL   R8,GET_DATE_LOADMOD
         ELSE
            BAL   R8,GET_DATE_STATS
         ENDIF
*
         B     GET_DATE_RET
*
GET_DATE_NOT_MVSDS EQU *
         MLWZMRPT RPTLINE=CL133' ..................... Name is not MVS X
               data set name'
         MVC   G_SAVE_ALTER_DATE,=16X'FF'
*
GET_DATE_RET EQU *
         L     R3,4(,R13)        * Restore address of callers SA
         FREEMAIN RU,LV=GET_DATE_DSECT_SIZ,A=(R13)
         LR    R13,R3
         LM    R14,R12,12(R13)
         BR    R14                    Return to caller
*
* Perform catalog search with IGGCSI00
*
GET_DATE_IGGCSI00 EQU *
         MVI   G_DSFOUND,C'N'
*
         LA    R1,DAREA_GD
         ST    R1,DAREAPTR_GD
         L     R2,=A(DAREA_GD_SIZ)
         ST    R2,0(,R1)
*
         LA    R2,CSIFIELD_GD
         L     R3,=A(CSIFIELD_GD_LEN)
         LA    R4,CONST_CSIFIELD_GD
         L     R5,=A(CONST_CSIFIELD_GD_LEN)
         MVCL  R2,R4
*
         LA    R2,CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)
         L     R3,G_SCAN_TOKENA
         L     R4,G_SCAN_TOKEN_LEN
         LT    R5,G_MVSDS_MEMBER_PTR
         IF (NZ) THEN
            SR    R5,R3
            BCTR  R5,R0
            LR    R4,R5
         ENDIF
         C     R4,=A(L'CSIFILTK)
         IF (H)
            L     R4,=A(L'CSIFILTK)
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
*
         LA    R1,PARMLIST_GD
         LA    R2,MODRSNRT_GD
         ST    R2,0(R1)
         LA    R2,CSIFIELD_GD
         ST    R2,4(R1)
         L     R2,DAREAPTR_GD
         O     R2,=X'80000000'
         ST    R2,8(R1)
*
         L     R15,G_IGGCSI00A
         BASR  R14,R15
*
         C     R15,=F'4'
         IF (H) THEN
            MLWZMRPT RPTLINE=CL133'0Catalog search interface returned eX
               rror code'
            MVC   G_RETCODE,=F'12'
            BR    R8
         ENDIF
*
         LA    R1,DAREA_GD
         CLC   8(4,R1),=F'64'
         IF (H) THEN
            MVI   G_DSFOUND,C'Y'
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Found inX
                catalog'
         ELSE
            MVC   G_SAVE_ALTER_DATE,=16X'FF'
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Not founX
               d in catalog'
         ENDIF
         L     R15,G_LWZMAKE_RPTA
         BASR  R14,R15
*
GET_DATE_IGGCSI00_RET EQU *
         BR    R8
*
* Perform CAMLST OBTAIN
*
GET_DATE_OBTAIN EQU *
         XR    R1,R1
         ICM   R1,B'1000',=AL1(193)
         ST    R1,DSCBPAR_GD
         MVC   OBTAIN_GD+(DS1DSNAM-OBTAIN_DSECT)(L'DS1DSNAM),CSIFIELD_GX
               D+(CSIFILTK-CSIFIELD_DSECT)
         LA    R1,OBTAIN_GD+(DS1DSNAM-OBTAIN_DSECT)
         ST    R1,DSCBPAR_GD+4
         LA    R1,DAREA_GD+110
         CLC   0(2,R1),=H'12'   * Is volume name present
         BL    GET_DATE_OBTAIN_RET
         LA    R1,6(,R1)
         ST    R1,DSCBPAR_GD+8
         LA    R1,OBTAIN_GD+(DS1FMTID-OBTAIN_DSECT)
         ST    R1,DSCBPAR_GD+12
*
         OBTAIN DSCBPAR_GD
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0CAMLST OBTAIN returned error code'
            MVC   G_RETCODE,=F'12'
            BR    R8
         ENDIF
*
         IF (TM,OBTAIN_GD+(DS1DSORG-OBTAIN_DSECT),DS1DSGPO,Z) THEN
            MVC   G_LWZMRPT_LINE,=CL133'0Member specified on non-PDS daX
               taset'
            MVC   G_LWZMRPT_LINE+37(L'CSIFILTK),CSIFIELD_GD+(CSIFILTK-CX
               SIFIELD_DSECT)
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
GET_DATE_OBTAIN_RET EQU *
         BR    R8
*
* Get the date from a load module
*
GET_DATE_LOADMOD EQU *
         MLWZMRPT RPTLINE=CL133' ..................... Retrieving load X
               module creation date/time'
*
         MVI   G_DSFOUND,C'N'
*
         MVC   MEM8_GD,=CL8' '
         LA    R2,MEM8_GD
         L     R3,G_MVSDS_MEMBER_PTR
         L     R4,G_MVSDS_MEMBER_LEN
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
*
         LA    R6,DYNALLOC_AREA_GD
         USING S99RBP,R6
         LA    R4,S99RBPTR+4
         USING S99RB,R4
         ST    R4,S99RBPTR
         OI    S99RBPTR,S99RBPND
         XC    S99RB(S99RBEND-S99RB),S99RB
         MVI   S99RBLN,S99RBEND-S99RB
         MVI   S99VERB,S99VRBAL
         OI    S99FLG11,S99MSGL0
         LA    R5,S99RB+(S99RBEND-S99RB)+12
         MVC   0(CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_GD_L,R5),CDSNTU_GD
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)
         LA    R3,S99RB+(S99RBEND-S99RB)
         ST    R3,S99TXTPP
         ST    R5,0(,R3)
         LA    R5,CDSNTU_GD_L(,R5)
         ST    R5,4(,R3)
         LA    R5,CSTATUSTU_GD_L(,R5)
         O     R5,=X'80000000'
         ST    R5,8(,R3)
         LA    R1,DYNALLOC_AREA_GD
         DYNALLOC
*
         DROP  R6
         DROP  R4
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
         L     R1,G_DCB_MEM_PTR
         LA    R2,DCBPDS_BDR-DCB_DSECT(,R1)
         MVC   0(LEN_DCBPDS_BDR,R2),CDCBPDS_BDR
         L     R6,DYNALLOC_AREA_GD
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_GD_L+CSTATUSTU_GD_L(,R6)
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)
*
         OPEN  ((R2)),MODE=31,MF=(E,G_OPEND)
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0OPEN failed for accessing PDS with X
               load modules'
            MVC   G_RETCODE,=F'8'
            B     GET_DATE_LOADMOD_DEALLOC
         ENDIF
*
         MVC   IEWBFDAT_SB_SB(2),=C'SB'
         MVC   IEWBFDAT_SB_SB+2(2),=X'0001'
         XC    IEWBFDAT_SB_MTOKEN,IEWBFDAT_SB_MTOKEN
         MVC   IEWBFDAT_SB_PGMNAME,MEM8_GD
*
         LA    R1,IEWBFDAT_SB_SB
         ST    R1,IEWBFDAT_SB_PAR4A
         LA    R1,IEWBFDAT_SB_MTOKEN
         ST    R1,IEWBFDAT_SB_PAR4A+4
         L     R14,G_DCB_MEM_PTR
         LA    R1,DCBPDS_BDR-DCB_DSECT(,R14)
         ST    R1,IEWBFDAT_SB_PAR4A+8
         LA    R1,IEWBFDAT_SB_PGMNAME
         O     R1,=X'80000000'
         ST    R1,IEWBFDAT_SB_PAR4A+12
         LA    R1,IEWBFDAT_SB_PAR4A
*
         L     R15,G_IEWBFDATA
         BASR  R14,R15
*
         C     R15,=A(0)
         BE    GET_DATE_LOADMOD_NOERR1
         C     R15,=A(4)
         BE    GET_DATE_LOADMOD_NOERR1
         C     R15,=A(12)
         IF (EQ) THEN
            C     R0,=X'10800032'
            BE    GET_DATE_LOADMOD_NOTFOUND
         ENDIF
         CVD   R15,G_DEC8         * convert return value to packed
         UNPK  G_ZONED8,G_DEC8    * convert return value to zoned
         OI    G_ZONED8+7,X'F0'   * get rid of sign
         MVC   G_HELPER_DATA(8),G_ZONED8
         MVI   G_HELPER_DATA+8,C' '
         ST    R0,G_DEC8          * Put ptr in area of at least 5 bytes
         UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex
         TR    G_ZONED8,GETDATE_HEXTAB * Turn into hex
         MVC   G_HELPER_DATA+9(8),G_ZONED8
         LA    R14,G_HELPER_DATA
         ST    R14,G_LWZMTRC_DATA_PTR
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)
         MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA
         MLWZMRPT RPTLINE=CL133'0Error starting binder fast data accessX
                session'
         MVC   G_RETCODE,=F'12'
         BR    R8
*
GET_DATE_LOADMOD_NOERR1 EQU *
         MVI   G_DSFOUND,C'Y'
*
IEWBIDB_BASE EQU R6                      Base register for IDRB buffer.
IDB_BASE     EQU R7                      Base register for IDRB entry.
         IEWBUFF FUNC=GETBUF,TYPE=IDRB   Get memory for IDRB buffer.
         IEWBUFF FUNC=INITBUF,TYPE=IDRB  Init IDRB buffer.
*
         MVC   IEWBFDAT_GD_GD(2),=C'GD'
         MVC   IEWBFDAT_GD_GD+2(2),=X'0001'
         MVC   IEWBFDAT_GD_MTOKEN,IEWBFDAT_SB_MTOKEN
         MVC   IEWBFDAT_GD_B_IDRB(2),=H'6'
         MVC   IEWBFDAT_GD_B_IDRB+2(6),=C'B_IDRB'
         XC    IEWBFDAT_GD_CURSOR,IEWBFDAT_GD_CURSOR
*
         LA    R1,IEWBFDAT_GD_GD
         ST    R1,IEWBFDAT_GD_PAR8A
         LA    R1,IEWBFDAT_GD_MTOKEN
         ST    R1,IEWBFDAT_GD_PAR8A+4
         LA    R1,IEWBFDAT_GD_B_IDRB
         ST    R1,IEWBFDAT_GD_PAR8A+8
         XR    R1,R1
         ST    R1,IEWBFDAT_GD_PAR8A+12
         ST    IEWBIDB_BASE,IEWBFDAT_GD_PAR8A+16
         LA    R1,IEWBFDAT_GD_CURSOR
         ST    R1,IEWBFDAT_GD_PAR8A+20
         LA    R1,IEWBFDAT_GD_COUNT
         ST    R1,IEWBFDAT_GD_PAR8A+24
         L     R1,=X'80000000'
         ST    R1,IEWBFDAT_GD_PAR8A+28
         LA    R1,IEWBFDAT_GD_PAR8A
*
         L     R15,G_IEWBFDATA
         BASR  R14,R15
*
         C     R15,=A(0)
         IF (NE) THEN
            C     R15,=A(4)
         ENDIF
         IF (NE) THEN
            CVD   R15,G_DEC8      * convert return value to packed
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned
            OI    G_ZONED8+7,X'F0' * get rid of sign
            MVC   G_HELPER_DATA(8),G_ZONED8
            MVI   G_HELPER_DATA+8,C' '
            ST    R0,G_DEC8       * Put ptr in area of at least 5 bytes
            UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex
            TR    G_ZONED8,GETDATE_HEXTAB * Turn into hex
            MVC   G_HELPER_DATA+9(8),G_ZONED8
            LA    R14,G_HELPER_DATA
            ST    R14,G_LWZMTRC_DATA_PTR
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA
            MLWZMRPT RPTLINE=CL133'0Error during binder fast data accesX
               s get data function'
            MVC   G_RETCODE,=F'12'
            BR    R8
         ENDIF
*
         MVC   IEWBFDAT_EN_EN(2),=C'EN'
         MVC   IEWBFDAT_EN_EN+2(2),=X'0001'
         MVC   IEWBFDAT_EN_MTOKEN,IEWBFDAT_SB_MTOKEN
*
         LA    R1,IEWBFDAT_EN_EN
         ST    R1,IEWBFDAT_EN_PAR2A
         LA    R1,IEWBFDAT_EN_MTOKEN
         ST    R1,IEWBFDAT_EN_PAR2A+4
         LA    R1,IEWBFDAT_EN_PAR2A
*
         L     R15,G_IEWBFDATA
         BASR  R14,R15
*
         LTR   R15,R15
         IF (NZ) THEN
            CVD   R15,G_DEC8      * convert return value to packed
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned
            OI    G_ZONED8+7,X'F0' * get rid of sign
            MVC   G_HELPER_DATA(8),G_ZONED8
            MVI   G_HELPER_DATA+8,C' '
            ST    R0,G_DEC8       * Put ptr in area of at least 5 bytes
            UNPK  G_ZONED8(9),G_DEC8(5)      * Turn into almost hex
            TR    G_ZONED8,GETDATE_HEXTAB    * Turn into hex
            MVC   G_HELPER_DATA+9(8),G_ZONED8
            LA    R14,G_HELPER_DATA
            ST    R14,G_LWZMTRC_DATA_PTR
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA
            MLWZMRPT RPTLINE=CL133'0Error ending binder fast data accesX
               s session'
            MVC   G_RETCODE,=F'12'
            BR    R8
         ENDIF
*
         MVI   CONVTOD_INAREA,X'00'
         MVC   CONVTOD_INAREA+1(15),CONVTOD_INAREA
         PACK  CONVTOD_INAREA(4),IDB_TIME_BOUND(L'IDB_TIME_BOUND+1)
         MVI   CONVTOD_INAREA+4,X'00'
         PACK  CONVTOD_INAREA+8(5),IDB_DATE_BOUND(L'IDB_DATE_BOUND+1)
         MVI   CONVTOD_OUTAREA,X'00'
         MVC   CONVTOD_OUTAREA+1(7),CONVTOD_OUTAREA
         MVI   STCKCONV_OUTAREA,X'00'
         MVC   STCKCONV_OUTAREA+1(15),STCKCONV_OUTAREA
         CONVTOD CONVVAL=CONVTOD_INAREA,TODVAL=CONVTOD_OUTAREA,TIMETYPEX
               =DEC,DATETYPE=YYYYDDD,MF=(E,CONVTOD_PLIST)
         STCKCONV STCKVAL=CONVTOD_OUTAREA,CONVVAL=STCKCONV_OUTAREA,TIMEX
               TYPE=DEC,DATETYPE=YYYYMMDD,MF=(E,STCKCONV_PLIST)
         MVC   DATEWORK_DEC_1(4),STCKCONV_OUTAREA+8
         MVC   DATEWORK_DEC_1+4(3),STCKCONV_OUTAREA
         MVO   DATEWORK_DEC_2,DATEWORK_DEC_1(7)
         MVN   DATEWORK_DEC_2+7(1),=X'0F'
         UNPK  DATEWORK_ZON,DATEWORK_DEC_2
         MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON
*
         IEWBUFF FUNC=FREEBUF,TYPE=IDRB  Free IDRB buffer.
*
GET_DATE_LOADMOD_NOTFOUND EQU *
*
         IF (CLI,G_DSFOUND,EQ,C'Y') THEN
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Load modX
               ule found in PDS, last altered on'
            MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON
            MVC   G_LWZMRPT_LINE+65(19),=C'0000-00-00 00:00:00'
            MVC   G_LWZMRPT_LINE+65(4),G_SAVE_ALTER_DATE+2
            MVC   G_LWZMRPT_LINE+70(2),G_SAVE_ALTER_DATE+6
            MVC   G_LWZMRPT_LINE+73(2),G_SAVE_ALTER_DATE+8
            MVC   G_LWZMRPT_LINE+76(2),G_SAVE_ALTER_DATE+10
            MVC   G_LWZMRPT_LINE+79(2),G_SAVE_ALTER_DATE+12
            MVC   G_LWZMRPT_LINE+82(2),G_SAVE_ALTER_DATE+14
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
         ELSE
            MVC   G_SAVE_ALTER_DATE,=16X'FF'
            MLWZMRPT RPTLINE=CL133' ..................... Load module nX
               ot found in PDS'
         ENDIF
*
         DROP  R6
         DROP  R7
*
GET_DATE_LOADMOD_CLOSE EQU *
         L     R14,G_DCB_MEM_PTR
         LA    R2,DCBPDS_BDR-DCB_DSECT(,R14)
         CLOSE ((R2)),MODE=31
*
GET_DATE_LOADMOD_DEALLOC EQU *
         LA    R6,DYNALLOC_AREA_GD
         USING S99RBP,R6
         LA    R4,S99RBPTR+4
         USING S99RB,R4
         ST    R4,S99RBPTR
         OI    S99RBPTR,S99RBPND
         XC    S99RB(S99RBEND-S99RB),S99RB
         MVI   S99RBLN,S99RBEND-S99RB
         MVI   S99VERB,S99VRBUN
         OI    S99FLG11,S99MSGL0
         LA    R5,S99RB+(S99RBEND-S99RB)+12
         MVC   0(CDSNTU_GD_L,R5),CDSNTU_GD
         LA    R2,CSIFIELD_GD
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)
         LA    R3,S99RB+(S99RBEND-S99RB)
         ST    R3,S99TXTPP
         O     R5,=X'80000000'
         ST    R5,0(,R3)
         LA    R1,DYNALLOC_AREA_GD
         DYNALLOC
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
GET_DATE_LOADMOD_RET EQU *
         BR    R8
*
* Get the date from member stats
*
GET_DATE_STATS EQU *
         MLWZMRPT RPTLINE=CL133' ..................... Retrieving PDS mX
               ember stats'
*
         MVI   G_DSFOUND,C'N'
*
         MVC   MEM8_GD,=CL8' '
         LA    R2,MEM8_GD
         L     R3,G_MVSDS_MEMBER_PTR
         L     R4,G_MVSDS_MEMBER_LEN
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
*
         LA    R6,DYNALLOC_AREA_GD
         USING S99RBP,R6
         LA    R4,S99RBPTR+4
         USING S99RB,R4
         ST    R4,S99RBPTR
         OI    S99RBPTR,S99RBPND
         XC    S99RB(S99RBEND-S99RB),S99RB
         MVI   S99RBLN,S99RBEND-S99RB
         MVI   S99VERB,S99VRBAL
         OI    S99FLG11,S99MSGL0
         LA    R5,S99RB+(S99RBEND-S99RB)+12
         MVC   0(CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_GD_L,R5),CDSNTU_GD
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)
         LA    R3,S99RB+(S99RBEND-S99RB)
         ST    R3,S99TXTPP
         ST    R5,0(,R3)
         LA    R5,CDSNTU_GD_L(,R5)
         ST    R5,4(,R3)
         LA    R5,CSTATUSTU_GD_L(,R5)
         O     R5,=X'80000000'
         ST    R5,8(,R3)
         LA    R1,DYNALLOC_AREA_GD
         DYNALLOC
*
         DROP  R6
         DROP  R4
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
         L     R1,G_DCB_MEM_PTR
         MVC   DCBPDS_DIR-DCB_DSECT(LEN_DCBPDS_DIR_GD,R1),CDCBPDS_DIR_GX
               D
         MVC   DCBEPDS_DIR-DCB_DSECT(LEN_DCBEPDS_DIR_GD,R1),CDCBEPDS_DIX
               R_GD
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)
         LA    R3,DCBEPDS_DIR-DCB_DSECT(,R1)
         ST    R3,DCBDCBE-IHADCB(,R2)
         LA    R4,PDSDIR_IS_EOF_GD
         ST    R4,DCBEEODA-DCBE(,R3)
         L     R6,DYNALLOC_AREA_GD
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_GD_L+CSTATUSTU_GD_L(,R6)
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)
*
         MVI   PDSDIR_EOF_GD,C'N'
*
         LA    R6,GET_DATE_OPENPDS
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)
GET_DATE_OPENPDS EQU *
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0OPEN failed for reading PDS directoX
               ry'
            MVC   G_RETCODE,=F'8'
            B     GET_DATE_DEALLOC
         ENDIF
*
GET_DATE_GET_DIRREC EQU *
         L     R1,G_DCB_MEM_PTR
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)
         LA    R6,GET_DATE_DIRREC_NOMORE
         GET   (R2),DIRREC_GD
*
         LA    R3,DIRREC_GD
         XR    R4,R4
         LH    R4,0(,R3)
         C     R4,=F'14'
         BL    GET_DATE_DIRREC_END_OF_BLOCK
         LA    R3,2(,R3)
         S     R4,=F'2'
GET_DATE_NEXT_DIRREC_ENTRY EQU *
         CLC   0(8,R3),=8X'FF'
         BE    GET_DATE_DIRREC_NOMORE
         CLC   0(8,R3),MEM8_GD
         IF (EQ) THEN
            MVI   G_DSFOUND,C'Y'
            L     R5,8(,R3)
            N     R5,=X'0000001F'
            SLL   R5,1
            C     R5,=F'30'
            IF (NL) THEN
               MVI   CONVTOD_INAREA,X'00'
               MVC   CONVTOD_INAREA+1(15),CONVTOD_INAREA
               MVC   CONVTOD_INAREA(2),24(R3)
               MVC   CONVTOD_INAREA+2(1),15(R3)
               MVC   CONVTOD_INAREA+8(4),20(R3)
               MVI   CONVTOD_OUTAREA,X'00'
               MVC   CONVTOD_OUTAREA+1(7),CONVTOD_OUTAREA
               MVI   STCKCONV_OUTAREA,X'00'
               MVC   STCKCONV_OUTAREA+1(15),STCKCONV_OUTAREA
               CONVTOD CONVVAL=CONVTOD_INAREA,TODVAL=CONVTOD_OUTAREA,TIX
               METYPE=DEC,DATETYPE=YYDDD,MF=(E,CONVTOD_PLIST)
               STCKCONV STCKVAL=CONVTOD_OUTAREA,CONVVAL=STCKCONV_OUTAREX
               A,TIMETYPE=DEC,DATETYPE=YYYYMMDD,MF=(E,STCKCONV_PLIST)
               MVC   G_LWZMRPT_LINE,=CL133' ..................... MembeX
               r found in PDS directory, last altered on'
               MVC   DATEWORK_DEC_1(4),STCKCONV_OUTAREA+8
               MVC   DATEWORK_DEC_1+4(3),STCKCONV_OUTAREA
               MVO   DATEWORK_DEC_2,DATEWORK_DEC_1(7)
               MVN   DATEWORK_DEC_2+7(1),=X'0F'
               UNPK  DATEWORK_ZON,DATEWORK_DEC_2
               MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON
               MVC   G_LWZMRPT_LINE+70(19),=C'0000-00-00 00:00:00'
               MVC   G_LWZMRPT_LINE+70(4),DATEWORK_ZON+2
               MVC   G_LWZMRPT_LINE+75(2),DATEWORK_ZON+6
               MVC   G_LWZMRPT_LINE+78(2),DATEWORK_ZON+8
               MVC   G_LWZMRPT_LINE+81(2),DATEWORK_ZON+10
               MVC   G_LWZMRPT_LINE+84(2),DATEWORK_ZON+12
               MVC   G_LWZMRPT_LINE+87(2),DATEWORK_ZON+14
               L     R15,G_LWZMAKE_RPTA
               BASR  R14,R15
            ELSE
               MLWZMRPT RPTLINE=CL133' ..................... Member fouX
               nd in PDS directory but without statistics!!!'
            ENDIF
            B     GET_DATE_DIRREC_NOMORE
         ELSE
            L     R5,8(,R3)
            N     R5,=X'0000001F'
            SLL   R5,1
            LA    R3,12(,R3)
            S     R4,=F'12'
            AR    R3,R5
            SR    R4,R5
            BC    B'0010',GET_DATE_NEXT_DIRREC_ENTRY
         ENDIF
*
GET_DATE_DIRREC_END_OF_BLOCK EQU *
         B     GET_DATE_GET_DIRREC
*
GET_DATE_DIRREC_NOMORE EQU *
*
         L     R1,G_DCB_MEM_PTR
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)
         CLOSE ((R2)),MODE=31
*
GET_DATE_DEALLOC EQU *
         LA    R6,DYNALLOC_AREA_GD
         USING S99RBP,R6
         LA    R4,S99RBPTR+4
         USING S99RB,R4
         ST    R4,S99RBPTR
         OI    S99RBPTR,S99RBPND
         XC    S99RB(S99RBEND-S99RB),S99RB
         MVI   S99RBLN,S99RBEND-S99RB
         MVI   S99VERB,S99VRBUN
         OI    S99FLG11,S99MSGL0
         LA    R5,S99RB+(S99RBEND-S99RB)+12
         MVC   0(CDSNTU_GD_L,R5),CDSNTU_GD
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)
         LA    R3,S99RB+(S99RBEND-S99RB)
         ST    R3,S99TXTPP
         O     R5,=X'80000000'
         ST    R5,0(,R3)
         LA    R1,DYNALLOC_AREA_GD
         DYNALLOC
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
         IF (CLI,G_DSFOUND,NE,C'Y') THEN
            MVC   G_SAVE_ALTER_DATE,=16X'FF'
         ENDIF
*
GET_DATE_STATS_RET EQU *
         BR    R8
*
* EODAD for DCBPDS
*
PDSDIR_IS_EOF_GD EQU *
         MVI   PDSDIR_EOF_GD,C'Y'
         BR    R6
*
         LTORG
*
* Translate table for conversion to hex
                            DS    0F
GETDATE_HEXTAB              EQU   *-C'0'
                            DC    C'0123456789ABCDEF'
*
TRT_ALPHANAT DS    0F A-Z $ # @
*                0 1 2 3 4 5 6 7 8 9 A B C D E F
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B
         DC    X'FF000000000000000000FFFFFFFFFFFF' C
         DC    X'FF000000000000000000FFFFFFFFFFFF' D
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F
*
TRT_ALPHANUMNATDASH DS    0F A-Z 0-9 $ # @ -
*                0 1 2 3 4 5 6 7 8 9 A B C D E F
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B
         DC    X'FF000000000000000000FFFFFFFFFFFF' C
         DC    X'FF000000000000000000FFFFFFFFFFFF' D
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E
         DC    X'00000000000000000000FFFFFFFFFFFF' F
*
CONST_CSIFIELD_GD DS 0F
         DC    CL44' '        CSIFILTK FILTER   KEY
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS
         DS    0CL16          CSIDTYPD ENTRY TYPES
         DC    CL16'                ' CSIDTYPS
         DS    0CL4           CSIOPTS  CSI OPTIONS
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' '
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' '
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' '
         DC    XL1'00'        CSIRESRV RESERVED
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME
CONST_CSIFIELD_GD_LEN EQU *-CONST_CSIFIELD_GD
*
CDSNTU_GD                   DC    AL2(DALDSNAM)
                            DC    X'0001'
                            DC    X'002C'
                            DC    CL44' '
CDSNTU_GD_L                 EQU   *-CDSNTU_GD
*
CSTATUSTU_GD                DC    AL2(DALSTATS)
                            DC    X'0001'
                            DC    X'0001'
                            DC    X'08'
CSTATUSTU_GD_L              EQU   *-CSTATUSTU_GD
*
CRETDDN_GD                  DC    AL2(DALRTDDN)
                            DC    X'0001'
                            DC    X'0008'
                            DC    CL8' '
CRETDDN_GD_L                EQU   *-CRETDDN_GD
*
CDCBPDS_DIR_GD              DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVDX
               =DA,DSORG=PS,RECFM=F,DCBE=CDCBEPDS_DIR_GD
LEN_DCBPDS_DIR_GD           EQU   *-CDCBPDS_DIR_GD
CDCBEPDS_DIR_GD             DCBE  EODAD=0,RMODE31=BUFF
LEN_DCBEPDS_DIR_GD          EQU   *-CDCBEPDS_DIR_GD
*
CDCBPDS_BDR                 DCB   MACRF=R,DSORG=PO,RECFM=U
LEN_DCBPDS_BDR              EQU   *-CDCBPDS_BDR
*
CONVTOD_L                   CONVTOD MF=L
CONVTOD_L_SIZ               EQU   *-CONVTOD_L
STCKCONV_L                  STCKCONV MF=L
STCKCONV_L_SIZ              EQU   *-STCKCONV_L
*
IDBBUF                      IEWBUFF FUNC=MAPBUF,TYPE=IDRB,VERSION=6,BYTX
               ES=2048
*
GET_DATE_DSECT              DSECT
                            DS    18F * My savearea
*
DAREAPTR_GD                 DS    A      DATA AREA POINTER (64K)
*
MODRSNRT_GD                 DS    0F
PARMRC_GD                   DS    0CL4
MODID_GD                    DS    CL2    MODULE ID
RSNCODE_GD                  DS    CL1    REASON CODE
RTNCODE_GD                  DS    CL1    RETURN CODE
*
CSIFIELD_GD                 DS    0F
                            ORG   *+CSIFIELD_DSECT_SIZ
CSIFIELD_GD_LEN             EQU   *-CSIFIELD_GD
*
PARMLIST_GD                 DS    0F
                            DS    A
                            DS    A
                            DS    A
*
                            DS    0F
CONVTOD_INAREA              DS    4F
CONVTOD_OUTAREA             DS    2F
CONVTOD_PLIST               DS    CL(CONVTOD_L_SIZ)
*
                            DS    0F
STCKCONV_OUTAREA            DS    CL16
STCKCONV_PLIST              DS    CL(STCKCONV_L_SIZ)
*
                            DS    0F
DATEWORK_DEC_1              DS    CL8
DATEWORK_DEC_2              DS    CL8
DATEWORK_ZON                DS    CL16
*
                            DS    0F
PDSDIR_EOF_GD               DS    C
*
                            DS    0F
DSCBPAR_GD                  DS    4F
OBTAIN_GD                   DS    0F
                            ORG   *+OBTAIN_DSECT_SIZ
*
DYNALLOC_AREA_GD            DS    0F
                            ORG   *+4
                            ORG   *+(S99RBEND-S99RB)
                            ORG   *+12
                            ORG   *+CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_X
               GD_L
*
                            DS    0F
DIRREC_GD                   DS    CL256
*
MEM8_GD                     DS    CL8
*
DAREA_GD                    DS    C
                            ORG   *+1023
DAREA_GD_SIZ                EQU   *-DAREA_GD
*
IEWBFDAT_SB_PAR4A           DS    4A
IEWBFDAT_SB_SB              DS    CL4
IEWBFDAT_SB_MTOKEN          DS    CL4
IEWBFDAT_SB_PGMNAME         DS    CL8
*
IEWBFDAT_GD_PAR8A           DS    8A
IEWBFDAT_GD_GD              DS    CL4
IEWBFDAT_GD_MTOKEN          DS    CL4
IEWBFDAT_GD_B_IDRB          DS    CL8
IEWBFDAT_GD_CURSOR          DS    F
IEWBFDAT_GD_COUNT           DS    F
*
IEWBFDAT_EN_PAR2A           DS    2A
IEWBFDAT_EN_EN              DS    CL4
IEWBFDAT_EN_MTOKEN          DS    CL4
*
GET_DATE_DSECT_SIZ          EQU   *-GET_DATE_DSECT
*
         IEFZB4D0
         IEFZB4D2
*
LWZMAKE  CSECT
*
* Get the member list of a data set
*
         DROP
*
LWZMAKE_GET_MEMLIST DS    0F
         STM   R14,R12,12(R13)   * Save callers registers
         LR    R10,R15
         LA    R11,4095(,R10)
         LA    R11,1(,R11)
         USING LWZMAKE_GET_MEMLIST,R10,R11
         GETMAIN RU,LV=GET_MEMLIST_DSECT_SIZ
         ST    R13,4(R1)         * Backward chain callers SA
         ST    R1,8(R13)         * Forward chain my SA
         LR    R13,R1            * Point R13 to my SA
         USING GET_MEMLIST_DSECT,R13 * Establish addressing of workarea
         USING GLOBAL_DATA_DSECT,R9
*
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_GET_MEMLIST'
*
         MVC   MEM8FILTER_LEN,=F'0'
         LT    R2,G_SCAN_TOKEN3_LEN
         IF (NZ) THEN
            C     R2,=F'8'
            IF (H) THEN
               L     R2,=F'8'
            ENDIF
            ST    R2,MEM8FILTER_LEN
            LA    R3,MEM8FILTER
            L     R4,G_SCAN_TOKEN3A
            BCTR  R2,R0
            B     *+10
            MVC   0(1,R3),0(R4)
            EX    R2,*-6
            MVC   G_SCAN_TOKEN3_LEN,=F'0'
         ENDIF
*
         L     R3,G_SCAN_TOKEN2A
         L     R4,G_SCAN_TOKEN2_LEN
         XR    R6,R6
         XR    R7,R7
*
GET_MEMLIST_TEST_QUAL1 EQU *
         LTR   R4,R4
         BZ    GET_MEMLIST_NOT_MVSDS
         TRT   0(1,R3),TRT_ALPHANAT_MEMLIST
         BNZ   GET_MEMLIST_NOT_MVSDS
         LA    R3,1(,R3)
         BCT   R4,*+8
         B     GET_MEMLIST_MVSDS
         LR    R5,R4
         C     R5,=F'7'
         IF (H) THEN
            L     R5,=F'7'
         ENDIF
         BCTR  R5,R0
         B     *+10
         TRT   0(1,R3),TRT_ALPHANUMNATDASH_MEMLIST
         EX    R5,*-6
         IF (Z) THEN
            LA    R5,1(,R5)
         ELSE
            LR    R5,R1
            SR    R5,R3
         ENDIF
         AR    R3,R5
         SR    R4,R5
         BZ    GET_MEMLIST_MVSDS
         IF (CLI,0(R3),EQ,C'.') THEN
            LA    R3,1(,R3)
            BCTR  R4,R0
            B     GET_MEMLIST_TEST_QUAL1
         ENDIF
         B     GET_MEMLIST_NOT_MVSDS
*
GET_MEMLIST_MVSDS EQU *
         BAL   R8,GET_MEMLIST_IGGCSI00
*
         CLC   G_RETCODE,=A(0)
         BNE   GET_MEMLIST_RET
*
         CLI   G_DSFOUND,C'Y'
         BNE   GET_MEMLIST_RET
*
         BAL   R8,GET_MEMLIST_OBTAIN
*
         CLC   G_RETCODE,=A(0)
         BNE   GET_MEMLIST_RET
*
         BAL   R8,GET_MEMLIST_MEMS
*
         B     GET_MEMLIST_RET
*
GET_MEMLIST_NOT_MVSDS EQU *
         MLWZMRPT RPTLINE=CL133'0Member list requested for non MVS fileX
               '
         MVC   G_RETCODE,=F'12'
*
GET_MEMLIST_RET EQU *
         L     R3,4(,R13)        * Restore address of callers SA
         FREEMAIN RU,LV=GET_MEMLIST_DSECT_SIZ,A=(R13)
         LR    R13,R3
         LM    R14,R12,12(R13)
         BR    R14                    Return to caller
*
* Perform catalog search with IGGCSI00
*
GET_MEMLIST_IGGCSI00 EQU *
         MVI   G_DSFOUND,C'N'
*
         LA    R1,DAREA_ML
         ST    R1,DAREAPTR_ML
         L     R2,=A(DAREA_ML_SIZ)
         ST    R2,0(,R1)
*
         LA    R2,CSIFIELD_ML
         L     R3,=A(CSIFIELD_ML_LEN)
         LA    R4,CONST_CSIFIELD_ML
         L     R5,=A(CONST_CSIFIELD_ML_LEN)
         MVCL  R2,R4
*
         LA    R2,CSIFIELD_ML+(CSIFILTK-CSIFIELD_DSECT)
         L     R3,G_SCAN_TOKEN2A
         L     R4,G_SCAN_TOKEN2_LEN
         LT    R5,G_MVSDS_MEMBER_PTR
         IF (NZ) THEN
            SR    R5,R3
            BCTR  R5,R0
            LR    R4,R5
         ENDIF
         C     R4,=A(L'CSIFILTK)
         IF (H)
            L     R4,=A(L'CSIFILTK)
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
*
         LA    R1,PARMLIST_ML
         LA    R2,MODRSNRT_ML
         ST    R2,0(R1)
         LA    R2,CSIFIELD_ML
         ST    R2,4(R1)
         L     R2,DAREAPTR_ML
         O     R2,=X'80000000'
         ST    R2,8(R1)
*
         L     R15,G_IGGCSI00A
         BASR  R14,R15
*
         C     R15,=F'4'
         IF (H) THEN
            MLWZMRPT RPTLINE=CL133'0Catalog search interface returned eX
               rror code'
            MVC   G_RETCODE,=F'12'
            BR    R8
         ENDIF
*
         LA    R1,DAREA_ML
         CLC   8(4,R1),=F'64'
         IF (H) THEN
            MVI   G_DSFOUND,C'Y'
         ENDIF
*
GET_MEMLIST_IGGCSI00_RET EQU *
         BR    R8
*
* Perform CAMLST OBTAIN
*
GET_MEMLIST_OBTAIN EQU *
         XR    R1,R1
         ICM   R1,B'1000',=AL1(193)
         ST    R1,DSCBPAR_ML
         MVC   OBTAIN_ML+(DS1DSNAM-OBTAIN_DSECT)(L'DS1DSNAM),CSIFIELD_MX
               L+(CSIFILTK-CSIFIELD_DSECT)
         LA    R1,OBTAIN_ML+(DS1DSNAM-OBTAIN_DSECT)
         ST    R1,DSCBPAR_ML+4
         LA    R1,DAREA_ML+110
         CLC   0(2,R1),=H'12'   * Is volume name present
         BL    GET_MEMLIST_OBTAIN_RET
         LA    R1,6(,R1)
         ST    R1,DSCBPAR_ML+8
         LA    R1,OBTAIN_ML+(DS1FMTID-OBTAIN_DSECT)
         ST    R1,DSCBPAR_ML+12
*
         OBTAIN DSCBPAR_ML
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0CAMLST OBTAIN returned error code'
            MVC   G_RETCODE,=F'12'
            BR    R8
         ENDIF
*
         IF (TM,OBTAIN_ML+(DS1DSORG-OBTAIN_DSECT),DS1DSGPO,Z) THEN
            MVC   G_LWZMRPT_LINE,=CL133'0Member list requested on non-PX
               DS dataset'
            LA    R2,CSIFIELD_ML
            MVC   G_LWZMRPT_LINE+37(L'CSIFILTK),CSIFILTK-CSIFIELD_DSECTX
               (R2)
            L     R15,G_LWZMAKE_RPTA
            BASR  R14,R15
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
GET_MEMLIST_OBTAIN_RET EQU *
         BR    R8
*
* Get the member list
*
GET_MEMLIST_MEMS EQU *
         LA    R6,DYNALLOC_AREA_ML
         USING S99RBP,R6
         LA    R4,S99RBPTR+4
         USING S99RB,R4
         ST    R4,S99RBPTR
         OI    S99RBPTR,S99RBPND
         XC    S99RB(S99RBEND-S99RB),S99RB
         MVI   S99RBLN,S99RBEND-S99RB
         MVI   S99VERB,S99VRBAL
         OI    S99FLG11,S99MSGL0
         LA    R5,S99RB+(S99RBEND-S99RB)+12
         MVC   0(CDSNTU_ML_L+CSTATUSTU_ML_L+CRETDDN_ML_L,R5),CDSNTU_ML
         MVC   6(44,R5),CSIFIELD_ML+(CSIFILTK-CSIFIELD_DSECT)
         LA    R3,S99RB+(S99RBEND-S99RB)
         ST    R3,S99TXTPP
         ST    R5,0(,R3)
         LA    R5,CDSNTU_ML_L(,R5)
         ST    R5,4(,R3)
         LA    R5,CSTATUSTU_ML_L(,R5)
         O     R5,=X'80000000'
         ST    R5,8(,R3)
         LA    R1,DYNALLOC_AREA_ML
         DYNALLOC
*
         DROP  R6
         DROP  R4
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
         L     R1,G_DCB_MEM_PTR
         MVC   DCBPDS_DIR-DCB_DSECT(LEN_DCBPDS_DIR_ML,R1),CDCBPDS_DIR_MX
               L
         MVC   DCBEPDS_DIR-DCB_DSECT(LEN_DCBEPDS_DIR_ML,R1),CDCBEPDS_DIX
               R_ML
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)
         LA    R3,DCBEPDS_DIR-DCB_DSECT(,R1)
         ST    R3,DCBDCBE-IHADCB(,R2)
         LA    R4,PDSDIR_IS_EOF_ML
         ST    R4,DCBEEODA-DCBE(,R3)
         L     R6,DYNALLOC_AREA_ML
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_ML_L+CSTATUSTU_ML_L(,R6)
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)
*
         MVI   PDSDIR_EOF_ML,C'N'
*
         LA    R6,GET_MEMLIST_OPENPDS
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)
GET_MEMLIST_OPENPDS EQU *
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0OPEN failed for reading PDS directoX
               ry'
            MVC   G_RETCODE,=F'8'
            B     GET_MEMLIST_DEALLOC
         ENDIF
*
GET_MEMLIST_GET_DIRREC EQU *
         L     R1,G_DCB_MEM_PTR
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)
         LA    R6,GET_MEMLIST_DIRREC_NOMORE
         GET   (R2),DIRREC_ML
*
         LA    R3,DIRREC_ML
         XR    R4,R4
         LH    R4,0(,R3)
         C     R4,=F'14'
         BL    GET_MEMLIST_DIRREC_END_OF_BLOCK
         LA    R3,2(,R3)
         S     R4,=F'2'
GET_MEMLIST_NEXT_DIRREC_ENTRY EQU *
         CLC   0(8,R3),=8X'FF'
         BE    GET_MEMLIST_DIRREC_NOMORE
         LT    R6,MEM8FILTER_LEN
         IF (NZ) THEN
            LA    R5,MEM8FILTER
            BCTR  R6,R0
            B     *+10
            CLC   0(1,R3),0(R5)
            EX    R6,*-6
            BNE   GET_MEMLIST_MEM_SKIP
         ENDIF
         L     R5,G_SCAN_TOKEN3A
         LT    R6,G_SCAN_TOKEN3_LEN
         IF (NZ) THEN
            LA    R6,9(,R6)
            C     R6,G_SCAN_TOKEN3_MAXLEN
            IF (H) THEN
               STM   R14,R12,GET_MEMLIST_SAVEAREA2+12
               L     R3,G_SCAN_TOKEN3_MAXLEN * Get current max len
               LR    R6,R3         * Save it for storage release
               SLL   R3,1          * Multiply max length by 2
               ST    R3,G_SCAN_TOKEN3_MAXLEN * Make it new max len
               STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block
               LR    R0,R1         * Have R0 point to new block
               L     R1,G_SCAN_TOKEN3_LEN * Get length of token 3
               L     R2,G_SCAN_TOKEN3A * Have R2 point to old block
               LR    R5,R2         * Save it for storage release
               LR    R3,R1         * Make sure no cropping/filling
               ST    R0,G_SCAN_TOKEN3A * Save ptr to new block
               MVCL  R0,R2            * Copy old to new block
               STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)
               LM    R14,R12,GET_MEMLIST_SAVEAREA2+12
               L     R5,G_SCAN_TOKEN3A
            ENDIF
            L     R6,G_SCAN_TOKEN3_LEN
            AR    R5,R6
            MVI   0(R5),C' '
            LA    R5,1(,R5)
            LA    R6,1(,R6)
            ST    R6,G_SCAN_TOKEN3_LEN
         ENDIF
         LR    R1,R3
         LA    R14,8
GET_MEMLIST_MEM_CHAR EQU *
         CLI   0(R1),C' '
         BE    GET_MEMLIST_MEM_DONE
         MVC   0(1,R5),0(R1)
         LA    R5,1(,R5)
         LA    R6,1(,R6)
         LA    R1,1(,R1)
         BCT   R14,GET_MEMLIST_MEM_CHAR
GET_MEMLIST_MEM_DONE EQU *
         ST    R6,G_SCAN_TOKEN3_LEN
GET_MEMLIST_MEM_SKIP EQU *
         L     R5,8(,R3)
         N     R5,=X'0000001F'
         SLL   R5,1
         LA    R3,12(,R3)
         S     R4,=F'12'
         AR    R3,R5
         SR    R4,R5
         BC    B'0010',GET_MEMLIST_NEXT_DIRREC_ENTRY
*
GET_MEMLIST_DIRREC_END_OF_BLOCK EQU *
         B     GET_MEMLIST_GET_DIRREC
*
GET_MEMLIST_DIRREC_NOMORE EQU *
*
         L     R1,G_DCB_MEM_PTR
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)
         CLOSE ((R2)),MODE=31
*
GET_MEMLIST_DEALLOC EQU *
         LA    R6,DYNALLOC_AREA_ML
         USING S99RBP,R6
         LA    R4,S99RBPTR+4
         USING S99RB,R4
         ST    R4,S99RBPTR
         OI    S99RBPTR,S99RBPND
         XC    S99RB(S99RBEND-S99RB),S99RB
         MVI   S99RBLN,S99RBEND-S99RB
         MVI   S99VERB,S99VRBUN
         OI    S99FLG11,S99MSGL0
         LA    R5,S99RB+(S99RBEND-S99RB)+12
         MVC   0(CDSNTU_ML_L,R5),CDSNTU_ML
         LA    R2,CSIFIELD_ML
         MVC   6(44,R5),CSIFILTK-CSIFIELD_DSECT(R2)
         LA    R3,S99RB+(S99RBEND-S99RB)
         ST    R3,S99TXTPP
         O     R5,=X'80000000'
         ST    R5,0(,R3)
         LA    R1,DYNALLOC_AREA_ML
         DYNALLOC
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'
            MVC   G_RETCODE,=F'8'
            BR    R8
         ENDIF
*
GET_MEMLIST_MEMS_RET EQU *
         BR    R8
*
* EODAD for DCBPDS
*
PDSDIR_IS_EOF_ML EQU *
         MVI   PDSDIR_EOF_ML,C'Y'
         BR    R6
*
         LTORG
*
TRT_ALPHANAT_MEMLIST DS    0F A-Z $ # @
*                0 1 2 3 4 5 6 7 8 9 A B C D E F
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B
         DC    X'FF000000000000000000FFFFFFFFFFFF' C
         DC    X'FF000000000000000000FFFFFFFFFFFF' D
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F
*
TRT_ALPHANUMNATDASH_MEMLIST DS    0F A-Z 0-9 $ # @ -
*                0 1 2 3 4 5 6 7 8 9 A B C D E F
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B
         DC    X'FF000000000000000000FFFFFFFFFFFF' C
         DC    X'FF000000000000000000FFFFFFFFFFFF' D
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E
         DC    X'00000000000000000000FFFFFFFFFFFF' F
*
CONST_CSIFIELD_ML DS 0F
         DC    CL44' '        CSIFILTK FILTER   KEY
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS
         DS    0CL16          CSIDTYPD ENTRY TYPES
         DC    CL16'                ' CSIDTYPS
         DS    0CL4           CSIOPTS  CSI OPTIONS
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' '
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' '
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' '
         DC    XL1'00'        CSIRESRV RESERVED
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME
CONST_CSIFIELD_ML_LEN EQU *-CONST_CSIFIELD_ML
*
CDSNTU_ML                   DC    AL2(DALDSNAM)
                            DC    X'0001'
                            DC    X'002C'
                            DC    CL44' '
CDSNTU_ML_L                 EQU   *-CDSNTU_ML
*
CSTATUSTU_ML                DC    AL2(DALSTATS)
                            DC    X'0001'
                            DC    X'0001'
                            DC    X'08'
CSTATUSTU_ML_L              EQU   *-CSTATUSTU_ML
*
CRETDDN_ML                  DC    AL2(DALRTDDN)
                            DC    X'0001'
                            DC    X'0008'
                            DC    CL8' '
CRETDDN_ML_L                EQU   *-CRETDDN_ML
*
CDCBPDS_DIR_ML              DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVDX
               =DA,DSORG=PS,RECFM=F,DCBE=CDCBEPDS_DIR_ML
LEN_DCBPDS_DIR_ML           EQU   *-CDCBPDS_DIR_ML
CDCBEPDS_DIR_ML             DCBE  EODAD=0,RMODE31=BUFF
LEN_DCBEPDS_DIR_ML          EQU   *-CDCBEPDS_DIR_ML
*
GET_MEMLIST_DSECT           DSECT
                            DS    18F * My savearea
GET_MEMLIST_SAVEAREA2       DS    18F
*
DAREAPTR_ML                 DS    A      DATA AREA POINTER (64K)
*
MODRSNRT_ML                 DS    0F
PARMRC_ML                   DS    0CL4
MODID_ML                    DS    CL2    MODULE ID
RSNCODE_ML                  DS    CL1    REASON CODE
RTNCODE_ML                  DS    CL1    RETURN CODE
*
CSIFIELD_ML                 DS    0F
                            ORG   *+CSIFIELD_DSECT_SIZ
CSIFIELD_ML_LEN             EQU   *-CSIFIELD_ML
*
PARMLIST_ML                 DS    0F
                            DS    A
                            DS    A
                            DS    A
*
                            DS    0F
PDSDIR_EOF_ML               DS    C
*
                            DS    0F
DSCBPAR_ML                  DS    4F
OBTAIN_ML                   DS    0F
                            ORG   *+OBTAIN_DSECT_SIZ
*
DYNALLOC_AREA_ML            DS    0F
                            ORG   *+4
                            ORG   *+(S99RBEND-S99RB)
                            ORG   *+12
                            ORG   *+CDSNTU_ML_L+CSTATUSTU_ML_L+CRETDDN_X
               ML_L
*
                            DS    0F
DIRREC_ML                   DS    CL256
*
MEM8_ML                     DS    CL8
MEM8FILTER_LEN              DS    F
MEM8FILTER                  DS    CL8
*
DAREA_ML                    DS    C
                            ORG   *+1023
DAREA_ML_SIZ                EQU   *-DAREA_ML
*
GET_MEMLIST_DSECT_SIZ       EQU   *-GET_MEMLIST_DSECT
*
LWZMAKE  CSECT
*
* Execute a REXX as a function that returns a string
*
         DROP
*
LWZMAKE_CALL_FUNC DS    0F
         STM   R14,R12,12(R13)   * Save callers registers
         LR    R10,R15
         LA    R11,4095(,R10)
         LA    R11,1(,R11)
         USING LWZMAKE_CALL_FUNC,R10,R11
         GETMAIN RU,LV=CALL_FUNC_DSECT_SIZ
         ST    R13,4(R1)         * Backward chain callers SA
         ST    R1,8(R13)         * Forward chain my SA
         LR    R13,R1            * Point R13 to my SA
         USING CALL_FUNC_DSECT,R13 * Establish addressing of workarea
         USING GLOBAL_DATA_DSECT,R9
*
*        Trace record to start section
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX
               KE_CALL_FUNC'
*
*        MVC   G_LWZMRPT_LINE,=CL133' '
*        MVC   G_LWZMRPT_LINE+1(4),G_SCAN_TOKEN2_LEN
*        L     R2,G_SCAN_TOKEN2A
*        MVC   G_LWZMRPT_LINE+6(80),0(R2)
*        L     R15,G_LWZMAKE_RPTA
*        BASR  R14,R15
*
*        MVC   G_LWZMRPT_LINE,=CL133' '
*        MVC   G_LWZMRPT_LINE+1(4),G_SCAN_TOKEN3_LEN
*        L     R2,G_SCAN_TOKEN3A
*        MVC   G_LWZMRPT_LINE+6(80),0(R2)
*        L     R15,G_LWZMAKE_RPTA
*        BASR  R14,R15
*
         IF (CLI,G_USE_ISPEXEC,EQ,C' ') THEN
            L     R15,LWZMAKE_IRXINITA_CALL_FUNC
            BASR  R14,R15
         ENDIF
*
         LA    R6,G_IRXEXEC_EXECBLK
         USING EXECBLK,R6
         MVC   EXEC_BLK_ACRYN,=CL8'IRXEXECB'
         LA    R5,EXECBLEN
         ST    R5,EXEC_BLK_LENGTH
         MVC   EXEC_MEMBER,=CL8' '
         LA    R2,EXEC_MEMBER
         L     R3,G_SCAN_TOKEN2A
         L     R4,G_SCAN_TOKEN2_LEN
         C     R4,=F'8'
         IF (H) THEN
            L     R4,=F'8'
         ENDIF
         BCTR  R4,R0
         B     *+10
         MVC   0(1,R2),0(R3)
         EX    R4,*-6
         MVC   EXEC_DDNAME,=CL8' '
         MVC   EXEC_SUBCOM,=CL8' '
         XR    R5,R5
         ST    R5,EXEC_BLK_LENGTH+4
         ST    R5,EXEC_DSNPTR
         ST    R5,EXEC_DSNLEN
         DROP  R6
*
         L     R6,G_EVALBLOCK_PTR
         USING EVALBLOCK,R6
         XR    R5,R5
         ST    R5,EVALBLOCK_EVPAD1
         ST    R5,EVALBLOCK_EVPAD2
         L     R5,G_EVALBLOCK_MAXLEN
         SRA   R5,3
         ST    R5,EVALBLOCK_EVSIZE
         DROP  R6
*
         LA    R1,G_IRXEXEC_EXECBLK
         ST    R1,G_IRXEXEC_EXECBLK_PTR
         CLC   G_SCAN_TOKEN3_LEN,=F'0'
         IF (NE) THEN
            MVC   G_IRXEXEC_ARGS(4),G_SCAN_TOKEN3A
            MVC   G_IRXEXEC_ARGS+4(4),G_SCAN_TOKEN3_LEN
            MVC   G_IRXEXEC_ARGS+8(8),=X'FFFFFFFFFFFFFFFF'
         ELSE
            MVC   G_IRXEXEC_ARGS,=X'FFFFFFFFFFFFFFFF'
         ENDIF
         LA    R1,G_IRXEXEC_ARGS
         ST    R1,G_IRXEXEC_ARGS_PTR
         MVC   G_IRXEXEC_FLAGS,=X'40000000'
         MVC   G_IRXEXEC_INSTBLK_PTR,=A(0)
         MVC   G_IRXEXEC_CPPL_PTR,=A(0)
         MVC   G_IRXEXEC_EVALBLK_PTR,G_EVALBLOCK_PTR
         MVC   G_IRXEXEC_WORKAREA_PTR,=A(0)
         MVC   G_IRXEXEC_USRFIELD_PTR,=X'8000000'
         MVC   G_IRXEXEC_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR
         LA    R1,G_IRXEXEC_REASON
         ST    R1,G_IRXEXEC_REASON_PTR
         XR    R0,R0
         LA    R1,G_IRXEXEC_EXECBLK_PTR
         ST    R1,G_IRXEXEC_PAR10A
         LA    R1,G_IRXEXEC_ARGS_PTR
         ST    R1,G_IRXEXEC_PAR10A+4
         LA    R1,G_IRXEXEC_FLAGS
         ST    R1,G_IRXEXEC_PAR10A+8
         LA    R1,G_IRXEXEC_INSTBLK_PTR
         ST    R1,G_IRXEXEC_PAR10A+12
         LA    R1,G_IRXEXEC_CPPL_PTR
         ST    R1,G_IRXEXEC_PAR10A+16
         LA    R1,G_IRXEXEC_EVALBLK_PTR
         ST    R1,G_IRXEXEC_PAR10A+20
         LA    R1,G_IRXEXEC_WORKAREA_PTR
         ST    R1,G_IRXEXEC_PAR10A+24
         LA    R1,G_IRXEXEC_USRFIELD_PTR
         ST    R1,G_IRXEXEC_PAR10A+28
         LA    R1,G_IRXEXEC_ENVBLOCK_PTR
         ST    R1,G_IRXEXEC_PAR10A+32
         LA    R1,G_IRXEXEC_REASON_PTR
         O     R1,=X'80000000'
         ST    R1,G_IRXEXEC_PAR10A+36
         LA    R1,G_IRXEXEC_PAR10A
*
         LINK  EP=IRXEXEC,SF=(E,G_LINKD)
*
         LTR   R15,R15
         IF (NZ) THEN
            MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'
            MVC   G_RETCODE,=F'12'
            B     CALL_FUNC_RET
         ENDIF
*
         MVC   G_IRXINIT_ENVBLOCK_PTR,G_IRXEXEC_ENVBLOCK_PTR
*
         L     R6,G_EVALBLOCK_PTR
         USING EVALBLOCK,R6
*
         LT    R2,EVALBLOCK_EVLEN
         IF (L) THEN
            X     R2,=X'FFFFFFFF'
            LA    R2,25(,R2)      * Add 1 plus room for 4A + 8
            STORAGE OBTAIN,LENGTH=(R2) * Allocate a mem block
            XR    R5,R5
            ST    R5,EVALBLOCK_EVPAD1-EVALBLOCK(,R1)
            ST    R5,EVALBLOCK_EVPAD2-EVALBLOCK(,R1)
            LR    R5,R2
            SRA   R5,3
            ST    R5,EVALBLOCK_EVSIZE-EVALBLOCK(,R1)
            LR    R3,R1
            L     R5,G_EVALBLOCK_MAXLEN
            STORAGE RELEASE,LENGTH=(R5),ADDR=(R6)
            ST    R3,G_EVALBLOCK_PTR
            ST    R2,G_EVALBLOCK_MAXLEN
            L     R6,G_EVALBLOCK_PTR
*
            MVC   G_IRXRLT_FUNCTION,=CL8'GETRLTE'
            ST    R6,G_IRXRLT_EVALBLK_PTR
            S     R2,=F'16'
            ST    R2,G_IRXRLT_EVALDATA_LEN
            MVC   G_IRXRLT_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR
            XR    R0,R0
            LA    R1,G_IRXRLT_FUNCTION
            ST    R1,G_IRXRLT_PAR5A
            LA    R1,G_IRXRLT_EVALBLK_PTR
            ST    R1,G_IRXRLT_PAR5A+4
            LA    R1,G_IRXRLT_EVALDATA_LEN
            ST    R1,G_IRXRLT_PAR5A+8
            LA    R1,G_IRXRLT_ENVBLOCK_PTR
            ST    R1,G_IRXRLT_PAR5A+12
            LA    R1,G_IRXRLT_REASON
            O     R1,=X'80000000'
            ST    R1,G_IRXRLT_PAR5A+16
            LA    R1,G_IRXRLT_PAR5A
*
            LINK  EP=IRXRLT,SF=(E,G_LINKD)
*
            LTR   R15,R15
            IF (NZ) THEN
               MLWZMRPT RPTLINE=CL133'0Error retrieving REXX return valX
               ue'
               MVC   G_RETCODE,=F'12'
               B     CALL_FUNC_RET
            ENDIF
         ENDIF
*
         L     R1,EVALBLOCK_EVLEN
         C     R1,G_SCAN_TOKEN3_MAXLEN
         IF (H) THEN
            L     R3,G_SCAN_TOKEN3_MAXLEN * Get current max len
            LR    R4,R3           * Save it for storage release
CALL_FUNC_ENLARGE_TOKEN EQU *
            SLL   R3,1            * Multiply max length by 2
            CR    R1,R3
            BH    CALL_FUNC_ENLARGE_TOKEN
            ST    R3,G_SCAN_TOKEN3_MAXLEN * Make it new max len
            STORAGE OBTAIN,LENGTH=(R3) * Allocate a mem block
            LR    R0,R1           * Have R0 point to new block
            L     R1,G_SCAN_TOKEN3_LEN * Get length of token
            L     R2,G_SCAN_TOKEN3A * Have R2 point to old blk
            LR    R5,R2           * Save it for storage release
            LR    R3,R1           * Make sure no cropping/filling
            ST    R0,G_SCAN_TOKEN3A * Save ptr to new block
            MVCL  R0,R2      * Copy old to new block
            STORAGE RELEASE,LENGTH=(R4),ADDR=(R5)
         ENDIF
         L     R0,G_SCAN_TOKEN3A
         L     R1,EVALBLOCK_EVLEN
         LA    R2,EVALBLOCK_EVDATA
         LR    R3,R1
         MVCL  R0,R2
         MVC   G_SCAN_TOKEN3_LEN,EVALBLOCK_EVLEN
*
         DROP  R6
*
CALL_FUNC_RET EQU *
         L     R3,4(,R13)        * Restore address of callers SA
         FREEMAIN RU,LV=CALL_FUNC_DSECT_SIZ,A=(R13)
         LR    R13,R3
         LM    R14,R12,12(R13)
         BR    R14                    Return to caller
*
         LTORG
*
LWZMAKE_IRXINITA_CALL_FUNC  DC    A(LWZMAKE_IRXINIT)
*
CALL_FUNC_DSECT             DSECT
CALL_FUNC_SA                DS    18F
CALL_FUNC_DSECT_SIZ         EQU *-CALL_FUNC_DSECT
*
LWZMAKE  CSECT
*
*
*
LWZMAKE_IRXINIT MLWZSAVE
*
         MVI   G_USE_ISPEXEC,C'N'
*
         MVC   G_IRXINIT_FUNCTION,=CL8'FINDENVB'
         MVC   G_IRXINIT_PARMMOD,=CL8' '
         MVC   G_IRXINIT_INSTORPARM_PTR,=A(0)
         MVC   G_IRXINIT_USRFIELD_PTR,=X'80000000'
         MVC   G_IRXINIT_RESERVED_PTR,=A(0)
         MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)
         MVC   G_IRXINIT_REASON,=A(0)
*
         XR    R0,R0
         LA    R1,G_IRXINIT_FUNCTION
         ST    R1,G_IRXINIT_PAR7A
         LA    R1,G_IRXINIT_PARMMOD
         ST    R1,G_IRXINIT_PAR7A+4
         LA    R1,G_IRXINIT_INSTORPARM_PTR
         ST    R1,G_IRXINIT_PAR7A+8
         LA    R1,G_IRXINIT_USRFIELD_PTR
         ST    R1,G_IRXINIT_PAR7A+12
         LA    R1,G_IRXINIT_RESERVED_PTR
         ST    R1,G_IRXINIT_PAR7A+16
         LA    R1,G_IRXINIT_ENVBLOCK_PTR
         ST    R1,G_IRXINIT_PAR7A+20
         LA    R1,G_IRXINIT_REASON
         O     R1,=X'80000000'
         ST    R1,G_IRXINIT_PAR7A+24
         LA    R1,G_IRXINIT_PAR7A
*
         LINK  EP=IRXINIT,SF=(E,G_LINKD)
*
         C     R15,=A(0)
         BE    IRXINIT_OK
         C     R15,=A(4)
         BE    IRXINIT_OK
         C     R15,=A(28)
         BE    IRXINIT_OK
         MLWZMRPT RPTLINE=CL133'0Error finding REXX environment'
         MVC   G_RETCODE,=F'12'
         B     RET_IRXINIT
IRXINIT_OK EQU *
*
         C     R15,=A(28)
         IF (NE) THEN
            L     R2,G_IRXINIT_ENVBLOCK_PTR
            L     R2,16(,R2)
            L     R2,20(,R2)
            L     R3,8(,R2)
            L     R4,12(,R2)
            L     R2,0(,R2)
FIND_ISPEXEC EQU  *
            CLC   0(8,R2),=CL8'ISPEXEC'
            BE    ISPEXEC_FOUND
            AR    R2,R4
            BCT   R3,FIND_ISPEXEC
            B     ISPEXEC_NOT_FOUND
ISPEXEC_FOUND EQU *
            MVI   G_USE_ISPEXEC,C'Y'
ISPEXEC_NOT_FOUND EQU *
         ENDIF
*
RET_IRXINIT EQU  *
         MLWZTERM                 * Return back to caller
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         END   LWZMAKE
