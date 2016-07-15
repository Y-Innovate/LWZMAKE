         TITLE 'LWZMAKE'                                                00010000
*********************************************************************** 00020000
* LL       WW      WW   ZZZZZZ   MM    MM    AAAAA    KK  KK   EEEEEE * 00030000
* LL       WW      WW      ZZ    MMMMMMMM   AA   AA   KK KK    EE     * 00040000
* LL        WW WW WW      ZZ     MM MM MM   AAAAAAA   KKKK     EEEE   * 00050000
* LL        WWWWWWWW     ZZ      MM    MM   AA   AA   KK KK    EE     * 00060000
* LLLLLL     WW  WW     ZZZZZZ   MM    MM   AA   AA   KK  KK   EEEEEE * 00070000
*                                                                     * 00080000
* =================================================================== * 00090000
* Description:                                                        * 00100000
* ------------------------------------------------------------------- * 00110000
* LWZMAKE is a build tool for z/OS that executes a zmake makefile.    * 00120000
* A zmake makefile is a script that defines targets (build output),   * 00130000
* their dependencies and the recipes (small script functions within   * 00140000
* the zmake makefile) to build those targets, very similar to make.   * 00150000
* The two major differences with make are 1) that LWZMAKE can handle  * 00160000
* both MVS and USS files, and 2) that LWZMAKE doesn't start USS       * 00170000
* commandline commands, but instead allows you to start a REXX with   * 00180000
* a parameter string (which could in turn start a USS command).       * 00190000
*                                                                     * 00200000
* A zmake makefile is processed in 2 phases.                          * 00210000
* Phase 1 parses the input and creates a linked list of statements    * 00220000
* in an internal format. While doing so, variable assignments with := * 00230000
* are resolved then, assignments with = are left to be resolved in    * 00240000
* phase 2. Similar for targets, whatever's left of : in a rule is     * 00250000
* resolved then, the rest is left to be resolved in phase 2.          * 00260000
* Phase 2 starts with the first target and for each dependency it     * 00270000
* recursively goes through any target it finds for those dependencies.* 00280000
* When all dependency targets are processed and LWZMAKE determines a  * 00290000
* target should be built, the recipe is executed.                     * 00300000
*                                                                     * 00310000
* As stated above, during both phases variable substitution takes     * 00320000
* place. This is also done recursively, in other words when a variable* 00330000
* is resolved to a value that also contains a variable, LWZMAKE keeps * 00340000
* resolving until no variables are found.                             * 00350000
*                                                                     * 00360000
* For remembering variables and their values an ordered linked list   * 00370000
* is used.                                                            * 00380000
*                                                                     * 00390000
* Parsing is done with a scanner section that reads the makefile      * 00400000
* character by character. This section is invoked by a tokenizer or   * 00410000
* lexer section that strings the scanned characters into tokens. This * 00420000
* section in turn is invoked by a statement parsing section that      * 00430000
* analyzes tokens that form statements, converts the statements into  * 00440000
* an in-memory internal format and saves the statements in a linked   * 00450000
* list.                                                               * 00460000
* For the tokenizer and parser to 'know' what is allowed at any given * 00470000
* point a 'state machine' is used. This is basically a stack of state * 00480000
* bytes. Every state byte corresponds with a fullword with bit        * 00490000
* positions that indicate whether a token is allowed or not.          * 00500000
*                                                                     * 00510000
* The input to the scanner (and thus also to the tokenizer and parser)* 00520000
* is also stacked. Initially this stack has one entry which represents* 00530000
* the makefile which was taken from the JCL. Next entries are pushed  * 00540000
* onto the input stack when for example the next characters need to   * 00550000
* temporarily be taken from a variable's value. After the variable's  * 00560000
* value is exhausted, the stack is popped and the scanner continues   * 00570000
* with the makefile input.                                            * 00580000
*                                                                     * 00590000
* =================================================================== * 00600000
* Module specifications:                                              * 00610000
* ------------------------------------------------------------------- * 00620000
* - Fully re-entrant                                                  * 00630000
* - AMODE 31                                                          * 00640000
* - RMODE any                                                         * 00650000
*                                                                     * 00660000
* =================================================================== * 00670000
* Required DD names:                                                  * 00680000
* ------------------------------------------------------------------- * 00690000
* - SYSEXEC   for finding REXX members                                * 00700000
* - SYSTSPRT  for REXX SAY output                                     * 00710000
* - LWZMRPT   for execution report                                    * 00720000
* - LWZMTRC   for trace output                                        * 00730000
* - MAKEFILE  input makefile script                                   * 00740000
*                                                                     * 00750000
* =================================================================== * 00760000
* Source conventions:                                                 * 00770000
* ------------------------------------------------------------------- * 00780000
* Basically everything's in this single source.                       * 00790000
* Major sections use standard MVS savearea chaining, within such      * 00800000
* sections, smaller subroutines are BAL'd to with R8.                 * 00810000
* Code is not LE-conforming (so no CEEENTRY - CEETERM), but does      * 00820000
* adhere to LE-enabled register conventions.                          * 00830000
*                                                                     * 00840000
* =================================================================== * 00850000
* Register conventions:                                               * 00860000
* ------------------------------------------------------------------- * 00870000
* R0   work reg                                                       * 00880000
* R1   work reg, parameter list ptr on entry, used as parameter list  * 00890000
*        ptr to call external routines                                * 00900000
* R2-7 work reg (sometimes R7 is also return address in BAL)          * 00910000
* R8   return address in BAL to subroutine                            * 00920000
* R9   USING GLOBAL_DATA_DSECT for 'global' data                      * 00930000
* R10  first base register                                            * 00940000
* R11  second base register                                           * 00950000
* R12  not used in order to be LE enabled                             * 00960000
* R13  points to save area and working storage                        * 00970000
* R14  standard linkage convention return address                     * 00980000
* R15  standard linkage convention entry address + return value       * 00990000
*                                                                     * 01000000
*********************************************************************** 01010000
*                                                                       01020000
         COPY  ASMMSP             * Enable HLASM struct.prog.macro's    01030000
*                                                                       01040000
* Macro MLWZMTRC for generating a snippet to write a trace record       01050000
*                                                                       01060000
         MACRO                                                          01070000
         MLWZMTRC &LEVEL=,&MSGNR=,&CONST=,&DATA                         01080000
         LCLA  &CONST_LEN                                               01090000
         IF (CLI,G_LWZMAKE_TRACE,NL,&LEVEL) THEN                        01100000
.* Check for constant data provided                                     01110000
         AIF   ('&CONST' EQ '').NOCONST                                 01120000
            LA    R15,=&CONST                                           01130000
            ST    R15,G_LWZMTRC_DATA_PTR                                01140000
&CONST_LEN  SETA  K'&CONST-3                                            01150000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(&CONST_LEN)                   01160000
         AGO   .WITHDATA                                                01170000
.NOCONST ANOP                                                           01180000
.* Check for presence of DATA keyword, if not present clear data size   01190000
         AIF   ('&DATA' NE '').WITHDATA                                 01200000
            XC    G_LWZMTRC_DATA_SIZ,G_LWZMTRC_DATA_SIZ * data siz = 0  01210000
.WITHDATA ANOP                                                          01220000
            MVC   G_LWZMTRC_MSGNR,=&MSGNR * Copy msg number to glb var  01230000
            L     R15,G_LWZMAKE_TRACEA  * Get address of trace section  01240000
            BASR  R14,R15               * Link to trace section         01250000
         ENDIF                                                          01260000
         MEND                                                           01270000
*                                                                       01280000
* Macro MLWZMRPT for generating a snippet to write a report line        01290000
*                                                                       01300000
         MACRO                                                          01310000
         MLWZMRPT &RPTLINE=,&APND_LC=                                   01320000
         MVC   G_LWZMRPT_LINE,=&RPTLINE * Copy report line to glb var   01330000
         AIF   ('&APND_LC' EQ '').NO_APND_LC                            01340000
         MVI   G_LWZMRPT_APND_LC,&APND_LC                               01350000
.NO_APND_LC ANOP                                                        01360000
         L     R15,G_LWZMAKE_RPTA * Get address of report section       01370000
         BASR  R14,R15            * Link to report section              01380000
         MEND                                                           01390000
*                                                                       01400000
* Macro MLWZSAVE for generating simple start of section                 01410000
*                                                                       01420000
         MACRO                                                          01430000
&NAME    MLWZSAVE                                                       01440000
*                                                                       01450000
* Make sure we start without any usings                                 01460000
         DROP                                                           01470000
*                                                                       01480000
&NAME    DS    0F                                                       01490000
         STM   R14,R12,12(R13)    * Save callers registers              01500000
         LR    R10,R15            * Setup R11 as base register          01510000
         LA    R11,4095(,R10)     * Setup R10 as second                 01520000
         LA    R11,1(,R11)        *   base register                     01530000
         USING &NAME,R10,R11      * Establish addressing                01540000
         GETMAIN RU,LV=72         * Get storage for SA                  01550000
         XC    0(2,R1),0(R1)      * Clear first 2 bytes of SA           01560000
         ST    R13,4(R1)          * Backward chain callers SA           01570000
         ST    R1,8(R13)          * Forward chain my SA                 01580000
         LR    R13,R1             * Point R13 to my SA                  01590000
         USING GLOBAL_DATA_DSECT,R9 * Establish addressing of glb data  01600000
         MEND                                                           01610000
*                                                                       01620000
* Macro MLWZTERM for generating simple end of section                   01630000
*                                                                       01640000
         MACRO                                                          01650000
         MLWZTERM                                                       01660000
         L     R3,4(,R13)         * Save address of callers SA          01670000
         FREEMAIN RU,LV=72,A=(R13) * Free storage for SA                01680000
         LR    R13,R3             * Restore address of callers SA       01690000
         LM    R14,R12,12(R13)    * Restore reg 14 through 12           01700000
         BR    R14                * Return to caller                    01710000
         MEND                                                           01720000
*********************************************************************** 01730000
* Section: LWZMAKE                                                    * 01740000
* Purpose: Main section                                               * 01750000
*********************************************************************** 01760000
LWZMAKE  AMODE 31                                                       01770000
LWZMAKE  RMODE ANY                                                      01780000
LWZMAKE  CSECT                                                          01790000
         J     LWZMAKE_ENTRY      * Jump past name/date/time/version    01800000
         DC    CL8'LWZMAKE'       * Module name                         01810000
         DC    CL8'&SYSDATE'      * Assembly date                       01820000
         DC    CL8'&SYSTIME'      * Assembly time                       01830000
LWZMAKE_ENTRY DS    0H                                                  01840000
         STM   R14,R12,12(R13)    * Save callers registers              01850000
         LARL  R10,LWZMAKE_ENTRY  * Set R10 to entry point              01860000
         LA    R11,4095(,R10)     * Setup R11 as second using           01870000
         LA    R11,1(,R11)        *   base register                     01880000
         USING LWZMAKE_ENTRY,R10,R11 * Establish addressing             01890000
         LR    R2,R1              * Save possible parameter ptr         01900000
         GETMAIN RU,LV=WORKAREA_SIZ * Get storage for SA+work stg+glb   01910000
         XC    0(2,R1),0(R1)      * Clear first 2 bytes of SA           01920000
         ST    R13,4(R1)          * Backward chain callers SA           01930000
         ST    R1,8(R13)          * Forward chain my SA                 01940000
         LR    R13,R1             * Point R13 to my SA                  01950000
         USING WORKAREA,R13       * Establish addressing of workarea    01960000
         LR    R1,R2              * Restore parameter list ptr in R1    01970000
                                                                        01980000
         ST    R1,PARML_PTR       * Save address of parameter ptrs      01990000
*                                                                       02000000
         LA    R9,GLOBAL_DATA     * Global storage was GM'd along with  02010000
*                                 * SA and working storage,             02020000
         USING GLOBAL_DATA_DSECT,R9 * Establish address of global data  02030000
*                                                                       02040000
         BAL   R8,INIT            * Perform initializations, LOAD       02050000
*                                 * external modules, OPEN DCB's        02060000
*                                                                       02070000
         LT    R15,G_RETCODE      * Check for error                     02080000
         BNZ   RET                * If error then skip to end           02090000
*                                                                       02100000
         BAL   R8,PARMS           * Perform parsing parameters          02110000
*                                                                       02120000
         LT    R15,G_RETCODE      * Check for error                     02130000
         BNZ   RET                * If error then skip to end           02140000
*                                                                       02150000
         L     R15,LWZMAKE_PHASE1A * Get address of phase 1 section     02160000
         BASR  R14,R15            * Link to phase 1 section             02170000
*                                                                       02180000
         LT    R15,G_RETCODE      * Check for error                     02190000
         BNZ   RET                * If error thenk skip to end          02200000
*                                                                       02210000
         L     R15,LWZMAKE_PHASE2A * Get address of phase 2 section     02220000
         BASR  R14,R15            * Link to phase 2 section             02230000
*                                                                       02240000
RET      EQU   *                                                        02250000
*                                                                       02260000
         BAL   R8,WRAPUP          * Perform wrapup                      02270000
*                                                                       02280000
         L     R2,G_RETCODE       * Save returncode                     02290000
         L     R3,4(,R13)         * Save address of callers SA          02300000
         FREEMAIN RU,LV=WORKAREA_SIZ,A=(R13) * Free workarea            02310000
         L     R0,=A(0)           * Load modifier (is an LE thing)      02320000
         LR    R13,R3             * Restore address of callers SA       02330000
         LR    R15,R2             * Set returncode                      02340000
         L     R14,12(,R13)       * Reload return address               02350000
         LM    R1,R12,24(R13)     * Restore reg 1 through 12            02360000
         BR    R14                * Return to caller                    02370000
*                                                                       02380000
* Initializations                                                       02390000
*                                                                       02400000
INIT     EQU   *                                                        02410000
*        Set trace level, for a different trace level change here       02420000
         MVI   G_LWZMAKE_TRACE,LWZMAKE_TRACE_WARNING                    02430000
*                                                                       02440000
         MVC   G_RETCODE,=F'0'    * Initial return code 0               02450000
*                                                                       02460000
         MVI   TRCOPEN,C'N'       * Flags for open DCB's trace,         02470000
         MVI   RPTOPEN,C'N'       *                      report         02480000
         MVI   MKFOPEN,C'N'       *                  and makefile       02490000
*                                                                       02500000
         MVI   G_MKFEOF,C'N'      * Preset EOF for makefile to N        02510000
*                                                                       02520000
         MVI   G_RECIPEPREFIX,X'05' * Default recipe prefix is tab      02530000
*                                                                       02540000
         MVI   G_PREV_STMT_TYPE,X'00' * Initial no prev statement type  02550000
*                                                                       02560000
         MVC   G_DCB_MEM_PTR,=A(0) * Clear DCB memory pointer           02570000
*                                                                       02580000
         MVC   G_STMT_LIST_PTR,=A(0) * Clear linked list pointers       02590000
         MVC   G_FIRST_VAR_PTR,=A(0)                                    02600000
         MVC   G_FIRST_TGT_PTR,=A(0)                                    02610000
         MVC   G_FIRST_PNY_PTR,=A(0)                                    02620000
*                                                                       02630000
         MVI   G_DEFAULT_TARGET,C' ' * Clear default target             02640000
         MVC   G_DEFAULT_TARGET+1(L'G_DEFAULT_TARGET-1),G_DEFAULT_TARGEX02650000
               T                                                        02660000
*                                                                       02670000
         MVC   G_SCAN_CURRCOL,=F'0'   '                                 02680000
         MVC   G_SCAN_CURRLINE,=F'0'     * so current line goes 0 > 1   02690000
         MVI   G_SCAN_CONTINUED_LINE,C'N' * Initial no continued line   02700000
         MVI   G_SCAN_APPEND_TO,X'00'    * Initial append to scratch    02710000
*                                        * token (1)                    02720000
*                                                                       02730000
         MVI   G_SCAN_STATE_STACK,X'00'                                 02740000
         MVC   G_SCAN_STATE_STACK+1(L'G_SCAN_STATE_STACK-1),G_SCAN_STATX02750000
               E_STACK                                                  02760000
*                                 * Clear scan state stack index        02770000
         MVI   G_SCAN_STATE_STACK_IDX,X'00'                             02780000
*                                                                       02790000
*                                 * Clear scan input stack              02800000
         MVI   G_SCAN_INPUT_STACK,X'00'                                 02810000
         MVC   G_SCAN_INPUT_STACK+1(L'G_SCAN_INPUT_STACK-1),G_SCAN_INPUX02820000
               T_STACK                                                  02830000
*                                 * Set initial inputstack              02840000
         MVI   G_SCAN_INPUT_STACK_IDX,X'01'                             02850000
*                                                                       02860000
         MVC   G_LWZMRPT_CURRLINE,=H'999' * Make sure first report line 02870000
*                                         * causes next line            02880000
         MVC   G_LWZMRPT_CURRPAGE,=F'0'   * so current page goes 0 > 1  02890000
         MVC   G_LWZMRPT_APND_LC,=C'N'    * Initial no append line/col  02900000
*                                                                       02910000
*                                         * Initialize page hader in    02920000
         MVC   G_PAGE_HEADER,CPAGE_HEADER * global var                  02930000
*                                                                       02940000
         MVC   LOADD,LOADL        * Copy list form of LOAD macro        02950000
*                                                                       02960000
         MVC   G_LINKD,LINKL      * Copy list form of LINK macro        02970000
*                                                                       02980000
         MVC   G_OPEND,OPENL      * Copy list form of LOAD macro        02990000
*                                                                       03000000
*        Save addresses of sections in global vars                      03010000
         MVC   G_LWZMAKE_TRACEA,LWZMAKE_TRACEA                          03020000
         MVC   G_LWZMAKE_RPTA,LWZMAKE_RPTA                              03030000
*                                                                       03040000
*        Clear REXX environment block pointer                           03050000
         MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)                             03060000
*                                                                       03070000
*        Allocate multi-purpose tokens                                  03080000
         GETMAIN RU,LV=SCAN_TOKEN_MAXLEN                                03090000
         ST    R1,G_SCAN_TOKENA                                         03100000
         MVC   G_SCAN_TOKEN_MAXLEN,=A(SCAN_TOKEN_MAXLEN)                03110000
         GETMAIN RU,LV=SCAN_TOKEN_MAXLEN                                03120000
         ST    R1,G_SCAN_TOKEN2A                                        03130000
         MVC   G_SCAN_TOKEN2_MAXLEN,=A(SCAN_TOKEN_MAXLEN)               03140000
         GETMAIN RU,LV=SCAN_TOKEN_MAXLEN                                03150000
         ST    R1,G_SCAN_TOKEN3A                                        03160000
         MVC   G_SCAN_TOKEN3_MAXLEN,=A(SCAN_TOKEN_MAXLEN)               03170000
*                                                                       03180000
*                                 * GM DCB storage below the line       03190000
         GETMAIN RU,LV=DCB_DSECT_SIZ,LOC=24                             03200000
         ST    R1,G_DCB_MEM_PTR   * and save in global var              03210000
*                                                                       03220000
*        Overwrite DCB's with hard coded ones (for re-entrancy)         03230000
         MVC   DCBTRC-DCB_DSECT(LEN_DCBTRC,R1),CDCBTRC                  03240000
         MVC   DCBRPT-DCB_DSECT(LEN_DCBRPT,R1),CDCBRPT                  03250000
         MVC   DCBMKF-DCB_DSECT(LEN_DCBMKF,R1),CDCBMKF                  03260000
         MVC   DCBEMKF-DCB_DSECT(LEN_DCBEMKF,R1),CDCBEMKF               03270000
*                                                                       03280000
         LA    R7,DCBMKF-DCB_DSECT(,R1)  * Point R7 to makefile DCB     03290000
         LA    R6,DCBEMKF-DCB_DSECT(,R1) * Point R6 to makefile DCBE    03300000
         ST    R6,DCBDCBE-IHADCB(,R7)    * Store ptr to DCBE in DCB     03310000
         LA    R1,MAKEFILE_IS_EOF        * Get address of EODAD routine 03320000
         ST    R1,DCBEEODA-DCBE(,R6)     * Store address in DCBE        03330000
*                                                                       03340000
*                                       * Load catalog search interface 03350000
         LOAD  EP=IGGCSI00,SF=(E,LOADD) * entry point IGGCSI00          03360000
*        No error handling, missing module will cause S806 abend        03370000
*                                                                       03380000
         ST    R0,G_IGGCSI00A     * and store in global var             03390000
*                                                                       03400000
*                                       * Load binder fast data access  03410000
         LOAD  EP=IEWBFDAT,SF=(E,LOADD) * entry point IEWBFDAT          03420000
*        No error handling, missing module will cause S806 abend        03430000
*                                                                       03440000
         ST    R0,G_IEWBFDATA     * and store in global var             03450000
*                                                                       03460000
*        OPEN the DCB's                                                 03470000
*                                                                       03480000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              03490000
         LA    R2,DCBTRC-DCB_DSECT(,R14) * Get address of LWZMTRC DCB   03500000
*                                 * and open for output                 03510000
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,G_OPEND)                     03520000
         LTR   R15,R15            * Check for returned 0                03530000
         IF (NZ) THEN             * non-zero means open failed          03540000
            CVD   R15,G_DEC8      * convert return value to packed      03550000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       03560000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    03570000
*                                                                       03580000
*           No other way of informing about error than doing a WTO      03590000
            MVC   G_WTOLEN,=H'26'                                       03600000
            MVC   G_WTOFIL,=H'0'                                        03610000
            MVC   G_WTOTEXT(20),=C'OPEN LWZMTRC FAILED '                03620000
            MVC   G_WTOTEXT+20(2),G_ZONED8+6                            03630000
            WTO   MF=(E,G_WTOBLOCK),ROUTCDE=11,DESC=7                   03640000
*                                                                       03650000
            MVC   G_RETCODE,=A(12) * Set return code to 12              03660000
            B     INIT_RET        * and skip rest of INIT               03670000
         ENDIF                                                          03680000
         MVI   TRCOPEN,C'Y'       * Remember to close trace DCB         03690000
*        Trace record LWZMAKE start                                     03700000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'601'                 03710000
*                                                                       03720000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              03730000
         LA    R2,DCBRPT-DCB_DSECT(,R14) * Get address of LWZMRPT DCB   03740000
*                                 * and open for output                 03750000
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,G_OPEND)                     03760000
         LTR   R15,R15            * Check for returned 0                03770000
         IF (NZ) THEN             * non-zero means open failed          03780000
            CVD   R15,G_DEC8      * convert return value to packed      03790000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       03800000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    03810000
            MVC   G_HELPER_DATA(2),G_ZONED8+6 * Copy to helper data     03820000
            LA    R14,G_HELPER_DATA  * Get ptr to helper data           03830000
            ST    R14,G_LWZMTRC_DATA_PTR * Save trace data ptr          03840000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(2) * Set trace data size      03850000
*           Trace record LWZMRPT DCB OPEN failed                        03860000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'001',DATA        03870000
            MVC   G_RETCODE,=F'12' * Set return code to 12              03880000
            B     INIT_RET        * and skip rest of INIT               03890000
         ENDIF                                                          03900000
         MVI   RPTOPEN,C'Y'       * Remember to close report DCB        03910000
*        Trace record LWZMRPT DCB open                                  03920000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'602',CONST=C'LWZMRPTX03930000
               '                                                        03940000
*        Report line LWZMAKE start                                      03950000
         MLWZMRPT RPTLINE=CL133' LWZMAKE start'                         03960000
*                                                                       03970000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              03980000
         LA    R2,DCBMKF-DCB_DSECT(,R14) * Get address of MAKEFILE DCB  03990000
*                                 * and open for input                  04000000
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                      04010000
         LTR   R15,R15            * Check for returned 0                04020000
         IF (NZ) THEN             * non-zero means open failed          04030000
            CVD   R15,G_DEC8      * convert return value to packed      04040000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       04050000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    04060000
            MVC   G_HELPER_DATA(2),G_ZONED8+6 * Copy to helper data     04070000
            LA    R14,G_HELPER_DATA  * Get ptr to helper data           04080000
            ST    R14,G_LWZMTRC_DATA_PTR * Save trace data ptr          04090000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(2) * Set trace data size      04100000
*           Trace record MAKEFILE DCB OPEN failed                       04110000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'002',DATA        04120000
            MVC   G_LWZMRPT_LINE,=CL133'0Error opening MAKEFILE'        04130000
            L     R15,G_LWZMAKE_RPTA * Get address of report section    04140000
            BASR  R14,R15          * Link to report section             04150000
            MVC   G_RETCODE,=F'12' * Set return code to 12              04160000
            B     INIT_RET        * and skip rest of INIT               04170000
         ENDIF                                                          04180000
         MVI   MKFOPEN,C'Y'       * Remember to close MAKEFILE DCB      04190000
*        Trace record MAKEFILE DCB open                                 04200000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'602',CONST=C'MAKEFILX04210000
               E'                                                       04220000
*        Report line MAKEFILE opened                                    04230000
         MLWZMRPT RPTLINE=CL133' MAKEFILE DD opened'                    04240000
*                                                                       04250000
*        Retrieve submitter user id                                     04260000
         IAZXJSAB READ,USERID=G_USERID                                  04270000
*                                                                       04280000
INIT_RET EQU   *                  * INIT done                           04290000
         BR    R8                 * Return                              04300000
*                                                                       04310000
* Parse parameters                                                      04320000
*                                                                       04330000
PARMS    EQU   *                                                        04340000
         LT    R1,PARML_PTR       * Retrieve parameter list pointer     04350000
         IF (Z) THEN              * Empty pointer?                      04360000
NO_PARAMETER EQU *                                                      04370000
            MLWZMRPT RPTLINE=CL133' No parameter received'              04380000
            B     PARMS_DONE                                            04390000
         ENDIF                                                          04400000
*                                                                       04410000
         L     R1,0(,R1)          * Get pointer to first parameter      04420000
         N     R1,=X'7FFFFFFF'    * Get rid of high order bit           04430000
*                                                                       04440000
         XR    R3,R3              * Clear R3                            04450000
         LH    R3,0(,R1)          * Get JCL parameter length            04460000
         LTR   R3,R3              * Check for zero                      04470000
         BZ    NO_PARAMETER       * Zero length parameter               04480000
         LA    R2,2(,R1)          * Point R2 to parameter               04490000
*                                                                       04500000
         LR    R4,R3                                                    04510000
         C     R4,=A(L'G_HELPER_DATA)                                   04520000
         IF (H) THEN                                                    04530000
            L     R4,=A(L'G_HELPER_DATA)                                04540000
         ENDIF                                                          04550000
         BCTR  R4,R0                                                    04560000
         B     *+10                                                     04570000
         MVC   G_HELPER_DATA(1),0(R2)                                   04580000
         EX    R4,*-6                                                   04590000
         LA    R4,1(,R4)                                                04600000
         ST    R2,G_LWZMTRC_DATA_PTR                                    04610000
         STH   R4,G_LWZMTRC_DATA_SIZ                                    04620000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'606',DATA            04630000
*                                                                       04640000
*        Split up parts of the parameter                                04650000
*                                                                       04660000
*        Start by putting parameter in input stack                      04670000
         XR    R4,R4              * Clear R4                            04680000
         XR    R5,R5              * Clear R5                            04690000
         IC    R5,G_SCAN_INPUT_STACK_IDX * Get current stack index      04700000
         C     R5,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra        04710000
*                                 * entry fit?                          04720000
         IF (NL) THEN             * If not write error                  04730000
            MLWZMRPT RPTLINE=CL133'0Internal error, state stack overfloX04740000
               w'                                                       04750000
            MVC   G_RETCODE,=F'12' * Set return code 12                 04760000
            B     PARMS_RET        * and return                         04770000
         ENDIF                                                          04780000
         LA    R5,1(,R5)          * Add 1 to stack size                 04790000
         STC   R5,G_SCAN_INPUT_STACK_IDX * And store it                 04800000
         BCTR  R5,R0              * Subtract 1 to calculate offset      04810000
         M     R4,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry    04820000
         LA    R4,G_SCAN_INPUT_STACK * Point R4 to input stack          04830000
         AR    R4,R5              * Add calculated offset               04840000
*                                                                       04850000
         USING INPUT_DSECT,R4     * Address with INPUT DSECT            04860000
*                                                                       04870000
         MVC   INPUTLEAD,=H'0'    * Clear leading spaces                04880000
         MVI   INPUTTYPE,X'02'    * Set type of input to ptr to string  04890000
         STH   R3,INPUTLEN        * Copy value length                   04900000
         ST    R2,INPUTPTR        * Copy value pointer                  04910000
         MVC   INPUTPOS,=H'0'     * Set initial scan position to start  04920000
*                                                                       04930000
         DROP  R4                                                       04940000
*                                                                       04950000
NEXT_PARMS_ROUND EQU *                                                  04960000
         L     R15,LWZMAKE_SCAN_TOKENA * Get address of SCAN_TOKEN      04970000
         BASR  R14,R15            * Link to SCAN_TOKEN section          04980000
*                                                                       04990000
         L     R14,G_SCAN_TOKENA  * Point R14 to token 1                05000000
         CLI   0(R14),X'00'       * Check if empty token was returned   05010000
         BE    PARMS_DONE         * Because that means done             05020000
*                                                                       05030000
         CLC   G_SCAN_TOKEN_LEN,=F'1' * Any valid parameter starts with 05040000
         BNE   PARMS_ERROR        * a '-' switch which has to be 1 long 05050000
*                                                                       05060000
         IF (CLI,0(R14),EQ,C'-') THEN * Check if it's a '-'             05070000
            L     R15,LWZMAKE_SCAN_TOKENA * Get address of SCAN_TOKEN   05080000
            BASR  R14,R15         * Link to SCAN_TOKEN section          05090000
*                                                                       05100000
            L     R14,G_SCAN_TOKENA * Point R14 to token 1              05110000
            CLI   0(R14),X'00'    * Check if empty token returned       05120000
            BE    PARMS_ERROR     * If so, then error                   05130000
*                                                                       05140000
            CLC   G_SCAN_TOKEN_LEN,=F'1' * Switch has to be 1 char      05150000
            BNE   PARMS_ERROR     * If not, then error                  05160000
*                                                                       05170000
            IF (CLI,0(R14),EQ,C't') THEN * Check for target sw?         05180000
               L     R15,LWZMAKE_SCAN_TOKENA * Get address SCAN_TOKEN   05190000
               BASR  R14,R15      * Link to SCAN_TOKEN section          05200000
*                                                                       05210000
               L     R14,G_SCAN_TOKENA * Point R14 to token 1           05220000
               CLI   0(R14),X'00' * Check if empty token returned       05230000
               BE    PARMS_ERROR  * If so, then error                   05240000
*                                                                       05250000
*              Double check length is between 1 and 72, if not error    05260000
               CLC   G_SCAN_TOKEN_LEN,=F'0'                             05270000
               BE    PARMS_ERROR                                        05280000
               CLC   G_SCAN_TOKEN_LEN,=F'72'                            05290000
               BH    PARMS_ERROR                                        05300000
*                                                                       05310000
*              * Check if normal token was returned                     05320000
               IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN  05330000
*                 Everything checks out, so copy to default target      05340000
                  LA    R2,G_DEFAULT_TARGET * Point R2 to default tgt   05350000
                  L     R3,G_SCAN_TOKENA    * Point R3 to token 1       05360000
                  L     R4,G_SCAN_TOKEN_LEN * Get length of target in 1 05370000
                  BCTR  R4,R0         * R4 = R4 - 1 for EX              05380000
                  B     *+10          * Skip MVC constant for EX        05390000
                  MVC   0(1,R2),0(R3) * MVC constant for EX             05400000
                  EX    R4,*-6        * EX previous MVC stmt with R4    05410000
*                                                                       05420000
                  CLI   G_SCAN_INPUT_STACK_IDX,X'01' * Check if we're   05430000
*                                        * still in parameter input     05440000
                  BNE   NEXT_PARMS_ROUND * If so, go for next parm      05450000
               ELSE                 * Else, not token type normal       05460000
                  B     PARMS_ERROR * means error                       05470000
               ENDIF                                                    05480000
            ELSE                  * Else, it's not a -t switch          05490000
               B     PARMS_ERROR  * means error                         05500000
            ENDIF                                                       05510000
         ELSE                     * Else, it's not a '-' switch char    05520000
            B     PARMS_ERROR     * means error                         05530000
         ENDIF                                                          05540000
*                                                                       05550000
PARMS_DONE EQU  *                                                       05560000
         MVC   G_SCAN_CURRCOL,=F'99999'  * Make sure first scanned char 05570000
*                                        * causes read record           05580000
PARMS_RET EQU  *                                                        05590000
         BR    R8                 * Return                              05600000
*                                                                       05610000
PARMS_ERROR EQU *                                                       05620000
         MLWZMRPT RPTLINE=CL133'0Wrong parameter received'              05630000
         MVC   G_RETCODE,=F'8'                                          05640000
         BR    R8                                                       05650000
*                                                                       05660000
* Wrap things up                                                        05670000
*                                                                       05680000
WRAPUP   EQU   *                                                        05690000
         LT    R3,G_STMT_LIST_PTR * Get first stmt in list              05700000
         DO WHILE=(NZ)                                                  05710000
            ST    R3,G_DEC8       * Put ptr in area of at least 5 bytes 05720000
            UNPK  G_ZONED8(9),G_DEC8(5)      * Turn into almost hex     05730000
            TR    G_ZONED8,MAIN_HEXTAB       * Turn into hex            05740000
            MVC   G_HELPER_DATA(8),G_ZONED8  * Copy hex to helper data  05750000
            LA    R14,G_HELPER_DATA          * Get ptr to helper data   05760000
            ST    R14,G_LWZMTRC_DATA_PTR     * Save it for trace data   05770000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace data length 8      05780000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'641',DATA        05790000
            L     R4,STMT_NEXT_PTR-STMT_DSECT(,R3) * Save ptr next stmt 05800000
            L     R2,STMT_LEN-STMT_DSECT(,R3)     * Get length of block 05810000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * and free it         05820000
            LTR   R3,R4           * Test pointer to next statement      05830000
         ENDDO                                                          05840000
         MVC   G_STMT_LIST_PTR,=A(0) * Clear first block ptr            05850000
*                                                                       05860000
         IF (CLI,MKFOPEN,EQ,C'Y') THEN      * Was MAKEFILE opened?      05870000
            L     R14,G_DCB_MEM_PTR         * Get DCB memory pointer    05880000
            LA    R2,DCBMKF-DCB_DSECT(,R14) * Get addr of MAKEFILE DCB  05890000
            CLOSE ((R2)),MODE=31            * and close it              05900000
*           Trace record MAKEFILE DCB closed                            05910000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'603',CONST=C'MAKEX05920000
               FILE'                                                    05930000
*           Report line MAKEFILE closed                                 05940000
            MLWZMRPT RPTLINE=CL133' MAKEFILE DD closed'                 05950000
         ENDIF                                                          05960000
*                                                                       05970000
         IF (CLI,RPTOPEN,EQ,C'Y') THEN * Was LWZMRPT opened?            05980000
            IF (CLC,G_RETCODE,NE,=F'0') THEN * Did an error occur?      05990000
               MLWZMRPT RPTLINE=CL133'0Error occurred, see LWZMTRC for X06000000
               more details.'                                           06010000
            ENDIF                                                       06020000
*           Write last report line                                      06030000
            MLWZMRPT RPTLINE=CL133'0LWZMAKE end'                        06040000
            L     R14,G_DCB_MEM_PTR         * Get DCB memory pointer    06050000
            LA    R2,DCBRPT-DCB_DSECT(,R14) * Get addr of LWZMRPT DCB   06060000
            CLOSE ((R2)),MODE=31            * and close it              06070000
*           Trace record LWZMRPT DCB closed                             06080000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_INFO,MSGNR=C'603',CONST=C'LWZMX06090000
               RPT'                                                     06100000
         ENDIF                                                          06110000
*                                                                       06120000
         IF (CLI,TRCOPEN,EQ,C'Y') THEN * Was LWZMTRC opened?            06130000
            L     R14,G_DCB_MEM_PTR         * Get DCB memory pointer    06140000
            LA    R2,DCBTRC-DCB_DSECT(,R14) * Get addr of LWZMTRC DCB   06150000
            CLOSE ((R2)),MODE=31            * and close it              06160000
         ENDIF                                                          06170000
*                                                                       06180000
         IF (CLC,G_DCB_MEM_PTR,NE,=A(0)) THEN * Was DCB memory alloc'd? 06190000
            L     R1,G_DCB_MEM_PTR    * Get DCB memory pointer          06200000
            FREEMAIN RU,LV=DCB_DSECT_SIZ,A=(R1) * and free it           06210000
            MVC   G_DCB_MEM_PTR,=A(0) * Clear DCB memory pointer        06220000
         ENDIF                                                          06230000
*                                                                       06240000
         BR    R8                 * Return                              06250000
*                                                                       06260000
* EODAD for MAKEFILE                                                    06270000
*                                                                       06280000
MAKEFILE_IS_EOF EQU *                                                   06290000
         MVI   G_MKFEOF,C'Y'      * Set MAKEFILE EOF to true            06300000
*        Pop input stack                                                06310000
         XR    R14,R14                                                  06320000
         IC    R14,G_SCAN_INPUT_STACK_IDX                               06330000
         BCTR  R14,R0                                                   06340000
         STC   R14,G_SCAN_INPUT_STACK_IDX                               06350000
*        R7 was set to instruction following GET macro                  06360000
         BR    R7                                                       06370000
*                                                                       06380000
* End of code of main section                                           06390000
*                                                                       06400000
         LTORG                                                          06410000
*                                                                       06420000
* Report header line template                                           06430000
CPAGE_HEADER DC CL133'1Light Weight LWZMAKE utility                    X06440000
                                    Date DD-MM-YYYY    Time HH:MM:SS   X06450000
                Page 00000000'                                          06460000
*                                                                       06470000
* Translate table for conversion to hex                                 06480000
                            DS    0F                                    06490000
MAIN_HEXTAB                 EQU   *-C'0'                                06500000
                            DC    C'0123456789ABCDEF'                   06510000
*                                                                       06520000
* Table of scan state full words of flag bits.                          06530000
* Every scan state is an index of this table, every entry in this       06540000
* table contains up to 32 flag bits indicating valid token types        06550000
* for that given scan state.                                            06560000
SCAN_STATE_TABLE            DS    0F                                    06570000
                            DC    AL4(SCAN_EXPECTED_NEWSTMT)            06580000
                            DC    AL4(SCAN_EXPECTED_NEWSTMT2)           06590000
                            DC    AL4(SCAN_EXPECTED_ASSIGN)             06600000
                            DC    AL4(SCAN_EXPECTED_ASSIGN2)            06610000
                            DC    AL4(SCAN_EXPECTED_VARIABLE)           06620000
                            DC    AL4(SCAN_EXPECTED_VARIABLER)          06630000
                            DC    AL4(SCAN_EXPECTED_VARIABLE2)          06640000
                            DC    AL4(SCAN_EXPECTED_RULE)               06650000
                            DC    AL4(SCAN_EXPECTED_RULE2)              06660000
                            DC    AL4(SCAN_EXPECTED_RULE3)              06670000
                            DC    AL4(SCAN_EXPECTED_CALL)               06680000
                            DC    AL4(SCAN_EXPECTED_CALL2)              06690000
                            DC    AL4(SCAN_EXPECTED_EXPAND)             06700000
                            DC    AL4(SCAN_EXPECTED_PHONY)              06710000
                            DC    AL4(SCAN_EXPECTED_PHONY2)             06720000
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME)         06730000
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME2)        06740000
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME3)        06750000
                            DC    AL4(SCAN_EXPECTED_ADDPDSNAME4)        06760000
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST)         06770000
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST2)        06780000
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST3)        06790003
                            DC    AL4(SCAN_EXPECTED_MEMBERLIST4)        06800003
*                                                                       06810000
* Local constant pointers to section addresses                          06820000
LWZMAKE_TRACEA              DC    A(LWZMAKE_TRACE)                      06830000
LWZMAKE_RPTA                DC    A(LWZMAKE_RPT)                        06840000
LWZMAKE_SCAN_TOKENA         DC    A(LWZMAKE_SCAN_TOKEN)                 06850000
LWZMAKE_PHASE1A             DC    A(LWZMAKE_PHASE1)                     06860000
LWZMAKE_PHASE2A             DC    A(LWZMAKE_PHASE2)                     06870000
*                                                                       06880000
* Constant list form of LOAD macro                                      06890000
                            DS    0F                                    06900000
LOADL                       LOAD  SF=L                                  06910000
LOAD_SIZ                    EQU   *-LOADL                               06920000
*                                                                       06930000
* Constant list form of LINK macro                                      06940000
                            DS    0F                                    06950000
LINKL                       LINK  SF=L                                  06960000
LINK_SIZ                    EQU   *-LINKL                               06970000
*                                                                       06980000
* Constant list form of OPEN macro                                      06990000
                            DS    0F                                    07000000
OPENL                       OPEN  (,),MODE=31,MF=L                      07010000
OPEN_SIZ                    EQU   *-OPENL                               07020000
*                                                                       07030000
* Constant DCB for LWZMTRC                                              07040000
CDCBTRC                     DCB   DDNAME=LWZMTRC,LRECL=133,MACRF=(PM),RX07050000
               ECFM=FBA,DSORG=PS                                        07060000
LEN_DCBTRC                  EQU   *-CDCBTRC                             07070000
*                                                                       07080000
* Constant DCB for LWZMRPT                                              07090000
CDCBRPT                     DCB   DDNAME=LWZMRPT,LRECL=133,MACRF=(PM),RX07100000
               ECFM=FBA,DSORG=PS                                        07110000
LEN_DCBRPT                  EQU   *-CDCBRPT                             07120000
*                                                                       07130000
* Constant DCB for MAKEFILE                                             07140000
CDCBMKF                     DCB   DDNAME=MAKEFILE,LRECL=80,MACRF=(GM),RX07150000
               ECFM=FB,DSORG=PS,DCBE=CDCBEMKF                           07160000
LEN_DCBMKF                  EQU   *-CDCBMKF                             07170000
*                                                                       07180000
* Constant DCBE for MAKEFILE                                            07190000
CDCBEMKF                    DCBE  EODAD=0,RMODE31=BUFF                  07200000
LEN_DCBEMKF                 EQU   *-CDCBEMKF                            07210000
*                                                                       07220000
* DSECT for SA, main section working storage, which also contains the   07230000
* area for global data. This DSECT is pointed to by R13, global data    07240000
* is pointed to by R9.                                                  07250000
*                                                                       07260000
WORKAREA                    DSECT                                       07270000
                            DS    18F * My savearea                     07280000
PARML_PTR                   DS    A   * Contents of R1 on entry         07290000
*                                                                       07300000
* Working storage list form of LOAD macro                               07310000
                            DS    0F                                    07320000
LOADD                       DS    CL(LOAD_SIZ)                          07330000
*                                                                       07340000
* Flags indicating DCB's open or not                                    07350000
                            DS    0F                                    07360000
TRCOPEN                     DS    C   * LWZMTRC  open flag              07370000
RPTOPEN                     DS    C   * LWZMRPT  open flag              07380000
MKFOPEN                     DS    C   * MAKEFILE open flag              07390000
*                                                                       07400000
* Area for global data, pointed to by R9                                07410000
                            DS    0F                                    07420000
GLOBAL_DATA                 DS    CL(GLOBAL_DATA_SIZ)                   07430000
*                                                                       07440000
WORKAREA_SIZ                EQU   *-WORKAREA                            07450000
*                                                                       07460000
* DSECT for global data pointed to by R9, global vars start with G_     07470000
* for visibility                                                        07480000
*                                                                       07490000
GLOBAL_DATA_DSECT           DSECT                                       07500000
*                                                                       07510000
* LWZMAKE return code                                                   07520000
G_RETCODE                   DS    F                                     07530000
*                                                                       07540000
* Pointers to section address                                           07550000
                            DS    0F                                    07560000
G_LWZMAKE_TRACEA            DS    A                                     07570000
G_LWZMAKE_RPTA              DS    A                                     07580000
*                                                                       07590000
* Working storage list form of OPEN macro                               07600000
                            DS    0F                                    07610000
G_OPEND                     DS    CL(OPEN_SIZ)                          07620000
*                                                                       07630000
* Working storage list form of LINK macro                               07640000
                            DS    0F                                    07650000
G_LINKD                     DS    CL(LINK_SIZ)                          07660000
*                                                                       07670000
* Pointer to 24-bit block of storage for DCB's, DCB_DSECT for overlay   07680000
                            DS    0F                                    07690000
G_DCB_MEM_PTR               DS    A                                     07700000
*                                                                       07710000
* Userid executing current task                                         07720000
G_USERID                    DS    CL8                                   07730000
*                                                                       07740000
* Helper for converting CALL to uppercase                               07750000
G_CALL4                     DS    CL4                                   07760000
*                                                                       07770000
* EOF flag for MAKEFILE                                                 07780000
G_MKFEOF                    DS    C                                     07790000
*                                                                       07800000
* RECIPEPREFIX, initialized to X'05', can be set in MAKEFILE script     07810000
G_RECIPEPREFIX              DS    C                                     07820000
*                                                                       07830000
* LWZMAKE trace level                                                   07840000
G_LWZMAKE_TRACE             DS    C                                     07850000
LWZMAKE_TRACE_NONE          EQU   C'0'                                  07860000
LWZMAKE_TRACE_ERROR         EQU   C'1'                                  07870000
LWZMAKE_TRACE_WARNING       EQU   C'2'                                  07880000
LWZMAKE_TRACE_INFO          EQU   C'3'                                  07890000
LWZMAKE_TRACE_DEBUG         EQU   C'4'                                  07900000
LWZMAKE_TRACE_DEEBUG        EQU   C'6'                                  07910000
LWZMAKE_TRACE_DEEEBUG       EQU   C'8'                                  07920000
*                                                                       07930000
* Trace message number '000' - '999'                                    07940000
G_LWZMTRC_MSGNR             DS    CL3                                   07950000
*                                                                       07960000
* Trace record                                                          07970000
G_LWZMTRC_RECORD            DS    CL133                                 07980000
*                                                                       07990000
* Trace record data pointer and size                                    08000000
                            DS    0F                                    08010000
G_LWZMTRC_DATA_PTR          DS    A                                     08020000
G_LWZMTRC_DATA_SIZ          DS    H                                     08030000
*                                                                       08040000
* Catalog search interface IGGCSI00 external function address           08050000
                            DS    0F                                    08060000
G_IGGCSI00A                 DS    A                                     08070000
*                                                                       08080000
* Pointer and length of member name in MVS data set string              08090000
G_MVSDS_MEMBER_PTR          DS    A                                     08100000
G_MVSDS_MEMBER_LEN          DS    F                                     08110000
*                                                                       08120000
* Binder fast data access external function address                     08130000
                            DS    0F                                    08140000
G_IEWBFDATA                 DS    A                                     08150000
*                                                                       08160000
* REXX execute IRXINIT parameters                                       08170000
G_IRXINIT_PAR7A             DS    10A                                   08180000
G_IRXINIT_FUNCTION          DS    CL8                                   08190000
G_IRXINIT_PARMMOD           DS    CL8                                   08200000
G_IRXINIT_INSTORPARM_PTR    DS    CL4                                   08210000
G_IRXINIT_USRFIELD_PTR      DS    CL4                                   08220000
G_IRXINIT_RESERVED_PTR      DS    CL4                                   08230000
G_IRXINIT_ENVBLOCK_PTR      DS    CL4                                   08240000
G_IRXINIT_REASON            DS    CL4                                   08250000
*                                                                       08260000
* REXX maintain entries host command env table IRXSUBCM parameters      08270000
G_IRXSUBCM_PAR5A            DS    5A                                    08280000
G_IRXSUBCM_FUNCTION         DS    CL8                                   08290000
G_IRXSUBCM_STRING           DS    CL32                                  08300000
G_IRXSUBCM_STRING_LEN       DS    CL4                                   08310000
G_IRXSUBCM_HOSTENV_NAME     DS    CL8                                   08320000
G_IRXSUBCM_ENVBLOCK_PTR     DS    CL4                                   08330000
*                                                                       08340000
* REXX execute ISPEXEC parameters                                       08350000
G_USE_ISPEXEC               DS    C                                     08360000
                            DS    0F                                    08370000
G_ISPEXEC_PAR2A             DS    2A                                    08380000
*                                                                       08390000
* REXX execute IRXEXEC parameters                                       08400000
G_IRXEXEC_PAR10A            DS    10A                                   08410000
G_IRXEXEC_EXECBLK_PTR       DS    CL4                                   08420000
G_IRXEXEC_ARGS_PTR          DS    CL4                                   08430000
G_IRXEXEC_FLAGS             DS    CL4                                   08440000
G_IRXEXEC_INSTBLK_PTR       DS    CL4                                   08450000
G_IRXEXEC_CPPL_PTR          DS    CL4                                   08460000
G_IRXEXEC_EVALBLK_PTR       DS    CL4                                   08470000
G_IRXEXEC_WORKAREA_PTR      DS    CL4                                   08480000
G_IRXEXEC_USRFIELD_PTR      DS    CL4                                   08490000
G_IRXEXEC_ENVBLOCK_PTR      DS    CL4                                   08500000
G_IRXEXEC_REASON_PTR        DS    CL4                                   08510000
*                                                                       08520000
* REXX exec block                                                       08530000
                            DS    0F                                    08540000
G_IRXEXEC_EXECBLK           DS    CL(EXECBLEN)                          08550000
*                                                                       08560000
* REXX exec arguments, 4 byte pointer + 4 byte length + 8 byte X'FF'    08570000
                            DS    0F                                    08580000
G_IRXEXEC_ARGS              DS    CL16                                  08590000
*                                                                       08600000
* REXX evaluation block                                                 08610000
G_IRXEXEC_EVALBLK           DS    0F                                    08620000
                            DS    4F                                    08630000
                            DS    CL256                                 08640000
EVALBLK_SIZ                 EQU   *-G_IRXEXEC_EVALBLK                   08650000
*                                                                       08660000
* REXX execute IRXEXEC reason code                                      08670000
                            DS    0F                                    08680000
G_IRXEXEC_REASON            DS    CL4                                   08690000
*                                                                       08700000
* Starting address of statement pointer block linked list               08710000
* This linked list links 4K blocks, each containing (possibly) 1022     08720000
* pointers to any STMT_*_DSECT area, first pointer is a backward chain, 08730000
* 1024th pointer is a forward chain                                     08740000
                            DS    0F                                    08750000
G_STMT_LIST_PTR             DS    A                                     08760000
*                                                                       08770000
* Pointer to previous statement used to chain stmts in linked list      08780000
G_STMT_SAVE_PTR             DS    A                                     08790000
*                                                                       08800000
* Statement operator ('=' or ':=' in assigment, ':' in rule)            08810000
G_STMT_SAVE_OP              DS    CL2                                   08820000
*                                                                       08830000
* Previous statement type ('A'ssignment, 'R'ule or 'C'all)              08840000
G_PREV_STMT_TYPE            DS    C                                     08850000
*                                                                       08860000
* Switch (Y/N) indicating previous statement was in recipe              08870000
G_PREV_STMT_IN_RECIPE       DS    C                                     08880000
*                                                                       08890000
* Parameters and return pointer for LWZMAKE_ALLOC_STMT section          08900000
G_STMT_ALLOC_LEN            DS    F    * size of memory to allocate     08910000
G_STMT_ALLOC_TYPE           DS    C    * statement type                 08920000
                            DS    CL3  * reserved                       08930000
G_STMT_ALLOC_RETURN_PTR     DS    A    * returned ptr to alloc'd memory 08940000
*                                                                       08950000
* Scanner/tokenizer/parser variables                                    08960000
G_SCAN_CURRLINE             DS    F    * Current line                   08970000
G_SCAN_CURRCOL              DS    F    * Current column within line     08980000
G_SCAN_SPACE_COUNT          DS    F    * Spaces since last token        08990000
G_SAVE_SPACE_COUNT          DS    F    * Place to remember space count  09000000
G_SCAN_CURRCHAR             DS    C    * Scanner current character      09010000
G_SCAN_PEEKCHAR             DS    C    * Scanner next character         09020000
G_SCAN_NEWLINE              DS    C    * Switch ind new line            09030000
G_SCAN_CONTINUED_LINE       DS    C    * Switch ind continued line (\)  09040000
G_SCAN_TOKENTYPE            DS    C    * Token type for G_SCAN_TOKEN    09050000
G_SCAN_TOKENTYPE2           DS    C    * Token type for G_SCAN_TOKEN2   09060000
G_SCAN_TOKENTYPE3           DS    C    * Token type for G_SCAN_TOKEN3   09070000
SCAN_TOKENTYPE_IGNORE       EQU   C'I' * Anything beyond pos 72         09080000
SCAN_TOKENTYPE_COMMENT      EQU   C'#' * Anything after #               09090000
SCAN_TOKENTYPE_NORMAL       EQU   C'A' * Any word pos 1 A-Z a-z         09100000
SCAN_TOKENTYPE_NUMBER       EQU   C'N' * Any work containing only 0-9   09110000
SCAN_TOKENTYPE_OPERATOR     EQU   C'=' * Any operator (:= =)            09120000
SCAN_TOKENTYPE_CONTINUATION EQU   C'\' * Line continuation char \       09130000
SCAN_TOKENTYPE_RULE         EQU   C':' * Rule separator :               09140000
SCAN_TOKENTYPE_SPECIAL      EQU   C'.' * Any special variable           09150000
SCAN_TOKENTYPE_VARIABLE     EQU   C'$' * The word $(                    09160000
SCAN_TOKENTYPE_CLOSEBRACKET EQU   C')' * Closing bracket char )         09170000
SCAN_TOKENTYPE_CALL         EQU   C'C' * The word CALL                  09180000
SCAN_TOKENTYPE_ACRO         EQU   C'@' * The word $@                    09190000
SCAN_TOKENTYPE_PERCENT      EQU   C'%' * The word $%                    09200000
SCAN_TOKENTYPE_RECIPEPREFIX EQU   X'05' * Pos 1 if it's equal to RPREF  09210000
SCAN_TOKENTYPE_COMMA        EQU   C',' * Comma character ,              09220000
G_SCAN_APPEND_TO            DS    C    * Which G_SCAN_TOKEN* to append  09230000
*                                      * to when scanning $() variable  09240000
G_SCAN_CLOSE_BRACKET        DS    C    * Save ) or } to check matching  09250000
*                                      * with ( or {                    09260000
G_SCAN_VAR_PRESERVE_SPACES  DS    C    * Preserve all spaces or just 1  09270000
*                                                                       09280000
* Stack of scan state bytes, highest entry is the last state before     09290000
* the one in G_SCAN_STATE                                               09300000
                            DS    0F                                    09310000
G_SCAN_STATE_STACK          DS    CL128                                 09320000
*                                                                       09330000
* The current scan state byte which is an index on SCAN_STATE_TABLE     09340000
G_SCAN_STATE                DS    C    1                                09350000
SCAN_STATE_NOT_IN_STMT      EQU   X'00'                                 09360000
SCAN_STATE_IN_STMT          EQU   X'01'                                 09370000
SCAN_STATE_IN_ASSIGN        EQU   X'02'                                 09380000
SCAN_STATE_IN_ASSIGN2       EQU   X'03'                                 09390000
SCAN_STATE_IN_VARIABLE      EQU   X'04'                                 09400000
SCAN_STATE_IN_VARIABLER     EQU   X'05'                                 09410000
SCAN_STATE_IN_VARIABLE2     EQU   X'06'                                 09420000
SCAN_STATE_IN_RULE          EQU   X'07'                                 09430000
SCAN_STATE_IN_RULE2         EQU   X'08'                                 09440000
SCAN_STATE_IN_RULE3         EQU   X'09'                                 09450000
SCAN_STATE_IN_CALL          EQU   X'0A'                                 09460000
SCAN_STATE_IN_CALL2         EQU   X'0B'                                 09470000
SCAN_STATE_IN_EXPAND        EQU   X'0C'                                 09480000
SCAN_STATE_IN_PHONY         EQU   X'0D'                                 09490000
SCAN_STATE_IN_PHONY2        EQU   X'0E'                                 09500000
SCAN_STATE_IN_ADDPDSNAME    EQU   X'0F'                                 09510000
SCAN_STATE_IN_ADDPDSNAME2   EQU   X'10'                                 09520000
SCAN_STATE_IN_ADDPDSNAME3   EQU   X'11'                                 09530000
SCAN_STATE_IN_ADDPDSNAME4   EQU   X'12'                                 09540000
SCAN_STATE_IN_MEMBERLIST    EQU   X'13'                                 09550000
SCAN_STATE_IN_MEMBERLIST2   EQU   X'14'                                 09560000
SCAN_STATE_IN_MEMBERLIST3   EQU   X'15'                                 09570003
SCAN_STATE_IN_MEMBERLIST4   EQU   X'16'                                 09580003
SCAN_STATE_IN_RECIPE        EQU   X'80'                                 09590000
                            DS    C    2                                09600000
*                                                                       09610000
* Current stack size, current scan state in G_SCAN_STATE, last scan     09620000
* state pushed on stack at G_SCAN_STATE_STACK_IDX                       09630000
G_SCAN_STATE_STACK_IDX      DS    C    3                                09640000
                            DS    C    4                                09650000
*                                                                       09660000
* Working field to contain the entry in SCAN_STATE_TABLE indexed by     09670000
* G_SCAN_STATE                                                          09680000
G_SCAN_EXPECTED             DS    CL4                                   09690000
*                                                                       09700000
* Flags for using TM byte by byte in G_SCAN_EXPECTED                    09710000
SCAN_EXPECTED1_EOF          EQU   X'80'                                 09720000
SCAN_EXPECTED1_NEWLINE      EQU   X'40'                                 09730000
SCAN_EXPECTED1_COMMENT      EQU   X'20'                                 09740000
SCAN_EXPECTED1_IGNORE       EQU   X'10'                                 09750000
SCAN_EXPECTED1_NORMAL       EQU   X'08'                                 09760000
SCAN_EXPECTED1_OPENVAR      EQU   X'04'                                 09770000
SCAN_EXPECTED1_OPENBRC      EQU   X'02'                                 09780000
SCAN_EXPECTED1_CLOSEBRC     EQU   X'01'                                 09790000
SCAN_EXPECTED2_NUMBER       EQU   X'80'                                 09800000
SCAN_EXPECTED2_OPERATOR     EQU   X'40'                                 09810000
SCAN_EXPECTED2_RULE         EQU   X'20'                                 09820000
SCAN_EXPECTED2_SPECIAL      EQU   X'10'                                 09830000
SCAN_EXPECTED2_CONTINUA     EQU   X'08'                                 09840000
SCAN_EXPECTED2_CALL         EQU   X'04'                                 09850000
SCAN_EXPECTED2_ACRO         EQU   X'02'                                 09860000
SCAN_EXPECTED2_PERCENT      EQU   X'01'                                 09870000
SCAN_EXPECTED3_RECIPREF     EQU   X'80'                                 09880000
SCAN_EXPECTED3_COMMA        EQU   X'40'                                 09890000
*                                                                       09900000
* Combinations of the flags above, used in SCAN_STATE_TABLE             09910000
SCAN_EXPECTED_NEWSTMT       EQU   B'11111100000101001000000000000000'   09920000
SCAN_EXPECTED_NEWSTMT2      EQU   B'00001100011010001000000000000000'   09930000
SCAN_EXPECTED_ASSIGN        EQU   B'01111111100010111100000000000000'   09940000
SCAN_EXPECTED_ASSIGN2       EQU   B'01111111100010111100000000000000'   09950000
SCAN_EXPECTED_VARIABLE      EQU   B'00001000000010111000000000000000'   09960000
SCAN_EXPECTED_VARIABLER     EQU   B'00001001000010111000000000000000'   09970000
SCAN_EXPECTED_VARIABLE2     EQU   B'00000001000010111000000000000000'   09980000
SCAN_EXPECTED_RULE          EQU   B'00001111001010001000000000000000'   09990000
SCAN_EXPECTED_RULE2         EQU   B'01111111000010111000000000000000'   10000000
SCAN_EXPECTED_RULE3         EQU   B'01111111000010111000000000000000'   10010000
SCAN_EXPECTED_CALL          EQU   B'00001100000010111000000000000000'   10020000
SCAN_EXPECTED_CALL2         EQU   B'01111111100010111100000000000000'   10030000
SCAN_EXPECTED_EXPAND        EQU   B'10001111100000110100000000000000'   10040000
SCAN_EXPECTED_PHONY         EQU   B'00001000000010000000000000000000'   10050000
SCAN_EXPECTED_PHONY2        EQU   B'01110000000010000000000000000000'   10060000
SCAN_EXPECTED_ADDPDSNAME    EQU   B'00001100000010110000000000000000'   10070000
SCAN_EXPECTED_ADDPDSNAME2   EQU   B'00001100000010110100000000000000'   10080000
SCAN_EXPECTED_ADDPDSNAME3   EQU   B'00001100100010110000000000000000'   10090000
SCAN_EXPECTED_ADDPDSNAME4   EQU   B'00001101100010110000000000000000'   10100000
SCAN_EXPECTED_MEMBERLIST    EQU   B'00001100000010100000000000000000'   10110000
SCAN_EXPECTED_MEMBERLIST2   EQU   B'00001101000010100100000000000000'   10120003
SCAN_EXPECTED_MEMBERLIST3   EQU   B'00001100000010110000000000000000'   10130003
SCAN_EXPECTED_MEMBERLIST4   EQU   B'00001101100010110100000000000000'   10140003
SCAN_EXPECTED_IGNORE        EQU   B'01010000000000000000000000000000'   10150000
SCAN_EXPECTED_NEWLINE       EQU   B'01000000000000000000000000000000'   10160000
SCAN_EXPECTED_COMMENT       EQU   B'01110000000000000000000000000000'   10170000
*                                                                       10180000
* Stack of INPUT_DSECT structures, highest stack entry is the one       10190000
* LWZMAKE_SCAN_CHAR reads from. Initial size is 1, entry 0 filled       10200000
* with all zeros, indicating input from MAKEFILE DD                     10210000
                            DS    0F                                    10220000
MAX_SCAN_INPUT_STACK_ENTRY  EQU   20                                    10230000
G_SCAN_INPUT_STACK          DS    CL(MAX_SCAN_INPUT_STACK_ENTRY*INPUT_DX10240000
               SECT_SIZ)                                                10250000
G_SCAN_INPUT_STACK_IDX      DS    C                                     10260000
*                                                                       10270000
* Starting address of binary search tree of variables, each variable    10280000
* addressed with VAR_DSECT                                              10290000
                            DS    0F                                    10300000
G_FIRST_VAR_PTR             DS    A                                     10310000
*                                                                       10320000
* Parameters and return value for LWZMAKE_FINDVAR                       10330000
G_SRCH_VAR_LEN              DS    H                                     10340000
G_SRCH_VAR                  DS    CL72                                  10350000
G_FOUND_VAR_PTR             DS    A                                     10360000
*                                                                       10370000
* Starting address of binary search tree of targets, each target        10380000
* addressed with TARGET_DSECT                                           10390000
                            DS    0F                                    10400000
G_FIRST_TGT_PTR             DS    A                                     10410000
*                                                                       10420000
* Default target to build starting phase 2                              10430000
G_DEFAULT_TARGET            DS    CL72                                  10440000
*                                                                       10450000
* Return value for LWZMAKE_FINDTGT (takes input from G_SCAN_TOKEN)      10460000
G_FOUND_TGT_PTR             DS    A                                     10470000
*                                                                       10480000
* Parameter area ptr + area for first call to LWZMAKE_EXEC_TGT          10490000
G_EXEC_TGT_PAR1A            DS    A                                     10500000
G_EXEC_TGT_PAR1             DS    CL(EXEC_TGT_PAR_LEN)                  10510000
*                                                                       10520000
* Starting address of binary search tree of PHONIES, each PHONY         10530000
* addressed with PHONY_DSECT                                            10540000
                            DS    0F                                    10550000
G_FIRST_PNY_PTR             DS    A                                     10560000
*                                                                       10570000
* Return value for LWZMAKE_FINDPNY (takes input from G_SCAN_TOKEN)      10580000
G_FOUND_PNY_PTR             DS    A                                     10590000
*                                                                       10600000
* Returned altered date+time from LWZMAKE_GET_DATE                      10610000
                            DS    0F                                    10620000
G_SAVE_ALTER_DATE           DS    CL16                                  10630000
G_DSFOUND                   DS    C                                     10640000
*                                                                       10650000
* Current MAKEFILE record being scanned, read by LWZMAKE_SCAN_CHAR      10660000
                            DS    0F                                    10670000
G_MAKEFILE_REC              DS    CL80                                  10680000
*                                                                       10690000
* Progress report current page and line                                 10700000
                            DS    0F                                    10710000
G_LWZMRPT_CURRPAGE          DS    F                                     10720000
G_LWZMRPT_CURRLINE          DS    H                                     10730000
*                                                                       10740000
* Switch indicating whether line n column m should be added to rpt line 10750000
G_LWZMRPT_APND_LC           DS    C                                     10760000
*                                                                       10770000
* Line to write to LWZMRPT next                                         10780000
                            DS    0F                                    10790000
G_LWZMRPT_LINE              DS    CL133                                 10800000
*                                                                       10810000
* Room for formatting data to put in G_LWZMRPT_LINE                     10820000
                            DS    0F                                    10830000
G_LWZMRPT_HELPER            DS    CL80                                  10840000
*                                                                       10850000
LINES_PER_PAGE              EQU   40                                    10860000
*                                                                       10870000
                            DS    0F                                    10880000
G_PAGE_HEADER               DS    CL133                                 10890000
                            ORG   G_PAGE_HEADER+75                      10900000
G_PAGE_HEADER_DAY           DS    CL2                                   10910000
                            DS    C                                     10920000
G_PAGE_HEADER_MONTH         DS    CL2                                   10930000
                            DS    C                                     10940000
G_PAGE_HEADER_YEAR          DS    CL4                                   10950000
                            ORG   G_PAGE_HEADER+94                      10960000
G_PAGE_HEADER_HOUR          DS    CL2                                   10970000
                            DS    C                                     10980000
G_PAGE_HEADER_MINUTE        DS    CL2                                   10990000
                            DS    C                                     11000000
G_PAGE_HEADER_SECOND        DS    CL2                                   11010000
                            ORG   G_PAGE_HEADER+111                     11020000
G_PAGE_HEADER_PAGENR        DS    CL8                                   11030000
                            ORG                                         11040000
*                                                                       11050000
* Generic variables                                                     11060000
                            DS    0F                                    11070000
G_DEC8                      DS    PL8  * for CVB / CVD                  11080000
G_ZONED8                    DS    CL8  * for PACK / UNPK                11090000
                            DS    CL4  * Extra bytes for hex conversion 11100000
G_TIMEDATE                  DS    PL16 * for TIME macro                 11110000
                            DS    0F                                    11120000
G_TIMEDATEZ                 DS    CL32 * for formatting time            11130000
G_HELPER_DATA               DS    CL133 * for formatting anything       11140000
*                                       * mostly trace data             11150000
*                                                                       11160000
* WTOBLOCK for WTO execute form                                         11170000
G_WTOBLOCK                  DS    0F                                    11180000
G_WTOLEN                    DS    H                                     11190000
G_WTOFIL                    DS    H                                     11200000
G_WTOTEXT                   DS    CL133                                 11210000
*                                                                       11220000
* 3 multi-purpose 4K token area's                                       11230000
                            DS    0F                                    11240000
G_SCAN_TOKEN_LEN            DS    F                                     11250000
G_SCAN_TOKEN2_LEN           DS    F                                     11260000
G_SCAN_TOKEN3_LEN           DS    F                                     11270000
G_SCAN_TOKEN_MAXLEN         DS    F                                     11280000
G_SCAN_TOKEN2_MAXLEN        DS    F                                     11290000
G_SCAN_TOKEN3_MAXLEN        DS    F                                     11300000
G_SCAN_TOKENA               DS    A                                     11310000
G_SCAN_TOKEN2A              DS    A                                     11320000
G_SCAN_TOKEN3A              DS    A                                     11330000
*                                                                       11340000
SCAN_TOKEN_MAXLEN           EQU   4096                                  11350000
*                                                                       11360000
GLOBAL_DATA_SIZ             EQU   *-GLOBAL_DATA_DSECT                   11370000
*                                                                       11380000
* DSECT for DCB memory below the line                                   11390000
*                                                                       11400000
DCB_DSECT                   DSECT                                       11410000
*                                                                       11420000
* DCB for LWZMTRC, constant CDCBTRC copied into here                    11430000
DCBTRC                      DS    0F                                    11440000
                            ORG   *+LEN_DCBTRC                          11450000
*                                                                       11460000
* DCB for LWZMRPT, constant CDCBRPT copied into here                    11470000
DCBRPT                      DS    0F                                    11480000
                            ORG   *+LEN_DCBRPT                          11490000
*                                                                       11500000
* DCB for MAKEFILE, constant CDCBMKF copied into here                   11510000
DCBMKF                      DS    0F                                    11520000
                            ORG   *+LEN_DCBMKF                          11530000
*                                                                       11540000
* DCBE for MAKEFILE, constant CDCBEMKF copied into here                 11550000
DCBEMKF                     EQU   *                                     11560000
                            ORG   *+LEN_DCBEMKF                         11570000
*                                                                       11580000
* DCB for any PDS dynamically allocated for reading directory           11590000
DCBPDS_DIR                  DS    0F                                    11600000
                            ORG   *+LEN_DCBPDS_DIR_GD                   11610000
*                                                                       11620000
* DCBE for any PDS dynamically allocated for reading directory          11630000
DCBEPDS_DIR                 EQU   *                                     11640000
                            ORG   *+LEN_DCBEPDS_DIR_GD                  11650000
*                                                                       11660000
* DCB for any PDS dynamically allocated for binder access               11670000
DCBPDS_BDR                  DS    0F                                    11680000
                            ORG   *+LEN_DCBPDS_BDR                      11690000
*                                                                       11700000
DCB_DSECT_SIZ               EQU   *-DCB_DSECT                           11710000
*                                                                       11720000
* Every parsed statement is converted to internal format, being one of  11730000
* the following DSECTs. Each DSECT is dynamically allocated, pointer is 11740000
* stored in the statement pointer block linked list (starts at          11750000
* G_STMT_LIST_PTR). Every statement area starts with a generic part     11760000
* addressed with STMT_DSECT. Every STMT_*_DSECT starts with this        11770000
* generic part, followed by the specifics of the type of statement.     11780000
*                                                                       11790000
STMT_DSECT                  DSECT                                       11800000
STMT_LEN                    DS    F    * length of stmt area (generic + 11810000
*                                      * STMT_*_DSECT part)             11820000
STMT_TYPE                   DS    C    * byte for type of statement     11830000
STMT_TYPE_ASSIGNMENT        EQU   C'A' * assignment, use STMT_A_DSECT   11840000
STMT_TYPE_RULE              EQU   C'R' * rule, use STMT_R_DSECT         11850000
STMT_TYPE_CALL              EQU   C'C' * call, use STMT_C_DSECT         11860000
STMT_TYPE_PHONY             EQU   C'P' * PHONY, use STMT_P_DSECT        11870000
STMT_IN_RECIPE              DS    C    * switch (Y/N) indicating stmt   11880000
*                                      * found in recipe                11890000
                            DS    CL2  * reserved                       11900000
STMT_NEXT_PTR               DS    A    * forward chain to next stmt     11910000
STMT_PREV_PTR               DS    A    * backward chain to prev stmt    11920000
STMT_DSECT_LEN              EQU   *-STMT_DSECT                          11930000
*                                                                       11940000
* Statement area for assignment type statement                          11950000
*                                                                       11960000
STMT_A_DSECT                DSECT                                       11970000
*                                 * generic part of area                11980000
                            DS    CL(STMT_DSECT_LEN)                    11990000
*                                                                       12000000
STMT_A_DESTLEN              DS    H    * length of variable name        12010000
STMT_A_DEST                 DS    CL72 * variable name (destination)    12020000
STMT_A_OPERATOR             DS    CL2  * type of assignment (= or :=)   12030000
STMT_A_SRCLEN               DS    H    * length of value                12040000
STMT_A_SRC                  DS    0C   * value (source) starts here     12050000
*                                      * length can vary                12060000
STMT_A_DSECT_LEN            EQU   *-STMT_A_DSECT                        12070000
*                                                                       12080000
* Statement area for rule type statement                                12090000
*                                                                       12100000
STMT_R_DSECT                DSECT                                       12110000
*                                 * generic part of area                12120000
                            DS    CL(STMT_DSECT_LEN)                    12130000
*                                                                       12140000
* STMT_R_TGT is the starting address of 3 strings: target name,         12150000
* requisite name and requisite for sequence only name. Each string      12160000
* directly follows the previous one, so the lengths are also the        12170000
* offsets to the next string.                                           12180000
STMT_R_TGTLEN               DS    H   * target name length              12190000
STMT_R_REQLEN               DS    H   * requisite name length           12200000
STMT_R_REQOLEN              DS    H   * requisite for sequence only len 12210000
                            DS    CL2 * reserved                        12220000
STMT_R_TGT                  DS    0C  * target name starts here         12230000
STMT_R_DSECT_LEN            EQU   *-STMT_R_DSECT                        12240000
*                                                                       12250000
* Statement area for call type statement                                12260000
*                                                                       12270000
STMT_C_DSECT                DSECT                                       12280000
*                                 * generic part of area                12290000
                            DS    CL(STMT_DSECT_LEN)                    12300000
*                                                                       12310000
* STMT_C_EXEC is the starting address of 2 strings: exec name and       12320000
* parameter string. Parameter string directly follows the exec name,    12330000
* so the exec length is also the offset to the parameter string.        12340000
STMT_C_EXECLEN              DS    H   * exec name length                12350000
STMT_C_PARMLEN              DS    H   * parameters string length        12360000
STMT_C_EXEC                 DS    0C  * exec name starts here           12370000
STMT_C_DSECT_LEN            EQU   *-STMT_C_DSECT                        12380000
*                                                                       12390000
* Statement area for PHONY type statement                               12400000
*                                                                       12410000
STMT_P_DSECT                DSECT                                       12420000
*                                 * generic part of area                12430000
                            DS    CL(STMT_DSECT_LEN)                    12440000
*                                                                       12450000
* STMT_P_PNY is the starting address of a phony target name.            12460000
STMT_P_PNYLEN               DS    H   * PHONY name length               12470000
                            DS    CL2 * reserved                        12480000
STMT_P_PNY                  DS    0C  * PHONY name starts here          12490000
STMT_P_DSECT_LEN            EQU   *-STMT_P_DSECT                        12500000
*                                                                       12510000
* Variable area, first one pointed to by G_FIRST_VAR_PTR, each VARLOW   12520000
* and VARHIGH (possibly) point to variables with a name lower or higher 12530000
*                                                                       12540000
VAR_DSECT                   DSECT                                       12550000
VARLEN                      DS    H    * length of variable name        12560000
VARNAME                     DS    CL72 * variable name                  12570000
VALLEN                      DS    H    * length of variable value       12580000
VALPTR                      DS    A    * pointer to value (getmain'd)   12590000
VARLOW                      DS    A    * pointer to variable with name  12600000
*                                      * lower than this one            12610000
VARHIGH                     DS    A    * pointer to variable with name  12620000
*                                      * higher than this one           12630000
VAR_DSECT_LEN               EQU   *-VAR_DSECT                           12640000
*                                                                       12650000
* Target area, first one pointed to by G_FIRST_TGT_PTR, each TGTLOW     12660000
* and TGTHIGH (possibly) point to target with a name lower or higher    12670000
*                                                                       12680000
TARGET_DSECT                DSECT                                       12690000
TGTLEN                      DS    F    * length of the whole block      12700000
TGTLOW                      DS    A    * pointer to target with name    12710000
*                                      * lower than this one            12720000
TGTHIGH                     DS    A    * pointer to target with name    12730000
*                                      * higher than this one           12740000
TGTSTMT                     DS    A    * pointer to stmt that resulted  12750000
*                                      * in this target                 12760000
TGTNAMELEN                  DS    H    * length of target name          12770000
TGTNAMEMEMLEN               DS    H    * length of target member name   12780000
TGTNAME                     DS    0C   * target name starts here        12790000
TARGET_DSECT_LEN            EQU   *-TARGET_DSECT                        12800000
*                                                                       12810000
* PHONY area, first one pointed to by G_FIRST_PNY_PTR, each PNYLOW      12820000
* and PNYHIGH (possibly) point to PHONY with a name lower or higher     12830000
*                                                                       12840000
PHONY_DSECT                 DSECT                                       12850000
PNYLEN                      DS    F    * length of the whole block      12860000
PNYLOW                      DS    A    * pointer to PHONY with name     12870000
*                                      * lower than this one            12880000
PNYHIGH                     DS    A    * pointer to PHONY with name     12890000
*                                      * higher than this one           12900000
PNYSTMT                     DS    A    * pointer to stmt that resulted  12910000
*                                      * in this PHONY                  12920000
PNYNAMELEN                  DS    H    * length of PHONY name           12930000
PNYNAME                     DS    0C   * PHONY name starts here         12940000
PHONY_DSECT_LEN             EQU   *-PHONY_DSECT                         12950000
*                                                                       12960000
* Input descriptor area, G_SCAN_INPUT_STACK consists of blocks with     12970000
* this layout. A block with all zeros means input is taken from the     12980000
* MAKEFILE DD. The value of a variable is another source of input.      12990000
*                                                                       13000000
INPUT_DSECT                 DSECT                                       13010000
INPUTTYPE                   DS    C    * type of input, X'00' means     13020000
*                                      * input from MAKEFILE DD         13030000
                            DS    C    * reserved                       13040000
*                                                                       13050000
INPUTLEAD                   DS    H    * leading spaces count           13060000
*                                                                       13070000
INPUTPTR                    DS    A    * for type != X'00' ptr to input 13080000
*                                      * string (e.g. variable value)   13090000
INPUTLEN                    DS    H    * length of string pointed to by 13100000
*                                      * INPUTPTR                       13110000
INPUTPOS                    DS    H    * current position in string     13120000
*                                      * pointed to by INPUTPTR         13130000
INPUT_DSECT_SIZ             EQU   *-INPUT_DSECT                         13140000
*                                                                       13150000
* DSECT for IGGCSI00 catalog search interface                           13160000
CSIFIELD_DSECT              DSECT                                       13170000
CSIFILTK                    DS    CL44    FILTER   KEY                  13180000
CSICATNM                    DS    CL44    CATALOG NAME OR BLANKS        13190000
CSIRESNM                    DS    CL44    RESUME NAME OR BLANKS         13200000
CSIDTYPD                    DS    0CL16   ENTRY TYPES                   13210000
CSIDTYPS                    DS    CL16                                  13220000
CSIOPTS                     DS    0CL4    CSI OPTIONS                   13230000
CSICLDI                     DS    CL1     RETURN DATA OR INDEX          13240000
CSIRESUM                    DS    CL1     RESUME FLAG                   13250000
CSIS1CAT                    DS    CL1     SEARCH CATALOG                13260000
CSIRESRV                    DS    XL1     RESERVED                      13270000
CSINUMEN                    DS    H       NUMBER OF ENTRIES FOLLOWING   13280000
CSIENTS                     DS    0CL8    VARIABLE NUMBER OF ENTRIES    13290000
CSIFLDNM                    DS    CL8     FIELD NAME                    13300000
CSIFIELD_DSECT_SIZ          EQU   *-CSIFIELD_DSECT                      13310000
*                                                                       13320000
* DSECT for OBTAIN                                                      13330000
OBTAIN_DSECT                DSECT                                       13340000
                            IECSDSL1 1                                  13350000
OBTAIN_DSECT_SIZ            EQU   *-OBTAIN_DSECT                        13360000
*                                                                       13370000
* DSECT for addressing a DCB                                            13380000
                            DCBD  DSORG=PS,DEVD=DA                      13390000
*                                                                       13400000
* DSECT for addresssing a DCBE                                          13410000
                            IHADCBE                                     13420000
*                                                                       13430000
* The following macro's are all needed to use IAZXJSAB for determining  13440000
* the submitter user id.                                                13450000
                            IHAPSA   DSECT=YES,LIST=YES                 13460000
                            IAZJSAB  DSECT=YES,LIST=NO                  13470000
                            IHAASCB  DSECT=YES,LIST=NO                  13480000
                            IHAASSB  LIST=NO                            13490000
                            IHASTCB  LIST=NO                            13500000
                            IKJTCB   DSECT=YES,LIST=NO                  13510000
*                                                                       13520000
* DSECT for addressing REXX EXEC block                                  13530000
                            IRXEXECB                                    13540000
*                                                                       13550000
* DSECT for addressing REXX EVAL block                                  13560000
                            IRXEVALB                                    13570000
*                                                                       13580000
* Continue with code                                                    13590000
LWZMAKE  CSECT                                                          13600000
*                                                                       13610000
*********************************************************************** 13620000
* Section: LWZMAKE_TRACE                                              * 13630000
* Purpose: This section writes a trace record to the LWZMTRC DD.      * 13640000
*          Mostly invoked using the macro MLWZMTRC at the top of this * 13650000
*          source file.                                               * 13660000
*          On entry, G_LWZMTRC_MSGNR should be filled with a 3 digit  * 13670000
*          message number (e.g. C'001'), optionally G_LWZMTRC_DATA_PTR* 13680000
*          and G_LWZMTRC_DATA_SIZ can also be provided. When DATA_SIZ * 13690000
*          is not zero, whatever DATA_PTR points to is appended to the* 13700000
*          trace record after the last non-space + 1.                 * 13710000
*          R9 should point to global data.                            * 13720000
*********************************************************************** 13730000
LWZMAKE_TRACE MLWZSAVE                                                  13740000
*                                                                       13750000
*        Make sure G_LWZMTRC_MSGNR is composed of 3 digits              13760000
         TRT   G_LWZMTRC_MSGNR,XLATENUM                                 13770000
         BNZ   RET_TRACE          * if not, skip to end                 13780000
*                                                                       13790000
*        Setup R6,R7 and R8 dependent on the hundreds digit             13800000
         SELECT CLI,G_LWZMTRC_MSGNR,EQ                                  13810000
            WHEN C'0'                                                   13820000
               LA    R8,LWZ000    * Start of 000-099 messages           13830000
               LA    R7,LWZ000T   * Table with 100 single byte offsets  13840000
               LA    R6,LWZ000X   * 2x2 byte offset+length table        13850000
            WHEN C'6'                                                   13860000
               LA    R8,LWZ600    * Start of 600-699 messages           13870000
               LA    R7,LWZ600T   * Table with 100 single byte offsets  13880000
               LA    R6,LWZ600X   * 2x2 byte offset+length table        13890000
            OTHRWISE                                                    13900000
               B     RET_TRACE    * if none of the above, skip to end   13910000
         ENDSEL                                                         13920000
*                                                                       13930000
*        Convert last 2 digits of message number to binary in R2        13940000
         MVC   G_ZONED8,=CL8'0'   * initialize to 8 zeros               13950000
         MVC   G_ZONED8+6(2),G_LWZMTRC_MSGNR+1 * put last 2 digits      13960000
*                                              * at the end             13970000
         PACK  G_DEC8,G_ZONED8    * convert to packed decimal           13980000
         CVB   R2,G_DEC8          * convert to binary                   13990000
*                                                                       14000000
*        Use binary value of last 2 digits as offset to LWZn00T         14010000
         AR    R2,R7              * offset LWZn00T by message number    14020000
         CLI   0(R2),X'FF'        * check if constant is present        14030000
         BE    RET_TRACE          * if not, skip to end                 14040000
*                                                                       14050000
*        Use byte in LWZn00T as index to LWZn00X                        14060000
         XR    R3,R3              * Clear R3                            14070000
         IC    R3,0(,R2)          * Put index byte in R3                14080000
         SLL   R3,2               * Multiply by 4                       14090000
         AR    R3,R6              * offset LWZn00X by index             14100000
*                                                                       14110000
*        Use offset in entry of LWZn00X to address message constant     14120000
         XR    R4,R4              * Clear R4                            14130000
         LH    R4,0(,R3)          * Load halfword offset to LWZn00      14140000
         AR    R4,R8              * offset LWZn00                       14150000
*                                                                       14160000
*        Put length in entry of LWZn00X in R5                           14170000
         XR    R5,R5              * Clear R5                            14180000
         LH    R5,2(,R3)          * Load length of message constant     14190000
         BCTR  R5,R0              * Minus 1 for EX of MVC               14200000
*                                                                       14210000
         LA    R1,G_LWZMTRC_RECORD * Put address of trace record in R1  14220000
         MVI   0(R1),C' '         * Initialize trace record to blanks   14230000
         MVC   1(L'G_LWZMTRC_RECORD-1,R1),0(R1)                         14240000
         MVC   1(3,R1),=C'LWZ'    * Put LWZ in pos 2-4                  14250000
         MVC   4(3,R1),G_LWZMTRC_MSGNR * Put message number in pos 5-7  14260000
*                                                                       14270000
*        Suffix message number with severity level                      14280000
         SELECT CLI,G_LWZMTRC_MSGNR,EQ                                  14290000
            WHEN C'0'             * Trace messages LWZ000E-LWZ099E      14300000
               MVI   7(R1),C'E'   * are error messages                  14310000
            WHEN C'6'             * Trace messages LWZ600I-LWZ699I      14320000
               MVI   7(R1),C'I'   * are informatory messages            14330000
         ENDSEL                                                         14340000
*                                                                       14350000
         B     *+10               * Skip MVC constant for EX            14360000
         MVC   9(1,R1),0(R4)      * MVC constant for EX                 14370000
         EX    R5,*-6             * EX previous MVC statement with R5   14380000
*                                                                       14390000
         BAL   R8,TRACE_APPEND    * Perform append of G_LWZMTRC_DATA    14400000
*                                                                       14410000
         L     R5,G_DCB_MEM_PTR   * Get DCB memory pointer              14420000
         LA    R5,DCBTRC-DCB_DSECT(,R5) * Get addr of LWZMTRC DCB       14430000
         PUT   (R5),G_LWZMTRC_RECORD * Write a trace record to LWZMTRC  14440000
*                                                                       14450000
RET_TRACE EQU  *                                                        14460000
         MLWZTERM                 * Return back to caller               14470000
*                                                                       14480000
* Append trace data to trace record after last non space + 1.           14490000
* Trace data is optionally pointed to by G_LWZMTRC_DATA_PTR, but only   14500000
* if length set in G_LWZMTRC_DATA_SIZ is non zero.                      14510000
*                                                                       14520000
TRACE_APPEND EQU *                                                      14530000
         CLC   G_LWZMTRC_DATA_SIZ,=H'0' * Check for zero length         14540000
         BE    TRACE_APPEND_RET   * If zero, skip append routine        14550000
*                                                                       14560000
*        Trim trailing spaces in trace record                           14570000
         LA    R6,G_LWZMTRC_RECORD * Point R6 to trace record           14580000
         LA    R5,132(,R6)        * Point R5 to last byte of record     14590000
         LA    R4,132             * Put trace record length - 1 in R4   14600000
TRACE_APPEND_TRIM EQU *                                                 14610000
         CLI   0(R5),C' '         * Check for space                     14620000
         BNE   TRACE_APPEND_TRIM_DONE * Non space means done trimming   14630000
         BCTR  R5,R0              * R5 = R5 - 1                         14640000
         BCT   R4,TRACE_APPEND_TRIM * R4 = R4 - 1 until R4 = 0          14650000
TRACE_APPEND_TRIM_DONE EQU *                                            14660000
         LA    R5,2(,R5)          * Go forward to last space + 1        14670000
         LA    R4,2(,R4)          * R4 = R4 + 2                         14680000
         C     R4,=F'133'         * Check if any space left in record   14690000
         BNL   TRACE_APPEND_RET   * If not, skip to end of routine      14700000
         LA    R3,133             * Put length of trace record in R3    14710000
         SR    R3,R4              * Subtract length of text + 2         14720000
         CH    R3,G_LWZMTRC_DATA_SIZ * Check if trace data length       14730000
         IF (H) THEN              * is less than remaining room in rcd  14740000
            LH    R3,G_LWZMTRC_DATA_SIZ * If so, replace with trace     14750000
         ENDIF                    * data length                         14760000
         BCTR  R3,R0              * Minus 1 for EX of MVC               14770000
         L     R2,G_LWZMTRC_DATA_PTR * Put trace data pointer in R2     14780000
         B     *+10               * Skip MVC constant for EX            14790000
         MVC   0(1,R5),0(R2)      * MVC constant for EX                 14800000
         EX    R3,*-6             * EX previous MVC statement with R3   14810000
*                                                                       14820000
TRACE_APPEND_RET EQU *                                                  14830000
         BR    R8                 * Return                              14840000
*                                                                       14850000
         LTORG                                                          14860000
*                                                                       14870000
* Translate table to check for digits C'0' - C'9'                       14880000
XLATENUM DS    0F                                                       14890000
         DC    256X'FF'                                                 14900000
         ORG   XLATENUM+C'0'                                            14910000
         DC    10X'00'                                                  14920000
         ORG                                                            14930000
*                                                                       14940000
* The following macro is used to generate constant tables used for      14950000
* addressing messages. It's used for a range of 100 message numbers,    14960000
* e.g. from LWZ000E to LWZ099E, and it's only used in this section.     14970000
* It expects a series of message constants to be defined, each one      14980000
* directly following the other, starting with a label like LWZ000.      14990000
* It goes through 100 message numbers in three passes.                  15000000
* Pass 1: populate the macro's local character variable array MSGS      15010000
* Pass 2: generate a table LWZn00T of 100 single byte entries, one for  15020000
*         each message number. A message number for which there's no    15030000
*         constant defined in the assembler source, a byte X'FF' is     15040000
*         generated, for message numbers with a constant defined a      15050000
*         byte with a sequence number is generated. That sequence       15060000
*         number is used as an index to the table generated in pass 3.  15070000
* Pass 3: generate a table LWZn00X with an entry for each defined       15080000
*         message. Each entry is 2x2 bytes, the first 2 are an offset   15090000
*         of the message constant to the starting label, the second 2   15100000
*         are the message constant length.                              15110000
*                                                                       15120000
                  MACRO                                                 15130000
.* MSGPREF   = first 4 characers of messages, e.g. LWZ0                 15140000
.* MSGSUFFIX = last character of message indicating severy level I/E    15150000
                  MTRCNTRS &MSGPREF=,&MSGSUFFIX=                        15160000
                  LCLA  &COUNTER   * for looping 100 times              15170000
                  LCLC  &MSGNR     * complete msg const name LWZnnna    15180000
                  LCLC  &MSGS(100) * table with 100 index values or FF  15190000
                  LCLA  &INDEX     * current index                      15200000
.*                                                                      15210000
.* start loop with 0                                                    15220000
&COUNTER          SETA  0                                               15230000
.* stop looping when counter > 99                                       15240000
.CHECK_LOOP1      AIF   ('&COUNTER' GT '99').STOP_LOOP1                 15250000
.* construct complete message constant name LWZnnna                     15260000
.* if counter is single digit, add a '0'                                15270000
                  AIF   (K'&COUNTER EQ 2).DOUBLE_DIGIT                  15280000
&MSGNR            SETC  '&MSGPREF'.'0'.'&COUNTER'.'&MSGSUFFIX'          15290000
                  AGO   .MSGNR_SET                                      15300000
.DOUBLE_DIGIT     ANOP                                                  15310000
&MSGNR            SETC  '&MSGPREF'.'&COUNTER'.'&MSGSUFFIX'              15320000
.MSGNR_SET        ANOP                                                  15330000
.* preset current message entry in table to empty                       15340000
&MSGS(&COUNTER+1) SETC ''                                               15350000
.* if assembler source contains defined constant with MSGNR as name     15360000
                  AIF   (NOT D'&MSGNR).INCREMENT_LOOP1                  15370000
.* put message name in table entry                                      15380000
&MSGS(&COUNTER+1) SETC '&MSGNR'                                         15390000
.* increase counter and loop around                                     15400000
.INCREMENT_LOOP1  ANOP                                                  15410000
&COUNTER          SETA  &COUNTER+1   * increase counter                 15420000
                  AGO   .CHECK_LOOP1 * and loop around                  15430000
.* done with loop 1                                                     15440000
.STOP_LOOP1       ANOP                                                  15450000
.*                                                                      15460000
.* define start of table with single byte offsets                       15470000
&MSGPREF.00T      DS    0F                                              15480000
.* initialize index (sequence number)                                   15490000
&INDEX            SETA  0                                               15500000
.* start loop with 0                                                    15510000
&COUNTER          SETA  0                                               15520000
.* stop looping when counter > 99                                       15530000
.CHECK_LOOP2      AIF   ('&COUNTER' GT '99').STOP_LOOP2                 15540000
.* if there was a constant defined for this message, the entry in       15550000
.* MSGS is non blank                                                    15560000
                  AIF   ('&MSGS(&COUNTER+1)' EQ '').EMPTY_MSG           15570000
.* in that case define a byte with the index in LWZn00T table           15580000
                  DC    AL1(&INDEX)                                     15590000
&INDEX            SETA  &INDEX+1  * and increase the index              15600000
                  AGO   .INCREMENT_LOOP2                                15610000
.* when entry in MSGS is blank define a byte X'FF' in LWZn00T table     15620000
.EMPTY_MSG        ANOP                                                  15630000
                  DC    X'FF'                                           15640000
.* increase counter and loop around                                     15650000
.INCREMENT_LOOP2  ANOP                                                  15660000
&COUNTER          SETA  &COUNTER+1   * increase counter                 15670000
                  AGO   .CHECK_LOOP2 * and loop around                  15680000
.* done with loop 2                                                     15690000
.STOP_LOOP2       ANOP                                                  15700000
.*                                                                      15710000
.* define start of table with 2x2 byte (offset+length) entries          15720000
&MSGPREF.00X      DS    0F                                              15730000
.* start loop with 0                                                    15740000
&COUNTER          SETA  0                                               15750000
.* stop looping when counter > 99                                       15760000
.CHECK_LOOP3      AIF   ('&COUNTER' GT '99').STOP_LOOP3                 15770000
.* if there was a constant defined for this message, the entry in       15780000
.* MSGS is non blank                                                    15790000
                  AIF   ('&MSGS(&COUNTER+1)' EQ '').INCREMENT_LOOP3     15800000
.* if so, copy message constant name to MSGNR                           15810000
&MSGNR            SETC  '&MSGS(&COUNTER+1)'                             15820000
.* and define an entry in LWZn00X table                                 15830000
                  DC    AL2(&MSGNR-&MSGPREF.00),AL2(L'&MSGNR)           15840000
.* increase counter and loop around                                     15850000
.INCREMENT_LOOP3  ANOP                                                  15860000
&COUNTER          SETA  &COUNTER+1   * increase counter                 15870000
                  AGO   .CHECK_LOOP3 * and loop around                  15880000
.* done with loop 3                                                     15890000
.STOP_LOOP3       ANOP                                                  15900000
.* done with macro                                                      15910000
                  MEND                                                  15920000
*                                                                       15930000
* Messages LWZ000E-LWZ099E                                              15940000
*                                                                       15950000
LWZ000   DS    0F                                                       15960000
LWZ001E  DC    C'LWZMRPT OPEN FAILED'                                   15970000
LWZ002E  DC    C'MAKEFILE OPEN FAILED'                                  15980000
LWZ003E  DC    C'PARSE ERROR'                                           15990000
LWZ010E  DC    C'BINDER ERROR'                                          16000000
*                                                                       16010000
* Generate LWZ000T and LWZ000X                                          16020000
*                                                                       16030000
         MTRCNTRS MSGPREF=LWZ0,MSGSUFFIX=E                              16040000
*                                                                       16050000
* Messages LWZ600I-LWZ699I                                              16060000
*                                                                       16070000
LWZ600   DS    0F                                                       16080000
LWZ601I  DC    C'LWZMAKE TRACE STARTED'                                 16090000
LWZ602I  DC    C'DCB OPENED'                                            16100000
LWZ603I  DC    C'DCB CLOSED'                                            16110000
LWZ604I  DC    C'SECTION STARTED'                                       16120000
LWZ605I  DC    C'SECTION ENDED'                                         16130000
LWZ606I  DC    C'PARAMETER RECEIVED'                                    16140000
LWZ608I  DC    C'START PARSE STATEMENT TYPE'                            16150000
LWZ609I  DC    C'FINISH PARSE STATEMENT'                                16160000
LWZ610I  DC    C'PARSED TOKEN'                                          16170000
LWZ611I  DC    C'PARSED CHAR '                                          16180000
LWZ612I  DC    C'START PARSE TOKEN TYPE'                                16190000
LWZ640I  DC    C'STATEMENT BLOCK ALLOCATE'                              16200000
LWZ641I  DC    C'STATEMENT BLOCK FREE'                                  16210000
LWZ642I  DC    C'VARIABLE BLOCK ALLOCATE'                               16220000
LWZ643I  DC    C'VARIABLE BLOCK FREE'                                   16230000
LWZ699I  DC    C'LWZMAKE TRACE ENDED'                                   16240000
*                                                                       16250000
* Generate LWZ600T and LWZ600X                                          16260000
*                                                                       16270000
         MTRCNTRS MSGPREF=LWZ6,MSGSUFFIX=I                              16280000
*                                                                       16290000
*********************************************************************** 16300000
* Section: LWZMAKE_RPT                                                * 16310000
* Purpose: This section writes a report line to the LWZMRPT DD.       * 16320000
*          Mostly invoked using the macro MLWZMRPT at the top of this * 16330000
*          source file.                                               * 16340000
*          On entry, G_LWZMRPT_LINE contains the line to be written.  * 16350000
*          If G_LWZMRPT_APND_LC is set to 'Y', G_SCAN_CURRLINE and    * 16360000
*          G_SCAN_CURRCOL are appended to the report line.            * 16370000
*          R9 should point to global data.                            * 16380000
*********************************************************************** 16390000
LWZMAKE_RPT MLWZSAVE                                                    16400000
*                                                                       16410000
*        If G_LWZMRPT_APND_LC = 'Y', append line and column nr to line  16420000
         IF (CLI,G_LWZMRPT_APND_LC,EQ,C'Y') THEN                        16430000
            BAL   R8,RPT_APPEND_LC  * Perform append line and column nr 16440000
            MVI   G_LWZMRPT_APND_LC,C'N' * Reset switch to 'N'          16450000
         ENDIF                                                          16460000
*                                                                       16470000
WRITE    EQU   *                                                        16480000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              16490000
         LA    R2,DCBRPT-DCB_DSECT(,R14) * Get addr of LWZMRPT DCB      16500000
*                                                                       16510000
         XR    R3,R3              * Clear R3                            16520000
         LH    R3,G_LWZMRPT_CURRLINE * Load current line number         16530000
         IF (CLI,G_LWZMRPT_LINE,EQ,C' ') THEN * Check ASA ' ' char      16540000
            LA    R3,1(,R3)       * If found, advance 1 line number     16550000
            B     CHECK_CURRLINE  * Skip other ASA checks               16560000
         ENDIF                                                          16570000
         IF (CLI,G_LWZMRPT_LINE,EQ,C'0') THEN * Check ASA '0' char      16580000
            LA    R3,2(,R3)       * If found, advance 2 line numbers    16590000
            B     CHECK_CURRLINE  * Skip other ASA checks               16600000
         ENDIF                                                          16610000
         IF (CLI,G_LWZMRPT_LINE,EQ,C'-') THEN * Check ASA '-' char      16620000
            LA    R3,3(,R3)       * If found, advance 3 line numbers    16630000
            B     CHECK_CURRLINE  * Skip other ASA checks               16640000
         ENDIF                                                          16650000
         CLI   G_LWZMRPT_LINE,C'1' * Check ASA '1' char                 16660000
         BE    PAGE_SKIP          * If found, jump to page skip         16670000
CHECK_CURRLINE EQU *                                                    16680000
         STH   R3,G_LWZMRPT_CURRLINE * Store the new line number        16690000
         CH    R3,=AL2(LINES_PER_PAGE) * Check if we crossed page bndry 16700000
         BNH   NO_PAGE_SKIP       * If not, don't do page skip          16710000
PAGE_SKIP DS   0H                                                       16720000
         L     R4,G_LWZMRPT_CURRPAGE * Get current page number          16730000
         LA    R4,1(,R4)          * page number = page number + 1       16740000
         ST    R4,G_LWZMRPT_CURRPAGE * and put it back                  16750000
         CVD   R4,G_DEC8          * convert to packed decimal           16760000
         UNPK  G_ZONED8,G_DEC8    * convert to zoned                    16770000
         OI    G_ZONED8+7,X'F0'   * get rid of sign nibble              16780000
         MVC   G_PAGE_HEADER_PAGENR,G_ZONED8 * put page nr in header    16790000
*        Get the current date+time                                      16800000
         TIME  DEC,G_TIMEDATE,ZONE=LT,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD  16810000
*        Convert time part to zoned                                     16820000
         UNPK  G_TIMEDATEZ+10(10),G_TIMEDATE(5)                         16830000
*        Convert date part to zoned                                     16840000
         UNPK  G_TIMEDATEZ(10),G_TIMEDATE+8(5)                          16850000
*        Put date and time in header                                    16860000
         MVC   G_PAGE_HEADER_DAY,G_TIMEDATEZ+7                          16870000
         MVC   G_PAGE_HEADER_MONTH,G_TIMEDATEZ+5                        16880000
         MVC   G_PAGE_HEADER_YEAR,G_TIMEDATEZ+1                         16890000
         MVC   G_PAGE_HEADER_HOUR,G_TIMEDATEZ+11                        16900000
         MVC   G_PAGE_HEADER_MINUTE,G_TIMEDATEZ+13                      16910000
         MVC   G_PAGE_HEADER_SECOND,G_TIMEDATEZ+15                      16920000
         PUT   (R2),G_PAGE_HEADER * Write a report line to LWZMRPT      16930000
         MVI   G_LWZMRPT_LINE,C'0' * Overwrite ASA char, skip 2 lines   16940000
         LH    R3,=H'1'           * Set R3 to 1                         16950000
         STH   R3,G_LWZMRPT_CURRLINE * and store it as current line nr  16960000
         B     WRITE              * Jump back to write the actual line  16970000
NO_PAGE_SKIP DS 0H                                                      16980000
         PUT   (R2),G_LWZMRPT_LINE * Write a report line to LWZMRPT     16990000
*                                                                       17000000
RET_RPT  EQU   *                                                        17010000
         MLWZTERM                 * Return back to caller               17020000
*                                                                       17030000
* Append ' at line x column y' to print line, only performed when       17040000
* G_LWZMRPT_APND_LC is set to 'Y'                                       17050000
*                                                                       17060000
RPT_APPEND_LC EQU *                                                     17070000
*        Initialize helper var                                          17080000
         MVC   G_LWZMRPT_HELPER,=CL80' at line'                         17090000
         LA    R7,G_LWZMRPT_HELPER+9 * Point R7 to where line nr starts 17100000
*                                                                       17110000
         L     R15,G_SCAN_CURRLINE * Get the current line number        17120000
         CVD   R15,G_DEC8         * Convert it to packed decimal        17130000
         UNPK  G_ZONED8,G_DEC8    * Convert it to zoned                 17140000
         OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble              17150000
*                                                                       17160000
*        Left trim the line number of leading zeros                     17170000
         LA    R2,G_ZONED8        * Point R2 to line number             17180000
         LA    R3,L'G_ZONED8-1    * Put byte counter - 1 in R3, so      17190000
*                                 * we're always left with min. 1 byte  17200000
*                                 * and R3 is ready to use with EX      17210000
RPT_TRIM_NR1 EQU *                                                      17220000
         CLI   0(R2),C'0'         * Is this a zero?                     17230000
         BNE   RPT_TRIM_NR1_END   * Nope, stop trimming                 17240000
         LA    R2,1(,R2)          * R2 = R2 + 1                         17250000
         BCT   R3,RPT_TRIM_NR1    * R3 = R3 - 1 until R3 = 0            17260000
RPT_TRIM_NR1_END EQU *                                                  17270000
         B     *+10               * Skip MVC constant for EX            17280000
         MVC   0(1,R7),0(R2)      * MVC constant for EX                 17290000
         EX    R3,*-6             * EX previous MVC statement with R3   17300000
         LA    R3,1(,R3)          * R3 = R3 + 1                         17310000
         AR    R7,R3              * Advance R7 past line number         17320000
*                                                                       17330000
         MVC   0(8,R7),=C' column ' * Append constant                   17340000
         LA    R7,8(,R7)          * Advance R7 to point after constant  17350000
*                                                                       17360000
         L     R15,G_SCAN_CURRCOL * Get the current column number       17370000
         CVD   R15,G_DEC8         * Convert it to packed decimal        17380000
         UNPK  G_ZONED8,G_DEC8    * Convert it to zoned                 17390000
         OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble              17400000
*                                                                       17410000
*        Left trim the column number of leading zeros                   17420000
         LA    R2,G_ZONED8        * Point R2 to column number           17430000
         LA    R3,L'G_ZONED8-1    * Put byte counter - 1 in R3, so      17440000
*                                 * we're always left with min. 1 byte  17450000
*                                 * and R3 is ready to use with EX      17460000
RPT_TRIM_NR2 EQU *                                                      17470000
         CLI   0(R2),C'0'         * Is this a zero?                     17480000
         BNE   RPT_TRIM_NR2_END   * Nope, stop trimming                 17490000
         LA    R2,1(,R2)          * R2 = R2 + 1                         17500000
         BCT   R3,RPT_TRIM_NR2    * R3 = R3 - 1 until R3 = 0            17510000
RPT_TRIM_NR2_END EQU *                                                  17520000
         B     *+10               * Skip MVC constant for EX            17530000
         MVC   0(1,R7),0(R2)      * MVC constant for EX                 17540000
         EX    R3,*-6             * EX previous MVC statement with R3   17550000
         LA    R3,1(,R3)          * R3 = R3 + 1                         17560000
         AR    R7,R3              * Advance R7 past column number       17570000
*                                                                       17580000
*        Calculate actual length of helper var                          17590000
         LA    R6,G_LWZMRPT_HELPER * Point R6 to helper var             17600000
         SR    R7,R6              * Subtract start from R7              17610000
*                                                                       17620000
*        Trim the report line of trailing spaces                        17630000
         LA    R2,G_LWZMRPT_LINE+L'G_LWZMRPT_LINE-1 * Point R2 to last  17640000
*                                 * byte of report line                 17650000
         LA    R3,L'G_LWZMRPT_LINE-1 * R3 = length of line - 1, so      17660000
*                                 * we're always left with min. 1 byte  17670000
RPT_TRIM_LINE EQU *                                                     17680000
         CLI   0(R2),C' '         * Is this a space?                    17690000
         BNE   RPT_TRIM_LINE_DONE * Nope, stop trimming                 17700000
         BCTR  R2,R0              * R2 = R2 - 1                         17710000
         BCT   R3,RPT_TRIM_LINE   * R3 = R3 - 1 until R3 = 0            17720000
RPT_TRIM_LINE_DONE EQU *                                                17730000
         LA    R2,1(,R2)          * Point R2 past last non space        17740000
         LA    R5,G_LWZMRPT_LINE+L'G_LWZMRPT_LINE * Point R5 past end   17750000
*                                 * of report line                      17760000
         SR    R5,R2              * Calculate room left                 17770000
         BO    RPT_APPEND_LC_END  * No room left, skip rest of append   17780000
         CR    R5,R7              * Check if room left is more than     17790000
         IF (H) THEN              * the room needed for append string   17800000
            LR    R5,R7           * If so, use length of append string  17810000
         ENDIF                                                          17820000
         BCTR  R5,R0              * R5 = R5 - 1 because of EX           17830000
         B     *+10               * Skip MVC constant for EX            17840000
         MVC   0(1,R2),G_LWZMRPT_HELPER * MVC constant for EX           17850000
         EX    R5,*-6             * EX previous MVC statement with R5   17860000
*                                                                       17870000
RPT_APPEND_LC_END EQU *                                                 17880000
         BR    R8                 * Return                              17890000
*                                                                       17900000
         LTORG                                                          17910000
*                                                                       17920000
*********************************************************************** 17930000
* Section: LWZMAKE_APPEND_TOKEN                                       * 17940000
* Purpose: This section appends token 1 to either token 2 or 3 and    * 17950000
*          takes care of allocating a larger memory block if there's  * 17960000
*          not enough room.                                           * 17970000
*          R9 should point to global data.                            * 17980000
*********************************************************************** 17990000
LWZMAKE_APPEND_TOKEN MLWZSAVE                                           18000000
*                                                                       18010000
*        If APPEND_TO != X'00' then skip to end                         18020000
         CLI   G_SCAN_APPEND_TO,X'00'                                   18030000
         BE    APPEND_RET                                               18040000
*                                                                       18050000
*        If token 1 length = 0 then skip to end                         18060000
         LT    R2,G_SCAN_TOKEN_LEN                                      18070000
         BZ    APPEND_RET                                               18080000
*                                                                       18090000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'01') THEN * Append to token 2    18100000
            L     R2,G_SCAN_TOKEN2_LEN * Get length of token 2          18110000
            A     R2,G_SCAN_SPACE_COUNT * Add number of spaces          18120000
            A     R2,G_SCAN_TOKEN_LEN  * Add length of token 1          18130000
            C     R2,G_SCAN_TOKEN2_MAXLEN * Will it fit?                18140000
            IF (H) THEN                                                 18150000
               L     R3,G_SCAN_TOKEN2_MAXLEN * Get current max length   18160000
               LR    R6,R3            * Save it for storage release     18170000
               SLL   R3,1             * Multiply max length by 2        18180000
               ST    R3,G_SCAN_TOKEN2_MAXLEN * Make it new max length   18190000
               STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block     18200000
               LR    R0,R1            * Have R0 point to new block      18210000
               L     R1,G_SCAN_TOKEN2_LEN * Get length of token 2       18220000
               L     R2,G_SCAN_TOKEN2A * Have R2 point to old block     18230000
               LR    R5,R2            * Save it for storage release     18240000
               LR    R3,R1            * Make sure no cropping/filling   18250000
               ST    R0,G_SCAN_TOKEN2A * Save ptr to new block          18260000
               MVCL  R0,R2            * Copy old to new block           18270000
               STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)                    18280000
            ENDIF                                                       18290000
            L     R2,G_SCAN_TOKEN2A    * Point R2 to token 2            18300000
            A     R2,G_SCAN_TOKEN2_LEN * Add length of token 2          18310000
            L     R3,G_SCAN_TOKEN2_LEN * Get length of token 2          18320000
            A     R3,G_SCAN_SPACE_COUNT * Add number of spaces          18330000
            A     R3,G_SCAN_TOKEN_LEN  * Add length of token 1          18340000
            ST    R3,G_SCAN_TOKEN2_LEN * Put it back as new len         18350000
         ELSE                                                           18360000
            IF (CLI,G_SCAN_APPEND_TO,EQ,X'02') THEN * Append token 3    18370000
               L     R2,G_SCAN_TOKEN3_LEN * Get length of token 3       18380000
               A     R2,G_SCAN_SPACE_COUNT * Add number of spaces       18390000
               A     R2,G_SCAN_TOKEN_LEN  * Add length of token 1       18400000
               C     R2,G_SCAN_TOKEN3_MAXLEN * Will it fit?             18410000
               IF (H) THEN                                              18420000
                  L     R3,G_SCAN_TOKEN3_MAXLEN * Get current max len   18430000
                  LR    R6,R3         * Save it for storage release     18440000
                  SLL   R3,1          * Multiply max length by 2        18450000
                  ST    R3,G_SCAN_TOKEN3_MAXLEN * Make it new max len   18460000
                  STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block  18470000
                  LR    R0,R1         * Have R0 point to new block      18480000
                  L     R1,G_SCAN_TOKEN3_LEN * Get length of token 3    18490000
                  L     R2,G_SCAN_TOKEN3A * Have R2 point to old block  18500000
                  LR    R5,R2         * Save it for storage release     18510000
                  LR    R3,R1         * Make sure no cropping/filling   18520000
                  ST    R0,G_SCAN_TOKEN3A * Save ptr to new block       18530000
                  MVCL  R0,R2            * Copy old to new block        18540000
                  STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)                 18550000
               ENDIF                                                    18560000
               L     R2,G_SCAN_TOKEN3A    * Point R0 to token 3         18570000
               A     R2,G_SCAN_TOKEN3_LEN * Add length of token 3       18580000
               L     R3,G_SCAN_TOKEN3_LEN * Get length of token 3       18590000
               A     R3,G_SCAN_SPACE_COUNT * Add number of spaces       18600000
               A     R3,G_SCAN_TOKEN_LEN  * Add length of token 1       18610000
               ST    R3,G_SCAN_TOKEN3_LEN * Put it back as new len      18620000
            ENDIF                                                       18630000
         ENDIF                                                          18640000
         LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?              18650000
         IF (NZ) THEN               * Yep...                            18660000
            MVI   0(R2),C' '        * Fill in first space               18670000
            L     R1,G_SCAN_SPACE_COUNT * Put number of spaces in R1    18680000
            S     R1,=F'2'          * Minus 1 for first space and minus 18690000
*                                   * another one for EX                18700000
            IF CC=10 THEN           * If R1 >= 0                        18710000
               B     *+10           * Skip MVC constant for EX          18720000
               MVC   1(1,R2),0(R2)  * MVC constant for EX               18730000
               EX    R1,*-6         * EX previous MVC statement with R1 18740000
            ENDIF                                                       18750000
            A     R2,G_SCAN_SPACE_COUNT                                 18760000
         ENDIF                                                          18770000
         L     R4,G_SCAN_TOKENA * Point R2 to token 1                   18780000
         L     R5,G_SCAN_TOKEN_LEN * Get length of token 1              18790000
         LR    R3,R5           * Make sure no cropping/filling          18800000
         MVCL  R2,R4           * Append to either token 2 or 3          18810000
*                                                                       18820000
APPEND_RET EQU   *                                                      18830000
         MLWZTERM                 * Return back to caller               18840000
*                                                                       18850000
         LTORG                                                          18860000
*                                                                       18870000
*********************************************************************** 18880000
* Section: LWZMAKE_PHASE1                                             * 18890000
* Purpose: This section performs phase 1 of executing a makefile.     * 18900000
*          During this phase the makefile source is parsed, a linked  * 18910000
*          list of statements is created and a binary search tree of  * 18920000
*          variables.                                                 * 18930000
*          Parsing is done by invoking section LWZMAKE_SCAN_STMT      * 18940000
*          statement by statement. It invokes section                 * 18950000
*          LWZMAKE_SCAN_TOKEN which in turn invokes LWZMAKE_SCAN_CHAR.* 18960000
*          R9 should point to global data.                            * 18970000
*********************************************************************** 18980000
LWZMAKE_PHASE1 MLWZSAVE                                                 18990000
*        Trace record to start section                                  19000000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX19010000
               KE_PHASE1'                                               19020000
*                                                                       19030000
*        Write report line to start parsing                             19040000
         MLWZMRPT RPTLINE=CL133' Phase 1 parsing .....'                 19050000
*                                                                       19060000
NEXTSTMT L     R15,LWZMAKE_SCAN_STMTA_PHASE1 * Get address of scan stmt 19070000
*                                            * section                  19080000
         BASR  R14,R15            * Link to scan stmt section           19090000
*                                                                       19100000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 19110000
         BNE   BREAK_STMT_LOOP    * Yes, stop looping                   19120000
         CLI   G_MKFEOF,C'Y'      * Are we at the end of makefile?      19130000
         BE    BREAK_STMT_LOOP    * Yes, stop looping                   19140000
         B     NEXTSTMT           * In all other cases loop around      19150000
*                                                                       19160000
BREAK_STMT_LOOP EQU *                                                   19170000
*                                                                       19180000
PHASE1_RET EQU *                                                        19190000
         MLWZTERM                 * Return back to caller               19200000
*                                                                       19210000
         LTORG                                                          19220000
*                                                                       19230000
* Local constant pointers to section addresses                          19240000
LWZMAKE_SCAN_STMTA_PHASE1   DC    A(LWZMAKE_SCAN_STMT)                  19250000
*                                                                       19260000
*********************************************************************** 19270000
* Section: LWZMAKE_PHASE2                                             * 19280000
* Purpose: This section performs phase 2 of executing a makefile,     * 19290000
*          starting with the executing of the first target and        * 19300000
*          recursively going through any prerequisite targets.        * 19310000
*          R9 should point to global data.                            * 19320000
*********************************************************************** 19330000
LWZMAKE_PHASE2 MLWZSAVE                                                 19340000
*        Trace record to start section                                  19350000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX19360000
               KE_PHASE2'                                               19370000
*                                                                       19380000
*        Write report line to start executing                           19390000
         MLWZMRPT RPTLINE=CL133' Phase 2 executing ...'                 19400000
*                                                                       19410000
*        Check if a default target is filled                            19420000
         IF (CLC,G_DEFAULT_TARGET,NE,=CL72' ') THEN                     19430000
            L     R2,G_SCAN_TOKENA                                      19440000
            MVC   0(L'G_DEFAULT_TARGET,R2),G_DEFAULT_TARGET             19450000
            LA    R2,71(,R2)                                            19460000
            L     R3,=A(71)                                             19470000
TRIM_DEFAULT_TARGET_CHAR EQU *                                          19480000
            IF (CLI,0(R2),EQ,C' ') THEN                                 19490000
               BCTR  R2,R0                                              19500000
               BCT   R3,TRIM_DEFAULT_TARGET_CHAR                        19510000
            ENDIF                                                       19520000
            LA    R3,1(,R3)                                             19530000
            ST    R3,G_SCAN_TOKEN_LEN                                   19540000
*                                                                       19550000
            L     R15,LWZMAKE_FINDTGTA_PHASE2                           19560000
            BASR  R14,R15                                               19570000
*                                                                       19580000
            CLC   G_RETCODE,=F'0'                                       19590000
            BNE   PHASE2_RET                                            19600000
*                                                                       19610000
            IF (CLC,G_FOUND_TGT_PTR,EQ,=A(0)) THEN                      19620000
               MLWZMRPT RPTLINE=CL133'0Default target not found'        19630000
               MVC   G_RETCODE,=F'8'                                    19640000
               B     PHASE2_RET                                         19650000
            ENDIF                                                       19660000
         ELSE                                                           19670000
            IF (CLC,G_FIRST_TGT_PTR,NE,=A(0)) THEN                      19680000
               MVC   G_FOUND_TGT_PTR,G_FIRST_TGT_PTR                    19690000
            ELSE                                                        19700000
               MLWZMRPT RPTLINE=CL133'0No targets found'                19710000
               B     PHASE2_RET                                         19720000
            ENDIF                                                       19730000
         ENDIF                                                          19740000
*                                                                       19750000
*        Fill execute target parameter block for first target           19760000
         LA    R1,G_EXEC_TGT_PAR1 * Point R1 to parameter block         19770000
*        Put target pointer in parameter block                          19780000
         MVC   EXEC_TGT_PTR-EXEC_TGT_PAR(4,R1),G_FOUND_TGT_PTR          19790000
         ST    R1,G_EXEC_TGT_PAR1A * Store address of parameter block   19800000
         LA    R1,G_EXEC_TGT_PAR1A * Load address of param block ptr    19810000
         L     R15,LWZMAKE_EXEC_TGTA_PHASE2 * Get address of EXEC_TGT   19820000
         BASR  R14,R15             * Link to EXEC_TGT section           19830000
*                                                                       19840000
PHASE2_RET EQU *                                                        19850000
         MLWZTERM                 * Return back to caller               19860000
*                                                                       19870000
         LTORG                                                          19880000
*                                                                       19890000
* Local constant pointers to section addresses                          19900000
LWZMAKE_FINDTGTA_PHASE2     DC    A(LWZMAKE_FINDTGT)                    19910000
LWZMAKE_EXEC_TGTA_PHASE2    DC    A(LWZMAKE_EXEC_TGT)                   19920000
*                                                                       19930000
*********************************************************************** 19940000
* Section: LWZMAKE_SCAN_STMT                                          * 19950000
* Purpose: This section performs the parsing of statements. As long   * 19960000
*          as the statement is not finished it keeps calling          * 19970000
*          LWZMAKE_SCAN_TOKEN for the next keyword. In most cases the * 19980000
*          first 2 tokens are needed to determine the type of state-  * 19990000
*          ment. 2 tokens are needed to determine the type of state-  * 20000000
*          R9 should point to global data.                            * 20010000
*********************************************************************** 20020000
LWZMAKE_SCAN_STMT MLWZSAVE                                              20030000
*        Trace record to start section                                  20040000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX20050000
               KE_SCAN_STMT'                                            20060000
*                                                                       20070000
*        Before resetting scan state, remember if previous statement    20080000
*        was in recipe                                                  20090000
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN               20100000
            MVI   G_PREV_STMT_IN_RECIPE,C'Y'                            20110000
         ELSE                                                           20120000
            MVI   G_PREV_STMT_IN_RECIPE,C'N'                            20130000
         ENDIF                                                          20140000
*        Reset scan state to initial state NOT_IN_STMT                  20150000
         MVI   G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT                      20160000
*        Reset token lengths to 0                                       20170000
         MVC   G_SCAN_TOKEN_LEN,=F'0'                                   20180000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  20190000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  20200000
*                                                                       20210000
*        Get the first token                                            20220000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 20230000
         BASR  R14,R15            * Link to SCAN_TOKEN section          20240000
*                                                                       20250000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 20260000
         BNE   SCAN_STMT_RET      * Yes, stop parsing statement         20270000
         CLI   G_MKFEOF,C'Y'      * Are we at the end of makefile?      20280000
         BE    SCAN_STMT_RET      * Yes, stop parsing statement         20290000
*                                                                       20300000
*        Only a rule type statement can start with a $ variable         20310000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_VARIABLE) THEN      20320000
*           So the second token is not needed                           20330000
            BAL   R8,STMT_RULE    * Perform parsing of rule statement   20340000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20350000
*                                 * so stop parsing                     20360000
         ENDIF                                                          20370000
*                                                                       20380000
*        Only a call type statement can start with the CALL keyword     20390000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_CALL) THEN          20400000
*           So the second token is not needed                           20410000
            BAL   R8,STMT_CALL    * Perform parsing of call statement   20420000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20430000
*                                 * so stop parsing                     20440000
         ENDIF                                                          20450000
*                                                                       20460000
*        If first token is .PHONY the statement is a PHONY              20470000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_SPECIAL) THEN       20480000
            CLC   G_SCAN_TOKEN_LEN,=A(6)                                20490000
            IF (EQ) THEN                                                20500000
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1        20510000
               CLC   0(6,R14),=C'.PHONY'  * Is it .PHONY?               20520000
               IF (EQ) THEN               * If so...                    20530000
                  BAL   R8,STMT_PHONY     * Perform parsing PHONY stmt  20540000
                  B     SCAN_STMT_RET     * Statement parsed in         20550000
*                                         * subroutine so stop parsing  20560000
               ENDIF                                                    20570000
            ENDIF                                                       20580000
         ENDIF                                                          20590000
*                                                                       20600000
*        Copy token 1 to token 2 so it can be used for next SCAN_TOKEN  20610000
         MVI   G_SCAN_APPEND_TO,X'01'                                   20620000
         MVC   G_SCAN_SPACE_COUNT,=A(0)                                 20630000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   20640000
         BASR  R14,R15            * Link to APPEND_TOKEN section        20650000
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type     20660000
*                                                                       20670000
*        Clear scan state except for left most bit indicating in recipe 20680000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        20690000
*        Set scan state bits to IN_STMT                                 20700000
         OI    G_SCAN_STATE,SCAN_STATE_IN_STMT                          20710000
*                                                                       20720000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 20730000
         BASR  R14,R15            * Link to SCAN_TOKEN section          20740000
*                                                                       20750000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 20760000
         BNE   SCAN_STMT_RET      * Yes, stop parsing statement         20770000
*                                                                       20780000
*        If second token is an operator the statement is an assignment  20790000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_OPERATOR) THEN      20800000
            BAL   R8,STMT_ASSIGNMENT * Perform parsing of assignment    20810000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20820000
*                                 * so stop parsing                     20830000
         ENDIF                                                          20840000
*                                                                       20850000
*        If second token is a rule separator the statement is a rule    20860000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN          20870000
            BAL   R8,STMT_RULE    * Perform parsing of rule statement   20880000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20890000
*                                 * so stop parsing                     20900000
         ENDIF                                                          20910000
*                                                                       20920000
*        If first and second token are both regular keywords the        20930000
*        statement is a rule                                            20940000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN        20950000
            IF (CLI,G_SCAN_TOKENTYPE2,EQ,SCAN_TOKENTYPE_NORMAL) THEN    20960000
               BAL   R8,STMT_RULE  * Perform parsing of rule statement  20970000
               B     SCAN_STMT_RET * Statement parsed in subroutine     20980000
*                                  * so stop parsing                    20990000
            ENDIF                                                       21000000
         ENDIF                                                          21010000
*                                                                       21020000
*        No valid combination of keywords found, so report syntax error 21030000
*        and give off return code 8                                     21040000
         MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'             21050000
         MVC   G_RETCODE,=F'8'                                          21060000
*                                                                       21070000
SCAN_STMT_RET EQU *                                                     21080000
         MLWZTERM                 * Return back to caller               21090000
*                                                                       21100000
* STMT assignment (e.g. 'foo = bar')                                    21110000
* At this point 2 tokens have been scanned, the assignment destination, 21120000
* which has been copied to token 2 and the operator still in token 1.   21130000
* From here on the source text (right side of the assignment) is parsed 21140000
* into token 3. When the statement is finished it's converted to        21150000
* internal memory format and added to the statement linked list. Also   21160000
* the variable and it's current value are added/updated to the variable 21170000
* binary search tree.                                                   21180000
*                                                                       21190000
STMT_ASSIGNMENT EQU *                                                   21200000
*        Write a trace record for statement type assignment             21210000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'ASSIGNX21220000
               MENT'                                                    21230000
*                                                                       21240000
*        Save operator so token 1 can be reused                         21250000
         L     R5,G_SCAN_TOKENA   * Point R5 to token 1                 21260000
         L     R6,G_SCAN_TOKEN_LEN * Put token 1 length in R6           21270000
         C     R6,=F'2'           * Was the operator 2 bytes?           21280000
         IF (EQ) THEN             * If so...                            21290000
            MVC   G_STMT_SAVE_OP(2),0(R5) * copy 2 bytes                21300000
         ELSE                     * otherwise...                        21310000
            MVC   G_STMT_SAVE_OP(1),0(R5) * copy 1 byte                 21320000
            MVI   G_STMT_SAVE_OP+1,X'00'  * and add a null char         21330000
         ENDIF                                                          21340000
*                                                                       21350000
*        Clear scan state except for left most bit indicating in recipe 21360000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        21370000
*        Set scan state bits to IN_ASSIGN                               21380000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ASSIGN                        21390000
*        Clear token 3 length, which will receive the assignment source 21400000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  21410000
*                                                                       21420000
STMT_A_NEXT_TOKEN EQU *                                                 21430000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 21440000
         BASR  R14,R15            * Link to SCAN_TOKEN section          21450000
*                                                                       21460000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 21470000
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement        21480000
*                                                                       21490000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     21500000
*        statement was finished                                         21510000
         IC    R14,G_SCAN_STATE   * Get the scan state                  21520000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   21530000
*                                 * the high order bit in the scan      21540000
*                                 * state for 'in recipe'               21550000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   21560000
         BE    STMT_A_FINISH      * If so, statement done               21570000
*                                                                       21580000
*        Check if we've hit a $() variable and we're in a simply        21590000
*        expanded variable assignment (:=). In that case the variable   21600000
*        is immediately resolved.                                       21610000
         IC    R14,G_SCAN_STATE   * Get the scan state                  21620000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   21630000
*                                 * the high order bit in the scan      21640000
*                                 * state for 'in recipe'               21650000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()   21660000
         IF (NE) THEN                                                   21670000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       21680000
         ENDIF                                                          21690000
         IF (EQ) THEN             * If so...                            21700000
            CLC   G_STMT_SAVE_OP,=C':=' * Check for simply expanded     21710000
            IF (EQ) THEN          * If so...                            21720000
               MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1     21730000
               MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces  21740000
               L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR  21750000
               BASR  R14,R15      * Link to SCAN_VAR section            21760000
*                                                                       21770000
               CLC   G_RETCODE,=F'0' * Did an error occur?              21780000
               BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement  21790000
*                                                                       21800000
               B     STMT_A_NEXT_TOKEN * Loop around to get next token  21810000
            ENDIF                                                       21820000
         ENDIF                                                          21830000
*                                                                       21840000
*        Append token 1 to token 3                                      21850000
         MVI   G_SCAN_APPEND_TO,X'02'                                   21860000
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3        21870000
         IF (Z) THEN                * Is this the first part of token 3 21880000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  21890000
         ENDIF                                                          21900000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   21910000
         BASR  R14,R15            * Link to APPEND_TOKEN section        21920000
*                                                                       21930000
*        Clear scan state except for left most bit indicating in recipe 21940000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        21950000
*        Set scan state bits to IN_ASSIGN2                              21960000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ASSIGN2                       21970000
*                                                                       21980000
         B     STMT_A_NEXT_TOKEN  * Loop around to get next token       21990000
*                                                                       22000000
STMT_A_FINISH EQU *                                                     22010000
*        Write trace record that statement is finished                  22020000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                22030000
*                                                                       22040000
*        Allocate a new memory block for this assignment                22050000
         L     R1,=A(STMT_A_DSECT_LEN) * Size of block without token    22060000
         A     R1,G_SCAN_TOKEN3_LEN    * Add token length               22070000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    22080000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_ASSIGNMENT * New block is    22090000
*                                 * for type assignment                 22100000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 22110000
         BASR  R14,R15            * Link to ALLOC_STMT section          22120000
*                                                                       22130000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 22140000
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement        22150000
*                                                                       22160000
*        Get returned pointer to new block of memory and put in in R7   22170000
*        It should stay in R7 for the LWZMAKE_STORE_VAR section         22180000
         LT    R7,G_STMT_ALLOC_RETURN_PTR                               22190000
         BZ    STMT_ASSIGNMENT_RET * If it was zero, stop parsing       22200000
*                                 * (failsafe, shouldn't happen)        22210000
*                                                                       22220000
         USING STMT_A_DSECT,R7    * Address with assignment DSECT       22230000
*                                                                       22240000
*        Fill in the operator in assignment block                       22250000
         MVC   STMT_A_OPERATOR,G_STMT_SAVE_OP * Copy 2 bytes operator   22260000
*                                                                       22270000
*        Fill in destination variable name (token 2) in assignment blk  22280000
         L     R6,G_SCAN_TOKEN2_LEN * Get length of variable name       22290000
         C     R6,=A(L'STMT_A_DEST) * Check if it fits                  22300000
         IF (H) THEN                * If not, write error and stop      22310000
            MLWZMRPT RPTLINE=CL133'0Internal error, variable name longeX22320000
               r than 72',APND_LC=C'Y'                                  22330000
            MVC   G_RETCODE,=F'12'  * Set return code 12                22340000
            BR    R8                * and return                        22350000
         ENDIF                                                          22360000
         STH   R6,STMT_A_DESTLEN  * Put variable name length in block   22370000
         L     R5,G_SCAN_TOKEN2A  * Point R5 to token 2                 22380000
         LA    R4,STMT_A_DEST     * Point R4 to var name in block       22390000
         BCTR  R6,R0              * Length minus 1 for EX               22400000
         B     *+10               * Skip MVC constant for EX            22410000
         MVC   0(1,R4),0(R5)      * MVC constant for EX                 22420000
         EX    R6,*-6             * EX previous MVC statement with R6   22430000
*                                                                       22440000
*        Fill in source text (token 3) in assignment block              22450000
         LA    R0,STMT_A_SRC        * Point R0 to source in block       22460000
         L     R1,G_SCAN_TOKEN3_LEN * Get length of source              22470000
         STH   R1,STMT_A_SRCLEN     * Store length in block             22480000
         L     R2,G_SCAN_TOKEN3A    * Point R2 to token 3               22490000
         LR    R3,R1                * Make sure no cropping/filling     22500000
         MVCL  R0,R2                * Copy source text                  22510000
*                                                                       22520000
*        Check for assignment of special var                            22530000
         L     R14,G_SCAN_TOKEN2A * Point R14 to token 2                22540000
         IF (CLI,0(R14),EQ,C'.') THEN * If token 2 starts with .        22550000
            CLC   G_SCAN_TOKEN2_LEN,=F'13' * Is length 13?              22560000
            IF (EQ) THEN              * Yes, so it can be recipepref    22570000
               CLC   0(13,R14),=C'.RECIPEPREFIX' * Is it?               22580000
               BNE   STMT_ASSIGNMENT_UNKNOWN_SPECIAL * No, error        22590000
               CLC   G_SCAN_TOKEN3_LEN,=F'1' * Was source text 1 pos?   22600000
               BNE   STMT_ASSIGNMENT_WRONG_REPPREFLEN * No, error       22610000
               L     R14,G_SCAN_TOKEN3A    * Point R14 to token 3       22620000
               MVC   G_RECIPEPREFIX,0(R14) * Copy recipeprefix          22630000
               B     STMT_ASSIGNMENT_RET   * Skip the rest              22640000
            ELSE                      * Length is not 13                22650000
               B     STMT_ASSIGNMENT_UNKNOWN_SPECIAL * so error         22660000
            ENDIF                                                       22670000
         ENDIF                                                          22680000
*                                                                       22690000
*        Add/update the variable to binary search tree for vars, but    22700000
*        not if we're in a recipe                                       22710000
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN               22720000
*           R7 points to the assignment statement                       22730000
            L     R15,LWZMAKE_STORE_VARA_STMT * Get address STORE_VAR   22740000
            BASR  R14,R15              * Link to STORE_VAR section      22750000
         ENDIF                                                          22760000
*                                                                       22770000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 22780000
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement        22790000
*                                                                       22800000
*        Remember this was an assignment for next statement's previous  22810000
*        statement type                                                 22820000
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_ASSIGNMENT                    22830000
*                                                                       22840000
STMT_ASSIGNMENT_RET EQU *                                               22850000
         BR    R8                                                       22860000
*                                                                       22870000
STMT_ASSIGNMENT_UNKNOWN_SPECIAL EQU *                                   22880000
         MLWZMRPT RPTLINE=CL133'0Unknown special variable',APND_LC=C'Y' 22890000
         MVC   G_RETCODE,=F'8'                                          22900000
         BR    R8                                                       22910000
*                                                                       22920000
STMT_ASSIGNMENT_WRONG_REPPREFLEN EQU *                                  22930000
         MLWZMRPT RPTLINE=CL133'0Recipeprefix can only be 1 character',X22940000
               APND_LC=C'Y'                                             22950000
         MVC   G_RETCODE,=F'8'                                          22960000
         BR    R8                                                       22970000
*                                                                       22980000
         DROP  R7                                                       22990000
*                                                                       23000000
* STMT rule 'bla : jodel'                                               23010000
* At this point 1 or 2 tokens have been scanned, 1 in the case the      23020000
* statement starts with a $() variable, 2 in either the case of a       23030000
* token followed by a rule separator (:), or in the case of 2 normal    23040000
* tokens.                                                               23050000
* From here on keywords are parsed and appended to token 2 until a rule 23060000
* separator is encountered, so that token 2 contains the target name(s) 23070000
* After that keywords are parsed and appended to token 3 until end of   23080000
* statement, so that token 3 contains the requisite(s).                 23090000
* Any variables encountered in the target name(s) are resolved here,    23100000
* but variables in the requisites are left intact and are resolved in   23110000
* phase 2.                                                              23120000
* When the statement is finished it's converted to internal memory      23130000
* format and added to the statement linked list.                        23140000
*                                                                       23150000
STMT_RULE EQU  *                                                        23160000
*        Write a trace record for statement type rule                   23170000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'RULE'  23180000
*                                                                       23190000
*        Check if we already have a rule separator, if so save it as    23200000
*        the operator and skip to target token complete                 23210000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN          23220000
            MVI   G_STMT_SAVE_OP,C':' * Save : as operator              23230000
            MVI   G_STMT_SAVE_OP+1,X'00' * Append null char             23240000
            B     STMT_R_TGT_TOKEN_COMPLETE * Skip parsing target name  23250000
         ELSE                                                           23260000
            MVC   G_STMT_SAVE_OP,=X'0000' * Initialize operator         23270000
*           Check if we ended up here because statement started with    23280000
*           a $() variable, in that case do a nasty jump right into     23290000
*           the keyword scanning loop below to expand this variable     23300000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_VARIABLE              23310000
            BE    STMT_R_SCAN_VAR                                       23320000
         ENDIF                                                          23330000
*                                                                       23340000
STMT_R_NEXT_TOKEN EQU *                                                 23350000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 23360000
         BASR  R14,R15            * Link to SCAN_TOKEN section          23370000
*                                                                       23380000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 23390000
         BNE   STMT_RULE_RET      * Yes, stop parsing statement         23400000
*                                                                       23410000
*        Check if we have a rule separator, if so save it as the        23420000
*        operator and skip to target token complete                     23430000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN          23440000
            MVI   G_STMT_SAVE_OP,C':' * Save : as operator              23450000
            MVI   G_STMT_SAVE_OP+1,X'00' * Append null char             23460000
            B     STMT_R_TGT_TOKEN_COMPLETE * Skip parsing target name  23470000
         ENDIF                                                          23480000
*                                                                       23490000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     23500000
*        section to expand it and loop around for the next keyword      23510000
         IC    R14,G_SCAN_STATE   * Get the scan state                  23520000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   23530000
*                                 * the high order bit in the scan      23540000
*                                 * state for 'in recipe'               23550000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   23560000
         IF (EQ) THEN             * If so...                            23570000
STMT_R_SCAN_VAR EQU *                                                   23580000
            MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1        23590000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1' * Preserve 1 space    23600000
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address of SCAN_VAR  23610000
            BASR  R14,R15         * Link to SCAN_VAR section            23620000
*                                                                       23630000
            CLC   G_RETCODE,=F'0' * Did an error occur?                 23640000
            BNE   STMT_RULE_RET   * Yes, stop parsing statement         23650000
*                                                                       23660000
            B     STMT_R_NEXT_TOKEN * Loop around to get next token     23670000
         ENDIF                                                          23680000
*                                                                       23690000
*        Append token 1 to token 2                                      23700000
         MVI   G_SCAN_APPEND_TO,X'01'                                   23710000
         LT    R1,G_SCAN_TOKEN2_LEN  * Get current length token 2       23720000
         IF (Z) THEN                * Is this the first part of token 2 23730000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  23740000
         ELSE                                                           23750000
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?           23760000
            IF (NZ) THEN               * Yep...                         23770000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           23780000
            ENDIF                                                       23790000
         ENDIF                                                          23800000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   23810000
         BASR  R14,R15            * Link to APPEND_TOKEN section        23820000
*                                                                       23830000
         B     STMT_R_NEXT_TOKEN  * Loop around to get next token       23840000
*                                                                       23850000
STMT_R_TGT_TOKEN_COMPLETE EQU *                                         23860000
*        Clear scan state except for left most bit indicating in recipe 23870000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        23880000
*        Set scan state bits to IN_RULE2, meaning target name complete  23890000
         OI    G_SCAN_STATE,SCAN_STATE_IN_RULE2                         23900000
*        Clear token 3 length, which will receive the requisites        23910000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  23920000
*                                                                       23930000
STMT_R_NEXT_TOKEN2 EQU *                                                23940000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 23950000
         BASR  R14,R15            * Link to SCAN_TOKEN section          23960000
*                                                                       23970000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 23980000
         BNE   STMT_RULE_RET      * Yes, stop parsing statement         23990000
*                                                                       24000000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     24010000
*        statement was finished                                         24020000
         IC    R14,G_SCAN_STATE   * Get the scan state                  24030000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   24040000
*                                 * the high order bit in the scan      24050000
*                                 * state for 'in recipe'               24060000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   24070000
         BE    STMT_R_FINISH      * If so, statement done               24080000
*                                                                       24090000
*        Check if we've hit a $() variable, if so parse that with the   24100000
*        same section that resolves them. Setting append to token 3     24110000
*        causes it not to resolve but simple parse and append.          24120000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()   24130000
         IF (EQ) THEN             * If so...                            24140000
            MVI   G_SCAN_APPEND_TO,X'02' * Set append to token 3        24150000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1' * Preserve 1 space    24160000
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address of SCAN_VAR  24170000
            BASR  R14,R15         * Link to SCAN_VAR section            24180000
*                                                                       24190000
            CLC   G_RETCODE,=F'0' * Did an error occur?                 24200000
            BNE   STMT_RULE_RET   *  Yes, stop parsing statement        24210000
*                                                                       24220000
            B     STMT_R_NEXT_TOKEN2 * Loop around to get next token    24230000
         ENDIF                                                          24240000
*                                                                       24250000
*        Append token 1 to token 3                                      24260000
         MVI   G_SCAN_APPEND_TO,X'02'                                   24270000
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3        24280000
         IF (Z) THEN                * Is this the first part of token 3 24290000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  24300000
         ELSE                                                           24310000
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?           24320000
            IF (NZ) THEN               * Yep...                         24330000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           24340000
            ENDIF                                                       24350000
         ENDIF                                                          24360000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   24370000
         BASR  R14,R15            * Link to APPEND_TOKEN section        24380000
*                                                                       24390000
*        Set scan scate tot RULE3                                       24400000
         IC    R14,G_SCAN_STATE   * Get the scan state                  24410000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   24420000
*                                 * the high order bit in the scan      24430000
*                                 * state for 'in recipe'               24440000
         C     R14,=A(SCAN_STATE_IN_RULE2) * Check for RULE2            24450000
         IF (EQ) THEN             * Only RULE2 can change to RULE3      24460000
*           Clear scan state except for left most bit for in recipe     24470000
            NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                     24480000
*           Set scan state bits to IN_RULE3, meaning in requisites      24490000
            OI    G_SCAN_STATE,SCAN_STATE_IN_RULE3                      24500000
         ENDIF                                                          24510000
*                                                                       24520000
         B     STMT_R_NEXT_TOKEN2 * Loop around for the next token      24530000
*                                                                       24540000
STMT_R_FINISH EQU *                                                     24550000
*        Write trace record that statement is finished                  24560000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                24570000
*                                                                       24580000
*        Allocate a new memory block for this rule                      24590000
         L     R1,=A(STMT_R_DSECT_LEN) * Size of block without tokens   24600000
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of target name(s)   24610000
         A     R1,G_SCAN_TOKEN3_LEN    * Add length of requisites       24620000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    24630000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_RULE * New block is for type 24640000
*                                               * rule                  24650000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 24660000
         BASR  R14,R15            * Link to ALLOC_STMT section          24670000
*                                                                       24680000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 24690000
         BNE   STMT_RULE_RET      * Yes, stop parsing statement         24700000
*                                                                       24710000
*        Get returned pointer to new block of memory and put in in R7   24720000
*        It should stay in R7 for the LWZMAKE_STORE_TGT section         24730000
         LT    R7,G_STMT_ALLOC_RETURN_PTR                               24740000
         BZ    STMT_RULE_RET      * If it was zero, stop parsing        24750000
*                                 * (failsafe, shouldn't happen)        24760000
*                                                                       24770000
         USING STMT_R_DSECT,R7    * Address with rule DSECT             24780000
*                                                                       24790000
*        Copy target name(s) to memory block of rule statement          24800000
         LA    R0,STMT_R_TGT      * Point R0 to start of target         24810000
         L     R1,G_SCAN_TOKEN2_LEN * Get target length                 24820000
         STH   R1,STMT_R_TGTLEN   * Put target length in block          24830000
         L     R2,G_SCAN_TOKEN2A  * Point R2 to token 2                 24840000
         LR    R3,R1              * Make sure no cropping/filling       24850000
         MVCL  R0,R2              * Copy target name(s) to block        24860000
*                                                                       24870000
*        Copy requisite name(s) to memory block of rule statement       24880000
         LA    R0,STMT_R_TGT      * Point R0 to start of target         24890000
         AH    R0,STMT_R_TGTLEN   * Add target length, so now points to 24900000
*                                 * start of requisite                  24910000
         L     R1,G_SCAN_TOKEN3_LEN * Get requisite length              24920000
         STH   R1,STMT_R_REQLEN   * Put requisite length in block       24930000
         L     R2,G_SCAN_TOKEN3A  * Point R2 to token 3                 24940000
         LR    R3,R1              * Make sure no cropping/filling       24950000
         MVCL  R0,R2              * Copy requisite name(s) to block     24960000
*                                                                       24970000
*        Split up space delimited target name(s) and for each name link 24980000
*        to STORE_TGT to allocate a target block and add it to the      24990000
*        target binary search tree.                                     25000000
         LA    R2,STMT_R_TGT      * Point R2 to target name(s)          25010000
         XR    R3,R3              * Clear R3                            25020000
         LH    R3,STMT_R_TGTLEN   * Put length of target name(s) in R3  25030000
STMT_R_SCAN_NEXT_TGT EQU *                                              25040000
         L     R4,G_SCAN_TOKENA   * Point R4 to token 1                 25050000
         XR    R5,R5              * Clear R5                            25060000
         ST    R5,G_SCAN_TOKEN_LEN * And clear length token 1           25070000
STMT_R_TGT_BLANK EQU *                                                  25080000
         IF (CLI,0(R2),EQ,C' ') THEN * Current pos a space?             25090000
            LA    R2,1(,R2)       * Skip space char                     25100000
            BCT   R3,STMT_R_TGT_BLANK * R3 = R3 - 1 until R3 = 0        25110000
            B     STMT_R_STORE_DONE * No more chars, done storing TGTs  25120000
         ENDIF                                                          25130000
STMT_R_TGT_NONBLANK EQU *                                               25140000
         MVC    0(1,R4),0(R2)     * Copy char to token 1                25150000
         LA     R4,1(,R4)         * Advance current pos token 1         25160000
         LA     R5,1(,R5)         * R5 = R5 - 1                         25170000
         BCTR   R3,R0             * R3 = R3 - 1                         25180000
         C      R3,=F'0'          * At end of target name(s)?           25190000
         IF (H) THEN              * If not...                           25200000
            LA     R2,1(,R2)      * Advance current pos target name(s)  25210000
            CLI    0(R2),C' '     * Current pos a space?                25220000
            BNE    STMT_R_TGT_NONBLANK * Loop around to copy next char  25230000
         ENDIF                                                          25240000
*        Either a space was found or we've reached the end of targets   25250000
         ST     R5,G_SCAN_TOKEN_LEN * Store target length               25260000
*                                                                       25270000
*        Add the target to binary search tree for targets               25280000
*        R7 points to the rule statement                                25290000
*        Token 1 contains one target name                               25300000
         L     R15,LWZMAKE_STORE_TGTA_STMT * Get address of STORE_TGT   25310000
         BASR  R14,R15            * Link to STORE_TGT section           25320000
*                                                                       25330000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 25340000
         BNE   STMT_RULE_RET      * Yes, stop storing targets           25350000
*                                                                       25360000
         C      R3,=F'0'          * More chars left in target name(s)?  25370000
         BH     STMT_R_SCAN_NEXT_TGT * If so, look for next target      25380000
*                                                                       25390000
         DROP  R7                                                       25400000
*                                                                       25410000
STMT_R_STORE_DONE EQU *                                                 25420000
*        Remember this was a rule for next statement's previous         25430000
*        statement type                                                 25440000
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_RULE                          25450000
*                                                                       25460000
STMT_RULE_RET EQU *                                                     25470000
         BR    R8                 * Return                              25480000
*                                                                       25490000
* STMT call 'CALL routine'                                              25500000
* At this point 1 token has been scanned, which is the CALL keyword.    25510000
* From here on the first next token will be the REXX exec called, which 25520000
* is stored in token 2. Any tokens after that are concatenated in token 25530000
* 3 which becomes the (optional) parameter to the REXX exec.            25540000
* When the statement is finished it's converted to internal memory      25550000
* format and added to the statement linked list.                        25560000
*                                                                       25570000
STMT_CALL EQU  *                                                        25580000
*        Write a trace record for statement type call                   25590000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'CALL'  25600000
*                                                                       25610000
*        Clear scan state except for left most bit indicating in recipe 25620000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        25630000
*        Set scan state bits to IN_CALL                                 25640000
         OI    G_SCAN_STATE,SCAN_STATE_IN_CALL                          25650000
*        Clear token 2 length, which will receive the REXX exec name    25660000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  25670000
*                                                                       25680000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 25690000
         BASR  R14,R15            * Link to SCAN_TOKEN section          25700000
*                                                                       25710000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 25720000
         BNE   STMT_CALL_RET      * Yes, stop parsing statement         25730000
*                                                                       25740000
         L     R2,G_SCAN_TOKEN_LEN * Get length of token 1              25750000
         C     R2,=A(8)           * Longer than 8 positions?            25760000
         IF (H) THEN              * If so...                            25770000
            MLWZMRPT RPTLINE=CL133'0REXX exec cannot be longer than 8 cX25780000
               haracters',APND_LC=C'Y'                                  25790000
            MVC   G_RETCODE,=F'8' * Set return code 8                   25800000
            BR    R8              * and return                          25810000
         ENDIF                                                          25820000
*                                                                       25830000
*        Copy token 1 to token 2                                        25840000
         MVI   G_SCAN_APPEND_TO,X'01'                                   25850000
         MVC   G_SCAN_SPACE_COUNT,=A(0)                                 25860000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   25870000
         BASR  R14,R15            * Link to APPEND_TOKEN section        25880000
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type     25890000
*                                                                       25900000
*        Clear scan state except for left most bit indicating in recipe 25910000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        25920000
*        Set scan state bits to IN_CALL2                                25930000
         OI    G_SCAN_STATE,SCAN_STATE_IN_CALL2                         25940000
*        Clear token 2 length, which will receive the REXX exec parm    25950000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  25960000
*                                                                       25970000
STMT_C_NEXT_TOKEN EQU *                                                 25980000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 25990000
         BASR  R14,R15            * Link to SCAN_TOKEN section          26000000
*                                                                       26010000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 26020000
         BNE   STMT_CALL_RET      * Yes, stop parsing statement         26030000
*                                                                       26040000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     26050000
*        statement was finished                                         26060000
         IC    R14,G_SCAN_STATE   * Get the scan state                  26070000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   26080000
*                                 * the high order bit in the scan      26090000
*                                 * state for 'in recipe'               26100000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   26110000
         BE    STMT_C_FINISH      * If so, statement done               26120000
*                                                                       26130000
*        Check if we've hit a $() variable                              26140000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()   26150000
         IF (NE) THEN                                                   26160000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       26170000
         ENDIF                                                          26180000
         IF (EQ) THEN             * If so...                            26190000
            MVI   G_SCAN_APPEND_TO,X'02' * Set append to token 3        26200000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces     26210000
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR     26220000
            BASR  R14,R15         * Link to SCAN_VAR section            26230000
*                                                                       26240000
            CLC   G_RETCODE,=F'0' * Did an error occur?                 26250000
            BNE   STMT_CALL_RET   * Yes, stop parsing statement         26260000
*                                                                       26270000
            B     STMT_C_NEXT_TOKEN * Loop around to get next token     26280000
         ENDIF                                                          26290000
*                                                                       26300000
*        Append token 1 to token 3, leading spaces since last token are 26310000
*        preserved (tokenizer counts them in G_SCAN_SPACE_COUNT)        26320000
         MVI   G_SCAN_APPEND_TO,X'02'                                   26330000
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3        26340000
         IF (Z) THEN                * Is this the first part of token 3 26350000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  26360000
         ELSE                                                           26370000
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?           26380000
            IF (NZ) THEN               * Yep...                         26390000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           26400000
            ENDIF                                                       26410000
         ENDIF                                                          26420000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   26430000
         BASR  R14,R15            * Link to APPEND_TOKEN section        26440000
*                                                                       26450000
         B     STMT_C_NEXT_TOKEN  * Loop around for the next token      26460000
*                                                                       26470000
STMT_C_FINISH EQU *                                                     26480000
*        Write trace record that statement is finished                  26490000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                26500000
*                                                                       26510000
*        Allocate a new memory block for this call                      26520000
         L     R1,=A(STMT_C_DSECT_LEN) * Size of block without tokens   26530000
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of REXX exec name   26540000
         A     R1,G_SCAN_TOKEN3_LEN    * Add length of REXX exec parm   26550000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    26560000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_CALL * New block is for type 26570000
*                                               * call                  26580000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 26590000
         BASR  R14,R15            * Link to ALLOC_STMT section          26600000
*                                                                       26610000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 26620000
         BNE   STMT_CALL_RET      * Yes, stop parsing statement         26630000
*                                                                       26640000
         LT    R7,G_STMT_ALLOC_RETURN_PTR * Get returned ptr to new     26650000
*                                         * block of memory             26660000
         BZ    STMT_CALL_RET      * If it was zero, stop parsing        26670000
*                                 * (failsafe, shouldn't happen)        26680000
*                                                                       26690000
         USING STMT_C_DSECT,R7    * Address with call DSECT             26700000
*                                                                       26710000
*        Copy REXX exec name                                            26720000
         LA    R2,STMT_C_EXEC     * Point R2 to start of exec           26730000
         L     R3,G_SCAN_TOKEN2A  * Point R3 to token 1                 26740000
         L     R4,G_SCAN_TOKEN2_LEN * Get length of token 2             26750000
         STH   R4,STMT_C_EXECLEN  * Store exec length in block          26760000
         BCTR  R4,R0              * Minus 1 for EX                      26770000
         B     *+10               * Skip MVC constant for EX            26780000
         MVC   0(1,R2),0(R3)      * MVC constant for EX                 26790000
         EX    R4,*-6             * EX previous MVC statement with R4   26800000
*                                                                       26810000
*        Copy REXX exec parameter                                       26820000
         LA    R0,STMT_C_EXEC     * Point R0 to start of exec in block  26830000
         AH    R0,STMT_C_EXECLEN  * Advance to start of parm in block   26840000
         L     R1,G_SCAN_TOKEN3_LEN * Get length of exec parm           26850000
         STH   R1,STMT_C_PARMLEN  * Store length of exec parm in block  26860000
         L     R2,G_SCAN_TOKEN3A  * Point R2 to token 3                 26870000
         LR    R3,R1              * Make sure no cropping/filling       26880000
         MVCL  R0,R2              * Copy REXX exec parm to block        26890000
*                                                                       26900000
*        Remember this was a call for next statement's previous         26910000
*        statement type                                                 26920000
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_CALL                          26930000
*                                                                       26940000
STMT_CALL_RET EQU *                                                     26950000
         BR    R8                 * Return                              26960000
*                                                                       26970000
         DROP  R7                                                       26980000
*                                                                       26990000
* STMT PHONY '.PHONY targetname'                                        27000000
* At this point 1 token has been scanned, which is the .PHONY keyword.  27010000
* From here on the first next token will be the PHONY target name.      27020000
* When the statement is finished it's converted to internal memory      27030000
* format and added to the statement linked list.                        27040000
* The PHONY target name is added to the binary search tree for phonies. 27050000
*                                                                       27060000
STMT_PHONY EQU  *                                                       27070000
*        Write a trace record for statement type call                   27080000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'PHONY' 27090000
*                                                                       27100000
*        Clear scan state except for left most bit indicating in recipe 27110000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        27120000
*        Set scan state bits to IN_PHONY                                27130000
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY                         27140000
*        Clear token 2 length, which will receive the PHONY target name 27150000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  27160000
*                                                                       27170000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 27180000
         BASR  R14,R15            * Link to SCAN_TOKEN section          27190000
*                                                                       27200000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 27210000
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement         27220000
*                                                                       27230000
         IF (CLI,G_SCAN_TOKENTYPE,NE,SCAN_TOKENTYPE_NORMAL) THEN        27240000
            MLWZMRPT RPTLINE=CL133'0.PHONY must be followed by a constaX27250000
               nt target name',APND_LC=C'Y'                             27260000
            MVC   G_RETCODE,=F'8' * Set return code 8                   27270000
            BR    R8              * and return                          27280000
         ENDIF                                                          27290000
*                                                                       27300000
*        Copy token 1 to token 2                                        27310000
         MVI   G_SCAN_APPEND_TO,X'01'                                   27320000
         MVC   G_SCAN_SPACE_COUNT,=A(0)                                 27330000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   27340000
         BASR  R14,R15            * Link to APPEND_TOKEN section        27350000
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type     27360000
*                                                                       27370000
*        Clear scan state except for left most bit indicating in recipe 27380000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        27390000
*        Set scan state bits to IN_PHONY2                               27400000
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY2                        27410000
*                                                                       27420000
STMT_P_NEXT_TOKEN EQU *                                                 27430000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 27440000
         BASR  R14,R15            * Link to SCAN_TOKEN section          27450000
*                                                                       27460000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 27470000
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement         27480000
*                                                                       27490000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     27500000
*        statement was finished                                         27510000
         IC    R14,G_SCAN_STATE   * Get the scan state                  27520000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   27530000
*                                 * the high order bit in the scan      27540000
*                                 * state for 'in recipe'               27550000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   27560000
         BNE   STMT_P_NEXT_TOKEN  * If not so, loop around              27570000
*                                                                       27580000
STMT_P_FINISH EQU *                                                     27590000
*        Write trace record that statement is finished                  27600000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                27610000
*                                                                       27620000
*        Allocate a new memory block for this PHONY                     27630000
         L     R1,=A(STMT_P_DSECT_LEN) * Size of block without tokens   27640000
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of PHONY name       27650000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    27660000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_PHONY * New block is for     27670000
*                                                * type PHONY           27680000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 27690000
         BASR  R14,R15            * Link to ALLOC_STMT section          27700000
*                                                                       27710000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 27720000
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement         27730000
*                                                                       27740000
*        Get returned pointer to new block of memory and put in in R7   27750000
*        It should stay in R7 for the LWZMAKE_STORE_PNY section         27760000
         LT    R7,G_STMT_ALLOC_RETURN_PTR                               27770000
         BZ    STMT_PHONY_RET     * If it was zero, stop parsing        27780000
*                                 * (failsafe, shouldn't happen)        27790000
*                                                                       27800000
         USING STMT_P_DSECT,R7    * Address with PHONY DSECT            27810000
*                                                                       27820000
*        Copy PHONY name to memory block of PHONY statement             27830000
         LA    R0,STMT_P_PNY      * Point R0 to start of PHONY          27840000
         L     R1,G_SCAN_TOKEN2_LEN * Get PHONY length                  27850000
         STH   R1,STMT_P_PNYLEN   * Put PHONY length in block           27860000
         L     R2,G_SCAN_TOKEN2A  * Point R2 to token 2                 27870000
         LR    R3,R1              * Make sure no cropping/filling       27880000
         MVCL  R0,R2              * Copy PHONY name to block            27890000
*                                                                       27900000
*        R7 points to the PHONY statement                               27910000
         L     R15,LWZMAKE_STORE_PNYA_STMT * Get address STORE_PNY      27920000
         BASR  R14,R15            * Link to STORE_PNY section           27930000
*                                                                       27940000
STMT_PHONY_RET EQU *                                                    27950000
         BR    R8                 * Return                              27960000
*                                                                       27970000
         LTORG                                                          27980000
*                                                                       27990000
* Local constant pointers to section addresses                          28000000
LWZMAKE_SCAN_TOKENA_STMT   DC    A(LWZMAKE_SCAN_TOKEN)                  28010000
LWZMAKE_APPEND_TOKENA_STMT DC    A(LWZMAKE_APPEND_TOKEN)                28020000
LWZMAKE_SCAN_VARA_STMT     DC    A(LWZMAKE_SCAN_VAR)                    28030000
LWZMAKE_ALLOC_STMTA_STMT   DC    A(LWZMAKE_ALLOC_STMT)                  28040000
LWZMAKE_STORE_VARA_STMT    DC    A(LWZMAKE_STORE_VAR)                   28050000
LWZMAKE_STORE_TGTA_STMT    DC    A(LWZMAKE_STORE_TGT)                   28060000
LWZMAKE_STORE_PNYA_STMT    DC    A(LWZMAKE_STORE_PNY)                   28070000
*                                                                       28080000
         DROP                                                           28090000
*                                                                       28100000
*********************************************************************** 28110000
* Section: LWZMAKE_SCAN_VAR                                           * 28120000
* Purpose: Parse a $() variable. At this point $( was parsed into     * 28130000
*          token 1. This section parses the variable and closing ).   * 28140000
*          If G_SCAN_APPEND_TO equals X'00' the variable is looked up * 28150000
*          in the variable binary search tree and its value is added  * 28160000
*          to the input stack so parsing first read from the variable * 28170000
*          value before continuing with the original input.           * 28180000
*          If G_SCAN_APPEND_TO is not X'00' $(var) is appended to     * 28190000
*          token 2 or 3.                                              * 28200000
*          R9 should point to global data.                            * 28210000
*********************************************************************** 28220000
LWZMAKE_SCAN_VAR DS    0F                                               28230000
         STM   R14,R12,12(R13)   * Save callers registers               28240000
         LR    R10,R15                                                  28250000
         LA    R11,4095(,R10)                                           28260000
         LA    R11,1(,R11)                                              28270000
         USING LWZMAKE_SCAN_VAR,R10,R11                                 28280000
         GETMAIN RU,LV=SCAN_VAR_DSECT_SIZ                               28290000
         ST    R13,4(R1)         * Backward chain callers SA            28300000
         ST    R1,8(R13)         * Forward chain my SA                  28310000
         LR    R13,R1            * Point R13 to my SA                   28320000
         USING SCAN_VAR_DSECT,R13 * Establish addressing of workarea    28330000
         USING GLOBAL_DATA_DSECT,R9                                     28340000
*                                                                       28350000
*        Trace record to start section                                  28360000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX28370000
               KE_SCAN_VAR'                                             28380000
*                                                                       28390000
         MVC   G_SAVE_SPACE_COUNT,=F'0'                                 28400000
*                                                                       28410000
*        In any case save the spaces                                    28420000
         LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?              28430000
         IF (NZ) THEN             * Yep...                              28440000
            IF (CLI,G_SCAN_VAR_PRESERVE_SPACES,NE,C'A') THEN            28450000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           28460000
            ENDIF                                                       28470000
            SELECT CLI,G_SCAN_APPEND_TO,EQ                              28480000
            WHEN X'00'                                                  28490000
               MVC   G_SAVE_SPACE_COUNT,G_SCAN_SPACE_COUNT              28500000
            WHEN X'01'                                                  28510000
               LT    R14,G_SCAN_TOKEN2_LEN    * Get token 2 len         28520000
               IF (Z) THEN                    * Currently empty?        28530000
                  MVC   G_SCAN_SPACE_COUNT,=F'0'                        28540000
               ENDIF                                                    28550000
            WHEN X'02'                                                  28560000
               LT    R14,G_SCAN_TOKEN3_LEN    * Get token 3 len         28570000
               IF (Z) THEN                    * Currently empty?        28580000
                  MVC   G_SCAN_SPACE_COUNT,=F'0'                        28590000
               ENDIF                                                    28600000
            ENDSEL                                                      28610000
         ENDIF                                                          28620000
*                                                                       28630000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            28640000
         BASR  R14,R15                                                  28650000
*                                                                       28660000
*        Get next token, which should be the variable name              28670000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  28680000
         BASR  R14,R15            * Link to SCAN_TOKEN section          28690000
*                                                                       28700000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 28710000
         BNE   SCAN_VAR_RET       * Yes, stop parsing variable          28720000
*                                                                       28730000
*        Check if scan state changed to anything other than IN_VARIABLE 28740000
*        which shouldn't happen if a valid variable name was found      28750000
         IC    R14,G_SCAN_STATE   * Get the scan state                  28760000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   28770000
*                                 * the high order bit in the scan      28780000
*                                 * state for 'in recipe'               28790000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   28800000
         IF (NE) THEN             * If not, write error and stop        28810000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       28820000
         ENDIF                                                          28830000
         IF (NE) THEN                                                   28840000
            MLWZMRPT RPTLINE=CL133'0Empty $()',APND_LC=C'Y'             28850000
            MVC   G_RETCODE,=F'8' * Set return code 8                   28860000
            B     SCAN_VAR_RET    * and return                          28870000
         ENDIF                                                          28880000
*                                                                       28890000
         L     R1,G_SCAN_TOKEN_LEN * Get length of variable name        28900000
         C     R1,=A(10)           * addpdsname is 10 long              28910000
         IF (EQ) THEN                                                   28920000
            L     R2,G_SCAN_TOKENA * Point R2 to token 1                28930000
            MVC   G_HELPER_DATA(10),0(R2) * Copy token to helper        28940000
            OC    G_HELPER_DATA(10),=10X'40' * Convert to uppercase     28950000
            CLC   G_HELPER_DATA(10),=C'ADDPDSNAME'                      28960000
            IF (EQ) THEN           * If it's the addpdsname function    28970000
               BAL   R8,SCAN_ADDPDSNAME * Scan the function             28980000
               B     SCAN_VAR_RET  * And skip the rest of SCAN_VAR      28990000
            ENDIF                                                       29000000
            CLC   G_HELPER_DATA(10),=C'MEMBERLIST'                      29010000
            IF (EQ) THEN           * If it's the memberlist function    29020000
               BAL   R8,SCAN_MEMBERLIST * Scan the function             29030000
               B     SCAN_VAR_RET  * And skip the rest of SCAN_VAR      29040000
            ENDIF                                                       29050000
         ENDIF                                                          29060000
*                                                                       29070000
*        Ending up here means it's not a function                       29080000
*        If APPEND_TO is X'00', copy variable name to input to FINDVAR  29090000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        29100000
            L     R1,G_SCAN_TOKEN_LEN * Get length of variable name     29110000
            C     R1,=A(L'G_SRCH_VAR) * If should always fit, but just  29120000
            IF (H) THEN               * in case check anyway            29130000
               MLWZMRPT RPTLINE=CL133'0Internal error, variable name loX29140000
               nger than 72',APND_LC=C'Y'                               29150000
               MVC   G_RETCODE,=F'12' * Set return code 12              29160000
               B     SCAN_VAR_RET     * and return                      29170000
            ENDIF                                                       29180000
*           Copy variable name to FINDVAR name                          29190000
            STH   R1,G_SRCH_VAR_LEN * Put length in FINDVAR search len  29200000
            LA    R0,G_SRCH_VAR   * Point R0 to FINDVAR search name     29210000
            L     R2,G_SCAN_TOKENA * Point R2 to token 1                29220000
            LR    R3,R1           * Make sure no cropping/filling       29230000
            MVCL  R0,R2           * Copy variable name to FINDVAR name  29240000
         ELSE                                                           29250000
*           Else if APPEND_TO is not X'00'                              29260000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         29270000
            BASR  R14,R15                                               29280000
         ENDIF                                                          29290000
*                                                                       29300000
*        Clear scan state except for left most bit indicating in recipe 29310000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        29320000
*        Set scan state bits to IN_VARIABLE2                            29330000
         OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLE2                     29340000
*                                                                       29350000
*        Get the next token which should be )                           29360000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  29370000
         BASR  R14,R15            * Link to SCAN_TOKEN section          29380000
*                                                                       29390000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 29400000
         BNE   SCAN_VAR_RET       * Yes, stop parsing variable          29410000
*                                                                       29420000
*        If APPEND_TO != X'00' then copy ) from token 1 to token n      29430000
         IF (CLI,G_SCAN_APPEND_TO,NE,X'00') THEN                        29440000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         29450000
            BASR  R14,R15                                               29460000
         ELSE                                                           29470000
*           Else if APPEND_TO is X'00'                                  29480000
            L     R15,LWZMAKE_FINDVARA_VAR * Get address to FINDVAR     29490000
            BASR  R14,R15         * Link to FINDVAR section             29500000
*                                                                       29510000
            LT    R4,G_FOUND_VAR_PTR * Check if a pointer was returned  29520000
            IF (Z) THEN           * If not write error and stop         29530000
               MLWZMRPT RPTLINE=CL133'0Variable not found',APND_LC=C'Y' 29540000
               MVC   G_RETCODE,=F'8' * Set return code 8                29550000
               B     SCAN_VAR_RET    * and return                       29560000
            ENDIF                                                       29570000
*                                                                       29580000
            USING VAR_DSECT,R4    * Address with VAR DSECT              29590000
*                                                                       29600000
            CLC   VALLEN,=H'0'    * Check empty variable value          29610000
            BE    SCAN_VAR_RET    * If empty skip rest of section       29620000
*                                                                       29630000
*           Push variable value on to input stack                       29640000
            XR    R2,R2           * Clear R2                            29650000
            XR    R3,R3           * Clear R3                            29660000
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index   29670000
            C     R3,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra     29680000
*                                 * entry fit?                          29690000
            IF (NL) THEN          * If not write error                  29700000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX29710000
               flow',APND_LC=C'Y'                                       29720000
               MVC   G_RETCODE,=F'12' * Set return code 12              29730000
               B     SCAN_VAR_RET     * and return                      29740000
            ENDIF                                                       29750000
            LA    R3,1(,R3)       * Add 1 to stack size                 29760000
            STC   R3,G_SCAN_INPUT_STACK_IDX * And store it              29770000
            BCTR  R3,R0           * Subtract 1 to calculate offset      29780000
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry 29790000
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack       29800000
            AR    R2,R3           * Add calculated offset               29810000
*                                                                       29820000
            USING INPUT_DSECT,R2  * Address with INPUT DSECT            29830000
*                                                                       29840000
            MVI   INPUTTYPE,X'01' * Set type of input to ptr to string  29850000
            MVC   INPUTLEAD,G_SAVE_SPACE_COUNT+2                        29860000
            MVC   INPUTLEN,VALLEN * Copy value length                   29870000
            MVC   INPUTPTR,VALPTR * Copy value pointer                  29880000
            MVC   INPUTPOS,=H'0'  * Set initial scan position to start  29890000
*                                                                       29900000
            DROP  R2                                                    29910000
            DROP  R4                                                    29920000
         ENDIF                                                          29930000
*                                                                       29940000
SCAN_VAR_RET EQU *                                                      29950000
         L     R3,4(,R13)        * Restore address of callers SA        29960000
         FREEMAIN RU,LV=SCAN_VAR_DSECT_SIZ,A=(R13)                      29970000
         LR    R13,R3                                                   29980000
         LM    R14,R12,12(R13)                                          29990000
         BR    R14                    Return to caller                  30000000
*                                                                       30010000
* Scan function ADDPDSNAME                                              30020000
*                                                                       30030000
SCAN_ADDPDSNAME EQU *                                                   30040000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            30050000
         BASR  R14,R15                                                  30060000
*                                                                       30070000
         BAL   R7,SCAN_VAR_SAVE                                         30080000
*                                                                       30090000
*        Clear scan state except for left most bit indicating in recipe 30100000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        30110000
*        Set scan state bits to IN_ADDPDSNAME                           30120000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME                    30130000
*                                                                       30140000
*        Get next token, which should be the start of the PDS name      30150000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  30160000
         BASR  R14,R15            * Link to SCAN_TOKEN section          30170000
*                                                                       30180000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 30190000
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function         30200000
*                                                                       30210000
         B     ADDPDSNAME_PDSNAME_CHECK_VAR                             30220000
*                                                                       30230000
ADDPDSNAME_PDSNAME_NEXT_TOKEN EQU *                                     30240000
*        Get next token, which should be the PDS name                   30250000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  30260000
         BASR  R14,R15            * Link to SCAN_TOKEN section          30270000
*                                                                       30280000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 30290000
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function         30300000
*                                                                       30310000
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA                    30320000
         BE    ADDPDSNAME3                                              30330000
*                                                                       30340000
         LT    R14,G_SCAN_TOKEN2_LEN                                    30350000
         IF (NZ) THEN                                                   30360000
            LT    R14,G_SCAN_SPACE_COUNT                                30370000
            IF (NZ) THEN                                                30380000
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as pds nameX30390000
                in addpdsname function',APND_LC=C'Y'                    30400000
               MVC   G_RETCODE,=F'8' * Set return code 8                30410000
               BR    R8             * and return                        30420000
            ENDIF                                                       30430000
         ENDIF                                                          30440000
*                                                                       30450000
ADDPDSNAME_PDSNAME_CHECK_VAR EQU *                                      30460000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     30470000
*        section to expand it and loop around for the next keyword      30480000
         IC    R14,G_SCAN_STATE   * Get the scan state                  30490000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   30500000
*                                 * the high order bit in the scan      30510000
*                                 * state for 'in recipe'               30520000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   30530000
         IF (NE) THEN                                                   30540000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       30550000
         ENDIF                                                          30560000
         IF (EQ) THEN             * If so...                            30570000
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   30580000
            BASR  R14,R15            * Link to SCAN_VAR section         30590000
*                                                                       30600000
            CLC   G_RETCODE,=F'0'    * Did an error occur?              30610000
            BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing statement     30620000
*                                                                       30630000
            B     ADDPDSNAME2                                           30640000
         ENDIF                                                          30650000
*                                                                       30660000
         MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO                 30670000
*                                                                       30680000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        30690000
*           Append token 1 to token 2                                   30700000
            MVI   G_SCAN_APPEND_TO,X'01'                                30710000
            LT    R1,G_SCAN_TOKEN2_LEN                                  30720000
            IF (Z) THEN                                                 30730000
               MVC   G_SCAN_SPACE_COUNT,=A(0)                           30740000
            ENDIF                                                       30750000
         ENDIF                                                          30760000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            30770000
         BASR  R14,R15                                                  30780000
         MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO                 30790000
*                                                                       30800000
ADDPDSNAME2 EQU *                                                       30810000
*        Clear scan state except for left most bit indicating in recipe 30820000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        30830000
*        Set scan state bits to IN_ADDPDSNAME2                          30840000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME2                   30850000
*                                                                       30860000
         B     ADDPDSNAME_PDSNAME_NEXT_TOKEN                            30870000
*                                                                       30880000
ADDPDSNAME3 EQU *                                                       30890000
*        Clear scan state except for left most bit indicating in recipe 30900000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        30910000
*        Set scan state bits to IN_ADDPDSNAME3                          30920000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME3                   30930000
*        If we're expanding                                             30940000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        30950000
*           Clear token 3 length, which will receive the member(s)      30960000
            MVC   G_SCAN_TOKEN3_LEN,=F'0'                               30970000
         ELSE                                                           30980000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         30990000
            BASR  R14,R15                                               31000000
         ENDIF                                                          31010000
*                                                                       31020000
ADDPDSNAME_NEXT_MEMBER EQU *                                            31030000
*        Get next token, which should be the a member name or variable  31040000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  31050000
         BASR  R14,R15            * Link to SCAN_TOKEN section          31060000
*                                                                       31070000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 31080000
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function         31090000
*                                                                       31100000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     31110000
*        section to expand it and loop around for the next keyword      31120000
         IC    R14,G_SCAN_STATE   * Get the scan state                  31130000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   31140000
*                                 * the high order bit in the scan      31150000
*                                 * state for 'in recipe'               31160000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   31170000
         IF (NE) THEN                                                   31180000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       31190000
         ENDIF                                                          31200000
         IF (EQ) THEN             * If so...                            31210000
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   31220000
            BASR  R14,R15            * Link to SCAN_VAR section         31230000
*                                                                       31240000
            CLC   G_RETCODE,=F'0'    * Did an error occur?              31250000
            BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing statement     31260000
*                                                                       31270000
            CLI   G_SCAN_APPEND_TO,X'00'                                31280000
            BE    ADDPDSNAME_NEXT_MEMBER * Loop around for next token   31290000
            B     ADDPDSNAME4                                           31300000
         ENDIF                                                          31310000
*                                                                       31320000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        31330000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          31340000
            BE    ADDPDSNAME_FINISH                                     31350000
*                                                                       31360000
*           Append member to token 3                                    31370000
            L     R0,G_SCAN_TOKEN2A                                     31380000
            A     R0,G_SCAN_TOKEN2_LEN                                  31390000
            L     R1,=F'1'                                              31400000
            LA    R2,=C'('                                              31410000
            LR    R3,R1                                                 31420000
            MVCL  R0,R2                                                 31430000
*                                                                       31440000
            L     R2,G_SCAN_TOKENA  * Point R2 to token 1               31450000
            L     R1,G_SCAN_TOKEN_LEN * Get length of token to append   31460000
            LR    R3,R1             * Make sure no cropping/filling     31470000
            MVCL  R0,R2             * Append to token 3                 31480000
*                                                                       31490000
            L     R1,=F'1'                                              31500000
            LA    R2,=C')'                                              31510000
            LR    R3,R1                                                 31520000
            MVCL  R0,R2                                                 31530000
*                                                                       31540000
            L     R0,G_SCAN_TOKENA                                      31550000
            L     R1,G_SCAN_TOKEN2_LEN                                  31560000
            A     R1,G_SCAN_TOKEN_LEN                                   31570000
            LA    R1,2(,R1)                                             31580000
            ST    R1,G_SCAN_TOKEN_LEN                                   31590000
            L     R2,G_SCAN_TOKEN2A                                     31600000
            LR    R3,R1                                                 31610000
            MVCL  R0,R2                                                 31620000
*                                                                       31630000
            MVI   G_SCAN_APPEND_TO,X'02'                                31640000
            LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3     31650000
            IF (Z) THEN             * Is this the first part of token 3 31660000
               MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of lead spaces  31670000
            ENDIF                                                       31680000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         31690000
            BASR  R14,R15                                               31700000
*                                                                       31710000
            MVI   G_SCAN_APPEND_TO,X'00'                                31720000
         ELSE                                                           31730000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         31740000
            BASR  R14,R15                                               31750000
*                                                                       31760000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          31770000
            BE    ADDPDSNAME_FINISH                                     31780000
         ENDIF                                                          31790000
*                                                                       31800000
ADDPDSNAME4 EQU *                                                       31810000
*        Clear scan state except for left most bit indicating in recipe 31820000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        31830000
*        Set scan state bits to IN_ADDPDSNAME4                          31840000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME4                   31850000
*                                                                       31860000
         B     ADDPDSNAME_NEXT_MEMBER * Loop around to get next token   31870000
*                                                                       31880000
ADDPDSNAME_FINISH EQU *                                                 31890000
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack     31900000
         XR    R2,R2                 * Clear R2                         31910000
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index      31920000
         BCTR  R2,R0                 * Subtract 1 from index            31930000
         IC    R15,0(R2,R1)          * Get stack state in that idx      31940000
         STC   R15,G_SCAN_STATE      * And save it as current state     31950000
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx      31960000
*                                                                       31970000
         BAL   R7,SCAN_VAR_RESTORE                                      31980000
*                                                                       31990000
SCAN_ADDPDSNAME_RET EQU *                                               32000000
         BR    R8                                                       32010000
*                                                                       32020000
* Scan function MEMBERLIST                                              32030000
*                                                                       32040000
SCAN_MEMBERLIST EQU *                                                   32050000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            32060000
         BASR  R14,R15                                                  32070000
*                                                                       32080000
         BAL   R7,SCAN_VAR_SAVE                                         32090000
*                                                                       32100000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        32110004
            MVC   G_SCAN_TOKEN3_LEN,=F'0'                               32120004
         ENDIF                                                          32130004
*                                                                       32140004
*        Clear scan state except for left most bit indicating in recipe 32150000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        32160000
*        Set scan state bits to IN_MEMBERLIST                           32170000
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST                    32180000
*                                                                       32190000
*        Get next token, which should be the start of the PDS name      32200000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  32210000
         BASR  R14,R15            * Link to SCAN_TOKEN section          32220000
*                                                                       32230000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 32240000
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function         32250000
*                                                                       32260000
         B     MEMBERLIST_PDSNAME_CHECK_VAR                             32270000
*                                                                       32280000
MEMBERLIST_PDSNAME_NEXT_TOKEN EQU *                                     32290000
*        Get next token, which should be the PDS name                   32300000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  32310000
         BASR  R14,R15            * Link to SCAN_TOKEN section          32320000
*                                                                       32330000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 32340000
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function         32350000
*                                                                       32360000
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA                    32370003
         BE    MEMBERLIST3                                              32380003
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET             32390003
         BE    MEMBERLIST_FINISH                                        32400003
*                                                                       32410003
         LT    R14,G_SCAN_TOKEN2_LEN                                    32420000
         IF (NZ) THEN                                                   32430000
            LT    R14,G_SCAN_SPACE_COUNT                                32440000
            IF (NZ) THEN                                                32450000
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as pds nameX32460000
                in memberlist function',APND_LC=C'Y'                    32470000
               MVC   G_RETCODE,=F'8' * Set return code 8                32480000
               BR    R8             * and return                        32490000
            ENDIF                                                       32500000
         ENDIF                                                          32510000
*                                                                       32520000
MEMBERLIST_PDSNAME_CHECK_VAR EQU *                                      32530000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     32540000
*        section to expand it and loop around for the next keyword      32550000
         IC    R14,G_SCAN_STATE   * Get the scan state                  32560000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   32570000
*                                 * the high order bit in the scan      32580000
*                                 * state for 'in recipe'               32590000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   32600000
         IF (NE) THEN                                                   32610000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       32620000
         ENDIF                                                          32630000
         IF (EQ) THEN             * If so...                            32640000
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   32650000
            BASR  R14,R15            * Link to SCAN_VAR section         32660000
*                                                                       32670000
            CLC   G_RETCODE,=F'0'    * Did an error occur?              32680000
            BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing statement     32690000
*                                                                       32700000
            B     MEMBERLIST2                                           32710000
         ENDIF                                                          32720000
*                                                                       32730000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        32740000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          32750000
            BE    MEMBERLIST_FINISH                                     32760000
*                                                                       32770000
*           Append token 1 to token 2                                   32780000
            MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO              32790000
            MVI   G_SCAN_APPEND_TO,X'01'                                32800000
            LT    R1,G_SCAN_TOKEN2_LEN                                  32810000
            IF (Z) THEN                                                 32820000
               MVC   G_SCAN_SPACE_COUNT,=A(0)                           32830000
            ENDIF                                                       32840000
*                                                                       32850000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         32860000
            BASR  R14,R15                                               32870000
*                                                                       32880000
            MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO              32890000
         ELSE                                                           32900000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         32910000
            BASR  R14,R15                                               32920000
*                                                                       32930000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          32940000
            BE    MEMBERLIST_FINISH                                     32950000
         ENDIF                                                          32960000
*                                                                       32970000
MEMBERLIST2 EQU *                                                       32980000
*        Clear scan state except for left most bit indicating in recipe 32990000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        33000000
*        Set scan state bits to IN_MEMBERLIST2                          33010000
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST2                   33020000
*                                                                       33030000
         B     MEMBERLIST_PDSNAME_NEXT_TOKEN                            33040000
*                                                                       33050003
MEMBERLIST3 EQU *                                                       33060003
*        Clear scan state except for left most bit indicating in recipe 33070003
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        33080003
*        Set scan state bits to IN_MEMBERLIST3                          33090003
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST3                   33100003
*        If we're expanding                                             33110003
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        33120003
*           Clear token 3 length, which will receive the member(s)      33130003
            MVC   G_SCAN_TOKEN3_LEN,=F'0'                               33140003
         ELSE                                                           33150003
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         33160003
            BASR  R14,R15                                               33170003
         ENDIF                                                          33180003
*                                                                       33190003
MEMBERLIST_MEMFILTER EQU *                                              33200003
*        Get next token, which should be the a member name or variable  33210003
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  33220003
         BASR  R14,R15            * Link to SCAN_TOKEN section          33230003
*                                                                       33240003
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 33250003
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function         33260003
*                                                                       33270003
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     33280003
*        section to expand it and loop around for the next keyword      33290003
         IC    R14,G_SCAN_STATE   * Get the scan state                  33300003
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   33310003
*                                 * the high order bit in the scan      33320003
*                                 * state for 'in recipe'               33330003
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   33340003
         IF (NE) THEN                                                   33350003
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       33360003
         ENDIF                                                          33370003
         IF (EQ) THEN             * If so...                            33380003
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   33390003
            BASR  R14,R15            * Link to SCAN_VAR section         33400003
*                                                                       33410003
            CLC   G_RETCODE,=F'0'    * Did an error occur?              33420003
            BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing statement     33430003
*                                                                       33440003
            CLI   G_SCAN_APPEND_TO,X'00'                                33450003
            BE    MEMBERLIST_MEMFILTER * Loop around for next token     33460003
            B     MEMBERLIST4                                           33470003
         ENDIF                                                          33480003
*                                                                       33490003
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        33500003
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          33510003
            BE    MEMBERLIST_FINISH                                     33520003
*                                                                       33530003
            MVI   G_SCAN_APPEND_TO,X'02'                                33540003
            LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3     33550003
            IF (Z) THEN             * Is this the first part of token 3 33560003
               MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of lead spaces  33570003
            ENDIF                                                       33580003
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         33590003
            BASR  R14,R15                                               33600003
*                                                                       33610003
            MVI   G_SCAN_APPEND_TO,X'00'                                33620003
         ELSE                                                           33630003
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         33640003
            BASR  R14,R15                                               33650003
*                                                                       33660003
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          33670003
            BE    MEMBERLIST_FINISH                                     33680003
         ENDIF                                                          33690003
*                                                                       33700003
MEMBERLIST4 EQU *                                                       33710003
*        Clear scan state except for left most bit indicating in recipe 33720003
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        33730003
*        Set scan state bits to IN_MEMBERLIST4                          33740003
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST4                   33750003
*                                                                       33760003
         B     MEMBERLIST_MEMFILTER * Loop around to get next token     33770003
*                                                                       33780000
MEMBERLIST_FINISH EQU *                                                 33790000
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack     33800000
         XR    R2,R2                 * Clear R2                         33810000
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index      33820000
         BCTR  R2,R0                 * Subtract 1 from index            33830000
         IC    R15,0(R2,R1)          * Get stack state in that idx      33840000
         STC   R15,G_SCAN_STATE      * And save it as current state     33850000
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx      33860000
*                                                                       33870000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        33880000
*           Get the member list                                         33890000
            L     R15,LWZMAKE_GET_MEMLISTA_VAR * Get addr GET_MEMLIST   33900000
            BASR  R14,R15         * Link to GET_MEMLIST section         33910000
         ENDIF                                                          33920000
*                                                                       33930000
         BAL   R7,SCAN_VAR_RESTORE                                      33940000
*                                                                       33950000
SCAN_MEMBERLIST_RET EQU *                                               33960000
         BR    R8                                                       33970000
*                                                                       33980000
* Save tokens                                                           33990000
*                                                                       34000000
SCAN_VAR_SAVE EQU *                                                     34010000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        34020000
            MVC   SCAN_VAR_SAVE_TOKENA,G_SCAN_TOKENA                    34030000
            MVC   SCAN_VAR_SAVE_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN        34040000
            MVC   SCAN_VAR_SAVE_TOKEN_LEN,G_SCAN_TOKEN_LEN              34050000
            L     R4,G_SCAN_TOKEN_MAXLEN                                34060000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        34070000
            ST    R1,G_SCAN_TOKENA                                      34080000
            MVC   G_SCAN_TOKEN_LEN,=A(0)                                34090000
*                                                                       34100000
            MVC   SCAN_VAR_SAVE_TOKEN2A,G_SCAN_TOKEN2A                  34110000
            MVC   SCAN_VAR_SAVE_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN      34120000
            MVC   SCAN_VAR_SAVE_TOKEN2_LEN,G_SCAN_TOKEN2_LEN            34130000
            L     R4,G_SCAN_TOKEN2_MAXLEN                               34140000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        34150000
            ST    R1,G_SCAN_TOKEN2A                                     34160000
            MVC   G_SCAN_TOKEN2_LEN,=A(0)                               34170000
*                                                                       34180000
            MVC   SCAN_VAR_SAVE_TOKEN3A,G_SCAN_TOKEN3A                  34190000
            MVC   SCAN_VAR_SAVE_TOKEN3_MAXLEN,G_SCAN_TOKEN3_MAXLEN      34200000
            MVC   SCAN_VAR_SAVE_TOKEN3_LEN,G_SCAN_TOKEN3_LEN            34210000
            L     R4,G_SCAN_TOKEN3_MAXLEN                               34220000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        34230000
            ST    R1,G_SCAN_TOKEN3A                                     34240000
            MVC   G_SCAN_TOKEN3_LEN,=A(0)                               34250000
         ENDIF                                                          34260000
*                                                                       34270000
         BR    R7                                                       34280000
*                                                                       34290000
* Restore tokens                                                        34300000
*                                                                       34310000
SCAN_VAR_RESTORE EQU *                                                  34320000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        34330000
            L     R2,G_SCAN_TOKEN_MAXLEN                                34340000
            L     R3,G_SCAN_TOKENA                                      34350000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  34360000
*                                                                       34370000
            MVC   G_SCAN_TOKENA,SCAN_VAR_SAVE_TOKENA                    34380000
            MVC   G_SCAN_TOKEN_MAXLEN,SCAN_VAR_SAVE_TOKEN_MAXLEN        34390000
            MVC   G_SCAN_TOKEN_LEN,SCAN_VAR_SAVE_TOKEN_LEN              34400000
*                                                                       34410000
            L     R2,G_SCAN_TOKEN2_MAXLEN                               34420000
            L     R3,G_SCAN_TOKEN2A                                     34430000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  34440000
            MVC   G_SCAN_TOKEN2A,SCAN_VAR_SAVE_TOKEN2A                  34450000
            MVC   G_SCAN_TOKEN2_MAXLEN,SCAN_VAR_SAVE_TOKEN2_MAXLEN      34460000
            MVC   G_SCAN_TOKEN2_LEN,SCAN_VAR_SAVE_TOKEN2_LEN            34470000
*                                                                       34480000
*           Push variable value on to input stack                       34490000
            XR    R2,R2           * Clear R2                            34500000
            XR    R3,R3           * Clear R3                            34510000
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index   34520000
            C     R3,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra     34530000
*                                 * entry fit?                          34540000
            IF (NL) THEN          * If not write error                  34550000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX34560000
               flow',APND_LC=C'Y'                                       34570000
               MVC   G_RETCODE,=F'12' * Set return code 12              34580000
               B     SCAN_VAR_RET     * and return                      34590000
            ENDIF                                                       34600000
            LA    R3,1(,R3)       * Add 1 to stack size                 34610000
            STC   R3,G_SCAN_INPUT_STACK_IDX * And store it              34620000
            BCTR  R3,R0           * Subtract 1 to calculate offset      34630000
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry 34640000
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack       34650000
            AR    R2,R3           * Add calculated offset               34660000
*                                                                       34670000
            USING INPUT_DSECT,R2  * Address with INPUT DSECT            34680000
*                                                                       34690000
            MVI   INPUTTYPE,X'01' * Set type of input to ptr to string  34700000
            MVC   INPUTLEAD,G_SCAN_SPACE_COUNT+2                        34710000
            MVC   INPUTLEN,G_SCAN_TOKEN3_LEN+2 * Copy value length      34720000
            MVC   INPUTPTR,G_SCAN_TOKEN3A * Copy value pointer          34730000
            MVC   INPUTPOS,=H'0'  * Set initial scan position to start  34740000
*                                                                       34750000
            DROP  R2                                                    34760000
*                                                                       34770000
            MVC   G_SCAN_TOKEN3A,SCAN_VAR_SAVE_TOKEN3A                  34780000
            MVC   G_SCAN_TOKEN3_MAXLEN,SCAN_VAR_SAVE_TOKEN3_MAXLEN      34790000
            MVC   G_SCAN_TOKEN3_LEN,SCAN_VAR_SAVE_TOKEN3_LEN            34800000
         ENDIF                                                          34810000
*                                                                       34820000
         BR    R7                                                       34830000
*                                                                       34840000
         LTORG                                                          34850000
*                                                                       34860000
* Local constant pointers to section addresses                          34870000
LWZMAKE_APPEND_TOKENA_VAR DC    A(LWZMAKE_APPEND_TOKEN)                 34880000
LWZMAKE_SCAN_TOKENA_VAR   DC    A(LWZMAKE_SCAN_TOKEN)                   34890000
LWZMAKE_FINDVARA_VAR      DC    A(LWZMAKE_FINDVAR)                      34900000
LWZMAKE_SCAN_VARA_VAR     DC    A(LWZMAKE_SCAN_VAR)                     34910000
LWZMAKE_GET_MEMLISTA_VAR  DC    A(LWZMAKE_GET_MEMLIST)                  34920000
*                                                                       34930000
SCAN_VAR_DSECT              DSECT                                       34940000
                            DS    18F * My savearea                     34950000
*                                                                       34960000
SCAN_VAR_SAVE_APPEND_TO     DS    C                                     34970000
*                                                                       34980000
                            DS    0F                                    34990000
SCAN_VAR_SAVE_TOKEN_LEN     DS    F                                     35000000
SCAN_VAR_SAVE_TOKEN2_LEN    DS    F                                     35010000
SCAN_VAR_SAVE_TOKEN3_LEN    DS    F                                     35020000
SCAN_VAR_SAVE_TOKEN_MAXLEN  DS    F                                     35030000
SCAN_VAR_SAVE_TOKEN2_MAXLEN DS    F                                     35040000
SCAN_VAR_SAVE_TOKEN3_MAXLEN DS    F                                     35050000
SCAN_VAR_SAVE_TOKENA        DS    A                                     35060000
SCAN_VAR_SAVE_TOKEN2A       DS    A                                     35070000
SCAN_VAR_SAVE_TOKEN3A       DS    A                                     35080000
*                                                                       35090000
SCAN_VAR_DSECT_SIZ          EQU   *-SCAN_VAR_DSECT                      35100000
*                                                                       35110000
LWZMAKE  CSECT                                                          35120000
*                                                                       35130000
*********************************************************************** 35140000
* Section: LWZMAKE_SCAN_TOKEN                                         * 35150000
* Purpose: Parsing lexer. This section invokes LWZMAKE_SCAN_CHAR to   * 35160000
*          get characetrs in order to return the next keyword. It     * 35170000
*          gets rid of whitespace, but counts the spaces leading the  * 35180000
*          next keyword should those need to be preserved.            * 35190000
*          Based on the current scan state, checks are performed      * 35200000
*          whether a type of token is allowed where it is found, in   * 35210000
*          other words most of the syntax checking is done here.      * 35220000
*          R7 is used as the index register to address the next       * 35230000
*          character position in G_SCAN_TOKEN.                          35240000
*          R9 should point to global data.                            * 35250000
*********************************************************************** 35260000
LWZMAKE_SCAN_TOKEN MLWZSAVE                                             35270000
*        Trace record to start section                                  35280000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX35290000
               KE_SCAN_TOKEN'                                           35300000
*                                                                       35310000
         MVC   G_SCAN_TOKEN_LEN,=F'0' * Initialize token 1 length       35320000
         MVI   G_SCAN_TOKENTYPE,X'00' * Initialize token 1 type         35330000
         MVC   G_SCAN_SPACE_COUNT,=F'0' * Initialize space count        35340000
         XR    R7,R7                  * Initialize index reg to token   35350000
*                                                                       35360000
STATE_AND_SCAN_FOR_WHITESPACE EQU *                                     35370000
*        Translate scan state to bitstring of allowed token types       35380000
         IC    R2,G_SCAN_STATE    * Get the scan state                  35390000
         N     R2,=X'0000007F'    * Clear out bits 0-56, so including   35400000
*                                 * the high order bit in the scan      35410000
*                                 * state for 'in recipe'               35420000
         SLL   R2,2               * Multiply by 4 (scan state table     35430000
*                                 * contains full words)                35440000
         L     R1,SCAN_STATE_TABLEA_TOKEN * Point R1 to scan state tab  35450000
         AR    R1,R2              * Add offset for current scan stata   35460000
         MVC   G_SCAN_EXPECTED,0(R1) * Get the corresponding bitstring  35470000
*                                                                       35480000
SCAN_FOR_WHITESPACE EQU *                                               35490000
         L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address to SCAN_CHAR  35500000
         BASR  R14,R15            * Link to SCAN_CHAR section           35510000
*                                                                       35520000
*        Check for end of file and whether it's expected                35530000
         IF (CLI,G_MKFEOF,EQ,C'Y') THEN * Is EOF switch on?             35540000
*                                 * And was it expected?                35550000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_EOF,Z) THEN           35560000
               MLWZMRPT RPTLINE=CL133'0Unexpected end of file',APND_LC=X35570000
               C'Y'                                                     35580000
               MVC   G_RETCODE,=F'8' * Set return code 8                35590000
            ENDIF                                                       35600000
            B     SCAN_TOKEN_RET  * Skip rest of tokenizer              35610000
         ENDIF                                                          35620000
*                                                                       35630000
*        Check for beginning a new line, when allowed and not in a      35640000
*        continued statement, this resets the scan state to not in stmt 35650000
         IF (CLI,G_SCAN_NEWLINE,EQ,C'Y') THEN * Is new line switch on?  35660000
*                                 * And was it expected?                35670000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_NEWLINE,Z) THEN       35680000
               MLWZMRPT RPTLINE=CL133'0Unexpected new line',APND_LC=C'YX35690000
               '                                                        35700000
               MVC   G_RETCODE,=F'8' * Set return code 8                35710000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           35720000
            ENDIF                                                       35730000
*           Check if we're not in a continued line                      35740000
            IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN                 35750000
               IC    R14,G_SCAN_STATE * Get the scan state              35760000
               N     R14,=X'0000007F' * Clear out bits 0-56, including  35770000
*                                     * the high order bit in the scan  35780000
*                                     * state for 'in recipe'           35790000
               C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in stmt 35800000
               IF (NE) THEN           * If not so...                    35810000
*                 Clear scan state except for left most bit 'in recipe' 35820000
                  NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE               35830000
*                 Set scan state bits to NOT_IN_STMT                    35840000
                  OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT             35850000
*                 Skip to finishing valid token                         35860000
                  B     SCAN_TOKEN_VALID                                35870000
               ENDIF                                                    35880000
            ENDIF                                                       35890000
            B     STATE_AND_SCAN_FOR_WHITESPACE * Loop for next char    35900000
         ENDIF                                                          35910000
*                                                                       35920000
*        Check space, if so add to leading space char count             35930000
         IF (CLI,G_SCAN_CURRCHAR,EQ,C' ') THEN                          35940000
            L     R2,G_SCAN_SPACE_COUNT * Get current space count       35950000
            LA    R2,1(,R2)             * Add 1                         35960000
            ST    R2,G_SCAN_SPACE_COUNT * And put it back               35970000
            B     SCAN_FOR_WHITESPACE   * Loop for next char            35980000
         ENDIF                                                          35990000
*                                                                       36000000
*        Anything beyond column 72 is ignored and considered the end    36010000
*        of a line                                                      36020000
         L     R6,G_SCAN_CURRCOL  * Get current column                  36030000
         C     R6,=F'72'          * Check if we're beyond col 72        36040000
         IF (GE) THEN             * If so (currcol starts with 0)       36050000
*           Set token type to ignore                                    36060000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_IGNORE                36070000
*           Was it expected?                                            36080000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_IGNORE,Z) THEN        36090000
*              Prepare helper data for parse error trace record         36100000
               MVC   G_HELPER_DATA(23),=C'UNEXPECTED END OF LINE '      36110000
               MVC   G_HELPER_DATA+23(1),G_SCAN_STATE                   36120000
               MVI   G_HELPER_DATA+24,C' '                              36130000
               MVC   G_HELPER_DATA+25(4),G_SCAN_EXPECTED                36140000
               LA    R14,G_HELPER_DATA * Get address to helper data     36150000
               ST    R14,G_LWZMTRC_DATA_PTR * Set it as trace data ptr  36160000
               MVC   G_LWZMTRC_DATA_SIZ,=AL2(29) * Set trace data len   36170000
               MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'003',DATA     36180000
               MLWZMRPT RPTLINE=CL133'0Unexpected end of line',APND_LC=X36190000
               C'Y'                                                     36200000
               MVC   G_RETCODE,=F'8' * Set return code 8                36210000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           36220000
            ENDIF                                                       36230000
*                                                                       36240000
CHECK_NEXT_IGNORE_CHAR EQU *                                            36250000
*           Reaching column 80 in combination with not a continued line 36260000
*           resets scan state to not in stmt                            36270000
            L     R6,G_SCAN_CURRCOL * Get current column                36280000
            C     R6,=F'79'         * Check column 80                   36290000
            IF (GE) THEN            * If we're there                    36300000
*                                   * And it's not a continued line     36310000
               IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN              36320000
                  IC    R14,G_SCAN_STATE * Get the scan state           36330000
                  N     R14,=X'0000007F' * Clear out bits 0-56,         36340000
*                                    * including the high order bit in  36350000
*                                    * the scan state for 'in recipe'   36360000
                  C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check          36370000
                  IF (NE) THEN       * not in stmt?                     36380000
*                    Clear scan state except left most bit 'in recipe'  36390000
                     NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE            36400000
*                    Set scan state bits to NOT_IN_STMT                 36410000
                     OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT          36420000
*                    Skip to finishing valid token                      36430000
                     B     SCAN_TOKEN_VALID                             36440000
                  ENDIF                                                 36450000
               ENDIF                                                    36460000
*              Overrule bitstring with that for a new line              36470000
               MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_NEWLINE)          36480000
*              Jump to scan for whitespace, which for a new line after  36490000
*              continuation jumps to STATE_AND_SCAN_FOR_WHITESPACE and  36500000
*              reset to the correct bitstring                           36510000
               B     SCAN_FOR_WHITESPACE                                36520000
            ENDIF                                                       36530000
*           We end up here for chars in columns 72-80                   36540000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  36550000
            BASR  R14,R15          * Link to SCAN_CHAR section          36560000
            CLI   G_MKFEOF,C'Y'    * At this point EOF should           36570000
            BE    SCAN_TOKEN_VALID * return valid ignore token          36580000
            CLI   G_SCAN_NEWLINE,C'Y' * Same goes for new line          36590000
            BE    SCAN_TOKEN_VALID * returns valid ignore token         36600000
            B     CHECK_NEXT_IGNORE_CHAR * None of the above, so loop   36610000
*                                  * around checking next ignore char   36620000
         ENDIF                                                          36630000
*                                                                       36640000
*        At this point we've ruled out EOF, new line and pos 72-80      36650000
*        so we can start checking token types.                          36660000
*        Point R5 to current char to start checking.                    36670000
         LA    R5,G_SCAN_CURRCHAR                                       36680000
*                                                                       36690000
*        Check for comments                                             36700000
         IF (CLI,0(R5),EQ,C'#') THEN                                    36710000
*           Set token type to comment                                   36720000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMENT               36730000
*           Prepare helper data for start parse token trace record      36740000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'612',CONST=C'COX36750000
               MMENT'                                                   36760000
*           Was comment expected? If not, write error and stop          36770000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_COMMENT,Z) THEN       36780000
               MLWZMRPT RPTLINE=CL133'0Unexpected comment',APND_LC=C'Y' 36790000
               MVC   G_RETCODE,=F'8' * Set return code 8                36800000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           36810000
            ENDIF                                                       36820000
CHECK_NEXT_COMMENT_CHAR EQU *                                           36830000
*           Anything up to column 72 is considered part of the comments 36840000
*           after that, if not in continued line we can reset scan      36850000
*           state to not in stmt                                        36860000
            L     R6,G_SCAN_CURRCOL * Get current column                36870000
            C     R6,=F'71'         * Check if we're at 72 yet          36880000
            IF (NL) THEN            * If so...                          36890000
*              And it's not a continued line                            36900000
               IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN              36910000
                  IC    R14,G_SCAN_STATE * Get the scan state           36920000
                  N     R14,=X'0000007F' * Clear out bits 0-56,         36930000
*                                    * including the high order bit in  36940000
*                                    * the scan state for 'in recipe'   36950000
                  C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check          36960000
                  IF (NE) THEN       * not in stmt?                     36970000
*                    Clear scan state except left most bit 'in recipe'  36980000
                     NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE            36990000
*                    Set scan state bits to NOT_IN_STMT                 37000000
                     OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT          37010000
*                    Skip to finishing valid token                      37020000
                     B     SCAN_TOKEN_VALID                             37030000
                  ENDIF                                                 37040000
               ENDIF                                                    37050000
*              Overrule bitstring with that for ignore chars            37060000
               MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_IGNORE)           37070000
*              Jump to scan for whitespace, to check for ignore chars   37080000
*              and newline / EOF                                        37090000
               B     SCAN_FOR_WHITESPACE                                37100000
            ENDIF                                                       37110000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  37120000
            BASR  R14,R15                 * Link to SCAN_CHAR section   37130000
            B     CHECK_NEXT_COMMENT_CHAR * Loop for next comment char  37140000
         ENDIF                                                          37150000
*                                                                       37160000
*        If we ended up here and we've already had line continuation    37170000
*        character, anything other than comment is a syntax error       37180000
         IF (CLI,G_SCAN_CONTINUED_LINE,EQ,C'Y') THEN                    37190000
            MLWZMRPT RPTLINE=CL133'0Syntax error, only comments allowedX37200000
                after continuation character',APND_LC=C'Y'              37210000
            MVC   G_RETCODE,=F'8' * Set return code 8                   37220000
            B     SCAN_TOKEN_RET  * Skip the rest of tokenizer          37230000
         ENDIF                                                          37240000
*                                                                       37250000
*        Check for continuation character                               37260000
         IF (CLI,0(R5),EQ,C'\') THEN                                    37270000
*           Set token type to continuation                              37280000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CONTINUATION          37290000
*           Set switch for continuation to true                         37300000
            MVI   G_SCAN_CONTINUED_LINE,C'Y'                            37310000
*           Overrule bitstring with that for comments                   37320000
            MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_COMMENT)             37330000
*           Jump to scan for whitespace, to look for comments, ignore   37340000
*           chars and newline / EOF                                     37350000
            B     SCAN_FOR_WHITESPACE                                   37360000
         ENDIF                                                          37370000
*                                                                       37380000
*        Only on column 1 check for recipe prefix                       37390000
         L     R6,G_SCAN_CURRCOL  * Get current column                  37400000
         C     R6,=F'0'           * Check for column 1                  37410000
         IF (EQ) THEN             * If so...                            37420000
            CLC   0(1,R5),G_RECIPEPREFIX * Check for recipe prefix char 37430000
            IF (EQ) THEN          * If that's the case...               37440000
*              Set token type to recipe prefix                          37450000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_RECIPEPREFIX       37460000
*              Was it expected? If not, write error and stop            37470000
               IF (TM,G_SCAN_EXPECTED+2,SCAN_EXPECTED3_RECIPREF,Z) THEN 37480000
UNEXPECTED_RECIPE EQU *                                                 37490000
                  MLWZMRPT RPTLINE=CL133'0Unexpected recipe',APND_LC=C'X37500000
               Y'                                                       37510000
                  MVC   G_RETCODE,=F'8' * Set return code 8             37520000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        37530000
               ENDIF                                                    37540000
*              In rare cases the expected bitstring doesn't cover it,   37550000
*              e.g. a recipe line followed by an empty line and         37560000
*              another recipe line. That's why this elaborate check.    37570000
*              If the previous statement wasn't a rule                  37580000
               IF (CLI,G_PREV_STMT_TYPE,NE,STMT_TYPE_RULE) THEN         37590000
*                 and the scan state isn't in recipe                    37600000
                  IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN      37610000
*                    and the previous scan state wasn't either          37620000
                     IF (CLI,G_PREV_STMT_IN_RECIPE,NE,C'Y') THEN        37630000
*                       then also write error and stop                  37640000
                        B     UNEXPECTED_RECIPE                         37650000
                     ENDIF                                              37660000
                  ENDIF                                                 37670000
               ENDIF                                                    37680000
*              If we end up here the recipe prefix was valid, so set    37690000
*              the bit in the scan state                                37700000
               OI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                  37710000
*              Loop around to continue scanning for whitespace          37720000
               B     SCAN_FOR_WHITESPACE                                37730000
            ENDIF                                                       37740000
         ENDIF                                                          37750000
*                                                                       37760000
*        Check for assignment operator                                  37770000
         IF (CLI,0(R5),EQ,C'=') THEN                                    37780000
*           Set token type to operator                                  37790000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_OPERATOR              37800000
*           Was it expected? If not, write error and stop               37810000
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_OPERATOR,Z) THEN    37820000
               MLWZMRPT RPTLINE=CL133'0Unexpected operator',APND_LC=C'YX37830000
               '                                                        37840000
               MVC   G_RETCODE,=F'8' * Set return code 8                37850000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           37860000
            ENDIF                                                       37870000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             37880000
            B     SCAN_TOKEN_VALID   * Skip to finishing valid token    37890000
         ENDIF                                                          37900000
*                                                                       37910000
*        Check for colon, which could be a rule or byte 1 of :=         37920000
         IF (CLI,0(R5),EQ,C':') THEN                                    37930000
*           Set token type to rule (check for := follows)               37940000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_RULE                  37950000
*           If the next char will return =                              37960000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'=') THEN                       37970000
*              Check if an operator is expected, if not error and stop  37980000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_OPERATOR,Z) THEN 37990000
                  MLWZMRPT RPTLINE=CL133'0Unexpected operator',APND_LC=X38000000
               C'Y'                                                     38010000
                  MVC   G_RETCODE,=F'8' * Set return code 8             38020000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        38030000
               ENDIF                                                    38040000
            ELSE                                                        38050000
*              If next char is not =, so it's a rule                    38060000
*              Check if a rule is expected, if not error and stop       38070000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_RULE,Z) THEN     38080000
                  MLWZMRPT RPTLINE=CL133'0Unexpected colon',APND_LC=C'YX38090000
               '                                                        38100000
                  MVC   G_RETCODE,=F'8' * Set return code 8             38110000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        38120000
               ENDIF                                                    38130000
            ENDIF                                                       38140000
*           If we end up here the rule or operator was valid            38150000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             38160000
*           Check for := on pos 71 (making = an ignore char)            38170000
            L     R6,G_SCAN_CURRCOL * Get current column                38180000
            C     R6,=F'71'         * Check for pos 72                  38190000
            BNL   SCAN_TOKEN_VALID  * Skip to finishing valid token     38200000
*           Check again for next char =                                 38210000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'=') THEN                       38220000
*              Set token type to operator                               38230000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_OPERATOR           38240000
               L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get addr SCAN_CHAR  38250000
               BASR  R14,R15             * Link to SCAN_CHAR section    38260000
               BAL   R8,STORE_TOKEN_CHAR * Add char to token 1          38270000
            ENDIF                                                       38280000
            B     SCAN_TOKEN_VALID * Skip to finishing valid token      38290000
         ENDIF                                                          38300000
*                                                                       38310000
*        Check for a variable                                           38320000
         IF (CLI,0(R5),EQ,C'$') THEN                                    38330000
*           Set token type to variable (check for $@ or $% follows)     38340000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_VARIABLE              38350000
*           Was it expected? If not, write error and stop               38360000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_OPENVAR,Z) THEN       38370000
               MLWZMRPT RPTLINE=CL133'0Unexpected variable or function'X38380000
               ,APND_LC=C'Y'                                            38390000
               MVC   G_RETCODE,=F'8' * Set return code 8                38400000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           38410000
            ENDIF                                                       38420000
*                                                                       38430000
*           Check if next char will be @                                38440000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'@') THEN                       38450000
*              Set token type to target variable                        38460000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_ACRO               38470000
*              Was it expected? If so, continue checking because the    38480000
*              expected bitstring isn't complete                        38490000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_ACRO,O) THEN     38500000
*                 Check if in recipe                                    38510000
                  TM    G_SCAN_STATE,SCAN_STATE_IN_RECIPE               38520000
                  IF (Z) THEN     * Not in recipe, keep on checking     38530000
*                    Check if in expand (resolving variables in rule    38540000
*                    requisites during phase 2)                         38550000
                     TM    G_SCAN_STATE,SCAN_STATE_IN_EXPAND            38560000
                  ENDIF                                                 38570000
*                 If either of the tests above gives CC ones            38580000
                  IF (O) THEN                                           38590000
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1    38600000
                     L     R15,LWZMAKE_SCAN_CHARA_TOKEN                 38610000
                     BASR  R14,R15             * Link to SCAN_CHAR      38620000
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1    38630000
                     B     SCAN_TOKEN_VALID    * Skip to finish token   38640000
                  ENDIF                                                 38650000
               ENDIF                                                    38660000
*              If not expected or the other tests above fail, write     38670000
*              error and stop                                           38680000
               MLWZMRPT RPTLINE=CL133'0Unexpected target variable',APNDX38690000
               _LC=C'Y'                                                 38700000
               MVC   G_RETCODE,=F'8' * Set return code 8                38710000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           38720000
            ENDIF                                                       38730000
*                                                                       38740000
*           Check if next char will be %                                38750000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'%') THEN                       38760000
*              Set token type to target member variable                 38770000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_PERCENT            38780000
*              Was it expected? If so, continue checking because the    38790000
*              expected bitstring isn't complete                        38800000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_PERCENT,O) THEN  38810000
*                 Check if in recipe                                    38820000
                  TM    G_SCAN_STATE,SCAN_STATE_IN_RULE2                38830000
                  IF (Z) THEN     * Not in rule2, keep on checking      38840000
                     TM    G_SCAN_STATE,SCAN_STATE_IN_RULE3             38850000
                  ENDIF                                                 38860000
                  IF (Z) THEN     * Not in rule3, keep op checking      38870000
*                    Check if in expand (resolving variables in rule    38880000
*                    requisites during phase 2)                         38890000
                     TM    G_SCAN_STATE,SCAN_STATE_IN_EXPAND            38900000
                  ENDIF                                                 38910000
*                 If either of the tests above gives CC ones            38920000
                  IF (NO) THEN                                          38930000
                     MVI   G_SCAN_TOKENTYPE,X'00'                       38940000
                  ENDIF                                                 38950000
                  BAL   R8,STORE_TOKEN_CHAR * Add char to token 1       38960000
                  L     R15,LWZMAKE_SCAN_CHARA_TOKEN                    38970000
                  BASR  R14,R15             * Link to SCAN_CHAR         38980000
                  BAL   R8,STORE_TOKEN_CHAR * Add char to token 1       38990000
                  B     SCAN_TOKEN_VALID    * Skip to finish token      39000000
               ELSE                                                     39010000
                  MLWZMRPT RPTLINE=CL133'0Unexpected target member variX39020000
               able',APND_LC=C'Y'                                       39030000
                  MVC   G_RETCODE,=F'8' * Set return code 8             39040000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        39050000
               ENDIF                                                    39060000
            ENDIF                                                       39070000
*                                                                       39080000
*           So it's not $@ or $%, continue checking normal variable     39090000
*           If we're at pos 71 or more, no sense checking the rest      39100000
*           because there's not enough room for a variable              39110000
            L     R6,G_SCAN_CURRCOL * Get current column                39120000
            C     R6,=F'70'         * Check for pos 71 or above         39130000
            IF (NL) THEN            * If so write error and stop        39140000
               MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'       39150000
               MVC   G_RETCODE,=F'8' * Set return code 8                39160000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           39170000
            ENDIF                                                       39180000
*                                                                       39190000
*           Check if next char will be either ( or {                    39200000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'(') THEN                       39210000
*              Save matching close bracket to check later               39220000
               MVI   G_SCAN_CLOSE_BRACKET,C')'                          39230000
            ELSE                                                        39240000
               IF (CLI,G_SCAN_PEEKCHAR,EQ,C'{') THEN                    39250000
*                 Save matching close bracket to check later            39260000
                  MVI   G_SCAN_CLOSE_BRACKET,C'}'                       39270000
               ELSE                                                     39280000
*                 Neither ( nor { is a syntax error                     39290000
                  MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'    39300000
                  MVC   G_RETCODE,=F'8' * Set return code 8             39310000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        39320000
               ENDIF                                                    39330000
            ENDIF                                                       39340000
*                                                                       39350000
*           Valid variable start, so store in token 1                   39360000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             39370000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  39380000
            BASR  R14,R15             * Link to SCAN_CHAR section       39390000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             39400000
*                                                                       39410000
*           The next bit of code is a dirty trick because determining   39420000
*           if the scan state should be set to rule should really only  39430000
*           be done in the statement parser section.                    39440000
*           The thing is, when a variable is the first token the scan   39450000
*           state should be IN_RULE before pushing it on the stack and  39460000
*           setting it to IN_VARIABLE(R)                                39470000
            IC    R14,G_SCAN_STATE * Get the scan state                 39480000
            N     R14,=X'0000007F' * Clear out bits 0-56, including     39490000
*                                  * the high order bit in the scan     39500000
*                                  * state for 'in recipe'              39510000
            C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in stmt    39520000
            IF (EQ) THEN           * If so...                           39530000
*              Clear scan state except for left most bit 'in recipe'    39540000
               NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                  39550000
*              Set scan state bits to IN_RULE                           39560000
               OI    G_SCAN_STATE,SCAN_STATE_IN_RULE                    39570000
            ENDIF                                                       39580000
*                                                                       39590000
*           Push current scan state on the stack before setting         39600000
*           it to IN_VARIABLE(R)                                        39610000
            LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack  39620000
            XR    R2,R2                 * Clear R2                      39630000
            IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index   39640000
            IC    R15,G_SCAN_STATE      * Get current scan state        39650000
            STC   R15,0(R2,R1)          * and store it in the stack     39660000
            LA    R2,1(,R2)             * Add 1 to stack index          39670000
            C     R2,=A(L'G_SCAN_STATE_STACK) * Is stack full?          39680000
            IF (H) THEN                 * Yep, write error and stop     39690000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX39700000
               flow',APND_LC=C'Y'                                       39710000
               MVC   G_RETCODE,=F'12'   * Set return code 12            39720000
               B     SCAN_TOKEN_RET     * Skip rest of tokenizer        39730000
            ENDIF                                                       39740000
            STC   R2,G_SCAN_STATE_STACK_IDX * Store new stack size      39750000
*                                                                       39760000
*           Clear scan state except left most bit 'in recipe', which    39770000
*           sets CC                                                     39780000
            NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                     39790000
            IF (Z) THEN           * If in recipe bit is off             39800000
*              Set scan state bits to IN_VARIABLE                       39810000
               OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLE                39820000
            ELSE                  * Else if recipe bit is on            39830000
*              Set scan state bits to IN_VARIABLER                      39840000
               OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLER               39850000
            ENDIF                                                       39860000
            B     SCAN_TOKEN_VALID * Skip to finishing valid token      39870000
         ENDIF                                                          39880000
*                                                                       39890000
*        Check for a closing bracket                                    39900000
         CLI   0(R5),C')'          * Is it a )                          39910000
         IF (NE) THEN              * If not                             39920000
            CLI   0(R5),C'}'       * Is it a }                          39930000
         ENDIF                                                          39940000
         IF (EQ) THEN              * If it was ) or }                   39950000
*           Set token type to closing bracket                           39960000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          39970000
*           Was if expected? If not, write error and stop               39980000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_CLOSEBRC,Z) THEN      39990000
UNEXPECTED_CLOSE_BRACKET EQU *                                          40000000
               MLWZMRPT RPTLINE=CL133'0Unexpected close bracket',APND_LX40010000
               C=C'Y'                                                   40020000
               MVC   G_RETCODE,=F'8' * Set return code 8                40030000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           40040000
            ENDIF                                                       40050000
*                                                                       40060000
*           Check if close bracket matches previous open bracket        40070000
            CLC   0(1,R5),G_SCAN_CLOSE_BRACKET                          40080000
            BNE UNEXPECTED_CLOSE_BRACKET                                40090000
*                                                                       40100000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             40110000
*                                                                       40120000
*           Check if scan state is form of IN_VARIABLE*, if not it's    40130000
*           just a closing bracket, meaning no pop of scan state stack  40140000
            IC    R14,G_SCAN_STATE * Get the scan state                 40150000
            N     R14,=X'0000007F' * Clear out bits 0-56, including     40160000
*                                  * the high order bit in the scan     40170000
*                                  * state for 'in recipe'              40180000
            C     R14,=A(SCAN_STATE_IN_VARIABLE2) * Check in variable2  40190000
            IF (NE) THEN           * If not, check in variableR         40200000
               C     R14,=A(SCAN_STATE_IN_VARIABLER)                    40210000
               BNE   SCAN_TOKEN_VALID * Neither, so skip to finish      40220000
            ENDIF                                                       40230000
            LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack  40240000
            XR    R2,R2                 * Clear R2                      40250000
            IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index   40260000
            BCTR  R2,R0                 * Subtract 1 from index         40270000
            IC    R15,0(R2,R1)          * Get stack state in that idx   40280000
            STC   R15,G_SCAN_STATE      * And save it as current state  40290000
            STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx   40300000
            B     SCAN_TOKEN_VALID      * Skip to finishing valid token 40310000
         ENDIF                                                          40320000
*                                                                       40330000
*        Check for a comma                                              40340000
         CLI   0(R5),C','          * Is it a ,                          40350000
         IF (EQ) THEN              * If it was ) or }                   40360000
*           Set token type to comma                                     40370000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA                 40380000
*           Was if expected? If not, write error and stop               40390000
            IF (TM,G_SCAN_EXPECTED+2,SCAN_EXPECTED3_COMMA,Z) THEN       40400000
               MLWZMRPT RPTLINE=CL133'0Unexpected comma',APND_LC=C'Y'   40410000
               MVC   G_RETCODE,=F'8' * Set return code 8                40420000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           40430000
            ENDIF                                                       40440000
*                                                                       40450000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             40460000
            B     SCAN_TOKEN_VALID      * Skip to finishing valid token 40470000
         ENDIF                                                          40480000
*                                                                       40490000
*        Special variables are the first token in a new statement, so   40500000
*        if we're already in one, skip the special variable check       40510000
         IC    R14,G_SCAN_STATE   * Get the scan state                  40520000
         N     R14,=X'0000007F'   * Clear out bits 0-56, including      40530000
*                                 * the high order bit in the scan      40540000
*                                 * state for 'in recipe'               40550000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in statement  40560000
         BNE   SKIP_SCAN_SPECIAL  * If not skip special var check       40570000
*                                                                       40580000
*        Check for special variable. This snippet only checks for a     40590000
*        variable that starts with . and consists of valid special var  40600000
*        characters. Whether it's semantically valid is checked in the  40610000
*        statement parser.                                              40620000
         IF (CLI,0(R5),EQ,C'.') THEN                                    40630000
*           Set token type to special variable                          40640000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_SPECIAL               40650000
*           Was it expected? If not, write error and stop               40660000
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_SPECIAL,Z) THEN     40670000
               MLWZMRPT RPTLINE=CL133'0Unexpected special variable',APNX40680000
               D_LC=C'Y'                                                40690000
               MVC   G_RETCODE,=F'8' * Set return code 8                40700000
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer       40710000
            ENDIF                                                       40720000
*                                                                       40730000
STORE_NEXT_SPECIAL_TOKEN_CHAR EQU *                                     40740000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             40750000
*                                                                       40760000
*           Positions up to 72 count as part of the special var name    40770000
*           We check peek char so when the next token is parsed,        40780000
*           scanning continues with the char directly after the special 40790000
*           variable name                                               40800000
            L     R6,G_SCAN_CURRCOL * Get current column                40810000
            C     R6,=F'71'         * Check pos 72 and if we're there   40820000
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token     40830000
*                                                                       40840000
            LA    R2,G_SCAN_PEEKCHAR * Point R2 to peek char            40850000
            TRT   0(1,R2),SPECIAL_TOKEN_NEXTCHAR * Check for valid char 40860000
            BNZ   SCAN_TOKEN_VALID  * If not, skip to finish token      40870000
*           Ending up here means the char is valid, to scan it and      40880000
*           loop around to store it and check the next                  40890000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  40900000
            BASR  R14,R15           * Link to SCAN_CHAR section         40910000
            B     STORE_NEXT_SPECIAL_TOKEN_CHAR * Loop around           40920000
         ENDIF                                                          40930000
*                                                                       40940000
SKIP_SCAN_SPECIAL EQU *                                                 40950000
*                                                                       40960000
*        Check for normal token                                         40970000
         TRT   0(1,R5),NORMAL_TOKEN_STARTCHAR                           40980000
         IF (Z) THEN                                                    40990000
*           Set token type to normal                                    41000000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_NORMAL                41010000
*           Was it expected? If not, write error and stop               41020000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_NORMAL,Z) THEN        41030000
               MLWZMRPT RPTLINE=CL133'0Unexpected token',APND_LC=C'Y'   41040000
               MVC   G_RETCODE,=F'8' * Set return code 8                41050000
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer       41060000
            ENDIF                                                       41070000
*                                                                       41080000
STORE_NEXT_NORMAL_TOKEN_CHAR EQU *                                      41090000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             41100000
*                                                                       41110000
*           Positions up to 72 count as part of the normal token name   41120000
*           We check peek char so when the next token is parsed,        41130000
*           scanning continues with the char directly after the normal  41140000
*           token name                                                  41150000
            L     R6,G_SCAN_CURRCOL * Get current column                41160000
            C     R6,=F'71'         * Check pos 72 and if we're there   41170000
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token     41180000
*                                                                       41190000
            LA    R2,G_SCAN_PEEKCHAR * Point R2 to peek char            41200000
            TRT   0(1,R2),NORMAL_TOKEN_NEXTCHAR * Check for valid char  41210000
            BNZ   SCAN_TOKEN_VALID   * If not, skip to finish token     41220000
*           Ending up here means the char is valid, to scan it and      41230000
*           loop around to store it and check the next                  41240000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  41250000
            BASR  R14,R15         * Link to SCAN_CHAR section           41260000
            B     STORE_NEXT_NORMAL_TOKEN_CHAR * Loop around            41270000
         ENDIF                                                          41280000
*                                                                       41290000
*        Check for number token                                         41300000
         TRT   0(1,R5),NUMBER_TOKEN_CHAR                                41310000
         IF (Z) THEN                                                    41320000
*           Set token type to number                                    41330000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_NUMBER                41340000
*           Was it expected? If not, write error and stop               41350000
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_NUMBER,Z) THEN      41360000
               MLWZMRPT RPTLINE=CL133'0Unexpected number',APND_LC=C'Y'  41370000
               MVC   G_RETCODE,=F'8' * Set return code 8                41380000
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer       41390000
            ENDIF                                                       41400000
*                                                                       41410000
STORE_NEXT_NUMBER_TOKEN_CHAR EQU *                                      41420000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             41430000
*                                                                       41440000
*           Positions up to 72 count as part of the number token name   41450000
*           We check peek char so when the next token is parsed,        41460000
*           scanning continues with the char directly after the number  41470000
*           token name                                                  41480000
            L     R6,G_SCAN_CURRCOL * Get current column                41490000
            C     R6,=F'71'         * Check pos 72 and if we're there   41500000
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token     41510000
*                                                                       41520000
            LA    R5,G_SCAN_PEEKCHAR * Point R2 to peek char            41530000
            TRT   0(1,R5),NUMBER_TOKEN_CHAR * Check for valid char      41540000
            BNZ   SCAN_TOKEN_VALID   * If not, skip to finish token     41550000
*           Ending up here means the char is valid, to scan it and      41560000
*           loop around to store it and check the next                  41570000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  41580000
            BASR  R14,R15         * Link to SCAN_CHAR section           41590000
            B     STORE_NEXT_NUMBER_TOKEN_CHAR * Loop around            41600000
         ENDIF                                                          41610000
*                                                                       41620000
*        If the scanned character doesn't start any of the previous     41630000
*        types, just store the character, don't set a type and just let 41640000
*        the parser take care of it                                     41650000
         BAL   R8,STORE_TOKEN_CHAR * Add char to token 1                41660000
*                                                                       41670000
*        Previous code jumps here when a valid done is completely       41680000
*        scanned                                                        41690000
SCAN_TOKEN_VALID EQU *                                                  41700000
*        If normal token, it could be a call keyword                    41710000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN        41720000
            CLC   G_SCAN_TOKEN_LEN,=F'4' * Is the token 4 bytes long?   41730000
            IF (EQ) THEN                                                41740000
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1        41750000
               MVC   G_CALL4,0(R14)       * Copy to helper var          41760000
               OC    G_CALL4,=X'40404040' * Convert to uppercase        41770000
               CLC   G_CALL4,=C'CALL'     * Is it CALL?                 41780000
               IF (EQ) THEN               * If so...                    41790000
*                 Set token type to call                                41800000
                  MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CALL            41810000
               ENDIF                                                    41820000
            ENDIF                                                       41830000
         ENDIF                                                          41840000
*                                                                       41850000
*        Write a trace record for the token we just scanned             41860000
*        Put token type char and scanned token in helper data           41870000
         MVC   G_HELPER_DATA(1),G_SCAN_TOKENTYPE * Store token type     41880000
         MVI   G_HELPER_DATA+1,C' '  * followed by space                41890000
         LA    R2,G_HELPER_DATA+2    * Point R2 to where token comes    41900000
         L     R3,G_SCAN_TOKENA      * Point R3 to scanned token        41910000
         L     R4,G_SCAN_TOKEN_LEN   * Get length of scanned token      41920000
         C     R4,=A(L'G_HELPER_DATA-2) * Compare it to leftover space  41930000
         IF (H) THEN                 * If too long                      41940000
            L     R4,=A(L'G_HELPER_DATA-2) * Replace len with what fits 41950000
         ENDIF                                                          41960000
         LA    R5,2(,R4)             * Put the correct length in R5     41970000
         BCTR  R4,R0                 * R4 = R4 - 1 for EX               41980000
         B     *+10                  * Skip MVC constant for EX         41990000
         MVC   0(1,R2),0(R3)         * MVC constant for EX              42000000
         EX    R4,*-6                * EX previous MVC stmt with R4     42010000
         LA    R2,G_HELPER_DATA      * Get address of helper data       42020000
         ST    R2,G_LWZMTRC_DATA_PTR * And store it as trace data ptr   42030000
         STH   R5,G_LWZMTRC_DATA_SIZ * And store length as data length  42040000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'610',DATA          42050000
*                                                                       42060000
SCAN_TOKEN_RET EQU *                                                    42070000
         MLWZTERM                 * Return back to caller               42080000
*                                                                       42090000
* Store token char routine, appends a scanned char to token 1           42100000
* R7 is expected to contain the position within token 1 where the next  42110000
* character should be stored (used as an index register).               42120000
*                                                                       42130000
STORE_TOKEN_CHAR EQU *                                                  42140000
         C     R7,=A(SCAN_TOKEN_MAXLEN) * Have we exhausted token 1?    42150000
         IF (GT) THEN             * If so, write error and stop         42160000
            MLWZMRPT RPTLINE=CL133'0Internal error, no more room left iX42170000
               n 4K token space',APND_LC=C'Y'                           42180000
            MVC   G_RETCODE,=F'12' * Set return code 12                 42190000
            B     SCAN_TOKEN_RET   * Dirty jump to end of tokenizer     42200000
         ENDIF                     * but quicker than check retcode     42210000
*                                  * everytime after this routine       42220000
         L     R4,G_SCAN_TOKENA   * Point R4 to token 1                 42230000
         IC    R5,G_SCAN_CURRCHAR * Get the last scanned character      42240000
         STC   R5,0(R7,R4)        * And store it at index R7            42250000
         LA    R7,1(,R7)          * Increase index R7 with 1            42260000
         ST    R7,G_SCAN_TOKEN_LEN * and store it as token 1 length     42270000
         BR    R8                 * Return                              42280000
*                                                                       42290000
         LTORG                                                          42300000
*                                                                       42310000
* Local constant pointers to section addresses                          42320000
LWZMAKE_SCAN_CHARA_TOKEN     DC    A(LWZMAKE_SCAN_CHAR)                 42330000
*                                                                       42340000
* Local constant pointers to previously defined constants, but too far  42350000
* away for local addressing                                             42360000
SCAN_STATE_TABLEA_TOKEN      DC    A(SCAN_STATE_TABLE)                  42370000
*                                                                       42380000
* Translate table for starting character for a normal token             42390000
* Can be [a-zA-Z]                                                       42400000
NORMAL_TOKEN_STARTCHAR DS 0F                                            42410000
         DC    256X'FF'                                                 42420000
         ORG   NORMAL_TOKEN_STARTCHAR+C'a'                              42430000
         DC    X'000000000000000000'                                    42440000
         ORG   NORMAL_TOKEN_STARTCHAR+C'j'                              42450000
         DC    X'000000000000000000'                                    42460000
         ORG   NORMAL_TOKEN_STARTCHAR+C's'                              42470000
         DC    X'0000000000000000'                                      42480000
         ORG   NORMAL_TOKEN_STARTCHAR+C'A'                              42490000
         DC    X'000000000000000000'                                    42500000
         ORG   NORMAL_TOKEN_STARTCHAR+C'J'                              42510000
         DC    X'000000000000000000'                                    42520000
         ORG   NORMAL_TOKEN_STARTCHAR+C'S'                              42530000
         DC    X'0000000000000000'                                      42540000
         ORG                                                            42550000
*                                                                       42560000
* Translate table for any character for a normal token except the first 42570000
* Can be [$#@_a-zA-Z0-9]                                                42580000
NORMAL_TOKEN_NEXTCHAR DS 0F                                             42590000
         DC    256X'FF'                                                 42600000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'$'                               42610000
         DC    X'00'                                                    42620000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'#'                               42630000
         DC    X'00'                                                    42640000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'@'                               42650000
         DC    X'00'                                                    42660000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'_'                               42670000
         DC    X'00'                                                    42680000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'a'                               42690000
         DC    X'000000000000000000'                                    42700000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'j'                               42710000
         DC    X'000000000000000000'                                    42720000
         ORG   NORMAL_TOKEN_NEXTCHAR+C's'                               42730000
         DC    X'0000000000000000'                                      42740000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'A'                               42750000
         DC    X'000000000000000000'                                    42760000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'J'                               42770000
         DC    X'000000000000000000'                                    42780000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'S'                               42790000
         DC    X'0000000000000000'                                      42800000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'0'                               42810000
         DC    X'00000000000000000000'                                  42820000
         ORG                                                            42830000
*                                                                       42840000
* Translate table for any character for a special token                 42850000
* Can be [_A-Z]                                                         42860000
SPECIAL_TOKEN_NEXTCHAR DS 0F                                            42870000
         DC    256X'FF'                                                 42880000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'_'                              42890000
         DC    X'00'                                                    42900000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'A'                              42910000
         DC    X'000000000000000000'                                    42920000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'J'                              42930000
         DC    X'000000000000000000'                                    42940000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'S'                              42950000
         DC    X'0000000000000000'                                      42960000
         ORG                                                            42970000
*                                                                       42980000
* Translate table for any character for a number token                  42990000
* Can be [0-9]                                                          43000000
NUMBER_TOKEN_CHAR DS 0F                                                 43010000
         DC    256X'FF'                                                 43020000
         ORG   NUMBER_TOKEN_CHAR+C'0'                                   43030000
         DC    X'00000000000000000000'                                  43040000
         ORG                                                            43050000
*                                                                       43060000
*********************************************************************** 43070000
* Section: LWZMAKE_SCAN_CHAR                                          * 43080000
* Purpose: Parsing scanner. This section gets the next character from * 43090000
*          whichever input is at the top of the input stack. For a    * 43100000
*          makefile, after column 80 a next record is read, any other * 43110000
*          type of input is simply read until its last char. If an    * 43120000
*          input is exhausted it is popped from the input stack.      * 43130000
*          R9 should point to global data.                            * 43140000
*********************************************************************** 43150000
LWZMAKE_SCAN_CHAR MLWZSAVE                                              43160000
*        Trace record to start section                                  43170000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEEBUG,MSGNR=C'604',CONST=C'LWZMX43180000
               AKE_SCAN_CHAR'                                           43190000
*                                                                       43200000
         MVI   G_SCAN_NEWLINE,C'N' * Initialize newline                 43210000
         MVC   G_SCAN_CURRCHAR(2),=X'0000' * Init CURRCHAR + PEEKCHAR   43220000
*                                                                       43230000
SCAN_CHAR_CHECK_INPUT_STACK EQU *                                       43240000
*        Check for empty input stack, of so set EOF and skip the rest   43250000
         IF (CLI,G_SCAN_INPUT_STACK_IDX,EQ,X'00') THEN                  43260000
            MVI   G_MKFEOF,C'Y'   * Set EOF switch to Y                 43270000
            B     SCAN_CHAR_RET   * Skip rest of scanner                43280000
         ELSE                                                           43290000
*           Get the top entry in the input stack                        43300000
            XR    R2,R2           * Clear R2                            43310000
            XR    R3,R3           *   and R3                            43320000
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index   43330000
            BCTR  R3,R0           * Subtract 1 for calculating offset   43340000
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to entry    43350000
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack       43360000
            AR    R2,R3           * Add calculated offset               43370000
*                                                                       43380000
            USING INPUT_DSECT,R2  * Address with INPUT DSECT            43390000
*                                                                       43400000
            CLI   INPUTTYPE,X'00' * Check for input from makefile       43410000
            BE    SCAN_CHAR_READ_FROM_MAKEFILE * If so jump ahead       43420000
*                                                                       43430000
            XR    R3,R3                                                 43440000
            LH    R3,INPUTLEAD                                          43450000
            CH    R3,=H'0'                                              43460000
            IF (NE) THEN                                                43470000
               MVI   G_SCAN_CURRCHAR,C' '                               43480000
               BCTR  R3,R0                                              43490000
               STH   R3,INPUTLEAD                                       43500000
               B     SCAN_CHAR_RET                                      43510000
            ENDIF                                                       43520000
*                                                                       43530000
            XR    R3,R3           * Clear R3                            43540000
            LH    R3,INPUTPOS     * Get next position in input          43550000
            CH    R3,INPUTLEN     * Has input been exhausted?           43560000
            IF (L) THEN           * If not...                           43570000
               L     R4,INPUTPTR  * Point R4 to input                   43580000
               IC    R5,0(R3,R4)  * Get the next character from input   43590000
               STC   R5,G_SCAN_CURRCHAR * and put it in CURRCHAR        43600000
               LA    R3,1(,R3)    * Advance next position               43610000
               STH   R3,INPUTPOS  * and store it in input block         43620000
               CH    R3,INPUTLEN  * Was this the last char?             43630000
               IF (L) THEN        * If not, also get PEEKCHAR           43640000
                  IC    R5,0(R3,R4) * Get the next char + 1 from input  43650000
                  STC   R5,G_SCAN_PEEKCHAR * and put it in PEEKCHAR     43660000
               ENDIF                                                    43670000
               B     SCAN_CHAR_RET * Skip rest of scanner               43680000
            ELSE                  * Else, input exhausted               43690000
               XR    R3,R3        * Clear R3                            43700000
               IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack idx  43710000
               BCTR  R3,R0        * Subtract 1                          43720000
               STC   R3,G_SCAN_INPUT_STACK_IDX * and put back in stack  43730000
               CLI   INPUTTYPE,X'01' * Input type string that continues 43740000
*                                 * with popped stack entry?            43750000
               BNE   SCAN_CHAR_RET * If not, skip rest of scan char     43760000
               B     SCAN_CHAR_CHECK_INPUT_STACK * Loop around to       43770000
*                                 * check the input stack again         43780000
            ENDIF                                                       43790000
         ENDIF                                                          43800000
         DROP  R2                                                       43810000
*                                                                       43820000
SCAN_CHAR_READ_FROM_MAKEFILE EQU *                                      43830000
*        A makefile has LRECL 80 but in order to return a line change   43840000
*        as a separate token, a fictitious 81st column is used for that 43850000
*        When we go beyond column 81 that triggers a next record to be  43860000
*        read from the makefile.                                        43870000
         L     R3,G_SCAN_CURRCOL  * Get the current column              43880000
         LA    R3,1(,R3)          * Advance 1 position                  43890000
         ST    R3,G_SCAN_CURRCOL  * and put it back                     43900000
         C     R3,=F'80'          * Check if we're past 81 yet          43910000
         IF (GT) THEN             * If so...                            43920000
            MVI   G_SCAN_CONTINUED_LINE,C'N' * Reset continued line     43930000
            BAL   R8,READNEXT     * Read another input record           43940000
         ELSE                     * Not past 81                         43950000
            IF (EQ) THEN          * Buf if exactly at 81                43960000
               MVI   G_SCAN_NEWLINE,C'Y' * Set new line switch to Y     43970000
               B     SCAN_CHAR_RET       * Skip rest of scanner         43980000
            ENDIF                                                       43990000
         ENDIF                                                          44000000
*                                                                       44010000
*        Check for EOF                                                  44020000
         CLI   G_MKFEOF,C'Y'      * Is the EOF switch set?              44030000
         BE    SCAN_CHAR_RET      * If so skip rest of scanner          44040000
*                                                                       44050000
*        If we're here, we're scanning a column between 1 and 80        44060000
         LA    R4,G_MAKEFILE_REC  * Point R4 to the last read record    44070000
         L     R3,G_SCAN_CURRCOL  * Get the current column              44080000
         IC    R5,0(R3,R4)        * Get the next character from input   44090000
         STC   R5,G_SCAN_CURRCHAR * and put it in CURRCHAR              44100000
         C     R3,=F'79'          * Check pos = 80                      44110000
         IF (LT) THEN             * Only pos < 80 there's a PEEKCHAR    44120000
            IC    R5,1(R3,R4)     * Get the next char + 1 from input    44130000
            STC   R5,G_SCAN_PEEKCHAR * and put it in PEEKCHAR           44140000
         ENDIF                                                          44150000
*                                                                       44160000
*        The rest of this scanner code is for writing a deeebug trace   44170000
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEEBUG) THEN         44180000
            LA    R2,G_HELPER_DATA   * Point R2 to helper data          44190000
            MVC   0(5,R2),=C'LINE '  * Start with line constant         44200000
            LA    R2,5(,R2)          * Advance R2 to right after        44210000
            LA    R6,5               * Use R6 as length                 44220000
            L     R3,G_SCAN_CURRLINE * Get the current line number      44230000
            CVD   R3,G_DEC8          * Convert it to packed decimal     44240000
            UNPK  G_ZONED8,G_DEC8    * Convert it to zoned              44250000
            OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble           44260000
            MVC   0(8,R2),G_ZONED8   * Append line number to trc data   44270000
            LA    R2,8(,R2)          * Advance R2 to right after        44280000
            LA    R6,8(,R6)          * Add 8 to length                  44290000
            MVC   0(8,R2),=C' COLUMN ' * Append column constant         44300000
            LA    R2,8(,R2)          * Advance R2 to right after        44310000
            LA    R6,8(,R6)          * Add 8 to length                  44320000
            L     R3,G_SCAN_CURRCOL  * Get the current column numer     44330000
            LA    R3,1(,R3)          * Add 1 because it's zero based    44340000
            CVD   R3,G_DEC8          * Convert it to packed decimal     44350000
            UNPK  G_ZONED8,G_DEC8    * Convert it to zoned              44360000
            OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble           44370000
            MVC   0(8,R2),G_ZONED8   * Append column number to trc data 44380000
            LA    R2,8(,R2)          * Advance R2 to right after        44390000
            LA    R6,8(,R6)          * Add 8 to length                  44400000
            MVI   0(R2),C' '         * Append a space                   44410000
            MVC   1(1,R2),G_SCAN_CURRCHAR * Append current char         44420000
            MVI   2(R2),C' '         * Append another space             44430000
            MVC   3(1,R2),G_SCAN_PEEKCHAR * Append peek char            44440000
            LA    R6,4(,R6)          * Add 4 to length                  44450000
            LA    R2,G_HELPER_DATA   * Get address of helper data       44460000
            ST    R2,G_LWZMTRC_DATA_PTR * Set is as trace data pointer  44470000
            STH   R6,G_LWZMTRC_DATA_SIZ * Store R6 as trace data length 44480000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEEBUG,MSGNR=C'611',DATA      44490000
         ENDIF                                                          44500000
*                                                                       44510000
SCAN_CHAR_RET EQU *                                                     44520000
         MLWZTERM                 * Return back to caller               44530000
*                                                                       44540000
* Read next MAKEFILE record                                             44550000
*                                                                       44560000
READNEXT EQU   *                                                        44570000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              44580000
         LA    R3,DCBMKF-DCB_DSECT(,R14) * Get addr of MAKEFILE DCB     44590000
         LA    R7,READNEXT_10     * Set R7 to statement EODAD routine   44600000
*                                 * branches to at EOF                  44610000
         GET   (R3),G_MAKEFILE_REC * Get a record from makefile         44620000
READNEXT_10 EQU *                                                       44630000
         IF (CLI,G_MKFEOF,NE,C'Y') THEN * Did we hit EOF? If not        44640000
            MVC   G_SCAN_CURRCOL,=F'0' * Reset current column to 0      44650000
            L     R4,G_SCAN_CURRLINE * Get the current line             44660000
            LA    R4,1(,R4)          * Advance 1 line count             44670000
            ST    R4,G_SCAN_CURRLINE * And put it back as current line  44680000
         ENDIF                                                          44690000
*                                                                       44700000
         BR    R8                 * Return                              44710000
*                                                                       44720000
         LTORG                                                          44730000
*********************************************************************** 44740000
* Section: LWZMAKE_ALLOC_STMT                                         * 44750000
* Purpose: Allocate a memory block for an internal representation of  * 44760000
*          a statement. This section also fills in the generic part   * 44770000
*          of the allocated statement block, including chaining into  * 44780000
*          the linked list of statements.                             * 44790000
*          R9 should point to global data.                            * 44800000
*********************************************************************** 44810000
LWZMAKE_ALLOC_STMT MLWZSAVE                                             44820000
*        Trace record to start section                                  44830000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX44840000
               KE_ALLOC_STMT'                                           44850000
*                                                                       44860000
         MVC   G_STMT_ALLOC_RETURN_PTR,=A(0) * Initialize return ptr    44870000
*                                                                       44880000
         MVC   G_STMT_SAVE_PTR,=A(0) * Initialize helper pointer        44890000
*                                                                       44900000
         LA    R7,G_STMT_LIST_PTR    * Get start of statement linked    44910000
*                                    * list                             44920000
FIND_STMT_SLOT EQU *                                                    44930000
         CLC   0(4,R7),=A(0)         * Have we reached an empty ptr?    44940000
         IF (NZ) THEN                * If not, go down the chain        44950000
            L     R6,0(,R7)          * Get the actual pointer           44960000
            ST    R6,G_STMT_SAVE_PTR * Save for back chain              44970000
            LA    R7,STMT_NEXT_PTR-STMT_DSECT(,R6) * Get next stmt ptr  44980000
            B     FIND_STMT_SLOT     * and loop around to check it      44990000
         ENDIF                                                          45000000
*                                                                       45010000
*        Allocate a statement block                                     45020000
         L     R4,G_STMT_ALLOC_LEN  * Get length to allocate            45030000
         STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block           45040000
         ST    R1,0(,R7)            * Save it at forward chain address  45050000
         ST    R1,G_STMT_ALLOC_RETURN_PTR * Also save it as return ptr  45060000
*                                                                       45070000
*        Write a trace record for allocated block                       45080000
         ST    R1,G_DEC8             * Put in var with at least 5 bytes 45090000
         UNPK  G_ZONED8(9),G_DEC8(5) * Turn into almost hex             45100000
         TR    G_ZONED8,STMT_HEXTAB  * Turn into hex                    45110000
         MVC   G_HELPER_DATA(8),G_ZONED8 * Copy 8 hex chars to helper   45120000
         LA    R2,G_HELPER_DATA      * Get address of helper data       45130000
         ST    R2,G_LWZMTRC_DATA_PTR * put in in trace record data ptr  45140000
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace record data length 8  45150000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'640',DATA           45160000
*                                                                       45170000
*        Initialize block                                               45180000
         L     R2,G_STMT_ALLOC_RETURN_PTR * Point R2 to memory block    45190000
         L     R3,G_STMT_ALLOC_LEN  * Get length of block               45200000
         XR    R0,R0                * Clear R0                          45210000
         XR    R1,R1                *   and R1                          45220000
         MVCL  R2,R0                * Zero out memory block             45230000
*                                                                       45240000
*        Fill in generic stuff                                          45250000
         L     R1,G_STMT_ALLOC_RETURN_PTR                               45260000
*        Fill in block length and statement type                        45270000
         MVC   STMT_LEN-STMT_DSECT(4,R1),G_STMT_ALLOC_LEN               45280000
         MVC   STMT_TYPE-STMT_DSECT(1,R1),G_STMT_ALLOC_TYPE             45290000
*        Fill in switch for statement in recipe                         45300000
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN               45310000
            MVI   STMT_IN_RECIPE-STMT_DSECT(R1),C'Y'                    45320000
         ELSE                                                           45330000
            MVI   STMT_IN_RECIPE-STMT_DSECT(R1),C'N'                    45340000
         ENDIF                                                          45350000
*        Fill in back chain statement pointer                           45360000
         MVC   STMT_PREV_PTR-STMT_DSECT(4,R1),G_STMT_SAVE_PTR           45370000
*                                                                       45380000
ALLOC_STMT_RET EQU *                                                    45390000
         MLWZTERM                 * Return back to caller               45400000
*                                                                       45410000
         LTORG                                                          45420000
*                                                                       45430000
* Translate table for hex conversion                                    45440000
STMT_HEXTAB EQU   *-C'0'                                                45450000
            DC    C'0123456789ABCDEF'                                   45460000
*********************************************************************** 45470000
* Section: LWZMAKE_STORE_VAR                                          * 45480000
* Purpose: Assign a value to a variable. The binary search tree is    * 45490000
*          searched, if the variable is found the new value is        * 45500000
*          assigned. If not found, a new variable memory block is     * 45510000
*          allocated, added to the binary search tree and then the    * 45520000
*          value is assigned.                                         * 45530000
*          R7 should point to a statement block for an assignment     * 45540000
*          R9 should point to global data.                            * 45550000
*********************************************************************** 45560000
LWZMAKE_STORE_VAR MLWZSAVE                                              45570000
*        Trace record to start section                                  45580000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX45590000
               KE_STORE_VAR'                                            45600000
*                                                                       45610000
         USING STMT_A_DSECT,R7    * Use R7 for addressing statement     45620000
         USING VAR_DSECT,R6       * Use R6 for addressing variable      45630000
*                                                                       45640000
         LT    R6,G_FIRST_VAR_PTR * Get the start of the search tree    45650000
         IF (Z) THEN              * If this is the first var            45660000
            BAL   R8,ALLOC_VAR    * Perform allocate variable routine   45670000
            LR    R6,R1           * Copy returned pointer to R6         45680000
            ST    R6,G_FIRST_VAR_PTR * And set it as start of tree      45690000
         ELSE                     * Else, search tree not empty         45700000
TEST_VARS   EQU   *               * Test this var for matching name     45710000
            XR    R3,R3           * Clear R3                            45720000
            LH    R3,STMT_A_DESTLEN * and get length of stmt var name   45730000
            CH    R3,VARLEN       * Compare it to current var name len  45740000
            IF (H) THEN           * Compare length of shortest of the 2 45750000
               LH    R3,VARLEN    * If current var name len is less     45760000
            ENDIF                 * then use that                       45770000
            BCTR  R3,R0           * Subtract 1 for EX                   45780000
            LA    R2,STMT_A_DEST  * Point R2 to statement var name      45790000
            LA    R4,VARNAME      * Point R4 to current var name        45800000
            B     *+10            * Skip CLC constant for EX            45810000
            CLC   0(1,R2),0(R4)   * CLC constant for EX                 45820000
            EX    R3,*-6          * EX previous CLC stmt with R3        45830000
            IF (L) THEN           * If statement var name is lower      45840000
               CLC   VARLOW,=A(0) * Check if current var's low ptr set  45850000
               IF (EQ) THEN       * If not                              45860000
                  BAL   R8,ALLOC_VAR * Perform allocate var routine     45870000
                  ST    R1,VARLOW * Store returned pointer as low ptr   45880000
                  LR    R6,R1     * And set it as the current var ptr   45890000
                  B     FILL_VAR  * Skip to filling variable value      45900000
               ELSE               * Else, current var's low ptr is set  45910000
                  L     R6,VARLOW * Replace current var by low var      45920000
                  B     TEST_VARS * Loop around to test that one        45930000
               ENDIF                                                    45940000
            ELSE                  * Else, statement var name is >=      45950000
               IF (EQ) THEN       * If the compared part is equal       45960000
                  CLC   STMT_A_DESTLEN,VARLEN * Check if they're also   45970000
                  IF (EQ) THEN    * of equal length                     45980000
                     B     FILL_VAR * because then no alloc needed,     45990000
                  ENDIF           * just replacing the value            46000000
               ENDIF                                                    46010000
*              If we end up here, it's not a match                      46020000
               CLC   VARHIGH,=A(0) * Check if curr var's high ptr set   46030000
               IF (EQ) THEN       * If not                              46040000
                  BAL   R8,ALLOC_VAR * Perform allocate var routine     46050000
                  ST    R1,VARHIGH * Store returned pointer as high ptr 46060000
                  LR    R6,R1     * And set it as the current var ptr   46070000
                  B     FILL_VAR  * Skip to filling variable value      46080000
               ELSE               * Else, current var's high ptr is set 46090000
                  L     R6,VARHIGH * Replace current var by high var    46100000
                  B     TEST_VARS * Loop around to test that one        46110000
               ENDIF                                                    46120000
            ENDIF                                                       46130000
         ENDIF                                                          46140000
*                                                                       46150000
* Set a value, either in a new variable just allocated, or replacing a  46160000
* value of an existing variable                                         46170000
FILL_VAR EQU   *                                                        46180000
         CLC   VALLEN,=AL2(0)     * Check if there's an existing value  46190000
         IF (NE) THEN             * If so...                            46200000
            XR    R2,R2           * Clear R2                            46210000
            LH    R2,VALLEN       * and put old value length in         46220000
            L     R3,VALPTR       * Get old value pointer               46230000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  46240000
         ENDIF                                                          46250000
         XR    R2,R2              * Clear R2                            46260000
         LH    R2,STMT_A_SRCLEN   * Get new value length                46270000
         STH   R2,VALLEN          * Put it in variable block            46280000
         STORAGE OBTAIN,LENGTH=(R2) * Allocate value storage            46290000
         ST    R1,VALPTR          * Put new memory ptr in var block     46300000
         LR    R0,R1              * Copy new value ptr to R0            46310000
         LR    R1,R2              * Copy new value length to R1         46320000
         LR    R3,R1              * Make sure no cropping/filling       46330000
         LA    R2,STMT_A_SRC      * Point R2 to new value               46340000
         MVCL  R0,R2              * Copy new value to variable value    46350000
*                                                                       46360000
*        Write a report line with variable assignment                   46370000
         MVC   G_LWZMRPT_LINE,=CL133' .....................'            46380000
         LA    R2,G_LWZMRPT_LINE+1                                      46390000
         LA    R3,21                                                    46400000
         LA    R4,VARNAME                                               46410000
         LR    R5,R3                                                    46420000
         CH    R5,VARLEN                                                46430000
         IF (H) THEN                                                    46440000
            LH    R5,VARLEN                                             46450000
         ENDIF                                                          46460000
         BCTR  R5,R0                                                    46470000
         B     *+10                                                     46480000
         MVC   0(1,R2),0(R4)                                            46490000
         EX    R5,*-6                                                   46500000
         LA    R2,G_LWZMRPT_LINE+23                                     46510000
         LA    R3,110                                                   46520000
         L     R4,VALPTR                                                46530000
         LR    R5,R3                                                    46540000
         CH    R5,VALLEN                                                46550000
         IF (H) THEN                                                    46560000
            LH    R5,VALLEN                                             46570000
         ENDIF                                                          46580000
         LTR   R5,R5                                                    46590000
         BZ    STORE_VAR_WRITE_RPT                                      46600000
         BCTR  R5,R0                                                    46610000
         B     *+10                                                     46620000
         MVC   0(1,R2),0(R4)                                            46630000
         EX    R5,*-6                                                   46640000
STORE_VAR_WRITE_RPT EQU *                                               46650000
         L     R15,G_LWZMAKE_RPTA                                       46660000
         BASR  R14,R15                                                  46670000
*                                                                       46680000
         DROP  R6                                                       46690000
*                                                                       46700000
STORE_VAR_RET EQU *                                                     46710000
         MLWZTERM                 * Return back to caller               46720000
*                                                                       46730000
* Allocate a variable block. Value for variable is allocated separately 46740000
* The using for R7 in previous code is left intact to keep addressing   46750000
* of the assignment statement possible.                                 46760000
*                                                                       46770000
ALLOC_VAR EQU  *                                                        46780000
         GETMAIN RU,LV=VAR_DSECT_LEN * Allocate memory for var block    46790000
*                                                                       46800000
         LR    R5,R1              * Copy allocated memory ptr to R5     46810000
         USING VAR_DSECT,R5       * Addressing of new variable block    46820000
*                                                                       46830000
         MVC   VARLEN,STMT_A_DESTLEN * Copy variable name length        46840000
         MVC   VARNAME,STMT_A_DEST * Copy variable name (both 72 long)  46850000
*        Set the rest of the variable block to zero's                   46860000
         MVI   VALLEN,X'00'                                             46870000
         MVC   VALLEN+1(VAR_DSECT_LEN-L'VARLEN-L'VARNAME-1),VALLEN      46880000
*                                                                       46890000
*        Write a trace record for allocated block                       46900000
         ST    R5,G_DEC8             * Put in var with at least 5 bytes 46910000
         UNPK  G_ZONED8(9),G_DEC8(5) * Turn into almost hex             46920000
         TR    G_ZONED8,VAR_HEXTAB   * Turn into hex                    46930000
         MVC   G_HELPER_DATA(8),G_ZONED8 * Copy 8 hex chars to helper   46940000
         LA    R2,G_HELPER_DATA      * Get address of helper data       46950000
         ST    R2,G_LWZMTRC_DATA_PTR * put in in trace record data ptr  46960000
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace record data length 8  46970000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'642',DATA           46980000
*                                                                       46990000
         LR    R1,R5              * Put ptr of allocated block in R1    47000000
*                                                                       47010000
         DROP  R5                                                       47020000
*                                                                       47030000
         BR    R8                 * Return                              47040000
*                                                                       47050000
         DROP  R7                                                       47060000
*                                                                       47070000
         LTORG                                                          47080000
*                                                                       47090000
* Translate table for hex conversion                                    47100000
VAR_HEXTAB EQU   *-C'0'                                                 47110000
           DC    C'0123456789ABCDEF'                                    47120000
*                                                                       47130000
*********************************************************************** 47140000
* Section: LWZMAKE_STORE_TGT                                          * 47150000
* Purpose: Store a target in binary search tree. The binary search    * 47160000
*          tree is searched, but an existing entry won't be replaced, * 47170000
*          the tree can contain duplicate entries. At the correct     * 47180000
*          place a new allocated memory block is inserted and filled  * 47190000
*          with the target data.                                      * 47200000
*          R7 should point to a rule statement block                  * 47210000
*          R9 should point to global data.                            * 47220000
*          Token 1 contains a single space delimited target name      * 47230000
*********************************************************************** 47240000
LWZMAKE_STORE_TGT MLWZSAVE                                              47250000
*        Trace record to start section                                  47260000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX47270000
               KE_STORE_TGT'                                            47280000
*                                                                       47290000
         USING TARGET_DSECT,R6    * Use R6 for addressing variable      47300000
*                                                                       47310000
         LT    R6,G_FIRST_TGT_PTR * Get the start of the search tree    47320000
         IF (Z) THEN              * If this is the first target         47330000
            BAL   R8,ALLOC_TGT    * Perform allocate target routine     47340000
            ST    R1,G_FIRST_TGT_PTR * Store return ptr start of tree   47350000
            B     STORE_TGT_RET   * Skip rest of store target           47360000
         ELSE                     * Else, search tree not empty         47370000
TEST_TGTS   EQU   *               * Test this target for matching name  47380000
            L     R3,G_SCAN_TOKEN_LEN * Get new target name length      47390000
            CH    R3,TGTNAMELEN   * Compare it to current tgt name len  47400000
            IF (H) THEN           * Compare length of shortest of the 2 47410000
               LH    R3,TGTNAMELEN * If current tgt name len is less    47420000
            ENDIF                 * then use that                       47430000
            BCTR  R3,R0           * Subtract 1 for EX                   47440000
            L     R2,G_SCAN_TOKENA * Point R2 to new target name        47450000
            LA    R4,TGTNAME      * Point R4 to current target name     47460000
            B     *+10            * Skip CLC constant for EX            47470000
            CLC   0(1,R2),0(R4)   * CLC constant for EX                 47480000
            EX    R3,*-6          * EX previous CLC stmt with R3        47490000
            IF (L) THEN           * If new target name is lower         47500000
               CLC   TGTLOW,=A(0) * Check if current tgt's low ptr set  47510000
               IF (EQ) THEN       * If not                              47520000
                  BAL   R8,ALLOC_TGT * Perform allocate tgt routine     47530000
                  ST    R1,TGTLOW * Store returned poitner as low ptr   47540000
                  B     STORE_TGT_RET * Skip rest of store target       47550000
               ELSE               * Else, current tgt's low ptr is set  47560000
                  L     R6,TGTLOW * Replace current tgt by low tgt      47570000
                  B     TEST_TGTS * Loop around to test that one        47580000
               ENDIF                                                    47590000
            ELSE                  * Else, new target name is >=         47600000
               CLC   TGTHIGH,=A(0) * Check if curr tgt's high ptr set   47610000
               IF (EQ) THEN       * If not                              47620000
                  BAL   R8,ALLOC_TGT * Perform allocate tgt routine     47630000
                  ST    R1,TGTHIGH * Store returned pointer as high ptr 47640000
                  B     STORE_TGT_RET * Skip rest of store target       47650000
               ELSE               * Else, current tgt's high ptr is set 47660000
                  L     R6,TGTHIGH * Replace current tgt by high tgt    47670000
                  B     TEST_TGTS * Loop around to test that one        47680000
               ENDIF                                                    47690000
            ENDIF                                                       47700000
         ENDIF                                                          47710000
*                                                                       47720000
         DROP  R6                                                       47730000
*                                                                       47740000
STORE_TGT_RET EQU *                                                     47750000
         MLWZTERM                 * Return back to caller               47760000
*                                                                       47770000
* Allocate a target block                                               47780000
* R7 is left intact pointing to the rule statement where this target    47790000
* originated                                                            47800000
*                                                                       47810000
ALLOC_TGT EQU  *                                                        47820000
         L     R3,=A(TARGET_DSECT_LEN) * Get size of fixed part of tgt  47830000
         A     R3,G_SCAN_TOKEN_LEN  * Add target name length            47840000
         A     R3,=A(8)           * Add length for optional member name 47850000
*                                                                       47860000
         STORAGE OBTAIN,LENGTH=(R3) *Allocate memory for tgt block      47870000
*                                                                       47880000
         USING TARGET_DSECT,R1    * Addressing of new target block      47890000
*                                                                       47900000
*        Clear memory block                                             47910000
         LR    R2,R1              * Copy ptr to target block to R2      47920000
         XR    R4,R4              * Clear R4                            47930000
         XR    R5,R5              *   and R5                            47940000
         MVCL  R2,R4              * Zero out memory                     47950000
*                                                                       47960000
*        Fill in new target block                                       47970000
         L     R3,=A(TARGET_DSECT_LEN) * Get size of fixed part of tgt  47980000
         A     R3,G_SCAN_TOKEN_LEN  * Add target name length            47990000
         A     R3,=A(8)           * Add length for optional member name 48000000
         ST    R3,TGTLEN          * Store total block length            48010000
         L     R2,G_SCAN_TOKEN_LEN * Get length of target name          48020000
         STH   R2,TGTNAMELEN      * Store target name length in block   48030000
         ST    R7,TGTSTMT         * Store target stmt ptr in block      48040000
         LA    R2,TGTNAME         * Point R2 to target name in block    48050000
         L     R3,G_SCAN_TOKEN_LEN * Get target name length             48060000
         L     R4,G_SCAN_TOKENA   * Point R4 to target name in token 1  48070000
         LR    R5,R3              * Make sure no cropping/filling       48080000
         MVCL  R2,R4              * Copy target name to block           48090000
*                                                                       48100000
         DROP  R1                                                       48110000
*                                                                       48120000
         BR    R8                 * Return                              48130000
*                                                                       48140000
*********************************************************************** 48150000
* Section: LWZMAKE_STORE_PNY                                          * 48160000
* Purpose: Store a PHONY in the binary search tree. Duplicates are    * 48170000
*          simply ignored. Every new phony is allocated a new phony   * 48180000
*          memory block, which is added to the binary search tree.    * 48190000
*          R7 should point to a statement block for a PHONY           * 48200000
*          R9 should point to global data.                            * 48210000
*********************************************************************** 48220000
LWZMAKE_STORE_PNY MLWZSAVE                                              48230000
*        Trace record to start section                                  48240000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX48250000
               KE_STORE_PNY'                                            48260000
*                                                                       48270000
         USING STMT_P_DSECT,R7    * Use R7 for addressing PHONY stmt    48280000
         USING PHONY_DSECT,R6     * Use R6 for addressing PHONY         48290000
*                                                                       48300000
         LT    R6,G_FIRST_PNY_PTR * Get the start of the search tree    48310000
         IF (Z) THEN              * If this is the first PHONY          48320000
            BAL   R8,ALLOC_PNY    * Perform allocate PHONY routine      48330000
            LR    R6,R1           * Copy returned pointer to R6         48340000
            ST    R6,G_FIRST_PNY_PTR * And set it as start of tree      48350000
         ELSE                     * Else, search tree not empty         48360000
TEST_PNYS   EQU   *               * Test this PHONY for matching name   48370000
            XR    R3,R3           * Clear R3                            48380000
            LH    R3,STMT_P_PNYLEN * and get length of stmt PNONY name  48390000
            CH    R3,PNYNAMELEN   * Compare it to current PHONY len     48400000
            IF (H) THEN           * Compare length of shortest of the 2 48410000
               LH    R3,PNYNAMELEN * If current PHONY name len is less  48420000
            ENDIF                 * then use that                       48430000
            BCTR  R3,R0           * Subtract 1 for EX                   48440000
            LA    R2,STMT_P_PNY   * Point R2 to statement PHONY name    48450000
            LA    R4,PNYNAME      * Point R4 to current PHONY name      48460000
            B     *+10            * Skip CLC constant for EX            48470000
            CLC   0(1,R2),0(R4)   * CLC constant for EX                 48480000
            EX    R3,*-6          * EX previous CLC stmt with R3        48490000
            IF (L) THEN           * If statement PHONY name is lower    48500000
               CLC   PNYLOW,=A(0) * Check if current PHONY low ptr set  48510000
               IF (EQ) THEN       * If not                              48520000
                  BAL   R8,ALLOC_PNY * Perform allocate PHONY routine   48530000
                  ST    R1,PNYLOW * Store returned pointer as low ptr   48540000
                  LR    R6,R1     * And set it as the current PHONY ptr 48550000
                  B     FILL_PNY  * Skip to writing report line         48560000
               ELSE               * Else, current PHONY low ptr is set  48570000
                  L     R6,PNYLOW * Replace current PHONY by low PHONY  48580000
                  B     TEST_PNYS * Loop around to test that one        48590000
               ENDIF                                                    48600000
            ELSE                  * Else, statement PHONY name is >=    48610000
               IF (EQ) THEN       * If the compared part is equal       48620000
                  CLC   STMT_P_PNYLEN,PNYNAMELEN * Check if they're     48630000
                  IF (EQ) THEN    * also of equal length                48640000
                     B     STORE_PNY_RET * we're done                   48650000
                  ENDIF                                                 48660000
               ENDIF                                                    48670000
*              If we end up here, it's not a match                      48680000
               CLC   PNYHIGH,=A(0) * Check if curr PHONY high ptr set   48690000
               IF (EQ) THEN       * If not                              48700000
                  BAL   R8,ALLOC_PNY * Perform allocate PHONY routine   48710000
                  ST    R1,PNYHIGH * Store returned pointer as high ptr 48720000
                  LR    R6,R1     * And set it as the current PHONY ptr 48730000
                  B     FILL_PNY  * Skip to writing report line         48740000
               ELSE               * Else, current PHONY high ptr is set 48750000
                  L     R6,PNYHIGH * Replace current PHONY by high      48760000
                  B     TEST_PNYS * Loop around to test that one        48770000
               ENDIF                                                    48780000
            ENDIF                                                       48790000
         ENDIF                                                          48800000
*                                                                       48810000
* Fill a new PHONY name                                                 48820000
FILL_PNY EQU   *                                                        48830000
*                                                                       48840000
*        Write a report line with PHONY                                 48850000
         MVC   G_LWZMRPT_LINE,=CL133' Phony target ........'            48860000
         LA    R2,G_LWZMRPT_LINE+23                                     48870000
         LA    R3,110                                                   48880000
         LA    R4,PNYNAME                                               48890000
         LR    R5,R3                                                    48900000
         CH    R5,PNYNAMELEN                                            48910000
         IF (H) THEN                                                    48920000
            LH    R5,PNYNAMELEN                                         48930000
         ENDIF                                                          48940000
         BCTR  R5,R0                                                    48950000
         B     *+10                                                     48960000
         MVC   0(1,R2),0(R4)                                            48970000
         EX    R5,*-6                                                   48980000
         L     R15,G_LWZMAKE_RPTA                                       48990000
         BASR  R14,R15                                                  49000000
*                                                                       49010000
         DROP  R6                                                       49020000
*                                                                       49030000
STORE_PNY_RET EQU *                                                     49040000
         MLWZTERM                 * Return back to caller               49050000
*                                                                       49060000
* Allocate a PHONY block.                                               49070000
* The using for R7 in previous code is left intact to keep addressing   49080000
* of the PHONY statement possible.                                      49090000
*                                                                       49100000
ALLOC_PNY EQU  *                                                        49110000
         L     R3,=A(PHONY_DSECT_LEN) * Get size of fixed part of PHONY 49120000
         AH    R3,STMT_P_PNYLEN   * Add PHONY name length               49130000
*                                                                       49140000
         STORAGE OBTAIN,LENGTH=(R3) * Allocate memory for PHONY block   49150000
*                                                                       49160000
         USING PHONY_DSECT,R1     * Addressing of new PHONY block       49170000
*                                                                       49180000
*        Clear memory block                                             49190000
         LR    R2,R1              * Copy ptr to target block to R2      49200000
         XR    R4,R4              * Clear R4                            49210000
         XR    R5,R5              *   and R5                            49220000
         MVCL  R2,R4              * Zero out memory                     49230000
*                                                                       49240000
*        Fill in new target block                                       49250000
         L     R3,=A(PHONY_DSECT_LEN) * Get size of fixed part of PHONY 49260000
         AH    R3,STMT_P_PNYLEN   * Add PHONY name length               49270000
         ST    R3,PNYLEN          * Store total block length            49280000
         XR    R3,R3              * Clear R2                            49290000
         LH    R3,STMT_P_PNYLEN   * Get length of PHONY name            49300000
         STH   R3,PNYNAMELEN      * Store PHONY name length in block    49310000
         ST    R7,PNYSTMT         * Store PHONY stmt ptr in block       49320000
         LA    R2,PNYNAME         * Point R2 to PHONY name in block     49330000
         LA    R4,STMT_P_PNY      * Point R4 to PHONY name in stmt      49340000
         LR    R5,R3              * Make sure no cropping/filling       49350000
         MVCL  R2,R4              * Copy PHONY name to block            49360000
*                                                                       49370000
         DROP  R1                                                       49380000
*                                                                       49390000
         BR    R8                 * Return                              49400000
*                                                                       49410000
*********************************************************************** 49420000
* Section: LWZMAKE_FINDVAR                                            * 49430000
* Purpose: Find a variable in variable binary search tree             * 49440000
*          R9 should point to global data.                            * 49450000
*********************************************************************** 49460000
LWZMAKE_FINDVAR MLWZSAVE                                                49470000
*        Trace record to start section                                  49480000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX49490000
               KE_FINDVAR'                                              49500000
*                                                                       49510000
         MVC   G_FOUND_VAR_PTR,=A(0) * Initialize return pointer        49520000
*                                                                       49530000
         USING VAR_DSECT,R6       * Use R6 for addressing variable      49540000
*                                                                       49550000
         LT    R6,G_FIRST_VAR_PTR * Get start of search tree for vars   49560000
         IF (Z) THEN              * If no vars in tree                  49570000
            B     FINDVAR_RET     * Skip rest of find var               49580000
         ENDIF                                                          49590000
TEST_VARF   EQU   *                                                     49600000
         XR    R3,R3              * Clear R3                            49610000
         LH    R3,G_SRCH_VAR_LEN  * Get var to find's name length       49620000
         CH    R3,VARLEN          * Compare it to current var name len  49630000
         IF (H) THEN              * Compare length of shortest of the 2 49640000
            LH    R3,VARLEN       * If current var name len is less     49650000
         ENDIF                    * then use that                       49660000
         BCTR  R3,R0              * Subtract 1 for EX                   49670000
         LA    R2,G_SRCH_VAR      * Point R2 to var to find's name      49680000
         LA    R4,VARNAME         * Point R4 to current var name        49690000
         B     *+10               * Skip CLC constant for EX            49700000
         CLC   0(1,R2),0(R4)      * CLC constant for EX                 49710000
         EX    R3,*-6             * EX previous CLC stmt with R3        49720000
         IF (L) THEN              * If find var name is lower           49730000
            CLC   VARLOW,=A(0)    * Check if current var's low ptr set  49740000
            IF (EQ) THEN          * If not                              49750000
               B     FINDVAR_RET  * Var not found, skip rest of findvar 49760000
            ELSE                  * Else, current var's low ptr set     49770000
               L     R6,VARLOW    * Replace current var by low var      49780000
               B     TEST_VARF    * Loop around to test that one        49790000
            ENDIF                                                       49800000
         ELSE                     * Else, find var name is >=           49810000
            IF (EQ) THEN          * If the compared part is equal       49820000
               CLC   G_SRCH_VAR_LEN,VARLEN * Check if they're also of   49830000
               IF (EQ) THEN       * equal length                        49840000
                  ST    R6,G_FOUND_VAR_PTR * Return this var's ptr      49850000
                  B     FINDVAR_RET * Skip rest of find var             49860000
               ENDIF                                                    49870000
            ENDIF                                                       49880000
*           If we end up here, it's not a match                         49890000
            CLC   VARHIGH,=A(0)   * Check if curr var's high ptr set    49900000
            IF (EQ) THEN          * If not                              49910000
               B     FINDVAR_RET  * Var not found, skip rest of findvar 49920000
            ELSE                  * Else, current var's high ptr set    49930000
               L     R6,VARHIGH   * Replace current var by high var     49940000
               B     TEST_VARF    * Loop around to test that one        49950000
            ENDIF                                                       49960000
         ENDIF                                                          49970000
*                                                                       49980000
         DROP  R6                                                       49990000
*                                                                       50000000
FINDVAR_RET EQU *                                                       50010000
         MLWZTERM                 * Return back to caller               50020000
*                                                                       50030000
         LTORG                                                          50040000
*********************************************************************** 50050000
* Section: LWZMAKE_FINDTGT                                            * 50060000
* Purpose: Find a target in target binary search tree                 * 50070000
*          The name of the target to find is in token 1.              * 50080000
*          R9 should point to global data.                            * 50090000
*********************************************************************** 50100000
LWZMAKE_FINDTGT MLWZSAVE                                                50110000
*        Trace record to start section                                  50120000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX50130000
               KE_FINDTGT'                                              50140000
*                                                                       50150000
         MVC   G_FOUND_TGT_PTR,=A(0) * Initialize return pointer        50160000
*                                                                       50170000
         USING TARGET_DSECT,R6    * Use R6 for addressing target        50180000
*                                                                       50190000
         LT    R6,G_FIRST_TGT_PTR * Get start of search tree for tgts   50200000
         IF (Z) THEN              * If no tgts in tree                  50210000
            B     FINDTGT_RET     * Skip rest of find tgt               50220000
         ENDIF                                                          50230000
TEST_TGTF EQU  *                                                        50240000
         L     R3,G_SCAN_TOKEN_LEN * Get tgt to find's name length      50250000
         CH    R3,TGTNAMELEN      * Compare it to current tgt name len  50260000
         IF (H) THEN              * Compare length of shortest of the 2 50270000
            LH    R3,TGTNAMELEN   * If current tgt name len is less     50280000
         ENDIF                    * then use that                       50290000
         BCTR  R3,R0              * Subtract 1 for EX                   50300000
         L     R2,G_SCAN_TOKENA   * Point R2 to tgt to find's name      50310000
         LA    R4,TGTNAME         * Point R4 to current tgt name        50320000
         B     *+10               * Skip CLC constant for EX            50330000
         CLC   0(1,R2),0(R4)      * CLC constant for EX                 50340000
         EX    R3,*-6             * EX previous CLC stmt with R3        50350000
         IF (L) THEN              * If find tgt name is lower           50360000
            CLC   TGTLOW,=A(0)    * Check if current tgt's low ptr set  50370000
            IF (EQ) THEN          * If not                              50380000
               B     FINDTGT_RET  * Tgt not found, skip rest of findtgt 50390000
            ELSE                  * Else, current tgt's low ptr set     50400000
               L     R6,TGTLOW    * Replace current tgt by low tgt      50410000
               B     TEST_TGTF    * Loop around to test that one        50420000
            ENDIF                                                       50430000
         ELSE                     * Else, find tgt name is >=           50440000
            IF (EQ) THEN          * If the compared part is equal       50450000
               CLC   G_SCAN_TOKEN_LEN+2(2),TGTNAMELEN * Check if also   50460000
               IF (EQ) THEN       * of equal length                     50470000
                  ST    R6,G_FOUND_TGT_PTR * Return this tgt's ptr      50480000
                  B     FINDTGT_RET * Skip rest of find tgt             50490000
               ENDIF                                                    50500000
            ENDIF                                                       50510000
*           If we end up here, it's not a match                         50520000
            CLC   TGTHIGH,=A(0)   * Check if curr tgt's high ptr set    50530000
            IF (EQ) THEN          * If not                              50540000
               B     FINDTGT_RET  * Tgt not found, skip rest of findtgt 50550000
            ELSE                  * Else, current tgt's high ptr set    50560000
               L     R6,TGTHIGH   * Replace current tgt by high tgt     50570000
               B     TEST_TGTF    * Loop around to test that one        50580000
            ENDIF                                                       50590000
         ENDIF                                                          50600000
*                                                                       50610000
FINDTGT_RET EQU *                                                       50620000
*        Write result of find var to report                             50630000
         CLC   G_FOUND_TGT_PTR,=A(0) * Was a tgt found?                 50640000
         IF (EQ) THEN                * Nope                             50650000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... No targeX50660000
               t found'                                                 50670000
         ELSE                        * Tgt was found                    50680000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target fX50690000
               ound'                                                    50700000
         ENDIF                                                          50710000
         L     R15,G_LWZMAKE_RPTA * Get address of LWZMAKE_RPT section  50720000
         BASR  R14,R15            * Link to LWZMAKE_RPT section         50730000
*                                                                       50740000
         DROP  R6                                                       50750000
*                                                                       50760000
         MLWZTERM                 * Return back to caller               50770000
*                                                                       50780000
         LTORG                                                          50790000
*                                                                       50800000
*********************************************************************** 50810000
* Section: LWZMAKE_FINDPNY                                            * 50820000
* Purpose: Find a PHONY in PHONY binary search tree                   * 50830000
*          The name of the PHONY to find is in token 1.               * 50840000
*          R9 should point to global data.                            * 50850000
*********************************************************************** 50860000
LWZMAKE_FINDPNY MLWZSAVE                                                50870000
*        Trace record to start section                                  50880000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX50890000
               KE_FINDPNY'                                              50900000
*                                                                       50910000
         MVC   G_FOUND_PNY_PTR,=A(0) * Initialize return pointer        50920000
*                                                                       50930000
         USING PHONY_DSECT,R6     * Use R6 for addressing PHONY         50940000
*                                                                       50950000
         LT    R6,G_FIRST_PNY_PTR * Get start of search tree for PNYs   50960000
         IF (Z) THEN              * If no PNYs in tree                  50970000
            B     FINDPNY_RET     * Skip rest of find PHONY             50980000
         ENDIF                                                          50990000
TEST_PNYF EQU  *                                                        51000000
         L     R3,G_SCAN_TOKEN_LEN * Get PNY to find's name length      51010000
         CH    R3,PNYNAMELEN      * Compare it to current PNY name len  51020000
         IF (H) THEN              * Compare length of shortest of the 2 51030000
            LH    R3,PNYNAMELEN   * If current PNY name len is less     51040000
         ENDIF                    * then use that                       51050000
         BCTR  R3,R0              * Subtract 1 for EX                   51060000
         L     R2,G_SCAN_TOKENA   * Point R2 to PNY to find's name      51070000
         LA    R4,PNYNAME         * Point R4 to current PNY name        51080000
         B     *+10               * Skip CLC constant for EX            51090000
         CLC   0(1,R2),0(R4)      * CLC constant for EX                 51100000
         EX    R3,*-6             * EX previous CLC stmt with R3        51110000
         IF (L) THEN              * If find PNY name is lower           51120000
            CLC   PNYLOW,=A(0)    * Check if current PNY's low ptr set  51130000
            IF (EQ) THEN          * If not                              51140000
               B     FINDPNY_RET  * PNY not found, skip rest of findpny 51150000
            ELSE                  * Else, current PNY's low ptr set     51160000
               L     R6,PNYLOW    * Replace current PNY by low PNY      51170000
               B     TEST_PNYF    * Loop around to test that one        51180000
            ENDIF                                                       51190000
         ELSE                     * Else, find PNY name is >=           51200000
            IF (EQ) THEN          * If the compared part is equal       51210000
               CLC   G_SCAN_TOKEN_LEN+2(2),PNYNAMELEN * Check if also   51220000
               IF (EQ) THEN       * of equal length                     51230000
                  ST    R6,G_FOUND_PNY_PTR * Return this PNY's ptr      51240000
                  B     FINDPNY_RET * Skip rest of find PNY             51250000
               ENDIF                                                    51260000
            ENDIF                                                       51270000
*           If we end up here, it's not a match                         51280000
            CLC   PNYHIGH,=A(0)   * Check if curr PNY's high ptr set    51290000
            IF (EQ) THEN          * If not                              51300000
               B     FINDPNY_RET  * PNY not found, skip rest of findpny 51310000
            ELSE                  * Else, current PNY's high ptr set    51320000
               L     R6,PNYHIGH   * Replace current PNY by high PNY     51330000
               B     TEST_PNYF    * Loop around to test that one        51340000
            ENDIF                                                       51350000
         ENDIF                                                          51360000
*                                                                       51370000
FINDPNY_RET EQU *                                                       51380000
*        Write result of find var to report                             51390000
         CLC   G_FOUND_PNY_PTR,=A(0) * Was a PNY found?                 51400000
         IF (EQ) THEN                * Nope                             51410000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target iX51420000
               s a file'                                                51430000
         ELSE                        * Tgt was found                    51440000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target iX51450000
               s PHONY'                                                 51460000
         ENDIF                                                          51470000
         L     R15,G_LWZMAKE_RPTA * Get address of LWZMAKE_RPT section  51480000
         BASR  R14,R15            * Link to LWZMAKE_RPT section         51490000
*                                                                       51500000
         DROP  R6                                                       51510000
*                                                                       51520000
         MLWZTERM                 * Return back to caller               51530000
*                                                                       51540000
         LTORG                                                          51550000
*                                                                       51560000
*********************************************************************** 51570000
* Section: LWZMAKE_EXEC_TGT                                           * 51580000
* Purpose: Execute a target. This is a recursive section, in other    * 51590000
*          words, it calls itself.                                    * 51600000
*          First, the target last altered date is acquired. Then the  * 51610000
*          requisites are 'expanded', meaning any variables in that   * 51620000
*          string are resolved. Then, for each requisite this section * 51630000
*          is invoked. If, after all requisites have been processed,  * 51640000
*          any of the requisites had a laster altered date more       * 51650000
*          recent than the target's, that target should be built.     * 51660000
*          If so, the recipe following the rult statement for this    * 51670000
*          target is executed.                                        * 51680000
*          R9 should point to global data.                            * 51690000
*********************************************************************** 51700000
*                                                                       51710000
LWZMAKE_EXEC_TGT DS    0F                                               51720000
         STM   R14,R12,12(R13)   * Save callers registers               51730000
         LR    R10,R15           * Set R10 to entry point               51740000
         LA    R11,4095(,R10)    * Setup R11 as second using            51750000
         LA    R11,1(,R11)       *   base register                      51760000
         USING LWZMAKE_EXEC_TGT,R10,R11 * Establish addressing          51770000
         LR    R2,R1             * Save possible parameter ptr          51780000
         GETMAIN RU,LV=WORKAREA_EXEC_TGT_LEN                            51790000
         ST    R13,4(R1)         * Backward chain callers SA            51800000
         ST    R1,8(R13)         * Forward chain my SA                  51810000
         LR    R13,R1            * Point R13 to my SA                   51820000
         USING WORKAREA_EXEC_TGT,R13 * Establish addressing             51830000
         USING GLOBAL_DATA_DSECT,R9                                     51840000
         LR    R1,R2             * Restore parameter list ptr in R1     51850000
         XR    R15,R15                                                  51860000
         ST    R15,RETCODE_EXEC_TGT                                     51870000
*                                                                       51880000
*        Trace record to start section                                  51890000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX51900000
               KE_EXEC_TGT'                                             51910000
*                                                                       51920000
*        Reset scanning variables                                       51930000
         MVC   G_SCAN_CURRCOL,=F'0' * Not really used, but it shouldn't 51940000
*                                 * be whatever value (like 80) left    51950000
*                                 * over from scanning makefile         51960000
         MVI   G_MKFEOF,C'N'      * Reset EOF switch                    51970000
*                                                                       51980000
*        This section is invoked with a parameter of EXEC_TGT_PAR. It   51990000
*        holds a pointer to a TARGET_DSECT, which in turn points to a   52000000
*        STMT_R_DSECT.                                                  52010000
         L     R1,0(,R1)          * Get pointer to first parameter      52020000
         ST    R1,EXEC_TGT_PARA   * Save it                             52030000
         L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1) * Get TARGET_DSECT ptr 52040000
         USING TARGET_DSECT,R6    * Address it with R6                  52050000
         L     R7,TGTSTMT         * Get originating rule statement ptr  52060000
         USING STMT_R_DSECT,R7    * Address it with R7                  52070000
*                                                                       52080000
*                                                                       52090000
         MVC   G_LWZMRPT_LINE,=CL133' Processing target ...'            52100000
         LA    R2,G_LWZMRPT_LINE+23                                     52110000
         LA    R3,TGTNAME                                               52120000
         L     R4,=F'110'                                               52130000
         CH    R4,TGTNAMELEN                                            52140000
         IF (H) THEN                                                    52150000
            LH    R4,TGTNAMELEN                                         52160000
         ENDIF                                                          52170000
         BCTR  R4,R0                                                    52180000
         B     *+10                                                     52190000
         MVC   0(1,R2),0(R3)                                            52200000
         EX    R4,*-6                                                   52210000
         L     R15,G_LWZMAKE_RPTA                                       52220000
         BASR  R14,R15                                                  52230000
*                                                                       52240000
         L     R2,G_SCAN_TOKENA                                         52250000
         LA    R3,TGTNAME                                               52260000
         XR    R4,R4                                                    52270000
         LH    R4,TGTNAMELEN                                            52280000
         ST    R4,G_SCAN_TOKEN_LEN                                      52290000
         BCTR  R4,R0                                                    52300000
         B     *+10                                                     52310000
         MVC   0(1,R2),0(R3)                                            52320000
         EX    R4,*-6                                                   52330000
         L     R15,LWZMAKE_FINDPNYA_EXEC                                52340000
         BASR  R14,R15                                                  52350000
*                                                                       52360000
         CLC   G_FOUND_PNY_PTR,=A(0)                                    52370000
         IF (NE) THEN                                                   52380000
            XC    TARGET_ALTER_DATE,TARGET_ALTER_DATE                   52390000
            B     EXEC_TGT_PREREQ                                       52400000
         ENDIF                                                          52410000
*                                                                       52420000
         L     R2,G_SCAN_TOKENA                                         52430000
         LA    R3,TGTNAME                                               52440000
         XR    R4,R4                                                    52450000
         LH    R4,TGTNAMELEN                                            52460000
         ST    R4,G_SCAN_TOKEN_LEN                                      52470000
         BCTR  R4,R0                                                    52480000
         B     *+10                                                     52490000
         MVC   0(1,R2),0(R3)                                            52500000
         EX    R4,*-6                                                   52510000
         L     R15,LWZMAKE_GET_DATEA_EXEC                               52520000
         BASR  R14,R15                                                  52530000
*                                                                       52540000
         CLC   G_RETCODE,=F'0'                                          52550000
         BNE   EXEC_TGT_RET                                             52560000
*                                                                       52570000
         LT    R3,G_MVSDS_MEMBER_LEN                                    52580000
         IF (NZ) THEN                                                   52590000
            STH   R3,TGTNAMEMEMLEN                                      52600000
            LA    R2,TGTNAME                                            52610000
            AH    R2,TGTNAMELEN                                         52620000
            L     R4,G_MVSDS_MEMBER_PTR                                 52630000
            BCTR  R3,R0                                                 52640000
            B     *+10                                                  52650000
            MVC   0(1,R2),0(R4)                                         52660000
            EX    R3,*-6                                                52670000
         ENDIF                                                          52680000
*                                                                       52690000
         MVC   TARGET_ALTER_DATE,G_SAVE_ALTER_DATE                      52700000
         CLC   TARGET_ALTER_DATE,=16X'FF'                               52710000
         IF (EQ) THEN                                                   52720000
            MLWZMRPT RPTLINE=CL133' ..................... Target has noX52730000
                last altered date, build required'                      52740000
         ENDIF                                                          52750000
*                                                                       52760000
EXEC_TGT_PREREQ EQU *                                                   52770000
         CLC   STMT_R_REQLEN,=H'0'                                      52780000
         BE    EXEC_TGT_PREREQ_DONE                                     52790000
*                                                                       52800000
         XR    R2,R2                                                    52810000
         XR    R3,R3                                                    52820000
         IC    R3,G_SCAN_INPUT_STACK_IDX                                52830000
         C     R3,=F'20'                                                52840000
         IF (NL) THEN                                                   52850000
            MLWZMRPT RPTLINE=CL133'0Internal error, state stack overfloX52860000
               w'                                                       52870000
            MVC   G_RETCODE,=F'12'                                      52880000
            B     EXEC_TGT_RET                                          52890000
         ENDIF                                                          52900000
         LA    R3,1(,R3)                                                52910000
         STC   R3,G_SCAN_INPUT_STACK_IDX                                52920000
         BCTR  R3,R0                                                    52930000
         M     R2,=F'12'                                                52940000
         LA    R2,G_SCAN_INPUT_STACK                                    52950000
         AR    R2,R3                                                    52960000
         USING INPUT_DSECT,R2                                           52970000
         MVI   INPUTTYPE,X'01'                                          52980000
         MVC   INPUTLEN,STMT_R_REQLEN                                   52990000
         LA    R4,STMT_R_TGT                                            53000000
         AH    R4,STMT_R_TGTLEN                                         53010000
         ST    R4,INPUTPTR                                              53020000
         MVC   INPUTPOS,=H'0'                                           53030000
         DROP  R2                                                       53040000
*                                                                       53050000
         MVI   G_SCAN_STATE,SCAN_STATE_IN_EXPAND                        53060000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  53070000
*                                                                       53080000
EXEC_TGT_PREREQ_NEXT_TOKEN EQU *                                        53090000
         L     R15,LWZMAKE_SCAN_TOKENA_EXEC                             53100000
         BASR  R14,R15                                                  53110000
*                                                                       53120000
         CLC   G_RETCODE,=F'0'                                          53130000
         BNE   EXEC_TGT_RET                                             53140000
*                                                                       53150000
         CLI   G_MKFEOF,C'Y'                                            53160000
         BE    EXEC_TGT_PREREQ_EXPANDED                                 53170000
*                                                                       53180000
         IC    R14,G_SCAN_STATE                                         53190000
         N     R14,=X'0000007F'                                         53200000
         C     R14,=A(SCAN_STATE_IN_VARIABLE)                           53210000
         IF (EQ) THEN                                                   53220000
            MVI   G_SCAN_APPEND_TO,X'00'                                53230000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1'                       53240000
            L     R15,LWZMAKE_SCAN_VARA_EXEC                            53250000
            BASR  R14,R15                                               53260000
*                                                                       53270000
            CLC   G_RETCODE,=F'0'                                       53280000
            BNE   EXEC_TGT_RET                                          53290000
*                                                                       53300000
            B     EXEC_TGT_PREREQ_NEXT_TOKEN                            53310000
         ENDIF                                                          53320000
*                                                                       53330000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_PERCENT) THEN       53340000
            L     R2,G_SCAN_TOKENA                                      53350000
            LA    R4,TGTNAME                                            53360000
            AH    R4,TGTNAMELEN                                         53370000
            XR    R3,R3                                                 53380000
            LH    R3,TGTNAMEMEMLEN                                      53390000
            ST    R3,G_SCAN_TOKEN_LEN                                   53400000
            BCTR  R3,R0                                                 53410000
            B     *+10                                                  53420000
            MVC   0(1,R2),0(R4)                                         53430000
            EX    R3,*-6                                                53440000
         ENDIF                                                          53450000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_ACRO) THEN          53460000
            L     R2,G_SCAN_TOKENA                                      53470000
            LA    R4,TGTNAME                                            53480000
            XR    R3,R3                                                 53490000
            LH    R3,TGTNAMELEN                                         53500000
            ST    R3,G_SCAN_TOKEN_LEN                                   53510000
            BCTR  R3,R0                                                 53520000
            B     *+10                                                  53530000
            MVC   0(1,R2),0(R4)                                         53540000
            EX    R3,*-6                                                53550000
         ENDIF                                                          53560000
*                                                                       53570000
*        Append token 1 to token 2                                      53580000
         MVI   G_SCAN_APPEND_TO,X'01'                                   53590000
         L     R15,LWZMAKE_APPEND_TOKENA_EXEC * Get addr APPEND_TOKEN   53600000
         BASR  R14,R15            * Link to APPEND_TOKEN section        53610000
*                                                                       53620000
         B     EXEC_TGT_PREREQ_NEXT_TOKEN                               53630000
*                                                                       53640000
EXEC_TGT_PREREQ_EXPANDED EQU *                                          53650000
         MVC   G_LWZMRPT_LINE,=CL133' Prerequisites........'            53660000
         LA    R2,G_LWZMRPT_LINE+23                                     53670000
         L     R3,G_SCAN_TOKEN2_LEN                                     53680000
         L     R4,G_SCAN_TOKEN2A                                        53690000
         C     R3,=A(110)                                               53700000
         IF (H) THEN                                                    53710000
            L     R3,=A(110)                                            53720000
         ENDIF                                                          53730000
         BCTR  R3,R0                                                    53740000
         B     *+10                                                     53750000
         MVC   0(1,R2),0(R4)                                            53760000
         EX    R3,*-6                                                   53770000
         L     R15,G_LWZMAKE_RPTA                                       53780000
         BASR  R14,R15                                                  53790000
*                                                                       53800000
         MVC   EXEC_WORD_SPLIT_PTR,G_SCAN_TOKEN2A                       53810000
         MVC   EXEC_WORD_SPLIT_LEN,G_SCAN_TOKEN2_LEN                    53820000
EXEC_TGT_SCAN_NEXT_PREREQ EQU *                                         53830000
         L     R2,EXEC_WORD_SPLIT_PTR                                   53840000
         L     R3,EXEC_WORD_SPLIT_LEN                                   53850000
         L     R4,G_SCAN_TOKENA                                         53860000
         XR    R5,R5                                                    53870000
         ST    R5,G_SCAN_TOKEN_LEN                                      53880000
EXEC_TGT_PREREQ_BLANK EQU *                                             53890000
         IF (CLI,0(R2),EQ,C' ') THEN                                    53900000
            LA    R2,1(,R2)                                             53910000
            BCT   R3,EXEC_TGT_PREREQ_BLANK                              53920000
            B     EXEC_TGT_RET                                          53930000
         ENDIF                                                          53940000
EXEC_TGT_PREREQ_NONBLANK EQU *                                          53950000
         MVC   0(1,R4),0(R2)                                            53960000
         LA    R4,1(,R4)                                                53970000
         LA    R5,1(,R5)                                                53980000
         BCTR  R3,R0                                                    53990000
         C     R3,=F'0'                                                 54000000
         IF (H) THEN                                                    54010000
            LA    R2,1(,R2)                                             54020000
            CLI    0(R2),C' '     * Current pos a space?                54030000
            BNE   EXEC_TGT_PREREQ_NONBLANK                              54040000
         ENDIF                                                          54050000
         ST    R5,G_SCAN_TOKEN_LEN                                      54060000
         ST    R2,EXEC_WORD_SPLIT_PTR                                   54070000
         ST    R3,EXEC_WORD_SPLIT_LEN                                   54080000
*                                                                       54090000
         MVC   G_LWZMRPT_LINE,=CL133' Next prereq for tgt .'            54100000
         LA    R2,G_LWZMRPT_LINE+23                                     54110000
         LA    R3,TGTNAME                                               54120000
         L     R4,=F'110'                                               54130000
         CH    R4,TGTNAMELEN                                            54140000
         IF (H) THEN                                                    54150000
            LH    R4,TGTNAMELEN                                         54160000
         ENDIF                                                          54170000
         BCTR  R4,R0                                                    54180000
         B     *+10                                                     54190000
         MVC   0(1,R2),0(R3)                                            54200000
         EX    R4,*-6                                                   54210000
         L     R15,G_LWZMAKE_RPTA                                       54220000
         BASR  R14,R15                                                  54230000
*                                                                       54240000
         MVC   G_LWZMRPT_LINE,=CL133' Checking prereq .....'            54250000
         LA    R2,G_LWZMRPT_LINE+23                                     54260000
         L     R3,G_SCAN_TOKENA                                         54270000
         L     R4,=F'110'                                               54280000
         C     R4,G_SCAN_TOKEN_LEN                                      54290000
         IF (H) THEN                                                    54300000
            L     R4,G_SCAN_TOKEN_LEN                                   54310000
         ENDIF                                                          54320000
         BCTR  R4,R0                                                    54330000
         B     *+10                                                     54340000
         MVC   0(1,R2),0(R3)                                            54350000
         EX    R4,*-6                                                   54360000
         L     R15,G_LWZMAKE_RPTA                                       54370000
         BASR  R14,R15                                                  54380000
*                                                                       54390000
         L     R15,LWZMAKE_FINDTGTA_EXEC                                54400000
         BASR  R14,R15                                                  54410000
*                                                                       54420000
         CLC   G_RETCODE,=F'0'                                          54430000
         BNE   EXEC_TGT_RET                                             54440000
*                                                                       54450000
         CLC   G_FOUND_TGT_PTR,=A(0)                                    54460000
         IF (NE) THEN                                                   54470000
            MVC   EXEC_SAVE_SCAN_TOKENA,G_SCAN_TOKENA                   54480000
            MVC   EXEC_SAVE_SCAN_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN       54490000
            MVC   EXEC_SAVE_SCAN_TOKEN_LEN,G_SCAN_TOKEN_LEN             54500000
            L     R4,G_SCAN_TOKEN_MAXLEN                                54510000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        54520000
            ST    R1,G_SCAN_TOKENA                                      54530000
            MVC   G_SCAN_TOKEN_LEN,=A(0)                                54540000
*                                                                       54550000
            MVC   EXEC_SAVE_SCAN_TOKEN2A,G_SCAN_TOKEN2A                 54560000
            MVC   EXEC_SAVE_SCAN_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN     54570000
            MVC   EXEC_SAVE_SCAN_TOKEN2_LEN,G_SCAN_TOKEN2_LEN           54580000
            L     R4,G_SCAN_TOKEN2_MAXLEN                               54590000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        54600000
            ST    R1,G_SCAN_TOKEN2A                                     54610000
            MVC   G_SCAN_TOKEN2_LEN,=A(0)                               54620000
*                                                                       54630000
            LA    R1,EXEC_NEXTTGT_PAR                                   54640000
            MVC   EXEC_TGT_PTR-EXEC_TGT_PAR(4,R1),G_FOUND_TGT_PTR       54650000
            ST    R1,EXEC_NEXTTGT_PARA                                  54660000
            LA    R1,EXEC_NEXTTGT_PARA                                  54670000
            L     R15,LWZMAKE_EXEC_TGTA_EXEC                            54680000
            BASR  R14,R15                                               54690000
*                                                                       54700000
            CLC   G_RETCODE,=F'0'                                       54710000
            BNE   EXEC_TGT_RET                                          54720000
*                                                                       54730000
            L     R2,G_SCAN_TOKEN_MAXLEN                                54740000
            L     R3,G_SCAN_TOKENA                                      54750000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  54760000
            MVC   G_SCAN_TOKENA,EXEC_SAVE_SCAN_TOKENA                   54770000
            MVC   G_SCAN_TOKEN_MAXLEN,EXEC_SAVE_SCAN_TOKEN_MAXLEN       54780000
            MVC   G_SCAN_TOKEN_LEN,EXEC_SAVE_SCAN_TOKEN_LEN             54790000
*                                                                       54800000
            L     R2,G_SCAN_TOKEN2_MAXLEN                               54810000
            L     R3,G_SCAN_TOKEN2A                                     54820000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  54830000
            MVC   G_SCAN_TOKEN2A,EXEC_SAVE_SCAN_TOKEN2A                 54840000
            MVC   G_SCAN_TOKEN2_MAXLEN,EXEC_SAVE_SCAN_TOKEN2_MAXLEN     54850000
            MVC   G_SCAN_TOKEN2_LEN,EXEC_SAVE_SCAN_TOKEN2_LEN           54860000
*                                                                       54870000
            MVC   G_LWZMRPT_LINE,=CL133' Continuing target ...'         54880000
            LA    R2,G_LWZMRPT_LINE+23                                  54890000
            LA    R3,TGTNAME                                            54900000
            L     R4,=F'110'                                            54910000
            CH    R4,TGTNAMELEN                                         54920000
            IF (H) THEN                                                 54930000
               LH    R4,TGTNAMELEN                                      54940000
            ENDIF                                                       54950000
            BCTR  R4,R0                                                 54960000
            B     *+10                                                  54970000
            MVC   0(1,R2),0(R3)                                         54980000
            EX    R4,*-6                                                54990000
            L     R15,G_LWZMAKE_RPTA                                    55000000
            BASR  R14,R15                                               55010000
*                                                                       55020000
            MVC   G_LWZMRPT_LINE,=CL133' Continuing prereq ...'         55030000
            LA    R2,G_LWZMRPT_LINE+23                                  55040000
            L     R3,G_SCAN_TOKENA                                      55050000
            L     R4,=F'110'                                            55060000
            C     R4,G_SCAN_TOKEN_LEN                                   55070000
            IF (H) THEN                                                 55080000
               L     R4,G_SCAN_TOKEN_LEN                                55090000
            ENDIF                                                       55100000
            BCTR  R4,R0                                                 55110000
            B     *+10                                                  55120000
            MVC   0(1,R2),0(R3)                                         55130000
            EX    R4,*-6                                                55140000
            L     R15,G_LWZMAKE_RPTA                                    55150000
            BASR  R14,R15                                               55160000
         ENDIF                                                          55170000
*                                                                       55180000
         L     R15,LWZMAKE_FINDPNYA_EXEC                                55190000
         BASR  R14,R15                                                  55200000
*                                                                       55210000
         CLC   G_FOUND_PNY_PTR,=A(0)                                    55220000
         BNE   EXEC_TGT_PREREQ_CHECK_LOOP                               55230000
*                                                                       55240000
         L     R15,LWZMAKE_GET_DATEA_EXEC                               55250000
         BASR  R14,R15                                                  55260000
*                                                                       55270000
         CLC   G_RETCODE,=F'0'                                          55280000
         BNE   EXEC_TGT_RET                                             55290000
*                                                                       55300000
         IF (CLI,G_DSFOUND,NE,C'Y') THEN                                55310000
            MLWZMRPT RPTLINE=CL133' ..................... Prerequisite X55320000
               not found, build stopped'                                55330000
            MVC   G_RETCODE,=F'8'                                       55340000
            B     EXEC_TGT_RET                                          55350000
         ENDIF                                                          55360000
*                                                                       55370000
         CLC   TARGET_ALTER_DATE,=16X'00'                               55380000
         BE    EXEC_TGT_PREREQ_CHECK_LOOP                               55390000
*                                                                       55400000
         CLC   TARGET_ALTER_DATE,=16X'FF'                               55410000
         IF (NE) THEN                                                   55420000
            CLC   TARGET_ALTER_DATE,G_SAVE_ALTER_DATE                   55430000
            IF (LT) THEN                                                55440000
               MLWZMRPT RPTLINE=CL133' ..................... Target is X55450000
               older than prereq, build required'                       55460000
               MVC   TARGET_ALTER_DATE,=16X'FF'                         55470000
            ENDIF                                                       55480000
         ENDIF                                                          55490000
*                                                                       55500000
EXEC_TGT_PREREQ_CHECK_LOOP EQU *                                        55510000
         L     R3,EXEC_WORD_SPLIT_LEN                                   55520000
         C     R3,=F'0'                                                 55530000
         BH    EXEC_TGT_SCAN_NEXT_PREREQ                                55540000
*                                                                       55550000
EXEC_TGT_PREREQ_DONE EQU *                                              55560000
         CLC   TARGET_ALTER_DATE,=16X'FF'                               55570000
         IF (NE) THEN                                                   55580000
            CLC   TARGET_ALTER_DATE,=16X'00'                            55590000
         ENDIF                                                          55600000
         IF (EQ) THEN                                                   55610000
            BAL   R8,EXEC_TGT_BUILD                                     55620000
         ELSE                                                           55630000
            MLWZMRPT RPTLINE=CL133' ..................... No need to buX55640000
               ild target'                                              55650000
            B     EXEC_TGT_RET                                          55660000
         ENDIF                                                          55670000
*                                                                       55680000
         DROP  R6                                                       55690000
         DROP  R7                                                       55700000
*                                                                       55710000
EXEC_TGT_RET EQU *                                                      55720000
         L     R2,RETCODE_EXEC_TGT * Save return value                  55730000
         L     R3,4(,R13)        * Restore address of callers SA        55740000
         FREEMAIN RU,LV=WORKAREA_EXEC_TGT_LEN,A=(R13)                   55750000
         LR    R15,R2            * Restore return value                 55760000
         LR    R13,R3            * Address of callers SA                55770000
         L     R14,12(R13)       * Restore callers R14                  55780000
         LM    R0,R12,20(R13)    * Restore callers registers 0-12       55790000
         BR    R14               * Return                               55800000
*                                                                       55810000
* Execute the recipe                                                    55820000
*                                                                       55830000
EXEC_TGT_BUILD EQU *                                                    55840000
         L     R1,EXEC_TGT_PARA                                         55850000
         L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)                        55860000
         USING TARGET_DSECT,R6                                          55870000
         L     R7,TGTSTMT                                               55880000
         USING STMT_DSECT,R7                                            55890000
*                                                                       55900000
         MVC   G_LWZMRPT_LINE,=CL133' Building target .....'            55910000
         LA    R2,G_LWZMRPT_LINE+23                                     55920000
         LA    R3,TGTNAME                                               55930000
         L     R4,=F'110'                                               55940000
         CH    R4,TGTNAMELEN                                            55950000
         IF (H) THEN                                                    55960000
            LH    R4,TGTNAMELEN                                         55970000
         ENDIF                                                          55980000
         BCTR  R4,R0                                                    55990000
         B     *+10                                                     56000000
         MVC   0(1,R2),0(R3)                                            56010000
         EX    R4,*-6                                                   56020000
         L     R15,G_LWZMAKE_RPTA                                       56030000
         BASR  R14,R15                                                  56040000
*                                                                       56050000
         DROP  R6                                                       56060000
*                                                                       56070000
         LT    R7,STMT_NEXT_PTR                                         56080000
         BZ    EXEC_TGT_BUILD_NO_RECIPE                                 56090000
*                                                                       56100000
         CLI   STMT_IN_RECIPE,C'Y'                                      56110000
         BNE   EXEC_TGT_BUILD_NO_RECIPE                                 56120000
*                                                                       56130000
NEXT_RECIPE_STMT EQU *                                                  56140000
         IF (CLI,STMT_TYPE,EQ,STMT_TYPE_CALL) THEN                      56150000
            DROP  R7                                                    56160000
            USING STMT_C_DSECT,R7                                       56170000
*                                                                       56180000
            MVC   EXEC_SAVE_SCAN_TOKENA,G_SCAN_TOKENA                   56190000
            MVC   EXEC_SAVE_SCAN_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN       56200000
            MVC   EXEC_SAVE_SCAN_TOKEN_LEN,G_SCAN_TOKEN_LEN             56210000
            L     R4,G_SCAN_TOKEN_MAXLEN                                56220000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        56230000
            ST    R1,G_SCAN_TOKENA                                      56240000
            MVC   G_SCAN_TOKEN_LEN,=A(0)                                56250000
*                                                                       56260000
            MVC   EXEC_SAVE_SCAN_TOKEN2A,G_SCAN_TOKEN2A                 56270000
            MVC   EXEC_SAVE_SCAN_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN     56280000
            MVC   EXEC_SAVE_SCAN_TOKEN2_LEN,G_SCAN_TOKEN2_LEN           56290000
            L     R4,G_SCAN_TOKEN2_MAXLEN                               56300000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        56310000
            ST    R1,G_SCAN_TOKEN2A                                     56320000
            MVC   G_SCAN_TOKEN2_LEN,=A(0)                               56330000
*                                                                       56340000
            XR    R2,R2                                                 56350000
            XR    R3,R3                                                 56360000
            IC    R3,G_SCAN_INPUT_STACK_IDX                             56370000
            C     R3,=F'20'                                             56380000
            IF (NL) THEN                                                56390000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX56400000
               flow'                                                    56410000
               MVC   G_RETCODE,=F'12'                                   56420000
               B     EXEC_CALL_END                                      56430000
            ENDIF                                                       56440000
            LA    R3,1(,R3)                                             56450000
            STC   R3,G_SCAN_INPUT_STACK_IDX                             56460000
            BCTR  R3,R0                                                 56470000
            M     R2,=F'12'                                             56480000
            LA    R2,G_SCAN_INPUT_STACK                                 56490000
            AR    R2,R3                                                 56500000
            USING INPUT_DSECT,R2                                        56510000
            MVI   INPUTTYPE,X'01'                                       56520000
            MVC   INPUTLEN,STMT_C_PARMLEN                               56530000
            LA    R6,STMT_C_EXEC                                        56540000
            AH    R6,STMT_C_EXECLEN                                     56550000
            ST    R6,INPUTPTR                                           56560000
            MVC   INPUTPOS,=H'0'                                        56570000
            MVI   G_MKFEOF,C'N'                                         56580000
*                                                                       56590000
            DROP  R2                                                    56600000
*                                                                       56610000
            MVI   G_SCAN_STATE,SCAN_STATE_IN_EXPAND                     56620000
            MVC   G_SCAN_TOKEN2_LEN,=F'0'                               56630000
*                                                                       56640000
EXEC_TGT_CALL_NEXT_TOKEN EQU *                                          56650000
            L     R15,LWZMAKE_SCAN_TOKENA_EXEC                          56660000
            BASR  R14,R15                                               56670000
*                                                                       56680000
            CLC   G_RETCODE,=F'0'                                       56690000
            BNE   EXEC_CALL_END                                         56700000
*                                                                       56710000
            CLI   G_MKFEOF,C'Y'                                         56720000
            BE    EXEC_TGT_CALL_EXPANDED                                56730000
*                                                                       56740000
            IC    R14,G_SCAN_STATE                                      56750000
            N     R14,=X'0000007F'                                      56760000
            C     R14,=A(SCAN_STATE_IN_VARIABLE)                        56770000
            IF (EQ) THEN                                                56780000
               MVI   G_SCAN_APPEND_TO,X'00'                             56790000
               MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1'                    56800000
               L     R15,LWZMAKE_SCAN_VARA_EXEC                         56810000
               BASR  R14,R15                                            56820000
*                                                                       56830000
               CLC   G_RETCODE,=F'0'                                    56840000
               BNE   EXEC_CALL_END                                      56850000
*                                                                       56860000
               B     EXEC_TGT_CALL_NEXT_TOKEN                           56870000
            ENDIF                                                       56880000
*                                                                       56890000
            L     R1,G_SCAN_TOKEN_LEN                                   56900000
            C     R1,=F'2'                                              56910000
            IF (EQ) THEN                                                56920000
               L     R2,G_SCAN_TOKENA                                   56930000
               CLC   0(2,R2),=C'$@'                                     56940000
               IF (EQ) THEN                                             56950000
                  L     R1,EXEC_TGT_PARA                                56960000
                  L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)               56970000
                  LA    R3,TGTNAME-TARGET_DSECT(,R6)                    56980000
                  XR    R4,R4                                           56990000
                  LH    R4,TGTNAMELEN-TARGET_DSECT(,R6)                 57000000
                  ST    R4,G_SCAN_TOKEN_LEN                             57010000
                  BCTR  R4,R0                                           57020000
                  B     *+10                                            57030000
                  MVC   0(1,R2),0(R3)                                   57040000
                  EX    R4,*-6                                          57050000
               ENDIF                                                    57060000
               CLC   0(2,R2),=C'$%'                                     57070000
               IF (EQ) THEN                                             57080000
                  L     R1,EXEC_TGT_PARA                                57090000
                  L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)               57100000
                  LA    R3,TGTNAME-TARGET_DSECT(,R6)                    57110000
                  XR    R4,R4                                           57120000
                  LH    R4,TGTNAMELEN-TARGET_DSECT(,R6)                 57130000
                  AR    R3,R4                                           57140000
                  LH    R4,TGTNAMEMEMLEN-TARGET_DSECT(,R6)              57150000
                  ST    R4,G_SCAN_TOKEN_LEN                             57160000
                  BCTR  R4,R0                                           57170000
                  B     *+10                                            57180000
                  MVC   0(1,R2),0(R3)                                   57190000
                  EX    R4,*-6                                          57200000
               ENDIF                                                    57210000
            ENDIF                                                       57220000
*                                                                       57230000
            MVI   G_SCAN_APPEND_TO,X'01'                                57240000
            L     R15,LWZMAKE_APPEND_TOKENA_EXEC                        57250000
            BASR  R14,R15         * Link to APPEND_TOKEN section        57260000
*                                                                       57270000
            B     EXEC_TGT_CALL_NEXT_TOKEN                              57280000
*                                                                       57290000
EXEC_TGT_CALL_EXPANDED EQU *                                            57300000
*                                                                       57310000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Calling X57320000
               REXX'                                                    57330000
            LA    R2,G_LWZMRPT_LINE+36                                  57340000
            LA    R3,STMT_C_EXEC                                        57350000
            XR    R4,R4                                                 57360000
            LH    R4,STMT_C_EXECLEN                                     57370000
            BCTR  R4,R0                                                 57380000
            B     *+10                                                  57390000
            MVC   0(1,R2),0(R3)                                         57400000
            EX    R4,*-6                                                57410000
            LT    R4,G_SCAN_TOKEN2_LEN                                  57420000
            IF (NZ) THEN                                                57430000
               AH    R2,STMT_C_EXECLEN                                  57440000
               LA    R2,1(,R2)                                          57450000
               LA    R3,G_LWZMRPT_LINE+133                              57460000
               SR    R3,R2                                              57470000
               CR    R4,R3                                              57480000
               IF (H) THEN                                              57490000
                  LR    R4,R3                                           57500000
               ENDIF                                                    57510000
               BCTR  R4,R0                                              57520000
               L     R3,G_SCAN_TOKEN2A                                  57530000
               B     *+10                                               57540000
               MVC   0(1,R2),0(R3)                                      57550000
               EX    R4,*-6                                             57560000
            ENDIF                                                       57570000
            L     R15,G_LWZMAKE_RPTA                                    57580000
            BASR  R14,R15                                               57590000
*                                                                       57600000
            IF (CLI,G_USE_ISPEXEC,EQ,C' ') THEN                         57610000
               MVI   G_USE_ISPEXEC,C'N'                                 57620000
*                                                                       57630000
               MVC   G_IRXINIT_FUNCTION,=CL8'FINDENVB'                  57640000
               MVC   G_IRXINIT_PARMMOD,=CL8' '                          57650000
               MVC   G_IRXINIT_INSTORPARM_PTR,=A(0)                     57660000
               MVC   G_IRXINIT_USRFIELD_PTR,=X'80000000'                57670000
               MVC   G_IRXINIT_RESERVED_PTR,=A(0)                       57680000
               MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)                       57690000
               MVC   G_IRXINIT_REASON,=A(0)                             57700000
*                                                                       57710000
               XR    R0,R0                                              57720000
               LA    R1,G_IRXINIT_FUNCTION                              57730000
               ST    R1,G_IRXINIT_PAR7A                                 57740000
               LA    R1,G_IRXINIT_PARMMOD                               57750000
               ST    R1,G_IRXINIT_PAR7A+4                               57760000
               LA    R1,G_IRXINIT_INSTORPARM_PTR                        57770000
               ST    R1,G_IRXINIT_PAR7A+8                               57780000
               LA    R1,G_IRXINIT_USRFIELD_PTR                          57790000
               ST    R1,G_IRXINIT_PAR7A+12                              57800000
               LA    R1,G_IRXINIT_RESERVED_PTR                          57810000
               ST    R1,G_IRXINIT_PAR7A+16                              57820000
               LA    R1,G_IRXINIT_ENVBLOCK_PTR                          57830000
               ST    R1,G_IRXINIT_PAR7A+20                              57840000
               LA    R1,G_IRXINIT_REASON                                57850000
               O     R1,=X'80000000'                                    57860000
               ST    R1,G_IRXINIT_PAR7A+24                              57870000
               LA    R1,G_IRXINIT_PAR7A                                 57880000
*                                                                       57890000
               LINK  EP=IRXINIT,SF=(E,G_LINKD)                          57900000
*                                                                       57910000
               C     R15,=A(0)                                          57920000
               BE    IRXINIT_OK                                         57930000
               C     R15,=A(4)                                          57940000
               BE    IRXINIT_OK                                         57950000
               C     R15,=A(28)                                         57960000
               BE    IRXINIT_OK                                         57970000
               MLWZMRPT RPTLINE=CL133'0Error finding REXX environment'  57980000
               MVC   G_RETCODE,=F'12'                                   57990000
               BR    R8                                                 58000000
IRXINIT_OK     EQU   *                                                  58010000
*                                                                       58020000
               C     R15,=A(28)                                         58030000
               IF (NE) THEN                                             58040000
                  MVC   G_IRXSUBCM_FUNCTION,=CL8'QUERY'                 58050000
                  MVC   G_IRXSUBCM_STRING,=CL32' '                      58060000
                  MVC   G_IRXSUBCM_STRING(8),=CL8'ISPEXEC'              58070000
                  MVC   G_IRXSUBCM_STRING_LEN,=A(32)                    58080000
                  MVC   G_IRXSUBCM_HOSTENV_NAME,=CL8' '                 58090000
                  MVC   G_IRXSUBCM_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR  58100000
*                                                                       58110000
                  XR    R0,R0                                           58120000
                  LA    R1,G_IRXSUBCM_FUNCTION                          58130000
                  ST    R1,G_IRXSUBCM_PAR5A                             58140000
                  LA    R1,G_IRXSUBCM_STRING                            58150000
                  ST    R1,G_IRXSUBCM_PAR5A+4                           58160000
                  LA    R1,G_IRXSUBCM_STRING_LEN                        58170000
                  ST    R1,G_IRXSUBCM_PAR5A+8                           58180000
                  LA    R1,G_IRXSUBCM_HOSTENV_NAME                      58190000
                  ST    R1,G_IRXSUBCM_PAR5A+12                          58200000
                  LA    R1,G_IRXSUBCM_ENVBLOCK_PTR                      58210000
                  O     R1,=X'80000000'                                 58220000
                  ST    R1,G_IRXSUBCM_PAR5A+16                          58230000
                  LA    R1,G_IRXSUBCM_PAR5A                             58240000
*                                                                       58250000
                  LINK  EP=IRXSUBCM,SF=(E,G_LINKD)                      58260000
*                                                                       58270000
                  LTR   R15,R15                                         58280000
                  IF (Z) THEN                                           58290000
                     MVI   G_USE_ISPEXEC,C'Y'                           58300000
                  ELSE                                                  58310000
                     C     R15,=A(8)                                    58320000
                     IF (NE) THEN                                       58330000
                        MLWZMRPT RPTLINE=CL133'0Error checking ISPEXEC X58340000
               availability'                                            58350000
                        MVC   G_RETCODE,=F'12'                          58360000
                        BR    R8                                        58370000
                     ENDIF                                              58380000
                  ENDIF                                                 58390000
               ENDIF                                                    58400000
            ENDIF                                                       58410000
*                                                                       58420000
            IF (CLI,G_USE_ISPEXEC,EQ,C'Y') THEN                         58430000
               L     R6,G_SCAN_TOKENA                                   58440000
               MVC   0(12,R6),=C'SELECT CMD(%'                          58450000
               LA    R5,12                                              58460000
               LA    R6,12(,R6)                                         58470000
               LA    R3,STMT_C_EXEC                                     58480000
               XR    R4,R4                                              58490000
               LH    R4,STMT_C_EXECLEN                                  58500000
               CH    R4,=H'8'                                           58510000
               IF (H) THEN                                              58520000
                  LH    R4,=H'8'                                        58530000
               ENDIF                                                    58540000
               BCTR  R4,R0                                              58550000
               B     *+10                                               58560000
               MVC   0(1,R6),0(R3)                                      58570000
               EX    R4,*-6                                             58580000
               LA    R4,1(,R4)                                          58590000
               AR    R6,R4                                              58600000
               AR    R5,R4                                              58610000
               CLC   G_SCAN_TOKEN2_LEN,=F'0'                            58620000
               IF (NE) THEN                                             58630000
                  MVI   0(R6),C' '                                      58640000
                  LA    R6,1(,R6)                                       58650000
                  LA    R5,1(,R5)                                       58660000
                  LR    R0,R6                                           58670000
                  L     R2,G_SCAN_TOKEN2A                               58680000
                  L     R1,G_SCAN_TOKEN2_LEN                            58690000
                  LR    R3,R1                                           58700000
                  MVCL  R0,R2                                           58710000
                  A     R5,G_SCAN_TOKEN2_LEN                            58720000
                  A     R6,G_SCAN_TOKEN2_LEN                            58730000
               ENDIF                                                    58740000
               MVI   0(R6),C')'                                         58750000
               LA    R5,1(,R5)                                          58760000
               ST    R5,G_SCAN_TOKEN_LEN                                58770000
               LA    R1,G_SCAN_TOKEN_LEN                                58780000
               ST    R1,G_ISPEXEC_PAR2A                                 58790000
               L     R1,G_SCAN_TOKENA                                   58800000
               O     R1,=X'80000000'                                    58810000
               ST    R1,G_ISPEXEC_PAR2A+4                               58820000
               LA    R1,G_ISPEXEC_PAR2A                                 58830000
*                                                                       58840000
               LINK  EP=ISPEXEC,SF=(E,G_LINKD)                          58850000
*                                                                       58860000
               LTR   R15,R15                                            58870000
               IF (NZ) THEN                                             58880000
                  MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'    58890000
                  MVC   G_RETCODE,=F'12'                                58900000
                  BR    R8                                              58910000
               ENDIF                                                    58920000
*                                                                       58930000
               B     EXEC_CALL_END                                      58940000
            ENDIF                                                       58950000
*                                                                       58960000
            LA    R6,G_IRXEXEC_EXECBLK                                  58970000
            USING EXECBLK,R6                                            58980000
            MVC   EXEC_BLK_ACRYN,=CL8'IRXEXECB'                         58990000
            LA    R5,EXECBLEN                                           59000000
            ST    R5,EXEC_BLK_LENGTH                                    59010000
            MVC   EXEC_MEMBER,=CL8' '                                   59020000
            LA    R2,EXEC_MEMBER                                        59030000
            LA    R3,STMT_C_EXEC                                        59040000
            XR    R4,R4                                                 59050000
            LH    R4,STMT_C_EXECLEN                                     59060000
            CH    R4,=H'8'                                              59070000
            IF (H) THEN                                                 59080000
               LH    R4,=H'8'                                           59090000
            ENDIF                                                       59100000
            BCTR  R4,R0                                                 59110000
            B     *+10                                                  59120000
            MVC   0(1,R2),0(R3)                                         59130000
            EX    R4,*-6                                                59140000
            MVC   EXEC_DDNAME,=CL8' '                                   59150000
            MVC   EXEC_SUBCOM,=CL8' '                                   59160000
            XR    R5,R5                                                 59170000
            ST    R5,EXEC_BLK_LENGTH+4                                  59180000
            ST    R5,EXEC_DSNPTR                                        59190000
            ST    R5,EXEC_DSNLEN                                        59200000
            DROP  R6                                                    59210000
*                                                                       59220000
            LA    R6,G_IRXEXEC_EVALBLK                                  59230000
            USING EVALBLOCK,R6                                          59240000
            XR    R5,R5                                                 59250000
            ST    R5,EVALBLOCK_EVPAD1                                   59260000
            ST    R5,EVALBLOCK_EVPAD2                                   59270000
            LA    R5,EVALBLK_SIZ                                        59280000
            SRA   R5,3                                                  59290000
            ST    R5,EVALBLOCK_EVSIZE                                   59300000
            DROP  R6                                                    59310000
*                                                                       59320000
            LA    R1,G_IRXEXEC_EXECBLK                                  59330000
            ST    R1,G_IRXEXEC_EXECBLK_PTR                              59340000
            CLC   G_SCAN_TOKEN2_LEN,=F'0'                               59350000
            IF (NE) THEN                                                59360000
               MVC   G_IRXEXEC_ARGS(4),G_SCAN_TOKEN2A                   59370000
               MVC   G_IRXEXEC_ARGS+4(4),G_SCAN_TOKEN2_LEN              59380000
               MVC   G_IRXEXEC_ARGS+8(8),=X'FFFFFFFFFFFFFFFF'           59390000
            ELSE                                                        59400000
               MVC   G_IRXEXEC_ARGS,=X'FFFFFFFFFFFFFFFF'                59410000
            ENDIF                                                       59420000
            LA    R1,G_IRXEXEC_ARGS                                     59430000
            ST    R1,G_IRXEXEC_ARGS_PTR                                 59440000
            MVC   G_IRXEXEC_FLAGS,=X'40000000'                          59450000
            MVC   G_IRXEXEC_INSTBLK_PTR,=A(0)                           59460000
            MVC   G_IRXEXEC_CPPL_PTR,=A(0)                              59470000
            LA    R1,G_IRXEXEC_EVALBLK                                  59480000
            ST    R1,G_IRXEXEC_EVALBLK_PTR                              59490000
            MVC   G_IRXEXEC_WORKAREA_PTR,=A(0)                          59500000
            MVC   G_IRXEXEC_USRFIELD_PTR,=X'8000000'                    59510000
            MVC   G_IRXEXEC_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR         59520000
            LA    R1,G_IRXEXEC_REASON                                   59530000
            ST    R1,G_IRXEXEC_REASON_PTR                               59540000
            XR    R0,R0                                                 59550000
            LA    R1,G_IRXEXEC_EXECBLK_PTR                              59560000
            ST    R1,G_IRXEXEC_PAR10A                                   59570000
            LA    R1,G_IRXEXEC_ARGS_PTR                                 59580000
            ST    R1,G_IRXEXEC_PAR10A+4                                 59590000
            LA    R1,G_IRXEXEC_FLAGS                                    59600000
            ST    R1,G_IRXEXEC_PAR10A+8                                 59610000
            LA    R1,G_IRXEXEC_INSTBLK_PTR                              59620000
            ST    R1,G_IRXEXEC_PAR10A+12                                59630000
            LA    R1,G_IRXEXEC_CPPL_PTR                                 59640000
            ST    R1,G_IRXEXEC_PAR10A+16                                59650000
            LA    R1,G_IRXEXEC_EVALBLK_PTR                              59660000
            ST    R1,G_IRXEXEC_PAR10A+20                                59670000
            LA    R1,G_IRXEXEC_WORKAREA_PTR                             59680000
            ST    R1,G_IRXEXEC_PAR10A+24                                59690000
            LA    R1,G_IRXEXEC_USRFIELD_PTR                             59700000
            ST    R1,G_IRXEXEC_PAR10A+28                                59710000
            LA    R1,G_IRXEXEC_ENVBLOCK_PTR                             59720000
            ST    R1,G_IRXEXEC_PAR10A+32                                59730000
            LA    R1,G_IRXEXEC_REASON_PTR                               59740000
            O     R1,=X'80000000'                                       59750000
            ST    R1,G_IRXEXEC_PAR10A+36                                59760000
            LA    R1,G_IRXEXEC_PAR10A                                   59770000
*                                                                       59780000
            LINK  EP=IRXEXEC,SF=(E,G_LINKD)                             59790000
*                                                                       59800000
            LTR   R15,R15                                               59810000
            IF (NZ) THEN                                                59820000
               MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'       59830000
               MVC   G_RETCODE,=F'12'                                   59840000
               BR    R8                                                 59850000
            ENDIF                                                       59860000
*                                                                       59870000
            MVC   G_IRXINIT_ENVBLOCK_PTR,G_IRXEXEC_ENVBLOCK_PTR         59880000
*                                                                       59890000
            LA    R5,G_IRXEXEC_EVALBLK                                  59900000
            USING EVALBLOCK,R5                                          59910000
*                                                                       59920000
            CLC   EVALBLOCK_EVLEN,=F'1'                                 59930000
            BNE   EXEC_REXX_ERROR                                       59940000
            CLI   EVALBLOCK_EVDATA,C'0'                                 59950000
            BE    EXEC_REXX_NO_ERROR                                    59960000
EXEC_REXX_ERROR EQU *                                                   59970000
            MVC   G_LWZMRPT_LINE,=CL133'0REXX exec returned'            59980000
            LA    R2,G_LWZMRPT_LINE+20                                  59990000
            LA    R3,EVALBLOCK_EVDATA                                   60000000
            L     R4,EVALBLOCK_EVLEN                                    60010000
            C     R4,=F'113'                                            60020000
            IF (H) THEN                                                 60030000
               L     R4,=F'113'                                         60040000
            ENDIF                                                       60050000
            BCTR  R4,R0                                                 60060000
            B     *+10                                                  60070000
            MVC   0(1,R2),0(R3)                                         60080000
            EX    R4,*-6                                                60090000
            L     R15,G_LWZMAKE_RPTA                                    60100000
            BASR  R14,R15                                               60110000
            MVC   G_RETCODE,=F'8'                                       60120000
            BR    R8                                                    60130000
EXEC_REXX_NO_ERROR EQU *                                                60140000
*                                                                       60150000
            DROP  R5                                                    60160000
*                                                                       60170000
EXEC_CALL_END EQU *                                                     60180000
            L     R2,G_SCAN_TOKEN_MAXLEN                                60190000
            L     R3,G_SCAN_TOKENA                                      60200000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  60210000
            MVC   G_SCAN_TOKENA,EXEC_SAVE_SCAN_TOKENA                   60220000
            MVC   G_SCAN_TOKEN_MAXLEN,EXEC_SAVE_SCAN_TOKEN_MAXLEN       60230000
            MVC   G_SCAN_TOKEN_LEN,EXEC_SAVE_SCAN_TOKEN_LEN             60240000
*                                                                       60250000
            L     R2,G_SCAN_TOKEN2_MAXLEN                               60260000
            L     R3,G_SCAN_TOKEN2A                                     60270000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  60280000
            MVC   G_SCAN_TOKEN2A,EXEC_SAVE_SCAN_TOKEN2A                 60290000
            MVC   G_SCAN_TOKEN2_MAXLEN,EXEC_SAVE_SCAN_TOKEN2_MAXLEN     60300000
            MVC   G_SCAN_TOKEN2_LEN,EXEC_SAVE_SCAN_TOKEN2_LEN           60310000
         ENDIF                                                          60320000
*                                                                       60330000
         DROP  R7                                                       60340000
         USING STMT_DSECT,R7                                            60350000
*                                                                       60360000
         LT    R7,STMT_NEXT_PTR                                         60370000
         BZ    EXEC_TGT_BUILD_RET                                       60380000
*                                                                       60390000
         CLI   STMT_IN_RECIPE,C'Y'                                      60400000
         BE    NEXT_RECIPE_STMT                                         60410000
*                                                                       60420000
         DROP  R7                                                       60430000
*                                                                       60440000
EXEC_TGT_BUILD_RET EQU *                                                60450000
         BR    R8                                                       60460000
*                                                                       60470000
EXEC_TGT_BUILD_NO_RECIPE EQU *                                          60480000
         MLWZMRPT RPTLINE=CL133' ..................... No recipe'       60490000
         BR    R8                                                       60500000
*                                                                       60510000
         LTORG                                                          60520000
*                                                                       60530000
LWZMAKE_SCAN_TOKENA_EXEC    DC    A(LWZMAKE_SCAN_TOKEN)                 60540000
LWZMAKE_SCAN_VARA_EXEC      DC    A(LWZMAKE_SCAN_VAR)                   60550000
LWZMAKE_FINDPNYA_EXEC       DC    A(LWZMAKE_FINDPNY)                    60560000
LWZMAKE_FINDTGTA_EXEC       DC    A(LWZMAKE_FINDTGT)                    60570000
LWZMAKE_EXEC_TGTA_EXEC      DC    A(LWZMAKE_EXEC_TGT)                   60580000
LWZMAKE_GET_DATEA_EXEC      DC    A(LWZMAKE_GET_DATE)                   60590000
LWZMAKE_APPEND_TOKENA_EXEC  DC    A(LWZMAKE_APPEND_TOKEN)               60600000
*                                                                       60610000
WORKAREA_EXEC_TGT           DSECT                                       60620000
EXEC_TGT_SA                 DS    18F                                   60630000
RETCODE_EXEC_TGT            DS    F                                     60640000
EXEC_TGT_PARA               DS    A                                     60650000
EXEC_NEXTTGT_PARA           DS    A                                     60660000
EXEC_NEXTTGT_PAR            DS    CL(EXEC_TGT_PAR_LEN)                  60670000
*                                                                       60680000
                            DS    0F                                    60690000
TARGET_ALTER_DATE           DS    CL16                                  60700000
*                                                                       60710000
                            DS    0F                                    60720000
EXEC_WORD_SPLIT_PTR         DS    A                                     60730000
EXEC_WORD_SPLIT_LEN         DS    F                                     60740000
*                                                                       60750000
                            DS    0F                                    60760000
EXEC_IRXEXECB               DS    CL(EXECBLK_V2_LEN)                    60770000
*                                                                       60780000
EXEC_WTOBLOCK               DS    0F                                    60790000
EXEC_WTOLEN                 DS    H                                     60800000
EXEC_WTOFIL                 DS    H                                     60810000
EXEC_WTOTEXT                DS    CL133                                 60820000
*                                                                       60830000
                            DS    0F                                    60840000
EXEC_SAVE_SCAN_TOKEN_LEN    DS    F                                     60850000
EXEC_SAVE_SCAN_TOKEN2_LEN   DS    F                                     60860000
EXEC_SAVE_SCAN_TOKEN_MAXLEN DS    F                                     60870000
EXEC_SAVE_SCAN_TOKEN2_MAXLEN DS    F                                    60880000
EXEC_SAVE_SCAN_TOKENA       DS    A                                     60890000
EXEC_SAVE_SCAN_TOKEN2A      DS    A                                     60900000
WORKAREA_EXEC_TGT_LEN       EQU *-WORKAREA_EXEC_TGT                     60910000
*                                                                       60920000
EXEC_TGT_PAR                DSECT                                       60930000
EXEC_TGT_PTR                DS    A                                     60940000
EXEC_TGT_PAR_LEN            EQU   *-EXEC_TGT_PAR                        60950000
*                                                                       60960000
LWZMAKE  CSECT                                                          60970000
*                                                                       60980000
* Get the date of a file                                                60990000
*                                                                       61000000
         DROP                                                           61010000
*                                                                       61020000
LWZMAKE_GET_DATE DS    0F                                               61030000
         STM   R14,R12,12(R13)   * Save callers registers               61040000
         LR    R10,R15                                                  61050000
         LA    R11,4095(,R10)                                           61060000
         LA    R11,1(,R11)                                              61070000
         USING LWZMAKE_GET_DATE,R10,R11                                 61080000
         GETMAIN RU,LV=GET_DATE_DSECT_SIZ                               61090000
         ST    R13,4(R1)         * Backward chain callers SA            61100000
         ST    R1,8(R13)         * Forward chain my SA                  61110000
         LR    R13,R1            * Point R13 to my SA                   61120000
         USING GET_DATE_DSECT,R13 * Establish addressing of workarea    61130000
         USING GLOBAL_DATA_DSECT,R9                                     61140000
*                                                                       61150000
*        Trace record to start section                                  61160000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX61170000
               KE_GET_DATE'                                             61180000
*                                                                       61190000
         MVC   G_SAVE_ALTER_DATE,=CL16' '                               61200000
*                                                                       61210000
         L     R3,G_SCAN_TOKENA                                         61220000
         L     R4,G_SCAN_TOKEN_LEN                                      61230000
         XR    R6,R6                                                    61240000
         XR    R7,R7                                                    61250000
*                                                                       61260000
GET_DATE_TEST_QUAL1 EQU *                                               61270000
         LTR   R4,R4                                                    61280000
         BZ    GET_DATE_NOT_MVSDS                                       61290000
         TRT   0(1,R3),TRT_ALPHANAT                                     61300000
         BNZ   GET_DATE_NOT_MVSDS                                       61310000
         LA    R3,1(,R3)                                                61320000
         BCT   R4,*+8                                                   61330000
         B     GET_DATE_MVSDS                                           61340000
         LR    R5,R4                                                    61350000
         C     R5,=F'7'                                                 61360000
         IF (H) THEN                                                    61370000
            L     R5,=F'7'                                              61380000
         ENDIF                                                          61390000
         BCTR  R5,R0                                                    61400000
         B     *+10                                                     61410000
         TRT   0(1,R3),TRT_ALPHANUMNATDASH                              61420000
         EX    R5,*-6                                                   61430000
         IF (Z) THEN                                                    61440000
            LA    R5,1(,R5)                                             61450000
         ELSE                                                           61460000
            LR    R5,R1                                                 61470000
            SR    R5,R3                                                 61480000
         ENDIF                                                          61490000
         AR    R3,R5                                                    61500000
         SR    R4,R5                                                    61510000
         BZ    GET_DATE_MVSDS                                           61520000
         IF (CLI,0(R3),EQ,C'.') THEN                                    61530000
            LA    R3,1(,R3)                                             61540000
            BCTR  R4,R0                                                 61550000
            B     GET_DATE_TEST_QUAL1                                   61560000
         ENDIF                                                          61570000
         CLI   0(R3),C'('                                               61580000
         BNE   GET_DATE_NOT_MVSDS                                       61590000
         LR    R6,R3                                                    61600000
         LA    R3,1(,R3)                                                61610000
         BCT   R4,*+8                                                   61620000
         B     GET_DATE_NOT_MVSDS                                       61630000
         TRT   0(1,R3),TRT_ALPHANAT                                     61640000
         BNZ   GET_DATE_NOT_MVSDS                                       61650000
         LR    R6,R3                                                    61660000
         LA    R3,1(,R3)                                                61670000
         BCT   R4,*+8                                                   61680000
         B     GET_DATE_NOT_MVSDS                                       61690000
         LR    R5,R4                                                    61700000
         C     R5,=F'7'                                                 61710000
         IF (H) THEN                                                    61720000
            L     R5,=F'7'                                              61730000
         ENDIF                                                          61740000
         BCTR  R5,R0                                                    61750000
         B     *+10                                                     61760000
         TRT   0(1,R3),TRT_ALPHANUMNATDASH                              61770000
         EX    R5,*-6                                                   61780000
         IF (Z) THEN                                                    61790000
            LA    R5,1(,R5)                                             61800000
         ELSE                                                           61810000
            LR    R5,R1                                                 61820000
            SR    R5,R3                                                 61830000
         ENDIF                                                          61840000
         AR    R3,R5                                                    61850000
         SR    R4,R5                                                    61860000
         BZ    GET_DATE_NOT_MVSDS                                       61870000
         CLI   0(R3),C')'                                               61880000
         BNE   GET_DATE_NOT_MVSDS                                       61890000
         LR    R7,R3                                                    61900000
         SR    R7,R6                                                    61910000
         LA    R3,1(,R3)                                                61920000
         BCT   R4,GET_DATE_NOT_MVSDS                                    61930000
*                                                                       61940000
GET_DATE_MVSDS EQU *                                                    61950000
         ST    R6,G_MVSDS_MEMBER_PTR                                    61960000
         ST    R7,G_MVSDS_MEMBER_LEN                                    61970000
*                                                                       61980000
         MLWZMRPT RPTLINE=CL133' ..................... Name is MVS dataX61990000
                set name'                                               62000000
*                                                                       62010000
         BAL   R8,GET_DATE_IGGCSI00                                     62020000
*                                                                       62030000
         CLC   G_RETCODE,=A(0)                                          62040000
         BNE   GET_DATE_RET                                             62050000
*                                                                       62060000
         CLI   G_DSFOUND,C'Y'                                           62070000
         BNE   GET_DATE_RET                                             62080000
*                                                                       62090000
         CLC   G_MVSDS_MEMBER_PTR,=A(0)                                 62100000
         BE    GET_DATE_RET                                             62110000
*                                                                       62120000
         BAL   R8,GET_DATE_OBTAIN                                       62130000
*                                                                       62140000
         CLC   G_RETCODE,=A(0)                                          62150000
         BNE   GET_DATE_RET                                             62160000
*                                                                       62170000
         IF (TM,OBTAIN_GD+(DS1RECFM-OBTAIN_DSECT),DS1RECFU,O) THEN      62180000
            BAL   R8,GET_DATE_LOADMOD                                   62190000
         ELSE                                                           62200000
            BAL   R8,GET_DATE_STATS                                     62210000
         ENDIF                                                          62220000
*                                                                       62230000
         B     GET_DATE_RET                                             62240000
*                                                                       62250000
GET_DATE_NOT_MVSDS EQU *                                                62260000
         MLWZMRPT RPTLINE=CL133' ..................... Name is not MVS X62270000
               data set name'                                           62280000
         MVC   G_SAVE_ALTER_DATE,=16X'FF'                               62290000
*                                                                       62300000
GET_DATE_RET EQU *                                                      62310000
         L     R3,4(,R13)        * Restore address of callers SA        62320000
         FREEMAIN RU,LV=GET_DATE_DSECT_SIZ,A=(R13)                      62330000
         LR    R13,R3                                                   62340000
         LM    R14,R12,12(R13)                                          62350000
         BR    R14                    Return to caller                  62360000
*                                                                       62370000
* Perform catalog search with IGGCSI00                                  62380000
*                                                                       62390000
GET_DATE_IGGCSI00 EQU *                                                 62400000
         MVI   G_DSFOUND,C'N'                                           62410000
*                                                                       62420000
         LA    R1,DAREA_GD                                              62430000
         ST    R1,DAREAPTR_GD                                           62440000
         L     R2,=A(DAREA_GD_SIZ)                                      62450000
         ST    R2,0(,R1)                                                62460000
*                                                                       62470000
         LA    R2,CSIFIELD_GD                                           62480000
         L     R3,=A(CSIFIELD_GD_LEN)                                   62490000
         LA    R4,CONST_CSIFIELD_GD                                     62500000
         L     R5,=A(CONST_CSIFIELD_GD_LEN)                             62510000
         MVCL  R2,R4                                                    62520000
*                                                                       62530000
         LA    R2,CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)                 62540000
         L     R3,G_SCAN_TOKENA                                         62550000
         L     R4,G_SCAN_TOKEN_LEN                                      62560000
         LT    R5,G_MVSDS_MEMBER_PTR                                    62570000
         IF (NZ) THEN                                                   62580000
            SR    R5,R3                                                 62590000
            BCTR  R5,R0                                                 62600000
            LR    R4,R5                                                 62610000
         ENDIF                                                          62620000
         C     R4,=A(L'CSIFILTK)                                        62630000
         IF (H)                                                         62640000
            L     R4,=A(L'CSIFILTK)                                     62650000
         ENDIF                                                          62660000
         BCTR  R4,R0                                                    62670000
         B     *+10                                                     62680000
         MVC   0(1,R2),0(R3)                                            62690000
         EX    R4,*-6                                                   62700000
*                                                                       62710000
         LA    R1,PARMLIST_GD                                           62720000
         LA    R2,MODRSNRT_GD                                           62730000
         ST    R2,0(R1)                                                 62740000
         LA    R2,CSIFIELD_GD                                           62750000
         ST    R2,4(R1)                                                 62760000
         L     R2,DAREAPTR_GD                                           62770000
         O     R2,=X'80000000'                                          62780000
         ST    R2,8(R1)                                                 62790000
*                                                                       62800000
         L     R15,G_IGGCSI00A                                          62810000
         BASR  R14,R15                                                  62820000
*                                                                       62830000
         C     R15,=F'4'                                                62840000
         IF (H) THEN                                                    62850000
            MLWZMRPT RPTLINE=CL133'0Catalog search interface returned eX62860000
               rror code'                                               62870000
            MVC   G_RETCODE,=F'12'                                      62880000
            BR    R8                                                    62890000
         ENDIF                                                          62900000
*                                                                       62910000
         LA    R1,DAREA_GD                                              62920000
         CLC   8(4,R1),=F'64'                                           62930000
         IF (H) THEN                                                    62940000
            MVI   G_DSFOUND,C'Y'                                        62950000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Found inX62960000
                catalog'                                                62970000
         ELSE                                                           62980000
            MVC   G_SAVE_ALTER_DATE,=16X'FF'                            62990000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Not founX63000000
               d in catalog'                                            63010000
         ENDIF                                                          63020000
         L     R15,G_LWZMAKE_RPTA                                       63030000
         BASR  R14,R15                                                  63040000
*                                                                       63050000
GET_DATE_IGGCSI00_RET EQU *                                             63060000
         BR    R8                                                       63070000
*                                                                       63080000
* Perform CAMLST OBTAIN                                                 63090000
*                                                                       63100000
GET_DATE_OBTAIN EQU *                                                   63110000
         XR    R1,R1                                                    63120000
         ICM   R1,B'1000',=AL1(193)                                     63130000
         ST    R1,DSCBPAR_GD                                            63140000
         MVC   OBTAIN_GD+(DS1DSNAM-OBTAIN_DSECT)(L'DS1DSNAM),CSIFIELD_GX63150000
               D+(CSIFILTK-CSIFIELD_DSECT)                              63160000
         LA    R1,OBTAIN_GD+(DS1DSNAM-OBTAIN_DSECT)                     63170000
         ST    R1,DSCBPAR_GD+4                                          63180000
         LA    R1,DAREA_GD+110                                          63190000
         CLC   0(2,R1),=H'12'   * Is volume name present                63200000
         BL    GET_DATE_OBTAIN_RET                                      63210000
         LA    R1,6(,R1)                                                63220000
         ST    R1,DSCBPAR_GD+8                                          63230000
         LA    R1,OBTAIN_GD+(DS1FMTID-OBTAIN_DSECT)                     63240000
         ST    R1,DSCBPAR_GD+12                                         63250000
*                                                                       63260000
         OBTAIN DSCBPAR_GD                                              63270000
*                                                                       63280000
         LTR   R15,R15                                                  63290000
         IF (NZ) THEN                                                   63300000
            MLWZMRPT RPTLINE=CL133'0CAMLST OBTAIN returned error code'  63310000
            MVC   G_RETCODE,=F'12'                                      63320000
            BR    R8                                                    63330000
         ENDIF                                                          63340000
*                                                                       63350000
         IF (TM,OBTAIN_GD+(DS1DSORG-OBTAIN_DSECT),DS1DSGPO,Z) THEN      63360000
            MVC   G_LWZMRPT_LINE,=CL133'0Member specified on non-PDS daX63370000
               taset'                                                   63380000
            MVC   G_LWZMRPT_LINE+37(L'CSIFILTK),CSIFIELD_GD+(CSIFILTK-CX63390000
               SIFIELD_DSECT)                                           63400000
            L     R15,G_LWZMAKE_RPTA                                    63410000
            BASR  R14,R15                                               63420000
            MVC   G_RETCODE,=F'8'                                       63430000
            BR    R8                                                    63440000
         ENDIF                                                          63450000
*                                                                       63460000
GET_DATE_OBTAIN_RET EQU *                                               63470000
         BR    R8                                                       63480000
*                                                                       63490000
* Get the date from a load module                                       63500000
*                                                                       63510000
GET_DATE_LOADMOD EQU *                                                  63520000
         MLWZMRPT RPTLINE=CL133' ..................... Retrieving load X63530000
               module creation date/time'                               63540000
*                                                                       63550000
         MVI   G_DSFOUND,C'N'                                           63560000
*                                                                       63570000
         MVC   MEM8_GD,=CL8' '                                          63580000
         LA    R2,MEM8_GD                                               63590000
         L     R3,G_MVSDS_MEMBER_PTR                                    63600000
         L     R4,G_MVSDS_MEMBER_LEN                                    63610000
         BCTR  R4,R0                                                    63620000
         B     *+10                                                     63630000
         MVC   0(1,R2),0(R3)                                            63640000
         EX    R4,*-6                                                   63650000
*                                                                       63660000
         LA    R6,DYNALLOC_AREA_GD                                      63670000
         USING S99RBP,R6                                                63680000
         LA    R4,S99RBPTR+4                                            63690000
         USING S99RB,R4                                                 63700000
         ST    R4,S99RBPTR                                              63710000
         OI    S99RBPTR,S99RBPND                                        63720000
         XC    S99RB(S99RBEND-S99RB),S99RB                              63730000
         MVI   S99RBLN,S99RBEND-S99RB                                   63740000
         MVI   S99VERB,S99VRBAL                                         63750000
         OI    S99FLG11,S99MSGL0                                        63760000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             63770000
         MVC   0(CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_GD_L,R5),CDSNTU_GD  63780000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           63790000
         LA    R3,S99RB+(S99RBEND-S99RB)                                63800000
         ST    R3,S99TXTPP                                              63810000
         ST    R5,0(,R3)                                                63820000
         LA    R5,CDSNTU_GD_L(,R5)                                      63830000
         ST    R5,4(,R3)                                                63840000
         LA    R5,CSTATUSTU_GD_L(,R5)                                   63850000
         O     R5,=X'80000000'                                          63860000
         ST    R5,8(,R3)                                                63870000
         LA    R1,DYNALLOC_AREA_GD                                      63880000
         DYNALLOC                                                       63890000
*                                                                       63900000
         DROP  R6                                                       63910000
         DROP  R4                                                       63920000
*                                                                       63930000
         LTR   R15,R15                                                  63940000
         IF (NZ) THEN                                                   63950000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'         63960000
            MVC   G_RETCODE,=F'8'                                       63970000
            BR    R8                                                    63980000
         ENDIF                                                          63990000
*                                                                       64000000
         L     R1,G_DCB_MEM_PTR                                         64010000
         LA    R2,DCBPDS_BDR-DCB_DSECT(,R1)                             64020000
         MVC   0(LEN_DCBPDS_BDR,R2),CDCBPDS_BDR                         64030000
         L     R6,DYNALLOC_AREA_GD                                      64040000
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_GD_L+CSTATUSTU_GD_L(,R6)   64050000
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)                              64060000
*                                                                       64070000
         OPEN  ((R2)),MODE=31,MF=(E,G_OPEND)                            64080000
*                                                                       64090000
         LTR   R15,R15                                                  64100000
         IF (NZ) THEN                                                   64110000
            MLWZMRPT RPTLINE=CL133'0OPEN failed for accessing PDS with X64120000
               load modules'                                            64130000
            MVC   G_RETCODE,=F'8'                                       64140000
            B     GET_DATE_LOADMOD_DEALLOC                              64150000
         ENDIF                                                          64160000
*                                                                       64170000
         MVC   IEWBFDAT_SB_SB(2),=C'SB'                                 64180000
         MVC   IEWBFDAT_SB_SB+2(2),=X'0001'                             64190000
         XC    IEWBFDAT_SB_MTOKEN,IEWBFDAT_SB_MTOKEN                    64200000
         MVC   IEWBFDAT_SB_PGMNAME,MEM8_GD                              64210000
*                                                                       64220000
         LA    R1,IEWBFDAT_SB_SB                                        64230000
         ST    R1,IEWBFDAT_SB_PAR4A                                     64240000
         LA    R1,IEWBFDAT_SB_MTOKEN                                    64250000
         ST    R1,IEWBFDAT_SB_PAR4A+4                                   64260000
         L     R14,G_DCB_MEM_PTR                                        64270000
         LA    R1,DCBPDS_BDR-DCB_DSECT(,R14)                            64280000
         ST    R1,IEWBFDAT_SB_PAR4A+8                                   64290000
         LA    R1,IEWBFDAT_SB_PGMNAME                                   64300000
         O     R1,=X'80000000'                                          64310000
         ST    R1,IEWBFDAT_SB_PAR4A+12                                  64320000
         LA    R1,IEWBFDAT_SB_PAR4A                                     64330000
*                                                                       64340000
         L     R15,G_IEWBFDATA                                          64350000
         BASR  R14,R15                                                  64360000
*                                                                       64370000
         C     R15,=A(0)                                                64380000
         BE    GET_DATE_LOADMOD_NOERR1                                  64390000
         C     R15,=A(4)                                                64400000
         BE    GET_DATE_LOADMOD_NOERR1                                  64410000
         C     R15,=A(12)                                               64420000
         IF (EQ) THEN                                                   64430000
            C     R0,=X'10800032'                                       64440000
            BE    GET_DATE_LOADMOD_NOTFOUND                             64450000
         ENDIF                                                          64460000
         CVD   R15,G_DEC8         * convert return value to packed      64470000
         UNPK  G_ZONED8,G_DEC8    * convert return value to zoned       64480000
         OI    G_ZONED8+7,X'F0'   * get rid of sign                     64490000
         MVC   G_HELPER_DATA(8),G_ZONED8                                64500000
         MVI   G_HELPER_DATA+8,C' '                                     64510000
         ST    R0,G_DEC8          * Put ptr in area of at least 5 bytes 64520000
         UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex           64530000
         TR    G_ZONED8,GETDATE_HEXTAB * Turn into hex                  64540000
         MVC   G_HELPER_DATA+9(8),G_ZONED8                              64550000
         LA    R14,G_HELPER_DATA                                        64560000
         ST    R14,G_LWZMTRC_DATA_PTR                                   64570000
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)                              64580000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA           64590000
         MLWZMRPT RPTLINE=CL133'0Error starting binder fast data accessX64600000
                session'                                                64610000
         MVC   G_RETCODE,=F'12'                                         64620000
         BR    R8                                                       64630000
*                                                                       64640000
GET_DATE_LOADMOD_NOERR1 EQU *                                           64650000
         MVI   G_DSFOUND,C'Y'                                           64660000
*                                                                       64670000
IEWBIDB_BASE EQU R6                      Base register for IDRB buffer. 64680000
IDB_BASE     EQU R7                      Base register for IDRB entry.  64690000
         IEWBUFF FUNC=GETBUF,TYPE=IDRB   Get memory for IDRB buffer.    64700000
         IEWBUFF FUNC=INITBUF,TYPE=IDRB  Init IDRB buffer.              64710000
*                                                                       64720000
         MVC   IEWBFDAT_GD_GD(2),=C'GD'                                 64730000
         MVC   IEWBFDAT_GD_GD+2(2),=X'0001'                             64740000
         MVC   IEWBFDAT_GD_MTOKEN,IEWBFDAT_SB_MTOKEN                    64750000
         MVC   IEWBFDAT_GD_B_IDRB(2),=H'6'                              64760000
         MVC   IEWBFDAT_GD_B_IDRB+2(6),=C'B_IDRB'                       64770000
         XC    IEWBFDAT_GD_CURSOR,IEWBFDAT_GD_CURSOR                    64780000
*                                                                       64790000
         LA    R1,IEWBFDAT_GD_GD                                        64800000
         ST    R1,IEWBFDAT_GD_PAR8A                                     64810000
         LA    R1,IEWBFDAT_GD_MTOKEN                                    64820000
         ST    R1,IEWBFDAT_GD_PAR8A+4                                   64830000
         LA    R1,IEWBFDAT_GD_B_IDRB                                    64840000
         ST    R1,IEWBFDAT_GD_PAR8A+8                                   64850000
         XR    R1,R1                                                    64860000
         ST    R1,IEWBFDAT_GD_PAR8A+12                                  64870000
         ST    IEWBIDB_BASE,IEWBFDAT_GD_PAR8A+16                        64880000
         LA    R1,IEWBFDAT_GD_CURSOR                                    64890000
         ST    R1,IEWBFDAT_GD_PAR8A+20                                  64900000
         LA    R1,IEWBFDAT_GD_COUNT                                     64910000
         ST    R1,IEWBFDAT_GD_PAR8A+24                                  64920000
         L     R1,=X'80000000'                                          64930000
         ST    R1,IEWBFDAT_GD_PAR8A+28                                  64940000
         LA    R1,IEWBFDAT_GD_PAR8A                                     64950000
*                                                                       64960000
         L     R15,G_IEWBFDATA                                          64970000
         BASR  R14,R15                                                  64980000
*                                                                       64990000
         C     R15,=A(0)                                                65000000
         IF (NE) THEN                                                   65010000
            C     R15,=A(4)                                             65020000
         ENDIF                                                          65030000
         IF (NE) THEN                                                   65040000
            CVD   R15,G_DEC8      * convert return value to packed      65050000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       65060000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    65070000
            MVC   G_HELPER_DATA(8),G_ZONED8                             65080000
            MVI   G_HELPER_DATA+8,C' '                                  65090000
            ST    R0,G_DEC8       * Put ptr in area of at least 5 bytes 65100000
            UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex        65110000
            TR    G_ZONED8,GETDATE_HEXTAB * Turn into hex               65120000
            MVC   G_HELPER_DATA+9(8),G_ZONED8                           65130000
            LA    R14,G_HELPER_DATA                                     65140000
            ST    R14,G_LWZMTRC_DATA_PTR                                65150000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)                           65160000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA        65170000
            MLWZMRPT RPTLINE=CL133'0Error during binder fast data accesX65180000
               s get data function'                                     65190000
            MVC   G_RETCODE,=F'12'                                      65200000
            BR    R8                                                    65210000
         ENDIF                                                          65220000
*                                                                       65230000
         MVC   IEWBFDAT_EN_EN(2),=C'EN'                                 65240000
         MVC   IEWBFDAT_EN_EN+2(2),=X'0001'                             65250000
         MVC   IEWBFDAT_EN_MTOKEN,IEWBFDAT_SB_MTOKEN                    65260000
*                                                                       65270000
         LA    R1,IEWBFDAT_EN_EN                                        65280000
         ST    R1,IEWBFDAT_EN_PAR2A                                     65290000
         LA    R1,IEWBFDAT_EN_MTOKEN                                    65300000
         ST    R1,IEWBFDAT_EN_PAR2A+4                                   65310000
         LA    R1,IEWBFDAT_EN_PAR2A                                     65320000
*                                                                       65330000
         L     R15,G_IEWBFDATA                                          65340000
         BASR  R14,R15                                                  65350000
*                                                                       65360000
         LTR   R15,R15                                                  65370000
         IF (NZ) THEN                                                   65380000
            CVD   R15,G_DEC8      * convert return value to packed      65390000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       65400000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    65410000
            MVC   G_HELPER_DATA(8),G_ZONED8                             65420000
            MVI   G_HELPER_DATA+8,C' '                                  65430000
            ST    R0,G_DEC8       * Put ptr in area of at least 5 bytes 65440000
            UNPK  G_ZONED8(9),G_DEC8(5)      * Turn into almost hex     65450000
            TR    G_ZONED8,GETDATE_HEXTAB    * Turn into hex            65460000
            MVC   G_HELPER_DATA+9(8),G_ZONED8                           65470000
            LA    R14,G_HELPER_DATA                                     65480000
            ST    R14,G_LWZMTRC_DATA_PTR                                65490000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)                           65500000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA        65510000
            MLWZMRPT RPTLINE=CL133'0Error ending binder fast data accesX65520000
               s session'                                               65530000
            MVC   G_RETCODE,=F'12'                                      65540000
            BR    R8                                                    65550000
         ENDIF                                                          65560000
*                                                                       65570000
         MVI   CONVTOD_INAREA,X'00'                                     65580000
         MVC   CONVTOD_INAREA+1(15),CONVTOD_INAREA                      65590000
         PACK  CONVTOD_INAREA(4),IDB_TIME_BOUND(L'IDB_TIME_BOUND+1)     65600000
         MVI   CONVTOD_INAREA+4,X'00'                                   65610000
         PACK  CONVTOD_INAREA+8(5),IDB_DATE_BOUND(L'IDB_DATE_BOUND+1)   65620000
         MVI   CONVTOD_OUTAREA,X'00'                                    65630000
         MVC   CONVTOD_OUTAREA+1(7),CONVTOD_OUTAREA                     65640000
         MVI   STCKCONV_OUTAREA,X'00'                                   65650000
         MVC   STCKCONV_OUTAREA+1(15),STCKCONV_OUTAREA                  65660000
         CONVTOD CONVVAL=CONVTOD_INAREA,TODVAL=CONVTOD_OUTAREA,TIMETYPEX65670000
               =DEC,DATETYPE=YYYYDDD,MF=(E,CONVTOD_PLIST)               65680000
         STCKCONV STCKVAL=CONVTOD_OUTAREA,CONVVAL=STCKCONV_OUTAREA,TIMEX65690000
               TYPE=DEC,DATETYPE=YYYYMMDD,MF=(E,STCKCONV_PLIST)         65700000
         MVC   DATEWORK_DEC_1(4),STCKCONV_OUTAREA+8                     65710000
         MVC   DATEWORK_DEC_1+4(3),STCKCONV_OUTAREA                     65720000
         MVO   DATEWORK_DEC_2,DATEWORK_DEC_1(7)                         65730000
         MVN   DATEWORK_DEC_2+7(1),=X'0F'                               65740000
         UNPK  DATEWORK_ZON,DATEWORK_DEC_2                              65750000
         MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON                           65760000
*                                                                       65770000
         IEWBUFF FUNC=FREEBUF,TYPE=IDRB  Free IDRB buffer.              65780000
*                                                                       65790000
GET_DATE_LOADMOD_NOTFOUND EQU *                                         65800000
*                                                                       65810000
         IF (CLI,G_DSFOUND,EQ,C'Y') THEN                                65820000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Load modX65830000
               ule found in PDS, last altered on'                       65840000
            MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON                        65850000
            MVC   G_LWZMRPT_LINE+65(19),=C'0000-00-00 00:00:00'         65860000
            MVC   G_LWZMRPT_LINE+65(4),G_SAVE_ALTER_DATE+2              65870000
            MVC   G_LWZMRPT_LINE+70(2),G_SAVE_ALTER_DATE+6              65880000
            MVC   G_LWZMRPT_LINE+73(2),G_SAVE_ALTER_DATE+8              65890000
            MVC   G_LWZMRPT_LINE+76(2),G_SAVE_ALTER_DATE+10             65900000
            MVC   G_LWZMRPT_LINE+79(2),G_SAVE_ALTER_DATE+12             65910000
            MVC   G_LWZMRPT_LINE+82(2),G_SAVE_ALTER_DATE+14             65920000
            L     R15,G_LWZMAKE_RPTA                                    65930000
            BASR  R14,R15                                               65940000
         ELSE                                                           65950000
            MVC   G_SAVE_ALTER_DATE,=16X'FF'                            65960000
            MLWZMRPT RPTLINE=CL133' ..................... Load module nX65970000
               ot found in PDS'                                         65980000
         ENDIF                                                          65990000
*                                                                       66000000
         DROP  R6                                                       66010000
         DROP  R7                                                       66020000
*                                                                       66030000
GET_DATE_LOADMOD_CLOSE EQU *                                            66040000
         L     R14,G_DCB_MEM_PTR                                        66050000
         LA    R2,DCBPDS_BDR-DCB_DSECT(,R14)                            66060000
         CLOSE ((R2)),MODE=31                                           66070000
*                                                                       66080000
GET_DATE_LOADMOD_DEALLOC EQU *                                          66090000
         LA    R6,DYNALLOC_AREA_GD                                      66100000
         USING S99RBP,R6                                                66110000
         LA    R4,S99RBPTR+4                                            66120000
         USING S99RB,R4                                                 66130000
         ST    R4,S99RBPTR                                              66140000
         OI    S99RBPTR,S99RBPND                                        66150000
         XC    S99RB(S99RBEND-S99RB),S99RB                              66160000
         MVI   S99RBLN,S99RBEND-S99RB                                   66170000
         MVI   S99VERB,S99VRBUN                                         66180000
         OI    S99FLG11,S99MSGL0                                        66190000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             66200000
         MVC   0(CDSNTU_GD_L,R5),CDSNTU_GD                              66210000
         LA    R2,CSIFIELD_GD                                           66220000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           66230000
         LA    R3,S99RB+(S99RBEND-S99RB)                                66240000
         ST    R3,S99TXTPP                                              66250000
         O     R5,=X'80000000'                                          66260000
         ST    R5,0(,R3)                                                66270000
         LA    R1,DYNALLOC_AREA_GD                                      66280000
         DYNALLOC                                                       66290000
*                                                                       66300000
         LTR   R15,R15                                                  66310000
         IF (NZ) THEN                                                   66320000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'       66330000
            MVC   G_RETCODE,=F'8'                                       66340000
            BR    R8                                                    66350000
         ENDIF                                                          66360000
*                                                                       66370000
GET_DATE_LOADMOD_RET EQU *                                              66380000
         BR    R8                                                       66390000
*                                                                       66400000
* Get the date from member stats                                        66410000
*                                                                       66420000
GET_DATE_STATS EQU *                                                    66430000
         MLWZMRPT RPTLINE=CL133' ..................... Retrieving PDS mX66440000
               ember stats'                                             66450000
*                                                                       66460000
         MVI   G_DSFOUND,C'N'                                           66470000
*                                                                       66480000
         MVC   MEM8_GD,=CL8' '                                          66490000
         LA    R2,MEM8_GD                                               66500000
         L     R3,G_MVSDS_MEMBER_PTR                                    66510000
         L     R4,G_MVSDS_MEMBER_LEN                                    66520000
         BCTR  R4,R0                                                    66530000
         B     *+10                                                     66540000
         MVC   0(1,R2),0(R3)                                            66550000
         EX    R4,*-6                                                   66560000
*                                                                       66570000
         LA    R6,DYNALLOC_AREA_GD                                      66580000
         USING S99RBP,R6                                                66590000
         LA    R4,S99RBPTR+4                                            66600000
         USING S99RB,R4                                                 66610000
         ST    R4,S99RBPTR                                              66620000
         OI    S99RBPTR,S99RBPND                                        66630000
         XC    S99RB(S99RBEND-S99RB),S99RB                              66640000
         MVI   S99RBLN,S99RBEND-S99RB                                   66650000
         MVI   S99VERB,S99VRBAL                                         66660000
         OI    S99FLG11,S99MSGL0                                        66670000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             66680000
         MVC   0(CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_GD_L,R5),CDSNTU_GD  66690000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           66700000
         LA    R3,S99RB+(S99RBEND-S99RB)                                66710000
         ST    R3,S99TXTPP                                              66720000
         ST    R5,0(,R3)                                                66730000
         LA    R5,CDSNTU_GD_L(,R5)                                      66740000
         ST    R5,4(,R3)                                                66750000
         LA    R5,CSTATUSTU_GD_L(,R5)                                   66760000
         O     R5,=X'80000000'                                          66770000
         ST    R5,8(,R3)                                                66780000
         LA    R1,DYNALLOC_AREA_GD                                      66790000
         DYNALLOC                                                       66800000
*                                                                       66810000
         DROP  R6                                                       66820000
         DROP  R4                                                       66830000
*                                                                       66840000
         LTR   R15,R15                                                  66850000
         IF (NZ) THEN                                                   66860000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'         66870000
            MVC   G_RETCODE,=F'8'                                       66880000
            BR    R8                                                    66890000
         ENDIF                                                          66900000
*                                                                       66910000
         L     R1,G_DCB_MEM_PTR                                         66920000
         MVC   DCBPDS_DIR-DCB_DSECT(LEN_DCBPDS_DIR_GD,R1),CDCBPDS_DIR_GX66930000
               D                                                        66940000
         MVC   DCBEPDS_DIR-DCB_DSECT(LEN_DCBEPDS_DIR_GD,R1),CDCBEPDS_DIX66950000
               R_GD                                                     66960000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             66970000
         LA    R3,DCBEPDS_DIR-DCB_DSECT(,R1)                            66980000
         ST    R3,DCBDCBE-IHADCB(,R2)                                   66990000
         LA    R4,PDSDIR_IS_EOF_GD                                      67000000
         ST    R4,DCBEEODA-DCBE(,R3)                                    67010000
         L     R6,DYNALLOC_AREA_GD                                      67020000
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_GD_L+CSTATUSTU_GD_L(,R6)   67030000
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)                              67040000
*                                                                       67050000
         MVI   PDSDIR_EOF_GD,C'N'                                       67060000
*                                                                       67070000
         LA    R6,GET_DATE_OPENPDS                                      67080000
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                      67090000
GET_DATE_OPENPDS EQU *                                                  67100000
*                                                                       67110000
         LTR   R15,R15                                                  67120000
         IF (NZ) THEN                                                   67130000
            MLWZMRPT RPTLINE=CL133'0OPEN failed for reading PDS directoX67140000
               ry'                                                      67150000
            MVC   G_RETCODE,=F'8'                                       67160000
            B     GET_DATE_DEALLOC                                      67170000
         ENDIF                                                          67180000
*                                                                       67190000
GET_DATE_GET_DIRREC EQU *                                               67200000
         L     R1,G_DCB_MEM_PTR                                         67210000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             67220000
         LA    R6,GET_DATE_DIRREC_NOMORE                                67230000
         GET   (R2),DIRREC_GD                                           67240000
*                                                                       67250000
         LA    R3,DIRREC_GD                                             67260000
         XR    R4,R4                                                    67270000
         LH    R4,0(,R3)                                                67280000
         C     R4,=F'14'                                                67290000
         BL    GET_DATE_DIRREC_END_OF_BLOCK                             67300000
         LA    R3,2(,R3)                                                67310000
         S     R4,=F'2'                                                 67320000
GET_DATE_NEXT_DIRREC_ENTRY EQU *                                        67330000
         CLC   0(8,R3),=8X'FF'                                          67340000
         BE    GET_DATE_DIRREC_NOMORE                                   67350000
         CLC   0(8,R3),MEM8_GD                                          67360000
         IF (EQ) THEN                                                   67370000
            MVI   G_DSFOUND,C'Y'                                        67380000
            L     R5,8(,R3)                                             67390000
            N     R5,=X'0000001F'                                       67400000
            SLL   R5,1                                                  67410000
            C     R5,=F'30'                                             67420000
            IF (NL) THEN                                                67430000
               MVI   CONVTOD_INAREA,X'00'                               67440000
               MVC   CONVTOD_INAREA+1(15),CONVTOD_INAREA                67450000
               MVC   CONVTOD_INAREA(2),24(R3)                           67460000
               MVC   CONVTOD_INAREA+2(1),15(R3)                         67470000
               MVC   CONVTOD_INAREA+8(4),20(R3)                         67480000
               MVI   CONVTOD_OUTAREA,X'00'                              67490000
               MVC   CONVTOD_OUTAREA+1(7),CONVTOD_OUTAREA               67500000
               MVI   STCKCONV_OUTAREA,X'00'                             67510000
               MVC   STCKCONV_OUTAREA+1(15),STCKCONV_OUTAREA            67520000
               CONVTOD CONVVAL=CONVTOD_INAREA,TODVAL=CONVTOD_OUTAREA,TIX67530000
               METYPE=DEC,DATETYPE=YYDDD,MF=(E,CONVTOD_PLIST)           67540000
               STCKCONV STCKVAL=CONVTOD_OUTAREA,CONVVAL=STCKCONV_OUTAREX67550000
               A,TIMETYPE=DEC,DATETYPE=YYYYMMDD,MF=(E,STCKCONV_PLIST)   67560000
               MVC   G_LWZMRPT_LINE,=CL133' ..................... MembeX67570000
               r found in PDS directory, last altered on'               67580000
               MVC   DATEWORK_DEC_1(4),STCKCONV_OUTAREA+8               67590000
               MVC   DATEWORK_DEC_1+4(3),STCKCONV_OUTAREA               67600000
               MVO   DATEWORK_DEC_2,DATEWORK_DEC_1(7)                   67610000
               MVN   DATEWORK_DEC_2+7(1),=X'0F'                         67620000
               UNPK  DATEWORK_ZON,DATEWORK_DEC_2                        67630000
               MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON                     67640000
               MVC   G_LWZMRPT_LINE+70(19),=C'0000-00-00 00:00:00'      67650000
               MVC   G_LWZMRPT_LINE+70(4),DATEWORK_ZON+2                67660000
               MVC   G_LWZMRPT_LINE+75(2),DATEWORK_ZON+6                67670000
               MVC   G_LWZMRPT_LINE+78(2),DATEWORK_ZON+8                67680000
               MVC   G_LWZMRPT_LINE+81(2),DATEWORK_ZON+10               67690000
               MVC   G_LWZMRPT_LINE+84(2),DATEWORK_ZON+12               67700000
               MVC   G_LWZMRPT_LINE+87(2),DATEWORK_ZON+14               67710000
               L     R15,G_LWZMAKE_RPTA                                 67720000
               BASR  R14,R15                                            67730000
            ELSE                                                        67740000
               MLWZMRPT RPTLINE=CL133' ..................... Member fouX67750000
               nd in PDS directory but without statistics!!!'           67760000
            ENDIF                                                       67770000
            B     GET_DATE_DIRREC_NOMORE                                67780000
         ELSE                                                           67790000
            L     R5,8(,R3)                                             67800000
            N     R5,=X'0000001F'                                       67810000
            SLL   R5,1                                                  67820000
            LA    R3,12(,R3)                                            67830000
            S     R4,=F'12'                                             67840000
            AR    R3,R5                                                 67850000
            SR    R4,R5                                                 67860000
            BC    B'0010',GET_DATE_NEXT_DIRREC_ENTRY                    67870000
         ENDIF                                                          67880000
*                                                                       67890000
GET_DATE_DIRREC_END_OF_BLOCK EQU *                                      67900000
         B     GET_DATE_GET_DIRREC                                      67910000
*                                                                       67920000
GET_DATE_DIRREC_NOMORE EQU *                                            67930000
*                                                                       67940000
         L     R1,G_DCB_MEM_PTR                                         67950000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             67960000
         CLOSE ((R2)),MODE=31                                           67970000
*                                                                       67980000
GET_DATE_DEALLOC EQU *                                                  67990000
         LA    R6,DYNALLOC_AREA_GD                                      68000000
         USING S99RBP,R6                                                68010000
         LA    R4,S99RBPTR+4                                            68020000
         USING S99RB,R4                                                 68030000
         ST    R4,S99RBPTR                                              68040000
         OI    S99RBPTR,S99RBPND                                        68050000
         XC    S99RB(S99RBEND-S99RB),S99RB                              68060000
         MVI   S99RBLN,S99RBEND-S99RB                                   68070000
         MVI   S99VERB,S99VRBUN                                         68080000
         OI    S99FLG11,S99MSGL0                                        68090000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             68100000
         MVC   0(CDSNTU_GD_L,R5),CDSNTU_GD                              68110000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           68120000
         LA    R3,S99RB+(S99RBEND-S99RB)                                68130000
         ST    R3,S99TXTPP                                              68140000
         O     R5,=X'80000000'                                          68150000
         ST    R5,0(,R3)                                                68160000
         LA    R1,DYNALLOC_AREA_GD                                      68170000
         DYNALLOC                                                       68180000
*                                                                       68190000
         LTR   R15,R15                                                  68200000
         IF (NZ) THEN                                                   68210000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'       68220000
            MVC   G_RETCODE,=F'8'                                       68230000
            BR    R8                                                    68240000
         ENDIF                                                          68250000
*                                                                       68260000
         IF (CLI,G_DSFOUND,NE,C'Y') THEN                                68270000
            MVC   G_SAVE_ALTER_DATE,=16X'FF'                            68280000
         ENDIF                                                          68290000
*                                                                       68300000
GET_DATE_STATS_RET EQU *                                                68310000
         BR    R8                                                       68320000
*                                                                       68330000
* EODAD for DCBPDS                                                      68340000
*                                                                       68350000
PDSDIR_IS_EOF_GD EQU *                                                  68360000
         MVI   PDSDIR_EOF_GD,C'Y'                                       68370000
         BR    R6                                                       68380000
*                                                                       68390000
         LTORG                                                          68400000
*                                                                       68410000
* Translate table for conversion to hex                                 68420000
                            DS    0F                                    68430000
GETDATE_HEXTAB              EQU   *-C'0'                                68440000
                            DC    C'0123456789ABCDEF'                   68450000
*                                                                       68460000
TRT_ALPHANAT DS    0F A-Z $ # @                                         68470000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        68480000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    68490000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    68500000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    68510000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    68520000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    68530000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    68540000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    68550000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    68560000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    68570000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    68580000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    68590000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    68600000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    68610000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    68620000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    68630000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F                    68640000
*                                                                       68650000
TRT_ALPHANUMNATDASH DS    0F A-Z 0-9 $ # @ -                            68660000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        68670000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    68680000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    68690000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    68700000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    68710000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    68720000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    68730000
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    68740000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    68750000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    68760000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    68770000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    68780000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    68790000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    68800000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    68810000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    68820000
         DC    X'00000000000000000000FFFFFFFFFFFF' F                    68830000
*                                                                       68840000
CONST_CSIFIELD_GD DS 0F                                                 68850000
         DC    CL44' '        CSIFILTK FILTER   KEY                     68860000
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS           68870000
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS            68880000
         DS    0CL16          CSIDTYPD ENTRY TYPES                      68890000
         DC    CL16'                ' CSIDTYPS                          68900000
         DS    0CL4           CSIOPTS  CSI OPTIONS                      68910000
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' ' 68920000
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' ' 68930000
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' ' 68940000
         DC    XL1'00'        CSIRESRV RESERVED                         68950000
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING      68960000
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES       68970000
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME                       68980000
CONST_CSIFIELD_GD_LEN EQU *-CONST_CSIFIELD_GD                           68990000
*                                                                       69000000
CDSNTU_GD                   DC    AL2(DALDSNAM)                         69010000
                            DC    X'0001'                               69020000
                            DC    X'002C'                               69030000
                            DC    CL44' '                               69040000
CDSNTU_GD_L                 EQU   *-CDSNTU_GD                           69050000
*                                                                       69060000
CSTATUSTU_GD                DC    AL2(DALSTATS)                         69070000
                            DC    X'0001'                               69080000
                            DC    X'0001'                               69090000
                            DC    X'08'                                 69100000
CSTATUSTU_GD_L              EQU   *-CSTATUSTU_GD                        69110000
*                                                                       69120000
CRETDDN_GD                  DC    AL2(DALRTDDN)                         69130000
                            DC    X'0001'                               69140000
                            DC    X'0008'                               69150000
                            DC    CL8' '                                69160000
CRETDDN_GD_L                EQU   *-CRETDDN_GD                          69170000
*                                                                       69180000
CDCBPDS_DIR_GD              DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVDX69190000
               =DA,DSORG=PS,RECFM=F,DCBE=CDCBEPDS_DIR_GD                69200000
LEN_DCBPDS_DIR_GD           EQU   *-CDCBPDS_DIR_GD                      69210000
CDCBEPDS_DIR_GD             DCBE  EODAD=0,RMODE31=BUFF                  69220000
LEN_DCBEPDS_DIR_GD          EQU   *-CDCBEPDS_DIR_GD                     69230000
*                                                                       69240000
CDCBPDS_BDR                 DCB   MACRF=R,DSORG=PO,RECFM=U              69250000
LEN_DCBPDS_BDR              EQU   *-CDCBPDS_BDR                         69260000
*                                                                       69270000
CONVTOD_L                   CONVTOD MF=L                                69280000
CONVTOD_L_SIZ               EQU   *-CONVTOD_L                           69290000
STCKCONV_L                  STCKCONV MF=L                               69300000
STCKCONV_L_SIZ              EQU   *-STCKCONV_L                          69310000
*                                                                       69320000
IDBBUF                      IEWBUFF FUNC=MAPBUF,TYPE=IDRB,VERSION=6,BYTX69330000
               ES=2048                                                  69340000
*                                                                       69350000
GET_DATE_DSECT              DSECT                                       69360000
                            DS    18F * My savearea                     69370000
*                                                                       69380000
DAREAPTR_GD                 DS    A      DATA AREA POINTER (64K)        69390000
*                                                                       69400000
MODRSNRT_GD                 DS    0F                                    69410000
PARMRC_GD                   DS    0CL4                                  69420000
MODID_GD                    DS    CL2    MODULE ID                      69430000
RSNCODE_GD                  DS    CL1    REASON CODE                    69440000
RTNCODE_GD                  DS    CL1    RETURN CODE                    69450000
*                                                                       69460000
CSIFIELD_GD                 DS    0F                                    69470000
                            ORG   *+CSIFIELD_DSECT_SIZ                  69480000
CSIFIELD_GD_LEN             EQU   *-CSIFIELD_GD                         69490000
*                                                                       69500000
PARMLIST_GD                 DS    0F                                    69510000
                            DS    A                                     69520000
                            DS    A                                     69530000
                            DS    A                                     69540000
*                                                                       69550000
                            DS    0F                                    69560000
CONVTOD_INAREA              DS    4F                                    69570000
CONVTOD_OUTAREA             DS    2F                                    69580000
CONVTOD_PLIST               DS    CL(CONVTOD_L_SIZ)                     69590000
*                                                                       69600000
                            DS    0F                                    69610000
STCKCONV_OUTAREA            DS    CL16                                  69620000
STCKCONV_PLIST              DS    CL(STCKCONV_L_SIZ)                    69630000
*                                                                       69640000
                            DS    0F                                    69650000
DATEWORK_DEC_1              DS    CL8                                   69660000
DATEWORK_DEC_2              DS    CL8                                   69670000
DATEWORK_ZON                DS    CL16                                  69680000
*                                                                       69690000
                            DS    0F                                    69700000
PDSDIR_EOF_GD               DS    C                                     69710000
*                                                                       69720000
                            DS    0F                                    69730000
DSCBPAR_GD                  DS    4F                                    69740000
OBTAIN_GD                   DS    0F                                    69750000
                            ORG   *+OBTAIN_DSECT_SIZ                    69760000
*                                                                       69770000
DYNALLOC_AREA_GD            DS    0F                                    69780000
                            ORG   *+4                                   69790000
                            ORG   *+(S99RBEND-S99RB)                    69800000
                            ORG   *+12                                  69810000
                            ORG   *+CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_X69820000
               GD_L                                                     69830000
*                                                                       69840000
                            DS    0F                                    69850000
DIRREC_GD                   DS    CL256                                 69860000
*                                                                       69870000
MEM8_GD                     DS    CL8                                   69880000
*                                                                       69890000
DAREA_GD                    DS    C                                     69900000
                            ORG   *+1023                                69910000
DAREA_GD_SIZ                EQU   *-DAREA_GD                            69920000
*                                                                       69930000
IEWBFDAT_SB_PAR4A           DS    4A                                    69940000
IEWBFDAT_SB_SB              DS    CL4                                   69950000
IEWBFDAT_SB_MTOKEN          DS    CL4                                   69960000
IEWBFDAT_SB_PGMNAME         DS    CL8                                   69970000
*                                                                       69980000
IEWBFDAT_GD_PAR8A           DS    8A                                    69990000
IEWBFDAT_GD_GD              DS    CL4                                   70000000
IEWBFDAT_GD_MTOKEN          DS    CL4                                   70010000
IEWBFDAT_GD_B_IDRB          DS    CL8                                   70020000
IEWBFDAT_GD_CURSOR          DS    F                                     70030000
IEWBFDAT_GD_COUNT           DS    F                                     70040000
*                                                                       70050000
IEWBFDAT_EN_PAR2A           DS    2A                                    70060000
IEWBFDAT_EN_EN              DS    CL4                                   70070000
IEWBFDAT_EN_MTOKEN          DS    CL4                                   70080000
*                                                                       70090000
GET_DATE_DSECT_SIZ          EQU   *-GET_DATE_DSECT                      70100000
*                                                                       70110000
         IEFZB4D0                                                       70120000
         IEFZB4D2                                                       70130000
*                                                                       70140000
LWZMAKE  CSECT                                                          70150000
*                                                                       70160000
* Get the member list of a data set                                     70170000
*                                                                       70180000
         DROP                                                           70190000
*                                                                       70200000
LWZMAKE_GET_MEMLIST DS    0F                                            70210000
         STM   R14,R12,12(R13)   * Save callers registers               70220000
         LR    R10,R15                                                  70230000
         LA    R11,4095(,R10)                                           70240000
         LA    R11,1(,R11)                                              70250000
         USING LWZMAKE_GET_MEMLIST,R10,R11                              70260000
         GETMAIN RU,LV=GET_MEMLIST_DSECT_SIZ                            70270000
         ST    R13,4(R1)         * Backward chain callers SA            70280000
         ST    R1,8(R13)         * Forward chain my SA                  70290000
         LR    R13,R1            * Point R13 to my SA                   70300000
         USING GET_MEMLIST_DSECT,R13 * Establish addressing of workarea 70310000
         USING GLOBAL_DATA_DSECT,R9                                     70320000
*                                                                       70330000
*        Trace record to start section                                  70340000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX70350000
               KE_GET_MEMLIST'                                          70360000
*                                                                       70370005
         MVC   MEM8FILTER_LEN,=F'0'                                     70380005
         LT    R2,G_SCAN_TOKEN3_LEN                                     70390005
         IF (NZ) THEN                                                   70400005
            C     R2,=F'8'                                              70410005
            IF (H) THEN                                                 70420005
               L     R2,=F'8'                                           70430005
            ENDIF                                                       70440005
            ST    R2,MEM8FILTER_LEN                                     70450006
            LA    R3,MEM8FILTER                                         70460005
            L     R4,G_SCAN_TOKEN3A                                     70470005
            BCTR  R2,R0                                                 70480005
            B     *+10                                                  70490005
            MVC   0(1,R3),0(R4)                                         70500005
            EX    R2,*-6                                                70510005
            MVC   G_SCAN_TOKEN3_LEN,=F'0'                               70520005
         ENDIF                                                          70530005
*                                                                       70540000
         L     R3,G_SCAN_TOKEN2A                                        70550000
         L     R4,G_SCAN_TOKEN2_LEN                                     70560000
         XR    R6,R6                                                    70570000
         XR    R7,R7                                                    70580000
*                                                                       70590000
GET_MEMLIST_TEST_QUAL1 EQU *                                            70600000
         LTR   R4,R4                                                    70610000
         BZ    GET_MEMLIST_NOT_MVSDS                                    70620000
         TRT   0(1,R3),TRT_ALPHANAT_MEMLIST                             70630000
         BNZ   GET_MEMLIST_NOT_MVSDS                                    70640000
         LA    R3,1(,R3)                                                70650000
         BCT   R4,*+8                                                   70660000
         B     GET_MEMLIST_MVSDS                                        70670000
         LR    R5,R4                                                    70680000
         C     R5,=F'7'                                                 70690000
         IF (H) THEN                                                    70700000
            L     R5,=F'7'                                              70710000
         ENDIF                                                          70720000
         BCTR  R5,R0                                                    70730000
         B     *+10                                                     70740000
         TRT   0(1,R3),TRT_ALPHANUMNATDASH_MEMLIST                      70750000
         EX    R5,*-6                                                   70760000
         IF (Z) THEN                                                    70770000
            LA    R5,1(,R5)                                             70780000
         ELSE                                                           70790000
            LR    R5,R1                                                 70800000
            SR    R5,R3                                                 70810000
         ENDIF                                                          70820000
         AR    R3,R5                                                    70830000
         SR    R4,R5                                                    70840000
         BZ    GET_MEMLIST_MVSDS                                        70850000
         IF (CLI,0(R3),EQ,C'.') THEN                                    70860000
            LA    R3,1(,R3)                                             70870000
            BCTR  R4,R0                                                 70880000
            B     GET_MEMLIST_TEST_QUAL1                                70890000
         ENDIF                                                          70900000
         B     GET_MEMLIST_NOT_MVSDS                                    70910000
*                                                                       70920000
GET_MEMLIST_MVSDS EQU *                                                 70930000
         BAL   R8,GET_MEMLIST_IGGCSI00                                  70940000
*                                                                       70950000
         CLC   G_RETCODE,=A(0)                                          70960000
         BNE   GET_MEMLIST_RET                                          70970000
*                                                                       70980000
         CLI   G_DSFOUND,C'Y'                                           70990000
         BNE   GET_MEMLIST_RET                                          71000000
*                                                                       71010000
         BAL   R8,GET_MEMLIST_OBTAIN                                    71020000
*                                                                       71030000
         CLC   G_RETCODE,=A(0)                                          71040000
         BNE   GET_MEMLIST_RET                                          71050000
*                                                                       71060000
         BAL   R8,GET_MEMLIST_MEMS                                      71070000
*                                                                       71080000
         B     GET_MEMLIST_RET                                          71090000
*                                                                       71100000
GET_MEMLIST_NOT_MVSDS EQU *                                             71110000
         MLWZMRPT RPTLINE=CL133'0Member list requested for non MVS fileX71120000
               '                                                        71130000
         MVC   G_RETCODE,=F'12'                                         71140000
*                                                                       71150000
GET_MEMLIST_RET EQU *                                                   71160000
         L     R3,4(,R13)        * Restore address of callers SA        71170000
         FREEMAIN RU,LV=GET_MEMLIST_DSECT_SIZ,A=(R13)                   71180000
         LR    R13,R3                                                   71190000
         LM    R14,R12,12(R13)                                          71200000
         BR    R14                    Return to caller                  71210000
*                                                                       71220000
* Perform catalog search with IGGCSI00                                  71230000
*                                                                       71240000
GET_MEMLIST_IGGCSI00 EQU *                                              71250000
         MVI   G_DSFOUND,C'N'                                           71260000
*                                                                       71270000
         LA    R1,DAREA_ML                                              71280000
         ST    R1,DAREAPTR_ML                                           71290000
         L     R2,=A(DAREA_ML_SIZ)                                      71300000
         ST    R2,0(,R1)                                                71310000
*                                                                       71320000
         LA    R2,CSIFIELD_ML                                           71330000
         L     R3,=A(CSIFIELD_ML_LEN)                                   71340000
         LA    R4,CONST_CSIFIELD_ML                                     71350000
         L     R5,=A(CONST_CSIFIELD_ML_LEN)                             71360000
         MVCL  R2,R4                                                    71370000
*                                                                       71380000
         LA    R2,CSIFIELD_ML+(CSIFILTK-CSIFIELD_DSECT)                 71390000
         L     R3,G_SCAN_TOKEN2A                                        71400000
         L     R4,G_SCAN_TOKEN2_LEN                                     71410000
         LT    R5,G_MVSDS_MEMBER_PTR                                    71420000
         IF (NZ) THEN                                                   71430000
            SR    R5,R3                                                 71440000
            BCTR  R5,R0                                                 71450000
            LR    R4,R5                                                 71460000
         ENDIF                                                          71470000
         C     R4,=A(L'CSIFILTK)                                        71480000
         IF (H)                                                         71490000
            L     R4,=A(L'CSIFILTK)                                     71500000
         ENDIF                                                          71510000
         BCTR  R4,R0                                                    71520000
         B     *+10                                                     71530000
         MVC   0(1,R2),0(R3)                                            71540000
         EX    R4,*-6                                                   71550000
*                                                                       71560000
         LA    R1,PARMLIST_ML                                           71570000
         LA    R2,MODRSNRT_ML                                           71580000
         ST    R2,0(R1)                                                 71590000
         LA    R2,CSIFIELD_ML                                           71600000
         ST    R2,4(R1)                                                 71610000
         L     R2,DAREAPTR_ML                                           71620000
         O     R2,=X'80000000'                                          71630000
         ST    R2,8(R1)                                                 71640000
*                                                                       71650000
         L     R15,G_IGGCSI00A                                          71660000
         BASR  R14,R15                                                  71670000
*                                                                       71680000
         C     R15,=F'4'                                                71690000
         IF (H) THEN                                                    71700000
            MLWZMRPT RPTLINE=CL133'0Catalog search interface returned eX71710000
               rror code'                                               71720000
            MVC   G_RETCODE,=F'12'                                      71730000
            BR    R8                                                    71740000
         ENDIF                                                          71750000
*                                                                       71760000
         LA    R1,DAREA_ML                                              71770000
         CLC   8(4,R1),=F'64'                                           71780000
         IF (H) THEN                                                    71790000
            MVI   G_DSFOUND,C'Y'                                        71800000
         ENDIF                                                          71810000
*                                                                       71820000
GET_MEMLIST_IGGCSI00_RET EQU *                                          71830000
         BR    R8                                                       71840000
*                                                                       71850000
* Perform CAMLST OBTAIN                                                 71860000
*                                                                       71870000
GET_MEMLIST_OBTAIN EQU *                                                71880000
         XR    R1,R1                                                    71890000
         ICM   R1,B'1000',=AL1(193)                                     71900000
         ST    R1,DSCBPAR_ML                                            71910000
         MVC   OBTAIN_ML+(DS1DSNAM-OBTAIN_DSECT)(L'DS1DSNAM),CSIFIELD_MX71920000
               L+(CSIFILTK-CSIFIELD_DSECT)                              71930000
         LA    R1,OBTAIN_ML+(DS1DSNAM-OBTAIN_DSECT)                     71940000
         ST    R1,DSCBPAR_ML+4                                          71950000
         LA    R1,DAREA_ML+110                                          71960000
         CLC   0(2,R1),=H'12'   * Is volume name present                71970000
         BL    GET_MEMLIST_OBTAIN_RET                                   71980000
         LA    R1,6(,R1)                                                71990000
         ST    R1,DSCBPAR_ML+8                                          72000000
         LA    R1,OBTAIN_ML+(DS1FMTID-OBTAIN_DSECT)                     72010000
         ST    R1,DSCBPAR_ML+12                                         72020000
*                                                                       72030000
         OBTAIN DSCBPAR_ML                                              72040000
*                                                                       72050000
         LTR   R15,R15                                                  72060000
         IF (NZ) THEN                                                   72070000
            MLWZMRPT RPTLINE=CL133'0CAMLST OBTAIN returned error code'  72080000
            MVC   G_RETCODE,=F'12'                                      72090000
            BR    R8                                                    72100000
         ENDIF                                                          72110000
*                                                                       72120000
         IF (TM,OBTAIN_ML+(DS1DSORG-OBTAIN_DSECT),DS1DSGPO,Z) THEN      72130000
            MVC   G_LWZMRPT_LINE,=CL133'0Member list requested on non-PX72140000
               DS dataset'                                              72150000
            LA    R2,CSIFIELD_ML                                        72160000
            MVC   G_LWZMRPT_LINE+37(L'CSIFILTK),CSIFILTK-CSIFIELD_DSECTX72170000
               (R2)                                                     72180000
            L     R15,G_LWZMAKE_RPTA                                    72190000
            BASR  R14,R15                                               72200000
            MVC   G_RETCODE,=F'8'                                       72210000
            BR    R8                                                    72220000
         ENDIF                                                          72230000
*                                                                       72240000
GET_MEMLIST_OBTAIN_RET EQU *                                            72250000
         BR    R8                                                       72260000
*                                                                       72270000
* Get the member list                                                   72280000
*                                                                       72290000
GET_MEMLIST_MEMS EQU *                                                  72300000
         LA    R6,DYNALLOC_AREA_ML                                      72310000
         USING S99RBP,R6                                                72320000
         LA    R4,S99RBPTR+4                                            72330000
         USING S99RB,R4                                                 72340000
         ST    R4,S99RBPTR                                              72350000
         OI    S99RBPTR,S99RBPND                                        72360000
         XC    S99RB(S99RBEND-S99RB),S99RB                              72370000
         MVI   S99RBLN,S99RBEND-S99RB                                   72380000
         MVI   S99VERB,S99VRBAL                                         72390000
         OI    S99FLG11,S99MSGL0                                        72400000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             72410000
         MVC   0(CDSNTU_ML_L+CSTATUSTU_ML_L+CRETDDN_ML_L,R5),CDSNTU_ML  72420000
         MVC   6(44,R5),CSIFIELD_ML+(CSIFILTK-CSIFIELD_DSECT)           72430000
         LA    R3,S99RB+(S99RBEND-S99RB)                                72440000
         ST    R3,S99TXTPP                                              72450000
         ST    R5,0(,R3)                                                72460000
         LA    R5,CDSNTU_ML_L(,R5)                                      72470000
         ST    R5,4(,R3)                                                72480000
         LA    R5,CSTATUSTU_ML_L(,R5)                                   72490000
         O     R5,=X'80000000'                                          72500000
         ST    R5,8(,R3)                                                72510000
         LA    R1,DYNALLOC_AREA_ML                                      72520000
         DYNALLOC                                                       72530000
*                                                                       72540000
         DROP  R6                                                       72550000
         DROP  R4                                                       72560000
*                                                                       72570000
         LTR   R15,R15                                                  72580000
         IF (NZ) THEN                                                   72590000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'         72600000
            MVC   G_RETCODE,=F'8'                                       72610000
            BR    R8                                                    72620000
         ENDIF                                                          72630000
*                                                                       72640000
         L     R1,G_DCB_MEM_PTR                                         72650000
         MVC   DCBPDS_DIR-DCB_DSECT(LEN_DCBPDS_DIR_ML,R1),CDCBPDS_DIR_MX72660000
               L                                                        72670000
         MVC   DCBEPDS_DIR-DCB_DSECT(LEN_DCBEPDS_DIR_ML,R1),CDCBEPDS_DIX72680000
               R_ML                                                     72690000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             72700000
         LA    R3,DCBEPDS_DIR-DCB_DSECT(,R1)                            72710000
         ST    R3,DCBDCBE-IHADCB(,R2)                                   72720000
         LA    R4,PDSDIR_IS_EOF_ML                                      72730000
         ST    R4,DCBEEODA-DCBE(,R3)                                    72740000
         L     R6,DYNALLOC_AREA_ML                                      72750000
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_ML_L+CSTATUSTU_ML_L(,R6)   72760000
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)                              72770000
*                                                                       72780000
         MVI   PDSDIR_EOF_ML,C'N'                                       72790000
*                                                                       72800000
         LA    R6,GET_MEMLIST_OPENPDS                                   72810000
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                      72820000
GET_MEMLIST_OPENPDS EQU *                                               72830000
*                                                                       72840000
         LTR   R15,R15                                                  72850000
         IF (NZ) THEN                                                   72860000
            MLWZMRPT RPTLINE=CL133'0OPEN failed for reading PDS directoX72870000
               ry'                                                      72880000
            MVC   G_RETCODE,=F'8'                                       72890000
            B     GET_MEMLIST_DEALLOC                                   72900000
         ENDIF                                                          72910000
*                                                                       72920000
GET_MEMLIST_GET_DIRREC EQU *                                            72930000
         L     R1,G_DCB_MEM_PTR                                         72940000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             72950000
         LA    R6,GET_MEMLIST_DIRREC_NOMORE                             72960000
         GET   (R2),DIRREC_ML                                           72970000
*                                                                       72980000
         LA    R3,DIRREC_ML                                             72990000
         XR    R4,R4                                                    73000000
         LH    R4,0(,R3)                                                73010000
         C     R4,=F'14'                                                73020000
         BL    GET_MEMLIST_DIRREC_END_OF_BLOCK                          73030000
         LA    R3,2(,R3)                                                73040000
         S     R4,=F'2'                                                 73050000
GET_MEMLIST_NEXT_DIRREC_ENTRY EQU *                                     73060000
         CLC   0(8,R3),=8X'FF'                                          73070000
         BE    GET_MEMLIST_DIRREC_NOMORE                                73080000
         LT    R6,MEM8FILTER_LEN                                        73090007
         IF (NZ) THEN                                                   73100007
            LA    R5,MEM8FILTER                                         73110007
            BCTR  R6,R0                                                 73120007
            B     *+10                                                  73130007
            CLC   0(1,R3),0(R5)                                         73140007
            EX    R6,*-6                                                73150007
            BNE   GET_MEMLIST_MEM_SKIP                                  73160007
         ENDIF                                                          73170007
         L     R5,G_SCAN_TOKEN3A                                        73180000
         LT    R6,G_SCAN_TOKEN3_LEN                                     73190000
         IF (NZ) THEN                                                   73200000
            LA    R6,9(,R6)                                             73210000
            C     R6,G_SCAN_TOKEN3_MAXLEN                               73220000
            IF (H) THEN                                                 73230000
               STM   R14,R12,GET_MEMLIST_SAVEAREA2+12                   73240000
               L     R3,G_SCAN_TOKEN3_MAXLEN * Get current max len      73250000
               LR    R6,R3         * Save it for storage release        73260000
               SLL   R3,1          * Multiply max length by 2           73270000
               ST    R3,G_SCAN_TOKEN3_MAXLEN * Make it new max len      73280000
               STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block     73290000
               LR    R0,R1         * Have R0 point to new block         73300000
               L     R1,G_SCAN_TOKEN3_LEN * Get length of token 3       73310000
               L     R2,G_SCAN_TOKEN3A * Have R2 point to old block     73320000
               LR    R5,R2         * Save it for storage release        73330000
               LR    R3,R1         * Make sure no cropping/filling      73340000
               ST    R0,G_SCAN_TOKEN3A * Save ptr to new block          73350000
               MVCL  R0,R2            * Copy old to new block           73360000
               STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)                    73370000
               LM    R14,R12,GET_MEMLIST_SAVEAREA2+12                   73380000
               L     R5,G_SCAN_TOKEN3A                                  73390000
            ENDIF                                                       73400000
            L     R6,G_SCAN_TOKEN3_LEN                                  73410001
            AR    R5,R6                                                 73420000
            MVI   0(R5),C' '                                            73430000
            LA    R5,1(,R5)                                             73440000
            LA    R6,1(,R6)                                             73450000
            ST    R6,G_SCAN_TOKEN3_LEN                                  73460000
         ENDIF                                                          73470000
         LR    R1,R3                                                    73480000
         LA    R14,8                                                    73490000
GET_MEMLIST_MEM_CHAR EQU *                                              73500000
         CLI   0(R1),C' '                                               73510000
         BE    GET_MEMLIST_MEM_DONE                                     73520000
         MVC   0(1,R5),0(R1)                                            73530000
         LA    R5,1(,R5)                                                73540000
         LA    R6,1(,R6)                                                73550000
         LA    R1,1(,R1)                                                73560000
         BCT   R14,GET_MEMLIST_MEM_CHAR                                 73570000
GET_MEMLIST_MEM_DONE EQU *                                              73580000
         ST    R6,G_SCAN_TOKEN3_LEN                                     73590000
GET_MEMLIST_MEM_SKIP EQU *                                              73600007
         L     R5,8(,R3)                                                73610000
         N     R5,=X'0000001F'                                          73620000
         SLL   R5,1                                                     73630000
         LA    R3,12(,R3)                                               73640000
         S     R4,=F'12'                                                73650000
         AR    R3,R5                                                    73660000
         SR    R4,R5                                                    73670000
         BC    B'0010',GET_MEMLIST_NEXT_DIRREC_ENTRY                    73680000
*                                                                       73690000
GET_MEMLIST_DIRREC_END_OF_BLOCK EQU *                                   73700000
         B     GET_MEMLIST_GET_DIRREC                                   73710000
*                                                                       73720000
GET_MEMLIST_DIRREC_NOMORE EQU *                                         73730000
*                                                                       73740000
         L     R1,G_DCB_MEM_PTR                                         73750000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             73760000
         CLOSE ((R2)),MODE=31                                           73770000
*                                                                       73780000
GET_MEMLIST_DEALLOC EQU *                                               73790000
         LA    R6,DYNALLOC_AREA_ML                                      73800000
         USING S99RBP,R6                                                73810000
         LA    R4,S99RBPTR+4                                            73820000
         USING S99RB,R4                                                 73830000
         ST    R4,S99RBPTR                                              73840000
         OI    S99RBPTR,S99RBPND                                        73850000
         XC    S99RB(S99RBEND-S99RB),S99RB                              73860000
         MVI   S99RBLN,S99RBEND-S99RB                                   73870000
         MVI   S99VERB,S99VRBUN                                         73880000
         OI    S99FLG11,S99MSGL0                                        73890000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             73900000
         MVC   0(CDSNTU_ML_L,R5),CDSNTU_ML                              73910000
         LA    R2,CSIFIELD_ML                                           73920000
         MVC   6(44,R5),CSIFILTK-CSIFIELD_DSECT(R2)                     73930000
         LA    R3,S99RB+(S99RBEND-S99RB)                                73940000
         ST    R3,S99TXTPP                                              73950000
         O     R5,=X'80000000'                                          73960000
         ST    R5,0(,R3)                                                73970000
         LA    R1,DYNALLOC_AREA_ML                                      73980000
         DYNALLOC                                                       73990000
*                                                                       74000000
         LTR   R15,R15                                                  74010000
         IF (NZ) THEN                                                   74020000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'       74030000
            MVC   G_RETCODE,=F'8'                                       74040000
            BR    R8                                                    74050000
         ENDIF                                                          74060000
*                                                                       74070000
GET_MEMLIST_MEMS_RET EQU *                                              74080000
         BR    R8                                                       74090000
*                                                                       74100000
* EODAD for DCBPDS                                                      74110000
*                                                                       74120000
PDSDIR_IS_EOF_ML EQU *                                                  74130000
         MVI   PDSDIR_EOF_ML,C'Y'                                       74140000
         BR    R6                                                       74150000
*                                                                       74160000
         LTORG                                                          74170000
*                                                                       74180000
TRT_ALPHANAT_MEMLIST DS    0F A-Z $ # @                                 74190000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        74200000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    74210000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    74220000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    74230000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    74240000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    74250000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    74260000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    74270000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    74280000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    74290000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    74300000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    74310000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    74320000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    74330000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    74340000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    74350000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F                    74360000
*                                                                       74370000
TRT_ALPHANUMNATDASH_MEMLIST DS    0F A-Z 0-9 $ # @ -                    74380000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        74390000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    74400000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    74410000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    74420000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    74430000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    74440000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    74450000
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    74460000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    74470000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    74480000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    74490000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    74500000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    74510000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    74520000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    74530000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    74540000
         DC    X'00000000000000000000FFFFFFFFFFFF' F                    74550000
*                                                                       74560000
CONST_CSIFIELD_ML DS 0F                                                 74570000
         DC    CL44' '        CSIFILTK FILTER   KEY                     74580000
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS           74590000
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS            74600000
         DS    0CL16          CSIDTYPD ENTRY TYPES                      74610000
         DC    CL16'                ' CSIDTYPS                          74620000
         DS    0CL4           CSIOPTS  CSI OPTIONS                      74630000
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' ' 74640000
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' ' 74650000
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' ' 74660000
         DC    XL1'00'        CSIRESRV RESERVED                         74670000
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING      74680000
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES       74690000
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME                       74700000
CONST_CSIFIELD_ML_LEN EQU *-CONST_CSIFIELD_ML                           74710000
*                                                                       74720000
CDSNTU_ML                   DC    AL2(DALDSNAM)                         74730000
                            DC    X'0001'                               74740000
                            DC    X'002C'                               74750000
                            DC    CL44' '                               74760000
CDSNTU_ML_L                 EQU   *-CDSNTU_ML                           74770000
*                                                                       74780000
CSTATUSTU_ML                DC    AL2(DALSTATS)                         74790000
                            DC    X'0001'                               74800000
                            DC    X'0001'                               74810000
                            DC    X'08'                                 74820000
CSTATUSTU_ML_L              EQU   *-CSTATUSTU_ML                        74830000
*                                                                       74840000
CRETDDN_ML                  DC    AL2(DALRTDDN)                         74850000
                            DC    X'0001'                               74860000
                            DC    X'0008'                               74870000
                            DC    CL8' '                                74880000
CRETDDN_ML_L                EQU   *-CRETDDN_ML                          74890000
*                                                                       74900000
CDCBPDS_DIR_ML              DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVDX74910000
               =DA,DSORG=PS,RECFM=F,DCBE=CDCBEPDS_DIR_ML                74920000
LEN_DCBPDS_DIR_ML           EQU   *-CDCBPDS_DIR_ML                      74930000
CDCBEPDS_DIR_ML             DCBE  EODAD=0,RMODE31=BUFF                  74940000
LEN_DCBEPDS_DIR_ML          EQU   *-CDCBEPDS_DIR_ML                     74950000
*                                                                       74960000
GET_MEMLIST_DSECT           DSECT                                       74970000
                            DS    18F * My savearea                     74980000
GET_MEMLIST_SAVEAREA2       DS    18F                                   74990000
*                                                                       75000000
DAREAPTR_ML                 DS    A      DATA AREA POINTER (64K)        75010000
*                                                                       75020000
MODRSNRT_ML                 DS    0F                                    75030000
PARMRC_ML                   DS    0CL4                                  75040000
MODID_ML                    DS    CL2    MODULE ID                      75050000
RSNCODE_ML                  DS    CL1    REASON CODE                    75060000
RTNCODE_ML                  DS    CL1    RETURN CODE                    75070000
*                                                                       75080000
CSIFIELD_ML                 DS    0F                                    75090000
                            ORG   *+CSIFIELD_DSECT_SIZ                  75100000
CSIFIELD_ML_LEN             EQU   *-CSIFIELD_ML                         75110000
*                                                                       75120000
PARMLIST_ML                 DS    0F                                    75130000
                            DS    A                                     75140000
                            DS    A                                     75150000
                            DS    A                                     75160000
*                                                                       75170000
                            DS    0F                                    75180000
PDSDIR_EOF_ML               DS    C                                     75190000
*                                                                       75200000
                            DS    0F                                    75210000
DSCBPAR_ML                  DS    4F                                    75220000
OBTAIN_ML                   DS    0F                                    75230000
                            ORG   *+OBTAIN_DSECT_SIZ                    75240000
*                                                                       75250000
DYNALLOC_AREA_ML            DS    0F                                    75260000
                            ORG   *+4                                   75270000
                            ORG   *+(S99RBEND-S99RB)                    75280000
                            ORG   *+12                                  75290000
                            ORG   *+CDSNTU_ML_L+CSTATUSTU_ML_L+CRETDDN_X75300000
               ML_L                                                     75310000
*                                                                       75320000
                            DS    0F                                    75330000
DIRREC_ML                   DS    CL256                                 75340000
*                                                                       75350000
MEM8_ML                     DS    CL8                                   75360000
MEM8FILTER_LEN              DS    F                                     75370004
MEM8FILTER                  DS    CL8                                   75380004
*                                                                       75390000
DAREA_ML                    DS    C                                     75400000
                            ORG   *+1023                                75410000
DAREA_ML_SIZ                EQU   *-DAREA_ML                            75420000
*                                                                       75430000
GET_MEMLIST_DSECT_SIZ       EQU   *-GET_MEMLIST_DSECT                   75440000
*                                                                       75450000
R0       EQU   0                                                        75460000
R1       EQU   1                                                        75470000
R2       EQU   2                                                        75480000
R3       EQU   3                                                        75490000
R4       EQU   4                                                        75500000
R5       EQU   5                                                        75510000
R6       EQU   6                                                        75520000
R7       EQU   7                                                        75530000
R8       EQU   8                                                        75540000
R9       EQU   9                                                        75550000
R10      EQU   10                                                       75560000
R11      EQU   11                                                       75570000
R12      EQU   12                                                       75580000
R13      EQU   13                                                       75590000
R14      EQU   14                                                       75600000
R15      EQU   15                                                       75610000
*                                                                       75620000
         END   LWZMAKE                                                  75630000
