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
*        Clear REXX environment block pointer                           03050001
         MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)                             03060001
*                                                                       03070001
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
*                                                                       06790000
* Local constant pointers to section addresses                          06800000
LWZMAKE_TRACEA              DC    A(LWZMAKE_TRACE)                      06810000
LWZMAKE_RPTA                DC    A(LWZMAKE_RPT)                        06820000
LWZMAKE_SCAN_TOKENA         DC    A(LWZMAKE_SCAN_TOKEN)                 06830000
LWZMAKE_PHASE1A             DC    A(LWZMAKE_PHASE1)                     06840000
LWZMAKE_PHASE2A             DC    A(LWZMAKE_PHASE2)                     06850000
*                                                                       06860000
* Constant list form of LOAD macro                                      06870000
                            DS    0F                                    06880000
LOADL                       LOAD  SF=L                                  06890000
LOAD_SIZ                    EQU   *-LOADL                               06900000
*                                                                       06910000
* Constant list form of LINK macro                                      06920000
                            DS    0F                                    06930000
LINKL                       LINK  SF=L                                  06940000
LINK_SIZ                    EQU   *-LINKL                               06950000
*                                                                       06960000
* Constant list form of OPEN macro                                      06970000
                            DS    0F                                    06980000
OPENL                       OPEN  (,),MODE=31,MF=L                      06990000
OPEN_SIZ                    EQU   *-OPENL                               07000000
*                                                                       07010000
* Constant DCB for LWZMTRC                                              07020000
CDCBTRC                     DCB   DDNAME=LWZMTRC,LRECL=133,MACRF=(PM),RX07030000
               ECFM=FBA,DSORG=PS                                        07040000
LEN_DCBTRC                  EQU   *-CDCBTRC                             07050000
*                                                                       07060000
* Constant DCB for LWZMRPT                                              07070000
CDCBRPT                     DCB   DDNAME=LWZMRPT,LRECL=133,MACRF=(PM),RX07080000
               ECFM=FBA,DSORG=PS                                        07090000
LEN_DCBRPT                  EQU   *-CDCBRPT                             07100000
*                                                                       07110000
* Constant DCB for MAKEFILE                                             07120000
CDCBMKF                     DCB   DDNAME=MAKEFILE,LRECL=80,MACRF=(GM),RX07130000
               ECFM=FB,DSORG=PS,DCBE=CDCBEMKF                           07140000
LEN_DCBMKF                  EQU   *-CDCBMKF                             07150000
*                                                                       07160000
* Constant DCBE for MAKEFILE                                            07170000
CDCBEMKF                    DCBE  EODAD=0,RMODE31=BUFF                  07180000
LEN_DCBEMKF                 EQU   *-CDCBEMKF                            07190000
*                                                                       07200000
* DSECT for SA, main section working storage, which also contains the   07210000
* area for global data. This DSECT is pointed to by R13, global data    07220000
* is pointed to by R9.                                                  07230000
*                                                                       07240000
WORKAREA                    DSECT                                       07250000
                            DS    18F * My savearea                     07260000
PARML_PTR                   DS    A   * Contents of R1 on entry         07270000
*                                                                       07280000
* Working storage list form of LOAD macro                               07290000
                            DS    0F                                    07300000
LOADD                       DS    CL(LOAD_SIZ)                          07310000
*                                                                       07320000
* Flags indicating DCB's open or not                                    07330000
                            DS    0F                                    07340000
TRCOPEN                     DS    C   * LWZMTRC  open flag              07350000
RPTOPEN                     DS    C   * LWZMRPT  open flag              07360000
MKFOPEN                     DS    C   * MAKEFILE open flag              07370000
*                                                                       07380000
* Area for global data, pointed to by R9                                07390000
                            DS    0F                                    07400000
GLOBAL_DATA                 DS    CL(GLOBAL_DATA_SIZ)                   07410000
*                                                                       07420000
WORKAREA_SIZ                EQU   *-WORKAREA                            07430000
*                                                                       07440000
* DSECT for global data pointed to by R9, global vars start with G_     07450000
* for visibility                                                        07460000
*                                                                       07470000
GLOBAL_DATA_DSECT           DSECT                                       07480000
*                                                                       07490000
* LWZMAKE return code                                                   07500000
G_RETCODE                   DS    F                                     07510000
*                                                                       07520000
* Pointers to section address                                           07530000
                            DS    0F                                    07540000
G_LWZMAKE_TRACEA            DS    A                                     07550000
G_LWZMAKE_RPTA              DS    A                                     07560000
*                                                                       07570000
* Working storage list form of OPEN macro                               07580000
                            DS    0F                                    07590000
G_OPEND                     DS    CL(OPEN_SIZ)                          07600000
*                                                                       07610000
* Working storage list form of LINK macro                               07620000
                            DS    0F                                    07630000
G_LINKD                     DS    CL(LINK_SIZ)                          07640000
*                                                                       07650000
* Pointer to 24-bit block of storage for DCB's, DCB_DSECT for overlay   07660000
                            DS    0F                                    07670000
G_DCB_MEM_PTR               DS    A                                     07680000
*                                                                       07690000
* Userid executing current task                                         07700000
G_USERID                    DS    CL8                                   07710000
*                                                                       07720000
* Helper for converting CALL to uppercase                               07730000
G_CALL4                     DS    CL4                                   07740000
*                                                                       07750000
* EOF flag for MAKEFILE                                                 07760000
G_MKFEOF                    DS    C                                     07770000
*                                                                       07780000
* RECIPEPREFIX, initialized to X'05', can be set in MAKEFILE script     07790000
G_RECIPEPREFIX              DS    C                                     07800000
*                                                                       07810000
* LWZMAKE trace level                                                   07820000
G_LWZMAKE_TRACE             DS    C                                     07830000
LWZMAKE_TRACE_NONE          EQU   C'0'                                  07840000
LWZMAKE_TRACE_ERROR         EQU   C'1'                                  07850000
LWZMAKE_TRACE_WARNING       EQU   C'2'                                  07860000
LWZMAKE_TRACE_INFO          EQU   C'3'                                  07870000
LWZMAKE_TRACE_DEBUG         EQU   C'4'                                  07880000
LWZMAKE_TRACE_DEEBUG        EQU   C'6'                                  07890000
LWZMAKE_TRACE_DEEEBUG       EQU   C'8'                                  07900000
*                                                                       07910000
* Trace message number '000' - '999'                                    07920000
G_LWZMTRC_MSGNR             DS    CL3                                   07930000
*                                                                       07940000
* Trace record                                                          07950000
G_LWZMTRC_RECORD            DS    CL133                                 07960000
*                                                                       07970000
* Trace record data pointer and size                                    07980000
                            DS    0F                                    07990000
G_LWZMTRC_DATA_PTR          DS    A                                     08000000
G_LWZMTRC_DATA_SIZ          DS    H                                     08010000
*                                                                       08020000
* Catalog search interface IGGCSI00 external function address           08030000
                            DS    0F                                    08040000
G_IGGCSI00A                 DS    A                                     08050000
*                                                                       08060000
* Pointer and length of member name in MVS data set string              08070000
G_MVSDS_MEMBER_PTR          DS    A                                     08080000
G_MVSDS_MEMBER_LEN          DS    F                                     08090000
*                                                                       08100000
* Binder fast data access external function address                     08110000
                            DS    0F                                    08120000
G_IEWBFDATA                 DS    A                                     08130000
*                                                                       08140000
* REXX execute IRXINIT parameters                                       08150000
G_IRXINIT_PAR7A             DS    10A                                   08160000
G_IRXINIT_FUNCTION          DS    CL8                                   08170000
G_IRXINIT_PARMMOD           DS    CL8                                   08180000
G_IRXINIT_INSTORPARM_PTR    DS    CL4                                   08190000
G_IRXINIT_USRFIELD_PTR      DS    CL4                                   08200000
G_IRXINIT_RESERVED_PTR      DS    CL4                                   08210000
G_IRXINIT_ENVBLOCK_PTR      DS    CL4                                   08220000
G_IRXINIT_REASON            DS    CL4                                   08230000
*                                                                       08240001
* REXX maintain entries host command env table IRXSUBCM parameters      08250001
G_IRXSUBCM_PAR5A            DS    5A                                    08260001
G_IRXSUBCM_FUNCTION         DS    CL8                                   08270001
G_IRXSUBCM_STRING           DS    CL32                                  08280001
G_IRXSUBCM_STRING_LEN       DS    CL4                                   08290001
G_IRXSUBCM_HOSTENV_NAME     DS    CL8                                   08300001
G_IRXSUBCM_ENVBLOCK_PTR     DS    CL4                                   08310001
*                                                                       08320000
* REXX execute ISPEXEC parameters                                       08330000
G_USE_ISPEXEC               DS    C                                     08340001
                            DS    0F                                    08350001
G_ISPEXEC_PAR2A             DS    2A                                    08360000
*                                                                       08370000
* REXX execute IRXEXEC parameters                                       08380000
G_IRXEXEC_PAR10A            DS    10A                                   08390000
G_IRXEXEC_EXECBLK_PTR       DS    CL4                                   08400000
G_IRXEXEC_ARGS_PTR          DS    CL4                                   08410000
G_IRXEXEC_FLAGS             DS    CL4                                   08420000
G_IRXEXEC_INSTBLK_PTR       DS    CL4                                   08430000
G_IRXEXEC_CPPL_PTR          DS    CL4                                   08440000
G_IRXEXEC_EVALBLK_PTR       DS    CL4                                   08450000
G_IRXEXEC_WORKAREA_PTR      DS    CL4                                   08460000
G_IRXEXEC_USRFIELD_PTR      DS    CL4                                   08470000
G_IRXEXEC_ENVBLOCK_PTR      DS    CL4                                   08480000
G_IRXEXEC_REASON_PTR        DS    CL4                                   08490000
*                                                                       08500000
* REXX exec block                                                       08510000
                            DS    0F                                    08520000
G_IRXEXEC_EXECBLK           DS    CL(EXECBLEN)                          08530000
*                                                                       08540000
* REXX exec arguments, 4 byte pointer + 4 byte length + 8 byte X'FF'    08550000
                            DS    0F                                    08560000
G_IRXEXEC_ARGS              DS    CL16                                  08570000
*                                                                       08580000
* REXX evaluation block                                                 08590000
G_IRXEXEC_EVALBLK           DS    0F                                    08600000
                            DS    4F                                    08610000
                            DS    CL256                                 08620000
EVALBLK_SIZ                 EQU   *-G_IRXEXEC_EVALBLK                   08630000
*                                                                       08640000
* REXX execute IRXEXEC reason code                                      08650000
                            DS    0F                                    08660000
G_IRXEXEC_REASON            DS    CL4                                   08670000
*                                                                       08680000
* Starting address of statement pointer block linked list               08690000
* This linked list links 4K blocks, each containing (possibly) 1022     08700000
* pointers to any STMT_*_DSECT area, first pointer is a backward chain, 08710000
* 1024th pointer is a forward chain                                     08720000
                            DS    0F                                    08730000
G_STMT_LIST_PTR             DS    A                                     08740000
*                                                                       08750000
* Pointer to previous statement used to chain stmts in linked list      08760000
G_STMT_SAVE_PTR             DS    A                                     08770000
*                                                                       08780000
* Statement operator ('=' or ':=' in assigment, ':' in rule)            08790000
G_STMT_SAVE_OP              DS    CL2                                   08800000
*                                                                       08810000
* Previous statement type ('A'ssignment, 'R'ule or 'C'all)              08820000
G_PREV_STMT_TYPE            DS    C                                     08830000
*                                                                       08840000
* Switch (Y/N) indicating previous statement was in recipe              08850000
G_PREV_STMT_IN_RECIPE       DS    C                                     08860000
*                                                                       08870000
* Parameters and return pointer for LWZMAKE_ALLOC_STMT section          08880000
G_STMT_ALLOC_LEN            DS    F    * size of memory to allocate     08890000
G_STMT_ALLOC_TYPE           DS    C    * statement type                 08900000
                            DS    CL3  * reserved                       08910000
G_STMT_ALLOC_RETURN_PTR     DS    A    * returned ptr to alloc'd memory 08920000
*                                                                       08930000
* Scanner/tokenizer/parser variables                                    08940000
G_SCAN_CURRLINE             DS    F    * Current line                   08950000
G_SCAN_CURRCOL              DS    F    * Current column within line     08960000
G_SCAN_SPACE_COUNT          DS    F    * Spaces since last token        08970000
G_SAVE_SPACE_COUNT          DS    F    * Place to remember space count  08980000
G_SCAN_CURRCHAR             DS    C    * Scanner current character      08990000
G_SCAN_PEEKCHAR             DS    C    * Scanner next character         09000000
G_SCAN_NEWLINE              DS    C    * Switch ind new line            09010000
G_SCAN_CONTINUED_LINE       DS    C    * Switch ind continued line (\)  09020000
G_SCAN_TOKENTYPE            DS    C    * Token type for G_SCAN_TOKEN    09030000
G_SCAN_TOKENTYPE2           DS    C    * Token type for G_SCAN_TOKEN2   09040000
G_SCAN_TOKENTYPE3           DS    C    * Token type for G_SCAN_TOKEN3   09050000
SCAN_TOKENTYPE_IGNORE       EQU   C'I' * Anything beyond pos 72         09060000
SCAN_TOKENTYPE_COMMENT      EQU   C'#' * Anything after #               09070000
SCAN_TOKENTYPE_NORMAL       EQU   C'A' * Any word pos 1 A-Z a-z         09080000
SCAN_TOKENTYPE_NUMBER       EQU   C'N' * Any work containing only 0-9   09090000
SCAN_TOKENTYPE_OPERATOR     EQU   C'=' * Any operator (:= =)            09100000
SCAN_TOKENTYPE_CONTINUATION EQU   C'\' * Line continuation char \       09110000
SCAN_TOKENTYPE_RULE         EQU   C':' * Rule separator :               09120000
SCAN_TOKENTYPE_SPECIAL      EQU   C'.' * Any special variable           09130000
SCAN_TOKENTYPE_VARIABLE     EQU   C'$' * The word $(                    09140000
SCAN_TOKENTYPE_CLOSEBRACKET EQU   C')' * Closing bracket char )         09150000
SCAN_TOKENTYPE_CALL         EQU   C'C' * The word CALL                  09160000
SCAN_TOKENTYPE_ACRO         EQU   C'@' * The word $@                    09170000
SCAN_TOKENTYPE_PERCENT      EQU   C'%' * The word $%                    09180000
SCAN_TOKENTYPE_RECIPEPREFIX EQU   X'05' * Pos 1 if it's equal to RPREF  09190000
SCAN_TOKENTYPE_COMMA        EQU   C',' * Comma character ,              09200000
G_SCAN_APPEND_TO            DS    C    * Which G_SCAN_TOKEN* to append  09210000
*                                      * to when scanning $() variable  09220000
G_SCAN_CLOSE_BRACKET        DS    C    * Save ) or } to check matching  09230000
*                                      * with ( or {                    09240000
G_SCAN_VAR_PRESERVE_SPACES  DS    C    * Preserve all spaces or just 1  09250000
*                                                                       09260000
* Stack of scan state bytes, highest entry is the last state before     09270000
* the one in G_SCAN_STATE                                               09280000
                            DS    0F                                    09290000
G_SCAN_STATE_STACK          DS    CL128                                 09300000
*                                                                       09310000
* The current scan state byte which is an index on SCAN_STATE_TABLE     09320000
G_SCAN_STATE                DS    C    1                                09330000
SCAN_STATE_NOT_IN_STMT      EQU   X'00'                                 09340000
SCAN_STATE_IN_STMT          EQU   X'01'                                 09350000
SCAN_STATE_IN_ASSIGN        EQU   X'02'                                 09360000
SCAN_STATE_IN_ASSIGN2       EQU   X'03'                                 09370000
SCAN_STATE_IN_VARIABLE      EQU   X'04'                                 09380000
SCAN_STATE_IN_VARIABLER     EQU   X'05'                                 09390000
SCAN_STATE_IN_VARIABLE2     EQU   X'06'                                 09400000
SCAN_STATE_IN_RULE          EQU   X'07'                                 09410000
SCAN_STATE_IN_RULE2         EQU   X'08'                                 09420000
SCAN_STATE_IN_RULE3         EQU   X'09'                                 09430000
SCAN_STATE_IN_CALL          EQU   X'0A'                                 09440000
SCAN_STATE_IN_CALL2         EQU   X'0B'                                 09450000
SCAN_STATE_IN_EXPAND        EQU   X'0C'                                 09460000
SCAN_STATE_IN_PHONY         EQU   X'0D'                                 09470000
SCAN_STATE_IN_PHONY2        EQU   X'0E'                                 09480000
SCAN_STATE_IN_ADDPDSNAME    EQU   X'0F'                                 09490000
SCAN_STATE_IN_ADDPDSNAME2   EQU   X'10'                                 09500000
SCAN_STATE_IN_ADDPDSNAME3   EQU   X'11'                                 09510000
SCAN_STATE_IN_ADDPDSNAME4   EQU   X'12'                                 09520000
SCAN_STATE_IN_MEMBERLIST    EQU   X'13'                                 09530000
SCAN_STATE_IN_MEMBERLIST2   EQU   X'14'                                 09540000
SCAN_STATE_IN_RECIPE        EQU   X'80'                                 09550000
                            DS    C    2                                09560000
*                                                                       09570000
* Current stack size, current scan state in G_SCAN_STATE, last scan     09580000
* state pushed on stack at G_SCAN_STATE_STACK_IDX                       09590000
G_SCAN_STATE_STACK_IDX      DS    C    3                                09600000
                            DS    C    4                                09610000
*                                                                       09620000
* Working field to contain the entry in SCAN_STATE_TABLE indexed by     09630000
* G_SCAN_STATE                                                          09640000
G_SCAN_EXPECTED             DS    CL4                                   09650000
*                                                                       09660000
* Flags for using TM byte by byte in G_SCAN_EXPECTED                    09670000
SCAN_EXPECTED1_EOF          EQU   X'80'                                 09680000
SCAN_EXPECTED1_NEWLINE      EQU   X'40'                                 09690000
SCAN_EXPECTED1_COMMENT      EQU   X'20'                                 09700000
SCAN_EXPECTED1_IGNORE       EQU   X'10'                                 09710000
SCAN_EXPECTED1_NORMAL       EQU   X'08'                                 09720000
SCAN_EXPECTED1_OPENVAR      EQU   X'04'                                 09730000
SCAN_EXPECTED1_OPENBRC      EQU   X'02'                                 09740000
SCAN_EXPECTED1_CLOSEBRC     EQU   X'01'                                 09750000
SCAN_EXPECTED2_NUMBER       EQU   X'80'                                 09760000
SCAN_EXPECTED2_OPERATOR     EQU   X'40'                                 09770000
SCAN_EXPECTED2_RULE         EQU   X'20'                                 09780000
SCAN_EXPECTED2_SPECIAL      EQU   X'10'                                 09790000
SCAN_EXPECTED2_CONTINUA     EQU   X'08'                                 09800000
SCAN_EXPECTED2_CALL         EQU   X'04'                                 09810000
SCAN_EXPECTED2_ACRO         EQU   X'02'                                 09820000
SCAN_EXPECTED2_PERCENT      EQU   X'01'                                 09830000
SCAN_EXPECTED3_RECIPREF     EQU   X'80'                                 09840000
SCAN_EXPECTED3_COMMA        EQU   X'40'                                 09850000
*                                                                       09860000
* Combinations of the flags above, used in SCAN_STATE_TABLE             09870000
SCAN_EXPECTED_NEWSTMT       EQU   B'11111100000101001000000000000000'   09880000
SCAN_EXPECTED_NEWSTMT2      EQU   B'00001100011010001000000000000000'   09890000
SCAN_EXPECTED_ASSIGN        EQU   B'01111111100010111100000000000000'   09900000
SCAN_EXPECTED_ASSIGN2       EQU   B'01111111100010111100000000000000'   09910000
SCAN_EXPECTED_VARIABLE      EQU   B'00001000000010111000000000000000'   09920000
SCAN_EXPECTED_VARIABLER     EQU   B'00001001000010111000000000000000'   09930000
SCAN_EXPECTED_VARIABLE2     EQU   B'00000001000010111000000000000000'   09940000
SCAN_EXPECTED_RULE          EQU   B'00001111001010001000000000000000'   09950000
SCAN_EXPECTED_RULE2         EQU   B'01111111000010111000000000000000'   09960000
SCAN_EXPECTED_RULE3         EQU   B'01111111000010111000000000000000'   09970000
SCAN_EXPECTED_CALL          EQU   B'00001100000010111000000000000000'   09980000
SCAN_EXPECTED_CALL2         EQU   B'01111111100010111100000000000000'   09990000
SCAN_EXPECTED_EXPAND        EQU   B'10001111100000110100000000000000'   10000000
SCAN_EXPECTED_PHONY         EQU   B'00001000000010000000000000000000'   10010000
SCAN_EXPECTED_PHONY2        EQU   B'01110000000010000000000000000000'   10020000
SCAN_EXPECTED_ADDPDSNAME    EQU   B'00001100000010110000000000000000'   10030000
SCAN_EXPECTED_ADDPDSNAME2   EQU   B'00001100000010110100000000000000'   10040000
SCAN_EXPECTED_ADDPDSNAME3   EQU   B'00001100100010110000000000000000'   10050000
SCAN_EXPECTED_ADDPDSNAME4   EQU   B'00001101100010110000000000000000'   10060000
SCAN_EXPECTED_MEMBERLIST    EQU   B'00001100000010100000000000000000'   10070000
SCAN_EXPECTED_MEMBERLIST2   EQU   B'00001101000010100000000000000000'   10080000
SCAN_EXPECTED_IGNORE        EQU   B'01010000000000000000000000000000'   10090000
SCAN_EXPECTED_NEWLINE       EQU   B'01000000000000000000000000000000'   10100000
SCAN_EXPECTED_COMMENT       EQU   B'01110000000000000000000000000000'   10110000
*                                                                       10120000
* Stack of INPUT_DSECT structures, highest stack entry is the one       10130000
* LWZMAKE_SCAN_CHAR reads from. Initial size is 1, entry 0 filled       10140000
* with all zeros, indicating input from MAKEFILE DD                     10150000
                            DS    0F                                    10160000
MAX_SCAN_INPUT_STACK_ENTRY  EQU   20                                    10170000
G_SCAN_INPUT_STACK          DS    CL(MAX_SCAN_INPUT_STACK_ENTRY*INPUT_DX10180000
               SECT_SIZ)                                                10190000
G_SCAN_INPUT_STACK_IDX      DS    C                                     10200000
*                                                                       10210000
* Starting address of binary search tree of variables, each variable    10220000
* addressed with VAR_DSECT                                              10230000
                            DS    0F                                    10240000
G_FIRST_VAR_PTR             DS    A                                     10250000
*                                                                       10260000
* Parameters and return value for LWZMAKE_FINDVAR                       10270000
G_SRCH_VAR_LEN              DS    H                                     10280000
G_SRCH_VAR                  DS    CL72                                  10290000
G_FOUND_VAR_PTR             DS    A                                     10300000
*                                                                       10310000
* Starting address of binary search tree of targets, each target        10320000
* addressed with TARGET_DSECT                                           10330000
                            DS    0F                                    10340000
G_FIRST_TGT_PTR             DS    A                                     10350000
*                                                                       10360000
* Default target to build starting phase 2                              10370000
G_DEFAULT_TARGET            DS    CL72                                  10380000
*                                                                       10390000
* Return value for LWZMAKE_FINDTGT (takes input from G_SCAN_TOKEN)      10400000
G_FOUND_TGT_PTR             DS    A                                     10410000
*                                                                       10420000
* Parameter area ptr + area for first call to LWZMAKE_EXEC_TGT          10430000
G_EXEC_TGT_PAR1A            DS    A                                     10440000
G_EXEC_TGT_PAR1             DS    CL(EXEC_TGT_PAR_LEN)                  10450000
*                                                                       10460000
* Starting address of binary search tree of PHONIES, each PHONY         10470000
* addressed with PHONY_DSECT                                            10480000
                            DS    0F                                    10490000
G_FIRST_PNY_PTR             DS    A                                     10500000
*                                                                       10510000
* Return value for LWZMAKE_FINDPNY (takes input from G_SCAN_TOKEN)      10520000
G_FOUND_PNY_PTR             DS    A                                     10530000
*                                                                       10540000
* Returned altered date+time from LWZMAKE_GET_DATE                      10550000
                            DS    0F                                    10560000
G_SAVE_ALTER_DATE           DS    CL16                                  10570000
G_DSFOUND                   DS    C                                     10580000
*                                                                       10590000
* Current MAKEFILE record being scanned, read by LWZMAKE_SCAN_CHAR      10600000
                            DS    0F                                    10610000
G_MAKEFILE_REC              DS    CL80                                  10620000
*                                                                       10630000
* Progress report current page and line                                 10640000
                            DS    0F                                    10650000
G_LWZMRPT_CURRPAGE          DS    F                                     10660000
G_LWZMRPT_CURRLINE          DS    H                                     10670000
*                                                                       10680000
* Switch indicating whether line n column m should be added to rpt line 10690000
G_LWZMRPT_APND_LC           DS    C                                     10700000
*                                                                       10710000
* Line to write to LWZMRPT next                                         10720000
                            DS    0F                                    10730000
G_LWZMRPT_LINE              DS    CL133                                 10740000
*                                                                       10750000
* Room for formatting data to put in G_LWZMRPT_LINE                     10760000
                            DS    0F                                    10770000
G_LWZMRPT_HELPER            DS    CL80                                  10780000
*                                                                       10790000
LINES_PER_PAGE              EQU   40                                    10800000
*                                                                       10810000
                            DS    0F                                    10820000
G_PAGE_HEADER               DS    CL133                                 10830000
                            ORG   G_PAGE_HEADER+75                      10840000
G_PAGE_HEADER_DAY           DS    CL2                                   10850000
                            DS    C                                     10860000
G_PAGE_HEADER_MONTH         DS    CL2                                   10870000
                            DS    C                                     10880000
G_PAGE_HEADER_YEAR          DS    CL4                                   10890000
                            ORG   G_PAGE_HEADER+94                      10900000
G_PAGE_HEADER_HOUR          DS    CL2                                   10910000
                            DS    C                                     10920000
G_PAGE_HEADER_MINUTE        DS    CL2                                   10930000
                            DS    C                                     10940000
G_PAGE_HEADER_SECOND        DS    CL2                                   10950000
                            ORG   G_PAGE_HEADER+111                     10960000
G_PAGE_HEADER_PAGENR        DS    CL8                                   10970000
                            ORG                                         10980000
*                                                                       10990000
* Generic variables                                                     11000000
                            DS    0F                                    11010000
G_DEC8                      DS    PL8  * for CVB / CVD                  11020000
G_ZONED8                    DS    CL8  * for PACK / UNPK                11030000
                            DS    CL4  * Extra bytes for hex conversion 11040000
G_TIMEDATE                  DS    PL16 * for TIME macro                 11050000
                            DS    0F                                    11060000
G_TIMEDATEZ                 DS    CL32 * for formatting time            11070000
G_HELPER_DATA               DS    CL133 * for formatting anything       11080000
*                                       * mostly trace data             11090000
*                                                                       11100000
* WTOBLOCK for WTO execute form                                         11110000
G_WTOBLOCK                  DS    0F                                    11120000
G_WTOLEN                    DS    H                                     11130000
G_WTOFIL                    DS    H                                     11140000
G_WTOTEXT                   DS    CL133                                 11150000
*                                                                       11160000
* 3 multi-purpose 4K token area's                                       11170000
                            DS    0F                                    11180000
G_SCAN_TOKEN_LEN            DS    F                                     11190000
G_SCAN_TOKEN2_LEN           DS    F                                     11200000
G_SCAN_TOKEN3_LEN           DS    F                                     11210000
G_SCAN_TOKEN_MAXLEN         DS    F                                     11220000
G_SCAN_TOKEN2_MAXLEN        DS    F                                     11230000
G_SCAN_TOKEN3_MAXLEN        DS    F                                     11240000
G_SCAN_TOKENA               DS    A                                     11250000
G_SCAN_TOKEN2A              DS    A                                     11260000
G_SCAN_TOKEN3A              DS    A                                     11270000
*                                                                       11280000
SCAN_TOKEN_MAXLEN           EQU   4096                                  11290000
*                                                                       11300000
GLOBAL_DATA_SIZ             EQU   *-GLOBAL_DATA_DSECT                   11310000
*                                                                       11320000
* DSECT for DCB memory below the line                                   11330000
*                                                                       11340000
DCB_DSECT                   DSECT                                       11350000
*                                                                       11360000
* DCB for LWZMTRC, constant CDCBTRC copied into here                    11370000
DCBTRC                      DS    0F                                    11380000
                            ORG   *+LEN_DCBTRC                          11390000
*                                                                       11400000
* DCB for LWZMRPT, constant CDCBRPT copied into here                    11410000
DCBRPT                      DS    0F                                    11420000
                            ORG   *+LEN_DCBRPT                          11430000
*                                                                       11440000
* DCB for MAKEFILE, constant CDCBMKF copied into here                   11450000
DCBMKF                      DS    0F                                    11460000
                            ORG   *+LEN_DCBMKF                          11470000
*                                                                       11480000
* DCBE for MAKEFILE, constant CDCBEMKF copied into here                 11490000
DCBEMKF                     EQU   *                                     11500000
                            ORG   *+LEN_DCBEMKF                         11510000
*                                                                       11520000
* DCB for any PDS dynamically allocated for reading directory           11530000
DCBPDS_DIR                  DS    0F                                    11540000
                            ORG   *+LEN_DCBPDS_DIR_GD                   11550000
*                                                                       11560000
* DCBE for any PDS dynamically allocated for reading directory          11570000
DCBEPDS_DIR                 EQU   *                                     11580000
                            ORG   *+LEN_DCBEPDS_DIR_GD                  11590000
*                                                                       11600000
* DCB for any PDS dynamically allocated for binder access               11610000
DCBPDS_BDR                  DS    0F                                    11620000
                            ORG   *+LEN_DCBPDS_BDR                      11630000
*                                                                       11640000
DCB_DSECT_SIZ               EQU   *-DCB_DSECT                           11650000
*                                                                       11660000
* Every parsed statement is converted to internal format, being one of  11670000
* the following DSECTs. Each DSECT is dynamically allocated, pointer is 11680000
* stored in the statement pointer block linked list (starts at          11690000
* G_STMT_LIST_PTR). Every statement area starts with a generic part     11700000
* addressed with STMT_DSECT. Every STMT_*_DSECT starts with this        11710000
* generic part, followed by the specifics of the type of statement.     11720000
*                                                                       11730000
STMT_DSECT                  DSECT                                       11740000
STMT_LEN                    DS    F    * length of stmt area (generic + 11750000
*                                      * STMT_*_DSECT part)             11760000
STMT_TYPE                   DS    C    * byte for type of statement     11770000
STMT_TYPE_ASSIGNMENT        EQU   C'A' * assignment, use STMT_A_DSECT   11780000
STMT_TYPE_RULE              EQU   C'R' * rule, use STMT_R_DSECT         11790000
STMT_TYPE_CALL              EQU   C'C' * call, use STMT_C_DSECT         11800000
STMT_TYPE_PHONY             EQU   C'P' * PHONY, use STMT_P_DSECT        11810000
STMT_IN_RECIPE              DS    C    * switch (Y/N) indicating stmt   11820000
*                                      * found in recipe                11830000
                            DS    CL2  * reserved                       11840000
STMT_NEXT_PTR               DS    A    * forward chain to next stmt     11850000
STMT_PREV_PTR               DS    A    * backward chain to prev stmt    11860000
STMT_DSECT_LEN              EQU   *-STMT_DSECT                          11870000
*                                                                       11880000
* Statement area for assignment type statement                          11890000
*                                                                       11900000
STMT_A_DSECT                DSECT                                       11910000
*                                 * generic part of area                11920000
                            DS    CL(STMT_DSECT_LEN)                    11930000
*                                                                       11940000
STMT_A_DESTLEN              DS    H    * length of variable name        11950000
STMT_A_DEST                 DS    CL72 * variable name (destination)    11960000
STMT_A_OPERATOR             DS    CL2  * type of assignment (= or :=)   11970000
STMT_A_SRCLEN               DS    H    * length of value                11980000
STMT_A_SRC                  DS    0C   * value (source) starts here     11990000
*                                      * length can vary                12000000
STMT_A_DSECT_LEN            EQU   *-STMT_A_DSECT                        12010000
*                                                                       12020000
* Statement area for rule type statement                                12030000
*                                                                       12040000
STMT_R_DSECT                DSECT                                       12050000
*                                 * generic part of area                12060000
                            DS    CL(STMT_DSECT_LEN)                    12070000
*                                                                       12080000
* STMT_R_TGT is the starting address of 3 strings: target name,         12090000
* requisite name and requisite for sequence only name. Each string      12100000
* directly follows the previous one, so the lengths are also the        12110000
* offsets to the next string.                                           12120000
STMT_R_TGTLEN               DS    H   * target name length              12130000
STMT_R_REQLEN               DS    H   * requisite name length           12140000
STMT_R_REQOLEN              DS    H   * requisite for sequence only len 12150000
                            DS    CL2 * reserved                        12160000
STMT_R_TGT                  DS    0C  * target name starts here         12170000
STMT_R_DSECT_LEN            EQU   *-STMT_R_DSECT                        12180000
*                                                                       12190000
* Statement area for call type statement                                12200000
*                                                                       12210000
STMT_C_DSECT                DSECT                                       12220000
*                                 * generic part of area                12230000
                            DS    CL(STMT_DSECT_LEN)                    12240000
*                                                                       12250000
* STMT_C_EXEC is the starting address of 2 strings: exec name and       12260000
* parameter string. Parameter string directly follows the exec name,    12270000
* so the exec length is also the offset to the parameter string.        12280000
STMT_C_EXECLEN              DS    H   * exec name length                12290000
STMT_C_PARMLEN              DS    H   * parameters string length        12300000
STMT_C_EXEC                 DS    0C  * exec name starts here           12310000
STMT_C_DSECT_LEN            EQU   *-STMT_C_DSECT                        12320000
*                                                                       12330000
* Statement area for PHONY type statement                               12340000
*                                                                       12350000
STMT_P_DSECT                DSECT                                       12360000
*                                 * generic part of area                12370000
                            DS    CL(STMT_DSECT_LEN)                    12380000
*                                                                       12390000
* STMT_P_PNY is the starting address of a phony target name.            12400000
STMT_P_PNYLEN               DS    H   * PHONY name length               12410000
                            DS    CL2 * reserved                        12420000
STMT_P_PNY                  DS    0C  * PHONY name starts here          12430000
STMT_P_DSECT_LEN            EQU   *-STMT_P_DSECT                        12440000
*                                                                       12450000
* Variable area, first one pointed to by G_FIRST_VAR_PTR, each VARLOW   12460000
* and VARHIGH (possibly) point to variables with a name lower or higher 12470000
*                                                                       12480000
VAR_DSECT                   DSECT                                       12490000
VARLEN                      DS    H    * length of variable name        12500000
VARNAME                     DS    CL72 * variable name                  12510000
VALLEN                      DS    H    * length of variable value       12520000
VALPTR                      DS    A    * pointer to value (getmain'd)   12530000
VARLOW                      DS    A    * pointer to variable with name  12540000
*                                      * lower than this one            12550000
VARHIGH                     DS    A    * pointer to variable with name  12560000
*                                      * higher than this one           12570000
VAR_DSECT_LEN               EQU   *-VAR_DSECT                           12580000
*                                                                       12590000
* Target area, first one pointed to by G_FIRST_TGT_PTR, each TGTLOW     12600000
* and TGTHIGH (possibly) point to target with a name lower or higher    12610000
*                                                                       12620000
TARGET_DSECT                DSECT                                       12630000
TGTLEN                      DS    F    * length of the whole block      12640000
TGTLOW                      DS    A    * pointer to target with name    12650000
*                                      * lower than this one            12660000
TGTHIGH                     DS    A    * pointer to target with name    12670000
*                                      * higher than this one           12680000
TGTSTMT                     DS    A    * pointer to stmt that resulted  12690000
*                                      * in this target                 12700000
TGTNAMELEN                  DS    H    * length of target name          12710000
TGTNAMEMEMLEN               DS    H    * length of target member name   12720000
TGTNAME                     DS    0C   * target name starts here        12730000
TARGET_DSECT_LEN            EQU   *-TARGET_DSECT                        12740000
*                                                                       12750000
* PHONY area, first one pointed to by G_FIRST_PNY_PTR, each PNYLOW      12760000
* and PNYHIGH (possibly) point to PHONY with a name lower or higher     12770000
*                                                                       12780000
PHONY_DSECT                 DSECT                                       12790000
PNYLEN                      DS    F    * length of the whole block      12800000
PNYLOW                      DS    A    * pointer to PHONY with name     12810000
*                                      * lower than this one            12820000
PNYHIGH                     DS    A    * pointer to PHONY with name     12830000
*                                      * higher than this one           12840000
PNYSTMT                     DS    A    * pointer to stmt that resulted  12850000
*                                      * in this PHONY                  12860000
PNYNAMELEN                  DS    H    * length of PHONY name           12870000
PNYNAME                     DS    0C   * PHONY name starts here         12880000
PHONY_DSECT_LEN             EQU   *-PHONY_DSECT                         12890000
*                                                                       12900000
* Input descriptor area, G_SCAN_INPUT_STACK consists of blocks with     12910000
* this layout. A block with all zeros means input is taken from the     12920000
* MAKEFILE DD. The value of a variable is another source of input.      12930000
*                                                                       12940000
INPUT_DSECT                 DSECT                                       12950000
INPUTTYPE                   DS    C    * type of input, X'00' means     12960000
*                                      * input from MAKEFILE DD         12970000
                            DS    C    * reserved                       12980000
*                                                                       12990000
INPUTLEAD                   DS    H    * leading spaces count           13000000
*                                                                       13010000
INPUTPTR                    DS    A    * for type != X'00' ptr to input 13020000
*                                      * string (e.g. variable value)   13030000
INPUTLEN                    DS    H    * length of string pointed to by 13040000
*                                      * INPUTPTR                       13050000
INPUTPOS                    DS    H    * current position in string     13060000
*                                      * pointed to by INPUTPTR         13070000
INPUT_DSECT_SIZ             EQU   *-INPUT_DSECT                         13080000
*                                                                       13090000
* DSECT for IGGCSI00 catalog search interface                           13100000
CSIFIELD_DSECT              DSECT                                       13110000
CSIFILTK                    DS    CL44    FILTER   KEY                  13120000
CSICATNM                    DS    CL44    CATALOG NAME OR BLANKS        13130000
CSIRESNM                    DS    CL44    RESUME NAME OR BLANKS         13140000
CSIDTYPD                    DS    0CL16   ENTRY TYPES                   13150000
CSIDTYPS                    DS    CL16                                  13160000
CSIOPTS                     DS    0CL4    CSI OPTIONS                   13170000
CSICLDI                     DS    CL1     RETURN DATA OR INDEX          13180000
CSIRESUM                    DS    CL1     RESUME FLAG                   13190000
CSIS1CAT                    DS    CL1     SEARCH CATALOG                13200000
CSIRESRV                    DS    XL1     RESERVED                      13210000
CSINUMEN                    DS    H       NUMBER OF ENTRIES FOLLOWING   13220000
CSIENTS                     DS    0CL8    VARIABLE NUMBER OF ENTRIES    13230000
CSIFLDNM                    DS    CL8     FIELD NAME                    13240000
CSIFIELD_DSECT_SIZ          EQU   *-CSIFIELD_DSECT                      13250000
*                                                                       13260000
* DSECT for OBTAIN                                                      13270000
OBTAIN_DSECT                DSECT                                       13280000
                            IECSDSL1 1                                  13290000
OBTAIN_DSECT_SIZ            EQU   *-OBTAIN_DSECT                        13300000
*                                                                       13310000
* DSECT for addressing a DCB                                            13320000
                            DCBD  DSORG=PS,DEVD=DA                      13330000
*                                                                       13340000
* DSECT for addresssing a DCBE                                          13350000
                            IHADCBE                                     13360000
*                                                                       13370000
* The following macro's are all needed to use IAZXJSAB for determining  13380000
* the submitter user id.                                                13390000
                            IHAPSA   DSECT=YES,LIST=YES                 13400000
                            IAZJSAB  DSECT=YES,LIST=NO                  13410000
                            IHAASCB  DSECT=YES,LIST=NO                  13420000
                            IHAASSB  LIST=NO                            13430000
                            IHASTCB  LIST=NO                            13440000
                            IKJTCB   DSECT=YES,LIST=NO                  13450000
*                                                                       13460000
* DSECT for addressing REXX EXEC block                                  13470000
                            IRXEXECB                                    13480000
*                                                                       13490000
* DSECT for addressing REXX EVAL block                                  13500000
                            IRXEVALB                                    13510000
*                                                                       13520000
* Continue with code                                                    13530000
LWZMAKE  CSECT                                                          13540000
*                                                                       13550000
*********************************************************************** 13560000
* Section: LWZMAKE_TRACE                                              * 13570000
* Purpose: This section writes a trace record to the LWZMTRC DD.      * 13580000
*          Mostly invoked using the macro MLWZMTRC at the top of this * 13590000
*          source file.                                               * 13600000
*          On entry, G_LWZMTRC_MSGNR should be filled with a 3 digit  * 13610000
*          message number (e.g. C'001'), optionally G_LWZMTRC_DATA_PTR* 13620000
*          and G_LWZMTRC_DATA_SIZ can also be provided. When DATA_SIZ * 13630000
*          is not zero, whatever DATA_PTR points to is appended to the* 13640000
*          trace record after the last non-space + 1.                 * 13650000
*          R9 should point to global data.                            * 13660000
*********************************************************************** 13670000
LWZMAKE_TRACE MLWZSAVE                                                  13680000
*                                                                       13690000
*        Make sure G_LWZMTRC_MSGNR is composed of 3 digits              13700000
         TRT   G_LWZMTRC_MSGNR,XLATENUM                                 13710000
         BNZ   RET_TRACE          * if not, skip to end                 13720000
*                                                                       13730000
*        Setup R6,R7 and R8 dependent on the hundreds digit             13740000
         SELECT CLI,G_LWZMTRC_MSGNR,EQ                                  13750000
            WHEN C'0'                                                   13760000
               LA    R8,LWZ000    * Start of 000-099 messages           13770000
               LA    R7,LWZ000T   * Table with 100 single byte offsets  13780000
               LA    R6,LWZ000X   * 2x2 byte offset+length table        13790000
            WHEN C'6'                                                   13800000
               LA    R8,LWZ600    * Start of 600-699 messages           13810000
               LA    R7,LWZ600T   * Table with 100 single byte offsets  13820000
               LA    R6,LWZ600X   * 2x2 byte offset+length table        13830000
            OTHRWISE                                                    13840000
               B     RET_TRACE    * if none of the above, skip to end   13850000
         ENDSEL                                                         13860000
*                                                                       13870000
*        Convert last 2 digits of message number to binary in R2        13880000
         MVC   G_ZONED8,=CL8'0'   * initialize to 8 zeros               13890000
         MVC   G_ZONED8+6(2),G_LWZMTRC_MSGNR+1 * put last 2 digits      13900000
*                                              * at the end             13910000
         PACK  G_DEC8,G_ZONED8    * convert to packed decimal           13920000
         CVB   R2,G_DEC8          * convert to binary                   13930000
*                                                                       13940000
*        Use binary value of last 2 digits as offset to LWZn00T         13950000
         AR    R2,R7              * offset LWZn00T by message number    13960000
         CLI   0(R2),X'FF'        * check if constant is present        13970000
         BE    RET_TRACE          * if not, skip to end                 13980000
*                                                                       13990000
*        Use byte in LWZn00T as index to LWZn00X                        14000000
         XR    R3,R3              * Clear R3                            14010000
         IC    R3,0(,R2)          * Put index byte in R3                14020000
         SLL   R3,2               * Multiply by 4                       14030000
         AR    R3,R6              * offset LWZn00X by index             14040000
*                                                                       14050000
*        Use offset in entry of LWZn00X to address message constant     14060000
         XR    R4,R4              * Clear R4                            14070000
         LH    R4,0(,R3)          * Load halfword offset to LWZn00      14080000
         AR    R4,R8              * offset LWZn00                       14090000
*                                                                       14100000
*        Put length in entry of LWZn00X in R5                           14110000
         XR    R5,R5              * Clear R5                            14120000
         LH    R5,2(,R3)          * Load length of message constant     14130000
         BCTR  R5,R0              * Minus 1 for EX of MVC               14140000
*                                                                       14150000
         LA    R1,G_LWZMTRC_RECORD * Put address of trace record in R1  14160000
         MVI   0(R1),C' '         * Initialize trace record to blanks   14170000
         MVC   1(L'G_LWZMTRC_RECORD-1,R1),0(R1)                         14180000
         MVC   1(3,R1),=C'LWZ'    * Put LWZ in pos 2-4                  14190000
         MVC   4(3,R1),G_LWZMTRC_MSGNR * Put message number in pos 5-7  14200000
*                                                                       14210000
*        Suffix message number with severity level                      14220000
         SELECT CLI,G_LWZMTRC_MSGNR,EQ                                  14230000
            WHEN C'0'             * Trace messages LWZ000E-LWZ099E      14240000
               MVI   7(R1),C'E'   * are error messages                  14250000
            WHEN C'6'             * Trace messages LWZ600I-LWZ699I      14260000
               MVI   7(R1),C'I'   * are informatory messages            14270000
         ENDSEL                                                         14280000
*                                                                       14290000
         B     *+10               * Skip MVC constant for EX            14300000
         MVC   9(1,R1),0(R4)      * MVC constant for EX                 14310000
         EX    R5,*-6             * EX previous MVC statement with R5   14320000
*                                                                       14330000
         BAL   R8,TRACE_APPEND    * Perform append of G_LWZMTRC_DATA    14340000
*                                                                       14350000
         L     R5,G_DCB_MEM_PTR   * Get DCB memory pointer              14360000
         LA    R5,DCBTRC-DCB_DSECT(,R5) * Get addr of LWZMTRC DCB       14370000
         PUT   (R5),G_LWZMTRC_RECORD * Write a trace record to LWZMTRC  14380000
*                                                                       14390000
RET_TRACE EQU  *                                                        14400000
         MLWZTERM                 * Return back to caller               14410000
*                                                                       14420000
* Append trace data to trace record after last non space + 1.           14430000
* Trace data is optionally pointed to by G_LWZMTRC_DATA_PTR, but only   14440000
* if length set in G_LWZMTRC_DATA_SIZ is non zero.                      14450000
*                                                                       14460000
TRACE_APPEND EQU *                                                      14470000
         CLC   G_LWZMTRC_DATA_SIZ,=H'0' * Check for zero length         14480000
         BE    TRACE_APPEND_RET   * If zero, skip append routine        14490000
*                                                                       14500000
*        Trim trailing spaces in trace record                           14510000
         LA    R6,G_LWZMTRC_RECORD * Point R6 to trace record           14520000
         LA    R5,132(,R6)        * Point R5 to last byte of record     14530000
         LA    R4,132             * Put trace record length - 1 in R4   14540000
TRACE_APPEND_TRIM EQU *                                                 14550000
         CLI   0(R5),C' '         * Check for space                     14560000
         BNE   TRACE_APPEND_TRIM_DONE * Non space means done trimming   14570000
         BCTR  R5,R0              * R5 = R5 - 1                         14580000
         BCT   R4,TRACE_APPEND_TRIM * R4 = R4 - 1 until R4 = 0          14590000
TRACE_APPEND_TRIM_DONE EQU *                                            14600000
         LA    R5,2(,R5)          * Go forward to last space + 1        14610000
         LA    R4,2(,R4)          * R4 = R4 + 2                         14620000
         C     R4,=F'133'         * Check if any space left in record   14630000
         BNL   TRACE_APPEND_RET   * If not, skip to end of routine      14640000
         LA    R3,133             * Put length of trace record in R3    14650000
         SR    R3,R4              * Subtract length of text + 2         14660000
         CH    R3,G_LWZMTRC_DATA_SIZ * Check if trace data length       14670000
         IF (H) THEN              * is less than remaining room in rcd  14680000
            LH    R3,G_LWZMTRC_DATA_SIZ * If so, replace with trace     14690000
         ENDIF                    * data length                         14700000
         BCTR  R3,R0              * Minus 1 for EX of MVC               14710000
         L     R2,G_LWZMTRC_DATA_PTR * Put trace data pointer in R2     14720000
         B     *+10               * Skip MVC constant for EX            14730000
         MVC   0(1,R5),0(R2)      * MVC constant for EX                 14740000
         EX    R3,*-6             * EX previous MVC statement with R3   14750000
*                                                                       14760000
TRACE_APPEND_RET EQU *                                                  14770000
         BR    R8                 * Return                              14780000
*                                                                       14790000
         LTORG                                                          14800000
*                                                                       14810000
* Translate table to check for digits C'0' - C'9'                       14820000
XLATENUM DS    0F                                                       14830000
         DC    256X'FF'                                                 14840000
         ORG   XLATENUM+C'0'                                            14850000
         DC    10X'00'                                                  14860000
         ORG                                                            14870000
*                                                                       14880000
* The following macro is used to generate constant tables used for      14890000
* addressing messages. It's used for a range of 100 message numbers,    14900000
* e.g. from LWZ000E to LWZ099E, and it's only used in this section.     14910000
* It expects a series of message constants to be defined, each one      14920000
* directly following the other, starting with a label like LWZ000.      14930000
* It goes through 100 message numbers in three passes.                  14940000
* Pass 1: populate the macro's local character variable array MSGS      14950000
* Pass 2: generate a table LWZn00T of 100 single byte entries, one for  14960000
*         each message number. A message number for which there's no    14970000
*         constant defined in the assembler source, a byte X'FF' is     14980000
*         generated, for message numbers with a constant defined a      14990000
*         byte with a sequence number is generated. That sequence       15000000
*         number is used as an index to the table generated in pass 3.  15010000
* Pass 3: generate a table LWZn00X with an entry for each defined       15020000
*         message. Each entry is 2x2 bytes, the first 2 are an offset   15030000
*         of the message constant to the starting label, the second 2   15040000
*         are the message constant length.                              15050000
*                                                                       15060000
                  MACRO                                                 15070000
.* MSGPREF   = first 4 characers of messages, e.g. LWZ0                 15080000
.* MSGSUFFIX = last character of message indicating severy level I/E    15090000
                  MTRCNTRS &MSGPREF=,&MSGSUFFIX=                        15100000
                  LCLA  &COUNTER   * for looping 100 times              15110000
                  LCLC  &MSGNR     * complete msg const name LWZnnna    15120000
                  LCLC  &MSGS(100) * table with 100 index values or FF  15130000
                  LCLA  &INDEX     * current index                      15140000
.*                                                                      15150000
.* start loop with 0                                                    15160000
&COUNTER          SETA  0                                               15170000
.* stop looping when counter > 99                                       15180000
.CHECK_LOOP1      AIF   ('&COUNTER' GT '99').STOP_LOOP1                 15190000
.* construct complete message constant name LWZnnna                     15200000
.* if counter is single digit, add a '0'                                15210000
                  AIF   (K'&COUNTER EQ 2).DOUBLE_DIGIT                  15220000
&MSGNR            SETC  '&MSGPREF'.'0'.'&COUNTER'.'&MSGSUFFIX'          15230000
                  AGO   .MSGNR_SET                                      15240000
.DOUBLE_DIGIT     ANOP                                                  15250000
&MSGNR            SETC  '&MSGPREF'.'&COUNTER'.'&MSGSUFFIX'              15260000
.MSGNR_SET        ANOP                                                  15270000
.* preset current message entry in table to empty                       15280000
&MSGS(&COUNTER+1) SETC ''                                               15290000
.* if assembler source contains defined constant with MSGNR as name     15300000
                  AIF   (NOT D'&MSGNR).INCREMENT_LOOP1                  15310000
.* put message name in table entry                                      15320000
&MSGS(&COUNTER+1) SETC '&MSGNR'                                         15330000
.* increase counter and loop around                                     15340000
.INCREMENT_LOOP1  ANOP                                                  15350000
&COUNTER          SETA  &COUNTER+1   * increase counter                 15360000
                  AGO   .CHECK_LOOP1 * and loop around                  15370000
.* done with loop 1                                                     15380000
.STOP_LOOP1       ANOP                                                  15390000
.*                                                                      15400000
.* define start of table with single byte offsets                       15410000
&MSGPREF.00T      DS    0F                                              15420000
.* initialize index (sequence number)                                   15430000
&INDEX            SETA  0                                               15440000
.* start loop with 0                                                    15450000
&COUNTER          SETA  0                                               15460000
.* stop looping when counter > 99                                       15470000
.CHECK_LOOP2      AIF   ('&COUNTER' GT '99').STOP_LOOP2                 15480000
.* if there was a constant defined for this message, the entry in       15490000
.* MSGS is non blank                                                    15500000
                  AIF   ('&MSGS(&COUNTER+1)' EQ '').EMPTY_MSG           15510000
.* in that case define a byte with the index in LWZn00T table           15520000
                  DC    AL1(&INDEX)                                     15530000
&INDEX            SETA  &INDEX+1  * and increase the index              15540000
                  AGO   .INCREMENT_LOOP2                                15550000
.* when entry in MSGS is blank define a byte X'FF' in LWZn00T table     15560000
.EMPTY_MSG        ANOP                                                  15570000
                  DC    X'FF'                                           15580000
.* increase counter and loop around                                     15590000
.INCREMENT_LOOP2  ANOP                                                  15600000
&COUNTER          SETA  &COUNTER+1   * increase counter                 15610000
                  AGO   .CHECK_LOOP2 * and loop around                  15620000
.* done with loop 2                                                     15630000
.STOP_LOOP2       ANOP                                                  15640000
.*                                                                      15650000
.* define start of table with 2x2 byte (offset+length) entries          15660000
&MSGPREF.00X      DS    0F                                              15670000
.* start loop with 0                                                    15680000
&COUNTER          SETA  0                                               15690000
.* stop looping when counter > 99                                       15700000
.CHECK_LOOP3      AIF   ('&COUNTER' GT '99').STOP_LOOP3                 15710000
.* if there was a constant defined for this message, the entry in       15720000
.* MSGS is non blank                                                    15730000
                  AIF   ('&MSGS(&COUNTER+1)' EQ '').INCREMENT_LOOP3     15740000
.* if so, copy message constant name to MSGNR                           15750000
&MSGNR            SETC  '&MSGS(&COUNTER+1)'                             15760000
.* and define an entry in LWZn00X table                                 15770000
                  DC    AL2(&MSGNR-&MSGPREF.00),AL2(L'&MSGNR)           15780000
.* increase counter and loop around                                     15790000
.INCREMENT_LOOP3  ANOP                                                  15800000
&COUNTER          SETA  &COUNTER+1   * increase counter                 15810000
                  AGO   .CHECK_LOOP3 * and loop around                  15820000
.* done with loop 3                                                     15830000
.STOP_LOOP3       ANOP                                                  15840000
.* done with macro                                                      15850000
                  MEND                                                  15860000
*                                                                       15870000
* Messages LWZ000E-LWZ099E                                              15880000
*                                                                       15890000
LWZ000   DS    0F                                                       15900000
LWZ001E  DC    C'LWZMRPT OPEN FAILED'                                   15910000
LWZ002E  DC    C'MAKEFILE OPEN FAILED'                                  15920000
LWZ003E  DC    C'PARSE ERROR'                                           15930000
LWZ010E  DC    C'BINDER ERROR'                                          15940000
*                                                                       15950000
* Generate LWZ000T and LWZ000X                                          15960000
*                                                                       15970000
         MTRCNTRS MSGPREF=LWZ0,MSGSUFFIX=E                              15980000
*                                                                       15990000
* Messages LWZ600I-LWZ699I                                              16000000
*                                                                       16010000
LWZ600   DS    0F                                                       16020000
LWZ601I  DC    C'LWZMAKE TRACE STARTED'                                 16030000
LWZ602I  DC    C'DCB OPENED'                                            16040000
LWZ603I  DC    C'DCB CLOSED'                                            16050000
LWZ604I  DC    C'SECTION STARTED'                                       16060000
LWZ605I  DC    C'SECTION ENDED'                                         16070000
LWZ606I  DC    C'PARAMETER RECEIVED'                                    16080000
LWZ608I  DC    C'START PARSE STATEMENT TYPE'                            16090000
LWZ609I  DC    C'FINISH PARSE STATEMENT'                                16100000
LWZ610I  DC    C'PARSED TOKEN'                                          16110000
LWZ611I  DC    C'PARSED CHAR '                                          16120000
LWZ612I  DC    C'START PARSE TOKEN TYPE'                                16130000
LWZ640I  DC    C'STATEMENT BLOCK ALLOCATE'                              16140000
LWZ641I  DC    C'STATEMENT BLOCK FREE'                                  16150000
LWZ642I  DC    C'VARIABLE BLOCK ALLOCATE'                               16160000
LWZ643I  DC    C'VARIABLE BLOCK FREE'                                   16170000
LWZ699I  DC    C'LWZMAKE TRACE ENDED'                                   16180000
*                                                                       16190000
* Generate LWZ600T and LWZ600X                                          16200000
*                                                                       16210000
         MTRCNTRS MSGPREF=LWZ6,MSGSUFFIX=I                              16220000
*                                                                       16230000
*********************************************************************** 16240000
* Section: LWZMAKE_RPT                                                * 16250000
* Purpose: This section writes a report line to the LWZMRPT DD.       * 16260000
*          Mostly invoked using the macro MLWZMRPT at the top of this * 16270000
*          source file.                                               * 16280000
*          On entry, G_LWZMRPT_LINE contains the line to be written.  * 16290000
*          If G_LWZMRPT_APND_LC is set to 'Y', G_SCAN_CURRLINE and    * 16300000
*          G_SCAN_CURRCOL are appended to the report line.            * 16310000
*          R9 should point to global data.                            * 16320000
*********************************************************************** 16330000
LWZMAKE_RPT MLWZSAVE                                                    16340000
*                                                                       16350000
*        If G_LWZMRPT_APND_LC = 'Y', append line and column nr to line  16360000
         IF (CLI,G_LWZMRPT_APND_LC,EQ,C'Y') THEN                        16370000
            BAL   R8,RPT_APPEND_LC  * Perform append line and column nr 16380000
            MVI   G_LWZMRPT_APND_LC,C'N' * Reset switch to 'N'          16390000
         ENDIF                                                          16400000
*                                                                       16410000
WRITE    EQU   *                                                        16420000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              16430000
         LA    R2,DCBRPT-DCB_DSECT(,R14) * Get addr of LWZMRPT DCB      16440000
*                                                                       16450000
         XR    R3,R3              * Clear R3                            16460000
         LH    R3,G_LWZMRPT_CURRLINE * Load current line number         16470000
         IF (CLI,G_LWZMRPT_LINE,EQ,C' ') THEN * Check ASA ' ' char      16480000
            LA    R3,1(,R3)       * If found, advance 1 line number     16490000
            B     CHECK_CURRLINE  * Skip other ASA checks               16500000
         ENDIF                                                          16510000
         IF (CLI,G_LWZMRPT_LINE,EQ,C'0') THEN * Check ASA '0' char      16520000
            LA    R3,2(,R3)       * If found, advance 2 line numbers    16530000
            B     CHECK_CURRLINE  * Skip other ASA checks               16540000
         ENDIF                                                          16550000
         IF (CLI,G_LWZMRPT_LINE,EQ,C'-') THEN * Check ASA '-' char      16560000
            LA    R3,3(,R3)       * If found, advance 3 line numbers    16570000
            B     CHECK_CURRLINE  * Skip other ASA checks               16580000
         ENDIF                                                          16590000
         CLI   G_LWZMRPT_LINE,C'1' * Check ASA '1' char                 16600000
         BE    PAGE_SKIP          * If found, jump to page skip         16610000
CHECK_CURRLINE EQU *                                                    16620000
         STH   R3,G_LWZMRPT_CURRLINE * Store the new line number        16630000
         CH    R3,=AL2(LINES_PER_PAGE) * Check if we crossed page bndry 16640000
         BNH   NO_PAGE_SKIP       * If not, don't do page skip          16650000
PAGE_SKIP DS   0H                                                       16660000
         L     R4,G_LWZMRPT_CURRPAGE * Get current page number          16670000
         LA    R4,1(,R4)          * page number = page number + 1       16680000
         ST    R4,G_LWZMRPT_CURRPAGE * and put it back                  16690000
         CVD   R4,G_DEC8          * convert to packed decimal           16700000
         UNPK  G_ZONED8,G_DEC8    * convert to zoned                    16710000
         OI    G_ZONED8+7,X'F0'   * get rid of sign nibble              16720000
         MVC   G_PAGE_HEADER_PAGENR,G_ZONED8 * put page nr in header    16730000
*        Get the current date+time                                      16740000
         TIME  DEC,G_TIMEDATE,ZONE=LT,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD  16750000
*        Convert time part to zoned                                     16760000
         UNPK  G_TIMEDATEZ+10(10),G_TIMEDATE(5)                         16770000
*        Convert date part to zoned                                     16780000
         UNPK  G_TIMEDATEZ(10),G_TIMEDATE+8(5)                          16790000
*        Put date and time in header                                    16800000
         MVC   G_PAGE_HEADER_DAY,G_TIMEDATEZ+7                          16810000
         MVC   G_PAGE_HEADER_MONTH,G_TIMEDATEZ+5                        16820000
         MVC   G_PAGE_HEADER_YEAR,G_TIMEDATEZ+1                         16830000
         MVC   G_PAGE_HEADER_HOUR,G_TIMEDATEZ+11                        16840000
         MVC   G_PAGE_HEADER_MINUTE,G_TIMEDATEZ+13                      16850000
         MVC   G_PAGE_HEADER_SECOND,G_TIMEDATEZ+15                      16860000
         PUT   (R2),G_PAGE_HEADER * Write a report line to LWZMRPT      16870000
         MVI   G_LWZMRPT_LINE,C'0' * Overwrite ASA char, skip 2 lines   16880000
         LH    R3,=H'1'           * Set R3 to 1                         16890000
         STH   R3,G_LWZMRPT_CURRLINE * and store it as current line nr  16900000
         B     WRITE              * Jump back to write the actual line  16910000
NO_PAGE_SKIP DS 0H                                                      16920000
         PUT   (R2),G_LWZMRPT_LINE * Write a report line to LWZMRPT     16930000
*                                                                       16940000
RET_RPT  EQU   *                                                        16950000
         MLWZTERM                 * Return back to caller               16960000
*                                                                       16970000
* Append ' at line x column y' to print line, only performed when       16980000
* G_LWZMRPT_APND_LC is set to 'Y'                                       16990000
*                                                                       17000000
RPT_APPEND_LC EQU *                                                     17010000
*        Initialize helper var                                          17020000
         MVC   G_LWZMRPT_HELPER,=CL80' at line'                         17030000
         LA    R7,G_LWZMRPT_HELPER+9 * Point R7 to where line nr starts 17040000
*                                                                       17050000
         L     R15,G_SCAN_CURRLINE * Get the current line number        17060000
         CVD   R15,G_DEC8         * Convert it to packed decimal        17070000
         UNPK  G_ZONED8,G_DEC8    * Convert it to zoned                 17080000
         OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble              17090000
*                                                                       17100000
*        Left trim the line number of leading zeros                     17110000
         LA    R2,G_ZONED8        * Point R2 to line number             17120000
         LA    R3,L'G_ZONED8-1    * Put byte counter - 1 in R3, so      17130000
*                                 * we're always left with min. 1 byte  17140000
*                                 * and R3 is ready to use with EX      17150000
RPT_TRIM_NR1 EQU *                                                      17160000
         CLI   0(R2),C'0'         * Is this a zero?                     17170000
         BNE   RPT_TRIM_NR1_END   * Nope, stop trimming                 17180000
         LA    R2,1(,R2)          * R2 = R2 + 1                         17190000
         BCT   R3,RPT_TRIM_NR1    * R3 = R3 - 1 until R3 = 0            17200000
RPT_TRIM_NR1_END EQU *                                                  17210000
         B     *+10               * Skip MVC constant for EX            17220000
         MVC   0(1,R7),0(R2)      * MVC constant for EX                 17230000
         EX    R3,*-6             * EX previous MVC statement with R3   17240000
         LA    R3,1(,R3)          * R3 = R3 + 1                         17250000
         AR    R7,R3              * Advance R7 past line number         17260000
*                                                                       17270000
         MVC   0(8,R7),=C' column ' * Append constant                   17280000
         LA    R7,8(,R7)          * Advance R7 to point after constant  17290000
*                                                                       17300000
         L     R15,G_SCAN_CURRCOL * Get the current column number       17310000
         CVD   R15,G_DEC8         * Convert it to packed decimal        17320000
         UNPK  G_ZONED8,G_DEC8    * Convert it to zoned                 17330000
         OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble              17340000
*                                                                       17350000
*        Left trim the column number of leading zeros                   17360000
         LA    R2,G_ZONED8        * Point R2 to column number           17370000
         LA    R3,L'G_ZONED8-1    * Put byte counter - 1 in R3, so      17380000
*                                 * we're always left with min. 1 byte  17390000
*                                 * and R3 is ready to use with EX      17400000
RPT_TRIM_NR2 EQU *                                                      17410000
         CLI   0(R2),C'0'         * Is this a zero?                     17420000
         BNE   RPT_TRIM_NR2_END   * Nope, stop trimming                 17430000
         LA    R2,1(,R2)          * R2 = R2 + 1                         17440000
         BCT   R3,RPT_TRIM_NR2    * R3 = R3 - 1 until R3 = 0            17450000
RPT_TRIM_NR2_END EQU *                                                  17460000
         B     *+10               * Skip MVC constant for EX            17470000
         MVC   0(1,R7),0(R2)      * MVC constant for EX                 17480000
         EX    R3,*-6             * EX previous MVC statement with R3   17490000
         LA    R3,1(,R3)          * R3 = R3 + 1                         17500000
         AR    R7,R3              * Advance R7 past column number       17510000
*                                                                       17520000
*        Calculate actual length of helper var                          17530000
         LA    R6,G_LWZMRPT_HELPER * Point R6 to helper var             17540000
         SR    R7,R6              * Subtract start from R7              17550000
*                                                                       17560000
*        Trim the report line of trailing spaces                        17570000
         LA    R2,G_LWZMRPT_LINE+L'G_LWZMRPT_LINE-1 * Point R2 to last  17580000
*                                 * byte of report line                 17590000
         LA    R3,L'G_LWZMRPT_LINE-1 * R3 = length of line - 1, so      17600000
*                                 * we're always left with min. 1 byte  17610000
RPT_TRIM_LINE EQU *                                                     17620000
         CLI   0(R2),C' '         * Is this a space?                    17630000
         BNE   RPT_TRIM_LINE_DONE * Nope, stop trimming                 17640000
         BCTR  R2,R0              * R2 = R2 - 1                         17650000
         BCT   R3,RPT_TRIM_LINE   * R3 = R3 - 1 until R3 = 0            17660000
RPT_TRIM_LINE_DONE EQU *                                                17670000
         LA    R2,1(,R2)          * Point R2 past last non space        17680000
         LA    R5,G_LWZMRPT_LINE+L'G_LWZMRPT_LINE * Point R5 past end   17690000
*                                 * of report line                      17700000
         SR    R5,R2              * Calculate room left                 17710000
         BO    RPT_APPEND_LC_END  * No room left, skip rest of append   17720000
         CR    R5,R7              * Check if room left is more than     17730000
         IF (H) THEN              * the room needed for append string   17740000
            LR    R5,R7           * If so, use length of append string  17750000
         ENDIF                                                          17760000
         BCTR  R5,R0              * R5 = R5 - 1 because of EX           17770000
         B     *+10               * Skip MVC constant for EX            17780000
         MVC   0(1,R2),G_LWZMRPT_HELPER * MVC constant for EX           17790000
         EX    R5,*-6             * EX previous MVC statement with R5   17800000
*                                                                       17810000
RPT_APPEND_LC_END EQU *                                                 17820000
         BR    R8                 * Return                              17830000
*                                                                       17840000
         LTORG                                                          17850000
*                                                                       17860000
*********************************************************************** 17870000
* Section: LWZMAKE_APPEND_TOKEN                                       * 17880000
* Purpose: This section appends token 1 to either token 2 or 3 and    * 17890000
*          takes care of allocating a larger memory block if there's  * 17900000
*          not enough room.                                           * 17910000
*          R9 should point to global data.                            * 17920000
*********************************************************************** 17930000
LWZMAKE_APPEND_TOKEN MLWZSAVE                                           17940000
*                                                                       17950000
*        If APPEND_TO != X'00' then skip to end                         17960000
         CLI   G_SCAN_APPEND_TO,X'00'                                   17970000
         BE    APPEND_RET                                               17980000
*                                                                       17990000
*        If token 1 length = 0 then skip to end                         18000000
         LT    R2,G_SCAN_TOKEN_LEN                                      18010000
         BZ    APPEND_RET                                               18020000
*                                                                       18030000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'01') THEN * Append to token 2    18040000
            L     R2,G_SCAN_TOKEN2_LEN * Get length of token 2          18050000
            A     R2,G_SCAN_SPACE_COUNT * Add number of spaces          18060000
            A     R2,G_SCAN_TOKEN_LEN  * Add length of token 1          18070000
            C     R2,G_SCAN_TOKEN2_MAXLEN * Will it fit?                18080000
            IF (H) THEN                                                 18090000
               L     R3,G_SCAN_TOKEN2_MAXLEN * Get current max length   18100000
               LR    R6,R3            * Save it for storage release     18110000
               SLL   R3,1             * Multiply max length by 2        18120000
               ST    R3,G_SCAN_TOKEN2_MAXLEN * Make it new max length   18130000
               STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block     18140000
               LR    R0,R1            * Have R0 point to new block      18150000
               L     R1,G_SCAN_TOKEN2_LEN * Get length of token 2       18160000
               L     R2,G_SCAN_TOKEN2A * Have R2 point to old block     18170000
               LR    R5,R2            * Save it for storage release     18180000
               LR    R3,R1            * Make sure no cropping/filling   18190000
               ST    R0,G_SCAN_TOKEN2A * Save ptr to new block          18200000
               MVCL  R0,R2            * Copy old to new block           18210000
               STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)                    18220000
            ENDIF                                                       18230000
            L     R2,G_SCAN_TOKEN2A    * Point R2 to token 2            18240000
            A     R2,G_SCAN_TOKEN2_LEN * Add length of token 2          18250000
            L     R3,G_SCAN_TOKEN2_LEN * Get length of token 2          18260000
            A     R3,G_SCAN_SPACE_COUNT * Add number of spaces          18270000
            A     R3,G_SCAN_TOKEN_LEN  * Add length of token 1          18280000
            ST    R3,G_SCAN_TOKEN2_LEN * Put it back as new len         18290000
         ELSE                                                           18300000
            IF (CLI,G_SCAN_APPEND_TO,EQ,X'02') THEN * Append token 3    18310000
               L     R2,G_SCAN_TOKEN3_LEN * Get length of token 3       18320000
               A     R2,G_SCAN_SPACE_COUNT * Add number of spaces       18330000
               A     R2,G_SCAN_TOKEN_LEN  * Add length of token 1       18340000
               C     R2,G_SCAN_TOKEN3_MAXLEN * Will it fit?             18350000
               IF (H) THEN                                              18360000
                  L     R3,G_SCAN_TOKEN3_MAXLEN * Get current max len   18370000
                  LR    R6,R3         * Save it for storage release     18380000
                  SLL   R3,1          * Multiply max length by 2        18390000
                  ST    R3,G_SCAN_TOKEN3_MAXLEN * Make it new max len   18400000
                  STORAGE OBTAIN,LENGTH=(R3) * Allocate a memory block  18410000
                  LR    R0,R1         * Have R0 point to new block      18420000
                  L     R1,G_SCAN_TOKEN3_LEN * Get length of token 3    18430000
                  L     R2,G_SCAN_TOKEN3A * Have R2 point to old block  18440000
                  LR    R5,R2         * Save it for storage release     18450000
                  LR    R3,R1         * Make sure no cropping/filling   18460000
                  ST    R0,G_SCAN_TOKEN3A * Save ptr to new block       18470000
                  MVCL  R0,R2            * Copy old to new block        18480000
                  STORAGE RELEASE,LENGTH=(R6),ADDR=(R5)                 18490000
               ENDIF                                                    18500000
               L     R2,G_SCAN_TOKEN3A    * Point R0 to token 3         18510000
               A     R2,G_SCAN_TOKEN3_LEN * Add length of token 3       18520000
               L     R3,G_SCAN_TOKEN3_LEN * Get length of token 3       18530000
               A     R3,G_SCAN_SPACE_COUNT * Add number of spaces       18540000
               A     R3,G_SCAN_TOKEN_LEN  * Add length of token 1       18550000
               ST    R3,G_SCAN_TOKEN3_LEN * Put it back as new len      18560000
            ENDIF                                                       18570000
         ENDIF                                                          18580000
         LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?              18590000
         IF (NZ) THEN               * Yep...                            18600000
            MVI   0(R2),C' '        * Fill in first space               18610000
            L     R1,G_SCAN_SPACE_COUNT * Put number of spaces in R1    18620000
            S     R1,=F'2'          * Minus 1 for first space and minus 18630000
*                                   * another one for EX                18640000
            IF CC=10 THEN           * If R1 >= 0                        18650000
               B     *+10           * Skip MVC constant for EX          18660000
               MVC   1(1,R2),0(R2)  * MVC constant for EX               18670000
               EX    R1,*-6         * EX previous MVC statement with R1 18680000
            ENDIF                                                       18690000
            A     R2,G_SCAN_SPACE_COUNT                                 18700000
         ENDIF                                                          18710000
         L     R4,G_SCAN_TOKENA * Point R2 to token 1                   18720000
         L     R5,G_SCAN_TOKEN_LEN * Get length of token 1              18730000
         LR    R3,R5           * Make sure no cropping/filling          18740000
         MVCL  R2,R4           * Append to either token 2 or 3          18750000
*                                                                       18760000
APPEND_RET EQU   *                                                      18770000
         MLWZTERM                 * Return back to caller               18780000
*                                                                       18790000
         LTORG                                                          18800000
*                                                                       18810000
*********************************************************************** 18820000
* Section: LWZMAKE_PHASE1                                             * 18830000
* Purpose: This section performs phase 1 of executing a makefile.     * 18840000
*          During this phase the makefile source is parsed, a linked  * 18850000
*          list of statements is created and a binary search tree of  * 18860000
*          variables.                                                 * 18870000
*          Parsing is done by invoking section LWZMAKE_SCAN_STMT      * 18880000
*          statement by statement. It invokes section                 * 18890000
*          LWZMAKE_SCAN_TOKEN which in turn invokes LWZMAKE_SCAN_CHAR.* 18900000
*          R9 should point to global data.                            * 18910000
*********************************************************************** 18920000
LWZMAKE_PHASE1 MLWZSAVE                                                 18930000
*        Trace record to start section                                  18940000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX18950000
               KE_PHASE1'                                               18960000
*                                                                       18970000
*        Write report line to start parsing                             18980000
         MLWZMRPT RPTLINE=CL133' Phase 1 parsing .....'                 18990000
*                                                                       19000000
NEXTSTMT L     R15,LWZMAKE_SCAN_STMTA_PHASE1 * Get address of scan stmt 19010000
*                                            * section                  19020000
         BASR  R14,R15            * Link to scan stmt section           19030000
*                                                                       19040000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 19050000
         BNE   BREAK_STMT_LOOP    * Yes, stop looping                   19060000
         CLI   G_MKFEOF,C'Y'      * Are we at the end of makefile?      19070000
         BE    BREAK_STMT_LOOP    * Yes, stop looping                   19080000
         B     NEXTSTMT           * In all other cases loop around      19090000
*                                                                       19100000
BREAK_STMT_LOOP EQU *                                                   19110000
*                                                                       19120000
PHASE1_RET EQU *                                                        19130000
         MLWZTERM                 * Return back to caller               19140000
*                                                                       19150000
         LTORG                                                          19160000
*                                                                       19170000
* Local constant pointers to section addresses                          19180000
LWZMAKE_SCAN_STMTA_PHASE1   DC    A(LWZMAKE_SCAN_STMT)                  19190000
*                                                                       19200000
*********************************************************************** 19210000
* Section: LWZMAKE_PHASE2                                             * 19220000
* Purpose: This section performs phase 2 of executing a makefile,     * 19230000
*          starting with the executing of the first target and        * 19240000
*          recursively going through any prerequisite targets.        * 19250000
*          R9 should point to global data.                            * 19260000
*********************************************************************** 19270000
LWZMAKE_PHASE2 MLWZSAVE                                                 19280000
*        Trace record to start section                                  19290000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX19300000
               KE_PHASE2'                                               19310000
*                                                                       19320000
*        Write report line to start executing                           19330000
         MLWZMRPT RPTLINE=CL133' Phase 2 executing ...'                 19340000
*                                                                       19350000
*        Check if a default target is filled                            19360000
         IF (CLC,G_DEFAULT_TARGET,NE,=CL72' ') THEN                     19370000
            L     R2,G_SCAN_TOKENA                                      19380000
            MVC   0(L'G_DEFAULT_TARGET,R2),G_DEFAULT_TARGET             19390000
            LA    R2,71(,R2)                                            19400000
            L     R3,=A(71)                                             19410000
TRIM_DEFAULT_TARGET_CHAR EQU *                                          19420000
            IF (CLI,0(R2),EQ,C' ') THEN                                 19430000
               BCTR  R2,R0                                              19440000
               BCT   R3,TRIM_DEFAULT_TARGET_CHAR                        19450000
            ENDIF                                                       19460000
            LA    R3,1(,R3)                                             19470000
            ST    R3,G_SCAN_TOKEN_LEN                                   19480000
*                                                                       19490000
            L     R15,LWZMAKE_FINDTGTA_PHASE2                           19500000
            BASR  R14,R15                                               19510000
*                                                                       19520000
            CLC   G_RETCODE,=F'0'                                       19530000
            BNE   PHASE2_RET                                            19540000
*                                                                       19550000
            IF (CLC,G_FOUND_TGT_PTR,EQ,=A(0)) THEN                      19560000
               MLWZMRPT RPTLINE=CL133'0Default target not found'        19570000
               MVC   G_RETCODE,=F'8'                                    19580000
               B     PHASE2_RET                                         19590000
            ENDIF                                                       19600000
         ELSE                                                           19610000
            IF (CLC,G_FIRST_TGT_PTR,NE,=A(0)) THEN                      19620000
               MVC   G_FOUND_TGT_PTR,G_FIRST_TGT_PTR                    19630000
            ELSE                                                        19640000
               MLWZMRPT RPTLINE=CL133'0No targets found'                19650000
               B     PHASE2_RET                                         19660000
            ENDIF                                                       19670000
         ENDIF                                                          19680000
*                                                                       19690000
*        Fill execute target parameter block for first target           19700000
         LA    R1,G_EXEC_TGT_PAR1 * Point R1 to parameter block         19710000
*        Put target pointer in parameter block                          19720000
         MVC   EXEC_TGT_PTR-EXEC_TGT_PAR(4,R1),G_FOUND_TGT_PTR          19730000
         ST    R1,G_EXEC_TGT_PAR1A * Store address of parameter block   19740000
         LA    R1,G_EXEC_TGT_PAR1A * Load address of param block ptr    19750000
         L     R15,LWZMAKE_EXEC_TGTA_PHASE2 * Get address of EXEC_TGT   19760000
         BASR  R14,R15             * Link to EXEC_TGT section           19770000
*                                                                       19780000
PHASE2_RET EQU *                                                        19790000
         MLWZTERM                 * Return back to caller               19800000
*                                                                       19810000
         LTORG                                                          19820000
*                                                                       19830000
* Local constant pointers to section addresses                          19840000
LWZMAKE_FINDTGTA_PHASE2     DC    A(LWZMAKE_FINDTGT)                    19850000
LWZMAKE_EXEC_TGTA_PHASE2    DC    A(LWZMAKE_EXEC_TGT)                   19860000
*                                                                       19870000
*********************************************************************** 19880000
* Section: LWZMAKE_SCAN_STMT                                          * 19890000
* Purpose: This section performs the parsing of statements. As long   * 19900000
*          as the statement is not finished it keeps calling          * 19910000
*          LWZMAKE_SCAN_TOKEN for the next keyword. In most cases the * 19920000
*          first 2 tokens are needed to determine the type of state-  * 19930000
*          ment. 2 tokens are needed to determine the type of state-  * 19940000
*          R9 should point to global data.                            * 19950000
*********************************************************************** 19960000
LWZMAKE_SCAN_STMT MLWZSAVE                                              19970000
*        Trace record to start section                                  19980000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX19990000
               KE_SCAN_STMT'                                            20000000
*                                                                       20010000
*        Before resetting scan state, remember if previous statement    20020000
*        was in recipe                                                  20030000
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN               20040000
            MVI   G_PREV_STMT_IN_RECIPE,C'Y'                            20050000
         ELSE                                                           20060000
            MVI   G_PREV_STMT_IN_RECIPE,C'N'                            20070000
         ENDIF                                                          20080000
*        Reset scan state to initial state NOT_IN_STMT                  20090000
         MVI   G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT                      20100000
*        Reset token lengths to 0                                       20110000
         MVC   G_SCAN_TOKEN_LEN,=F'0'                                   20120000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  20130000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  20140000
*                                                                       20150000
*        Get the first token                                            20160000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 20170000
         BASR  R14,R15            * Link to SCAN_TOKEN section          20180000
*                                                                       20190000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 20200000
         BNE   SCAN_STMT_RET      * Yes, stop parsing statement         20210000
         CLI   G_MKFEOF,C'Y'      * Are we at the end of makefile?      20220000
         BE    SCAN_STMT_RET      * Yes, stop parsing statement         20230000
*                                                                       20240000
*        Only a rule type statement can start with a $ variable         20250000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_VARIABLE) THEN      20260000
*           So the second token is not needed                           20270000
            BAL   R8,STMT_RULE    * Perform parsing of rule statement   20280000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20290000
*                                 * so stop parsing                     20300000
         ENDIF                                                          20310000
*                                                                       20320000
*        Only a call type statement can start with the CALL keyword     20330000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_CALL) THEN          20340000
*           So the second token is not needed                           20350000
            BAL   R8,STMT_CALL    * Perform parsing of call statement   20360000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20370000
*                                 * so stop parsing                     20380000
         ENDIF                                                          20390000
*                                                                       20400000
*        If first token is .PHONY the statement is a PHONY              20410000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_SPECIAL) THEN       20420000
            CLC   G_SCAN_TOKEN_LEN,=A(6)                                20430000
            IF (EQ) THEN                                                20440000
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1        20450000
               CLC   0(6,R14),=C'.PHONY'  * Is it .PHONY?               20460000
               IF (EQ) THEN               * If so...                    20470000
                  BAL   R8,STMT_PHONY     * Perform parsing PHONY stmt  20480000
                  B     SCAN_STMT_RET     * Statement parsed in         20490000
*                                         * subroutine so stop parsing  20500000
               ENDIF                                                    20510000
            ENDIF                                                       20520000
         ENDIF                                                          20530000
*                                                                       20540000
*        Copy token 1 to token 2 so it can be used for next SCAN_TOKEN  20550000
         MVI   G_SCAN_APPEND_TO,X'01'                                   20560000
         MVC   G_SCAN_SPACE_COUNT,=A(0)                                 20570000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   20580000
         BASR  R14,R15            * Link to APPEND_TOKEN section        20590000
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type     20600000
*                                                                       20610000
*        Clear scan state except for left most bit indicating in recipe 20620000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        20630000
*        Set scan state bits to IN_STMT                                 20640000
         OI    G_SCAN_STATE,SCAN_STATE_IN_STMT                          20650000
*                                                                       20660000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 20670000
         BASR  R14,R15            * Link to SCAN_TOKEN section          20680000
*                                                                       20690000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 20700000
         BNE   SCAN_STMT_RET      * Yes, stop parsing statement         20710000
*                                                                       20720000
*        If second token is an operator the statement is an assignment  20730000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_OPERATOR) THEN      20740000
            BAL   R8,STMT_ASSIGNMENT * Perform parsing of assignment    20750000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20760000
*                                 * so stop parsing                     20770000
         ENDIF                                                          20780000
*                                                                       20790000
*        If second token is a rule separator the statement is a rule    20800000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN          20810000
            BAL   R8,STMT_RULE    * Perform parsing of rule statement   20820000
            B     SCAN_STMT_RET   * Statement parsed in subroutine      20830000
*                                 * so stop parsing                     20840000
         ENDIF                                                          20850000
*                                                                       20860000
*        If first and second token are both regular keywords the        20870000
*        statement is a rule                                            20880000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN        20890000
            IF (CLI,G_SCAN_TOKENTYPE2,EQ,SCAN_TOKENTYPE_NORMAL) THEN    20900000
               BAL   R8,STMT_RULE  * Perform parsing of rule statement  20910000
               B     SCAN_STMT_RET * Statement parsed in subroutine     20920000
*                                  * so stop parsing                    20930000
            ENDIF                                                       20940000
         ENDIF                                                          20950000
*                                                                       20960000
*        No valid combination of keywords found, so report syntax error 20970000
*        and give off return code 8                                     20980000
         MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'             20990000
         MVC   G_RETCODE,=F'8'                                          21000000
*                                                                       21010000
SCAN_STMT_RET EQU *                                                     21020000
         MLWZTERM                 * Return back to caller               21030000
*                                                                       21040000
* STMT assignment (e.g. 'foo = bar')                                    21050000
* At this point 2 tokens have been scanned, the assignment destination, 21060000
* which has been copied to token 2 and the operator still in token 1.   21070000
* From here on the source text (right side of the assignment) is parsed 21080000
* into token 3. When the statement is finished it's converted to        21090000
* internal memory format and added to the statement linked list. Also   21100000
* the variable and it's current value are added/updated to the variable 21110000
* binary search tree.                                                   21120000
*                                                                       21130000
STMT_ASSIGNMENT EQU *                                                   21140000
*        Write a trace record for statement type assignment             21150000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'ASSIGNX21160000
               MENT'                                                    21170000
*                                                                       21180000
*        Save operator so token 1 can be reused                         21190000
         L     R5,G_SCAN_TOKENA   * Point R5 to token 1                 21200000
         L     R6,G_SCAN_TOKEN_LEN * Put token 1 length in R6           21210000
         C     R6,=F'2'           * Was the operator 2 bytes?           21220000
         IF (EQ) THEN             * If so...                            21230000
            MVC   G_STMT_SAVE_OP(2),0(R5) * copy 2 bytes                21240000
         ELSE                     * otherwise...                        21250000
            MVC   G_STMT_SAVE_OP(1),0(R5) * copy 1 byte                 21260000
            MVI   G_STMT_SAVE_OP+1,X'00'  * and add a null char         21270000
         ENDIF                                                          21280000
*                                                                       21290000
*        Clear scan state except for left most bit indicating in recipe 21300000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        21310000
*        Set scan state bits to IN_ASSIGN                               21320000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ASSIGN                        21330000
*        Clear token 3 length, which will receive the assignment source 21340000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  21350000
*                                                                       21360000
STMT_A_NEXT_TOKEN EQU *                                                 21370000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 21380000
         BASR  R14,R15            * Link to SCAN_TOKEN section          21390000
*                                                                       21400000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 21410000
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement        21420000
*                                                                       21430000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     21440000
*        statement was finished                                         21450000
         IC    R14,G_SCAN_STATE   * Get the scan state                  21460000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   21470000
*                                 * the high order bit in the scan      21480000
*                                 * state for 'in recipe'               21490000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   21500000
         BE    STMT_A_FINISH      * If so, statement done               21510000
*                                                                       21520000
*        Check if we've hit a $() variable and we're in a simply        21530000
*        expanded variable assignment (:=). In that case the variable   21540000
*        is immediately resolved.                                       21550000
         IC    R14,G_SCAN_STATE   * Get the scan state                  21560000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   21570000
*                                 * the high order bit in the scan      21580000
*                                 * state for 'in recipe'               21590000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()   21600000
         IF (NE) THEN                                                   21610000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       21620000
         ENDIF                                                          21630000
         IF (EQ) THEN             * If so...                            21640000
            CLC   G_STMT_SAVE_OP,=C':=' * Check for simply expanded     21650000
            IF (EQ) THEN          * If so...                            21660000
               MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1     21670000
               MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces  21680000
               L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR  21690000
               BASR  R14,R15      * Link to SCAN_VAR section            21700000
*                                                                       21710000
               CLC   G_RETCODE,=F'0' * Did an error occur?              21720000
               BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement  21730000
*                                                                       21740000
               B     STMT_A_NEXT_TOKEN * Loop around to get next token  21750000
            ENDIF                                                       21760000
         ENDIF                                                          21770000
*                                                                       21780000
*        Append token 1 to token 3                                      21790000
         MVI   G_SCAN_APPEND_TO,X'02'                                   21800000
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3        21810000
         IF (Z) THEN                * Is this the first part of token 3 21820000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  21830000
         ENDIF                                                          21840000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   21850000
         BASR  R14,R15            * Link to APPEND_TOKEN section        21860000
*                                                                       21870000
*        Clear scan state except for left most bit indicating in recipe 21880000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        21890000
*        Set scan state bits to IN_ASSIGN2                              21900000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ASSIGN2                       21910000
*                                                                       21920000
         B     STMT_A_NEXT_TOKEN  * Loop around to get next token       21930000
*                                                                       21940000
STMT_A_FINISH EQU *                                                     21950000
*        Write trace record that statement is finished                  21960000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                21970000
*                                                                       21980000
*        Allocate a new memory block for this assignment                21990000
         L     R1,=A(STMT_A_DSECT_LEN) * Size of block without token    22000000
         A     R1,G_SCAN_TOKEN3_LEN    * Add token length               22010000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    22020000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_ASSIGNMENT * New block is    22030000
*                                 * for type assignment                 22040000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 22050000
         BASR  R14,R15            * Link to ALLOC_STMT section          22060000
*                                                                       22070000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 22080000
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement        22090000
*                                                                       22100000
*        Get returned pointer to new block of memory and put in in R7   22110000
*        It should stay in R7 for the LWZMAKE_STORE_VAR section         22120000
         LT    R7,G_STMT_ALLOC_RETURN_PTR                               22130000
         BZ    STMT_ASSIGNMENT_RET * If it was zero, stop parsing       22140000
*                                 * (failsafe, shouldn't happen)        22150000
*                                                                       22160000
         USING STMT_A_DSECT,R7    * Address with assignment DSECT       22170000
*                                                                       22180000
*        Fill in the operator in assignment block                       22190000
         MVC   STMT_A_OPERATOR,G_STMT_SAVE_OP * Copy 2 bytes operator   22200000
*                                                                       22210000
*        Fill in destination variable name (token 2) in assignment blk  22220000
         L     R6,G_SCAN_TOKEN2_LEN * Get length of variable name       22230000
         C     R6,=A(L'STMT_A_DEST) * Check if it fits                  22240000
         IF (H) THEN                * If not, write error and stop      22250000
            MLWZMRPT RPTLINE=CL133'0Internal error, variable name longeX22260000
               r than 72',APND_LC=C'Y'                                  22270000
            MVC   G_RETCODE,=F'12'  * Set return code 12                22280000
            BR    R8                * and return                        22290000
         ENDIF                                                          22300000
         STH   R6,STMT_A_DESTLEN  * Put variable name length in block   22310000
         L     R5,G_SCAN_TOKEN2A  * Point R5 to token 2                 22320000
         LA    R4,STMT_A_DEST     * Point R4 to var name in block       22330000
         BCTR  R6,R0              * Length minus 1 for EX               22340000
         B     *+10               * Skip MVC constant for EX            22350000
         MVC   0(1,R4),0(R5)      * MVC constant for EX                 22360000
         EX    R6,*-6             * EX previous MVC statement with R6   22370000
*                                                                       22380000
*        Fill in source text (token 3) in assignment block              22390000
         LA    R0,STMT_A_SRC        * Point R0 to source in block       22400000
         L     R1,G_SCAN_TOKEN3_LEN * Get length of source              22410000
         STH   R1,STMT_A_SRCLEN     * Store length in block             22420000
         L     R2,G_SCAN_TOKEN3A    * Point R2 to token 3               22430000
         LR    R3,R1                * Make sure no cropping/filling     22440000
         MVCL  R0,R2                * Copy source text                  22450000
*                                                                       22460000
*        Check for assignment of special var                            22470000
         L     R14,G_SCAN_TOKEN2A * Point R14 to token 2                22480000
         IF (CLI,0(R14),EQ,C'.') THEN * If token 2 starts with .        22490000
            CLC   G_SCAN_TOKEN2_LEN,=F'13' * Is length 13?              22500000
            IF (EQ) THEN              * Yes, so it can be recipepref    22510000
               CLC   0(13,R14),=C'.RECIPEPREFIX' * Is it?               22520000
               BNE   STMT_ASSIGNMENT_UNKNOWN_SPECIAL * No, error        22530000
               CLC   G_SCAN_TOKEN3_LEN,=F'1' * Was source text 1 pos?   22540000
               BNE   STMT_ASSIGNMENT_WRONG_REPPREFLEN * No, error       22550000
               L     R14,G_SCAN_TOKEN3A    * Point R14 to token 3       22560000
               MVC   G_RECIPEPREFIX,0(R14) * Copy recipeprefix          22570000
               B     STMT_ASSIGNMENT_RET   * Skip the rest              22580000
            ELSE                      * Length is not 13                22590000
               B     STMT_ASSIGNMENT_UNKNOWN_SPECIAL * so error         22600000
            ENDIF                                                       22610000
         ENDIF                                                          22620000
*                                                                       22630000
*        Add/update the variable to binary search tree for vars, but    22640000
*        not if we're in a recipe                                       22650000
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN               22660000
*           R7 points to the assignment statement                       22670000
            L     R15,LWZMAKE_STORE_VARA_STMT * Get address STORE_VAR   22680000
            BASR  R14,R15              * Link to STORE_VAR section      22690000
         ENDIF                                                          22700000
*                                                                       22710000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 22720000
         BNE   STMT_ASSIGNMENT_RET * Yes, stop parsing statement        22730000
*                                                                       22740000
*        Remember this was an assignment for next statement's previous  22750000
*        statement type                                                 22760000
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_ASSIGNMENT                    22770000
*                                                                       22780000
STMT_ASSIGNMENT_RET EQU *                                               22790000
         BR    R8                                                       22800000
*                                                                       22810000
STMT_ASSIGNMENT_UNKNOWN_SPECIAL EQU *                                   22820000
         MLWZMRPT RPTLINE=CL133'0Unknown special variable',APND_LC=C'Y' 22830000
         MVC   G_RETCODE,=F'8'                                          22840000
         BR    R8                                                       22850000
*                                                                       22860000
STMT_ASSIGNMENT_WRONG_REPPREFLEN EQU *                                  22870000
         MLWZMRPT RPTLINE=CL133'0Recipeprefix can only be 1 character',X22880000
               APND_LC=C'Y'                                             22890000
         MVC   G_RETCODE,=F'8'                                          22900000
         BR    R8                                                       22910000
*                                                                       22920000
         DROP  R7                                                       22930000
*                                                                       22940000
* STMT rule 'bla : jodel'                                               22950000
* At this point 1 or 2 tokens have been scanned, 1 in the case the      22960000
* statement starts with a $() variable, 2 in either the case of a       22970000
* token followed by a rule separator (:), or in the case of 2 normal    22980000
* tokens.                                                               22990000
* From here on keywords are parsed and appended to token 2 until a rule 23000000
* separator is encountered, so that token 2 contains the target name(s) 23010000
* After that keywords are parsed and appended to token 3 until end of   23020000
* statement, so that token 3 contains the requisite(s).                 23030000
* Any variables encountered in the target name(s) are resolved here,    23040000
* but variables in the requisites are left intact and are resolved in   23050000
* phase 2.                                                              23060000
* When the statement is finished it's converted to internal memory      23070000
* format and added to the statement linked list.                        23080000
*                                                                       23090000
STMT_RULE EQU  *                                                        23100000
*        Write a trace record for statement type rule                   23110000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'RULE'  23120000
*                                                                       23130000
*        Check if we already have a rule separator, if so save it as    23140000
*        the operator and skip to target token complete                 23150000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN          23160000
            MVI   G_STMT_SAVE_OP,C':' * Save : as operator              23170000
            MVI   G_STMT_SAVE_OP+1,X'00' * Append null char             23180000
            B     STMT_R_TGT_TOKEN_COMPLETE * Skip parsing target name  23190000
         ELSE                                                           23200000
            MVC   G_STMT_SAVE_OP,=X'0000' * Initialize operator         23210000
*           Check if we ended up here because statement started with    23220000
*           a $() variable, in that case do a nasty jump right into     23230000
*           the keyword scanning loop below to expand this variable     23240000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_VARIABLE              23250000
            BE    STMT_R_SCAN_VAR                                       23260000
         ENDIF                                                          23270000
*                                                                       23280000
STMT_R_NEXT_TOKEN EQU *                                                 23290000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 23300000
         BASR  R14,R15            * Link to SCAN_TOKEN section          23310000
*                                                                       23320000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 23330000
         BNE   STMT_RULE_RET      * Yes, stop parsing statement         23340000
*                                                                       23350000
*        Check if we have a rule separator, if so save it as the        23360000
*        operator and skip to target token complete                     23370000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_RULE) THEN          23380000
            MVI   G_STMT_SAVE_OP,C':' * Save : as operator              23390000
            MVI   G_STMT_SAVE_OP+1,X'00' * Append null char             23400000
            B     STMT_R_TGT_TOKEN_COMPLETE * Skip parsing target name  23410000
         ENDIF                                                          23420000
*                                                                       23430000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     23440000
*        section to expand it and loop around for the next keyword      23450000
         IC    R14,G_SCAN_STATE   * Get the scan state                  23460000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   23470000
*                                 * the high order bit in the scan      23480000
*                                 * state for 'in recipe'               23490000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   23500000
         IF (EQ) THEN             * If so...                            23510000
STMT_R_SCAN_VAR EQU *                                                   23520000
            MVI   G_SCAN_APPEND_TO,X'00' * Set append to token 1        23530000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1' * Preserve 1 space    23540000
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address of SCAN_VAR  23550000
            BASR  R14,R15         * Link to SCAN_VAR section            23560000
*                                                                       23570000
            CLC   G_RETCODE,=F'0' * Did an error occur?                 23580000
            BNE   STMT_RULE_RET   * Yes, stop parsing statement         23590000
*                                                                       23600000
            B     STMT_R_NEXT_TOKEN * Loop around to get next token     23610000
         ENDIF                                                          23620000
*                                                                       23630000
*        Append token 1 to token 2                                      23640000
         MVI   G_SCAN_APPEND_TO,X'01'                                   23650000
         LT    R1,G_SCAN_TOKEN2_LEN  * Get current length token 2       23660000
         IF (Z) THEN                * Is this the first part of token 2 23670000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  23680000
         ELSE                                                           23690000
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?           23700000
            IF (NZ) THEN               * Yep...                         23710000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           23720000
            ENDIF                                                       23730000
         ENDIF                                                          23740000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   23750000
         BASR  R14,R15            * Link to APPEND_TOKEN section        23760000
*                                                                       23770000
         B     STMT_R_NEXT_TOKEN  * Loop around to get next token       23780000
*                                                                       23790000
STMT_R_TGT_TOKEN_COMPLETE EQU *                                         23800000
*        Clear scan state except for left most bit indicating in recipe 23810000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        23820000
*        Set scan state bits to IN_RULE2, meaning target name complete  23830000
         OI    G_SCAN_STATE,SCAN_STATE_IN_RULE2                         23840000
*        Clear token 3 length, which will receive the requisites        23850000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  23860000
*                                                                       23870000
STMT_R_NEXT_TOKEN2 EQU *                                                23880000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 23890000
         BASR  R14,R15            * Link to SCAN_TOKEN section          23900000
*                                                                       23910000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 23920000
         BNE   STMT_RULE_RET      * Yes, stop parsing statement         23930000
*                                                                       23940000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     23950000
*        statement was finished                                         23960000
         IC    R14,G_SCAN_STATE   * Get the scan state                  23970000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   23980000
*                                 * the high order bit in the scan      23990000
*                                 * state for 'in recipe'               24000000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   24010000
         BE    STMT_R_FINISH      * If so, statement done               24020000
*                                                                       24030000
*        Check if we've hit a $() variable, if so parse that with the   24040000
*        same section that resolves them. Setting append to token 3     24050000
*        causes it not to resolve but simple parse and append.          24060000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()   24070000
         IF (EQ) THEN             * If so...                            24080000
            MVI   G_SCAN_APPEND_TO,X'02' * Set append to token 3        24090000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1' * Preserve 1 space    24100000
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address of SCAN_VAR  24110000
            BASR  R14,R15         * Link to SCAN_VAR section            24120000
*                                                                       24130000
            CLC   G_RETCODE,=F'0' * Did an error occur?                 24140000
            BNE   STMT_RULE_RET   *  Yes, stop parsing statement        24150000
*                                                                       24160000
            B     STMT_R_NEXT_TOKEN2 * Loop around to get next token    24170000
         ENDIF                                                          24180000
*                                                                       24190000
*        Append token 1 to token 3                                      24200000
         MVI   G_SCAN_APPEND_TO,X'02'                                   24210000
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3        24220000
         IF (Z) THEN                * Is this the first part of token 3 24230000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  24240000
         ELSE                                                           24250000
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?           24260000
            IF (NZ) THEN               * Yep...                         24270000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           24280000
            ENDIF                                                       24290000
         ENDIF                                                          24300000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   24310000
         BASR  R14,R15            * Link to APPEND_TOKEN section        24320000
*                                                                       24330000
*        Set scan scate tot RULE3                                       24340000
         IC    R14,G_SCAN_STATE   * Get the scan state                  24350000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   24360000
*                                 * the high order bit in the scan      24370000
*                                 * state for 'in recipe'               24380000
         C     R14,=A(SCAN_STATE_IN_RULE2) * Check for RULE2            24390000
         IF (EQ) THEN             * Only RULE2 can change to RULE3      24400000
*           Clear scan state except for left most bit for in recipe     24410000
            NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                     24420000
*           Set scan state bits to IN_RULE3, meaning in requisites      24430000
            OI    G_SCAN_STATE,SCAN_STATE_IN_RULE3                      24440000
         ENDIF                                                          24450000
*                                                                       24460000
         B     STMT_R_NEXT_TOKEN2 * Loop around for the next token      24470000
*                                                                       24480000
STMT_R_FINISH EQU *                                                     24490000
*        Write trace record that statement is finished                  24500000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                24510000
*                                                                       24520000
*        Allocate a new memory block for this rule                      24530000
         L     R1,=A(STMT_R_DSECT_LEN) * Size of block without tokens   24540000
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of target name(s)   24550000
         A     R1,G_SCAN_TOKEN3_LEN    * Add length of requisites       24560000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    24570000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_RULE * New block is for type 24580000
*                                               * rule                  24590000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 24600000
         BASR  R14,R15            * Link to ALLOC_STMT section          24610000
*                                                                       24620000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 24630000
         BNE   STMT_RULE_RET      * Yes, stop parsing statement         24640000
*                                                                       24650000
*        Get returned pointer to new block of memory and put in in R7   24660000
*        It should stay in R7 for the LWZMAKE_STORE_TGT section         24670000
         LT    R7,G_STMT_ALLOC_RETURN_PTR                               24680000
         BZ    STMT_RULE_RET      * If it was zero, stop parsing        24690000
*                                 * (failsafe, shouldn't happen)        24700000
*                                                                       24710000
         USING STMT_R_DSECT,R7    * Address with rule DSECT             24720000
*                                                                       24730000
*        Copy target name(s) to memory block of rule statement          24740000
         LA    R0,STMT_R_TGT      * Point R0 to start of target         24750000
         L     R1,G_SCAN_TOKEN2_LEN * Get target length                 24760000
         STH   R1,STMT_R_TGTLEN   * Put target length in block          24770000
         L     R2,G_SCAN_TOKEN2A  * Point R2 to token 2                 24780000
         LR    R3,R1              * Make sure no cropping/filling       24790000
         MVCL  R0,R2              * Copy target name(s) to block        24800000
*                                                                       24810000
*        Copy requisite name(s) to memory block of rule statement       24820000
         LA    R0,STMT_R_TGT      * Point R0 to start of target         24830000
         AH    R0,STMT_R_TGTLEN   * Add target length, so now points to 24840000
*                                 * start of requisite                  24850000
         L     R1,G_SCAN_TOKEN3_LEN * Get requisite length              24860000
         STH   R1,STMT_R_REQLEN   * Put requisite length in block       24870000
         L     R2,G_SCAN_TOKEN3A  * Point R2 to token 3                 24880000
         LR    R3,R1              * Make sure no cropping/filling       24890000
         MVCL  R0,R2              * Copy requisite name(s) to block     24900000
*                                                                       24910000
*        Split up space delimited target name(s) and for each name link 24920000
*        to STORE_TGT to allocate a target block and add it to the      24930000
*        target binary search tree.                                     24940000
         LA    R2,STMT_R_TGT      * Point R2 to target name(s)          24950000
         XR    R3,R3              * Clear R3                            24960000
         LH    R3,STMT_R_TGTLEN   * Put length of target name(s) in R3  24970000
STMT_R_SCAN_NEXT_TGT EQU *                                              24980000
         L     R4,G_SCAN_TOKENA   * Point R4 to token 1                 24990000
         XR    R5,R5              * Clear R5                            25000000
         ST    R5,G_SCAN_TOKEN_LEN * And clear length token 1           25010000
STMT_R_TGT_BLANK EQU *                                                  25020000
         IF (CLI,0(R2),EQ,C' ') THEN * Current pos a space?             25030000
            LA    R2,1(,R2)       * Skip space char                     25040000
            BCT   R3,STMT_R_TGT_BLANK * R3 = R3 - 1 until R3 = 0        25050000
            B     STMT_R_STORE_DONE * No more chars, done storing TGTs  25060000
         ENDIF                                                          25070000
STMT_R_TGT_NONBLANK EQU *                                               25080000
         MVC    0(1,R4),0(R2)     * Copy char to token 1                25090000
         LA     R4,1(,R4)         * Advance current pos token 1         25100000
         LA     R5,1(,R5)         * R5 = R5 - 1                         25110000
         BCTR   R3,R0             * R3 = R3 - 1                         25120000
         C      R3,=F'0'          * At end of target name(s)?           25130000
         IF (H) THEN              * If not...                           25140000
            LA     R2,1(,R2)      * Advance current pos target name(s)  25150000
            CLI    0(R2),C' '     * Current pos a space?                25160000
            BNE    STMT_R_TGT_NONBLANK * Loop around to copy next char  25170000
         ENDIF                                                          25180000
*        Either a space was found or we've reached the end of targets   25190000
         ST     R5,G_SCAN_TOKEN_LEN * Store target length               25200000
*                                                                       25210000
*        Add the target to binary search tree for targets               25220000
*        R7 points to the rule statement                                25230000
*        Token 1 contains one target name                               25240000
         L     R15,LWZMAKE_STORE_TGTA_STMT * Get address of STORE_TGT   25250000
         BASR  R14,R15            * Link to STORE_TGT section           25260000
*                                                                       25270000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 25280000
         BNE   STMT_RULE_RET      * Yes, stop storing targets           25290000
*                                                                       25300000
         C      R3,=F'0'          * More chars left in target name(s)?  25310000
         BH     STMT_R_SCAN_NEXT_TGT * If so, look for next target      25320000
*                                                                       25330000
         DROP  R7                                                       25340000
*                                                                       25350000
STMT_R_STORE_DONE EQU *                                                 25360000
*        Remember this was a rule for next statement's previous         25370000
*        statement type                                                 25380000
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_RULE                          25390000
*                                                                       25400000
STMT_RULE_RET EQU *                                                     25410000
         BR    R8                 * Return                              25420000
*                                                                       25430000
* STMT call 'CALL routine'                                              25440000
* At this point 1 token has been scanned, which is the CALL keyword.    25450000
* From here on the first next token will be the REXX exec called, which 25460000
* is stored in token 2. Any tokens after that are concatenated in token 25470000
* 3 which becomes the (optional) parameter to the REXX exec.            25480000
* When the statement is finished it's converted to internal memory      25490000
* format and added to the statement linked list.                        25500000
*                                                                       25510000
STMT_CALL EQU  *                                                        25520000
*        Write a trace record for statement type call                   25530000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'CALL'  25540000
*                                                                       25550000
*        Clear scan state except for left most bit indicating in recipe 25560000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        25570000
*        Set scan state bits to IN_CALL                                 25580000
         OI    G_SCAN_STATE,SCAN_STATE_IN_CALL                          25590000
*        Clear token 2 length, which will receive the REXX exec name    25600000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  25610000
*                                                                       25620000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 25630000
         BASR  R14,R15            * Link to SCAN_TOKEN section          25640000
*                                                                       25650000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 25660000
         BNE   STMT_CALL_RET      * Yes, stop parsing statement         25670000
*                                                                       25680000
         L     R2,G_SCAN_TOKEN_LEN * Get length of token 1              25690000
         C     R2,=A(8)           * Longer than 8 positions?            25700000
         IF (H) THEN              * If so...                            25710000
            MLWZMRPT RPTLINE=CL133'0REXX exec cannot be longer than 8 cX25720000
               haracters',APND_LC=C'Y'                                  25730000
            MVC   G_RETCODE,=F'8' * Set return code 8                   25740000
            BR    R8              * and return                          25750000
         ENDIF                                                          25760000
*                                                                       25770000
*        Copy token 1 to token 2                                        25780000
         MVI   G_SCAN_APPEND_TO,X'01'                                   25790000
         MVC   G_SCAN_SPACE_COUNT,=A(0)                                 25800000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   25810000
         BASR  R14,R15            * Link to APPEND_TOKEN section        25820000
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type     25830000
*                                                                       25840000
*        Clear scan state except for left most bit indicating in recipe 25850000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        25860000
*        Set scan state bits to IN_CALL2                                25870000
         OI    G_SCAN_STATE,SCAN_STATE_IN_CALL2                         25880000
*        Clear token 2 length, which will receive the REXX exec parm    25890000
         MVC   G_SCAN_TOKEN3_LEN,=F'0'                                  25900000
*                                                                       25910000
STMT_C_NEXT_TOKEN EQU *                                                 25920000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 25930000
         BASR  R14,R15            * Link to SCAN_TOKEN section          25940000
*                                                                       25950000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 25960000
         BNE   STMT_CALL_RET      * Yes, stop parsing statement         25970000
*                                                                       25980000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     25990000
*        statement was finished                                         26000000
         IC    R14,G_SCAN_STATE   * Get the scan state                  26010000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   26020000
*                                 * the high order bit in the scan      26030000
*                                 * state for 'in recipe'               26040000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   26050000
         BE    STMT_C_FINISH      * If so, statement done               26060000
*                                                                       26070000
*        Check if we've hit a $() variable                              26080000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check if we're in $()   26090000
         IF (NE) THEN                                                   26100000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       26110000
         ENDIF                                                          26120000
         IF (EQ) THEN             * If so...                            26130000
            MVI   G_SCAN_APPEND_TO,X'02' * Set append to token 3        26140000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'A' * Preserve spaces     26150000
            L     R15,LWZMAKE_SCAN_VARA_STMT * Get address SCAN_VAR     26160000
            BASR  R14,R15         * Link to SCAN_VAR section            26170000
*                                                                       26180000
            CLC   G_RETCODE,=F'0' * Did an error occur?                 26190000
            BNE   STMT_CALL_RET   * Yes, stop parsing statement         26200000
*                                                                       26210000
            B     STMT_C_NEXT_TOKEN * Loop around to get next token     26220000
         ENDIF                                                          26230000
*                                                                       26240000
*        Append token 1 to token 3, leading spaces since last token are 26250000
*        preserved (tokenizer counts them in G_SCAN_SPACE_COUNT)        26260000
         MVI   G_SCAN_APPEND_TO,X'02'                                   26270000
         LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3        26280000
         IF (Z) THEN                * Is this the first part of token 3 26290000
            MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of leading spaces  26300000
         ELSE                                                           26310000
            LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?           26320000
            IF (NZ) THEN               * Yep...                         26330000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           26340000
            ENDIF                                                       26350000
         ENDIF                                                          26360000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   26370000
         BASR  R14,R15            * Link to APPEND_TOKEN section        26380000
*                                                                       26390000
         B     STMT_C_NEXT_TOKEN  * Loop around for the next token      26400000
*                                                                       26410000
STMT_C_FINISH EQU *                                                     26420000
*        Write trace record that statement is finished                  26430000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                26440000
*                                                                       26450000
*        Allocate a new memory block for this call                      26460000
         L     R1,=A(STMT_C_DSECT_LEN) * Size of block without tokens   26470000
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of REXX exec name   26480000
         A     R1,G_SCAN_TOKEN3_LEN    * Add length of REXX exec parm   26490000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    26500000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_CALL * New block is for type 26510000
*                                               * call                  26520000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 26530000
         BASR  R14,R15            * Link to ALLOC_STMT section          26540000
*                                                                       26550000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 26560000
         BNE   STMT_CALL_RET      * Yes, stop parsing statement         26570000
*                                                                       26580000
         LT    R7,G_STMT_ALLOC_RETURN_PTR * Get returned ptr to new     26590000
*                                         * block of memory             26600000
         BZ    STMT_CALL_RET      * If it was zero, stop parsing        26610000
*                                 * (failsafe, shouldn't happen)        26620000
*                                                                       26630000
         USING STMT_C_DSECT,R7    * Address with call DSECT             26640000
*                                                                       26650000
*        Copy REXX exec name                                            26660000
         LA    R2,STMT_C_EXEC     * Point R2 to start of exec           26670000
         L     R3,G_SCAN_TOKEN2A  * Point R3 to token 1                 26680000
         L     R4,G_SCAN_TOKEN2_LEN * Get length of token 2             26690000
         STH   R4,STMT_C_EXECLEN  * Store exec length in block          26700000
         BCTR  R4,R0              * Minus 1 for EX                      26710000
         B     *+10               * Skip MVC constant for EX            26720000
         MVC   0(1,R2),0(R3)      * MVC constant for EX                 26730000
         EX    R4,*-6             * EX previous MVC statement with R4   26740000
*                                                                       26750000
*        Copy REXX exec parameter                                       26760000
         LA    R0,STMT_C_EXEC     * Point R0 to start of exec in block  26770000
         AH    R0,STMT_C_EXECLEN  * Advance to start of parm in block   26780000
         L     R1,G_SCAN_TOKEN3_LEN * Get length of exec parm           26790000
         STH   R1,STMT_C_PARMLEN  * Store length of exec parm in block  26800000
         L     R2,G_SCAN_TOKEN3A  * Point R2 to token 3                 26810000
         LR    R3,R1              * Make sure no cropping/filling       26820000
         MVCL  R0,R2              * Copy REXX exec parm to block        26830000
*                                                                       26840000
*        Remember this was a call for next statement's previous         26850000
*        statement type                                                 26860000
         MVI   G_PREV_STMT_TYPE,STMT_TYPE_CALL                          26870000
*                                                                       26880000
STMT_CALL_RET EQU *                                                     26890000
         BR    R8                 * Return                              26900000
*                                                                       26910000
         DROP  R7                                                       26920000
*                                                                       26930000
* STMT PHONY '.PHONY targetname'                                        26940000
* At this point 1 token has been scanned, which is the .PHONY keyword.  26950000
* From here on the first next token will be the PHONY target name.      26960000
* When the statement is finished it's converted to internal memory      26970000
* format and added to the statement linked list.                        26980000
* The PHONY target name is added to the binary search tree for phonies. 26990000
*                                                                       27000000
STMT_PHONY EQU  *                                                       27010000
*        Write a trace record for statement type call                   27020000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'608',CONST=C'PHONY' 27030000
*                                                                       27040000
*        Clear scan state except for left most bit indicating in recipe 27050000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        27060000
*        Set scan state bits to IN_PHONY                                27070000
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY                         27080000
*        Clear token 2 length, which will receive the PHONY target name 27090000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  27100000
*                                                                       27110000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 27120000
         BASR  R14,R15            * Link to SCAN_TOKEN section          27130000
*                                                                       27140000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 27150000
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement         27160000
*                                                                       27170000
         IF (CLI,G_SCAN_TOKENTYPE,NE,SCAN_TOKENTYPE_NORMAL) THEN        27180000
            MLWZMRPT RPTLINE=CL133'0.PHONY must be followed by a constaX27190000
               nt target name',APND_LC=C'Y'                             27200000
            MVC   G_RETCODE,=F'8' * Set return code 8                   27210000
            BR    R8              * and return                          27220000
         ENDIF                                                          27230000
*                                                                       27240000
*        Copy token 1 to token 2                                        27250000
         MVI   G_SCAN_APPEND_TO,X'01'                                   27260000
         MVC   G_SCAN_SPACE_COUNT,=A(0)                                 27270000
         L     R15,LWZMAKE_APPEND_TOKENA_STMT * Get addr APPEND_TOKEN   27280000
         BASR  R14,R15            * Link to APPEND_TOKEN section        27290000
         MVC   G_SCAN_TOKENTYPE2,G_SCAN_TOKENTYPE * Copy token type     27300000
*                                                                       27310000
*        Clear scan state except for left most bit indicating in recipe 27320000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        27330000
*        Set scan state bits to IN_PHONY2                               27340000
         OI    G_SCAN_STATE,SCAN_STATE_IN_PHONY2                        27350000
*                                                                       27360000
STMT_P_NEXT_TOKEN EQU *                                                 27370000
         L     R15,LWZMAKE_SCAN_TOKENA_STMT * Get address of SCAN_TOKEN 27380000
         BASR  R14,R15            * Link to SCAN_TOKEN section          27390000
*                                                                       27400000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 27410000
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement         27420000
*                                                                       27430000
*        Check if scan state was reset to NOT_IN_STMT, meaning this     27440000
*        statement was finished                                         27450000
         IC    R14,G_SCAN_STATE   * Get the scan state                  27460000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   27470000
*                                 * the high order bit in the scan      27480000
*                                 * state for 'in recipe'               27490000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check for not in stmt   27500000
         BNE   STMT_P_NEXT_TOKEN  * If not so, loop around              27510000
*                                                                       27520000
STMT_P_FINISH EQU *                                                     27530000
*        Write trace record that statement is finished                  27540000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'609'                27550000
*                                                                       27560000
*        Allocate a new memory block for this PHONY                     27570000
         L     R1,=A(STMT_P_DSECT_LEN) * Size of block without tokens   27580000
         A     R1,G_SCAN_TOKEN2_LEN    * Add length of PHONY name       27590000
         ST    R1,G_STMT_ALLOC_LEN     * Store as size to be alloc'd    27600000
         MVI   G_STMT_ALLOC_TYPE,STMT_TYPE_PHONY * New block is for     27610000
*                                                * type PHONY           27620000
         L     R15,LWZMAKE_ALLOC_STMTA_STMT * Get address of ALLOC_STMT 27630000
         BASR  R14,R15            * Link to ALLOC_STMT section          27640000
*                                                                       27650000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 27660000
         BNE   STMT_PHONY_RET     * Yes, stop parsing statement         27670000
*                                                                       27680000
*        Get returned pointer to new block of memory and put in in R7   27690000
*        It should stay in R7 for the LWZMAKE_STORE_PNY section         27700000
         LT    R7,G_STMT_ALLOC_RETURN_PTR                               27710000
         BZ    STMT_PHONY_RET     * If it was zero, stop parsing        27720000
*                                 * (failsafe, shouldn't happen)        27730000
*                                                                       27740000
         USING STMT_P_DSECT,R7    * Address with PHONY DSECT            27750000
*                                                                       27760000
*        Copy PHONY name to memory block of PHONY statement             27770000
         LA    R0,STMT_P_PNY      * Point R0 to start of PHONY          27780000
         L     R1,G_SCAN_TOKEN2_LEN * Get PHONY length                  27790000
         STH   R1,STMT_P_PNYLEN   * Put PHONY length in block           27800000
         L     R2,G_SCAN_TOKEN2A  * Point R2 to token 2                 27810000
         LR    R3,R1              * Make sure no cropping/filling       27820000
         MVCL  R0,R2              * Copy PHONY name to block            27830000
*                                                                       27840000
*        R7 points to the PHONY statement                               27850000
         L     R15,LWZMAKE_STORE_PNYA_STMT * Get address STORE_PNY      27860000
         BASR  R14,R15            * Link to STORE_PNY section           27870000
*                                                                       27880000
STMT_PHONY_RET EQU *                                                    27890000
         BR    R8                 * Return                              27900000
*                                                                       27910000
         LTORG                                                          27920000
*                                                                       27930000
* Local constant pointers to section addresses                          27940000
LWZMAKE_SCAN_TOKENA_STMT   DC    A(LWZMAKE_SCAN_TOKEN)                  27950000
LWZMAKE_APPEND_TOKENA_STMT DC    A(LWZMAKE_APPEND_TOKEN)                27960000
LWZMAKE_SCAN_VARA_STMT     DC    A(LWZMAKE_SCAN_VAR)                    27970000
LWZMAKE_ALLOC_STMTA_STMT   DC    A(LWZMAKE_ALLOC_STMT)                  27980000
LWZMAKE_STORE_VARA_STMT    DC    A(LWZMAKE_STORE_VAR)                   27990000
LWZMAKE_STORE_TGTA_STMT    DC    A(LWZMAKE_STORE_TGT)                   28000000
LWZMAKE_STORE_PNYA_STMT    DC    A(LWZMAKE_STORE_PNY)                   28010000
*                                                                       28020000
         DROP                                                           28030000
*                                                                       28040000
*********************************************************************** 28050000
* Section: LWZMAKE_SCAN_VAR                                           * 28060000
* Purpose: Parse a $() variable. At this point $( was parsed into     * 28070000
*          token 1. This section parses the variable and closing ).   * 28080000
*          If G_SCAN_APPEND_TO equals X'00' the variable is looked up * 28090000
*          in the variable binary search tree and its value is added  * 28100000
*          to the input stack so parsing first read from the variable * 28110000
*          value before continuing with the original input.           * 28120000
*          If G_SCAN_APPEND_TO is not X'00' $(var) is appended to     * 28130000
*          token 2 or 3.                                              * 28140000
*          R9 should point to global data.                            * 28150000
*********************************************************************** 28160000
LWZMAKE_SCAN_VAR DS    0F                                               28170000
         STM   R14,R12,12(R13)   * Save callers registers               28180000
         LR    R10,R15                                                  28190000
         LA    R11,4095(,R10)                                           28200000
         LA    R11,1(,R11)                                              28210000
         USING LWZMAKE_SCAN_VAR,R10,R11                                 28220000
         GETMAIN RU,LV=SCAN_VAR_DSECT_SIZ                               28230000
         ST    R13,4(R1)         * Backward chain callers SA            28240000
         ST    R1,8(R13)         * Forward chain my SA                  28250000
         LR    R13,R1            * Point R13 to my SA                   28260000
         USING SCAN_VAR_DSECT,R13 * Establish addressing of workarea    28270000
         USING GLOBAL_DATA_DSECT,R9                                     28280000
*                                                                       28290000
*        Trace record to start section                                  28300000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX28310000
               KE_SCAN_VAR'                                             28320000
*                                                                       28330000
         MVC   G_SAVE_SPACE_COUNT,=F'0'                                 28340000
*                                                                       28350000
*        In any case save the spaces                                    28360000
         LT    R1,G_SCAN_SPACE_COUNT * Any leading spaces?              28370000
         IF (NZ) THEN             * Yep...                              28380000
            IF (CLI,G_SCAN_VAR_PRESERVE_SPACES,NE,C'A') THEN            28390000
               MVC   G_SCAN_SPACE_COUNT,=F'1'                           28400000
            ENDIF                                                       28410000
            SELECT CLI,G_SCAN_APPEND_TO,EQ                              28420000
            WHEN X'00'                                                  28430000
               MVC   G_SAVE_SPACE_COUNT,G_SCAN_SPACE_COUNT              28440000
            WHEN X'01'                                                  28450000
               LT    R14,G_SCAN_TOKEN2_LEN    * Get token 2 len         28460000
               IF (Z) THEN                    * Currently empty?        28470000
                  MVC   G_SCAN_SPACE_COUNT,=F'0'                        28480000
               ENDIF                                                    28490000
            WHEN X'02'                                                  28500000
               LT    R14,G_SCAN_TOKEN3_LEN    * Get token 3 len         28510000
               IF (Z) THEN                    * Currently empty?        28520000
                  MVC   G_SCAN_SPACE_COUNT,=F'0'                        28530000
               ENDIF                                                    28540000
            ENDSEL                                                      28550000
         ENDIF                                                          28560000
*                                                                       28570000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            28580000
         BASR  R14,R15                                                  28590000
*                                                                       28600000
*        Get next token, which should be the variable name              28610000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  28620000
         BASR  R14,R15            * Link to SCAN_TOKEN section          28630000
*                                                                       28640000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 28650000
         BNE   SCAN_VAR_RET       * Yes, stop parsing variable          28660000
*                                                                       28670000
*        Check if scan state changed to anything other than IN_VARIABLE 28680000
*        which shouldn't happen if a valid variable name was found      28690000
         IC    R14,G_SCAN_STATE   * Get the scan state                  28700000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   28710000
*                                 * the high order bit in the scan      28720000
*                                 * state for 'in recipe'               28730000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   28740000
         IF (NE) THEN             * If not, write error and stop        28750000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       28760000
         ENDIF                                                          28770000
         IF (NE) THEN                                                   28780000
            MLWZMRPT RPTLINE=CL133'0Empty $()',APND_LC=C'Y'             28790000
            MVC   G_RETCODE,=F'8' * Set return code 8                   28800000
            B     SCAN_VAR_RET    * and return                          28810000
         ENDIF                                                          28820000
*                                                                       28830000
         L     R1,G_SCAN_TOKEN_LEN * Get length of variable name        28840000
         C     R1,=A(10)           * addpdsname is 10 long              28850000
         IF (EQ) THEN                                                   28860000
            L     R2,G_SCAN_TOKENA * Point R2 to token 1                28870000
            MVC   G_HELPER_DATA(10),0(R2) * Copy token to helper        28880000
            OC    G_HELPER_DATA(10),=10X'40' * Convert to uppercase     28890000
            CLC   G_HELPER_DATA(10),=C'ADDPDSNAME'                      28900000
            IF (EQ) THEN           * If it's the addpdsname function    28910000
               BAL   R8,SCAN_ADDPDSNAME * Scan the function             28920000
               B     SCAN_VAR_RET  * And skip the rest of SCAN_VAR      28930000
            ENDIF                                                       28940000
            CLC   G_HELPER_DATA(10),=C'MEMBERLIST'                      28950000
            IF (EQ) THEN           * If it's the memberlist function    28960000
               BAL   R8,SCAN_MEMBERLIST * Scan the function             28970000
               B     SCAN_VAR_RET  * And skip the rest of SCAN_VAR      28980000
            ENDIF                                                       28990000
         ENDIF                                                          29000000
*                                                                       29010000
*        Ending up here means it's not a function                       29020000
*        If APPEND_TO is X'00', copy variable name to input to FINDVAR  29030000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        29040000
            L     R1,G_SCAN_TOKEN_LEN * Get length of variable name     29050000
            C     R1,=A(L'G_SRCH_VAR) * If should always fit, but just  29060000
            IF (H) THEN               * in case check anyway            29070000
               MLWZMRPT RPTLINE=CL133'0Internal error, variable name loX29080000
               nger than 72',APND_LC=C'Y'                               29090000
               MVC   G_RETCODE,=F'12' * Set return code 12              29100000
               B     SCAN_VAR_RET     * and return                      29110000
            ENDIF                                                       29120000
*           Copy variable name to FINDVAR name                          29130000
            STH   R1,G_SRCH_VAR_LEN * Put length in FINDVAR search len  29140000
            LA    R0,G_SRCH_VAR   * Point R0 to FINDVAR search name     29150000
            L     R2,G_SCAN_TOKENA * Point R2 to token 1                29160000
            LR    R3,R1           * Make sure no cropping/filling       29170000
            MVCL  R0,R2           * Copy variable name to FINDVAR name  29180000
         ELSE                                                           29190000
*           Else if APPEND_TO is not X'00'                              29200000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         29210000
            BASR  R14,R15                                               29220000
         ENDIF                                                          29230000
*                                                                       29240000
*        Clear scan state except for left most bit indicating in recipe 29250000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        29260000
*        Set scan state bits to IN_VARIABLE2                            29270000
         OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLE2                     29280000
*                                                                       29290000
*        Get the next token which should be )                           29300000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  29310000
         BASR  R14,R15            * Link to SCAN_TOKEN section          29320000
*                                                                       29330000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 29340000
         BNE   SCAN_VAR_RET       * Yes, stop parsing variable          29350000
*                                                                       29360000
*        If APPEND_TO != X'00' then copy ) from token 1 to token n      29370000
         IF (CLI,G_SCAN_APPEND_TO,NE,X'00') THEN                        29380000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         29390000
            BASR  R14,R15                                               29400000
         ELSE                                                           29410000
*           Else if APPEND_TO is X'00'                                  29420000
            L     R15,LWZMAKE_FINDVARA_VAR * Get address to FINDVAR     29430000
            BASR  R14,R15         * Link to FINDVAR section             29440000
*                                                                       29450000
            LT    R4,G_FOUND_VAR_PTR * Check if a pointer was returned  29460000
            IF (Z) THEN           * If not write error and stop         29470000
               MLWZMRPT RPTLINE=CL133'0Variable not found',APND_LC=C'Y' 29480000
               MVC   G_RETCODE,=F'8' * Set return code 8                29490000
               B     SCAN_VAR_RET    * and return                       29500000
            ENDIF                                                       29510000
*                                                                       29520000
            USING VAR_DSECT,R4    * Address with VAR DSECT              29530000
*                                                                       29540000
            CLC   VALLEN,=H'0'    * Check empty variable value          29550000
            BE    SCAN_VAR_RET    * If empty skip rest of section       29560000
*                                                                       29570000
*           Push variable value on to input stack                       29580000
            XR    R2,R2           * Clear R2                            29590000
            XR    R3,R3           * Clear R3                            29600000
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index   29610000
            C     R3,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra     29620000
*                                 * entry fit?                          29630000
            IF (NL) THEN          * If not write error                  29640000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX29650000
               flow',APND_LC=C'Y'                                       29660000
               MVC   G_RETCODE,=F'12' * Set return code 12              29670000
               B     SCAN_VAR_RET     * and return                      29680000
            ENDIF                                                       29690000
            LA    R3,1(,R3)       * Add 1 to stack size                 29700000
            STC   R3,G_SCAN_INPUT_STACK_IDX * And store it              29710000
            BCTR  R3,R0           * Subtract 1 to calculate offset      29720000
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry 29730000
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack       29740000
            AR    R2,R3           * Add calculated offset               29750000
*                                                                       29760000
            USING INPUT_DSECT,R2  * Address with INPUT DSECT            29770000
*                                                                       29780000
            MVI   INPUTTYPE,X'01' * Set type of input to ptr to string  29790000
            MVC   INPUTLEAD,G_SAVE_SPACE_COUNT+2                        29800000
            MVC   INPUTLEN,VALLEN * Copy value length                   29810000
            MVC   INPUTPTR,VALPTR * Copy value pointer                  29820000
            MVC   INPUTPOS,=H'0'  * Set initial scan position to start  29830000
*                                                                       29840000
            DROP  R2                                                    29850000
            DROP  R4                                                    29860000
         ENDIF                                                          29870000
*                                                                       29880000
SCAN_VAR_RET EQU *                                                      29890000
         L     R3,4(,R13)        * Restore address of callers SA        29900000
         FREEMAIN RU,LV=SCAN_VAR_DSECT_SIZ,A=(R13)                      29910000
         LR    R13,R3                                                   29920000
         LM    R14,R12,12(R13)                                          29930000
         BR    R14                    Return to caller                  29940000
*                                                                       29950000
* Scan function ADDPDSNAME                                              29960000
*                                                                       29970000
SCAN_ADDPDSNAME EQU *                                                   29980000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            29990000
         BASR  R14,R15                                                  30000000
*                                                                       30010000
         BAL   R7,SCAN_VAR_SAVE                                         30020000
*                                                                       30030000
*        Clear scan state except for left most bit indicating in recipe 30040000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        30050000
*        Set scan state bits to IN_ADDPDSNAME                           30060000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME                    30070000
*                                                                       30080000
*        Get next token, which should be the start of the PDS name      30090000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  30100000
         BASR  R14,R15            * Link to SCAN_TOKEN section          30110000
*                                                                       30120000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 30130000
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function         30140000
*                                                                       30150000
         B     ADDPDSNAME_PDSNAME_CHECK_VAR                             30160000
*                                                                       30170000
ADDPDSNAME_PDSNAME_NEXT_TOKEN EQU *                                     30180000
*        Get next token, which should be the PDS name                   30190000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  30200000
         BASR  R14,R15            * Link to SCAN_TOKEN section          30210000
*                                                                       30220000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 30230000
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function         30240000
*                                                                       30250000
         CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA                    30260000
         BE    ADDPDSNAME3                                              30270000
*                                                                       30280000
         LT    R14,G_SCAN_TOKEN2_LEN                                    30290000
         IF (NZ) THEN                                                   30300000
            LT    R14,G_SCAN_SPACE_COUNT                                30310000
            IF (NZ) THEN                                                30320000
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as pds nameX30330000
                in addpdsname function',APND_LC=C'Y'                    30340000
               MVC   G_RETCODE,=F'8' * Set return code 8                30350000
               BR    R8             * and return                        30360000
            ENDIF                                                       30370000
         ENDIF                                                          30380000
*                                                                       30390000
ADDPDSNAME_PDSNAME_CHECK_VAR EQU *                                      30400000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     30410000
*        section to expand it and loop around for the next keyword      30420000
         IC    R14,G_SCAN_STATE   * Get the scan state                  30430000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   30440000
*                                 * the high order bit in the scan      30450000
*                                 * state for 'in recipe'               30460000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   30470000
         IF (NE) THEN                                                   30480000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       30490000
         ENDIF                                                          30500000
         IF (EQ) THEN             * If so...                            30510000
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   30520000
            BASR  R14,R15            * Link to SCAN_VAR section         30530000
*                                                                       30540000
            CLC   G_RETCODE,=F'0'    * Did an error occur?              30550000
            BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing statement     30560000
*                                                                       30570000
            B     ADDPDSNAME2                                           30580000
         ENDIF                                                          30590000
*                                                                       30600000
         MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO                 30610000
*                                                                       30620000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        30630000
*           Append token 1 to token 2                                   30640000
            MVI   G_SCAN_APPEND_TO,X'01'                                30650000
            LT    R1,G_SCAN_TOKEN2_LEN                                  30660000
            IF (Z) THEN                                                 30670000
               MVC   G_SCAN_SPACE_COUNT,=A(0)                           30680000
            ENDIF                                                       30690000
         ENDIF                                                          30700000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            30710000
         BASR  R14,R15                                                  30720000
         MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO                 30730000
*                                                                       30740000
ADDPDSNAME2 EQU *                                                       30750000
*        Clear scan state except for left most bit indicating in recipe 30760000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        30770000
*        Set scan state bits to IN_ADDPDSNAME2                          30780000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME2                   30790000
*                                                                       30800000
         B     ADDPDSNAME_PDSNAME_NEXT_TOKEN                            30810000
*                                                                       30820000
ADDPDSNAME3 EQU *                                                       30830000
*        Clear scan state except for left most bit indicating in recipe 30840000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        30850000
*        Set scan state bits to IN_ADDPDSNAME3                          30860000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME3                   30870000
*        If we're expanding                                             30880000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        30890000
*           Clear token 3 length, which will receive the member(s)      30900000
            MVC   G_SCAN_TOKEN3_LEN,=F'0'                               30910000
         ELSE                                                           30920000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         30930000
            BASR  R14,R15                                               30940000
         ENDIF                                                          30950000
*                                                                       30960000
ADDPDSNAME_NEXT_MEMBER EQU *                                            30970000
*        Get next token, which should be the a member name or variable  30980000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  30990000
         BASR  R14,R15            * Link to SCAN_TOKEN section          31000000
*                                                                       31010000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 31020000
         BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing function         31030000
*                                                                       31040000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     31050000
*        section to expand it and loop around for the next keyword      31060000
         IC    R14,G_SCAN_STATE   * Get the scan state                  31070000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   31080000
*                                 * the high order bit in the scan      31090000
*                                 * state for 'in recipe'               31100000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   31110000
         IF (NE) THEN                                                   31120000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       31130000
         ENDIF                                                          31140000
         IF (EQ) THEN             * If so...                            31150000
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   31160000
            BASR  R14,R15            * Link to SCAN_VAR section         31170000
*                                                                       31180000
            CLC   G_RETCODE,=F'0'    * Did an error occur?              31190000
            BNE   SCAN_ADDPDSNAME_RET * Yes, stop parsing statement     31200000
*                                                                       31210000
            CLI   G_SCAN_APPEND_TO,X'00'                                31220000
            BE    ADDPDSNAME_NEXT_MEMBER * Loop around for next token   31230000
            B     ADDPDSNAME4                                           31240000
         ENDIF                                                          31250000
*                                                                       31260000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        31270000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          31280000
            BE    ADDPDSNAME_FINISH                                     31290000
*                                                                       31300000
*           Append member to token 3                                    31310000
            L     R0,G_SCAN_TOKEN2A                                     31320000
            A     R0,G_SCAN_TOKEN2_LEN                                  31330000
            L     R1,=F'1'                                              31340000
            LA    R2,=C'('                                              31350000
            LR    R3,R1                                                 31360000
            MVCL  R0,R2                                                 31370000
*                                                                       31380000
            L     R2,G_SCAN_TOKENA  * Point R2 to token 1               31390000
            L     R1,G_SCAN_TOKEN_LEN * Get length of token to append   31400000
            LR    R3,R1             * Make sure no cropping/filling     31410000
            MVCL  R0,R2             * Append to token 3                 31420000
*                                                                       31430000
            L     R1,=F'1'                                              31440000
            LA    R2,=C')'                                              31450000
            LR    R3,R1                                                 31460000
            MVCL  R0,R2                                                 31470000
*                                                                       31480000
            L     R0,G_SCAN_TOKENA                                      31490000
            L     R1,G_SCAN_TOKEN2_LEN                                  31500000
            A     R1,G_SCAN_TOKEN_LEN                                   31510000
            LA    R1,2(,R1)                                             31520000
            ST    R1,G_SCAN_TOKEN_LEN                                   31530000
            L     R2,G_SCAN_TOKEN2A                                     31540000
            LR    R3,R1                                                 31550000
            MVCL  R0,R2                                                 31560000
*                                                                       31570000
            MVI   G_SCAN_APPEND_TO,X'02'                                31580000
            LT    R1,G_SCAN_TOKEN3_LEN * Get current length token 3     31590000
            IF (Z) THEN             * Is this the first part of token 3 31600000
               MVC   G_SCAN_SPACE_COUNT,=F'0' * Get rid of lead spaces  31610000
            ENDIF                                                       31620000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         31630000
            BASR  R14,R15                                               31640000
*                                                                       31650000
            MVI   G_SCAN_APPEND_TO,X'00'                                31660000
         ELSE                                                           31670000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         31680000
            BASR  R14,R15                                               31690000
*                                                                       31700000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          31710000
            BE    ADDPDSNAME_FINISH                                     31720000
         ENDIF                                                          31730000
*                                                                       31740000
ADDPDSNAME4 EQU *                                                       31750000
*        Clear scan state except for left most bit indicating in recipe 31760000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        31770000
*        Set scan state bits to IN_ADDPDSNAME4                          31780000
         OI    G_SCAN_STATE,SCAN_STATE_IN_ADDPDSNAME4                   31790000
*                                                                       31800000
         B     ADDPDSNAME_NEXT_MEMBER * Loop around to get next token   31810000
*                                                                       31820000
ADDPDSNAME_FINISH EQU *                                                 31830000
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack     31840000
         XR    R2,R2                 * Clear R2                         31850000
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index      31860000
         BCTR  R2,R0                 * Subtract 1 from index            31870000
         IC    R15,0(R2,R1)          * Get stack state in that idx      31880000
         STC   R15,G_SCAN_STATE      * And save it as current state     31890000
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx      31900000
*                                                                       31910000
         BAL   R7,SCAN_VAR_RESTORE                                      31920000
*                                                                       31930000
SCAN_ADDPDSNAME_RET EQU *                                               31940000
         BR    R8                                                       31950000
*                                                                       31960000
* Scan function MEMBERLIST                                              31970000
*                                                                       31980000
SCAN_MEMBERLIST EQU *                                                   31990000
         L     R15,LWZMAKE_APPEND_TOKENA_VAR                            32000000
         BASR  R14,R15                                                  32010000
*                                                                       32020000
         BAL   R7,SCAN_VAR_SAVE                                         32030000
*                                                                       32040000
*        Clear scan state except for left most bit indicating in recipe 32050000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        32060000
*        Set scan state bits to IN_MEMBERLIST                           32070000
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST                    32080000
*                                                                       32090000
*        Get next token, which should be the start of the PDS name      32100000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  32110000
         BASR  R14,R15            * Link to SCAN_TOKEN section          32120000
*                                                                       32130000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 32140000
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function         32150000
*                                                                       32160000
         B     MEMBERLIST_PDSNAME_CHECK_VAR                             32170000
*                                                                       32180000
MEMBERLIST_PDSNAME_NEXT_TOKEN EQU *                                     32190000
*        Get next token, which should be the PDS name                   32200000
         L     R15,LWZMAKE_SCAN_TOKENA_VAR * Get address of SCAN_TOKEN  32210000
         BASR  R14,R15            * Link to SCAN_TOKEN section          32220000
*                                                                       32230000
         CLC   G_RETCODE,=F'0'    * Did an error occur?                 32240000
         BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing function         32250000
*                                                                       32260000
         LT    R14,G_SCAN_TOKEN2_LEN                                    32270000
         IF (NZ) THEN                                                   32280000
            LT    R14,G_SCAN_SPACE_COUNT                                32290000
            IF (NZ) THEN                                                32300000
               MLWZMRPT RPTLINE=CL133'0Only 1 token allowed as pds nameX32310000
                in memberlist function',APND_LC=C'Y'                    32320000
               MVC   G_RETCODE,=F'8' * Set return code 8                32330000
               BR    R8             * and return                        32340000
            ENDIF                                                       32350000
         ENDIF                                                          32360000
*                                                                       32370000
MEMBERLIST_PDSNAME_CHECK_VAR EQU *                                      32380000
*        Check if scan state is IN_VARIABLE, if so link to SCAN_VAR     32390000
*        section to expand it and loop around for the next keyword      32400000
         IC    R14,G_SCAN_STATE   * Get the scan state                  32410000
         N     R14,=X'0000007F'   * Clear out bits 0-56, so including   32420000
*                                 * the high order bit in the scan      32430000
*                                 * state for 'in recipe'               32440000
         C     R14,=A(SCAN_STATE_IN_VARIABLE) * Check for in variable   32450000
         IF (NE) THEN                                                   32460000
            C     R14,=A(SCAN_STATE_IN_VARIABLER)                       32470000
         ENDIF                                                          32480000
         IF (EQ) THEN             * If so...                            32490000
            L     R15,LWZMAKE_SCAN_VARA_VAR * Get address of SCAN_VAR   32500000
            BASR  R14,R15            * Link to SCAN_VAR section         32510000
*                                                                       32520000
            CLC   G_RETCODE,=F'0'    * Did an error occur?              32530000
            BNE   SCAN_MEMBERLIST_RET * Yes, stop parsing statement     32540000
*                                                                       32550000
            B     MEMBERLIST2                                           32560000
         ENDIF                                                          32570000
*                                                                       32580000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        32590000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          32600000
            BE    MEMBERLIST_FINISH                                     32610000
*                                                                       32620000
*           Append token 1 to token 2                                   32630000
            MVC   SCAN_VAR_SAVE_APPEND_TO,G_SCAN_APPEND_TO              32640000
            MVI   G_SCAN_APPEND_TO,X'01'                                32650000
            LT    R1,G_SCAN_TOKEN2_LEN                                  32660000
            IF (Z) THEN                                                 32670000
               MVC   G_SCAN_SPACE_COUNT,=A(0)                           32680000
            ENDIF                                                       32690000
*                                                                       32700000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         32710000
            BASR  R14,R15                                               32720000
*                                                                       32730000
            MVC   G_SCAN_APPEND_TO,SCAN_VAR_SAVE_APPEND_TO              32740000
         ELSE                                                           32750000
            L     R15,LWZMAKE_APPEND_TOKENA_VAR                         32760000
            BASR  R14,R15                                               32770000
*                                                                       32780000
            CLI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          32790000
            BE    MEMBERLIST_FINISH                                     32800000
         ENDIF                                                          32810000
*                                                                       32820000
MEMBERLIST2 EQU *                                                       32830000
*        Clear scan state except for left most bit indicating in recipe 32840000
         NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                        32850000
*        Set scan state bits to IN_MEMBERLIST2                          32860000
         OI    G_SCAN_STATE,SCAN_STATE_IN_MEMBERLIST2                   32870000
*                                                                       32880000
         B     MEMBERLIST_PDSNAME_NEXT_TOKEN                            32890000
*                                                                       32900000
MEMBERLIST_FINISH EQU *                                                 32910000
         LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack     32920000
         XR    R2,R2                 * Clear R2                         32930000
         IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index      32940000
         BCTR  R2,R0                 * Subtract 1 from index            32950000
         IC    R15,0(R2,R1)          * Get stack state in that idx      32960000
         STC   R15,G_SCAN_STATE      * And save it as current state     32970000
         STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx      32980000
*                                                                       32990000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        33000000
*           Get the member list                                         33010000
            L     R15,LWZMAKE_GET_MEMLISTA_VAR * Get addr GET_MEMLIST   33020000
            BASR  R14,R15         * Link to GET_MEMLIST section         33030000
         ENDIF                                                          33040000
*                                                                       33050000
         BAL   R7,SCAN_VAR_RESTORE                                      33060000
*                                                                       33070000
SCAN_MEMBERLIST_RET EQU *                                               33080000
         BR    R8                                                       33090000
*                                                                       33100000
* Save tokens                                                           33110000
*                                                                       33120000
SCAN_VAR_SAVE EQU *                                                     33130000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        33140000
            MVC   SCAN_VAR_SAVE_TOKENA,G_SCAN_TOKENA                    33150000
            MVC   SCAN_VAR_SAVE_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN        33160000
            MVC   SCAN_VAR_SAVE_TOKEN_LEN,G_SCAN_TOKEN_LEN              33170000
            L     R4,G_SCAN_TOKEN_MAXLEN                                33180000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        33190000
            ST    R1,G_SCAN_TOKENA                                      33200000
            MVC   G_SCAN_TOKEN_LEN,=A(0)                                33210000
*                                                                       33220000
            MVC   SCAN_VAR_SAVE_TOKEN2A,G_SCAN_TOKEN2A                  33230000
            MVC   SCAN_VAR_SAVE_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN      33240000
            MVC   SCAN_VAR_SAVE_TOKEN2_LEN,G_SCAN_TOKEN2_LEN            33250000
            L     R4,G_SCAN_TOKEN2_MAXLEN                               33260000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        33270000
            ST    R1,G_SCAN_TOKEN2A                                     33280000
            MVC   G_SCAN_TOKEN2_LEN,=A(0)                               33290000
*                                                                       33300000
            MVC   SCAN_VAR_SAVE_TOKEN3A,G_SCAN_TOKEN3A                  33310000
            MVC   SCAN_VAR_SAVE_TOKEN3_MAXLEN,G_SCAN_TOKEN3_MAXLEN      33320000
            MVC   SCAN_VAR_SAVE_TOKEN3_LEN,G_SCAN_TOKEN3_LEN            33330000
            L     R4,G_SCAN_TOKEN3_MAXLEN                               33340000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        33350000
            ST    R1,G_SCAN_TOKEN3A                                     33360000
            MVC   G_SCAN_TOKEN3_LEN,=A(0)                               33370000
         ENDIF                                                          33380000
*                                                                       33390000
         BR    R7                                                       33400000
*                                                                       33410000
* Restore tokens                                                        33420000
*                                                                       33430000
SCAN_VAR_RESTORE EQU *                                                  33440000
         IF (CLI,G_SCAN_APPEND_TO,EQ,X'00') THEN                        33450000
            L     R2,G_SCAN_TOKEN_MAXLEN                                33460000
            L     R3,G_SCAN_TOKENA                                      33470000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  33480000
*                                                                       33490000
            MVC   G_SCAN_TOKENA,SCAN_VAR_SAVE_TOKENA                    33500000
            MVC   G_SCAN_TOKEN_MAXLEN,SCAN_VAR_SAVE_TOKEN_MAXLEN        33510000
            MVC   G_SCAN_TOKEN_LEN,SCAN_VAR_SAVE_TOKEN_LEN              33520000
*                                                                       33530000
            L     R2,G_SCAN_TOKEN2_MAXLEN                               33540000
            L     R3,G_SCAN_TOKEN2A                                     33550000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  33560000
            MVC   G_SCAN_TOKEN2A,SCAN_VAR_SAVE_TOKEN2A                  33570000
            MVC   G_SCAN_TOKEN2_MAXLEN,SCAN_VAR_SAVE_TOKEN2_MAXLEN      33580000
            MVC   G_SCAN_TOKEN2_LEN,SCAN_VAR_SAVE_TOKEN2_LEN            33590000
*                                                                       33600000
*           Push variable value on to input stack                       33610000
            XR    R2,R2           * Clear R2                            33620000
            XR    R3,R3           * Clear R3                            33630000
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index   33640000
            C     R3,=A(MAX_SCAN_INPUT_STACK_ENTRY) * Will an extra     33650000
*                                 * entry fit?                          33660000
            IF (NL) THEN          * If not write error                  33670000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX33680000
               flow',APND_LC=C'Y'                                       33690000
               MVC   G_RETCODE,=F'12' * Set return code 12              33700000
               B     SCAN_VAR_RET     * and return                      33710000
            ENDIF                                                       33720000
            LA    R3,1(,R3)       * Add 1 to stack size                 33730000
            STC   R3,G_SCAN_INPUT_STACK_IDX * And store it              33740000
            BCTR  R3,R0           * Subtract 1 to calculate offset      33750000
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to new ntry 33760000
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack       33770000
            AR    R2,R3           * Add calculated offset               33780000
*                                                                       33790000
            USING INPUT_DSECT,R2  * Address with INPUT DSECT            33800000
*                                                                       33810000
            MVI   INPUTTYPE,X'01' * Set type of input to ptr to string  33820000
            MVC   INPUTLEAD,G_SCAN_SPACE_COUNT+2                        33830000
            MVC   INPUTLEN,G_SCAN_TOKEN3_LEN+2 * Copy value length      33840000
            MVC   INPUTPTR,G_SCAN_TOKEN3A * Copy value pointer          33850000
            MVC   INPUTPOS,=H'0'  * Set initial scan position to start  33860000
*                                                                       33870000
            DROP  R2                                                    33880000
*                                                                       33890000
            MVC   G_SCAN_TOKEN3A,SCAN_VAR_SAVE_TOKEN3A                  33900000
            MVC   G_SCAN_TOKEN3_MAXLEN,SCAN_VAR_SAVE_TOKEN3_MAXLEN      33910000
            MVC   G_SCAN_TOKEN3_LEN,SCAN_VAR_SAVE_TOKEN3_LEN            33920000
         ENDIF                                                          33930000
*                                                                       33940000
         BR    R7                                                       33950000
*                                                                       33960000
         LTORG                                                          33970000
*                                                                       33980000
* Local constant pointers to section addresses                          33990000
LWZMAKE_APPEND_TOKENA_VAR DC    A(LWZMAKE_APPEND_TOKEN)                 34000000
LWZMAKE_SCAN_TOKENA_VAR   DC    A(LWZMAKE_SCAN_TOKEN)                   34010000
LWZMAKE_FINDVARA_VAR      DC    A(LWZMAKE_FINDVAR)                      34020000
LWZMAKE_SCAN_VARA_VAR     DC    A(LWZMAKE_SCAN_VAR)                     34030000
LWZMAKE_GET_MEMLISTA_VAR  DC    A(LWZMAKE_GET_MEMLIST)                  34040000
*                                                                       34050000
SCAN_VAR_DSECT              DSECT                                       34060000
                            DS    18F * My savearea                     34070000
*                                                                       34080000
SCAN_VAR_SAVE_APPEND_TO     DS    C                                     34090000
*                                                                       34100000
                            DS    0F                                    34110000
SCAN_VAR_SAVE_TOKEN_LEN     DS    F                                     34120000
SCAN_VAR_SAVE_TOKEN2_LEN    DS    F                                     34130000
SCAN_VAR_SAVE_TOKEN3_LEN    DS    F                                     34140000
SCAN_VAR_SAVE_TOKEN_MAXLEN  DS    F                                     34150000
SCAN_VAR_SAVE_TOKEN2_MAXLEN DS    F                                     34160000
SCAN_VAR_SAVE_TOKEN3_MAXLEN DS    F                                     34170000
SCAN_VAR_SAVE_TOKENA        DS    A                                     34180000
SCAN_VAR_SAVE_TOKEN2A       DS    A                                     34190000
SCAN_VAR_SAVE_TOKEN3A       DS    A                                     34200000
*                                                                       34210000
SCAN_VAR_DSECT_SIZ          EQU   *-SCAN_VAR_DSECT                      34220000
*                                                                       34230000
LWZMAKE  CSECT                                                          34240000
*                                                                       34250000
*********************************************************************** 34260000
* Section: LWZMAKE_SCAN_TOKEN                                         * 34270000
* Purpose: Parsing lexer. This section invokes LWZMAKE_SCAN_CHAR to   * 34280000
*          get characetrs in order to return the next keyword. It     * 34290000
*          gets rid of whitespace, but counts the spaces leading the  * 34300000
*          next keyword should those need to be preserved.            * 34310000
*          Based on the current scan state, checks are performed      * 34320000
*          whether a type of token is allowed where it is found, in   * 34330000
*          other words most of the syntax checking is done here.      * 34340000
*          R7 is used as the index register to address the next       * 34350000
*          character position in G_SCAN_TOKEN.                          34360000
*          R9 should point to global data.                            * 34370000
*********************************************************************** 34380000
LWZMAKE_SCAN_TOKEN MLWZSAVE                                             34390000
*        Trace record to start section                                  34400000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX34410000
               KE_SCAN_TOKEN'                                           34420000
*                                                                       34430000
         MVC   G_SCAN_TOKEN_LEN,=F'0' * Initialize token 1 length       34440000
         MVI   G_SCAN_TOKENTYPE,X'00' * Initialize token 1 type         34450000
         MVC   G_SCAN_SPACE_COUNT,=F'0' * Initialize space count        34460000
         XR    R7,R7                  * Initialize index reg to token   34470000
*                                                                       34480000
STATE_AND_SCAN_FOR_WHITESPACE EQU *                                     34490000
*        Translate scan state to bitstring of allowed token types       34500000
         IC    R2,G_SCAN_STATE    * Get the scan state                  34510000
         N     R2,=X'0000007F'    * Clear out bits 0-56, so including   34520000
*                                 * the high order bit in the scan      34530000
*                                 * state for 'in recipe'               34540000
         SLL   R2,2               * Multiply by 4 (scan state table     34550000
*                                 * contains full words)                34560000
         L     R1,SCAN_STATE_TABLEA_TOKEN * Point R1 to scan state tab  34570000
         AR    R1,R2              * Add offset for current scan stata   34580000
         MVC   G_SCAN_EXPECTED,0(R1) * Get the corresponding bitstring  34590000
*                                                                       34600000
SCAN_FOR_WHITESPACE EQU *                                               34610000
         L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address to SCAN_CHAR  34620000
         BASR  R14,R15            * Link to SCAN_CHAR section           34630000
*                                                                       34640000
*        Check for end of file and whether it's expected                34650000
         IF (CLI,G_MKFEOF,EQ,C'Y') THEN * Is EOF switch on?             34660000
*                                 * And was it expected?                34670000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_EOF,Z) THEN           34680000
               MLWZMRPT RPTLINE=CL133'0Unexpected end of file',APND_LC=X34690000
               C'Y'                                                     34700000
               MVC   G_RETCODE,=F'8' * Set return code 8                34710000
            ENDIF                                                       34720000
            B     SCAN_TOKEN_RET  * Skip rest of tokenizer              34730000
         ENDIF                                                          34740000
*                                                                       34750000
*        Check for beginning a new line, when allowed and not in a      34760000
*        continued statement, this resets the scan state to not in stmt 34770000
         IF (CLI,G_SCAN_NEWLINE,EQ,C'Y') THEN * Is new line switch on?  34780000
*                                 * And was it expected?                34790000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_NEWLINE,Z) THEN       34800000
               MLWZMRPT RPTLINE=CL133'0Unexpected new line',APND_LC=C'YX34810000
               '                                                        34820000
               MVC   G_RETCODE,=F'8' * Set return code 8                34830000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           34840000
            ENDIF                                                       34850000
*           Check if we're not in a continued line                      34860000
            IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN                 34870000
               IC    R14,G_SCAN_STATE * Get the scan state              34880000
               N     R14,=X'0000007F' * Clear out bits 0-56, including  34890000
*                                     * the high order bit in the scan  34900000
*                                     * state for 'in recipe'           34910000
               C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in stmt 34920000
               IF (NE) THEN           * If not so...                    34930000
*                 Clear scan state except for left most bit 'in recipe' 34940000
                  NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE               34950000
*                 Set scan state bits to NOT_IN_STMT                    34960000
                  OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT             34970000
*                 Skip to finishing valid token                         34980000
                  B     SCAN_TOKEN_VALID                                34990000
               ENDIF                                                    35000000
            ENDIF                                                       35010000
            B     STATE_AND_SCAN_FOR_WHITESPACE * Loop for next char    35020000
         ENDIF                                                          35030000
*                                                                       35040000
*        Check space, if so add to leading space char count             35050000
         IF (CLI,G_SCAN_CURRCHAR,EQ,C' ') THEN                          35060000
            L     R2,G_SCAN_SPACE_COUNT * Get current space count       35070000
            LA    R2,1(,R2)             * Add 1                         35080000
            ST    R2,G_SCAN_SPACE_COUNT * And put it back               35090000
            B     SCAN_FOR_WHITESPACE   * Loop for next char            35100000
         ENDIF                                                          35110000
*                                                                       35120000
*        Anything beyond column 72 is ignored and considered the end    35130000
*        of a line                                                      35140000
         L     R6,G_SCAN_CURRCOL  * Get current column                  35150000
         C     R6,=F'72'          * Check if we're beyond col 72        35160000
         IF (GE) THEN             * If so (currcol starts with 0)       35170000
*           Set token type to ignore                                    35180000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_IGNORE                35190000
*           Was it expected?                                            35200000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_IGNORE,Z) THEN        35210000
*              Prepare helper data for parse error trace record         35220000
               MVC   G_HELPER_DATA(23),=C'UNEXPECTED END OF LINE '      35230000
               MVC   G_HELPER_DATA+23(1),G_SCAN_STATE                   35240000
               MVI   G_HELPER_DATA+24,C' '                              35250000
               MVC   G_HELPER_DATA+25(4),G_SCAN_EXPECTED                35260000
               LA    R14,G_HELPER_DATA * Get address to helper data     35270000
               ST    R14,G_LWZMTRC_DATA_PTR * Set it as trace data ptr  35280000
               MVC   G_LWZMTRC_DATA_SIZ,=AL2(29) * Set trace data len   35290000
               MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'003',DATA     35300000
               MLWZMRPT RPTLINE=CL133'0Unexpected end of line',APND_LC=X35310000
               C'Y'                                                     35320000
               MVC   G_RETCODE,=F'8' * Set return code 8                35330000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           35340000
            ENDIF                                                       35350000
*                                                                       35360000
CHECK_NEXT_IGNORE_CHAR EQU *                                            35370000
*           Reaching column 80 in combination with not a continued line 35380000
*           resets scan state to not in stmt                            35390000
            L     R6,G_SCAN_CURRCOL * Get current column                35400000
            C     R6,=F'79'         * Check column 80                   35410000
            IF (GE) THEN            * If we're there                    35420000
*                                   * And it's not a continued line     35430000
               IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN              35440000
                  IC    R14,G_SCAN_STATE * Get the scan state           35450000
                  N     R14,=X'0000007F' * Clear out bits 0-56,         35460000
*                                    * including the high order bit in  35470000
*                                    * the scan state for 'in recipe'   35480000
                  C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check          35490000
                  IF (NE) THEN       * not in stmt?                     35500000
*                    Clear scan state except left most bit 'in recipe'  35510000
                     NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE            35520000
*                    Set scan state bits to NOT_IN_STMT                 35530000
                     OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT          35540000
*                    Skip to finishing valid token                      35550000
                     B     SCAN_TOKEN_VALID                             35560000
                  ENDIF                                                 35570000
               ENDIF                                                    35580000
*              Overrule bitstring with that for a new line              35590000
               MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_NEWLINE)          35600000
*              Jump to scan for whitespace, which for a new line after  35610000
*              continuation jumps to STATE_AND_SCAN_FOR_WHITESPACE and  35620000
*              reset to the correct bitstring                           35630000
               B     SCAN_FOR_WHITESPACE                                35640000
            ENDIF                                                       35650000
*           We end up here for chars in columns 72-80                   35660000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  35670000
            BASR  R14,R15          * Link to SCAN_CHAR section          35680000
            CLI   G_MKFEOF,C'Y'    * At this point EOF should           35690000
            BE    SCAN_TOKEN_VALID * return valid ignore token          35700000
            CLI   G_SCAN_NEWLINE,C'Y' * Same goes for new line          35710000
            BE    SCAN_TOKEN_VALID * returns valid ignore token         35720000
            B     CHECK_NEXT_IGNORE_CHAR * None of the above, so loop   35730000
*                                  * around checking next ignore char   35740000
         ENDIF                                                          35750000
*                                                                       35760000
*        At this point we've ruled out EOF, new line and pos 72-80      35770000
*        so we can start checking token types.                          35780000
*        Point R5 to current char to start checking.                    35790000
         LA    R5,G_SCAN_CURRCHAR                                       35800000
*                                                                       35810000
*        Check for comments                                             35820000
         IF (CLI,0(R5),EQ,C'#') THEN                                    35830000
*           Set token type to comment                                   35840000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMENT               35850000
*           Prepare helper data for start parse token trace record      35860000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'612',CONST=C'COX35870000
               MMENT'                                                   35880000
*           Was comment expected? If not, write error and stop          35890000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_COMMENT,Z) THEN       35900000
               MLWZMRPT RPTLINE=CL133'0Unexpected comment',APND_LC=C'Y' 35910000
               MVC   G_RETCODE,=F'8' * Set return code 8                35920000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           35930000
            ENDIF                                                       35940000
CHECK_NEXT_COMMENT_CHAR EQU *                                           35950000
*           Anything up to column 72 is considered part of the comments 35960000
*           after that, if not in continued line we can reset scan      35970000
*           state to not in stmt                                        35980000
            L     R6,G_SCAN_CURRCOL * Get current column                35990000
            C     R6,=F'71'         * Check if we're at 72 yet          36000000
            IF (NL) THEN            * If so...                          36010000
*              And it's not a continued line                            36020000
               IF (CLI,G_SCAN_CONTINUED_LINE,NE,C'Y') THEN              36030000
                  IC    R14,G_SCAN_STATE * Get the scan state           36040000
                  N     R14,=X'0000007F' * Clear out bits 0-56,         36050000
*                                    * including the high order bit in  36060000
*                                    * the scan state for 'in recipe'   36070000
                  C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check          36080000
                  IF (NE) THEN       * not in stmt?                     36090000
*                    Clear scan state except left most bit 'in recipe'  36100000
                     NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE            36110000
*                    Set scan state bits to NOT_IN_STMT                 36120000
                     OI    G_SCAN_STATE,SCAN_STATE_NOT_IN_STMT          36130000
*                    Skip to finishing valid token                      36140000
                     B     SCAN_TOKEN_VALID                             36150000
                  ENDIF                                                 36160000
               ENDIF                                                    36170000
*              Overrule bitstring with that for ignore chars            36180000
               MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_IGNORE)           36190000
*              Jump to scan for whitespace, to check for ignore chars   36200000
*              and newline / EOF                                        36210000
               B     SCAN_FOR_WHITESPACE                                36220000
            ENDIF                                                       36230000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  36240000
            BASR  R14,R15                 * Link to SCAN_CHAR section   36250000
            B     CHECK_NEXT_COMMENT_CHAR * Loop for next comment char  36260000
         ENDIF                                                          36270000
*                                                                       36280000
*        If we ended up here and we've already had line continuation    36290000
*        character, anything other than comment is a syntax error       36300000
         IF (CLI,G_SCAN_CONTINUED_LINE,EQ,C'Y') THEN                    36310000
            MLWZMRPT RPTLINE=CL133'0Syntax error, only comments allowedX36320000
                after continuation character',APND_LC=C'Y'              36330000
            MVC   G_RETCODE,=F'8' * Set return code 8                   36340000
            B     SCAN_TOKEN_RET  * Skip the rest of tokenizer          36350000
         ENDIF                                                          36360000
*                                                                       36370000
*        Check for continuation character                               36380000
         IF (CLI,0(R5),EQ,C'\') THEN                                    36390000
*           Set token type to continuation                              36400000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CONTINUATION          36410000
*           Set switch for continuation to true                         36420000
            MVI   G_SCAN_CONTINUED_LINE,C'Y'                            36430000
*           Overrule bitstring with that for comments                   36440000
            MVC   G_SCAN_EXPECTED,=A(SCAN_EXPECTED_COMMENT)             36450000
*           Jump to scan for whitespace, to look for comments, ignore   36460000
*           chars and newline / EOF                                     36470000
            B     SCAN_FOR_WHITESPACE                                   36480000
         ENDIF                                                          36490000
*                                                                       36500000
*        Only on column 1 check for recipe prefix                       36510000
         L     R6,G_SCAN_CURRCOL  * Get current column                  36520000
         C     R6,=F'0'           * Check for column 1                  36530000
         IF (EQ) THEN             * If so...                            36540000
            CLC   0(1,R5),G_RECIPEPREFIX * Check for recipe prefix char 36550000
            IF (EQ) THEN          * If that's the case...               36560000
*              Set token type to recipe prefix                          36570000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_RECIPEPREFIX       36580000
*              Was it expected? If not, write error and stop            36590000
               IF (TM,G_SCAN_EXPECTED+2,SCAN_EXPECTED3_RECIPREF,Z) THEN 36600000
UNEXPECTED_RECIPE EQU *                                                 36610000
                  MLWZMRPT RPTLINE=CL133'0Unexpected recipe',APND_LC=C'X36620000
               Y'                                                       36630000
                  MVC   G_RETCODE,=F'8' * Set return code 8             36640000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        36650000
               ENDIF                                                    36660000
*              In rare cases the expected bitstring doesn't cover it,   36670000
*              e.g. a recipe line followed by an empty line and         36680000
*              another recipe line. That's why this elaborate check.    36690000
*              If the previous statement wasn't a rule                  36700000
               IF (CLI,G_PREV_STMT_TYPE,NE,STMT_TYPE_RULE) THEN         36710000
*                 and the scan state isn't in recipe                    36720000
                  IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,Z) THEN      36730000
*                    and the previous scan state wasn't either          36740000
                     IF (CLI,G_PREV_STMT_IN_RECIPE,NE,C'Y') THEN        36750000
*                       then also write error and stop                  36760000
                        B     UNEXPECTED_RECIPE                         36770000
                     ENDIF                                              36780000
                  ENDIF                                                 36790000
               ENDIF                                                    36800000
*              If we end up here the recipe prefix was valid, so set    36810000
*              the bit in the scan state                                36820000
               OI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                  36830000
*              Loop around to continue scanning for whitespace          36840000
               B     SCAN_FOR_WHITESPACE                                36850000
            ENDIF                                                       36860000
         ENDIF                                                          36870000
*                                                                       36880000
*        Check for assignment operator                                  36890000
         IF (CLI,0(R5),EQ,C'=') THEN                                    36900000
*           Set token type to operator                                  36910000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_OPERATOR              36920000
*           Was it expected? If not, write error and stop               36930000
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_OPERATOR,Z) THEN    36940000
               MLWZMRPT RPTLINE=CL133'0Unexpected operator',APND_LC=C'YX36950000
               '                                                        36960000
               MVC   G_RETCODE,=F'8' * Set return code 8                36970000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           36980000
            ENDIF                                                       36990000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             37000000
            B     SCAN_TOKEN_VALID   * Skip to finishing valid token    37010000
         ENDIF                                                          37020000
*                                                                       37030000
*        Check for colon, which could be a rule or byte 1 of :=         37040000
         IF (CLI,0(R5),EQ,C':') THEN                                    37050000
*           Set token type to rule (check for := follows)               37060000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_RULE                  37070000
*           If the next char will return =                              37080000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'=') THEN                       37090000
*              Check if an operator is expected, if not error and stop  37100000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_OPERATOR,Z) THEN 37110000
                  MLWZMRPT RPTLINE=CL133'0Unexpected operator',APND_LC=X37120000
               C'Y'                                                     37130000
                  MVC   G_RETCODE,=F'8' * Set return code 8             37140000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        37150000
               ENDIF                                                    37160000
            ELSE                                                        37170000
*              If next char is not =, so it's a rule                    37180000
*              Check if a rule is expected, if not error and stop       37190000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_RULE,Z) THEN     37200000
                  MLWZMRPT RPTLINE=CL133'0Unexpected colon',APND_LC=C'YX37210000
               '                                                        37220000
                  MVC   G_RETCODE,=F'8' * Set return code 8             37230000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        37240000
               ENDIF                                                    37250000
            ENDIF                                                       37260000
*           If we end up here the rule or operator was valid            37270000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             37280000
*           Check for := on pos 71 (making = an ignore char)            37290000
            L     R6,G_SCAN_CURRCOL * Get current column                37300000
            C     R6,=F'71'         * Check for pos 72                  37310000
            BNL   SCAN_TOKEN_VALID  * Skip to finishing valid token     37320000
*           Check again for next char =                                 37330000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'=') THEN                       37340000
*              Set token type to operator                               37350000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_OPERATOR           37360000
               L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get addr SCAN_CHAR  37370000
               BASR  R14,R15             * Link to SCAN_CHAR section    37380000
               BAL   R8,STORE_TOKEN_CHAR * Add char to token 1          37390000
            ENDIF                                                       37400000
            B     SCAN_TOKEN_VALID * Skip to finishing valid token      37410000
         ENDIF                                                          37420000
*                                                                       37430000
*        Check for a variable                                           37440000
         IF (CLI,0(R5),EQ,C'$') THEN                                    37450000
*           Set token type to variable (check for $@ or $% follows)     37460000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_VARIABLE              37470000
*           Was it expected? If not, write error and stop               37480000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_OPENVAR,Z) THEN       37490000
               MLWZMRPT RPTLINE=CL133'0Unexpected variable or function'X37500000
               ,APND_LC=C'Y'                                            37510000
               MVC   G_RETCODE,=F'8' * Set return code 8                37520000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           37530000
            ENDIF                                                       37540000
*                                                                       37550000
*           Check if next char will be @                                37560000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'@') THEN                       37570000
*              Set token type to target variable                        37580000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_ACRO               37590000
*              Was it expected? If so, continue checking because the    37600000
*              expected bitstring isn't complete                        37610000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_ACRO,O) THEN     37620000
*                 Check if in recipe                                    37630000
                  TM    G_SCAN_STATE,SCAN_STATE_IN_RECIPE               37640000
                  IF (Z) THEN     * Not in recipe, keep on checking     37650000
*                    Check if in expand (resolving variables in rule    37660000
*                    requisites during phase 2)                         37670000
                     TM    G_SCAN_STATE,SCAN_STATE_IN_EXPAND            37680000
                  ENDIF                                                 37690000
*                 If either of the tests above gives CC ones            37700000
                  IF (O) THEN                                           37710000
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1    37720000
                     L     R15,LWZMAKE_SCAN_CHARA_TOKEN                 37730000
                     BASR  R14,R15             * Link to SCAN_CHAR      37740000
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1    37750000
                     B     SCAN_TOKEN_VALID    * Skip to finish token   37760000
                  ENDIF                                                 37770000
               ENDIF                                                    37780000
*              If not expected or the other tests above fail, write     37790000
*              error and stop                                           37800000
               MLWZMRPT RPTLINE=CL133'0Unexpected target variable',APNDX37810000
               _LC=C'Y'                                                 37820000
               MVC   G_RETCODE,=F'8' * Set return code 8                37830000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           37840000
            ENDIF                                                       37850000
*                                                                       37860000
*           Check if next char will be %                                37870000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'%') THEN                       37880000
*              Set token type to target member variable                 37890000
               MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_PERCENT            37900000
*              Was it expected? If so, continue checking because the    37910000
*              expected bitstring isn't complete                        37920000
               IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_PERCENT,O) THEN  37930000
*                 Check if in recipe                                    37940000
                  TM    G_SCAN_STATE,SCAN_STATE_IN_RULE2                37950000
                  IF (Z) THEN     * Not in rule2, keep on checking      37960000
                     TM    G_SCAN_STATE,SCAN_STATE_IN_RULE3             37970000
                  ENDIF                                                 37980000
                  IF (Z) THEN     * Not in rule3, keep op checking      37990000
*                    Check if in expand (resolving variables in rule    38000000
*                    requisites during phase 2)                         38010000
                     TM    G_SCAN_STATE,SCAN_STATE_IN_EXPAND            38020000
                  ENDIF                                                 38030000
*                 If either of the tests above gives CC ones            38040000
                  IF (O) THEN                                           38050000
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1    38060000
                     L     R15,LWZMAKE_SCAN_CHARA_TOKEN                 38070000
                     BASR  R14,R15             * Link to SCAN_CHAR      38080000
                     BAL   R8,STORE_TOKEN_CHAR * Add char to token 1    38090000
                     B     SCAN_TOKEN_VALID    * Skip to finish token   38100000
                  ENDIF                                                 38110000
               ENDIF                                                    38120000
*              If not expected or the other tests above fail, write     38130000
*              error and stop                                           38140000
               MLWZMRPT RPTLINE=CL133'0Unexpected target member variablX38150000
               e',APND_LC=C'Y'                                          38160000
               MVC   G_RETCODE,=F'8' * Set return code 8                38170000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           38180000
            ENDIF                                                       38190000
*                                                                       38200000
*           So it's not $@ or $%, continue checking normal variable     38210000
*           If we're at pos 71 or more, no sense checking the rest      38220000
*           because there's not enough room for a variable              38230000
            L     R6,G_SCAN_CURRCOL * Get current column                38240000
            C     R6,=F'70'         * Check for pos 71 or above         38250000
            IF (NL) THEN            * If so write error and stop        38260000
               MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'       38270000
               MVC   G_RETCODE,=F'8' * Set return code 8                38280000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           38290000
            ENDIF                                                       38300000
*                                                                       38310000
*           Check if next char will be either ( or {                    38320000
            IF (CLI,G_SCAN_PEEKCHAR,EQ,C'(') THEN                       38330000
*              Save matching close bracket to check later               38340000
               MVI   G_SCAN_CLOSE_BRACKET,C')'                          38350000
            ELSE                                                        38360000
               IF (CLI,G_SCAN_PEEKCHAR,EQ,C'{') THEN                    38370000
*                 Save matching close bracket to check later            38380000
                  MVI   G_SCAN_CLOSE_BRACKET,C'}'                       38390000
               ELSE                                                     38400000
*                 Neither ( nor { is a syntax error                     38410000
                  MLWZMRPT RPTLINE=CL133'0Syntax error',APND_LC=C'Y'    38420000
                  MVC   G_RETCODE,=F'8' * Set return code 8             38430000
                  B     SCAN_TOKEN_RET  * Skip rest of tokenizer        38440000
               ENDIF                                                    38450000
            ENDIF                                                       38460000
*                                                                       38470000
*           Valid variable start, so store in token 1                   38480000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             38490000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  38500000
            BASR  R14,R15             * Link to SCAN_CHAR section       38510000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             38520000
*                                                                       38530000
*           The next bit of code is a dirty trick because determining   38540000
*           if the scan state should be set to rule should really only  38550000
*           be done in the statement parser section.                    38560000
*           The thing is, when a variable is the first token the scan   38570000
*           state should be IN_RULE before pushing it on the stack and  38580000
*           setting it to IN_VARIABLE(R)                                38590000
            IC    R14,G_SCAN_STATE * Get the scan state                 38600000
            N     R14,=X'0000007F' * Clear out bits 0-56, including     38610000
*                                  * the high order bit in the scan     38620000
*                                  * state for 'in recipe'              38630000
            C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in stmt    38640000
            IF (EQ) THEN           * If so...                           38650000
*              Clear scan state except for left most bit 'in recipe'    38660000
               NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                  38670000
*              Set scan state bits to IN_RULE                           38680000
               OI    G_SCAN_STATE,SCAN_STATE_IN_RULE                    38690000
            ENDIF                                                       38700000
*                                                                       38710000
*           Push current scan state on the stack before setting         38720000
*           it to IN_VARIABLE(R)                                        38730000
            LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack  38740000
            XR    R2,R2                 * Clear R2                      38750000
            IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index   38760000
            IC    R15,G_SCAN_STATE      * Get current scan state        38770000
            STC   R15,0(R2,R1)          * and store it in the stack     38780000
            LA    R2,1(,R2)             * Add 1 to stack index          38790000
            C     R2,=A(L'G_SCAN_STATE_STACK) * Is stack full?          38800000
            IF (H) THEN                 * Yep, write error and stop     38810000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX38820000
               flow',APND_LC=C'Y'                                       38830000
               MVC   G_RETCODE,=F'12'   * Set return code 12            38840000
               B     SCAN_TOKEN_RET     * Skip rest of tokenizer        38850000
            ENDIF                                                       38860000
            STC   R2,G_SCAN_STATE_STACK_IDX * Store new stack size      38870000
*                                                                       38880000
*           Clear scan state except left most bit 'in recipe', which    38890000
*           sets CC                                                     38900000
            NI    G_SCAN_STATE,SCAN_STATE_IN_RECIPE                     38910000
            IF (Z) THEN           * If in recipe bit is off             38920000
*              Set scan state bits to IN_VARIABLE                       38930000
               OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLE                38940000
            ELSE                  * Else if recipe bit is on            38950000
*              Set scan state bits to IN_VARIABLER                      38960000
               OI    G_SCAN_STATE,SCAN_STATE_IN_VARIABLER               38970000
            ENDIF                                                       38980000
            B     SCAN_TOKEN_VALID * Skip to finishing valid token      38990000
         ENDIF                                                          39000000
*                                                                       39010000
*        Check for a closing bracket                                    39020000
         CLI   0(R5),C')'          * Is it a )                          39030000
         IF (NE) THEN              * If not                             39040000
            CLI   0(R5),C'}'       * Is it a }                          39050000
         ENDIF                                                          39060000
         IF (EQ) THEN              * If it was ) or }                   39070000
*           Set token type to closing bracket                           39080000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CLOSEBRACKET          39090000
*           Was if expected? If not, write error and stop               39100000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_CLOSEBRC,Z) THEN      39110000
UNEXPECTED_CLOSE_BRACKET EQU *                                          39120000
               MLWZMRPT RPTLINE=CL133'0Unexpected close bracket',APND_LX39130000
               C=C'Y'                                                   39140000
               MVC   G_RETCODE,=F'8' * Set return code 8                39150000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           39160000
            ENDIF                                                       39170000
*                                                                       39180000
*           Check if close bracket matches previous open bracket        39190000
            CLC   0(1,R5),G_SCAN_CLOSE_BRACKET                          39200000
            BNE UNEXPECTED_CLOSE_BRACKET                                39210000
*                                                                       39220000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             39230000
*                                                                       39240000
*           Check if scan state is form of IN_VARIABLE*, if not it's    39250000
*           just a closing bracket, meaning no pop of scan state stack  39260000
            IC    R14,G_SCAN_STATE * Get the scan state                 39270000
            N     R14,=X'0000007F' * Clear out bits 0-56, including     39280000
*                                  * the high order bit in the scan     39290000
*                                  * state for 'in recipe'              39300000
            C     R14,=A(SCAN_STATE_IN_VARIABLE2) * Check in variable2  39310000
            IF (NE) THEN           * If not, check in variableR         39320000
               C     R14,=A(SCAN_STATE_IN_VARIABLER)                    39330000
               BNE   SCAN_TOKEN_VALID * Neither, so skip to finish      39340000
            ENDIF                                                       39350000
            LA    R1,G_SCAN_STATE_STACK * Point R1 to scan state stack  39360000
            XR    R2,R2                 * Clear R2                      39370000
            IC    R2,G_SCAN_STATE_STACK_IDX * Get current stack index   39380000
            BCTR  R2,R0                 * Subtract 1 from index         39390000
            IC    R15,0(R2,R1)          * Get stack state in that idx   39400000
            STC   R15,G_SCAN_STATE      * And save it as current state  39410000
            STC   R2,G_SCAN_STATE_STACK_IDX * Also save new stack idx   39420000
            B     SCAN_TOKEN_VALID      * Skip to finishing valid token 39430000
         ENDIF                                                          39440000
*                                                                       39450000
*        Check for a comma                                              39460000
         CLI   0(R5),C','          * Is it a ,                          39470000
         IF (EQ) THEN              * If it was ) or }                   39480000
*           Set token type to comma                                     39490000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_COMMA                 39500000
*           Was if expected? If not, write error and stop               39510000
            IF (TM,G_SCAN_EXPECTED+2,SCAN_EXPECTED3_COMMA,Z) THEN       39520000
               MLWZMRPT RPTLINE=CL133'0Unexpected comma',APND_LC=C'Y'   39530000
               MVC   G_RETCODE,=F'8' * Set return code 8                39540000
               B     SCAN_TOKEN_RET  * Skip rest of tokenizer           39550000
            ENDIF                                                       39560000
*                                                                       39570000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             39580000
            B     SCAN_TOKEN_VALID      * Skip to finishing valid token 39590000
         ENDIF                                                          39600000
*                                                                       39610000
*        Special variables are the first token in a new statement, so   39620000
*        if we're already in one, skip the special variable check       39630000
         IC    R14,G_SCAN_STATE   * Get the scan state                  39640000
         N     R14,=X'0000007F'   * Clear out bits 0-56, including      39650000
*                                 * the high order bit in the scan      39660000
*                                 * state for 'in recipe'               39670000
         C     R14,=A(SCAN_STATE_NOT_IN_STMT) * Check not in statement  39680000
         BNE   SKIP_SCAN_SPECIAL  * If not skip special var check       39690000
*                                                                       39700000
*        Check for special variable. This snippet only checks for a     39710000
*        variable that starts with . and consists of valid special var  39720000
*        characters. Whether it's semantically valid is checked in the  39730000
*        statement parser.                                              39740000
         IF (CLI,0(R5),EQ,C'.') THEN                                    39750000
*           Set token type to special variable                          39760000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_SPECIAL               39770000
*           Was it expected? If not, write error and stop               39780000
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_SPECIAL,Z) THEN     39790000
               MLWZMRPT RPTLINE=CL133'0Unexpected special variable',APNX39800000
               D_LC=C'Y'                                                39810000
               MVC   G_RETCODE,=F'8' * Set return code 8                39820000
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer       39830000
            ENDIF                                                       39840000
*                                                                       39850000
STORE_NEXT_SPECIAL_TOKEN_CHAR EQU *                                     39860000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             39870000
*                                                                       39880000
*           Positions up to 72 count as part of the special var name    39890000
*           We check peek char so when the next token is parsed,        39900000
*           scanning continues with the char directly after the special 39910000
*           variable name                                               39920000
            L     R6,G_SCAN_CURRCOL * Get current column                39930000
            C     R6,=F'71'         * Check pos 72 and if we're there   39940000
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token     39950000
*                                                                       39960000
            LA    R2,G_SCAN_PEEKCHAR * Point R2 to peek char            39970000
            TRT   0(1,R2),SPECIAL_TOKEN_NEXTCHAR * Check for valid char 39980000
            BNZ   SCAN_TOKEN_VALID  * If not, skip to finish token      39990000
*           Ending up here means the char is valid, to scan it and      40000000
*           loop around to store it and check the next                  40010000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  40020000
            BASR  R14,R15           * Link to SCAN_CHAR section         40030000
            B     STORE_NEXT_SPECIAL_TOKEN_CHAR * Loop around           40040000
         ENDIF                                                          40050000
*                                                                       40060000
SKIP_SCAN_SPECIAL EQU *                                                 40070000
*                                                                       40080000
*        Check for normal token                                         40090000
         TRT   0(1,R5),NORMAL_TOKEN_STARTCHAR                           40100000
         IF (Z) THEN                                                    40110000
*           Set token type to normal                                    40120000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_NORMAL                40130000
*           Was it expected? If not, write error and stop               40140000
            IF (TM,G_SCAN_EXPECTED,SCAN_EXPECTED1_NORMAL,Z) THEN        40150000
               MLWZMRPT RPTLINE=CL133'0Unexpected token',APND_LC=C'Y'   40160000
               MVC   G_RETCODE,=F'8' * Set return code 8                40170000
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer       40180000
            ENDIF                                                       40190000
*                                                                       40200000
STORE_NEXT_NORMAL_TOKEN_CHAR EQU *                                      40210000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             40220000
*                                                                       40230000
*           Positions up to 72 count as part of the normal token name   40240000
*           We check peek char so when the next token is parsed,        40250000
*           scanning continues with the char directly after the normal  40260000
*           token name                                                  40270000
            L     R6,G_SCAN_CURRCOL * Get current column                40280000
            C     R6,=F'71'         * Check pos 72 and if we're there   40290000
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token     40300000
*                                                                       40310000
            LA    R2,G_SCAN_PEEKCHAR * Point R2 to peek char            40320000
            TRT   0(1,R2),NORMAL_TOKEN_NEXTCHAR * Check for valid char  40330000
            BNZ   SCAN_TOKEN_VALID   * If not, skip to finish token     40340000
*           Ending up here means the char is valid, to scan it and      40350000
*           loop around to store it and check the next                  40360000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  40370000
            BASR  R14,R15         * Link to SCAN_CHAR section           40380000
            B     STORE_NEXT_NORMAL_TOKEN_CHAR * Loop around            40390000
         ENDIF                                                          40400000
*                                                                       40410000
*        Check for number token                                         40420000
         TRT   0(1,R5),NUMBER_TOKEN_CHAR                                40430000
         IF (Z) THEN                                                    40440000
*           Set token type to number                                    40450000
            MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_NUMBER                40460000
*           Was it expected? If not, write error and stop               40470000
            IF (TM,G_SCAN_EXPECTED+1,SCAN_EXPECTED2_NUMBER,Z) THEN      40480000
               MLWZMRPT RPTLINE=CL133'0Unexpected number',APND_LC=C'Y'  40490000
               MVC   G_RETCODE,=F'8' * Set return code 8                40500000
               B     SCAN_TOKEN_RET  * Skip the rest of tokenizer       40510000
            ENDIF                                                       40520000
*                                                                       40530000
STORE_NEXT_NUMBER_TOKEN_CHAR EQU *                                      40540000
            BAL   R8,STORE_TOKEN_CHAR * Add char to token 1             40550000
*                                                                       40560000
*           Positions up to 72 count as part of the number token name   40570000
*           We check peek char so when the next token is parsed,        40580000
*           scanning continues with the char directly after the number  40590000
*           token name                                                  40600000
            L     R6,G_SCAN_CURRCOL * Get current column                40610000
            C     R6,=F'71'         * Check pos 72 and if we're there   40620000
            BNL   SCAN_TOKEN_VALID  * skip to finishing valid token     40630000
*                                                                       40640000
            LA    R5,G_SCAN_PEEKCHAR * Point R2 to peek char            40650000
            TRT   0(1,R5),NUMBER_TOKEN_CHAR * Check for valid char      40660000
            BNZ   SCAN_TOKEN_VALID   * If not, skip to finish token     40670000
*           Ending up here means the char is valid, to scan it and      40680000
*           loop around to store it and check the next                  40690000
            L     R15,LWZMAKE_SCAN_CHARA_TOKEN * Get address SCAN_CHAR  40700000
            BASR  R14,R15         * Link to SCAN_CHAR section           40710000
            B     STORE_NEXT_NUMBER_TOKEN_CHAR * Loop around            40720000
         ENDIF                                                          40730000
*                                                                       40740000
*        If the scanned character doesn't start any of the previous     40750000
*        types, just store the character, don't set a type and just let 40760000
*        the parser take care of it                                     40770000
         BAL   R8,STORE_TOKEN_CHAR * Add char to token 1                40780000
*                                                                       40790000
*        Previous code jumps here when a valid done is completely       40800000
*        scanned                                                        40810000
SCAN_TOKEN_VALID EQU *                                                  40820000
*        If normal token, it could be a call keyword                    40830000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_NORMAL) THEN        40840000
            CLC   G_SCAN_TOKEN_LEN,=F'4' * Is the token 4 bytes long?   40850000
            IF (EQ) THEN                                                40860000
               L     R14,G_SCAN_TOKENA    * Point R14 to token 1        40870000
               MVC   G_CALL4,0(R14)       * Copy to helper var          40880000
               OC    G_CALL4,=X'40404040' * Convert to uppercase        40890000
               CLC   G_CALL4,=C'CALL'     * Is it CALL?                 40900000
               IF (EQ) THEN               * If so...                    40910000
*                 Set token type to call                                40920000
                  MVI   G_SCAN_TOKENTYPE,SCAN_TOKENTYPE_CALL            40930000
               ENDIF                                                    40940000
            ENDIF                                                       40950000
         ENDIF                                                          40960000
*                                                                       40970000
*        Write a trace record for the token we just scanned             40980000
*        Put token type char and scanned token in helper data           40990000
         MVC   G_HELPER_DATA(1),G_SCAN_TOKENTYPE * Store token type     41000000
         MVI   G_HELPER_DATA+1,C' '  * followed by space                41010000
         LA    R2,G_HELPER_DATA+2    * Point R2 to where token comes    41020000
         L     R3,G_SCAN_TOKENA      * Point R3 to scanned token        41030000
         L     R4,G_SCAN_TOKEN_LEN   * Get length of scanned token      41040000
         C     R4,=A(L'G_HELPER_DATA-2) * Compare it to leftover space  41050000
         IF (H) THEN                 * If too long                      41060000
            L     R4,=A(L'G_HELPER_DATA-2) * Replace len with what fits 41070000
         ENDIF                                                          41080000
         LA    R5,2(,R4)             * Put the correct length in R5     41090000
         BCTR  R4,R0                 * R4 = R4 - 1 for EX               41100000
         B     *+10                  * Skip MVC constant for EX         41110000
         MVC   0(1,R2),0(R3)         * MVC constant for EX              41120000
         EX    R4,*-6                * EX previous MVC stmt with R4     41130000
         LA    R2,G_HELPER_DATA      * Get address of helper data       41140000
         ST    R2,G_LWZMTRC_DATA_PTR * And store it as trace data ptr   41150000
         STH   R5,G_LWZMTRC_DATA_SIZ * And store length as data length  41160000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'610',DATA          41170000
*                                                                       41180000
SCAN_TOKEN_RET EQU *                                                    41190000
         MLWZTERM                 * Return back to caller               41200000
*                                                                       41210000
* Store token char routine, appends a scanned char to token 1           41220000
* R7 is expected to contain the position within token 1 where the next  41230000
* character should be stored (used as an index register).               41240000
*                                                                       41250000
STORE_TOKEN_CHAR EQU *                                                  41260000
         C     R7,=A(SCAN_TOKEN_MAXLEN) * Have we exhausted token 1?    41270000
         IF (GT) THEN             * If so, write error and stop         41280000
            MLWZMRPT RPTLINE=CL133'0Internal error, no more room left iX41290000
               n 4K token space',APND_LC=C'Y'                           41300000
            MVC   G_RETCODE,=F'12' * Set return code 12                 41310000
            B     SCAN_TOKEN_RET   * Dirty jump to end of tokenizer     41320000
         ENDIF                     * but quicker than check retcode     41330000
*                                  * everytime after this routine       41340000
         L     R4,G_SCAN_TOKENA   * Point R4 to token 1                 41350000
         IC    R5,G_SCAN_CURRCHAR * Get the last scanned character      41360000
         STC   R5,0(R7,R4)        * And store it at index R7            41370000
         LA    R7,1(,R7)          * Increase index R7 with 1            41380000
         ST    R7,G_SCAN_TOKEN_LEN * and store it as token 1 length     41390000
         BR    R8                 * Return                              41400000
*                                                                       41410000
         LTORG                                                          41420000
*                                                                       41430000
* Local constant pointers to section addresses                          41440000
LWZMAKE_SCAN_CHARA_TOKEN     DC    A(LWZMAKE_SCAN_CHAR)                 41450000
*                                                                       41460000
* Local constant pointers to previously defined constants, but too far  41470000
* away for local addressing                                             41480000
SCAN_STATE_TABLEA_TOKEN      DC    A(SCAN_STATE_TABLE)                  41490000
*                                                                       41500000
* Translate table for starting character for a normal token             41510000
* Can be [a-zA-Z]                                                       41520000
NORMAL_TOKEN_STARTCHAR DS 0F                                            41530000
         DC    256X'FF'                                                 41540000
         ORG   NORMAL_TOKEN_STARTCHAR+C'a'                              41550000
         DC    X'000000000000000000'                                    41560000
         ORG   NORMAL_TOKEN_STARTCHAR+C'j'                              41570000
         DC    X'000000000000000000'                                    41580000
         ORG   NORMAL_TOKEN_STARTCHAR+C's'                              41590000
         DC    X'0000000000000000'                                      41600000
         ORG   NORMAL_TOKEN_STARTCHAR+C'A'                              41610000
         DC    X'000000000000000000'                                    41620000
         ORG   NORMAL_TOKEN_STARTCHAR+C'J'                              41630000
         DC    X'000000000000000000'                                    41640000
         ORG   NORMAL_TOKEN_STARTCHAR+C'S'                              41650000
         DC    X'0000000000000000'                                      41660000
         ORG                                                            41670000
*                                                                       41680000
* Translate table for any character for a normal token except the first 41690000
* Can be [$#@_a-zA-Z0-9]                                                41700000
NORMAL_TOKEN_NEXTCHAR DS 0F                                             41710000
         DC    256X'FF'                                                 41720000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'$'                               41730000
         DC    X'00'                                                    41740000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'#'                               41750000
         DC    X'00'                                                    41760000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'@'                               41770000
         DC    X'00'                                                    41780000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'_'                               41790000
         DC    X'00'                                                    41800000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'a'                               41810000
         DC    X'000000000000000000'                                    41820000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'j'                               41830000
         DC    X'000000000000000000'                                    41840000
         ORG   NORMAL_TOKEN_NEXTCHAR+C's'                               41850000
         DC    X'0000000000000000'                                      41860000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'A'                               41870000
         DC    X'000000000000000000'                                    41880000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'J'                               41890000
         DC    X'000000000000000000'                                    41900000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'S'                               41910000
         DC    X'0000000000000000'                                      41920000
         ORG   NORMAL_TOKEN_NEXTCHAR+C'0'                               41930000
         DC    X'00000000000000000000'                                  41940000
         ORG                                                            41950000
*                                                                       41960000
* Translate table for any character for a special token                 41970000
* Can be [_A-Z]                                                         41980000
SPECIAL_TOKEN_NEXTCHAR DS 0F                                            41990000
         DC    256X'FF'                                                 42000000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'_'                              42010000
         DC    X'00'                                                    42020000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'A'                              42030000
         DC    X'000000000000000000'                                    42040000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'J'                              42050000
         DC    X'000000000000000000'                                    42060000
         ORG   SPECIAL_TOKEN_NEXTCHAR+C'S'                              42070000
         DC    X'0000000000000000'                                      42080000
         ORG                                                            42090000
*                                                                       42100000
* Translate table for any character for a number token                  42110000
* Can be [0-9]                                                          42120000
NUMBER_TOKEN_CHAR DS 0F                                                 42130000
         DC    256X'FF'                                                 42140000
         ORG   NUMBER_TOKEN_CHAR+C'0'                                   42150000
         DC    X'00000000000000000000'                                  42160000
         ORG                                                            42170000
*                                                                       42180000
*********************************************************************** 42190000
* Section: LWZMAKE_SCAN_CHAR                                          * 42200000
* Purpose: Parsing scanner. This section gets the next character from * 42210000
*          whichever input is at the top of the input stack. For a    * 42220000
*          makefile, after column 80 a next record is read, any other * 42230000
*          type of input is simply read until its last char. If an    * 42240000
*          input is exhausted it is popped from the input stack.      * 42250000
*          R9 should point to global data.                            * 42260000
*********************************************************************** 42270000
LWZMAKE_SCAN_CHAR MLWZSAVE                                              42280000
*        Trace record to start section                                  42290000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEEBUG,MSGNR=C'604',CONST=C'LWZMX42300000
               AKE_SCAN_CHAR'                                           42310000
*                                                                       42320000
         MVI   G_SCAN_NEWLINE,C'N' * Initialize newline                 42330000
         MVC   G_SCAN_CURRCHAR(2),=X'0000' * Init CURRCHAR + PEEKCHAR   42340000
*                                                                       42350000
SCAN_CHAR_CHECK_INPUT_STACK EQU *                                       42360000
*        Check for empty input stack, of so set EOF and skip the rest   42370000
         IF (CLI,G_SCAN_INPUT_STACK_IDX,EQ,X'00') THEN                  42380000
            MVI   G_MKFEOF,C'Y'   * Set EOF switch to Y                 42390000
            B     SCAN_CHAR_RET   * Skip rest of scanner                42400000
         ELSE                                                           42410000
*           Get the top entry in the input stack                        42420000
            XR    R2,R2           * Clear R2                            42430000
            XR    R3,R3           *   and R3                            42440000
            IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack index   42450000
            BCTR  R3,R0           * Subtract 1 for calculating offset   42460000
            M     R2,=A(INPUT_DSECT_SIZ) * Calculate offset to entry    42470000
            LA    R2,G_SCAN_INPUT_STACK * Point R2 to input stack       42480000
            AR    R2,R3           * Add calculated offset               42490000
*                                                                       42500000
            USING INPUT_DSECT,R2  * Address with INPUT DSECT            42510000
*                                                                       42520000
            CLI   INPUTTYPE,X'00' * Check for input from makefile       42530000
            BE    SCAN_CHAR_READ_FROM_MAKEFILE * If so jump ahead       42540000
*                                                                       42550000
            XR    R3,R3                                                 42560000
            LH    R3,INPUTLEAD                                          42570000
            CH    R3,=H'0'                                              42580000
            IF (NE) THEN                                                42590000
               MVI   G_SCAN_CURRCHAR,C' '                               42600000
               BCTR  R3,R0                                              42610000
               STH   R3,INPUTLEAD                                       42620000
               B     SCAN_CHAR_RET                                      42630000
            ENDIF                                                       42640000
*                                                                       42650000
            XR    R3,R3           * Clear R3                            42660000
            LH    R3,INPUTPOS     * Get next position in input          42670000
            CH    R3,INPUTLEN     * Has input been exhausted?           42680000
            IF (L) THEN           * If not...                           42690000
               L     R4,INPUTPTR  * Point R4 to input                   42700000
               IC    R5,0(R3,R4)  * Get the next character from input   42710000
               STC   R5,G_SCAN_CURRCHAR * and put it in CURRCHAR        42720000
               LA    R3,1(,R3)    * Advance next position               42730000
               STH   R3,INPUTPOS  * and store it in input block         42740000
               CH    R3,INPUTLEN  * Was this the last char?             42750000
               IF (L) THEN        * If not, also get PEEKCHAR           42760000
                  IC    R5,0(R3,R4) * Get the next char + 1 from input  42770000
                  STC   R5,G_SCAN_PEEKCHAR * and put it in PEEKCHAR     42780000
               ENDIF                                                    42790000
               B     SCAN_CHAR_RET * Skip rest of scanner               42800000
            ELSE                  * Else, input exhausted               42810000
               XR    R3,R3        * Clear R3                            42820000
               IC    R3,G_SCAN_INPUT_STACK_IDX * Get current stack idx  42830000
               BCTR  R3,R0        * Subtract 1                          42840000
               STC   R3,G_SCAN_INPUT_STACK_IDX * and put back in stack  42850000
               CLI   INPUTTYPE,X'01' * Input type string that continues 42860000
*                                 * with popped stack entry?            42870000
               BNE   SCAN_CHAR_RET * If not, skip rest of scan char     42880000
               B     SCAN_CHAR_CHECK_INPUT_STACK * Loop around to       42890000
*                                 * check the input stack again         42900000
            ENDIF                                                       42910000
         ENDIF                                                          42920000
         DROP  R2                                                       42930000
*                                                                       42940000
SCAN_CHAR_READ_FROM_MAKEFILE EQU *                                      42950000
*        A makefile has LRECL 80 but in order to return a line change   42960000
*        as a separate token, a fictitious 81st column is used for that 42970000
*        When we go beyond column 81 that triggers a next record to be  42980000
*        read from the makefile.                                        42990000
         L     R3,G_SCAN_CURRCOL  * Get the current column              43000000
         LA    R3,1(,R3)          * Advance 1 position                  43010000
         ST    R3,G_SCAN_CURRCOL  * and put it back                     43020000
         C     R3,=F'80'          * Check if we're past 81 yet          43030000
         IF (GT) THEN             * If so...                            43040000
            MVI   G_SCAN_CONTINUED_LINE,C'N' * Reset continued line     43050000
            BAL   R8,READNEXT     * Read another input record           43060000
         ELSE                     * Not past 81                         43070000
            IF (EQ) THEN          * Buf if exactly at 81                43080000
               MVI   G_SCAN_NEWLINE,C'Y' * Set new line switch to Y     43090000
               B     SCAN_CHAR_RET       * Skip rest of scanner         43100000
            ENDIF                                                       43110000
         ENDIF                                                          43120000
*                                                                       43130000
*        Check for EOF                                                  43140000
         CLI   G_MKFEOF,C'Y'      * Is the EOF switch set?              43150000
         BE    SCAN_CHAR_RET      * If so skip rest of scanner          43160000
*                                                                       43170000
*        If we're here, we're scanning a column between 1 and 80        43180000
         LA    R4,G_MAKEFILE_REC  * Point R4 to the last read record    43190000
         L     R3,G_SCAN_CURRCOL  * Get the current column              43200000
         IC    R5,0(R3,R4)        * Get the next character from input   43210000
         STC   R5,G_SCAN_CURRCHAR * and put it in CURRCHAR              43220000
         C     R3,=F'79'          * Check pos = 80                      43230000
         IF (LT) THEN             * Only pos < 80 there's a PEEKCHAR    43240000
            IC    R5,1(R3,R4)     * Get the next char + 1 from input    43250000
            STC   R5,G_SCAN_PEEKCHAR * and put it in PEEKCHAR           43260000
         ENDIF                                                          43270000
*                                                                       43280000
*        The rest of this scanner code is for writing a deeebug trace   43290000
         IF (CLI,G_LWZMAKE_TRACE,NL,LWZMAKE_TRACE_DEEEBUG) THEN         43300000
            LA    R2,G_HELPER_DATA   * Point R2 to helper data          43310000
            MVC   0(5,R2),=C'LINE '  * Start with line constant         43320000
            LA    R2,5(,R2)          * Advance R2 to right after        43330000
            LA    R6,5               * Use R6 as length                 43340000
            L     R3,G_SCAN_CURRLINE * Get the current line number      43350000
            CVD   R3,G_DEC8          * Convert it to packed decimal     43360000
            UNPK  G_ZONED8,G_DEC8    * Convert it to zoned              43370000
            OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble           43380000
            MVC   0(8,R2),G_ZONED8   * Append line number to trc data   43390000
            LA    R2,8(,R2)          * Advance R2 to right after        43400000
            LA    R6,8(,R6)          * Add 8 to length                  43410000
            MVC   0(8,R2),=C' COLUMN ' * Append column constant         43420000
            LA    R2,8(,R2)          * Advance R2 to right after        43430000
            LA    R6,8(,R6)          * Add 8 to length                  43440000
            L     R3,G_SCAN_CURRCOL  * Get the current column numer     43450000
            LA    R3,1(,R3)          * Add 1 because it's zero based    43460000
            CVD   R3,G_DEC8          * Convert it to packed decimal     43470000
            UNPK  G_ZONED8,G_DEC8    * Convert it to zoned              43480000
            OI    G_ZONED8+7,X'F0'   * Get rid of sign nibble           43490000
            MVC   0(8,R2),G_ZONED8   * Append column number to trc data 43500000
            LA    R2,8(,R2)          * Advance R2 to right after        43510000
            LA    R6,8(,R6)          * Add 8 to length                  43520000
            MVI   0(R2),C' '         * Append a space                   43530000
            MVC   1(1,R2),G_SCAN_CURRCHAR * Append current char         43540000
            MVI   2(R2),C' '         * Append another space             43550000
            MVC   3(1,R2),G_SCAN_PEEKCHAR * Append peek char            43560000
            LA    R6,4(,R6)          * Add 4 to length                  43570000
            LA    R2,G_HELPER_DATA   * Get address of helper data       43580000
            ST    R2,G_LWZMTRC_DATA_PTR * Set is as trace data pointer  43590000
            STH   R6,G_LWZMTRC_DATA_SIZ * Store R6 as trace data length 43600000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEEBUG,MSGNR=C'611',DATA      43610000
         ENDIF                                                          43620000
*                                                                       43630000
SCAN_CHAR_RET EQU *                                                     43640000
         MLWZTERM                 * Return back to caller               43650000
*                                                                       43660000
* Read next MAKEFILE record                                             43670000
*                                                                       43680000
READNEXT EQU   *                                                        43690000
         L     R14,G_DCB_MEM_PTR  * Get DCB memory pointer              43700000
         LA    R3,DCBMKF-DCB_DSECT(,R14) * Get addr of MAKEFILE DCB     43710000
         LA    R7,READNEXT_10     * Set R7 to statement EODAD routine   43720000
*                                 * branches to at EOF                  43730000
         GET   (R3),G_MAKEFILE_REC * Get a record from makefile         43740000
READNEXT_10 EQU *                                                       43750000
         IF (CLI,G_MKFEOF,NE,C'Y') THEN * Did we hit EOF? If not        43760000
            MVC   G_SCAN_CURRCOL,=F'0' * Reset current column to 0      43770000
            L     R4,G_SCAN_CURRLINE * Get the current line             43780000
            LA    R4,1(,R4)          * Advance 1 line count             43790000
            ST    R4,G_SCAN_CURRLINE * And put it back as current line  43800000
         ENDIF                                                          43810000
*                                                                       43820000
         BR    R8                 * Return                              43830000
*                                                                       43840000
         LTORG                                                          43850000
*********************************************************************** 43860000
* Section: LWZMAKE_ALLOC_STMT                                         * 43870000
* Purpose: Allocate a memory block for an internal representation of  * 43880000
*          a statement. This section also fills in the generic part   * 43890000
*          of the allocated statement block, including chaining into  * 43900000
*          the linked list of statements.                             * 43910000
*          R9 should point to global data.                            * 43920000
*********************************************************************** 43930000
LWZMAKE_ALLOC_STMT MLWZSAVE                                             43940000
*        Trace record to start section                                  43950000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX43960000
               KE_ALLOC_STMT'                                           43970000
*                                                                       43980000
         MVC   G_STMT_ALLOC_RETURN_PTR,=A(0) * Initialize return ptr    43990000
*                                                                       44000000
         MVC   G_STMT_SAVE_PTR,=A(0) * Initialize helper pointer        44010000
*                                                                       44020000
         LA    R7,G_STMT_LIST_PTR    * Get start of statement linked    44030000
*                                    * list                             44040000
FIND_STMT_SLOT EQU *                                                    44050000
         CLC   0(4,R7),=A(0)         * Have we reached an empty ptr?    44060000
         IF (NZ) THEN                * If not, go down the chain        44070000
            L     R6,0(,R7)          * Get the actual pointer           44080000
            ST    R6,G_STMT_SAVE_PTR * Save for back chain              44090000
            LA    R7,STMT_NEXT_PTR-STMT_DSECT(,R6) * Get next stmt ptr  44100000
            B     FIND_STMT_SLOT     * and loop around to check it      44110000
         ENDIF                                                          44120000
*                                                                       44130000
*        Allocate a statement block                                     44140000
         L     R4,G_STMT_ALLOC_LEN  * Get length to allocate            44150000
         STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block           44160000
         ST    R1,0(,R7)            * Save it at forward chain address  44170000
         ST    R1,G_STMT_ALLOC_RETURN_PTR * Also save it as return ptr  44180000
*                                                                       44190000
*        Write a trace record for allocated block                       44200000
         ST    R1,G_DEC8             * Put in var with at least 5 bytes 44210000
         UNPK  G_ZONED8(9),G_DEC8(5) * Turn into almost hex             44220000
         TR    G_ZONED8,STMT_HEXTAB  * Turn into hex                    44230000
         MVC   G_HELPER_DATA(8),G_ZONED8 * Copy 8 hex chars to helper   44240000
         LA    R2,G_HELPER_DATA      * Get address of helper data       44250000
         ST    R2,G_LWZMTRC_DATA_PTR * put in in trace record data ptr  44260000
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace record data length 8  44270000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'640',DATA           44280000
*                                                                       44290000
*        Initialize block                                               44300000
         L     R2,G_STMT_ALLOC_RETURN_PTR * Point R2 to memory block    44310000
         L     R3,G_STMT_ALLOC_LEN  * Get length of block               44320000
         XR    R0,R0                * Clear R0                          44330000
         XR    R1,R1                *   and R1                          44340000
         MVCL  R2,R0                * Zero out memory block             44350000
*                                                                       44360000
*        Fill in generic stuff                                          44370000
         L     R1,G_STMT_ALLOC_RETURN_PTR                               44380000
*        Fill in block length and statement type                        44390000
         MVC   STMT_LEN-STMT_DSECT(4,R1),G_STMT_ALLOC_LEN               44400000
         MVC   STMT_TYPE-STMT_DSECT(1,R1),G_STMT_ALLOC_TYPE             44410000
*        Fill in switch for statement in recipe                         44420000
         IF (TM,G_SCAN_STATE,SCAN_STATE_IN_RECIPE,O) THEN               44430000
            MVI   STMT_IN_RECIPE-STMT_DSECT(R1),C'Y'                    44440000
         ELSE                                                           44450000
            MVI   STMT_IN_RECIPE-STMT_DSECT(R1),C'N'                    44460000
         ENDIF                                                          44470000
*        Fill in back chain statement pointer                           44480000
         MVC   STMT_PREV_PTR-STMT_DSECT(4,R1),G_STMT_SAVE_PTR           44490000
*                                                                       44500000
ALLOC_STMT_RET EQU *                                                    44510000
         MLWZTERM                 * Return back to caller               44520000
*                                                                       44530000
         LTORG                                                          44540000
*                                                                       44550000
* Translate table for hex conversion                                    44560000
STMT_HEXTAB EQU   *-C'0'                                                44570000
            DC    C'0123456789ABCDEF'                                   44580000
*********************************************************************** 44590000
* Section: LWZMAKE_STORE_VAR                                          * 44600000
* Purpose: Assign a value to a variable. The binary search tree is    * 44610000
*          searched, if the variable is found the new value is        * 44620000
*          assigned. If not found, a new variable memory block is     * 44630000
*          allocated, added to the binary search tree and then the    * 44640000
*          value is assigned.                                         * 44650000
*          R7 should point to a statement block for an assignment     * 44660000
*          R9 should point to global data.                            * 44670000
*********************************************************************** 44680000
LWZMAKE_STORE_VAR MLWZSAVE                                              44690000
*        Trace record to start section                                  44700000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX44710000
               KE_STORE_VAR'                                            44720000
*                                                                       44730000
         USING STMT_A_DSECT,R7    * Use R7 for addressing statement     44740000
         USING VAR_DSECT,R6       * Use R6 for addressing variable      44750000
*                                                                       44760000
         LT    R6,G_FIRST_VAR_PTR * Get the start of the search tree    44770000
         IF (Z) THEN              * If this is the first var            44780000
            BAL   R8,ALLOC_VAR    * Perform allocate variable routine   44790000
            LR    R6,R1           * Copy returned pointer to R6         44800000
            ST    R6,G_FIRST_VAR_PTR * And set it as start of tree      44810000
         ELSE                     * Else, search tree not empty         44820000
TEST_VARS   EQU   *               * Test this var for matching name     44830000
            XR    R3,R3           * Clear R3                            44840000
            LH    R3,STMT_A_DESTLEN * and get length of stmt var name   44850000
            CH    R3,VARLEN       * Compare it to current var name len  44860000
            IF (H) THEN           * Compare length of shortest of the 2 44870000
               LH    R3,VARLEN    * If current var name len is less     44880000
            ENDIF                 * then use that                       44890000
            BCTR  R3,R0           * Subtract 1 for EX                   44900000
            LA    R2,STMT_A_DEST  * Point R2 to statement var name      44910000
            LA    R4,VARNAME      * Point R4 to current var name        44920000
            B     *+10            * Skip CLC constant for EX            44930000
            CLC   0(1,R2),0(R4)   * CLC constant for EX                 44940000
            EX    R3,*-6          * EX previous CLC stmt with R3        44950000
            IF (L) THEN           * If statement var name is lower      44960000
               CLC   VARLOW,=A(0) * Check if current var's low ptr set  44970000
               IF (EQ) THEN       * If not                              44980000
                  BAL   R8,ALLOC_VAR * Perform allocate var routine     44990000
                  ST    R1,VARLOW * Store returned pointer as low ptr   45000000
                  LR    R6,R1     * And set it as the current var ptr   45010000
                  B     FILL_VAR  * Skip to filling variable value      45020000
               ELSE               * Else, current var's low ptr is set  45030000
                  L     R6,VARLOW * Replace current var by low var      45040000
                  B     TEST_VARS * Loop around to test that one        45050000
               ENDIF                                                    45060000
            ELSE                  * Else, statement var name is >=      45070000
               IF (EQ) THEN       * If the compared part is equal       45080000
                  CLC   STMT_A_DESTLEN,VARLEN * Check if they're also   45090000
                  IF (EQ) THEN    * of equal length                     45100000
                     B     FILL_VAR * because then no alloc needed,     45110000
                  ENDIF           * just replacing the value            45120000
               ENDIF                                                    45130000
*              If we end up here, it's not a match                      45140000
               CLC   VARHIGH,=A(0) * Check if curr var's high ptr set   45150000
               IF (EQ) THEN       * If not                              45160000
                  BAL   R8,ALLOC_VAR * Perform allocate var routine     45170000
                  ST    R1,VARHIGH * Store returned pointer as high ptr 45180000
                  LR    R6,R1     * And set it as the current var ptr   45190000
                  B     FILL_VAR  * Skip to filling variable value      45200000
               ELSE               * Else, current var's high ptr is set 45210000
                  L     R6,VARHIGH * Replace current var by high var    45220000
                  B     TEST_VARS * Loop around to test that one        45230000
               ENDIF                                                    45240000
            ENDIF                                                       45250000
         ENDIF                                                          45260000
*                                                                       45270000
* Set a value, either in a new variable just allocated, or replacing a  45280000
* value of an existing variable                                         45290000
FILL_VAR EQU   *                                                        45300000
         CLC   VALLEN,=AL2(0)     * Check if there's an existing value  45310000
         IF (NE) THEN             * If so...                            45320000
            XR    R2,R2           * Clear R2                            45330000
            LH    R2,VALLEN       * and put old value length in         45340000
            L     R3,VALPTR       * Get old value pointer               45350000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  45360000
         ENDIF                                                          45370000
         XR    R2,R2              * Clear R2                            45380000
         LH    R2,STMT_A_SRCLEN   * Get new value length                45390000
         STH   R2,VALLEN          * Put it in variable block            45400000
         STORAGE OBTAIN,LENGTH=(R2) * Allocate value storage            45410000
         ST    R1,VALPTR          * Put new memory ptr in var block     45420000
         LR    R0,R1              * Copy new value ptr to R0            45430000
         LR    R1,R2              * Copy new value length to R1         45440000
         LR    R3,R1              * Make sure no cropping/filling       45450000
         LA    R2,STMT_A_SRC      * Point R2 to new value               45460000
         MVCL  R0,R2              * Copy new value to variable value    45470000
*                                                                       45480000
*        Write a report line with variable assignment                   45490000
         MVC   G_LWZMRPT_LINE,=CL133' .....................'            45500000
         LA    R2,G_LWZMRPT_LINE+1                                      45510000
         LA    R3,21                                                    45520000
         LA    R4,VARNAME                                               45530000
         LR    R5,R3                                                    45540000
         CH    R5,VARLEN                                                45550000
         IF (H) THEN                                                    45560000
            LH    R5,VARLEN                                             45570000
         ENDIF                                                          45580000
         BCTR  R5,R0                                                    45590000
         B     *+10                                                     45600000
         MVC   0(1,R2),0(R4)                                            45610000
         EX    R5,*-6                                                   45620000
         LA    R2,G_LWZMRPT_LINE+23                                     45630000
         LA    R3,110                                                   45640000
         L     R4,VALPTR                                                45650000
         LR    R5,R3                                                    45660000
         CH    R5,VALLEN                                                45670000
         IF (H) THEN                                                    45680000
            LH    R5,VALLEN                                             45690000
         ENDIF                                                          45700000
         LTR   R5,R5                                                    45710000
         BZ    STORE_VAR_WRITE_RPT                                      45720000
         BCTR  R5,R0                                                    45730000
         B     *+10                                                     45740000
         MVC   0(1,R2),0(R4)                                            45750000
         EX    R5,*-6                                                   45760000
STORE_VAR_WRITE_RPT EQU *                                               45770000
         L     R15,G_LWZMAKE_RPTA                                       45780000
         BASR  R14,R15                                                  45790000
*                                                                       45800000
         DROP  R6                                                       45810000
*                                                                       45820000
STORE_VAR_RET EQU *                                                     45830000
         MLWZTERM                 * Return back to caller               45840000
*                                                                       45850000
* Allocate a variable block. Value for variable is allocated separately 45860000
* The using for R7 in previous code is left intact to keep addressing   45870000
* of the assignment statement possible.                                 45880000
*                                                                       45890000
ALLOC_VAR EQU  *                                                        45900000
         GETMAIN RU,LV=VAR_DSECT_LEN * Allocate memory for var block    45910000
*                                                                       45920000
         LR    R5,R1              * Copy allocated memory ptr to R5     45930000
         USING VAR_DSECT,R5       * Addressing of new variable block    45940000
*                                                                       45950000
         MVC   VARLEN,STMT_A_DESTLEN * Copy variable name length        45960000
         MVC   VARNAME,STMT_A_DEST * Copy variable name (both 72 long)  45970000
*        Set the rest of the variable block to zero's                   45980000
         MVI   VALLEN,X'00'                                             45990000
         MVC   VALLEN+1(VAR_DSECT_LEN-L'VARLEN-L'VARNAME-1),VALLEN      46000000
*                                                                       46010000
*        Write a trace record for allocated block                       46020000
         ST    R5,G_DEC8             * Put in var with at least 5 bytes 46030000
         UNPK  G_ZONED8(9),G_DEC8(5) * Turn into almost hex             46040000
         TR    G_ZONED8,VAR_HEXTAB   * Turn into hex                    46050000
         MVC   G_HELPER_DATA(8),G_ZONED8 * Copy 8 hex chars to helper   46060000
         LA    R2,G_HELPER_DATA      * Get address of helper data       46070000
         ST    R2,G_LWZMTRC_DATA_PTR * put in in trace record data ptr  46080000
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(8) * Trace record data length 8  46090000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEBUG,MSGNR=C'642',DATA           46100000
*                                                                       46110000
         LR    R1,R5              * Put ptr of allocated block in R1    46120000
*                                                                       46130000
         DROP  R5                                                       46140000
*                                                                       46150000
         BR    R8                 * Return                              46160000
*                                                                       46170000
         DROP  R7                                                       46180000
*                                                                       46190000
         LTORG                                                          46200000
*                                                                       46210000
* Translate table for hex conversion                                    46220000
VAR_HEXTAB EQU   *-C'0'                                                 46230000
           DC    C'0123456789ABCDEF'                                    46240000
*                                                                       46250000
*********************************************************************** 46260000
* Section: LWZMAKE_STORE_TGT                                          * 46270000
* Purpose: Store a target in binary search tree. The binary search    * 46280000
*          tree is searched, but an existing entry won't be replaced, * 46290000
*          the tree can contain duplicate entries. At the correct     * 46300000
*          place a new allocated memory block is inserted and filled  * 46310000
*          with the target data.                                      * 46320000
*          R7 should point to a rule statement block                  * 46330000
*          R9 should point to global data.                            * 46340000
*          Token 1 contains a single space delimited target name      * 46350000
*********************************************************************** 46360000
LWZMAKE_STORE_TGT MLWZSAVE                                              46370000
*        Trace record to start section                                  46380000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX46390000
               KE_STORE_TGT'                                            46400000
*                                                                       46410000
         USING TARGET_DSECT,R6    * Use R6 for addressing variable      46420000
*                                                                       46430000
         LT    R6,G_FIRST_TGT_PTR * Get the start of the search tree    46440000
         IF (Z) THEN              * If this is the first target         46450000
            BAL   R8,ALLOC_TGT    * Perform allocate target routine     46460000
            ST    R1,G_FIRST_TGT_PTR * Store return ptr start of tree   46470000
            B     STORE_TGT_RET   * Skip rest of store target           46480000
         ELSE                     * Else, search tree not empty         46490000
TEST_TGTS   EQU   *               * Test this target for matching name  46500000
            L     R3,G_SCAN_TOKEN_LEN * Get new target name length      46510000
            CH    R3,TGTNAMELEN   * Compare it to current tgt name len  46520000
            IF (H) THEN           * Compare length of shortest of the 2 46530000
               LH    R3,TGTNAMELEN * If current tgt name len is less    46540000
            ENDIF                 * then use that                       46550000
            BCTR  R3,R0           * Subtract 1 for EX                   46560000
            L     R2,G_SCAN_TOKENA * Point R2 to new target name        46570000
            LA    R4,TGTNAME      * Point R4 to current target name     46580000
            B     *+10            * Skip CLC constant for EX            46590000
            CLC   0(1,R2),0(R4)   * CLC constant for EX                 46600000
            EX    R3,*-6          * EX previous CLC stmt with R3        46610000
            IF (L) THEN           * If new target name is lower         46620000
               CLC   TGTLOW,=A(0) * Check if current tgt's low ptr set  46630000
               IF (EQ) THEN       * If not                              46640000
                  BAL   R8,ALLOC_TGT * Perform allocate tgt routine     46650000
                  ST    R1,TGTLOW * Store returned poitner as low ptr   46660000
                  B     STORE_TGT_RET * Skip rest of store target       46670000
               ELSE               * Else, current tgt's low ptr is set  46680000
                  L     R6,TGTLOW * Replace current tgt by low tgt      46690000
                  B     TEST_TGTS * Loop around to test that one        46700000
               ENDIF                                                    46710000
            ELSE                  * Else, new target name is >=         46720000
               CLC   TGTHIGH,=A(0) * Check if curr tgt's high ptr set   46730000
               IF (EQ) THEN       * If not                              46740000
                  BAL   R8,ALLOC_TGT * Perform allocate tgt routine     46750000
                  ST    R1,TGTHIGH * Store returned pointer as high ptr 46760000
                  B     STORE_TGT_RET * Skip rest of store target       46770000
               ELSE               * Else, current tgt's high ptr is set 46780000
                  L     R6,TGTHIGH * Replace current tgt by high tgt    46790000
                  B     TEST_TGTS * Loop around to test that one        46800000
               ENDIF                                                    46810000
            ENDIF                                                       46820000
         ENDIF                                                          46830000
*                                                                       46840000
         DROP  R6                                                       46850000
*                                                                       46860000
STORE_TGT_RET EQU *                                                     46870000
         MLWZTERM                 * Return back to caller               46880000
*                                                                       46890000
* Allocate a target block                                               46900000
* R7 is left intact pointing to the rule statement where this target    46910000
* originated                                                            46920000
*                                                                       46930000
ALLOC_TGT EQU  *                                                        46940000
         L     R3,=A(TARGET_DSECT_LEN) * Get size of fixed part of tgt  46950000
         A     R3,G_SCAN_TOKEN_LEN  * Add target name length            46960000
         A     R3,=A(8)           * Add length for optional member name 46970000
*                                                                       46980000
         STORAGE OBTAIN,LENGTH=(R3) *Allocate memory for tgt block      46990000
*                                                                       47000000
         USING TARGET_DSECT,R1    * Addressing of new target block      47010000
*                                                                       47020000
*        Clear memory block                                             47030000
         LR    R2,R1              * Copy ptr to target block to R2      47040000
         XR    R4,R4              * Clear R4                            47050000
         XR    R5,R5              *   and R5                            47060000
         MVCL  R2,R4              * Zero out memory                     47070000
*                                                                       47080000
*        Fill in new target block                                       47090000
         L     R3,=A(TARGET_DSECT_LEN) * Get size of fixed part of tgt  47100000
         A     R3,G_SCAN_TOKEN_LEN  * Add target name length            47110000
         A     R3,=A(8)           * Add length for optional member name 47120000
         ST    R3,TGTLEN          * Store total block length            47130000
         L     R2,G_SCAN_TOKEN_LEN * Get length of target name          47140000
         STH   R2,TGTNAMELEN      * Store target name length in block   47150000
         ST    R7,TGTSTMT         * Store target stmt ptr in block      47160000
         LA    R2,TGTNAME         * Point R2 to target name in block    47170000
         L     R3,G_SCAN_TOKEN_LEN * Get target name length             47180000
         L     R4,G_SCAN_TOKENA   * Point R4 to target name in token 1  47190000
         LR    R5,R3              * Make sure no cropping/filling       47200000
         MVCL  R2,R4              * Copy target name to block           47210000
*                                                                       47220000
         DROP  R1                                                       47230000
*                                                                       47240000
         BR    R8                 * Return                              47250000
*                                                                       47260000
*********************************************************************** 47270000
* Section: LWZMAKE_STORE_PNY                                          * 47280000
* Purpose: Store a PHONY in the binary search tree. Duplicates are    * 47290000
*          simply ignored. Every new phony is allocated a new phony   * 47300000
*          memory block, which is added to the binary search tree.    * 47310000
*          R7 should point to a statement block for a PHONY           * 47320000
*          R9 should point to global data.                            * 47330000
*********************************************************************** 47340000
LWZMAKE_STORE_PNY MLWZSAVE                                              47350000
*        Trace record to start section                                  47360000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX47370000
               KE_STORE_PNY'                                            47380000
*                                                                       47390000
         USING STMT_P_DSECT,R7    * Use R7 for addressing PHONY stmt    47400000
         USING PHONY_DSECT,R6     * Use R6 for addressing PHONY         47410000
*                                                                       47420000
         LT    R6,G_FIRST_PNY_PTR * Get the start of the search tree    47430000
         IF (Z) THEN              * If this is the first PHONY          47440000
            BAL   R8,ALLOC_PNY    * Perform allocate PHONY routine      47450000
            LR    R6,R1           * Copy returned pointer to R6         47460000
            ST    R6,G_FIRST_PNY_PTR * And set it as start of tree      47470000
         ELSE                     * Else, search tree not empty         47480000
TEST_PNYS   EQU   *               * Test this PHONY for matching name   47490000
            XR    R3,R3           * Clear R3                            47500000
            LH    R3,STMT_P_PNYLEN * and get length of stmt PNONY name  47510000
            CH    R3,PNYNAMELEN   * Compare it to current PHONY len     47520000
            IF (H) THEN           * Compare length of shortest of the 2 47530000
               LH    R3,PNYNAMELEN * If current PHONY name len is less  47540000
            ENDIF                 * then use that                       47550000
            BCTR  R3,R0           * Subtract 1 for EX                   47560000
            LA    R2,STMT_P_PNY   * Point R2 to statement PHONY name    47570000
            LA    R4,PNYNAME      * Point R4 to current PHONY name      47580000
            B     *+10            * Skip CLC constant for EX            47590000
            CLC   0(1,R2),0(R4)   * CLC constant for EX                 47600000
            EX    R3,*-6          * EX previous CLC stmt with R3        47610000
            IF (L) THEN           * If statement PHONY name is lower    47620000
               CLC   PNYLOW,=A(0) * Check if current PHONY low ptr set  47630000
               IF (EQ) THEN       * If not                              47640000
                  BAL   R8,ALLOC_PNY * Perform allocate PHONY routine   47650000
                  ST    R1,PNYLOW * Store returned pointer as low ptr   47660000
                  LR    R6,R1     * And set it as the current PHONY ptr 47670000
                  B     FILL_PNY  * Skip to writing report line         47680000
               ELSE               * Else, current PHONY low ptr is set  47690000
                  L     R6,PNYLOW * Replace current PHONY by low PHONY  47700000
                  B     TEST_PNYS * Loop around to test that one        47710000
               ENDIF                                                    47720000
            ELSE                  * Else, statement PHONY name is >=    47730000
               IF (EQ) THEN       * If the compared part is equal       47740000
                  CLC   STMT_P_PNYLEN,PNYNAMELEN * Check if they're     47750000
                  IF (EQ) THEN    * also of equal length                47760000
                     B     STORE_PNY_RET * we're done                   47770000
                  ENDIF                                                 47780000
               ENDIF                                                    47790000
*              If we end up here, it's not a match                      47800000
               CLC   PNYHIGH,=A(0) * Check if curr PHONY high ptr set   47810000
               IF (EQ) THEN       * If not                              47820000
                  BAL   R8,ALLOC_PNY * Perform allocate PHONY routine   47830000
                  ST    R1,PNYHIGH * Store returned pointer as high ptr 47840000
                  LR    R6,R1     * And set it as the current PHONY ptr 47850000
                  B     FILL_PNY  * Skip to writing report line         47860000
               ELSE               * Else, current PHONY high ptr is set 47870000
                  L     R6,PNYHIGH * Replace current PHONY by high      47880000
                  B     TEST_PNYS * Loop around to test that one        47890000
               ENDIF                                                    47900000
            ENDIF                                                       47910000
         ENDIF                                                          47920000
*                                                                       47930000
* Fill a new PHONY name                                                 47940000
FILL_PNY EQU   *                                                        47950000
*                                                                       47960000
*        Write a report line with PHONY                                 47970000
         MVC   G_LWZMRPT_LINE,=CL133' Phony target ........'            47980000
         LA    R2,G_LWZMRPT_LINE+23                                     47990000
         LA    R3,110                                                   48000000
         LA    R4,PNYNAME                                               48010000
         LR    R5,R3                                                    48020000
         CH    R5,PNYNAMELEN                                            48030000
         IF (H) THEN                                                    48040000
            LH    R5,PNYNAMELEN                                         48050000
         ENDIF                                                          48060000
         BCTR  R5,R0                                                    48070000
         B     *+10                                                     48080000
         MVC   0(1,R2),0(R4)                                            48090000
         EX    R5,*-6                                                   48100000
         L     R15,G_LWZMAKE_RPTA                                       48110000
         BASR  R14,R15                                                  48120000
*                                                                       48130000
         DROP  R6                                                       48140000
*                                                                       48150000
STORE_PNY_RET EQU *                                                     48160000
         MLWZTERM                 * Return back to caller               48170000
*                                                                       48180000
* Allocate a PHONY block.                                               48190000
* The using for R7 in previous code is left intact to keep addressing   48200000
* of the PHONY statement possible.                                      48210000
*                                                                       48220000
ALLOC_PNY EQU  *                                                        48230000
         L     R3,=A(PHONY_DSECT_LEN) * Get size of fixed part of PHONY 48240000
         AH    R3,STMT_P_PNYLEN   * Add PHONY name length               48250000
*                                                                       48260000
         STORAGE OBTAIN,LENGTH=(R3) * Allocate memory for PHONY block   48270000
*                                                                       48280000
         USING PHONY_DSECT,R1     * Addressing of new PHONY block       48290000
*                                                                       48300000
*        Clear memory block                                             48310000
         LR    R2,R1              * Copy ptr to target block to R2      48320000
         XR    R4,R4              * Clear R4                            48330000
         XR    R5,R5              *   and R5                            48340000
         MVCL  R2,R4              * Zero out memory                     48350000
*                                                                       48360000
*        Fill in new target block                                       48370000
         L     R3,=A(PHONY_DSECT_LEN) * Get size of fixed part of PHONY 48380000
         AH    R3,STMT_P_PNYLEN   * Add PHONY name length               48390000
         ST    R3,PNYLEN          * Store total block length            48400000
         XR    R3,R3              * Clear R2                            48410000
         LH    R3,STMT_P_PNYLEN   * Get length of PHONY name            48420000
         STH   R3,PNYNAMELEN      * Store PHONY name length in block    48430000
         ST    R7,PNYSTMT         * Store PHONY stmt ptr in block       48440000
         LA    R2,PNYNAME         * Point R2 to PHONY name in block     48450000
         LA    R4,STMT_P_PNY      * Point R4 to PHONY name in stmt      48460000
         LR    R5,R3              * Make sure no cropping/filling       48470000
         MVCL  R2,R4              * Copy PHONY name to block            48480000
*                                                                       48490000
         DROP  R1                                                       48500000
*                                                                       48510000
         BR    R8                 * Return                              48520000
*                                                                       48530000
*********************************************************************** 48540000
* Section: LWZMAKE_FINDVAR                                            * 48550000
* Purpose: Find a variable in variable binary search tree             * 48560000
*          R9 should point to global data.                            * 48570000
*********************************************************************** 48580000
LWZMAKE_FINDVAR MLWZSAVE                                                48590000
*        Trace record to start section                                  48600000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX48610000
               KE_FINDVAR'                                              48620000
*                                                                       48630000
         MVC   G_FOUND_VAR_PTR,=A(0) * Initialize return pointer        48640000
*                                                                       48650000
         USING VAR_DSECT,R6       * Use R6 for addressing variable      48660000
*                                                                       48670000
         LT    R6,G_FIRST_VAR_PTR * Get start of search tree for vars   48680000
         IF (Z) THEN              * If no vars in tree                  48690000
            B     FINDVAR_RET     * Skip rest of find var               48700000
         ENDIF                                                          48710000
TEST_VARF   EQU   *                                                     48720000
         XR    R3,R3              * Clear R3                            48730000
         LH    R3,G_SRCH_VAR_LEN  * Get var to find's name length       48740000
         CH    R3,VARLEN          * Compare it to current var name len  48750000
         IF (H) THEN              * Compare length of shortest of the 2 48760000
            LH    R3,VARLEN       * If current var name len is less     48770000
         ENDIF                    * then use that                       48780000
         BCTR  R3,R0              * Subtract 1 for EX                   48790000
         LA    R2,G_SRCH_VAR      * Point R2 to var to find's name      48800000
         LA    R4,VARNAME         * Point R4 to current var name        48810000
         B     *+10               * Skip CLC constant for EX            48820000
         CLC   0(1,R2),0(R4)      * CLC constant for EX                 48830000
         EX    R3,*-6             * EX previous CLC stmt with R3        48840000
         IF (L) THEN              * If find var name is lower           48850000
            CLC   VARLOW,=A(0)    * Check if current var's low ptr set  48860000
            IF (EQ) THEN          * If not                              48870000
               B     FINDVAR_RET  * Var not found, skip rest of findvar 48880000
            ELSE                  * Else, current var's low ptr set     48890000
               L     R6,VARLOW    * Replace current var by low var      48900000
               B     TEST_VARF    * Loop around to test that one        48910000
            ENDIF                                                       48920000
         ELSE                     * Else, find var name is >=           48930000
            IF (EQ) THEN          * If the compared part is equal       48940000
               CLC   G_SRCH_VAR_LEN,VARLEN * Check if they're also of   48950000
               IF (EQ) THEN       * equal length                        48960000
                  ST    R6,G_FOUND_VAR_PTR * Return this var's ptr      48970000
                  B     FINDVAR_RET * Skip rest of find var             48980000
               ENDIF                                                    48990000
            ENDIF                                                       49000000
*           If we end up here, it's not a match                         49010000
            CLC   VARHIGH,=A(0)   * Check if curr var's high ptr set    49020000
            IF (EQ) THEN          * If not                              49030000
               B     FINDVAR_RET  * Var not found, skip rest of findvar 49040000
            ELSE                  * Else, current var's high ptr set    49050000
               L     R6,VARHIGH   * Replace current var by high var     49060000
               B     TEST_VARF    * Loop around to test that one        49070000
            ENDIF                                                       49080000
         ENDIF                                                          49090000
*                                                                       49100000
         DROP  R6                                                       49110000
*                                                                       49120000
FINDVAR_RET EQU *                                                       49130000
         MLWZTERM                 * Return back to caller               49140000
*                                                                       49150000
         LTORG                                                          49160000
*********************************************************************** 49170000
* Section: LWZMAKE_FINDTGT                                            * 49180000
* Purpose: Find a target in target binary search tree                 * 49190000
*          The name of the target to find is in token 1.              * 49200000
*          R9 should point to global data.                            * 49210000
*********************************************************************** 49220000
LWZMAKE_FINDTGT MLWZSAVE                                                49230000
*        Trace record to start section                                  49240000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX49250000
               KE_FINDTGT'                                              49260000
*                                                                       49270000
         MVC   G_FOUND_TGT_PTR,=A(0) * Initialize return pointer        49280000
*                                                                       49290000
         USING TARGET_DSECT,R6    * Use R6 for addressing target        49300000
*                                                                       49310000
         LT    R6,G_FIRST_TGT_PTR * Get start of search tree for tgts   49320000
         IF (Z) THEN              * If no tgts in tree                  49330000
            B     FINDTGT_RET     * Skip rest of find tgt               49340000
         ENDIF                                                          49350000
TEST_TGTF EQU  *                                                        49360000
         L     R3,G_SCAN_TOKEN_LEN * Get tgt to find's name length      49370000
         CH    R3,TGTNAMELEN      * Compare it to current tgt name len  49380000
         IF (H) THEN              * Compare length of shortest of the 2 49390000
            LH    R3,TGTNAMELEN   * If current tgt name len is less     49400000
         ENDIF                    * then use that                       49410000
         BCTR  R3,R0              * Subtract 1 for EX                   49420000
         L     R2,G_SCAN_TOKENA   * Point R2 to tgt to find's name      49430000
         LA    R4,TGTNAME         * Point R4 to current tgt name        49440000
         B     *+10               * Skip CLC constant for EX            49450000
         CLC   0(1,R2),0(R4)      * CLC constant for EX                 49460000
         EX    R3,*-6             * EX previous CLC stmt with R3        49470000
         IF (L) THEN              * If find tgt name is lower           49480000
            CLC   TGTLOW,=A(0)    * Check if current tgt's low ptr set  49490000
            IF (EQ) THEN          * If not                              49500000
               B     FINDTGT_RET  * Tgt not found, skip rest of findtgt 49510000
            ELSE                  * Else, current tgt's low ptr set     49520000
               L     R6,TGTLOW    * Replace current tgt by low tgt      49530000
               B     TEST_TGTF    * Loop around to test that one        49540000
            ENDIF                                                       49550000
         ELSE                     * Else, find tgt name is >=           49560000
            IF (EQ) THEN          * If the compared part is equal       49570000
               CLC   G_SCAN_TOKEN_LEN+2(2),TGTNAMELEN * Check if also   49580000
               IF (EQ) THEN       * of equal length                     49590000
                  ST    R6,G_FOUND_TGT_PTR * Return this tgt's ptr      49600000
                  B     FINDTGT_RET * Skip rest of find tgt             49610000
               ENDIF                                                    49620000
            ENDIF                                                       49630000
*           If we end up here, it's not a match                         49640000
            CLC   TGTHIGH,=A(0)   * Check if curr tgt's high ptr set    49650000
            IF (EQ) THEN          * If not                              49660000
               B     FINDTGT_RET  * Tgt not found, skip rest of findtgt 49670000
            ELSE                  * Else, current tgt's high ptr set    49680000
               L     R6,TGTHIGH   * Replace current tgt by high tgt     49690000
               B     TEST_TGTF    * Loop around to test that one        49700000
            ENDIF                                                       49710000
         ENDIF                                                          49720000
*                                                                       49730000
FINDTGT_RET EQU *                                                       49740000
*        Write result of find var to report                             49750000
         CLC   G_FOUND_TGT_PTR,=A(0) * Was a tgt found?                 49760000
         IF (EQ) THEN                * Nope                             49770000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... No targeX49780000
               t found'                                                 49790000
         ELSE                        * Tgt was found                    49800000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target fX49810000
               ound'                                                    49820000
         ENDIF                                                          49830000
         L     R15,G_LWZMAKE_RPTA * Get address of LWZMAKE_RPT section  49840000
         BASR  R14,R15            * Link to LWZMAKE_RPT section         49850000
*                                                                       49860000
         DROP  R6                                                       49870000
*                                                                       49880000
         MLWZTERM                 * Return back to caller               49890000
*                                                                       49900000
         LTORG                                                          49910000
*                                                                       49920000
*********************************************************************** 49930000
* Section: LWZMAKE_FINDPNY                                            * 49940000
* Purpose: Find a PHONY in PHONY binary search tree                   * 49950000
*          The name of the PHONY to find is in token 1.               * 49960000
*          R9 should point to global data.                            * 49970000
*********************************************************************** 49980000
LWZMAKE_FINDPNY MLWZSAVE                                                49990000
*        Trace record to start section                                  50000000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX50010000
               KE_FINDPNY'                                              50020000
*                                                                       50030000
         MVC   G_FOUND_PNY_PTR,=A(0) * Initialize return pointer        50040000
*                                                                       50050000
         USING PHONY_DSECT,R6     * Use R6 for addressing PHONY         50060000
*                                                                       50070000
         LT    R6,G_FIRST_PNY_PTR * Get start of search tree for PNYs   50080000
         IF (Z) THEN              * If no PNYs in tree                  50090000
            B     FINDPNY_RET     * Skip rest of find PHONY             50100000
         ENDIF                                                          50110000
TEST_PNYF EQU  *                                                        50120000
         L     R3,G_SCAN_TOKEN_LEN * Get PNY to find's name length      50130000
         CH    R3,PNYNAMELEN      * Compare it to current PNY name len  50140000
         IF (H) THEN              * Compare length of shortest of the 2 50150000
            LH    R3,PNYNAMELEN   * If current PNY name len is less     50160000
         ENDIF                    * then use that                       50170000
         BCTR  R3,R0              * Subtract 1 for EX                   50180000
         L     R2,G_SCAN_TOKENA   * Point R2 to PNY to find's name      50190000
         LA    R4,PNYNAME         * Point R4 to current PNY name        50200000
         B     *+10               * Skip CLC constant for EX            50210000
         CLC   0(1,R2),0(R4)      * CLC constant for EX                 50220000
         EX    R3,*-6             * EX previous CLC stmt with R3        50230000
         IF (L) THEN              * If find PNY name is lower           50240000
            CLC   PNYLOW,=A(0)    * Check if current PNY's low ptr set  50250000
            IF (EQ) THEN          * If not                              50260000
               B     FINDPNY_RET  * PNY not found, skip rest of findpny 50270000
            ELSE                  * Else, current PNY's low ptr set     50280000
               L     R6,PNYLOW    * Replace current PNY by low PNY      50290000
               B     TEST_PNYF    * Loop around to test that one        50300000
            ENDIF                                                       50310000
         ELSE                     * Else, find PNY name is >=           50320000
            IF (EQ) THEN          * If the compared part is equal       50330000
               CLC   G_SCAN_TOKEN_LEN+2(2),PNYNAMELEN * Check if also   50340000
               IF (EQ) THEN       * of equal length                     50350000
                  ST    R6,G_FOUND_PNY_PTR * Return this PNY's ptr      50360000
                  B     FINDPNY_RET * Skip rest of find PNY             50370000
               ENDIF                                                    50380000
            ENDIF                                                       50390000
*           If we end up here, it's not a match                         50400000
            CLC   PNYHIGH,=A(0)   * Check if curr PNY's high ptr set    50410000
            IF (EQ) THEN          * If not                              50420000
               B     FINDPNY_RET  * PNY not found, skip rest of findpny 50430000
            ELSE                  * Else, current PNY's high ptr set    50440000
               L     R6,PNYHIGH   * Replace current PNY by high PNY     50450000
               B     TEST_PNYF    * Loop around to test that one        50460000
            ENDIF                                                       50470000
         ENDIF                                                          50480000
*                                                                       50490000
FINDPNY_RET EQU *                                                       50500000
*        Write result of find var to report                             50510000
         CLC   G_FOUND_PNY_PTR,=A(0) * Was a PNY found?                 50520000
         IF (EQ) THEN                * Nope                             50530000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target iX50540000
               s a file'                                                50550000
         ELSE                        * Tgt was found                    50560000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Target iX50570000
               s PHONY'                                                 50580000
         ENDIF                                                          50590000
         L     R15,G_LWZMAKE_RPTA * Get address of LWZMAKE_RPT section  50600000
         BASR  R14,R15            * Link to LWZMAKE_RPT section         50610000
*                                                                       50620000
         DROP  R6                                                       50630000
*                                                                       50640000
         MLWZTERM                 * Return back to caller               50650000
*                                                                       50660000
         LTORG                                                          50670000
*                                                                       50680000
*********************************************************************** 50690000
* Section: LWZMAKE_EXEC_TGT                                           * 50700000
* Purpose: Execute a target. This is a recursive section, in other    * 50710000
*          words, it calls itself.                                    * 50720000
*          First, the target last altered date is acquired. Then the  * 50730000
*          requisites are 'expanded', meaning any variables in that   * 50740000
*          string are resolved. Then, for each requisite this section * 50750000
*          is invoked. If, after all requisites have been processed,  * 50760000
*          any of the requisites had a laster altered date more       * 50770000
*          recent than the target's, that target should be built.     * 50780000
*          If so, the recipe following the rult statement for this    * 50790000
*          target is executed.                                        * 50800000
*          R9 should point to global data.                            * 50810000
*********************************************************************** 50820000
*                                                                       50830000
LWZMAKE_EXEC_TGT DS    0F                                               50840000
         STM   R14,R12,12(R13)   * Save callers registers               50850000
         LR    R10,R15           * Set R10 to entry point               50860000
         LA    R11,4095(,R10)    * Setup R11 as second using            50870000
         LA    R11,1(,R11)       *   base register                      50880000
         USING LWZMAKE_EXEC_TGT,R10,R11 * Establish addressing          50890000
         LR    R2,R1             * Save possible parameter ptr          50900000
         GETMAIN RU,LV=WORKAREA_EXEC_TGT_LEN                            50910000
         ST    R13,4(R1)         * Backward chain callers SA            50920000
         ST    R1,8(R13)         * Forward chain my SA                  50930000
         LR    R13,R1            * Point R13 to my SA                   50940000
         USING WORKAREA_EXEC_TGT,R13 * Establish addressing             50950000
         USING GLOBAL_DATA_DSECT,R9                                     50960000
         LR    R1,R2             * Restore parameter list ptr in R1     50970000
         XR    R15,R15                                                  50980000
         ST    R15,RETCODE_EXEC_TGT                                     50990000
*                                                                       51000000
*        Trace record to start section                                  51010000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX51020000
               KE_EXEC_TGT'                                             51030000
*                                                                       51040000
*        Reset scanning variables                                       51050000
         MVC   G_SCAN_CURRCOL,=F'0' * Not really used, but it shouldn't 51060000
*                                 * be whatever value (like 80) left    51070000
*                                 * over from scanning makefile         51080000
         MVI   G_MKFEOF,C'N'      * Reset EOF switch                    51090000
*                                                                       51100000
*        This section is invoked with a parameter of EXEC_TGT_PAR. It   51110000
*        holds a pointer to a TARGET_DSECT, which in turn points to a   51120000
*        STMT_R_DSECT.                                                  51130000
         L     R1,0(,R1)          * Get pointer to first parameter      51140000
         ST    R1,EXEC_TGT_PARA   * Save it                             51150000
         L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1) * Get TARGET_DSECT ptr 51160000
         USING TARGET_DSECT,R6    * Address it with R6                  51170000
         L     R7,TGTSTMT         * Get originating rule statement ptr  51180000
         USING STMT_R_DSECT,R7    * Address it with R7                  51190000
*                                                                       51200000
*                                                                       51210000
         MVC   G_LWZMRPT_LINE,=CL133' Processing target ...'            51220000
         LA    R2,G_LWZMRPT_LINE+23                                     51230000
         LA    R3,TGTNAME                                               51240000
         L     R4,=F'110'                                               51250000
         CH    R4,TGTNAMELEN                                            51260000
         IF (H) THEN                                                    51270000
            LH    R4,TGTNAMELEN                                         51280000
         ENDIF                                                          51290000
         BCTR  R4,R0                                                    51300000
         B     *+10                                                     51310000
         MVC   0(1,R2),0(R3)                                            51320000
         EX    R4,*-6                                                   51330000
         L     R15,G_LWZMAKE_RPTA                                       51340000
         BASR  R14,R15                                                  51350000
*                                                                       51360000
         L     R2,G_SCAN_TOKENA                                         51370000
         LA    R3,TGTNAME                                               51380000
         XR    R4,R4                                                    51390000
         LH    R4,TGTNAMELEN                                            51400000
         ST    R4,G_SCAN_TOKEN_LEN                                      51410000
         BCTR  R4,R0                                                    51420000
         B     *+10                                                     51430000
         MVC   0(1,R2),0(R3)                                            51440000
         EX    R4,*-6                                                   51450000
         L     R15,LWZMAKE_FINDPNYA_EXEC                                51460000
         BASR  R14,R15                                                  51470000
*                                                                       51480000
         CLC   G_FOUND_PNY_PTR,=A(0)                                    51490000
         IF (NE) THEN                                                   51500000
            XC    TARGET_ALTER_DATE,TARGET_ALTER_DATE                   51510000
            B     EXEC_TGT_PREREQ                                       51520000
         ENDIF                                                          51530000
*                                                                       51540000
         L     R2,G_SCAN_TOKENA                                         51550000
         LA    R3,TGTNAME                                               51560000
         XR    R4,R4                                                    51570000
         LH    R4,TGTNAMELEN                                            51580000
         ST    R4,G_SCAN_TOKEN_LEN                                      51590000
         BCTR  R4,R0                                                    51600000
         B     *+10                                                     51610000
         MVC   0(1,R2),0(R3)                                            51620000
         EX    R4,*-6                                                   51630000
         L     R15,LWZMAKE_GET_DATEA_EXEC                               51640000
         BASR  R14,R15                                                  51650000
*                                                                       51660000
         CLC   G_RETCODE,=F'0'                                          51670000
         BNE   EXEC_TGT_RET                                             51680000
*                                                                       51690000
         LT    R3,G_MVSDS_MEMBER_LEN                                    51700000
         IF (NZ) THEN                                                   51710000
            STH   R3,TGTNAMEMEMLEN                                      51720000
            LA    R2,TGTNAME                                            51730000
            AH    R2,TGTNAMELEN                                         51740000
            L     R4,G_MVSDS_MEMBER_PTR                                 51750000
            BCTR  R3,R0                                                 51760000
            B     *+10                                                  51770000
            MVC   0(1,R2),0(R4)                                         51780000
            EX    R3,*-6                                                51790000
         ENDIF                                                          51800000
*                                                                       51810000
         MVC   TARGET_ALTER_DATE,G_SAVE_ALTER_DATE                      51820000
         CLC   TARGET_ALTER_DATE,=16X'FF'                               51830000
         IF (EQ) THEN                                                   51840000
            MLWZMRPT RPTLINE=CL133' ..................... Target has noX51850000
                last altered date, build required'                      51860000
         ENDIF                                                          51870000
*                                                                       51880000
EXEC_TGT_PREREQ EQU *                                                   51890000
         CLC   STMT_R_REQLEN,=H'0'                                      51900000
         BE    EXEC_TGT_PREREQ_DONE                                     51910000
*                                                                       51920000
         XR    R2,R2                                                    51930000
         XR    R3,R3                                                    51940000
         IC    R3,G_SCAN_INPUT_STACK_IDX                                51950000
         C     R3,=F'20'                                                51960000
         IF (NL) THEN                                                   51970000
            MLWZMRPT RPTLINE=CL133'0Internal error, state stack overfloX51980000
               w'                                                       51990000
            MVC   G_RETCODE,=F'12'                                      52000000
            B     EXEC_TGT_RET                                          52010000
         ENDIF                                                          52020000
         LA    R3,1(,R3)                                                52030000
         STC   R3,G_SCAN_INPUT_STACK_IDX                                52040000
         BCTR  R3,R0                                                    52050000
         M     R2,=F'12'                                                52060000
         LA    R2,G_SCAN_INPUT_STACK                                    52070000
         AR    R2,R3                                                    52080000
         USING INPUT_DSECT,R2                                           52090000
         MVI   INPUTTYPE,X'01'                                          52100000
         MVC   INPUTLEN,STMT_R_REQLEN                                   52110000
         LA    R4,STMT_R_TGT                                            52120000
         AH    R4,STMT_R_TGTLEN                                         52130000
         ST    R4,INPUTPTR                                              52140000
         MVC   INPUTPOS,=H'0'                                           52150000
         DROP  R2                                                       52160000
*                                                                       52170000
         MVI   G_SCAN_STATE,SCAN_STATE_IN_EXPAND                        52180000
         MVC   G_SCAN_TOKEN2_LEN,=F'0'                                  52190000
*                                                                       52200000
EXEC_TGT_PREREQ_NEXT_TOKEN EQU *                                        52210000
         L     R15,LWZMAKE_SCAN_TOKENA_EXEC                             52220000
         BASR  R14,R15                                                  52230000
*                                                                       52240000
         CLC   G_RETCODE,=F'0'                                          52250000
         BNE   EXEC_TGT_RET                                             52260000
*                                                                       52270000
         CLI   G_MKFEOF,C'Y'                                            52280000
         BE    EXEC_TGT_PREREQ_EXPANDED                                 52290000
*                                                                       52300000
         IC    R14,G_SCAN_STATE                                         52310000
         N     R14,=X'0000007F'                                         52320000
         C     R14,=A(SCAN_STATE_IN_VARIABLE)                           52330000
         IF (EQ) THEN                                                   52340000
            MVI   G_SCAN_APPEND_TO,X'00'                                52350000
            MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1'                       52360000
            L     R15,LWZMAKE_SCAN_VARA_EXEC                            52370000
            BASR  R14,R15                                               52380000
*                                                                       52390000
            CLC   G_RETCODE,=F'0'                                       52400000
            BNE   EXEC_TGT_RET                                          52410000
*                                                                       52420000
            B     EXEC_TGT_PREREQ_NEXT_TOKEN                            52430000
         ENDIF                                                          52440000
*                                                                       52450000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_PERCENT) THEN       52460000
            L     R2,G_SCAN_TOKENA                                      52470000
            LA    R4,TGTNAME                                            52480000
            AH    R4,TGTNAMELEN                                         52490000
            XR    R3,R3                                                 52500000
            LH    R3,TGTNAMEMEMLEN                                      52510000
            ST    R3,G_SCAN_TOKEN_LEN                                   52520000
            BCTR  R3,R0                                                 52530000
            B     *+10                                                  52540000
            MVC   0(1,R2),0(R4)                                         52550000
            EX    R3,*-6                                                52560000
         ENDIF                                                          52570000
         IF (CLI,G_SCAN_TOKENTYPE,EQ,SCAN_TOKENTYPE_ACRO) THEN          52580000
            L     R2,G_SCAN_TOKENA                                      52590000
            LA    R4,TGTNAME                                            52600000
            XR    R3,R3                                                 52610000
            LH    R3,TGTNAMELEN                                         52620000
            ST    R3,G_SCAN_TOKEN_LEN                                   52630000
            BCTR  R3,R0                                                 52640000
            B     *+10                                                  52650000
            MVC   0(1,R2),0(R4)                                         52660000
            EX    R3,*-6                                                52670000
         ENDIF                                                          52680000
*                                                                       52690000
*        Append token 1 to token 2                                      52700000
         MVI   G_SCAN_APPEND_TO,X'01'                                   52710000
         L     R15,LWZMAKE_APPEND_TOKENA_EXEC * Get addr APPEND_TOKEN   52720000
         BASR  R14,R15            * Link to APPEND_TOKEN section        52730000
*                                                                       52740000
         B     EXEC_TGT_PREREQ_NEXT_TOKEN                               52750000
*                                                                       52760000
EXEC_TGT_PREREQ_EXPANDED EQU *                                          52770000
         MVC   G_LWZMRPT_LINE,=CL133' Prerequisites........'            52780000
         LA    R2,G_LWZMRPT_LINE+23                                     52790000
         L     R3,G_SCAN_TOKEN2_LEN                                     52800000
         L     R4,G_SCAN_TOKEN2A                                        52810000
         C     R3,=A(110)                                               52820000
         IF (H) THEN                                                    52830000
            L     R3,=A(110)                                            52840000
         ENDIF                                                          52850000
         BCTR  R3,R0                                                    52860000
         B     *+10                                                     52870000
         MVC   0(1,R2),0(R4)                                            52880000
         EX    R3,*-6                                                   52890000
         L     R15,G_LWZMAKE_RPTA                                       52900000
         BASR  R14,R15                                                  52910000
*                                                                       52920000
         MVC   EXEC_WORD_SPLIT_PTR,G_SCAN_TOKEN2A                       52930000
         MVC   EXEC_WORD_SPLIT_LEN,G_SCAN_TOKEN2_LEN                    52940000
EXEC_TGT_SCAN_NEXT_PREREQ EQU *                                         52950000
         L     R2,EXEC_WORD_SPLIT_PTR                                   52960000
         L     R3,EXEC_WORD_SPLIT_LEN                                   52970000
         L     R4,G_SCAN_TOKENA                                         52980000
         XR    R5,R5                                                    52990000
         ST    R5,G_SCAN_TOKEN_LEN                                      53000000
EXEC_TGT_PREREQ_BLANK EQU *                                             53010000
         IF (CLI,0(R2),EQ,C' ') THEN                                    53020000
            LA    R2,1(,R2)                                             53030000
            BCT   R3,EXEC_TGT_PREREQ_BLANK                              53040000
            B     EXEC_TGT_RET                                          53050000
         ENDIF                                                          53060000
EXEC_TGT_PREREQ_NONBLANK EQU *                                          53070000
         MVC   0(1,R4),0(R2)                                            53080000
         LA    R4,1(,R4)                                                53090000
         LA    R5,1(,R5)                                                53100000
         BCTR  R3,R0                                                    53110000
         C     R3,=F'0'                                                 53120000
         IF (H) THEN                                                    53130000
            LA    R2,1(,R2)                                             53140000
            CLI    0(R2),C' '     * Current pos a space?                53150000
            BNE   EXEC_TGT_PREREQ_NONBLANK                              53160000
         ENDIF                                                          53170000
         ST    R5,G_SCAN_TOKEN_LEN                                      53180000
         ST    R2,EXEC_WORD_SPLIT_PTR                                   53190000
         ST    R3,EXEC_WORD_SPLIT_LEN                                   53200000
*                                                                       53210000
         MVC   G_LWZMRPT_LINE,=CL133' Next prereq for tgt .'            53220000
         LA    R2,G_LWZMRPT_LINE+23                                     53230000
         LA    R3,TGTNAME                                               53240000
         L     R4,=F'110'                                               53250000
         CH    R4,TGTNAMELEN                                            53260000
         IF (H) THEN                                                    53270000
            LH    R4,TGTNAMELEN                                         53280000
         ENDIF                                                          53290000
         BCTR  R4,R0                                                    53300000
         B     *+10                                                     53310000
         MVC   0(1,R2),0(R3)                                            53320000
         EX    R4,*-6                                                   53330000
         L     R15,G_LWZMAKE_RPTA                                       53340000
         BASR  R14,R15                                                  53350000
*                                                                       53360000
         MVC   G_LWZMRPT_LINE,=CL133' Checking prereq .....'            53370000
         LA    R2,G_LWZMRPT_LINE+23                                     53380000
         L     R3,G_SCAN_TOKENA                                         53390000
         L     R4,=F'110'                                               53400000
         C     R4,G_SCAN_TOKEN_LEN                                      53410000
         IF (H) THEN                                                    53420000
            L     R4,G_SCAN_TOKEN_LEN                                   53430000
         ENDIF                                                          53440000
         BCTR  R4,R0                                                    53450000
         B     *+10                                                     53460000
         MVC   0(1,R2),0(R3)                                            53470000
         EX    R4,*-6                                                   53480000
         L     R15,G_LWZMAKE_RPTA                                       53490000
         BASR  R14,R15                                                  53500000
*                                                                       53510000
         L     R15,LWZMAKE_FINDTGTA_EXEC                                53520000
         BASR  R14,R15                                                  53530000
*                                                                       53540000
         CLC   G_RETCODE,=F'0'                                          53550000
         BNE   EXEC_TGT_RET                                             53560000
*                                                                       53570000
         CLC   G_FOUND_TGT_PTR,=A(0)                                    53580000
         IF (NE) THEN                                                   53590000
            MVC   EXEC_SAVE_SCAN_TOKENA,G_SCAN_TOKENA                   53600000
            MVC   EXEC_SAVE_SCAN_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN       53610000
            MVC   EXEC_SAVE_SCAN_TOKEN_LEN,G_SCAN_TOKEN_LEN             53620000
            L     R4,G_SCAN_TOKEN_MAXLEN                                53630000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        53640000
            ST    R1,G_SCAN_TOKENA                                      53650000
            MVC   G_SCAN_TOKEN_LEN,=A(0)                                53660000
*                                                                       53670000
            MVC   EXEC_SAVE_SCAN_TOKEN2A,G_SCAN_TOKEN2A                 53680000
            MVC   EXEC_SAVE_SCAN_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN     53690000
            MVC   EXEC_SAVE_SCAN_TOKEN2_LEN,G_SCAN_TOKEN2_LEN           53700000
            L     R4,G_SCAN_TOKEN2_MAXLEN                               53710000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        53720000
            ST    R1,G_SCAN_TOKEN2A                                     53730000
            MVC   G_SCAN_TOKEN2_LEN,=A(0)                               53740000
*                                                                       53750000
            LA    R1,EXEC_NEXTTGT_PAR                                   53760000
            MVC   EXEC_TGT_PTR-EXEC_TGT_PAR(4,R1),G_FOUND_TGT_PTR       53770000
            ST    R1,EXEC_NEXTTGT_PARA                                  53780000
            LA    R1,EXEC_NEXTTGT_PARA                                  53790000
            L     R15,LWZMAKE_EXEC_TGTA_EXEC                            53800000
            BASR  R14,R15                                               53810000
*                                                                       53820000
            CLC   G_RETCODE,=F'0'                                       53830000
            BNE   EXEC_TGT_RET                                          53840000
*                                                                       53850000
            L     R2,G_SCAN_TOKEN_MAXLEN                                53860000
            L     R3,G_SCAN_TOKENA                                      53870000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  53880000
            MVC   G_SCAN_TOKENA,EXEC_SAVE_SCAN_TOKENA                   53890000
            MVC   G_SCAN_TOKEN_MAXLEN,EXEC_SAVE_SCAN_TOKEN_MAXLEN       53900000
            MVC   G_SCAN_TOKEN_LEN,EXEC_SAVE_SCAN_TOKEN_LEN             53910000
*                                                                       53920000
            L     R2,G_SCAN_TOKEN2_MAXLEN                               53930000
            L     R3,G_SCAN_TOKEN2A                                     53940000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  53950000
            MVC   G_SCAN_TOKEN2A,EXEC_SAVE_SCAN_TOKEN2A                 53960000
            MVC   G_SCAN_TOKEN2_MAXLEN,EXEC_SAVE_SCAN_TOKEN2_MAXLEN     53970000
            MVC   G_SCAN_TOKEN2_LEN,EXEC_SAVE_SCAN_TOKEN2_LEN           53980000
*                                                                       53990000
            MVC   G_LWZMRPT_LINE,=CL133' Continuing target ...'         54000000
            LA    R2,G_LWZMRPT_LINE+23                                  54010000
            LA    R3,TGTNAME                                            54020000
            L     R4,=F'110'                                            54030000
            CH    R4,TGTNAMELEN                                         54040000
            IF (H) THEN                                                 54050000
               LH    R4,TGTNAMELEN                                      54060000
            ENDIF                                                       54070000
            BCTR  R4,R0                                                 54080000
            B     *+10                                                  54090000
            MVC   0(1,R2),0(R3)                                         54100000
            EX    R4,*-6                                                54110000
            L     R15,G_LWZMAKE_RPTA                                    54120000
            BASR  R14,R15                                               54130000
*                                                                       54140000
            MVC   G_LWZMRPT_LINE,=CL133' Continuing prereq ...'         54150000
            LA    R2,G_LWZMRPT_LINE+23                                  54160000
            L     R3,G_SCAN_TOKENA                                      54170000
            L     R4,=F'110'                                            54180000
            C     R4,G_SCAN_TOKEN_LEN                                   54190000
            IF (H) THEN                                                 54200000
               L     R4,G_SCAN_TOKEN_LEN                                54210000
            ENDIF                                                       54220000
            BCTR  R4,R0                                                 54230000
            B     *+10                                                  54240000
            MVC   0(1,R2),0(R3)                                         54250000
            EX    R4,*-6                                                54260000
            L     R15,G_LWZMAKE_RPTA                                    54270000
            BASR  R14,R15                                               54280000
         ENDIF                                                          54290000
*                                                                       54300000
         L     R15,LWZMAKE_FINDPNYA_EXEC                                54310000
         BASR  R14,R15                                                  54320000
*                                                                       54330000
         CLC   G_FOUND_PNY_PTR,=A(0)                                    54340000
         BNE   EXEC_TGT_PREREQ_CHECK_LOOP                               54350000
*                                                                       54360000
         L     R15,LWZMAKE_GET_DATEA_EXEC                               54370000
         BASR  R14,R15                                                  54380000
*                                                                       54390000
         CLC   G_RETCODE,=F'0'                                          54400000
         BNE   EXEC_TGT_RET                                             54410000
*                                                                       54420000
         IF (CLI,G_DSFOUND,NE,C'Y') THEN                                54430000
            MLWZMRPT RPTLINE=CL133' ..................... Prerequisite X54440000
               not found, build stopped'                                54450000
            MVC   G_RETCODE,=F'8'                                       54460000
            B     EXEC_TGT_RET                                          54470000
         ENDIF                                                          54480000
*                                                                       54490000
         CLC   TARGET_ALTER_DATE,=16X'00'                               54500000
         BE    EXEC_TGT_PREREQ_CHECK_LOOP                               54510000
*                                                                       54520000
         CLC   TARGET_ALTER_DATE,=16X'FF'                               54530000
         IF (NE) THEN                                                   54540000
            CLC   TARGET_ALTER_DATE,G_SAVE_ALTER_DATE                   54550000
            IF (LT) THEN                                                54560000
               MLWZMRPT RPTLINE=CL133' ..................... Target is X54570000
               older than prereq, build required'                       54580000
               MVC   TARGET_ALTER_DATE,=16X'FF'                         54590000
            ENDIF                                                       54600000
         ENDIF                                                          54610000
*                                                                       54620000
EXEC_TGT_PREREQ_CHECK_LOOP EQU *                                        54630000
         L     R3,EXEC_WORD_SPLIT_LEN                                   54640000
         C     R3,=F'0'                                                 54650000
         BH    EXEC_TGT_SCAN_NEXT_PREREQ                                54660000
*                                                                       54670000
EXEC_TGT_PREREQ_DONE EQU *                                              54680000
         CLC   TARGET_ALTER_DATE,=16X'FF'                               54690000
         IF (NE) THEN                                                   54700000
            CLC   TARGET_ALTER_DATE,=16X'00'                            54710000
         ENDIF                                                          54720000
         IF (EQ) THEN                                                   54730000
            BAL   R8,EXEC_TGT_BUILD                                     54740000
         ELSE                                                           54750000
            MLWZMRPT RPTLINE=CL133' ..................... No need to buX54760000
               ild target'                                              54770000
            B     EXEC_TGT_RET                                          54780000
         ENDIF                                                          54790000
*                                                                       54800000
         DROP  R6                                                       54810000
         DROP  R7                                                       54820000
*                                                                       54830000
EXEC_TGT_RET EQU *                                                      54840000
         L     R2,RETCODE_EXEC_TGT * Save return value                  54850000
         L     R3,4(,R13)        * Restore address of callers SA        54860000
         FREEMAIN RU,LV=WORKAREA_EXEC_TGT_LEN,A=(R13)                   54870000
         LR    R15,R2            * Restore return value                 54880000
         LR    R13,R3            * Address of callers SA                54890000
         L     R14,12(R13)       * Restore callers R14                  54900000
         LM    R0,R12,20(R13)    * Restore callers registers 0-12       54910000
         BR    R14               * Return                               54920000
*                                                                       54930000
* Execute the recipe                                                    54940000
*                                                                       54950000
EXEC_TGT_BUILD EQU *                                                    54960000
         L     R1,EXEC_TGT_PARA                                         54970000
         L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)                        54980000
         USING TARGET_DSECT,R6                                          54990000
         L     R7,TGTSTMT                                               55000000
         USING STMT_DSECT,R7                                            55010000
*                                                                       55020000
         MVC   G_LWZMRPT_LINE,=CL133' Building target .....'            55030000
         LA    R2,G_LWZMRPT_LINE+23                                     55040000
         LA    R3,TGTNAME                                               55050000
         L     R4,=F'110'                                               55060000
         CH    R4,TGTNAMELEN                                            55070000
         IF (H) THEN                                                    55080000
            LH    R4,TGTNAMELEN                                         55090000
         ENDIF                                                          55100000
         BCTR  R4,R0                                                    55110000
         B     *+10                                                     55120000
         MVC   0(1,R2),0(R3)                                            55130000
         EX    R4,*-6                                                   55140000
         L     R15,G_LWZMAKE_RPTA                                       55150000
         BASR  R14,R15                                                  55160000
*                                                                       55170000
         DROP  R6                                                       55180000
*                                                                       55190000
         LT    R7,STMT_NEXT_PTR                                         55200000
         BZ    EXEC_TGT_BUILD_NO_RECIPE                                 55210000
*                                                                       55220000
         CLI   STMT_IN_RECIPE,C'Y'                                      55230000
         BNE   EXEC_TGT_BUILD_NO_RECIPE                                 55240000
*                                                                       55250000
NEXT_RECIPE_STMT EQU *                                                  55260000
         IF (CLI,STMT_TYPE,EQ,STMT_TYPE_CALL) THEN                      55270000
            DROP  R7                                                    55280000
            USING STMT_C_DSECT,R7                                       55290000
*                                                                       55300000
            MVC   EXEC_SAVE_SCAN_TOKENA,G_SCAN_TOKENA                   55310000
            MVC   EXEC_SAVE_SCAN_TOKEN_MAXLEN,G_SCAN_TOKEN_MAXLEN       55320000
            MVC   EXEC_SAVE_SCAN_TOKEN_LEN,G_SCAN_TOKEN_LEN             55330000
            L     R4,G_SCAN_TOKEN_MAXLEN                                55340000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        55350000
            ST    R1,G_SCAN_TOKENA                                      55360000
            MVC   G_SCAN_TOKEN_LEN,=A(0)                                55370000
*                                                                       55380000
            MVC   EXEC_SAVE_SCAN_TOKEN2A,G_SCAN_TOKEN2A                 55390000
            MVC   EXEC_SAVE_SCAN_TOKEN2_MAXLEN,G_SCAN_TOKEN2_MAXLEN     55400000
            MVC   EXEC_SAVE_SCAN_TOKEN2_LEN,G_SCAN_TOKEN2_LEN           55410000
            L     R4,G_SCAN_TOKEN2_MAXLEN                               55420000
            STORAGE OBTAIN,LENGTH=(R4) * Allocate a memory block        55430000
            ST    R1,G_SCAN_TOKEN2A                                     55440000
            MVC   G_SCAN_TOKEN2_LEN,=A(0)                               55450000
*                                                                       55460000
            XR    R2,R2                                                 55470000
            XR    R3,R3                                                 55480000
            IC    R3,G_SCAN_INPUT_STACK_IDX                             55490000
            C     R3,=F'20'                                             55500000
            IF (NL) THEN                                                55510000
               MLWZMRPT RPTLINE=CL133'0Internal error, state stack overX55520000
               flow'                                                    55530000
               MVC   G_RETCODE,=F'12'                                   55540000
               B     EXEC_CALL_END                                      55550000
            ENDIF                                                       55560000
            LA    R3,1(,R3)                                             55570000
            STC   R3,G_SCAN_INPUT_STACK_IDX                             55580000
            BCTR  R3,R0                                                 55590000
            M     R2,=F'12'                                             55600000
            LA    R2,G_SCAN_INPUT_STACK                                 55610000
            AR    R2,R3                                                 55620000
            USING INPUT_DSECT,R2                                        55630000
            MVI   INPUTTYPE,X'01'                                       55640000
            MVC   INPUTLEN,STMT_C_PARMLEN                               55650000
            LA    R6,STMT_C_EXEC                                        55660000
            AH    R6,STMT_C_EXECLEN                                     55670000
            ST    R6,INPUTPTR                                           55680000
            MVC   INPUTPOS,=H'0'                                        55690000
            MVI   G_MKFEOF,C'N'                                         55700000
*                                                                       55710000
            DROP  R2                                                    55720000
*                                                                       55730000
            MVI   G_SCAN_STATE,SCAN_STATE_IN_EXPAND                     55740000
            MVC   G_SCAN_TOKEN2_LEN,=F'0'                               55750000
*                                                                       55760000
EXEC_TGT_CALL_NEXT_TOKEN EQU *                                          55770000
            L     R15,LWZMAKE_SCAN_TOKENA_EXEC                          55780000
            BASR  R14,R15                                               55790000
*                                                                       55800000
            CLC   G_RETCODE,=F'0'                                       55810000
            BNE   EXEC_CALL_END                                         55820000
*                                                                       55830000
            CLI   G_MKFEOF,C'Y'                                         55840000
            BE    EXEC_TGT_CALL_EXPANDED                                55850000
*                                                                       55860000
            IC    R14,G_SCAN_STATE                                      55870000
            N     R14,=X'0000007F'                                      55880000
            C     R14,=A(SCAN_STATE_IN_VARIABLE)                        55890000
            IF (EQ) THEN                                                55900000
               MVI   G_SCAN_APPEND_TO,X'00'                             55910000
               MVI   G_SCAN_VAR_PRESERVE_SPACES,C'1'                    55920000
               L     R15,LWZMAKE_SCAN_VARA_EXEC                         55930000
               BASR  R14,R15                                            55940000
*                                                                       55950000
               CLC   G_RETCODE,=F'0'                                    55960000
               BNE   EXEC_CALL_END                                      55970000
*                                                                       55980000
               B     EXEC_TGT_CALL_NEXT_TOKEN                           55990000
            ENDIF                                                       56000000
*                                                                       56010000
            L     R1,G_SCAN_TOKEN_LEN                                   56020000
            C     R1,=F'2'                                              56030000
            IF (EQ) THEN                                                56040000
               L     R2,G_SCAN_TOKENA                                   56050000
               CLC   0(2,R2),=C'$@'                                     56060000
               IF (EQ) THEN                                             56070000
                  L     R1,EXEC_TGT_PARA                                56080000
                  L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)               56090000
                  LA    R3,TGTNAME-TARGET_DSECT(,R6)                    56100000
                  XR    R4,R4                                           56110000
                  LH    R4,TGTNAMELEN-TARGET_DSECT(,R6)                 56120000
                  ST    R4,G_SCAN_TOKEN_LEN                             56130000
                  BCTR  R4,R0                                           56140000
                  B     *+10                                            56150000
                  MVC   0(1,R2),0(R3)                                   56160000
                  EX    R4,*-6                                          56170000
               ENDIF                                                    56180000
               CLC   0(2,R2),=C'$%'                                     56190000
               IF (EQ) THEN                                             56200000
                  L     R1,EXEC_TGT_PARA                                56210000
                  L     R6,EXEC_TGT_PTR-EXEC_TGT_PAR(,R1)               56220000
                  LA    R3,TGTNAME-TARGET_DSECT(,R6)                    56230000
                  XR    R4,R4                                           56240000
                  LH    R4,TGTNAMELEN-TARGET_DSECT(,R6)                 56250000
                  AR    R3,R4                                           56260000
                  LH    R4,TGTNAMEMEMLEN-TARGET_DSECT(,R6)              56270000
                  ST    R4,G_SCAN_TOKEN_LEN                             56280000
                  BCTR  R4,R0                                           56290000
                  B     *+10                                            56300000
                  MVC   0(1,R2),0(R3)                                   56310000
                  EX    R4,*-6                                          56320000
               ENDIF                                                    56330000
            ENDIF                                                       56340000
*                                                                       56350000
            MVI   G_SCAN_APPEND_TO,X'01'                                56360000
            L     R15,LWZMAKE_APPEND_TOKENA_EXEC                        56370000
            BASR  R14,R15         * Link to APPEND_TOKEN section        56380000
*                                                                       56390000
            B     EXEC_TGT_CALL_NEXT_TOKEN                              56400000
*                                                                       56410000
EXEC_TGT_CALL_EXPANDED EQU *                                            56420000
*                                                                       56430000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Calling X56440000
               REXX'                                                    56450000
            LA    R2,G_LWZMRPT_LINE+36                                  56460000
            LA    R3,STMT_C_EXEC                                        56470000
            XR    R4,R4                                                 56480000
            LH    R4,STMT_C_EXECLEN                                     56490000
            BCTR  R4,R0                                                 56500000
            B     *+10                                                  56510000
            MVC   0(1,R2),0(R3)                                         56520000
            EX    R4,*-6                                                56530000
            LT    R4,G_SCAN_TOKEN2_LEN                                  56540000
            IF (NZ) THEN                                                56550000
               AH    R2,STMT_C_EXECLEN                                  56560000
               LA    R2,1(,R2)                                          56570000
               LA    R3,G_LWZMRPT_LINE+133                              56580000
               SR    R3,R2                                              56590000
               CR    R4,R3                                              56600000
               IF (H) THEN                                              56610000
                  LR    R4,R3                                           56620000
               ENDIF                                                    56630000
               BCTR  R4,R0                                              56640000
               L     R3,G_SCAN_TOKEN2A                                  56650000
               B     *+10                                               56660000
               MVC   0(1,R2),0(R3)                                      56670000
               EX    R4,*-6                                             56680000
            ENDIF                                                       56690000
            L     R15,G_LWZMAKE_RPTA                                    56700000
            BASR  R14,R15                                               56710000
*                                                                       56720000
            IF (CLI,G_USE_ISPEXEC,EQ,C' ') THEN                         56730001
               MVI   G_USE_ISPEXEC,C'N'                                 56740001
*                                                                       56750001
               MVC   G_IRXINIT_FUNCTION,=CL8'FINDENVB'                  56760001
               MVC   G_IRXINIT_PARMMOD,=CL8' '                          56770001
               MVC   G_IRXINIT_INSTORPARM_PTR,=A(0)                     56780001
               MVC   G_IRXINIT_USRFIELD_PTR,=X'80000000'                56790001
               MVC   G_IRXINIT_RESERVED_PTR,=A(0)                       56800001
               MVC   G_IRXINIT_ENVBLOCK_PTR,=A(0)                       56810001
               MVC   G_IRXINIT_REASON,=A(0)                             56820001
*                                                                       56830000
               XR    R0,R0                                              56840001
               LA    R1,G_IRXINIT_FUNCTION                              56850001
               ST    R1,G_IRXINIT_PAR7A                                 56860001
               LA    R1,G_IRXINIT_PARMMOD                               56870001
               ST    R1,G_IRXINIT_PAR7A+4                               56880001
               LA    R1,G_IRXINIT_INSTORPARM_PTR                        56890001
               ST    R1,G_IRXINIT_PAR7A+8                               56900001
               LA    R1,G_IRXINIT_USRFIELD_PTR                          56910001
               ST    R1,G_IRXINIT_PAR7A+12                              56920001
               LA    R1,G_IRXINIT_RESERVED_PTR                          56930001
               ST    R1,G_IRXINIT_PAR7A+16                              56940001
               LA    R1,G_IRXINIT_ENVBLOCK_PTR                          56950001
               ST    R1,G_IRXINIT_PAR7A+20                              56960001
               LA    R1,G_IRXINIT_REASON                                56970001
               O     R1,=X'80000000'                                    56980001
               ST    R1,G_IRXINIT_PAR7A+24                              56990001
               LA    R1,G_IRXINIT_PAR7A                                 57000001
*                                                                       57010000
               LINK  EP=IRXINIT,SF=(E,G_LINKD)                          57020001
*                                                                       57030000
               C     R15,=A(0)                                          57040001
               BE    IRXINIT_OK                                         57050001
               C     R15,=A(4)                                          57060001
               BE    IRXINIT_OK                                         57070001
               C     R15,=A(28)                                         57080001
               BE    IRXINIT_OK                                         57090001
               MLWZMRPT RPTLINE=CL133'0Error finding REXX environment'  57100001
               MVC   G_RETCODE,=F'12'                                   57110001
               BR    R8                                                 57120001
IRXINIT_OK     EQU   *                                                  57130001
*                                                                       57140001
               C     R15,=A(28)                                         57150001
               IF (NE) THEN                                             57160001
                  MVC   G_IRXSUBCM_FUNCTION,=CL8'QUERY'                 57170001
                  MVC   G_IRXSUBCM_STRING,=CL32' '                      57180001
                  MVC   G_IRXSUBCM_STRING(8),=CL8'ISPEXEC'              57190001
                  MVC   G_IRXSUBCM_STRING_LEN,=A(32)                    57200001
                  MVC   G_IRXSUBCM_HOSTENV_NAME,=CL8' '                 57210001
                  MVC   G_IRXSUBCM_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR  57220001
*                                                                       57230001
                  XR    R0,R0                                           57240001
                  LA    R1,G_IRXSUBCM_FUNCTION                          57250001
                  ST    R1,G_IRXSUBCM_PAR5A                             57260001
                  LA    R1,G_IRXSUBCM_STRING                            57270001
                  ST    R1,G_IRXSUBCM_PAR5A+4                           57280001
                  LA    R1,G_IRXSUBCM_STRING_LEN                        57290001
                  ST    R1,G_IRXSUBCM_PAR5A+8                           57300001
                  LA    R1,G_IRXSUBCM_HOSTENV_NAME                      57310001
                  ST    R1,G_IRXSUBCM_PAR5A+12                          57320001
                  LA    R1,G_IRXSUBCM_ENVBLOCK_PTR                      57330001
                  O     R1,=X'80000000'                                 57340001
                  ST    R1,G_IRXSUBCM_PAR5A+16                          57350001
                  LA    R1,G_IRXSUBCM_PAR5A                             57360002
*                                                                       57370001
                  LINK  EP=IRXSUBCM,SF=(E,G_LINKD)                      57380001
*                                                                       57390001
                  LTR   R15,R15                                         57400001
                  IF (Z) THEN                                           57410001
                     MVI   G_USE_ISPEXEC,C'Y'                           57420001
                  ELSE                                                  57430001
                     C     R15,=A(8)                                    57440001
                     IF (NE) THEN                                       57450001
                        MLWZMRPT RPTLINE=CL133'0Error checking ISPEXEC X57460001
               availability'                                            57470001
                        MVC   G_RETCODE,=F'12'                          57480001
                        BR    R8                                        57490001
                     ENDIF                                              57500001
                  ENDIF                                                 57510001
               ENDIF                                                    57520001
            ENDIF                                                       57530001
*                                                                       57540000
            IF (CLI,G_USE_ISPEXEC,EQ,C'Y') THEN                         57541001
               L     R6,G_SCAN_TOKENA                                   57550001
               MVC   0(12,R6),=C'SELECT CMD(%'                          57560001
               LA    R5,12                                              57570001
               LA    R6,12(,R6)                                         57580001
               LA    R3,STMT_C_EXEC                                     57590001
               XR    R4,R4                                              57600001
               LH    R4,STMT_C_EXECLEN                                  57610001
               CH    R4,=H'8'                                           57620001
               IF (H) THEN                                              57630001
                  LH    R4,=H'8'                                        57640001
               ENDIF                                                    57650001
               BCTR  R4,R0                                              57660001
               B     *+10                                               57670001
               MVC   0(1,R6),0(R3)                                      57680001
               EX    R4,*-6                                             57690001
               LA    R4,1(,R4)                                          57700001
               AR    R6,R4                                              57710001
               AR    R5,R4                                              57720001
               CLC   G_SCAN_TOKEN2_LEN,=F'0'                            57730001
               IF (NE) THEN                                             57740001
                  MVI   0(R6),C' '                                      57750001
                  LA    R6,1(,R6)                                       57760001
                  LA    R5,1(,R5)                                       57770001
                  LR    R0,R6                                           57780001
                  L     R2,G_SCAN_TOKEN2A                               57790001
                  L     R1,G_SCAN_TOKEN2_LEN                            57800001
                  LR    R3,R1                                           57810001
                  MVCL  R0,R2                                           57820001
                  A     R5,G_SCAN_TOKEN2_LEN                            57830001
                  A     R6,G_SCAN_TOKEN2_LEN                            57840001
               ENDIF                                                    57850001
               MVI   0(R6),C')'                                         57860001
               LA    R5,1(,R5)                                          57870001
               ST    R5,G_SCAN_TOKEN_LEN                                57880001
               LA    R1,G_SCAN_TOKEN_LEN                                57890001
               ST    R1,G_ISPEXEC_PAR2A                                 57900001
               L     R1,G_SCAN_TOKENA                                   57910001
               O     R1,=X'80000000'                                    57920001
               ST    R1,G_ISPEXEC_PAR2A+4                               57930001
               LA    R1,G_ISPEXEC_PAR2A                                 57940001
*                                                                       57950001
               LINK  EP=ISPEXEC,SF=(E,G_LINKD)                          57960001
*                                                                       57970001
               LTR   R15,R15                                            57980001
               IF (NZ) THEN                                             57990001
                  MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'    58000001
                  MVC   G_RETCODE,=F'12'                                58010001
                  BR    R8                                              58020001
               ENDIF                                                    58030001
*                                                                       58040001
               B     EXEC_CALL_END                                      58050001
            ENDIF                                                       58051001
*                                                                       58060000
            LA    R6,G_IRXEXEC_EXECBLK                                  58070000
            USING EXECBLK,R6                                            58080000
            MVC   EXEC_BLK_ACRYN,=CL8'IRXEXECB'                         58090000
            LA    R5,EXECBLEN                                           58100000
            ST    R5,EXEC_BLK_LENGTH                                    58110000
            MVC   EXEC_MEMBER,=CL8' '                                   58120000
            LA    R2,EXEC_MEMBER                                        58130000
            LA    R3,STMT_C_EXEC                                        58140000
            XR    R4,R4                                                 58150000
            LH    R4,STMT_C_EXECLEN                                     58160000
            CH    R4,=H'8'                                              58170000
            IF (H) THEN                                                 58180000
               LH    R4,=H'8'                                           58190000
            ENDIF                                                       58200000
            BCTR  R4,R0                                                 58210000
            B     *+10                                                  58220000
            MVC   0(1,R2),0(R3)                                         58230000
            EX    R4,*-6                                                58240000
            MVC   EXEC_DDNAME,=CL8' '                                   58250000
            MVC   EXEC_SUBCOM,=CL8' '                                   58260000
            XR    R5,R5                                                 58270000
            ST    R5,EXEC_BLK_LENGTH+4                                  58280000
            ST    R5,EXEC_DSNPTR                                        58290000
            ST    R5,EXEC_DSNLEN                                        58300000
            DROP  R6                                                    58310000
*                                                                       58320000
            LA    R6,G_IRXEXEC_EVALBLK                                  58330000
            USING EVALBLOCK,R6                                          58340000
            XR    R5,R5                                                 58350000
            ST    R5,EVALBLOCK_EVPAD1                                   58360000
            ST    R5,EVALBLOCK_EVPAD2                                   58370000
            LA    R5,EVALBLK_SIZ                                        58380000
            SRA   R5,3                                                  58390000
            ST    R5,EVALBLOCK_EVSIZE                                   58400000
            DROP  R6                                                    58410000
*                                                                       58420000
            LA    R1,G_IRXEXEC_EXECBLK                                  58430000
            ST    R1,G_IRXEXEC_EXECBLK_PTR                              58440000
            CLC   G_SCAN_TOKEN2_LEN,=F'0'                               58450000
            IF (NE) THEN                                                58460000
               MVC   G_IRXEXEC_ARGS(4),G_SCAN_TOKEN2A                   58470000
               MVC   G_IRXEXEC_ARGS+4(4),G_SCAN_TOKEN2_LEN              58480000
               MVC   G_IRXEXEC_ARGS+8(8),=X'FFFFFFFFFFFFFFFF'           58490000
            ELSE                                                        58500000
               MVC   G_IRXEXEC_ARGS,=X'FFFFFFFFFFFFFFFF'                58510000
            ENDIF                                                       58520000
            LA    R1,G_IRXEXEC_ARGS                                     58530000
            ST    R1,G_IRXEXEC_ARGS_PTR                                 58540000
            MVC   G_IRXEXEC_FLAGS,=X'40000000'                          58550000
            MVC   G_IRXEXEC_INSTBLK_PTR,=A(0)                           58560000
            MVC   G_IRXEXEC_CPPL_PTR,=A(0)                              58570000
            LA    R1,G_IRXEXEC_EVALBLK                                  58580000
            ST    R1,G_IRXEXEC_EVALBLK_PTR                              58590000
            MVC   G_IRXEXEC_WORKAREA_PTR,=A(0)                          58600000
            MVC   G_IRXEXEC_USRFIELD_PTR,=X'8000000'                    58610000
            MVC   G_IRXEXEC_ENVBLOCK_PTR,G_IRXINIT_ENVBLOCK_PTR         58620000
            LA    R1,G_IRXEXEC_REASON                                   58630000
            ST    R1,G_IRXEXEC_REASON_PTR                               58640000
            XR    R0,R0                                                 58650000
            LA    R1,G_IRXEXEC_EXECBLK_PTR                              58660000
            ST    R1,G_IRXEXEC_PAR10A                                   58670000
            LA    R1,G_IRXEXEC_ARGS_PTR                                 58680000
            ST    R1,G_IRXEXEC_PAR10A+4                                 58690000
            LA    R1,G_IRXEXEC_FLAGS                                    58700000
            ST    R1,G_IRXEXEC_PAR10A+8                                 58710000
            LA    R1,G_IRXEXEC_INSTBLK_PTR                              58720000
            ST    R1,G_IRXEXEC_PAR10A+12                                58730000
            LA    R1,G_IRXEXEC_CPPL_PTR                                 58740000
            ST    R1,G_IRXEXEC_PAR10A+16                                58750000
            LA    R1,G_IRXEXEC_EVALBLK_PTR                              58760000
            ST    R1,G_IRXEXEC_PAR10A+20                                58770000
            LA    R1,G_IRXEXEC_WORKAREA_PTR                             58780000
            ST    R1,G_IRXEXEC_PAR10A+24                                58790000
            LA    R1,G_IRXEXEC_USRFIELD_PTR                             58800000
            ST    R1,G_IRXEXEC_PAR10A+28                                58810000
            LA    R1,G_IRXEXEC_ENVBLOCK_PTR                             58820000
            ST    R1,G_IRXEXEC_PAR10A+32                                58830000
            LA    R1,G_IRXEXEC_REASON_PTR                               58840000
            O     R1,=X'80000000'                                       58850000
            ST    R1,G_IRXEXEC_PAR10A+36                                58860000
            LA    R1,G_IRXEXEC_PAR10A                                   58870000
*                                                                       58880000
            LINK  EP=IRXEXEC,SF=(E,G_LINKD)                             58890000
*                                                                       58900000
            LTR   R15,R15                                               58910000
            IF (NZ) THEN                                                58920000
               MLWZMRPT RPTLINE=CL133'0Error executing REXX exec'       58930000
               MVC   G_RETCODE,=F'12'                                   58940000
               BR    R8                                                 58950000
            ENDIF                                                       58960000
*                                                                       58970000
            MVC   G_IRXINIT_ENVBLOCK_PTR,G_IRXEXEC_ENVBLOCK_PTR         58980001
*                                                                       58990001
            LA    R5,G_IRXEXEC_EVALBLK                                  59000000
            USING EVALBLOCK,R5                                          59010000
*                                                                       59020000
            CLC   EVALBLOCK_EVLEN,=F'1'                                 59030000
            BNE   EXEC_REXX_ERROR                                       59040000
            CLI   EVALBLOCK_EVDATA,C'0'                                 59050000
            BE    EXEC_REXX_NO_ERROR                                    59060000
EXEC_REXX_ERROR EQU *                                                   59070000
            MVC   G_LWZMRPT_LINE,=CL133'0REXX exec returned'            59080000
            LA    R2,G_LWZMRPT_LINE+20                                  59090000
            LA    R3,EVALBLOCK_EVDATA                                   59100000
            L     R4,EVALBLOCK_EVLEN                                    59110000
            C     R4,=F'113'                                            59120000
            IF (H) THEN                                                 59130000
               L     R4,=F'113'                                         59140000
            ENDIF                                                       59150000
            BCTR  R4,R0                                                 59160000
            B     *+10                                                  59170000
            MVC   0(1,R2),0(R3)                                         59180000
            EX    R4,*-6                                                59190000
            L     R15,G_LWZMAKE_RPTA                                    59200000
            BASR  R14,R15                                               59210000
            MVC   G_RETCODE,=F'8'                                       59220000
            BR    R8                                                    59230000
EXEC_REXX_NO_ERROR EQU *                                                59240000
*                                                                       59250000
            DROP  R5                                                    59260000
*                                                                       59270000
EXEC_CALL_END EQU *                                                     59280000
            L     R2,G_SCAN_TOKEN_MAXLEN                                59290000
            L     R3,G_SCAN_TOKENA                                      59300000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  59310000
            MVC   G_SCAN_TOKENA,EXEC_SAVE_SCAN_TOKENA                   59320000
            MVC   G_SCAN_TOKEN_MAXLEN,EXEC_SAVE_SCAN_TOKEN_MAXLEN       59330000
            MVC   G_SCAN_TOKEN_LEN,EXEC_SAVE_SCAN_TOKEN_LEN             59340000
*                                                                       59350000
            L     R2,G_SCAN_TOKEN2_MAXLEN                               59360000
            L     R3,G_SCAN_TOKEN2A                                     59370000
            STORAGE RELEASE,LENGTH=(R2),ADDR=(R3) * Free value storage  59380000
            MVC   G_SCAN_TOKEN2A,EXEC_SAVE_SCAN_TOKEN2A                 59390000
            MVC   G_SCAN_TOKEN2_MAXLEN,EXEC_SAVE_SCAN_TOKEN2_MAXLEN     59400000
            MVC   G_SCAN_TOKEN2_LEN,EXEC_SAVE_SCAN_TOKEN2_LEN           59410000
         ENDIF                                                          59420000
*                                                                       59430000
         DROP  R7                                                       59440000
         USING STMT_DSECT,R7                                            59450000
*                                                                       59460000
         LT    R7,STMT_NEXT_PTR                                         59470000
         BZ    EXEC_TGT_BUILD_RET                                       59480000
*                                                                       59490000
         CLI   STMT_IN_RECIPE,C'Y'                                      59500000
         BE    NEXT_RECIPE_STMT                                         59510000
*                                                                       59520000
         DROP  R7                                                       59530000
*                                                                       59540000
EXEC_TGT_BUILD_RET EQU *                                                59550000
         BR    R8                                                       59560000
*                                                                       59570000
EXEC_TGT_BUILD_NO_RECIPE EQU *                                          59580000
         MLWZMRPT RPTLINE=CL133' ..................... No recipe'       59590000
         BR    R8                                                       59600000
*                                                                       59610000
         LTORG                                                          59620000
*                                                                       59630000
LWZMAKE_SCAN_TOKENA_EXEC    DC    A(LWZMAKE_SCAN_TOKEN)                 59640000
LWZMAKE_SCAN_VARA_EXEC      DC    A(LWZMAKE_SCAN_VAR)                   59650000
LWZMAKE_FINDPNYA_EXEC       DC    A(LWZMAKE_FINDPNY)                    59660000
LWZMAKE_FINDTGTA_EXEC       DC    A(LWZMAKE_FINDTGT)                    59670000
LWZMAKE_EXEC_TGTA_EXEC      DC    A(LWZMAKE_EXEC_TGT)                   59680000
LWZMAKE_GET_DATEA_EXEC      DC    A(LWZMAKE_GET_DATE)                   59690000
LWZMAKE_APPEND_TOKENA_EXEC  DC    A(LWZMAKE_APPEND_TOKEN)               59700000
*                                                                       59710000
WORKAREA_EXEC_TGT           DSECT                                       59720000
EXEC_TGT_SA                 DS    18F                                   59730000
RETCODE_EXEC_TGT            DS    F                                     59740000
EXEC_TGT_PARA               DS    A                                     59750000
EXEC_NEXTTGT_PARA           DS    A                                     59760000
EXEC_NEXTTGT_PAR            DS    CL(EXEC_TGT_PAR_LEN)                  59770000
*                                                                       59780000
                            DS    0F                                    59790000
TARGET_ALTER_DATE           DS    CL16                                  59800000
*                                                                       59810000
                            DS    0F                                    59820000
EXEC_WORD_SPLIT_PTR         DS    A                                     59830000
EXEC_WORD_SPLIT_LEN         DS    F                                     59840000
*                                                                       59850000
                            DS    0F                                    59860000
EXEC_IRXEXECB               DS    CL(EXECBLK_V2_LEN)                    59870000
*                                                                       59880000
EXEC_WTOBLOCK               DS    0F                                    59890000
EXEC_WTOLEN                 DS    H                                     59900000
EXEC_WTOFIL                 DS    H                                     59910000
EXEC_WTOTEXT                DS    CL133                                 59920000
*                                                                       59930000
                            DS    0F                                    59940000
EXEC_SAVE_SCAN_TOKEN_LEN    DS    F                                     59950000
EXEC_SAVE_SCAN_TOKEN2_LEN   DS    F                                     59960000
EXEC_SAVE_SCAN_TOKEN_MAXLEN DS    F                                     59970000
EXEC_SAVE_SCAN_TOKEN2_MAXLEN DS    F                                    59980000
EXEC_SAVE_SCAN_TOKENA       DS    A                                     59990000
EXEC_SAVE_SCAN_TOKEN2A      DS    A                                     60000000
WORKAREA_EXEC_TGT_LEN       EQU *-WORKAREA_EXEC_TGT                     60010000
*                                                                       60020000
EXEC_TGT_PAR                DSECT                                       60030000
EXEC_TGT_PTR                DS    A                                     60040000
EXEC_TGT_PAR_LEN            EQU   *-EXEC_TGT_PAR                        60050000
*                                                                       60060000
LWZMAKE  CSECT                                                          60070000
*                                                                       60080000
* Get the date of a file                                                60090000
*                                                                       60100000
         DROP                                                           60110000
*                                                                       60120000
LWZMAKE_GET_DATE DS    0F                                               60130000
         STM   R14,R12,12(R13)   * Save callers registers               60140000
         LR    R10,R15                                                  60150000
         LA    R11,4095(,R10)                                           60160000
         LA    R11,1(,R11)                                              60170000
         USING LWZMAKE_GET_DATE,R10,R11                                 60180000
         GETMAIN RU,LV=GET_DATE_DSECT_SIZ                               60190000
         ST    R13,4(R1)         * Backward chain callers SA            60200000
         ST    R1,8(R13)         * Forward chain my SA                  60210000
         LR    R13,R1            * Point R13 to my SA                   60220000
         USING GET_DATE_DSECT,R13 * Establish addressing of workarea    60230000
         USING GLOBAL_DATA_DSECT,R9                                     60240000
*                                                                       60250000
*        Trace record to start section                                  60260000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX60270000
               KE_GET_DATE'                                             60280000
*                                                                       60290000
         MVC   G_SAVE_ALTER_DATE,=CL16' '                               60300000
*                                                                       60310000
         L     R3,G_SCAN_TOKENA                                         60320000
         L     R4,G_SCAN_TOKEN_LEN                                      60330000
         XR    R6,R6                                                    60340000
         XR    R7,R7                                                    60350000
*                                                                       60360000
GET_DATE_TEST_QUAL1 EQU *                                               60370000
         LTR   R4,R4                                                    60380000
         BZ    GET_DATE_NOT_MVSDS                                       60390000
         TRT   0(1,R3),TRT_ALPHANAT                                     60400000
         BNZ   GET_DATE_NOT_MVSDS                                       60410000
         LA    R3,1(,R3)                                                60420000
         BCT   R4,*+8                                                   60430000
         B     GET_DATE_MVSDS                                           60440000
         LR    R5,R4                                                    60450000
         C     R5,=F'7'                                                 60460000
         IF (H) THEN                                                    60470000
            L     R5,=F'7'                                              60480000
         ENDIF                                                          60490000
         BCTR  R5,R0                                                    60500000
         B     *+10                                                     60510000
         TRT   0(1,R3),TRT_ALPHANUMNATDASH                              60520000
         EX    R5,*-6                                                   60530000
         IF (Z) THEN                                                    60540000
            LA    R5,1(,R5)                                             60550000
         ELSE                                                           60560000
            LR    R5,R1                                                 60570000
            SR    R5,R3                                                 60580000
         ENDIF                                                          60590000
         AR    R3,R5                                                    60600000
         SR    R4,R5                                                    60610000
         BZ    GET_DATE_MVSDS                                           60620000
         IF (CLI,0(R3),EQ,C'.') THEN                                    60630000
            LA    R3,1(,R3)                                             60640000
            BCTR  R4,R0                                                 60650000
            B     GET_DATE_TEST_QUAL1                                   60660000
         ENDIF                                                          60670000
         CLI   0(R3),C'('                                               60680000
         BNE   GET_DATE_NOT_MVSDS                                       60690000
         LR    R6,R3                                                    60700000
         LA    R3,1(,R3)                                                60710000
         BCT   R4,*+8                                                   60720000
         B     GET_DATE_NOT_MVSDS                                       60730000
         TRT   0(1,R3),TRT_ALPHANAT                                     60740000
         BNZ   GET_DATE_NOT_MVSDS                                       60750000
         LR    R6,R3                                                    60760000
         LA    R3,1(,R3)                                                60770000
         BCT   R4,*+8                                                   60780000
         B     GET_DATE_NOT_MVSDS                                       60790000
         LR    R5,R4                                                    60800000
         C     R5,=F'7'                                                 60810000
         IF (H) THEN                                                    60820000
            L     R5,=F'7'                                              60830000
         ENDIF                                                          60840000
         BCTR  R5,R0                                                    60850000
         B     *+10                                                     60860000
         TRT   0(1,R3),TRT_ALPHANUMNATDASH                              60870000
         EX    R5,*-6                                                   60880000
         IF (Z) THEN                                                    60890000
            LA    R5,1(,R5)                                             60900000
         ELSE                                                           60910000
            LR    R5,R1                                                 60920000
            SR    R5,R3                                                 60930000
         ENDIF                                                          60940000
         AR    R3,R5                                                    60950000
         SR    R4,R5                                                    60960000
         BZ    GET_DATE_NOT_MVSDS                                       60970000
         CLI   0(R3),C')'                                               60980000
         BNE   GET_DATE_NOT_MVSDS                                       60990000
         LR    R7,R3                                                    61000000
         SR    R7,R6                                                    61010000
         LA    R3,1(,R3)                                                61020000
         BCT   R4,GET_DATE_NOT_MVSDS                                    61030000
*                                                                       61040000
GET_DATE_MVSDS EQU *                                                    61050000
         ST    R6,G_MVSDS_MEMBER_PTR                                    61060000
         ST    R7,G_MVSDS_MEMBER_LEN                                    61070000
*                                                                       61080000
         MLWZMRPT RPTLINE=CL133' ..................... Name is MVS dataX61090000
                set name'                                               61100000
*                                                                       61110000
         BAL   R8,GET_DATE_IGGCSI00                                     61120000
*                                                                       61130000
         CLC   G_RETCODE,=A(0)                                          61140000
         BNE   GET_DATE_RET                                             61150000
*                                                                       61160000
         CLI   G_DSFOUND,C'Y'                                           61170000
         BNE   GET_DATE_RET                                             61180000
*                                                                       61190000
         CLC   G_MVSDS_MEMBER_PTR,=A(0)                                 61200000
         BE    GET_DATE_RET                                             61210000
*                                                                       61220000
         BAL   R8,GET_DATE_OBTAIN                                       61230000
*                                                                       61240000
         CLC   G_RETCODE,=A(0)                                          61250000
         BNE   GET_DATE_RET                                             61260000
*                                                                       61270000
         IF (TM,OBTAIN_GD+(DS1RECFM-OBTAIN_DSECT),DS1RECFU,O) THEN      61280000
            BAL   R8,GET_DATE_LOADMOD                                   61290000
         ELSE                                                           61300000
            BAL   R8,GET_DATE_STATS                                     61310000
         ENDIF                                                          61320000
*                                                                       61330000
         B     GET_DATE_RET                                             61340000
*                                                                       61350000
GET_DATE_NOT_MVSDS EQU *                                                61360000
         MLWZMRPT RPTLINE=CL133' ..................... Name is not MVS X61370000
               data set name'                                           61380000
         MVC   G_SAVE_ALTER_DATE,=16X'FF'                               61390000
*                                                                       61400000
GET_DATE_RET EQU *                                                      61410000
         L     R3,4(,R13)        * Restore address of callers SA        61420000
         FREEMAIN RU,LV=GET_DATE_DSECT_SIZ,A=(R13)                      61430000
         LR    R13,R3                                                   61440000
         LM    R14,R12,12(R13)                                          61450000
         BR    R14                    Return to caller                  61460000
*                                                                       61470000
* Perform catalog search with IGGCSI00                                  61480000
*                                                                       61490000
GET_DATE_IGGCSI00 EQU *                                                 61500000
         MVI   G_DSFOUND,C'N'                                           61510000
*                                                                       61520000
         LA    R1,DAREA_GD                                              61530000
         ST    R1,DAREAPTR_GD                                           61540000
         L     R2,=A(DAREA_GD_SIZ)                                      61550000
         ST    R2,0(,R1)                                                61560000
*                                                                       61570000
         LA    R2,CSIFIELD_GD                                           61580000
         L     R3,=A(CSIFIELD_GD_LEN)                                   61590000
         LA    R4,CONST_CSIFIELD_GD                                     61600000
         L     R5,=A(CONST_CSIFIELD_GD_LEN)                             61610000
         MVCL  R2,R4                                                    61620000
*                                                                       61630000
         LA    R2,CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)                 61640000
         L     R3,G_SCAN_TOKENA                                         61650000
         L     R4,G_SCAN_TOKEN_LEN                                      61660000
         LT    R5,G_MVSDS_MEMBER_PTR                                    61670000
         IF (NZ) THEN                                                   61680000
            SR    R5,R3                                                 61690000
            BCTR  R5,R0                                                 61700000
            LR    R4,R5                                                 61710000
         ENDIF                                                          61720000
         C     R4,=A(L'CSIFILTK)                                        61730000
         IF (H)                                                         61740000
            L     R4,=A(L'CSIFILTK)                                     61750000
         ENDIF                                                          61760000
         BCTR  R4,R0                                                    61770000
         B     *+10                                                     61780000
         MVC   0(1,R2),0(R3)                                            61790000
         EX    R4,*-6                                                   61800000
*                                                                       61810000
         LA    R1,PARMLIST_GD                                           61820000
         LA    R2,MODRSNRT_GD                                           61830000
         ST    R2,0(R1)                                                 61840000
         LA    R2,CSIFIELD_GD                                           61850000
         ST    R2,4(R1)                                                 61860000
         L     R2,DAREAPTR_GD                                           61870000
         O     R2,=X'80000000'                                          61880000
         ST    R2,8(R1)                                                 61890000
*                                                                       61900000
         L     R15,G_IGGCSI00A                                          61910000
         BASR  R14,R15                                                  61920000
*                                                                       61930000
         C     R15,=F'4'                                                61940000
         IF (H) THEN                                                    61950000
            MLWZMRPT RPTLINE=CL133'0Catalog search interface returned eX61960000
               rror code'                                               61970000
            MVC   G_RETCODE,=F'12'                                      61980000
            BR    R8                                                    61990000
         ENDIF                                                          62000000
*                                                                       62010000
         LA    R1,DAREA_GD                                              62020000
         CLC   8(4,R1),=F'64'                                           62030000
         IF (H) THEN                                                    62040000
            MVI   G_DSFOUND,C'Y'                                        62050000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Found inX62060000
                catalog'                                                62070000
         ELSE                                                           62080000
            MVC   G_SAVE_ALTER_DATE,=16X'FF'                            62090000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Not founX62100000
               d in catalog'                                            62110000
         ENDIF                                                          62120000
         L     R15,G_LWZMAKE_RPTA                                       62130000
         BASR  R14,R15                                                  62140000
*                                                                       62150000
GET_DATE_IGGCSI00_RET EQU *                                             62160000
         BR    R8                                                       62170000
*                                                                       62180000
* Perform CAMLST OBTAIN                                                 62190000
*                                                                       62200000
GET_DATE_OBTAIN EQU *                                                   62210000
         XR    R1,R1                                                    62220000
         ICM   R1,B'1000',=AL1(193)                                     62230000
         ST    R1,DSCBPAR_GD                                            62240000
         MVC   OBTAIN_GD+(DS1DSNAM-OBTAIN_DSECT)(L'DS1DSNAM),CSIFIELD_GX62250000
               D+(CSIFILTK-CSIFIELD_DSECT)                              62260000
         LA    R1,OBTAIN_GD+(DS1DSNAM-OBTAIN_DSECT)                     62270000
         ST    R1,DSCBPAR_GD+4                                          62280000
         LA    R1,DAREA_GD+110                                          62290000
         CLC   0(2,R1),=H'12'   * Is volume name present                62300000
         BL    GET_DATE_OBTAIN_RET                                      62310000
         LA    R1,6(,R1)                                                62320000
         ST    R1,DSCBPAR_GD+8                                          62330000
         LA    R1,OBTAIN_GD+(DS1FMTID-OBTAIN_DSECT)                     62340000
         ST    R1,DSCBPAR_GD+12                                         62350000
*                                                                       62360000
         OBTAIN DSCBPAR_GD                                              62370000
*                                                                       62380000
         LTR   R15,R15                                                  62390000
         IF (NZ) THEN                                                   62400000
            MLWZMRPT RPTLINE=CL133'0CAMLST OBTAIN returned error code'  62410000
            MVC   G_RETCODE,=F'12'                                      62420000
            BR    R8                                                    62430000
         ENDIF                                                          62440000
*                                                                       62450000
         IF (TM,OBTAIN_GD+(DS1DSORG-OBTAIN_DSECT),DS1DSGPO,Z) THEN      62460000
            MVC   G_LWZMRPT_LINE,=CL133'0Member specified on non-PDS daX62470000
               taset'                                                   62480000
            MVC   G_LWZMRPT_LINE+37(L'CSIFILTK),CSIFIELD_GD+(CSIFILTK-CX62490000
               SIFIELD_DSECT)                                           62500000
            L     R15,G_LWZMAKE_RPTA                                    62510000
            BASR  R14,R15                                               62520000
            MVC   G_RETCODE,=F'8'                                       62530000
            BR    R8                                                    62540000
         ENDIF                                                          62550000
*                                                                       62560000
GET_DATE_OBTAIN_RET EQU *                                               62570000
         BR    R8                                                       62580000
*                                                                       62590000
* Get the date from a load module                                       62600000
*                                                                       62610000
GET_DATE_LOADMOD EQU *                                                  62620000
         MLWZMRPT RPTLINE=CL133' ..................... Retrieving load X62630000
               module creation date/time'                               62640000
*                                                                       62650000
         MVI   G_DSFOUND,C'N'                                           62660000
*                                                                       62670000
         MVC   MEM8_GD,=CL8' '                                          62680000
         LA    R2,MEM8_GD                                               62690000
         L     R3,G_MVSDS_MEMBER_PTR                                    62700000
         L     R4,G_MVSDS_MEMBER_LEN                                    62710000
         BCTR  R4,R0                                                    62720000
         B     *+10                                                     62730000
         MVC   0(1,R2),0(R3)                                            62740000
         EX    R4,*-6                                                   62750000
*                                                                       62760000
         LA    R6,DYNALLOC_AREA_GD                                      62770000
         USING S99RBP,R6                                                62780000
         LA    R4,S99RBPTR+4                                            62790000
         USING S99RB,R4                                                 62800000
         ST    R4,S99RBPTR                                              62810000
         OI    S99RBPTR,S99RBPND                                        62820000
         XC    S99RB(S99RBEND-S99RB),S99RB                              62830000
         MVI   S99RBLN,S99RBEND-S99RB                                   62840000
         MVI   S99VERB,S99VRBAL                                         62850000
         OI    S99FLG11,S99MSGL0                                        62860000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             62870000
         MVC   0(CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_GD_L,R5),CDSNTU_GD  62880000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           62890000
         LA    R3,S99RB+(S99RBEND-S99RB)                                62900000
         ST    R3,S99TXTPP                                              62910000
         ST    R5,0(,R3)                                                62920000
         LA    R5,CDSNTU_GD_L(,R5)                                      62930000
         ST    R5,4(,R3)                                                62940000
         LA    R5,CSTATUSTU_GD_L(,R5)                                   62950000
         O     R5,=X'80000000'                                          62960000
         ST    R5,8(,R3)                                                62970000
         LA    R1,DYNALLOC_AREA_GD                                      62980000
         DYNALLOC                                                       62990000
*                                                                       63000000
         DROP  R6                                                       63010000
         DROP  R4                                                       63020000
*                                                                       63030000
         LTR   R15,R15                                                  63040000
         IF (NZ) THEN                                                   63050000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'         63060000
            MVC   G_RETCODE,=F'8'                                       63070000
            BR    R8                                                    63080000
         ENDIF                                                          63090000
*                                                                       63100000
         L     R1,G_DCB_MEM_PTR                                         63110000
         LA    R2,DCBPDS_BDR-DCB_DSECT(,R1)                             63120000
         MVC   0(LEN_DCBPDS_BDR,R2),CDCBPDS_BDR                         63130000
         L     R6,DYNALLOC_AREA_GD                                      63140000
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_GD_L+CSTATUSTU_GD_L(,R6)   63150000
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)                              63160000
*                                                                       63170000
         OPEN  ((R2)),MODE=31,MF=(E,G_OPEND)                            63180000
*                                                                       63190000
         LTR   R15,R15                                                  63200000
         IF (NZ) THEN                                                   63210000
            MLWZMRPT RPTLINE=CL133'0OPEN failed for accessing PDS with X63220000
               load modules'                                            63230000
            MVC   G_RETCODE,=F'8'                                       63240000
            B     GET_DATE_LOADMOD_DEALLOC                              63250000
         ENDIF                                                          63260000
*                                                                       63270000
         MVC   IEWBFDAT_SB_SB(2),=C'SB'                                 63280000
         MVC   IEWBFDAT_SB_SB+2(2),=X'0001'                             63290000
         XC    IEWBFDAT_SB_MTOKEN,IEWBFDAT_SB_MTOKEN                    63300000
         MVC   IEWBFDAT_SB_PGMNAME,MEM8_GD                              63310000
*                                                                       63320000
         LA    R1,IEWBFDAT_SB_SB                                        63330000
         ST    R1,IEWBFDAT_SB_PAR4A                                     63340000
         LA    R1,IEWBFDAT_SB_MTOKEN                                    63350000
         ST    R1,IEWBFDAT_SB_PAR4A+4                                   63360000
         L     R14,G_DCB_MEM_PTR                                        63370000
         LA    R1,DCBPDS_BDR-DCB_DSECT(,R14)                            63380000
         ST    R1,IEWBFDAT_SB_PAR4A+8                                   63390000
         LA    R1,IEWBFDAT_SB_PGMNAME                                   63400000
         O     R1,=X'80000000'                                          63410000
         ST    R1,IEWBFDAT_SB_PAR4A+12                                  63420000
         LA    R1,IEWBFDAT_SB_PAR4A                                     63430000
*                                                                       63440000
         L     R15,G_IEWBFDATA                                          63450000
         BASR  R14,R15                                                  63460000
*                                                                       63470000
         C     R15,=A(0)                                                63480000
         BE    GET_DATE_LOADMOD_NOERR1                                  63490000
         C     R15,=A(4)                                                63500000
         BE    GET_DATE_LOADMOD_NOERR1                                  63510000
         C     R15,=A(12)                                               63520000
         IF (EQ) THEN                                                   63530000
            C     R0,=X'10800032'                                       63540000
            BE    GET_DATE_LOADMOD_NOTFOUND                             63550000
         ENDIF                                                          63560000
         CVD   R15,G_DEC8         * convert return value to packed      63570000
         UNPK  G_ZONED8,G_DEC8    * convert return value to zoned       63580000
         OI    G_ZONED8+7,X'F0'   * get rid of sign                     63590000
         MVC   G_HELPER_DATA(8),G_ZONED8                                63600000
         MVI   G_HELPER_DATA+8,C' '                                     63610000
         ST    R0,G_DEC8          * Put ptr in area of at least 5 bytes 63620000
         UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex           63630000
         TR    G_ZONED8,GETDATE_HEXTAB * Turn into hex                  63640000
         MVC   G_HELPER_DATA+9(8),G_ZONED8                              63650000
         LA    R14,G_HELPER_DATA                                        63660000
         ST    R14,G_LWZMTRC_DATA_PTR                                   63670000
         MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)                              63680000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA           63690000
         MLWZMRPT RPTLINE=CL133'0Error starting binder fast data accessX63700000
                session'                                                63710000
         MVC   G_RETCODE,=F'12'                                         63720000
         BR    R8                                                       63730000
*                                                                       63740000
GET_DATE_LOADMOD_NOERR1 EQU *                                           63750000
         MVI   G_DSFOUND,C'Y'                                           63760000
*                                                                       63770000
IEWBIDB_BASE EQU R6                      Base register for IDRB buffer. 63780000
IDB_BASE     EQU R7                      Base register for IDRB entry.  63790000
         IEWBUFF FUNC=GETBUF,TYPE=IDRB   Get memory for IDRB buffer.    63800000
         IEWBUFF FUNC=INITBUF,TYPE=IDRB  Init IDRB buffer.              63810000
*                                                                       63820000
         MVC   IEWBFDAT_GD_GD(2),=C'GD'                                 63830000
         MVC   IEWBFDAT_GD_GD+2(2),=X'0001'                             63840000
         MVC   IEWBFDAT_GD_MTOKEN,IEWBFDAT_SB_MTOKEN                    63850000
         MVC   IEWBFDAT_GD_B_IDRB(2),=H'6'                              63860000
         MVC   IEWBFDAT_GD_B_IDRB+2(6),=C'B_IDRB'                       63870000
         XC    IEWBFDAT_GD_CURSOR,IEWBFDAT_GD_CURSOR                    63880000
*                                                                       63890000
         LA    R1,IEWBFDAT_GD_GD                                        63900000
         ST    R1,IEWBFDAT_GD_PAR8A                                     63910000
         LA    R1,IEWBFDAT_GD_MTOKEN                                    63920000
         ST    R1,IEWBFDAT_GD_PAR8A+4                                   63930000
         LA    R1,IEWBFDAT_GD_B_IDRB                                    63940000
         ST    R1,IEWBFDAT_GD_PAR8A+8                                   63950000
         XR    R1,R1                                                    63960000
         ST    R1,IEWBFDAT_GD_PAR8A+12                                  63970000
         ST    IEWBIDB_BASE,IEWBFDAT_GD_PAR8A+16                        63980000
         LA    R1,IEWBFDAT_GD_CURSOR                                    63990000
         ST    R1,IEWBFDAT_GD_PAR8A+20                                  64000000
         LA    R1,IEWBFDAT_GD_COUNT                                     64010000
         ST    R1,IEWBFDAT_GD_PAR8A+24                                  64020000
         L     R1,=X'80000000'                                          64030000
         ST    R1,IEWBFDAT_GD_PAR8A+28                                  64040000
         LA    R1,IEWBFDAT_GD_PAR8A                                     64050000
*                                                                       64060000
         L     R15,G_IEWBFDATA                                          64070000
         BASR  R14,R15                                                  64080000
*                                                                       64090000
         C     R15,=A(0)                                                64100000
         IF (NE) THEN                                                   64110000
            C     R15,=A(4)                                             64120000
         ENDIF                                                          64130000
         IF (NE) THEN                                                   64140000
            CVD   R15,G_DEC8      * convert return value to packed      64150000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       64160000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    64170000
            MVC   G_HELPER_DATA(8),G_ZONED8                             64180000
            MVI   G_HELPER_DATA+8,C' '                                  64190000
            ST    R0,G_DEC8       * Put ptr in area of at least 5 bytes 64200000
            UNPK  G_ZONED8(9),G_DEC8(5)   * Turn into almost hex        64210000
            TR    G_ZONED8,GETDATE_HEXTAB * Turn into hex               64220000
            MVC   G_HELPER_DATA+9(8),G_ZONED8                           64230000
            LA    R14,G_HELPER_DATA                                     64240000
            ST    R14,G_LWZMTRC_DATA_PTR                                64250000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)                           64260000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA        64270000
            MLWZMRPT RPTLINE=CL133'0Error during binder fast data accesX64280000
               s get data function'                                     64290000
            MVC   G_RETCODE,=F'12'                                      64300000
            BR    R8                                                    64310000
         ENDIF                                                          64320000
*                                                                       64330000
         MVC   IEWBFDAT_EN_EN(2),=C'EN'                                 64340000
         MVC   IEWBFDAT_EN_EN+2(2),=X'0001'                             64350000
         MVC   IEWBFDAT_EN_MTOKEN,IEWBFDAT_SB_MTOKEN                    64360000
*                                                                       64370000
         LA    R1,IEWBFDAT_EN_EN                                        64380000
         ST    R1,IEWBFDAT_EN_PAR2A                                     64390000
         LA    R1,IEWBFDAT_EN_MTOKEN                                    64400000
         ST    R1,IEWBFDAT_EN_PAR2A+4                                   64410000
         LA    R1,IEWBFDAT_EN_PAR2A                                     64420000
*                                                                       64430000
         L     R15,G_IEWBFDATA                                          64440000
         BASR  R14,R15                                                  64450000
*                                                                       64460000
         LTR   R15,R15                                                  64470000
         IF (NZ) THEN                                                   64480000
            CVD   R15,G_DEC8      * convert return value to packed      64490000
            UNPK  G_ZONED8,G_DEC8 * convert return value to zoned       64500000
            OI    G_ZONED8+7,X'F0' * get rid of sign                    64510000
            MVC   G_HELPER_DATA(8),G_ZONED8                             64520000
            MVI   G_HELPER_DATA+8,C' '                                  64530000
            ST    R0,G_DEC8       * Put ptr in area of at least 5 bytes 64540000
            UNPK  G_ZONED8(9),G_DEC8(5)      * Turn into almost hex     64550000
            TR    G_ZONED8,GETDATE_HEXTAB    * Turn into hex            64560000
            MVC   G_HELPER_DATA+9(8),G_ZONED8                           64570000
            LA    R14,G_HELPER_DATA                                     64580000
            ST    R14,G_LWZMTRC_DATA_PTR                                64590000
            MVC   G_LWZMTRC_DATA_SIZ,=AL2(17)                           64600000
            MLWZMTRC LEVEL=LWZMAKE_TRACE_ERROR,MSGNR=C'010',DATA        64610000
            MLWZMRPT RPTLINE=CL133'0Error ending binder fast data accesX64620000
               s session'                                               64630000
            MVC   G_RETCODE,=F'12'                                      64640000
            BR    R8                                                    64650000
         ENDIF                                                          64660000
*                                                                       64670000
         MVI   CONVTOD_INAREA,X'00'                                     64680000
         MVC   CONVTOD_INAREA+1(15),CONVTOD_INAREA                      64690000
         PACK  CONVTOD_INAREA(4),IDB_TIME_BOUND(L'IDB_TIME_BOUND+1)     64700000
         MVI   CONVTOD_INAREA+4,X'00'                                   64710000
         PACK  CONVTOD_INAREA+8(5),IDB_DATE_BOUND(L'IDB_DATE_BOUND+1)   64720000
         MVI   CONVTOD_OUTAREA,X'00'                                    64730000
         MVC   CONVTOD_OUTAREA+1(7),CONVTOD_OUTAREA                     64740000
         MVI   STCKCONV_OUTAREA,X'00'                                   64750000
         MVC   STCKCONV_OUTAREA+1(15),STCKCONV_OUTAREA                  64760000
         CONVTOD CONVVAL=CONVTOD_INAREA,TODVAL=CONVTOD_OUTAREA,TIMETYPEX64770000
               =DEC,DATETYPE=YYYYDDD,MF=(E,CONVTOD_PLIST)               64780000
         STCKCONV STCKVAL=CONVTOD_OUTAREA,CONVVAL=STCKCONV_OUTAREA,TIMEX64790000
               TYPE=DEC,DATETYPE=YYYYMMDD,MF=(E,STCKCONV_PLIST)         64800000
         MVC   DATEWORK_DEC_1(4),STCKCONV_OUTAREA+8                     64810000
         MVC   DATEWORK_DEC_1+4(3),STCKCONV_OUTAREA                     64820000
         MVO   DATEWORK_DEC_2,DATEWORK_DEC_1(7)                         64830000
         MVN   DATEWORK_DEC_2+7(1),=X'0F'                               64840000
         UNPK  DATEWORK_ZON,DATEWORK_DEC_2                              64850000
         MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON                           64860000
*                                                                       64870000
         IEWBUFF FUNC=FREEBUF,TYPE=IDRB  Free IDRB buffer.              64880000
*                                                                       64890000
GET_DATE_LOADMOD_NOTFOUND EQU *                                         64900000
*                                                                       64910000
         IF (CLI,G_DSFOUND,EQ,C'Y') THEN                                64920000
            MVC   G_LWZMRPT_LINE,=CL133' ..................... Load modX64930000
               ule found in PDS, last altered on'                       64940000
            MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON                        64950000
            MVC   G_LWZMRPT_LINE+65(19),=C'0000-00-00 00:00:00'         64960000
            MVC   G_LWZMRPT_LINE+65(4),G_SAVE_ALTER_DATE+2              64970000
            MVC   G_LWZMRPT_LINE+70(2),G_SAVE_ALTER_DATE+6              64980000
            MVC   G_LWZMRPT_LINE+73(2),G_SAVE_ALTER_DATE+8              64990000
            MVC   G_LWZMRPT_LINE+76(2),G_SAVE_ALTER_DATE+10             65000000
            MVC   G_LWZMRPT_LINE+79(2),G_SAVE_ALTER_DATE+12             65010000
            MVC   G_LWZMRPT_LINE+82(2),G_SAVE_ALTER_DATE+14             65020000
            L     R15,G_LWZMAKE_RPTA                                    65030000
            BASR  R14,R15                                               65040000
         ELSE                                                           65050000
            MVC   G_SAVE_ALTER_DATE,=16X'FF'                            65060000
            MLWZMRPT RPTLINE=CL133' ..................... Load module nX65070000
               ot found in PDS'                                         65080000
         ENDIF                                                          65090000
*                                                                       65100000
         DROP  R6                                                       65110000
         DROP  R7                                                       65120000
*                                                                       65130000
GET_DATE_LOADMOD_CLOSE EQU *                                            65140000
         L     R14,G_DCB_MEM_PTR                                        65150000
         LA    R2,DCBPDS_BDR-DCB_DSECT(,R14)                            65160000
         CLOSE ((R2)),MODE=31                                           65170000
*                                                                       65180000
GET_DATE_LOADMOD_DEALLOC EQU *                                          65190000
         LA    R6,DYNALLOC_AREA_GD                                      65200000
         USING S99RBP,R6                                                65210000
         LA    R4,S99RBPTR+4                                            65220000
         USING S99RB,R4                                                 65230000
         ST    R4,S99RBPTR                                              65240000
         OI    S99RBPTR,S99RBPND                                        65250000
         XC    S99RB(S99RBEND-S99RB),S99RB                              65260000
         MVI   S99RBLN,S99RBEND-S99RB                                   65270000
         MVI   S99VERB,S99VRBUN                                         65280000
         OI    S99FLG11,S99MSGL0                                        65290000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             65300000
         MVC   0(CDSNTU_GD_L,R5),CDSNTU_GD                              65310000
         LA    R2,CSIFIELD_GD                                           65320000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           65330000
         LA    R3,S99RB+(S99RBEND-S99RB)                                65340000
         ST    R3,S99TXTPP                                              65350000
         O     R5,=X'80000000'                                          65360000
         ST    R5,0(,R3)                                                65370000
         LA    R1,DYNALLOC_AREA_GD                                      65380000
         DYNALLOC                                                       65390000
*                                                                       65400000
         LTR   R15,R15                                                  65410000
         IF (NZ) THEN                                                   65420000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'       65430000
            MVC   G_RETCODE,=F'8'                                       65440000
            BR    R8                                                    65450000
         ENDIF                                                          65460000
*                                                                       65470000
GET_DATE_LOADMOD_RET EQU *                                              65480000
         BR    R8                                                       65490000
*                                                                       65500000
* Get the date from member stats                                        65510000
*                                                                       65520000
GET_DATE_STATS EQU *                                                    65530000
         MLWZMRPT RPTLINE=CL133' ..................... Retrieving PDS mX65540000
               ember stats'                                             65550000
*                                                                       65560000
         MVI   G_DSFOUND,C'N'                                           65570000
*                                                                       65580000
         MVC   MEM8_GD,=CL8' '                                          65590000
         LA    R2,MEM8_GD                                               65600000
         L     R3,G_MVSDS_MEMBER_PTR                                    65610000
         L     R4,G_MVSDS_MEMBER_LEN                                    65620000
         BCTR  R4,R0                                                    65630000
         B     *+10                                                     65640000
         MVC   0(1,R2),0(R3)                                            65650000
         EX    R4,*-6                                                   65660000
*                                                                       65670000
         LA    R6,DYNALLOC_AREA_GD                                      65680000
         USING S99RBP,R6                                                65690000
         LA    R4,S99RBPTR+4                                            65700000
         USING S99RB,R4                                                 65710000
         ST    R4,S99RBPTR                                              65720000
         OI    S99RBPTR,S99RBPND                                        65730000
         XC    S99RB(S99RBEND-S99RB),S99RB                              65740000
         MVI   S99RBLN,S99RBEND-S99RB                                   65750000
         MVI   S99VERB,S99VRBAL                                         65760000
         OI    S99FLG11,S99MSGL0                                        65770000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             65780000
         MVC   0(CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_GD_L,R5),CDSNTU_GD  65790000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           65800000
         LA    R3,S99RB+(S99RBEND-S99RB)                                65810000
         ST    R3,S99TXTPP                                              65820000
         ST    R5,0(,R3)                                                65830000
         LA    R5,CDSNTU_GD_L(,R5)                                      65840000
         ST    R5,4(,R3)                                                65850000
         LA    R5,CSTATUSTU_GD_L(,R5)                                   65860000
         O     R5,=X'80000000'                                          65870000
         ST    R5,8(,R3)                                                65880000
         LA    R1,DYNALLOC_AREA_GD                                      65890000
         DYNALLOC                                                       65900000
*                                                                       65910000
         DROP  R6                                                       65920000
         DROP  R4                                                       65930000
*                                                                       65940000
         LTR   R15,R15                                                  65950000
         IF (NZ) THEN                                                   65960000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'         65970000
            MVC   G_RETCODE,=F'8'                                       65980000
            BR    R8                                                    65990000
         ENDIF                                                          66000000
*                                                                       66010000
         L     R1,G_DCB_MEM_PTR                                         66020000
         MVC   DCBPDS_DIR-DCB_DSECT(LEN_DCBPDS_DIR_GD,R1),CDCBPDS_DIR_GX66030000
               D                                                        66040000
         MVC   DCBEPDS_DIR-DCB_DSECT(LEN_DCBEPDS_DIR_GD,R1),CDCBEPDS_DIX66050000
               R_GD                                                     66060000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             66070000
         LA    R3,DCBEPDS_DIR-DCB_DSECT(,R1)                            66080000
         ST    R3,DCBDCBE-IHADCB(,R2)                                   66090000
         LA    R4,PDSDIR_IS_EOF_GD                                      66100000
         ST    R4,DCBEEODA-DCBE(,R3)                                    66110000
         L     R6,DYNALLOC_AREA_GD                                      66120000
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_GD_L+CSTATUSTU_GD_L(,R6)   66130000
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)                              66140000
*                                                                       66150000
         MVI   PDSDIR_EOF_GD,C'N'                                       66160000
*                                                                       66170000
         LA    R6,GET_DATE_OPENPDS                                      66180000
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                      66190000
GET_DATE_OPENPDS EQU *                                                  66200000
*                                                                       66210000
         LTR   R15,R15                                                  66220000
         IF (NZ) THEN                                                   66230000
            MLWZMRPT RPTLINE=CL133'0OPEN failed for reading PDS directoX66240000
               ry'                                                      66250000
            MVC   G_RETCODE,=F'8'                                       66260000
            B     GET_DATE_DEALLOC                                      66270000
         ENDIF                                                          66280000
*                                                                       66290000
GET_DATE_GET_DIRREC EQU *                                               66300000
         L     R1,G_DCB_MEM_PTR                                         66310000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             66320000
         LA    R6,GET_DATE_DIRREC_NOMORE                                66330000
         GET   (R2),DIRREC_GD                                           66340000
*                                                                       66350000
         LA    R3,DIRREC_GD                                             66360000
         XR    R4,R4                                                    66370000
         LH    R4,0(,R3)                                                66380000
         C     R4,=F'14'                                                66390000
         BL    GET_DATE_DIRREC_END_OF_BLOCK                             66400000
         LA    R3,2(,R3)                                                66410000
         S     R4,=F'2'                                                 66420000
GET_DATE_NEXT_DIRREC_ENTRY EQU *                                        66430000
         CLC   0(8,R3),=8X'FF'                                          66440000
         BE    GET_DATE_DIRREC_NOMORE                                   66450000
         CLC   0(8,R3),MEM8_GD                                          66460000
         IF (EQ) THEN                                                   66470000
            MVI   G_DSFOUND,C'Y'                                        66480000
            L     R5,8(,R3)                                             66490000
            N     R5,=X'0000001F'                                       66500000
            SLL   R5,1                                                  66510000
            C     R5,=F'30'                                             66520000
            IF (NL) THEN                                                66530000
               MVI   CONVTOD_INAREA,X'00'                               66540000
               MVC   CONVTOD_INAREA+1(15),CONVTOD_INAREA                66550000
               MVC   CONVTOD_INAREA(2),24(R3)                           66560000
               MVC   CONVTOD_INAREA+2(1),15(R3)                         66570000
               MVC   CONVTOD_INAREA+8(4),20(R3)                         66580000
               MVI   CONVTOD_OUTAREA,X'00'                              66590000
               MVC   CONVTOD_OUTAREA+1(7),CONVTOD_OUTAREA               66600000
               MVI   STCKCONV_OUTAREA,X'00'                             66610000
               MVC   STCKCONV_OUTAREA+1(15),STCKCONV_OUTAREA            66620000
               CONVTOD CONVVAL=CONVTOD_INAREA,TODVAL=CONVTOD_OUTAREA,TIX66630000
               METYPE=DEC,DATETYPE=YYDDD,MF=(E,CONVTOD_PLIST)           66640000
               STCKCONV STCKVAL=CONVTOD_OUTAREA,CONVVAL=STCKCONV_OUTAREX66650000
               A,TIMETYPE=DEC,DATETYPE=YYYYMMDD,MF=(E,STCKCONV_PLIST)   66660000
               MVC   G_LWZMRPT_LINE,=CL133' ..................... MembeX66670000
               r found in PDS directory, last altered on'               66680000
               MVC   DATEWORK_DEC_1(4),STCKCONV_OUTAREA+8               66690000
               MVC   DATEWORK_DEC_1+4(3),STCKCONV_OUTAREA               66700000
               MVO   DATEWORK_DEC_2,DATEWORK_DEC_1(7)                   66710000
               MVN   DATEWORK_DEC_2+7(1),=X'0F'                         66720000
               UNPK  DATEWORK_ZON,DATEWORK_DEC_2                        66730000
               MVC   G_SAVE_ALTER_DATE,DATEWORK_ZON                     66740000
               MVC   G_LWZMRPT_LINE+70(19),=C'0000-00-00 00:00:00'      66750000
               MVC   G_LWZMRPT_LINE+70(4),DATEWORK_ZON+2                66760000
               MVC   G_LWZMRPT_LINE+75(2),DATEWORK_ZON+6                66770000
               MVC   G_LWZMRPT_LINE+78(2),DATEWORK_ZON+8                66780000
               MVC   G_LWZMRPT_LINE+81(2),DATEWORK_ZON+10               66790000
               MVC   G_LWZMRPT_LINE+84(2),DATEWORK_ZON+12               66800000
               MVC   G_LWZMRPT_LINE+87(2),DATEWORK_ZON+14               66810000
               L     R15,G_LWZMAKE_RPTA                                 66820000
               BASR  R14,R15                                            66830000
            ELSE                                                        66840000
               MLWZMRPT RPTLINE=CL133' ..................... Member fouX66850000
               nd in PDS directory but without statistics!!!'           66860000
            ENDIF                                                       66870000
            B     GET_DATE_DIRREC_NOMORE                                66880000
         ELSE                                                           66890000
            L     R5,8(,R3)                                             66900000
            N     R5,=X'0000001F'                                       66910000
            SLL   R5,1                                                  66920000
            LA    R3,12(,R3)                                            66930000
            S     R4,=F'12'                                             66940000
            AR    R3,R5                                                 66950000
            SR    R4,R5                                                 66960000
            BC    B'0010',GET_DATE_NEXT_DIRREC_ENTRY                    66970000
         ENDIF                                                          66980000
*                                                                       66990000
GET_DATE_DIRREC_END_OF_BLOCK EQU *                                      67000000
         B     GET_DATE_GET_DIRREC                                      67010000
*                                                                       67020000
GET_DATE_DIRREC_NOMORE EQU *                                            67030000
*                                                                       67040000
         L     R1,G_DCB_MEM_PTR                                         67050000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             67060000
         CLOSE ((R2)),MODE=31                                           67070000
*                                                                       67080000
GET_DATE_DEALLOC EQU *                                                  67090000
         LA    R6,DYNALLOC_AREA_GD                                      67100000
         USING S99RBP,R6                                                67110000
         LA    R4,S99RBPTR+4                                            67120000
         USING S99RB,R4                                                 67130000
         ST    R4,S99RBPTR                                              67140000
         OI    S99RBPTR,S99RBPND                                        67150000
         XC    S99RB(S99RBEND-S99RB),S99RB                              67160000
         MVI   S99RBLN,S99RBEND-S99RB                                   67170000
         MVI   S99VERB,S99VRBUN                                         67180000
         OI    S99FLG11,S99MSGL0                                        67190000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             67200000
         MVC   0(CDSNTU_GD_L,R5),CDSNTU_GD                              67210000
         MVC   6(44,R5),CSIFIELD_GD+(CSIFILTK-CSIFIELD_DSECT)           67220000
         LA    R3,S99RB+(S99RBEND-S99RB)                                67230000
         ST    R3,S99TXTPP                                              67240000
         O     R5,=X'80000000'                                          67250000
         ST    R5,0(,R3)                                                67260000
         LA    R1,DYNALLOC_AREA_GD                                      67270000
         DYNALLOC                                                       67280000
*                                                                       67290000
         LTR   R15,R15                                                  67300000
         IF (NZ) THEN                                                   67310000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'       67320000
            MVC   G_RETCODE,=F'8'                                       67330000
            BR    R8                                                    67340000
         ENDIF                                                          67350000
*                                                                       67360000
         IF (CLI,G_DSFOUND,NE,C'Y') THEN                                67370000
            MVC   G_SAVE_ALTER_DATE,=16X'FF'                            67380000
         ENDIF                                                          67390000
*                                                                       67400000
GET_DATE_STATS_RET EQU *                                                67410000
         BR    R8                                                       67420000
*                                                                       67430000
* EODAD for DCBPDS                                                      67440000
*                                                                       67450000
PDSDIR_IS_EOF_GD EQU *                                                  67460000
         MVI   PDSDIR_EOF_GD,C'Y'                                       67470000
         BR    R6                                                       67480000
*                                                                       67490000
         LTORG                                                          67500000
*                                                                       67510000
* Translate table for conversion to hex                                 67520000
                            DS    0F                                    67530000
GETDATE_HEXTAB              EQU   *-C'0'                                67540000
                            DC    C'0123456789ABCDEF'                   67550000
*                                                                       67560000
TRT_ALPHANAT DS    0F A-Z $ # @                                         67570000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        67580000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    67590000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    67600000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    67610000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    67620000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    67630000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    67640000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    67650000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    67660000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    67670000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    67680000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    67690000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    67700000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    67710000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    67720000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    67730000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F                    67740000
*                                                                       67750000
TRT_ALPHANUMNATDASH DS    0F A-Z 0-9 $ # @ -                            67760000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        67770000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    67780000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    67790000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    67800000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    67810000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    67820000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    67830000
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    67840000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    67850000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    67860000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    67870000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    67880000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    67890000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    67900000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    67910000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    67920000
         DC    X'00000000000000000000FFFFFFFFFFFF' F                    67930000
*                                                                       67940000
CONST_CSIFIELD_GD DS 0F                                                 67950000
         DC    CL44' '        CSIFILTK FILTER   KEY                     67960000
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS           67970000
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS            67980000
         DS    0CL16          CSIDTYPD ENTRY TYPES                      67990000
         DC    CL16'                ' CSIDTYPS                          68000000
         DS    0CL4           CSIOPTS  CSI OPTIONS                      68010000
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' ' 68020000
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' ' 68030000
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' ' 68040000
         DC    XL1'00'        CSIRESRV RESERVED                         68050000
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING      68060000
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES       68070000
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME                       68080000
CONST_CSIFIELD_GD_LEN EQU *-CONST_CSIFIELD_GD                           68090000
*                                                                       68100000
CDSNTU_GD                   DC    AL2(DALDSNAM)                         68110000
                            DC    X'0001'                               68120000
                            DC    X'002C'                               68130000
                            DC    CL44' '                               68140000
CDSNTU_GD_L                 EQU   *-CDSNTU_GD                           68150000
*                                                                       68160000
CSTATUSTU_GD                DC    AL2(DALSTATS)                         68170000
                            DC    X'0001'                               68180000
                            DC    X'0001'                               68190000
                            DC    X'08'                                 68200000
CSTATUSTU_GD_L              EQU   *-CSTATUSTU_GD                        68210000
*                                                                       68220000
CRETDDN_GD                  DC    AL2(DALRTDDN)                         68230000
                            DC    X'0001'                               68240000
                            DC    X'0008'                               68250000
                            DC    CL8' '                                68260000
CRETDDN_GD_L                EQU   *-CRETDDN_GD                          68270000
*                                                                       68280000
CDCBPDS_DIR_GD              DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVDX68290000
               =DA,DSORG=PS,RECFM=F,DCBE=CDCBEPDS_DIR_GD                68300000
LEN_DCBPDS_DIR_GD           EQU   *-CDCBPDS_DIR_GD                      68310000
CDCBEPDS_DIR_GD             DCBE  EODAD=0,RMODE31=BUFF                  68320000
LEN_DCBEPDS_DIR_GD          EQU   *-CDCBEPDS_DIR_GD                     68330000
*                                                                       68340000
CDCBPDS_BDR                 DCB   MACRF=R,DSORG=PO,RECFM=U              68350000
LEN_DCBPDS_BDR              EQU   *-CDCBPDS_BDR                         68360000
*                                                                       68370000
CONVTOD_L                   CONVTOD MF=L                                68380000
CONVTOD_L_SIZ               EQU   *-CONVTOD_L                           68390000
STCKCONV_L                  STCKCONV MF=L                               68400000
STCKCONV_L_SIZ              EQU   *-STCKCONV_L                          68410000
*                                                                       68420000
IDBBUF                      IEWBUFF FUNC=MAPBUF,TYPE=IDRB,VERSION=6,BYTX68430000
               ES=2048                                                  68440000
*                                                                       68450000
GET_DATE_DSECT              DSECT                                       68460000
                            DS    18F * My savearea                     68470000
*                                                                       68480000
DAREAPTR_GD                 DS    A      DATA AREA POINTER (64K)        68490000
*                                                                       68500000
MODRSNRT_GD                 DS    0F                                    68510000
PARMRC_GD                   DS    0CL4                                  68520000
MODID_GD                    DS    CL2    MODULE ID                      68530000
RSNCODE_GD                  DS    CL1    REASON CODE                    68540000
RTNCODE_GD                  DS    CL1    RETURN CODE                    68550000
*                                                                       68560000
CSIFIELD_GD                 DS    0F                                    68570000
                            ORG   *+CSIFIELD_DSECT_SIZ                  68580000
CSIFIELD_GD_LEN             EQU   *-CSIFIELD_GD                         68590000
*                                                                       68600000
PARMLIST_GD                 DS    0F                                    68610000
                            DS    A                                     68620000
                            DS    A                                     68630000
                            DS    A                                     68640000
*                                                                       68650000
                            DS    0F                                    68660000
CONVTOD_INAREA              DS    4F                                    68670000
CONVTOD_OUTAREA             DS    2F                                    68680000
CONVTOD_PLIST               DS    CL(CONVTOD_L_SIZ)                     68690000
*                                                                       68700000
                            DS    0F                                    68710000
STCKCONV_OUTAREA            DS    CL16                                  68720000
STCKCONV_PLIST              DS    CL(STCKCONV_L_SIZ)                    68730000
*                                                                       68740000
                            DS    0F                                    68750000
DATEWORK_DEC_1              DS    CL8                                   68760000
DATEWORK_DEC_2              DS    CL8                                   68770000
DATEWORK_ZON                DS    CL16                                  68780000
*                                                                       68790000
                            DS    0F                                    68800000
PDSDIR_EOF_GD               DS    C                                     68810000
*                                                                       68820000
                            DS    0F                                    68830000
DSCBPAR_GD                  DS    4F                                    68840000
OBTAIN_GD                   DS    0F                                    68850000
                            ORG   *+OBTAIN_DSECT_SIZ                    68860000
*                                                                       68870000
DYNALLOC_AREA_GD            DS    0F                                    68880000
                            ORG   *+4                                   68890000
                            ORG   *+(S99RBEND-S99RB)                    68900000
                            ORG   *+12                                  68910000
                            ORG   *+CDSNTU_GD_L+CSTATUSTU_GD_L+CRETDDN_X68920000
               GD_L                                                     68930000
*                                                                       68940000
                            DS    0F                                    68950000
DIRREC_GD                   DS    CL256                                 68960000
*                                                                       68970000
MEM8_GD                     DS    CL8                                   68980000
*                                                                       68990000
DAREA_GD                    DS    C                                     69000000
                            ORG   *+1023                                69010000
DAREA_GD_SIZ                EQU   *-DAREA_GD                            69020000
*                                                                       69030000
IEWBFDAT_SB_PAR4A           DS    4A                                    69040000
IEWBFDAT_SB_SB              DS    CL4                                   69050000
IEWBFDAT_SB_MTOKEN          DS    CL4                                   69060000
IEWBFDAT_SB_PGMNAME         DS    CL8                                   69070000
*                                                                       69080000
IEWBFDAT_GD_PAR8A           DS    8A                                    69090000
IEWBFDAT_GD_GD              DS    CL4                                   69100000
IEWBFDAT_GD_MTOKEN          DS    CL4                                   69110000
IEWBFDAT_GD_B_IDRB          DS    CL8                                   69120000
IEWBFDAT_GD_CURSOR          DS    F                                     69130000
IEWBFDAT_GD_COUNT           DS    F                                     69140000
*                                                                       69150000
IEWBFDAT_EN_PAR2A           DS    2A                                    69160000
IEWBFDAT_EN_EN              DS    CL4                                   69170000
IEWBFDAT_EN_MTOKEN          DS    CL4                                   69180000
*                                                                       69190000
GET_DATE_DSECT_SIZ          EQU   *-GET_DATE_DSECT                      69200000
*                                                                       69210000
         IEFZB4D0                                                       69220000
         IEFZB4D2                                                       69230000
*                                                                       69240000
LWZMAKE  CSECT                                                          69250000
*                                                                       69260000
* Get the member list of a data set                                     69270000
*                                                                       69280000
         DROP                                                           69290000
*                                                                       69300000
LWZMAKE_GET_MEMLIST DS    0F                                            69310000
         STM   R14,R12,12(R13)   * Save callers registers               69320000
         LR    R10,R15                                                  69330000
         LA    R11,4095(,R10)                                           69340000
         LA    R11,1(,R11)                                              69350000
         USING LWZMAKE_GET_MEMLIST,R10,R11                              69360000
         GETMAIN RU,LV=GET_MEMLIST_DSECT_SIZ                            69370000
         ST    R13,4(R1)         * Backward chain callers SA            69380000
         ST    R1,8(R13)         * Forward chain my SA                  69390000
         LR    R13,R1            * Point R13 to my SA                   69400000
         USING GET_MEMLIST_DSECT,R13 * Establish addressing of workarea 69410000
         USING GLOBAL_DATA_DSECT,R9                                     69420000
*                                                                       69430000
*        Trace record to start section                                  69440000
         MLWZMTRC LEVEL=LWZMAKE_TRACE_DEEBUG,MSGNR=C'604',CONST=C'LWZMAX69450000
               KE_GET_MEMLIST'                                          69460000
*                                                                       69470000
         L     R3,G_SCAN_TOKEN2A                                        69480000
         L     R4,G_SCAN_TOKEN2_LEN                                     69490000
         XR    R6,R6                                                    69500000
         XR    R7,R7                                                    69510000
*                                                                       69520000
GET_MEMLIST_TEST_QUAL1 EQU *                                            69530000
         LTR   R4,R4                                                    69540000
         BZ    GET_MEMLIST_NOT_MVSDS                                    69550000
         TRT   0(1,R3),TRT_ALPHANAT_MEMLIST                             69560000
         BNZ   GET_MEMLIST_NOT_MVSDS                                    69570000
         LA    R3,1(,R3)                                                69580000
         BCT   R4,*+8                                                   69590000
         B     GET_MEMLIST_MVSDS                                        69600000
         LR    R5,R4                                                    69610000
         C     R5,=F'7'                                                 69620000
         IF (H) THEN                                                    69630000
            L     R5,=F'7'                                              69640000
         ENDIF                                                          69650000
         BCTR  R5,R0                                                    69660000
         B     *+10                                                     69670000
         TRT   0(1,R3),TRT_ALPHANUMNATDASH_MEMLIST                      69680000
         EX    R5,*-6                                                   69690000
         IF (Z) THEN                                                    69700000
            LA    R5,1(,R5)                                             69710000
         ELSE                                                           69720000
            LR    R5,R1                                                 69730000
            SR    R5,R3                                                 69740000
         ENDIF                                                          69750000
         AR    R3,R5                                                    69760000
         SR    R4,R5                                                    69770000
         BZ    GET_MEMLIST_MVSDS                                        69780000
         IF (CLI,0(R3),EQ,C'.') THEN                                    69790000
            LA    R3,1(,R3)                                             69800000
            BCTR  R4,R0                                                 69810000
            B     GET_MEMLIST_TEST_QUAL1                                69820000
         ENDIF                                                          69830000
         B     GET_MEMLIST_NOT_MVSDS                                    69840000
*                                                                       69850000
GET_MEMLIST_MVSDS EQU *                                                 69860000
         BAL   R8,GET_MEMLIST_IGGCSI00                                  69870000
*                                                                       69880000
         CLC   G_RETCODE,=A(0)                                          69890000
         BNE   GET_MEMLIST_RET                                          69900000
*                                                                       69910000
         CLI   G_DSFOUND,C'Y'                                           69920000
         BNE   GET_MEMLIST_RET                                          69930000
*                                                                       69940000
         BAL   R8,GET_MEMLIST_OBTAIN                                    69950000
*                                                                       69960000
         CLC   G_RETCODE,=A(0)                                          69970000
         BNE   GET_MEMLIST_RET                                          69980000
*                                                                       69990000
         BAL   R8,GET_MEMLIST_MEMS                                      70000000
*                                                                       70010000
         B     GET_MEMLIST_RET                                          70020000
*                                                                       70030000
GET_MEMLIST_NOT_MVSDS EQU *                                             70040000
         MLWZMRPT RPTLINE=CL133'0Member list requested for non MVS fileX70050000
               '                                                        70060000
         MVC   G_RETCODE,=F'12'                                         70070000
*                                                                       70080000
GET_MEMLIST_RET EQU *                                                   70090000
         L     R3,4(,R13)        * Restore address of callers SA        70100000
         FREEMAIN RU,LV=GET_MEMLIST_DSECT_SIZ,A=(R13)                   70110000
         LR    R13,R3                                                   70120000
         LM    R14,R12,12(R13)                                          70130000
         BR    R14                    Return to caller                  70140000
*                                                                       70150000
* Perform catalog search with IGGCSI00                                  70160000
*                                                                       70170000
GET_MEMLIST_IGGCSI00 EQU *                                              70180000
         MVI   G_DSFOUND,C'N'                                           70190000
*                                                                       70200000
         LA    R1,DAREA_ML                                              70210000
         ST    R1,DAREAPTR_ML                                           70220000
         L     R2,=A(DAREA_ML_SIZ)                                      70230000
         ST    R2,0(,R1)                                                70240000
*                                                                       70250000
         LA    R2,CSIFIELD_ML                                           70260000
         L     R3,=A(CSIFIELD_ML_LEN)                                   70270000
         LA    R4,CONST_CSIFIELD_ML                                     70280000
         L     R5,=A(CONST_CSIFIELD_ML_LEN)                             70290000
         MVCL  R2,R4                                                    70300000
*                                                                       70310000
         LA    R2,CSIFIELD_ML+(CSIFILTK-CSIFIELD_DSECT)                 70320000
         L     R3,G_SCAN_TOKEN2A                                        70330000
         L     R4,G_SCAN_TOKEN2_LEN                                     70340000
         LT    R5,G_MVSDS_MEMBER_PTR                                    70350000
         IF (NZ) THEN                                                   70360000
            SR    R5,R3                                                 70370000
            BCTR  R5,R0                                                 70380000
            LR    R4,R5                                                 70390000
         ENDIF                                                          70400000
         C     R4,=A(L'CSIFILTK)                                        70410000
         IF (H)                                                         70420000
            L     R4,=A(L'CSIFILTK)                                     70430000
         ENDIF                                                          70440000
         BCTR  R4,R0                                                    70450000
         B     *+10                                                     70460000
         MVC   0(1,R2),0(R3)                                            70470000
         EX    R4,*-6                                                   70480000
*                                                                       70490000
         LA    R1,PARMLIST_ML                                           70500000
         LA    R2,MODRSNRT_ML                                           70510000
         ST    R2,0(R1)                                                 70520000
         LA    R2,CSIFIELD_ML                                           70530000
         ST    R2,4(R1)                                                 70540000
         L     R2,DAREAPTR_ML                                           70550000
         O     R2,=X'80000000'                                          70560000
         ST    R2,8(R1)                                                 70570000
*                                                                       70580000
         L     R15,G_IGGCSI00A                                          70590000
         BASR  R14,R15                                                  70600000
*                                                                       70610000
         C     R15,=F'4'                                                70620000
         IF (H) THEN                                                    70630000
            MLWZMRPT RPTLINE=CL133'0Catalog search interface returned eX70640000
               rror code'                                               70650000
            MVC   G_RETCODE,=F'12'                                      70660000
            BR    R8                                                    70670000
         ENDIF                                                          70680000
*                                                                       70690000
         LA    R1,DAREA_ML                                              70700000
         CLC   8(4,R1),=F'64'                                           70710000
         IF (H) THEN                                                    70720000
            MVI   G_DSFOUND,C'Y'                                        70730000
         ENDIF                                                          70740000
*                                                                       70750000
GET_MEMLIST_IGGCSI00_RET EQU *                                          70760000
         BR    R8                                                       70770000
*                                                                       70780000
* Perform CAMLST OBTAIN                                                 70790000
*                                                                       70800000
GET_MEMLIST_OBTAIN EQU *                                                70810000
         XR    R1,R1                                                    70820000
         ICM   R1,B'1000',=AL1(193)                                     70830000
         ST    R1,DSCBPAR_ML                                            70840000
         MVC   OBTAIN_ML+(DS1DSNAM-OBTAIN_DSECT)(L'DS1DSNAM),CSIFIELD_MX70850000
               L+(CSIFILTK-CSIFIELD_DSECT)                              70860000
         LA    R1,OBTAIN_ML+(DS1DSNAM-OBTAIN_DSECT)                     70870000
         ST    R1,DSCBPAR_ML+4                                          70880000
         LA    R1,DAREA_ML+110                                          70890000
         CLC   0(2,R1),=H'12'   * Is volume name present                70900000
         BL    GET_MEMLIST_OBTAIN_RET                                   70910000
         LA    R1,6(,R1)                                                70920000
         ST    R1,DSCBPAR_ML+8                                          70930000
         LA    R1,OBTAIN_ML+(DS1FMTID-OBTAIN_DSECT)                     70940000
         ST    R1,DSCBPAR_ML+12                                         70950000
*                                                                       70960000
         OBTAIN DSCBPAR_ML                                              70970000
*                                                                       70980000
         LTR   R15,R15                                                  70990000
         IF (NZ) THEN                                                   71000000
            MLWZMRPT RPTLINE=CL133'0CAMLST OBTAIN returned error code'  71010000
            MVC   G_RETCODE,=F'12'                                      71020000
            BR    R8                                                    71030000
         ENDIF                                                          71040000
*                                                                       71050000
         IF (TM,OBTAIN_ML+(DS1DSORG-OBTAIN_DSECT),DS1DSGPO,Z) THEN      71060000
            MVC   G_LWZMRPT_LINE,=CL133'0Member list requested on non-PX71070000
               DS dataset'                                              71080000
            LA    R2,CSIFIELD_ML                                        71090000
            MVC   G_LWZMRPT_LINE+37(L'CSIFILTK),CSIFILTK-CSIFIELD_DSECTX71100000
               (R2)                                                     71110000
            L     R15,G_LWZMAKE_RPTA                                    71120000
            BASR  R14,R15                                               71130000
            MVC   G_RETCODE,=F'8'                                       71140000
            BR    R8                                                    71150000
         ENDIF                                                          71160000
*                                                                       71170000
GET_MEMLIST_OBTAIN_RET EQU *                                            71180000
         BR    R8                                                       71190000
*                                                                       71200000
* Get the member list                                                   71210000
*                                                                       71220000
GET_MEMLIST_MEMS EQU *                                                  71230000
         LA    R6,DYNALLOC_AREA_ML                                      71240000
         USING S99RBP,R6                                                71250000
         LA    R4,S99RBPTR+4                                            71260000
         USING S99RB,R4                                                 71270000
         ST    R4,S99RBPTR                                              71280000
         OI    S99RBPTR,S99RBPND                                        71290000
         XC    S99RB(S99RBEND-S99RB),S99RB                              71300000
         MVI   S99RBLN,S99RBEND-S99RB                                   71310000
         MVI   S99VERB,S99VRBAL                                         71320000
         OI    S99FLG11,S99MSGL0                                        71330000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             71340000
         MVC   0(CDSNTU_ML_L+CSTATUSTU_ML_L+CRETDDN_ML_L,R5),CDSNTU_ML  71350000
         MVC   6(44,R5),CSIFIELD_ML+(CSIFILTK-CSIFIELD_DSECT)           71360000
         LA    R3,S99RB+(S99RBEND-S99RB)                                71370000
         ST    R3,S99TXTPP                                              71380000
         ST    R5,0(,R3)                                                71390000
         LA    R5,CDSNTU_ML_L(,R5)                                      71400000
         ST    R5,4(,R3)                                                71410000
         LA    R5,CSTATUSTU_ML_L(,R5)                                   71420000
         O     R5,=X'80000000'                                          71430000
         ST    R5,8(,R3)                                                71440000
         LA    R1,DYNALLOC_AREA_ML                                      71450000
         DYNALLOC                                                       71460000
*                                                                       71470000
         DROP  R6                                                       71480000
         DROP  R4                                                       71490000
*                                                                       71500000
         LTR   R15,R15                                                  71510000
         IF (NZ) THEN                                                   71520000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC allocation failed'         71530000
            MVC   G_RETCODE,=F'8'                                       71540000
            BR    R8                                                    71550000
         ENDIF                                                          71560000
*                                                                       71570000
         L     R1,G_DCB_MEM_PTR                                         71580000
         MVC   DCBPDS_DIR-DCB_DSECT(LEN_DCBPDS_DIR_ML,R1),CDCBPDS_DIR_MX71590000
               L                                                        71600000
         MVC   DCBEPDS_DIR-DCB_DSECT(LEN_DCBEPDS_DIR_ML,R1),CDCBEPDS_DIX71610000
               R_ML                                                     71620000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             71630000
         LA    R3,DCBEPDS_DIR-DCB_DSECT(,R1)                            71640000
         ST    R3,DCBDCBE-IHADCB(,R2)                                   71650000
         LA    R4,PDSDIR_IS_EOF_ML                                      71660000
         ST    R4,DCBEEODA-DCBE(,R3)                                    71670000
         L     R6,DYNALLOC_AREA_ML                                      71680000
         LA    R6,(S99RBEND-S99RB)+12+CDSNTU_ML_L+CSTATUSTU_ML_L(,R6)   71690000
         MVC   DCBDDNAM-IHADCB(8,R2),6(R6)                              71700000
*                                                                       71710000
         MVI   PDSDIR_EOF_ML,C'N'                                       71720000
*                                                                       71730000
         LA    R6,GET_MEMLIST_OPENPDS                                   71740000
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                      71750000
GET_MEMLIST_OPENPDS EQU *                                               71760000
*                                                                       71770000
         LTR   R15,R15                                                  71780000
         IF (NZ) THEN                                                   71790000
            MLWZMRPT RPTLINE=CL133'0OPEN failed for reading PDS directoX71800000
               ry'                                                      71810000
            MVC   G_RETCODE,=F'8'                                       71820000
            B     GET_MEMLIST_DEALLOC                                   71830000
         ENDIF                                                          71840000
*                                                                       71850000
GET_MEMLIST_GET_DIRREC EQU *                                            71860000
         L     R1,G_DCB_MEM_PTR                                         71870000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             71880000
         LA    R6,GET_MEMLIST_DIRREC_NOMORE                             71890000
         GET   (R2),DIRREC_ML                                           71900000
*                                                                       71910000
         LA    R3,DIRREC_ML                                             71920000
         XR    R4,R4                                                    71930000
         LH    R4,0(,R3)                                                71940000
         C     R4,=F'14'                                                71950000
         BL    GET_MEMLIST_DIRREC_END_OF_BLOCK                          71960000
         LA    R3,2(,R3)                                                71970000
         S     R4,=F'2'                                                 71980000
GET_MEMLIST_NEXT_DIRREC_ENTRY EQU *                                     71990000
         CLC   0(8,R3),=8X'FF'                                          72000000
         BE    GET_MEMLIST_DIRREC_NOMORE                                72010000
         L     R5,G_SCAN_TOKEN3A                                        72020000
         LT    R6,G_SCAN_TOKEN3_LEN                                     72030000
         IF (NZ) THEN                                                   72040000
            AR    R5,R6                                                 72050000
            MVI   0(R5),C' '                                            72060000
            LA    R5,1(,R5)                                             72070000
            LA    R6,1(,R6)                                             72080000
            ST    R6,G_SCAN_TOKEN3_LEN                                  72090000
         ENDIF                                                          72100000
         LR    R1,R3                                                    72110000
         LA    R14,8                                                    72120000
GET_MEMLIST_MEM_CHAR EQU *                                              72130000
         CLI   0(R1),C' '                                               72140000
         BE    GET_MEMLIST_MEM_DONE                                     72150000
         MVC   0(1,R5),0(R1)                                            72160000
         LA    R5,1(,R5)                                                72170000
         LA    R6,1(,R6)                                                72180000
         LA    R1,1(,R1)                                                72190000
         BCT   R14,GET_MEMLIST_MEM_CHAR                                 72200000
GET_MEMLIST_MEM_DONE EQU *                                              72210000
         ST    R6,G_SCAN_TOKEN3_LEN                                     72220000
         L     R5,8(,R3)                                                72230000
         N     R5,=X'0000001F'                                          72240000
         SLL   R5,1                                                     72250000
         LA    R3,12(,R3)                                               72260000
         S     R4,=F'12'                                                72270000
         AR    R3,R5                                                    72280000
         SR    R4,R5                                                    72290000
         BC    B'0010',GET_MEMLIST_NEXT_DIRREC_ENTRY                    72300000
*                                                                       72310000
GET_MEMLIST_DIRREC_END_OF_BLOCK EQU *                                   72320000
         B     GET_MEMLIST_GET_DIRREC                                   72330000
*                                                                       72340000
GET_MEMLIST_DIRREC_NOMORE EQU *                                         72350000
*                                                                       72360000
         L     R1,G_DCB_MEM_PTR                                         72370000
         LA    R2,DCBPDS_DIR-DCB_DSECT(,R1)                             72380000
         CLOSE ((R2)),MODE=31                                           72390000
*                                                                       72400000
GET_MEMLIST_DEALLOC EQU *                                               72410000
         LA    R6,DYNALLOC_AREA_ML                                      72420000
         USING S99RBP,R6                                                72430000
         LA    R4,S99RBPTR+4                                            72440000
         USING S99RB,R4                                                 72450000
         ST    R4,S99RBPTR                                              72460000
         OI    S99RBPTR,S99RBPND                                        72470000
         XC    S99RB(S99RBEND-S99RB),S99RB                              72480000
         MVI   S99RBLN,S99RBEND-S99RB                                   72490000
         MVI   S99VERB,S99VRBUN                                         72500000
         OI    S99FLG11,S99MSGL0                                        72510000
         LA    R5,S99RB+(S99RBEND-S99RB)+12                             72520000
         MVC   0(CDSNTU_ML_L,R5),CDSNTU_ML                              72530000
         LA    R2,CSIFIELD_ML                                           72540000
         MVC   6(44,R5),CSIFILTK-CSIFIELD_DSECT(R2)                     72550000
         LA    R3,S99RB+(S99RBEND-S99RB)                                72560000
         ST    R3,S99TXTPP                                              72570000
         O     R5,=X'80000000'                                          72580000
         ST    R5,0(,R3)                                                72590000
         LA    R1,DYNALLOC_AREA_ML                                      72600000
         DYNALLOC                                                       72610000
*                                                                       72620000
         LTR   R15,R15                                                  72630000
         IF (NZ) THEN                                                   72640000
            MLWZMRPT RPTLINE=CL133'0DYNALLOC deallocation failed'       72650000
            MVC   G_RETCODE,=F'8'                                       72660000
            BR    R8                                                    72670000
         ENDIF                                                          72680000
*                                                                       72690000
GET_MEMLIST_MEMS_RET EQU *                                              72700000
         BR    R8                                                       72710000
*                                                                       72720000
* EODAD for DCBPDS                                                      72730000
*                                                                       72740000
PDSDIR_IS_EOF_ML EQU *                                                  72750000
         MVI   PDSDIR_EOF_ML,C'Y'                                       72760000
         BR    R6                                                       72770000
*                                                                       72780000
         LTORG                                                          72790000
*                                                                       72800000
TRT_ALPHANAT_MEMLIST DS    0F A-Z $ # @                                 72810000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        72820000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    72830000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    72840000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    72850000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    72860000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    72870000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    72880000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    72890000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    72900000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    72910000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    72920000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    72930000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    72940000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    72950000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    72960000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    72970000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F                    72980000
*                                                                       72990000
TRT_ALPHANUMNATDASH_MEMLIST DS    0F A-Z 0-9 $ # @ -                    73000000
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        73010000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0                    73020000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1                    73030000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2                    73040000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3                    73050000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4                    73060000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 5                    73070000
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6                    73080000
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 7                    73090000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8                    73100000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9                    73110000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A                    73120000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B                    73130000
         DC    X'FF000000000000000000FFFFFFFFFFFF' C                    73140000
         DC    X'FF000000000000000000FFFFFFFFFFFF' D                    73150000
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E                    73160000
         DC    X'00000000000000000000FFFFFFFFFFFF' F                    73170000
*                                                                       73180000
CONST_CSIFIELD_ML DS 0F                                                 73190000
         DC    CL44' '        CSIFILTK FILTER   KEY                     73200000
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS           73210000
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS            73220000
         DS    0CL16          CSIDTYPD ENTRY TYPES                      73230000
         DC    CL16'                ' CSIDTYPS                          73240000
         DS    0CL4           CSIOPTS  CSI OPTIONS                      73250000
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' ' 73260000
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' ' 73270000
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' ' 73280000
         DC    XL1'00'        CSIRESRV RESERVED                         73290000
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING      73300000
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES       73310000
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME                       73320000
CONST_CSIFIELD_ML_LEN EQU *-CONST_CSIFIELD_ML                           73330000
*                                                                       73340000
CDSNTU_ML                   DC    AL2(DALDSNAM)                         73350000
                            DC    X'0001'                               73360000
                            DC    X'002C'                               73370000
                            DC    CL44' '                               73380000
CDSNTU_ML_L                 EQU   *-CDSNTU_ML                           73390000
*                                                                       73400000
CSTATUSTU_ML                DC    AL2(DALSTATS)                         73410000
                            DC    X'0001'                               73420000
                            DC    X'0001'                               73430000
                            DC    X'08'                                 73440000
CSTATUSTU_ML_L              EQU   *-CSTATUSTU_ML                        73450000
*                                                                       73460000
CRETDDN_ML                  DC    AL2(DALRTDDN)                         73470000
                            DC    X'0001'                               73480000
                            DC    X'0008'                               73490000
                            DC    CL8' '                                73500000
CRETDDN_ML_L                EQU   *-CRETDDN_ML                          73510000
*                                                                       73520000
CDCBPDS_DIR_ML              DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVDX73530000
               =DA,DSORG=PS,RECFM=F,DCBE=CDCBEPDS_DIR_ML                73540000
LEN_DCBPDS_DIR_ML           EQU   *-CDCBPDS_DIR_ML                      73550000
CDCBEPDS_DIR_ML             DCBE  EODAD=0,RMODE31=BUFF                  73560000
LEN_DCBEPDS_DIR_ML          EQU   *-CDCBEPDS_DIR_ML                     73570000
*                                                                       73580000
GET_MEMLIST_DSECT           DSECT                                       73590000
                            DS    18F * My savearea                     73600000
*                                                                       73610000
DAREAPTR_ML                 DS    A      DATA AREA POINTER (64K)        73620000
*                                                                       73630000
MODRSNRT_ML                 DS    0F                                    73640000
PARMRC_ML                   DS    0CL4                                  73650000
MODID_ML                    DS    CL2    MODULE ID                      73660000
RSNCODE_ML                  DS    CL1    REASON CODE                    73670000
RTNCODE_ML                  DS    CL1    RETURN CODE                    73680000
*                                                                       73690000
CSIFIELD_ML                 DS    0F                                    73700000
                            ORG   *+CSIFIELD_DSECT_SIZ                  73710000
CSIFIELD_ML_LEN             EQU   *-CSIFIELD_ML                         73720000
*                                                                       73730000
PARMLIST_ML                 DS    0F                                    73740000
                            DS    A                                     73750000
                            DS    A                                     73760000
                            DS    A                                     73770000
*                                                                       73780000
                            DS    0F                                    73790000
PDSDIR_EOF_ML               DS    C                                     73800000
*                                                                       73810000
                            DS    0F                                    73820000
DSCBPAR_ML                  DS    4F                                    73830000
OBTAIN_ML                   DS    0F                                    73840000
                            ORG   *+OBTAIN_DSECT_SIZ                    73850000
*                                                                       73860000
DYNALLOC_AREA_ML            DS    0F                                    73870000
                            ORG   *+4                                   73880000
                            ORG   *+(S99RBEND-S99RB)                    73890000
                            ORG   *+12                                  73900000
                            ORG   *+CDSNTU_ML_L+CSTATUSTU_ML_L+CRETDDN_X73910000
               ML_L                                                     73920000
*                                                                       73930000
                            DS    0F                                    73940000
DIRREC_ML                   DS    CL256                                 73950000
*                                                                       73960000
MEM8_ML                     DS    CL8                                   73970000
*                                                                       73980000
DAREA_ML                    DS    C                                     73990000
                            ORG   *+1023                                74000000
DAREA_ML_SIZ                EQU   *-DAREA_ML                            74010000
*                                                                       74020000
GET_MEMLIST_DSECT_SIZ       EQU   *-GET_MEMLIST_DSECT                   74030000
*                                                                       74040000
R0       EQU   0                                                        74050000
R1       EQU   1                                                        74060000
R2       EQU   2                                                        74070000
R3       EQU   3                                                        74080000
R4       EQU   4                                                        74090000
R5       EQU   5                                                        74100000
R6       EQU   6                                                        74110000
R7       EQU   7                                                        74120000
R8       EQU   8                                                        74130000
R9       EQU   9                                                        74140000
R10      EQU   10                                                       74150000
R11      EQU   11                                                       74160000
R12      EQU   12                                                       74170000
R13      EQU   13                                                       74180000
R14      EQU   14                                                       74190000
R15      EQU   15                                                       74200000
*                                                                       74210000
         END   LWZMAKE                                                  74220000
