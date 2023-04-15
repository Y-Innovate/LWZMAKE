*
* LOG obj's Vtbl
*
ILOG_Vtbl                    DSECT
ILOG_QueryInterfacePtr       DS    A   * Regular
ILOG_AddRefPtr               DS    A   *   COM
ILOG_ReleasePtr              DS    A   *     Methods
ILOG_OpenPtr                 DS    A   * Open log file
ILOG_ClosePtr                DS    A   * Close log file
ILOG_WritePtr                DS    A   * Write to log file
ILOG_SetLogLevelPtr          DS    A   * Set the log level
                             DS    0F
ILOG_Vtbl_SIZ                EQU   *-ILOG_Vtbl
*
         MACRO
         ILOG_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_QueryInterfacePtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         ILOG_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_AddRefPtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         ILOG_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_ReleasePtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         ILOG_Open &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_OpenPtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Open entry point
         MEND
*
         MACRO
         ILOG_Close &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_ClosePtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Close entry point
         MEND
*
         MACRO
         ILOG_Write &OBJECT=,&WORK=,&LINE=,&LOGLEVEL=3
         L     R15,&OBJECT  * Ptr to Vtbl
         CLI   13(R15),&LOGLEVEL * Dirty shortcut to skip
         BL    L&SYSNDX     * Skip if cLogLevel is less
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&LINE    * Get ptr to line to write
         ST    R14,4(,R1)   * Set ptr to line to write as parm 2
         MVC   8(4,R1),=A(&LOGLEVEL) * Set fword log level parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_WritePtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Write entry point
L&SYSNDX EQU   *
         MEND
*
         MACRO
         ILOG_SetLogLevel &OBJECT=,&WORK=,&LOGLEVEL=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&LOGLEVEL * Set fword log level parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ILOG_SetLogLevelPtr-ILOG_Vtbl(,R15)
         BASR  R14,R15      * Branch to SetLogLevel entry point
         MEND
*
* Log levels
*
LOG_LEVEL_NONE               EQU   0
LOG_LEVEL_ERROR              EQU   1
LOG_LEVEL_WARNING            EQU   2
LOG_LEVEL_INFO               EQU   3
LOG_LEVEL_DEBUG              EQU   4
LOG_LEVEL_DEBUG2             EQU   5
LOG_LEVEL_DEBUG3             EQU   6
