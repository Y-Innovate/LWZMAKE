*
* INS obj's Vtbl
*
IINS_Vtbl                    DSECT
IINS_QueryInterfacePtr       DS    A   * Regular
IINS_AddRefPtr               DS    A   *   COM
IINS_ReleasePtr              DS    A   *     Methods
IINS_GetNextCharPtr          DS    A   * Get next character
IINS_InitPtr                 DS    A   * Initialize INS
                             DS    0F
IINS_Vtbl_SIZ                EQU   *-IINS_Vtbl
*
         MACRO
         IINS_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IINS_QueryInterfacePtr-IINS_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IINS_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IINS_AddRefPtr-IINS_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IINS_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IINS_ReleasePtr-IINS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IINS_GetNextChar &OBJECT=,&WORK=,&IIFO=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IIFO * Set IIFO ptr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IINS_GetNextCharPtr-IINS_Vtbl(,R15)
         BASR  R14,R15      * Branch to GetNextChar entry point
         MEND
*
         MACRO
         IINS_Init &OBJECT=,&WORK=,&ZSTR=,&LINENO=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ZSTR * Set ptr to zero term string as parm 2
         MVC   8(4,R1),&LINENO * Set line number as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IINS_InitPtr-IINS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
