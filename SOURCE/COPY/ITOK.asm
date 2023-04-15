*
* TOK obj's Vtbl
*
ITOK_Vtbl                    DSECT
ITOK_QueryInterfacePtr       DS    A   * Regular
ITOK_AddRefPtr               DS    A   *   COM
ITOK_ReleasePtr              DS    A   *     Methods
ITOK_InitPtr                 DS    A   * Initialize tokenizer
ITOK_GetNextTokenPtr         DS    A   * Get next token
                             DS    0F
ITOK_Vtbl_SIZ                EQU   *-ITOK_Vtbl
*
         MACRO
         ITOK_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITOK_QueryInterfacePtr-ITOK_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         ITOK_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITOK_AddRefPtr-ITOK_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         ITOK_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITOK_ReleasePtr-ITOK_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         ITOK_Init &OBJECT=,&WORK=,&IIN=,&IIFO=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IIN * Set IIN ptr as parm 2
         MVC   8(4,R1),&IIFO * Set IIFO ptr as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITOK_InitPtr-ITOK_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
*
         MACRO
         ITOK_GetNextToken &OBJECT=,&WORK=,&IPSS=,&ITFOPTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IPSS * Set IPSS ptr as parm 2
         LA    R14,&ITFOPTR * Get ITFO ptr ptr
         ST    R14,8(,R1)   * Set ITFO ptr ptr as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITOK_GetNextTokenPtr-ITOK_Vtbl(,R15)
         BASR  R14,R15      * Branch to GetNextToken entry point
         MEND
