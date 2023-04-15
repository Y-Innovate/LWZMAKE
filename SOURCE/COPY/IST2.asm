*
* IST2 obj's Vtbl
*
IST2_Vtbl                    DSECT
IST2_QueryInterfacePtr       DS    A   * Regular
IST2_AddRefPtr               DS    A   *   COM
IST2_ReleasePtr              DS    A   *     Methods
IST2_InitPtr                 DS    A   * Initialize Call Statement
                             DS    0F
IST2_Vtbl_SIZ                EQU   *-IST2_Vtbl
*
         MACRO
         IST2_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST2_QueryInterfacePtr-IST2_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST2_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST2_AddRefPtr-IST2_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST2_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST2_ReleasePtr-IST2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST2_Init &OBJECT=,&WORK=,&ITFOCALL=,&RECIPESTMT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFOCALL * Set ITFO Call as parm 2
         XR    R14,R14      * Clear R14
         IC    R14,&RECIPESTMT * Insert flag recipeStatement
         ST    R14,8(,R1)   * Set flag recipeStatement as parm 35
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST2_InitPtr-IST2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
