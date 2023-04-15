*
* IST6 obj's Vtbl
*
IST6_Vtbl                    DSECT
IST6_QueryInterfacePtr       DS    A   * Regular
IST6_AddRefPtr               DS    A   *   COM
IST6_ReleasePtr              DS    A   *     Methods
IST6_InitPtr                 DS    A   * Initialize Sh Statement
                             DS    0F
IST6_Vtbl_SIZ                EQU   *-IST6_Vtbl
*
         MACRO
         IST6_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST6_QueryInterfacePtr-IST6_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST6_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST6_AddRefPtr-IST6_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST6_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST6_ReleasePtr-IST6_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST6_Init &OBJECT=,&WORK=,&ITFOSH=,&RECIPESTMT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFOSH * Set ITFO sh as parm 2
         XR    R14,R14      * Clear R14
         IC    R14,&RECIPESTMT * Insert flag recipeStatement
         ST    R14,8(,R1)   * Set flag recipeStatement as parm 35
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST6_InitPtr-IST6_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
