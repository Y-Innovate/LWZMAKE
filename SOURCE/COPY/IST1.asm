*
* IST1 obj's Vtbl
*
IST1_Vtbl                    DSECT
IST1_QueryInterfacePtr       DS    A   * Regular
IST1_AddRefPtr               DS    A   *   COM
IST1_ReleasePtr              DS    A   *     Methods
IST1_InitPtr                 DS    A   * Initialize Assignment Stmt
                             DS    0F
IST1_Vtbl_SIZ                EQU   *-IST1_Vtbl
*
         MACRO
         IST1_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST1_QueryInterfacePtr-IST1_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST1_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST1_AddRefPtr-IST1_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST1_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST1_ReleasePtr-IST1_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST1_Init &OBJECT=,&WORK=,&ITFONAME=,&ITFOOPER=,&ITFOVALUE=,  X
               &RECIPESTMT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFONAME * Set ITFO Name as parm 2
         MVC   8(4,R1),&ITFOOPER * Set ITFO Operator as parm 3
         MVC   12(4,R1),&ITFOVALUE * Set ITFO Value as parm 4
         XR    R14,R14      * Clear R14
         IC    R14,&RECIPESTMT * Insert flag recipeStatement
         ST    R14,16(,R1)  * Set flag recipeStatement as parm 5
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST1_InitPtr-IST1_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
