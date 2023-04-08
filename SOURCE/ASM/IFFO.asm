*
* FFO obj's Vtbl
*
IFFO_Vtbl                    DSECT
IFFO_QueryInterfacePtr       DS    A   * Regular
IFFO_AddRefPtr               DS    A   *   COM
IFFO_ReleasePtr              DS    A   *     Methods
                             DS    0F
IFFO_Vtbl_SIZ                EQU   *-IFFO_Vtbl
*
         MACRO
         IFFO_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFFO_QueryInterfacePtr-IFFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IFFO_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFFO_AddRefPtr-IFFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IFFO_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFFO_ReleasePtr-IFFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
