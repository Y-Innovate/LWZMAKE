*
* IST3 obj's Vtbl
*
IST3_Vtbl                    DSECT
IST3_QueryInterfacePtr       DS    A   * Regular
IST3_AddRefPtr               DS    A   *   COM
IST3_ReleasePtr              DS    A   *     Methods
IST3_InitPtr                 DS    A   * Initialize Phony statement
                             DS    0F
IST3_Vtbl_SIZ                EQU   *-IST3_Vtbl
*
         MACRO
         IST3_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST3_QueryInterfacePtr-IST3_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST3_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST3_AddRefPtr-IST3_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST3_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST3_ReleasePtr-IST3_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST3_Init &OBJECT=,&WORK=,&ITFOPHONY=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFOPHONY * Set ITFO Phony as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST3_InitPtr-IST3_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
