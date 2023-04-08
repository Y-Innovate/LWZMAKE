*
* IST8 obj's Vtbl
*
IST8_Vtbl                    DSECT
IST8_QueryInterfacePtr       DS    A   * Regular
IST8_AddRefPtr               DS    A   *   COM
IST8_ReleasePtr              DS    A   *     Methods
IST8_InitPtr                 DS    A   * Initialize BuildWhen stmt
                             DS    0F
IST8_Vtbl_SIZ                EQU   *-IST8_Vtbl
*
         MACRO
         IST8_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST8_QueryInterfacePtr-IST8_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST8_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST8_AddRefPtr-IST8_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST8_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST8_ReleasePtr-IST8_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST8_Init &OBJECT=,&WORK=,&ITFOBUILDWHEN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFOBUILDWHEN * Set ITFO BUILDWHEN as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST8_InitPtr-IST8_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
