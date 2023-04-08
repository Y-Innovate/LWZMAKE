*
* IST4 obj's Vtbl
*
IST4_Vtbl                    DSECT
IST4_QueryInterfacePtr       DS    A   * Regular
IST4_AddRefPtr               DS    A   *   COM
IST4_ReleasePtr              DS    A   *     Methods
IST4_InitPtr                 DS    A   * Initialize Recipepref stmt
                             DS    0F
IST4_Vtbl_SIZ                EQU   *-IST4_Vtbl
*
         MACRO
         IST4_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST4_QueryInterfacePtr-IST4_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST4_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST4_AddRefPtr-IST4_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST4_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST4_ReleasePtr-IST4_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST4_Init &OBJECT=,&WORK=,&ITFORECIP=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFORECIP * Set ITFO Rec.prf as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST4_InitPtr-IST4_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
