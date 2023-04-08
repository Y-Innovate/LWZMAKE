*
* IST7 obj's Vtbl
*
IST7_Vtbl                    DSECT
IST7_QueryInterfacePtr       DS    A   * Regular
IST7_AddRefPtr               DS    A   *   COM
IST7_ReleasePtr              DS    A   *     Methods
IST7_InitPtr                 DS    A   * Initialize Recipepref stmt
                             DS    0F
IST7_Vtbl_SIZ                EQU   *-IST7_Vtbl
*
         MACRO
         IST7_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST7_QueryInterfacePtr-IST7_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST7_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST7_AddRefPtr-IST7_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST7_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST7_ReleasePtr-IST7_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST7_Init &OBJECT=,&WORK=,&ITFOHOME=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFOHOME * Set ITFO USSHOME as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST7_InitPtr-IST7_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
