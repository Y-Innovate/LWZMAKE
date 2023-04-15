*
* IST5 obj's Vtbl
*
IST5_Vtbl                    DSECT
IST5_QueryInterfacePtr       DS    A   * Regular
IST5_AddRefPtr               DS    A   *   COM
IST5_ReleasePtr              DS    A   *     Methods
IST5_InitPtr                 DS    A   * Initialize rule statement
                             DS    0F
IST5_Vtbl_SIZ                EQU   *-IST5_Vtbl
*
         MACRO
         IST5_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST5_QueryInterfacePtr-IST5_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IST5_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST5_AddRefPtr-IST5_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IST5_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST5_ReleasePtr-IST5_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IST5_Init &OBJECT=,&WORK=,&ITFOTGTS=,&ITFOPREQ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFOTGTS * Set ITFO Targets as parm 2
         MVC   8(4,R1),&ITFOPREQ * Set ITFO Prereqs as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IST5_InitPtr-IST5_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
