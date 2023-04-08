*
* REX obj's Vtbl
*
IREX_Vtbl                    DSECT
IREX_QueryInterfacePtr       DS    A   * Regular
IREX_AddRefPtr               DS    A   *   COM
IREX_ReleasePtr              DS    A   *     Methods
IREX_ExecPtr                 DS    A   * Execute a REXX
                             DS    0F
IREX_Vtbl_SIZ                EQU   *-IREX_Vtbl
*
         MACRO
         IREX_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IREX_QueryInterfacePtr-IREX_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IREX_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IREX_AddRefPtr-IREX_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IREX_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IREX_ReleasePtr-IREX_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IREX_Exec &OBJECT=,&WORK=,&ISTBEXEC=,&ISTBPTR_RETVAL=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ISTBEXEC * Set ptr to EXEC cmd ISTR as parm 2
         MVC   8(4,R1),&ISTBPTR_RETVAL * Set ptr to ISTR ret val parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IREX_ExecPtr-IREX_Vtbl(,R15)
         BASR  R14,R15      * Branch to Exec entry point
         MEND
