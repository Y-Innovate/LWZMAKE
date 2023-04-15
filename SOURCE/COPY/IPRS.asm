*
* PRS obj's Vtbl
*
IPRS_Vtbl                    DSECT
IPRS_QueryInterfacePtr       DS    A   * Regular
IPRS_AddRefPtr               DS    A   *   COM
IPRS_ReleasePtr              DS    A   *     Methods
IPRS_ParseParameterPtr       DS    A   * Parse next parameter
IPRS_ParseStatementPtr       DS    A   * Parse next statement
IPRS_ResolveParameterPtr     DS    A   * Resolve parameter string
IPRS_BuildTargetsPtr         DS    A   * Build targets
                             DS    0F
IPRS_Vtbl_SIZ                EQU   *-IPRS_Vtbl
*
         MACRO
         IPRS_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_QueryInterfacePtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IPRS_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_AddRefPtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IPRS_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_ReleasePtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IPRS_ParseParameter &OBJECT=,&WORK=,&IPSS=,&ITOK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IPSS * Set IPSS ptr as parm 2
         MVC   8(4,R1),&ITOK * Set ITOK ptr as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_ParseParameterPtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to ParseParameter entry point
         MEND
*
         MACRO
         IPRS_ParseStatement &OBJECT=,&WORK=,&IPSS=,&ITOK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IPSS * Set IPSS ptr as parm 2
         MVC   8(4,R1),&ITOK * Set ITOK ptr as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_ParseStatementPtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to ParseStatement entry point
         MEND
*
         MACRO
         IPRS_ResolveParameter &OBJECT=,&WORK=,&PARMS=,&IAV2TGTS=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&PARMS * Set parameter string of targets parm 3
         MVC   8(4,R1),&IAV2TGTS * Set IAV2 of target names as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_ResolveParameterPtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to ResolveParameter entry point
         MEND
*
         MACRO
         IPRS_BuildTargets &OBJECT=,&WORK=,&IAV2TGTS=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IAV2TGTS * Set IAV2 of targets as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPRS_BuildTargetsPtr-IPRS_Vtbl(,R15)
         BASR  R14,R15      * Branch to BuildTargets entry point
         MEND
