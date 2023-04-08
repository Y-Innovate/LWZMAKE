*
* PSS obj's Vtbl
*
IPSS_Vtbl                    DSECT
IPSS_QueryInterfacePtr       DS    A   * Regular
IPSS_AddRefPtr               DS    A   *   COM
IPSS_ReleasePtr              DS    A   *     Methods
IPSS_IsExpectedTokenTypePtr  DS    A   * Is token expected here
IPSS_GetLastStatePtr         DS    A   * Get the last state entry
IPSS_AlterLastStatePtr       DS    A   * Alter the last state entry
IPSS_PushPtr                 DS    A   * Add entry at the end
IPSS_PopPtr                  DS    A   * Erase the last state entry
IPSS_SetPhasePtr             DS    A   * Set phase nr
                             DS    0F
IPSS_Vtbl_SIZ                EQU   *-IPSS_Vtbl
*
         MACRO
         IPSS_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_QueryInterfacePtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IPSS_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_AddRefPtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IPSS_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_ReleasePtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IPSS_IsExpectedTokenType &OBJECT=,&WORK=,&ITFO=,&RESULT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ITFO * Set ITFO ptr as parm 2
         LA    R14,&RESULT  * Get ptr to flag
         ST    R14,8(,R1)   * Set ptr to flag as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_IsExpectedTokenTypePtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to IsExpectedTokenType entry point
         MEND
*
         MACRO
         IPSS_GetLastState &OBJECT=,&WORK=,&STATE_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&STATE_OUT * Get ptr to fword state
         ST    R14,4(,R1)   * Set ptr to fword state
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_GetLastStatePtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to GetLastState entry point
         MEND
*
         MACRO
         IPSS_AlterLastState &OBJECT=,&WORK=,&STATE_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&STATE_IN * Set fword state as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_AlterLastStatePtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to AlterLastState entry point
         MEND
*
         MACRO
         IPSS_Push &OBJECT=,&WORK=,&STATE_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&STATE_IN * State to push as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_PushPtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Push entry point
         MEND
*
         MACRO
         IPSS_Pop &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_PopPtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Pop entry point
         MEND
*
         MACRO
         IPSS_SetPhase &OBJECT=,&WORK=,&PHASE=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         XR    R14,R14      * Clear R14
         IC    R14,&PHASE   * Insert phase nr
         ST    R14,4(,R1)   * Set phase nr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IPSS_SetPhasePtr-IPSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Pop entry point
         MEND
*
* Parser states
*
PARSER_STATE_UNDETERMINED        EQU   0
PARSER_STATE_IN_ASSIGNMENT1      EQU   1
PARSER_STATE_IN_ASSIGNMENT2      EQU   2
PARSER_STATE_IN_ASSIGNMENT3      EQU   3
PARSER_STATE_IN_PHONY1           EQU   4
PARSER_STATE_IN_PHONY2           EQU   5
PARSER_STATE_IN_NORMAL           EQU   6
PARSER_STATE_IN_CONTINUATION     EQU   7
PARSER_STATE_IN_RULE1            EQU   8
PARSER_STATE_IN_RULE2            EQU   9
PARSER_STATE_IN_RULE3            EQU   10
PARSER_STATE_IN_VARIABLE1        EQU   11
PARSER_STATE_IN_VARIABLE2A       EQU   12
PARSER_STATE_IN_VARIABLE2B       EQU   13
PARSER_STATE_IN_RECIPE           EQU   14
PARSER_STATE_IN_CALL1            EQU   15
PARSER_STATE_IN_CALL2            EQU   16
PARSER_STATE_IN_SH1              EQU   17
PARSER_STATE_IN_SH2              EQU   18
PARSER_STATE_IN_MEMBERLIST1      EQU   19
PARSER_STATE_IN_MEMBERLIST2A     EQU   20
PARSER_STATE_IN_MEMBERLIST2B     EQU   21
PARSER_STATE_IN_MEMBERLIST3      EQU   22
PARSER_STATE_IN_MEMBERLIST4A     EQU   23
PARSER_STATE_IN_MEMBERLIST4B     EQU   24
PARSER_STATE_IN_ADDPDSNAME1      EQU   25
PARSER_STATE_IN_ADDPDSNAME2A     EQU   26
PARSER_STATE_IN_ADDPDSNAME2B     EQU   27
PARSER_STATE_IN_APPEND1          EQU   28
PARSER_STATE_IN_APPEND2A         EQU   29
PARSER_STATE_IN_APPEND2B         EQU   30
PARSER_STATE_IN_PREPEND1         EQU   31
PARSER_STATE_IN_PREPEND2A        EQU   32
PARSER_STATE_IN_PREPEND2B        EQU   33
PARSER_STATE_IN_STRIPEXT1        EQU   34
PARSER_STATE_IN_STRIPEXT2A       EQU   35
PARSER_STATE_IN_STRIPEXT2B       EQU   36
PARSER_STATE_IN_FUNCTION1        EQU   37
PARSER_STATE_IN_FUNCTION2A       EQU   38
PARSER_STATE_IN_FUNCTION2B       EQU   39
PARSER_STATE_IN_FUNCTION3        EQU   40
PARSER_STATE_IN_FUNCTION4A       EQU   41
PARSER_STATE_IN_FUNCTION4B       EQU   42
PARSER_STATE_IN_SHFUNCTION1      EQU   43
PARSER_STATE_IN_SHFUNCTION2A     EQU   44
PARSER_STATE_IN_SHFUNCTION2B     EQU   45
PARSER_STATE_IN_PARAMETER        EQU   46
PARSER_STATE_IN_LOGPARM          EQU   47
PARSER_STATE_IN_TARGETPARM1      EQU   48
PARSER_STATE_IN_TARGETPARM2      EQU   49
