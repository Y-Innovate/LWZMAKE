*
* USS obj's Vtbl
*
IUSS_Vtbl                    DSECT
IUSS_QueryInterfacePtr       DS    A   * Regular
IUSS_AddRefPtr               DS    A   *   COM
IUSS_ReleasePtr              DS    A   *     Methods
IUSS_BPX1STAPtr              DS    A   * Stat a USS file
IUSS_BPX1SPNPtr              DS    A   * Execute a USS sh
IUSS_BPX1OPNPtr              DS    A   * Open a USS file
IUSS_BPX1CLOPtr              DS    A   * Close a USS file
IUSS_BPX1REDPtr              DS    A   * Read a character
                             DS    0F
IUSS_Vtbl_SIZ                EQU   *-IUSS_Vtbl
*
         MACRO
         IUSS_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_QueryInterfacePtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IUSS_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_AddRefPtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IUSS_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_ReleasePtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IUSS_BPX1STA &OBJECT=,&WORK=,&IFFO=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IFFO * Set IFFO as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_BPX1STAPtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to BPX1STA entry point
         MEND
*
         MACRO
         IUSS_BPX1SPN &OBJECT=,&WORK=,&ISTBSH=,&ISTBPTR_RETVAL=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ISTBSH * Set ptr to USS sh ISTR as parm 2
         LA    R14,&ISTBPTR_RETVAL * Get ptr to ISTR return value
         ST    R14,8(,R1)   * Set ptr to ISTR return value as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_BPX1SPNPtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to BPX1SPN entry point
         MEND
*
         MACRO
         IUSS_BPX1OPN &OBJECT=,&WORK=,&PATH=,&FDESC_RETVAL=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&PATH * Set USS path as parm 2
         LA    R14,&FDESC_RETVAL * Get ptr to file descriptor
         ST    R14,8(,R1)   * Set ptr to file descriptor as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_BPX1OPNPtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to BPX1OPN entry point
         MEND
*
         MACRO
         IUSS_BPX1CLO &OBJECT=,&WORK=,&FDESC=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&FDESC * Set file descriptor as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_BPX1CLOPtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to BPX1CLO entry point
         MEND
*
         MACRO
         IUSS_BPX1RED &OBJECT=,&WORK=,&FDESC=,&RETCHAR=,&RETEOF=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&FDESC * Set file descriptor as parm 2
         LA    R14,&RETCHAR * Get ptr to return character
         ST    R14,8(,R1)   * Set ptr to return character as parm 3
         LA    R14,&RETEOF  * Get ptr to return EOF
         ST    R14,12(,R1)  * Set ptr to return EOF as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IUSS_BPX1REDPtr-IUSS_Vtbl(,R15)
         BASR  R14,R15      * Branch to BPX1RED entry point
         MEND
