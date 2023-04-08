*
* IND obj's Vtbl
*
IIND_Vtbl                    DSECT
IIND_QueryInterfacePtr       DS    A   * Regular
IIND_AddRefPtr               DS    A   *   COM
IIND_ReleasePtr              DS    A   *     Methods
IIND_GetNextCharPtr          DS    A   * Get next character
IIND_OpenPtr                 DS    A   * Open input file
IIND_ClosePtr                DS    A   * Close input file
                             DS    0F
IIND_Vtbl_SIZ                EQU   *-IIND_Vtbl
*
         MACRO
         IIND_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IIND_QueryInterfacePtr-IIND_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IIND_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IIND_AddRefPtr-IIND_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IIND_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IIND_ReleasePtr-IIND_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IIND_GetNextChar &OBJECT=,&WORK=,&IIFO=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&IIFO * Set IIFO ptr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IIND_GetNextCharPtr-IIND_Vtbl(,R15)
         BASR  R14,R15      * Branch to GetNextChar entry point
         MEND
*
         MACRO
         IIND_Open &OBJECT=,&WORK=,&DDNAME=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&DDNAME  * Get ptr to DD Name
         ST    R14,4(,R1)   * Set ptr to DD name as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IIND_OpenPtr-IIND_Vtbl(,R15)
         BASR  R14,R15      * Branch to Open entry point
         MEND
*
         MACRO
         IIND_Close &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IIND_ClosePtr-IIND_Vtbl(,R15)
         BASR  R14,R15      * Branch to Close entry point
         MEND
