*
* STB obj's Vtbl
*
ISTB_Vtbl                    DSECT
ISTB_QueryInterfacePtr       DS    A   * Regular
ISTB_AddRefPtr               DS    A   *   COM
ISTB_ReleasePtr              DS    A   *     Methods
ISTB_InitPtr                 DS    A   * Initialize StringBuilder
ISTB_EqualsPtr               DS    A   * Compare to other string
ISTB_EqualsZStrPtr           DS    A   * Compare to zero term string
ISTB_CharAtPtr               DS    A   * Get character at position
ISTB_ToUpperCasePtr          DS    A   * Get uppercase copy of string
ISTB_ToLowerCasePtr          DS    A   * Get lowercase copy of string
ISTB_AppendCharPtr           DS    A   * Append a single char
ISTB_AppendStringPtr         DS    A   * Append an ISTR string
ISTB_APpendZStringPtr        DS    A   * Append a zero term string
ISTB_ToStringPtr             DS    A   * Return as ISTR
                             DS    0F
ISTB_Vtbl_SIZ                EQU   *-ISTB_Vtbl
*
         MACRO
         ISTB_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_QueryInterfacePtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         ISTB_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_AddRefPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         ISTB_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_ReleasePtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         ISTB_Init &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_InitPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
*
         MACRO
         ISTB_Equals &OBJECT=,&WORK=,&ISTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ISTR * Set ISTR ptr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_EqualsPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to Equals entry point
         MEND
*
         MACRO
         ISTB_EqualsZStr &OBJECT=,&WORK=,&ZSTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ZSTR    * Get ptr to zero term string
         ST    R14,4(,R1)   * Set ptr to zero term string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_EqualsZStrPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to EqualsZStr entry point
         MEND
*
         MACRO
         ISTB_CharAt &OBJECT=,&WORK=,&POS=,&CHAROUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&POS * Set position as parm 2
         LA    R14,&CHAROUT * Get ptr to char
         ST    R14,8(,R1)   * Set ptr to char as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_CharAtPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to CharAt entry point
         MEND
*
         MACRO
         ISTB_ToUpperCase &OBJECT=,&WORK=,&ISTRPTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ISTRPTR * Get ptr to output string
         ST    R14,4(,R1)   * Set ptr to output string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_ToUpperCasePtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to ToUpperCase entry point
         MEND
*
         MACRO
         ISTB_ToLowerCase &OBJECT=,&WORK=,&ISTRPTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ISTRPTR * Get ptr to output string
         ST    R14,4(,R1)   * Set ptr to output string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_ToLowerCasePtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to ToLowerCase entry point
         MEND
*
         MACRO
         ISTB_AppendChar &OBJECT=,&WORK=,&CHAR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         XR    R14,R14      * Clear R14
         IC    R14,&CHAR    * Put char in R14
         ST    R14,4(,R1)   * Set char as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_AppendCharPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to AppendChar entry point
         MEND
*
         MACRO
         ISTB_AppendString &OBJECT=,&WORK=,&ISTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ISTR * Set ISTR ptr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_AppendStringPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to AppendString entry point
         MEND
*
         MACRO
         ISTB_AppendZString &OBJECT=,&WORK=,&ZSTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ZSTR    * Get ptr to zero term string
         ST    R14,4(,R1)   * Set ptr to zero term string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_AppendZStringPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to AppendZString entry point
         MEND
*
         MACRO
         ISTB_ToString &OBJECT=,&WORK=,&ISTRPTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ISTRPTR * Get ISTR ptr ptr
         ST    R14,4(,R1)   * Set ISTR ptr ptr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTB_ToStringPtr-ISTB_Vtbl(,R15)
         BASR  R14,R15      * Branch to ToString entry point
         MEND
