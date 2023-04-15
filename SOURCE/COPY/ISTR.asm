*
* STR obj's Vtbl
*
ISTR_Vtbl                    DSECT
ISTR_QueryInterfacePtr       DS    A   * Regular
ISTR_AddRefPtr               DS    A   *   COM
ISTR_ReleasePtr              DS    A   *     Methods
ISTR_SetPtr                  DS    A   * Set string value
ISTR_EqualsPtr               DS    A   * Compare to other string
ISTR_EqualsZStrPtr           DS    A   * Compare to zero term string
ISTR_CharAtPtr               DS    A   * Get character at position
ISTR_ToUpperCasePtr          DS    A   * Get uppercase copy of string
ISTR_ToLowerCasePtr          DS    A   * Get lowercase copy of string
                             DS    0F
ISTR_Vtbl_SIZ                EQU   *-ISTR_Vtbl
*
         MACRO
         ISTR_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_QueryInterfacePtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         ISTR_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_AddRefPtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         ISTR_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_ReleasePtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         ISTR_Set &OBJECT=,&WORK=,&STR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&STR     * Get ptr to zero term string
         ST    R14,4(,R1)   * Set ptr to zero term string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_SetPtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to Set entry point
         MEND
*
         MACRO
         ISTR_Equals &OBJECT=,&WORK=,&ISTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&ISTR * Set ISTR ptr as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_EqualsPtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to Equals entry point
         MEND
*
         MACRO
         ISTR_EqualsZStr &OBJECT=,&WORK=,&ZSTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ZSTR    * Get ptr to zero term string
         ST    R14,4(,R1)   * Set ptr to zero term string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_EqualsZStrPtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to EqualsZStr entry point
         MEND
*
         MACRO
         ISTR_CharAt &OBJECT=,&WORK=,&POS=,&CHAROUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&POS * Set position as parm 2
         LA    R14,&CHAROUT * Get ptr to char
         ST    R14,8(,R1)   * Set ptr to char as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_CharAtPtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to CharAt entry point
         MEND
*
         MACRO
         ISTR_ToUpperCase &OBJECT=,&WORK=,&ISTRPTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ISTRPTR * Get ptr to output string
         ST    R14,4(,R1)   * Set ptr to output string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_ToUpperCasePtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to ToUpperCase entry point
         MEND
*
         MACRO
         ISTR_ToLowerCase &OBJECT=,&WORK=,&ISTRPTR=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&ISTRPTR * Get ptr to output string
         ST    R14,4(,R1)   * Set ptr to output string as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ISTR_ToLowerCasePtr-ISTR_Vtbl(,R15)
         BASR  R14,R15      * Branch to ToLowerCase entry point
         MEND
