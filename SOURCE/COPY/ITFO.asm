*
* TFO obj's Vtbl
*
ITFO_Vtbl                    DSECT
ITFO_QueryInterfacePtr       DS    A   * Regular
ITFO_AddRefPtr               DS    A   *   COM
ITFO_ReleasePtr              DS    A   *     Methods
ITFO_InitPtr                 DS    A   * Initialize token
                             DS    0F
ITFO_Vtbl_SIZ                EQU   *-ITFO_Vtbl
*
         MACRO
         ITFO_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITFO_QueryInterfacePtr-ITFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         ITFO_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITFO_AddRefPtr-ITFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         ITFO_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITFO_ReleasePtr-ITFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         ITFO_Init &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,ITFO_InitPtr-ITFO_Vtbl(,R15)
         BASR  R14,R15      * Branch to Init entry point
         MEND
*
* Token type constants
*
TOKEN_TYPE_UNDETERMINED      EQU   X'00000000'
TOKEN_TYPE_EOF               EQU   X'80000000'
TOKEN_TYPE_EOL               EQU   X'40000000'
TOKEN_TYPE_COMMENT           EQU   X'20000000'
TOKEN_TYPE_SPECIAL           EQU   X'10000000'
TOKEN_TYPE_EQUALS            EQU   X'08000000'
TOKEN_TYPE_IMMEDEQUALS       EQU   X'04000000'
TOKEN_TYPE_CONDEQUALS        EQU   X'02000000'
TOKEN_TYPE_RULE              EQU   X'01000000'
TOKEN_TYPE_CONTINUATION      EQU   X'00800000'
TOKEN_TYPE_NORMAL            EQU   X'00400000'
TOKEN_TYPE_VARIABLE          EQU   X'00200000'
TOKEN_TYPE_TARGETVAR         EQU   X'00100000'
TOKEN_TYPE_MEMBERVAR         EQU   X'00080000'
TOKEN_TYPE_CLOSEBRACKET      EQU   X'00040000'
TOKEN_TYPE_CLOSECURLY        EQU   X'00020000'
TOKEN_TYPE_RECIPEPREFIX      EQU   X'00010000'
TOKEN_TYPE_CALL              EQU   X'00008000'
TOKEN_TYPE_SH                EQU   X'00004000'
TOKEN_TYPE_COMMA             EQU   X'00002000'
TOKEN_TYPE_LOGSWITCH         EQU   X'00001000'
TOKEN_TYPE_TARGETSWITCH      EQU   X'00000800'
