*
* AVL obj's Vtbl
*
IAVL_Vtbl                    DSECT
IAVL_QueryInterfacePtr       DS    A   * Regular
IAVL_AddRefPtr               DS    A   *   COM
IAVL_ReleasePtr              DS    A   *     Methods
IAVL_CountPtr                DS    A   * Get array size
IAVL_ExistsPtr               DS    A   * Check if entry exists
IAVL_QueryPtr                DS    A   * Get any entry starting from
IAVL_NextPtr                 DS    A   * Get next entry
IAVL_Next2Ptr                DS    A   * Get next entry exactMatch
IAVL_InsertPtr               DS    A   * Add new entry
IAVL_UpdatePtr               DS    A   * Update existing entry
IAVL_DeletePtr               DS    A   * Remove entry
IAVL_ContainsPtr             DS    A   * Check if value exists
IAVL_PutPtr                  DS    A   * Update entry or insert
                             DS    0F
IAVL_Vtbl_SIZ                EQU   *-IAVL_Vtbl
*
         MACRO
         IAVL_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_QueryInterfacePtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IAVL_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_AddRefPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IAVL_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_ReleasePtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IAVL_Count &OBJECT=,&WORK=,&COUNT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&COUNT_OUT * Get ptr to FWORD
         ST    R14,4(,R1)   * Set ptr to FWORD as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_CountPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Count entry point
         MEND
*
         MACRO
         IAVL_Exists &OBJECT=,&WORK=,&NAME_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_ExistsPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Exists entry point
         MEND
*
         MACRO
         IAVL_Query &OBJECT=,&WORK=,&NAME_IN=,&VARIANT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         LA    R14,&VARIANT_OUT * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_QueryPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Query entry point
         MEND
*
         MACRO
         IAVL_Next &OBJECT=,&WORK=,&NAME_IN=,&NAMEPTR_OUT=,            X
               &VARIANT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         LA    R14,&NAMEPTR_OUT * Get ptr to output name ptr
         ST    R14,8(,R1)   * Set ptr to output name ptr as parm 3
         LA    R14,&VARIANT_OUT * Get ptr to VARIANT
         ST    R14,12(,R1)  * Set ptr to VARIANT as parm 4
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_NextPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Next entry point
         MEND
*
         MACRO
         IAVL_Next2 &OBJECT=,&WORK=,&NAME_IN=,&NAMEPTR_OUT=,           X
               &VARIANT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         LA    R14,&NAMEPTR_OUT * Get ptr to output name ptr
         ST    R14,8(,R1)   * Set ptr to output name ptr as parm 3
         LA    R14,&VARIANT_OUT * Get ptr to VARIANT
         ST    R14,12(,R1)  * Set ptr to VARIANT as parm 4
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_Next2Ptr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Next2 entry point
         MEND
*
         MACRO
         IAVL_Insert &OBJECT=,&WORK=,&NAME_IN=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         LA    R14,&VARIANT_IN * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_InsertPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Insert entry point
         MEND
*
         MACRO
         IAVL_Update &OBJECT=,&WORK=,&NAME_IN=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         LA    R14,&VARIANT_IN * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_UpdatePtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Update entry point
         MEND
*
         MACRO
         IAVL_Delete &OBJECT=,&WORK=,&NAME_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_DeletePtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Delete entry point
         MEND
*
         MACRO
         IAVL_Contains &OBJECT=,&WORK=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&VARIANT_IN * Get ptr to VARIANT with value
         ST    R14,4(,R1)   * Set ptr to VARIANT as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_ContainsPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Contains entry point
         MEND
*
         MACRO
         IAVL_Put &OBJECT=,&WORK=,&NAME_IN=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&NAME_IN * Set ptr to name as parm 2
         LA    R14,&VARIANT_IN * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAVL_PutPtr-IAVL_Vtbl(,R15)
         BASR  R14,R15      * Branch to Put entry point
         MEND
