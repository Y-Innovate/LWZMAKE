*
* AV2 obj's Vtbl
*
IAV2_Vtbl                    DSECT
IAV2_QueryInterfacePtr       DS    A   * Regular
IAV2_AddRefPtr               DS    A   *   COM
IAV2_ReleasePtr              DS    A   *     Methods
IAV2_CountPtr                DS    A   * Get array size
IAV2_ExistsPtr               DS    A   * Check if entry exists
IAV2_QueryPtr                DS    A   * Get any entry starting from
IAV2_NextPtr                 DS    A   * Get next entry
IAV2_Next2Ptr                DS    A   * Get next entry exactMatch
IAV2_InsertPtr               DS    A   * Add new entry
IAV2_UpdatePtr               DS    A   * Update existing entry
IAV2_DeletePtr               DS    A   * Remove entry
IAV2_ContainsPtr             DS    A   * Check if value exists
                             DS    0F
IAV2_Vtbl_SIZ                EQU   *-IAV2_Vtbl
*
         MACRO
         IAV2_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_QueryInterfacePtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IAV2_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_AddRefPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IAV2_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_ReleasePtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IAV2_Count &OBJECT=,&WORK=,&COUNT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&COUNT_OUT * Get ptr to FWORD
         ST    R14,4(,R1)   * Set ptr to FWORD as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_CountPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Count entry point
         MEND
*
         MACRO
         IAV2_Exists &OBJECT=,&WORK=,&INDEX_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set fword as parm2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_ExistsPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Exists entry point
         MEND
*
         MACRO
         IAV2_Query &OBJECT=,&WORK=,&INDEX_IN=,&VARIANT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set fword as parm 2
         LA    R14,&VARIANT_OUT * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_QueryPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Query entry point
         MEND
*
         MACRO
         IAV2_Next &OBJECT=,&WORK=,&INDEX_IN=,&INDEXPTR_OUT=,          X
               &VARIANT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set search index as parm 2
         LA    R14,&INDEXPTR_OUT * Get ptr to return index ptr
         ST    R14,8(,R1)   * Set ptr to return index ptr as parm 3
         LA    R14,&VARIANT_OUT * Get ptr to VARIANT
         ST    R14,12(,R1)  * Set ptr to VARIANT as parm 4
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_NextPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Next entry point
         MEND
*
         MACRO
         IAV2_Next2 &OBJECT=,&WORK=,&INDEX_IN=,&INDEXPTR_OUT=,         X
               &VARIANT_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set search index as parm 2
         LA    R14,&INDEXPTR_OUT * Get ptr to return index ptr
         ST    R14,8(,R1)   * Set ptr to return index ptr as parm 3
         LA    R14,&VARIANT_OUT * Get ptr to VARIANT
         ST    R14,12(,R1)  * Set ptr to VARIANT as parm 4
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_Next2Ptr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Next2 entry point
         MEND
*
         MACRO
         IAV2_Insert &OBJECT=,&WORK=,&INDEX_IN=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set fword index as parm 2
         LA    R14,&VARIANT_IN * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_InsertPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Insert entry point
         MEND
*
         MACRO
         IAV2_Update &OBJECT=,&WORK=,&INDEX_IN=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set fword index as parm 2
         LA    R14,&VARIANT_IN * Get ptr to VARIANT
         ST    R14,8(,R1)   * Set ptr to VARIANT as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_UpdatePtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Update entry point
         MEND
*
         MACRO
         IAV2_Delete &OBJECT=,&WORK=,&INDEX_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         MVC   4(4,R1),&INDEX_IN * Set fword index as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_DeletePtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Delete entry point
         MEND
*
         MACRO
         IAV2_Contains &OBJECT=,&WORK=,&VARIANT_IN=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&VARIANT_IN * Get ptr to VARIANT with value
         ST    R14,4(,R1)   * Set ptr to VARIANT as parm 2
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IAV2_ContainsPtr-IAV2_Vtbl(,R15)
         BASR  R14,R15      * Branch to Contains entry point
         MEND
