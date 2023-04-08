*---------------------------------------------------------------------*
* Program    : LWZMAVL                                                *
* Description: COM object for AVL tree with either named entries (AVL)*
*              or numbered entries (AV2). An AVL tree is essentially  *
*              an array that is accessed and managed as a self balan- *
*              cing binary search tree which looks like this:         *
*                          * D                                        *
*                         / \                                         *
*                        /   \                                        *
*                       /     \                                       *
*                    B *       * F                                    *
*                     / \     / \                                     *
*                    /   \   /   \                                    *
*                 A *   C * * E   * G                                 *
*                                  \                                  *
*                                   \                                 *
*                                    * H                              *
*                                                                     *
*              Each entry in the array is represented by a node,      *
*              which is an AVLNODE DSECT with the necessary informa-  *
*              tion to allow LWZMAVL(2) to traverse the array.        *
*              Every AVLNODE has a value in the form of a VARIANT,    *
*              which is a small DSECT that allows for a type as well  *
*              as a value (types like int, string, object*, etc.)     *
*---------------------------------------------------------------------*
         TITLE 'LWZMAVL'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine creates a new AVL or AV2 object
*
LWZMAVL  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSA,R13       * Address DSA and extra stg for vars
*
         L     R6,0(,R1)         * Pointer to GUID
         L     R5,4(,R1)         * Pointer to pointer object
         ST    R5,RETPTR         * Save return object ptr ptr
         MVC   0(4,R5),=A(0)     * By default return NULL object
*
         USING GLOBAL,R9         * Address global area DSECT
*
*        Was a new AVL object requested?
         IF (CLC,0(16,R6),EQ,G_IAVL_GUID) THEN
            MNEWOBJ OBJTYPE=AVL,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING AVL_obj,R14
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_AVL
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
*        Was a new IIFO object requested?
         ELSEIF (CLC,0(16,R6),EQ,G_IAV2_GUID) THEN
            MNEWOBJ OBJTYPE=AV2,WORK=WORK * Alloc new object
*
*           Init obj attributes
            USING AV2_obj,R14
            DROP  R14
*
            ASI   G_OBJCOUNT,1   * Increate global obj count
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R14,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=MAK501D_AV2
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
*
         ELSE
            CALL  CEE3ABD,(=A(1003),=A(3)),MF=(E,WORK)
         ENDIF
*
LWZMAVL_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
*
                             DS    0F
AVL#01A                      DC    A(AVL#01)   * QueryInterface
AVL#02A                      DC    A(AVL#02)   * AddRef
AVL#03A                      DC    A(AVL#03)   * Release
AVL#04A                      DC    A(AVL#04)   * Count
AVL#05A                      DC    A(AVL#05)   * Exists
AVL#06A                      DC    A(AVL#06)   * Query
AVL#07A                      DC    A(AVL#07)   * Next
AVL#08A                      DC    A(AVL#08)   * Next2
AVL#09A                      DC    A(AVL#09)   * Insert
AVL#10A                      DC    A(AVL#10)   * Update
AVL#11A                      DC    A(AVL#11)   * Delete
AVL#12A                      DC    A(AVL#12)   * Contains
AVL#13A                      DC    A(AVL#13)   * Put
*
                             DS    0F
AV2#01A                      DC    A(AV2#01)   * QueryInterface
AV2#02A                      DC    A(AV2#02)   * AddRef
AV2#03A                      DC    A(AV2#03)   * Release
AV2#04A                      DC    A(AV2#04)   * Count
AV2#05A                      DC    A(AV2#05)   * Exists
AV2#06A                      DC    A(AV2#06)   * Query
AV2#07A                      DC    A(AV2#07)   * Next
AV2#08A                      DC    A(AV2#08)   * Next2
AV2#09A                      DC    A(AV2#09)   * Insert
AV2#10A                      DC    A(AV2#10)   * Update
AV2#11A                      DC    A(AV2#11)   * Delete
AV2#12A                      DC    A(AV2#12)   * Contains
*
                             DS    0F
MAK501D_AVL                  DC    C'MAK501D Created IAVL object '
                             DC    X'00'
                             DS    0F
MAK501D_AV2                  DC    C'MAK501D Created IAV2 object '
                             DC    X'00'
*
PPA      CEEPPA
*
         CEECAA
*
         CEEDSA
*
WORKDSA                      DSECT
*
                             ORG   *+CEEDSASZ
*
FC                           DS    3A  * Feedback code
WORK                         DS    4A  * Work area
RETPTR                       DS    A   * Address of return ptr
OBJPTR                       DS    A   * Address of new obj stg
*
WORKDSA_SIZ                  EQU   *-WORKDSA
*
         MGLOBAL                 * Global area DSECT
*
         COPY  DSCOMOBJ          * Generic COM obj DSECT
*
         COPY  DSAVL             * IAVL obj DSECT
         COPY  DSSTB             * ISTB obj DSECT
*
         COPY  VARIANT           * DSECT for typed value
*
         DROP
*
LWZMAVL  CSECT
*
* IAVL QueryInterface
*
AVL#01   MQRYIFCE SUF=A01,IFACE=IAVL
*
* IAVL AddRef
*
AVL#02   MADDREF
*
* IAVL Release
*
AVL#03   CEEENTRY AUTO=WORKDSAA03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAA03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    AVL#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING AVL_obj,R6
*
            LA    R15,AVLNODE_root
            IF (CLC,0(4,R15),NE,=A(0)) THEN
               LA    R1,WORKA03
               ST    R15,0(,R1)
               L     R15,AVL#95A_A03
               BASR  R14,R15
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRA03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRA03,FCA03),MF=(E,WORKA03)
*
            L     R15,G_OBJCOUNT
            BCTR  R15,R0
            ST    R15,G_OBJCOUNT
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R6,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKA03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKA03,      X
               ZSTR=MAK502D_AVL
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKA03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKA03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
AVL#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
AVL#95A_A03                  DC    A(AVL#95)
*
                             DS    0F
MAK502D_AVL                  DC    C'MAK502D Deleted IAVL object '
                             DC    X'00'
*
WORKDSAA03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRA03                    DS    A
WORKA03                      DS    3A
*
WORKDSAA03_SIZ               EQU   *-WORKDSAA03
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Count
*
AVL#04   MGETPFW OBJ=AVL,PROP=nodeCount
*
* IAVL Exists
*
AVL#05   CEEENTRY AUTO=WORKDSAA05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA05,4(R1) * Parm 2 is search name ptr
*
*        Call recursive Query function
         MVC   AVLNODEPTRA05,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKA05        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA05 * Set search name ptr as parm 2
         LA    R15,AVLNODEPTRA05 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AVL#94A_A05   * Get AVL#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query found a node
         IF (CLC,AVLNODEPTRA05,NE,=A(0)) THEN
            LA    R2,1
         ELSE
            XR    R2,R2
         ENDIF
*
AVL#05_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A05                  DC    A(AVL#94) QueryAVLNODE
*
WORKDSAA05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA05                      DS    4A
*
PARMNAMEA05                  DS    A
PARMEXISTSA05                DS    A
*
AVLNODEPTRA05                DS    A
*
WORKDSAA05_SIZ               EQU   *-WORKDSAA05
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Query
*
AVL#06   CEEENTRY AUTO=WORKDSAA06_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA06,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA06,4(R1) * Parm 2 is search name ptr
*
         MVC   PARMVALUEA06,8(R1) * Parm 3 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRA06,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKA06        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA06 * Set search name ptr as parm 2
         LA    R15,AVLNODEPTRA06 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AVL#94A_A06   * Get AVL#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRA06,EQ,=A(0)) THEN
*           Set return value VARIANT to VT_NULL (which is not VT_EMPTY)
            L     R4,PARMVALUEA06 * Get ptr to VARIANT
            MVC   vt-VARIANT(4,R4),=A(VT_NULL)
            MVC   value-VARIANT(4,R4),=A(0)
            MVC   value-VARIANT+4(4,R4),=A(0)
*
            XR    R2,R2
*
*        If recursive Query found a node
         ELSE
*           Copy the found node's value
            LA    R1,WORKA06     * Point R1 to parameter area
            L     R15,AVLNODEPTRA06 * Get ptr to AVLNODE
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Get ptr to VRNTSRC
            ST    R15,0(,R1)     * Set ptr to VRNTSRC as parm 1
            MVC   4(4,R1),PARMVALUEA06 * Set ptr to VRNTTGT as parm 2
            L     R15,LWZMVCPA_A06 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            LA    R2,1
         ENDIF
*
AVL#06_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A06                  DC    A(AVL#94) QueryAVLNODE
LWZMVCPA_A06                 DC    V(LWZMVCP)
*
WORKDSAA06                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA06                      DS    4A
*
PARMNAMEA06                  DS    A
PARMVALUEA06                 DS    A
*
AVLNODEPTRA06                DS    A
*
WORKDSAA06_SIZ               EQU   *-WORKDSAA06
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Next
*
AVL#07   CEEENTRY AUTO=WORKDSAA07_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA07,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA07,4(R1) * Parm 2 is search name ptr
*
         L     R15,8(,R1)        * Parm 3 is return name ptr
         ST    R15,PARMNAMEPTRA07 * Save as local var
         MVC   0(4,R15),=A(0)    * Initialize return name to NULL
*
         MVC   PARMVALUEA07,12(R1) * Parm 4 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRA07,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKA07        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA07 * Set search name ptr as parm 2
         LA    R15,AVLNODEPTRA07 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(0)    * Set no exact match as parm 4
         L     R15,AVL#94A_A07   * Get AVL#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRA07,EQ,=A(0)) THEN
*           Set return value VARIANT to VT_NULL (which is not VT_EMPTY)
            L     R4,PARMVALUEA07 * Get ptr to VARIANT
            MVC   vt-VARIANT(4,R4),=A(VT_NULL)
            MVC   value-VARIANT(4,R4),=A(0)
            MVC   value-VARIANT+4(4,R4),=A(0)
*
            XR    R2,R2
*
*        If recursive Query found a node
         ELSE
*           Allocate memory to copy found node's name
            XR    R0,R0          * Search for terminating zero
            L     R1,AVLNODEPTRA07 * Address found node
            L     R1,AVLNODE_NAME-AVLNODE(,R1) * Point R1 to name
            LA    R2,4095(,R1)   * Search for max 4K
            SRST  R2,R1          * Scan for zero terminator
            BRC   1,*-4          * Scan was incomplete, try again
            BRC   2,*-12         * Not found, try another 4K
*
            L     R1,AVLNODEPTRA07 * Address found node
            L     R1,AVLNODE_NAME-AVLNODE(,R1) * Point R3 to name
            SR    R2,R1          * Calculate length
            LA    R2,1(,R2)      * Add 1 for zero term
            ST    R2,G_GTSTSIZ   * And save it for GTST
*
*           Allocate name copy memory
            LA    R1,WORKA07
            ST    R2,0(,R1)
            MVC   4(4,R1),PARMNAMEPTRA07
            L     R15,G_GTST
            BASR  R14,R15
*
*           Copy the found node's name
            L     R4,PARMNAMEPTRA07 * Get return name parameter
            L     R4,0(,R4)
            L     R5,G_GTSTSIZ   * Size of name + len + zero term
            L     R2,AVLNODEPTRA07 * Point R2 to found node
            L     R2,AVLNODE_NAME-AVLNODE(,R2) * Point R2 to name
            LR    R3,R5          * Same length
            MVCL  R4,R2          * Copy name
*
*           Copy the found node's value
            LA    R1,WORKA07     * Point R1 to parameter area
            L     R15,AVLNODEPTRA07 * Get ptr to AVLNODE
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Get ptr to VRNTSRC
            ST    R15,0(,R1)     * Set ptr to VRNTSRC as parm 1
            MVC   4(4,R1),PARMVALUEA07 * Set ptr to VRNTTGT as parm 2
            L     R15,LWZMVCPA_A07 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            LA    R2,1
         ENDIF
*
AVL#07_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A07                  DC    A(AVL#94)  QueryAVLNODE
LWZMVCPA_A07                 DC    V(LWZMVCP)
*
WORKDSAA07                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA07                      DS    4A
*
PARMNAMEA07                  DS    A
PARMNAMEPTRA07               DS    A
PARMVALUEA07                 DS    A
*
AVLNODEPTRA07                DS    A
*
WORKDSAA07_SIZ               EQU   *-WORKDSAA07
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Next2
*
AVL#08   CEEENTRY AUTO=WORKDSAA08_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA08,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA08,4(R1) * Parm 2 is search name ptr
*
         L     R15,8(,R1)        * Parm 3 is return name ptr
         ST    R15,PARMNAMEPTRA08 * Save as local var
         MVC   0(4,R15),=A(0)    * Initialize return name to NULL
*
         MVC   PARMVALUEA08,12(R1) * Parm 4 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRA08,=A(0) * Initialize AVLNODEPTR
*
*        If search name is not empty
         IF (CLC,PARMNAMEA08,NE,=A(0)) THEN
            LA    R1,WORKA08     * Point R1 to parameter area
            MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root parm 1
            MVC   4(4,R1),PARMNAMEA08 * Set search name ptr as parm 2
            LA    R15,AVLNODEPTRA08 * Get ptr to AVLNODEPTR
            ST    R15,8(,R1)     * Set ptr to AVLNODEPTR as parm 3
            MVC   12(4,R1),=A(1) * Set exact match as parm 4
            L     R15,AVL#94A_A08 * Get AVL#94 entry point
            BASR  R14,R15        * Go for it
*
*           If a node was found, copy the next ptr's node
            IF (CLC,AVLNODEPTRA08,NE,=A(0)) THEN
               L     R15,AVLNODEPTRA08 * Address found node
               MVC   AVLNODEPTRA08,AVLNODE_NEXT-AVLNODE(R15) * copy nxt
            ENDIF
         ELSE
            MVC   AVLNODEPTRA08,firstNode
         ENDIF
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRA08,EQ,=A(0)) THEN
*           Set return value VARIANT to VT_NULL (which is not VT_EMPTY)
            L     R4,PARMVALUEA08 * Get ptr to VARIANT
            MVC   vt-VARIANT(4,R4),=A(VT_NULL)
            MVC   value-VARIANT(4,R4),=A(0)
            MVC   value-VARIANT+4(4,R4),=A(0)
*
            XR    R2,R2
*
*        If recursive Query found a node
         ELSE
*           Allocate memory to copy found node's name
            XR    R0,R0          * Search for terminating zero
            L     R1,AVLNODEPTRA08 * Address found node
            L     R1,AVLNODE_NAME-AVLNODE(,R1) * Point R1 to name
            LA    R2,4095(,R1)   * Search for max 4K
            SRST  R2,R1          * Scan for zero terminator
            BRC   1,*-4          * Scan was incomplete, try again
            BRC   2,*-12         * Not found, try another 4K
*
            L     R1,AVLNODEPTRA08 * Address found node
            L     R1,AVLNODE_NAME-AVLNODE(,R1) * Point R3 to name
            SR    R2,R1          * Calculate length
            LA    R2,1(,R2)      * Add 1 for zero term
            ST    R2,G_GTSTSIZ   * And save it for GTST
*
*           Allocate name copy memory
            LA    R1,WORKA08
            ST    R2,0(,R1)
            MVC   4(4,R1),PARMNAMEPTRA08
            L     R15,G_GTST
            BASR  R14,R15
*
*           Copy the found node's name
            L     R4,PARMNAMEPTRA08 * Get return name parameter
            L     R5,G_GTSTSIZ   * Size of name + len + zero term
            L     R2,AVLNODEPTRA08 * Point R2 to found node
            L     R2,AVLNODE_NAME-AVLNODE(,R2) * Point R2 to name
            LR    R3,R5          * Same length
            MVCL  R4,R2          * Copy name
*
*           Copy the found node's value
            LA    R1,WORKA08     * Point R1 to parameter area
            L     R15,AVLNODEPTRA08 * Get ptr to AVLNODE
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Get ptr to VRNTSRC
            ST    R15,0(,R1)     * Set ptr to VRNTSRC as parm 1
            MVC   4(4,R1),PARMVALUEA08 * Set ptr to VRNTTGT as parm 2
            L     R15,LWZMVCPA_A08 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            LA    R2,1
         ENDIF
*
AVL#08_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A08                  DC    A(AVL#94)  QueryAVLNODE
LWZMVCPA_A08                 DC    V(LWZMVCP)
*
WORKDSAA08                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA08                      DS    4A
*
PARMNAMEA08                  DS    A
PARMNAMEPTRA08               DS    A
PARMVALUEA08                 DS    A
*
AVLNODEPTRA08                DS    A
*
WORKDSAA08_SIZ               EQU   *-WORKDSAA08
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Insert
*
AVL#09   CEEENTRY AUTO=WORKDSAA09_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA09,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA09,4(R1) * Parm 2 is new node name ptr
*
         MVC   PARMVALUEA09,8(R1) * Parm 3 is ptr to VARIANT
*
         LA    R1,WORKA09        * Point R1 to parameter area
         LA    R15,AVLNODE_root  * Get ptr to ptr to AVLNODE
         ST    R15,0(,R1)        * Set ptr to ptr to AVLNODE as parm 1
         MVC   4(4,R1),PARMNAMEA09 * Set node name ptr as parm 2
         MVC   8(4,R1),PARMVALUEA09 * Set node value ptr as parm 3
         OI    8(R1),X'80'       * Indicate last parm
         L     R15,AVL#99A_A09   * Get AVL#99 entry point
         BASR  R14,R15           * Go for it
*
         LTR   R15,R15
         IF (Z) THEN
            ASI   nodeCount,1    * Increate node count
*
            XR    R2,R2
         ELSE
            LA    R2,12
         ENDIF
*
AVL#09_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#99A_A09                  DC    A(AVL#99)
*
WORKDSAA09                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA09                      DS    3A
*
PARMNAMEA09                  DS    A
PARMVALUEA09                 DS    A
*
WORKDSAA09_SIZ               EQU   *-WORKDSAA09
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Update
*
AVL#10   CEEENTRY AUTO=WORKDSAA10_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA10,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA10,4(R1) * Parm 2 is new node name ptr
*
         MVC   PARMVALUEA10,8(R1) * Parm 3 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRA10,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKA10        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA10 * Set search name ptr as parm 2
         LA    R15,AVLNODEPTRA10 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AVL#94A_A10   * Get AVL#94 entry point
         BASR  R14,R15           * Go for it
*
         LTR   R15,R15
         IF (NZ) THEN
*
*           If recursive Query found a node
            LT    R15,AVLNODEPTRA10
            IF (NZ) THEN
*              Copy the found node's value
               LA    R1,WORKA10  * Point R1 to parameter area
               MVC   0(4,R1),PARMVALUEA10 * Set ptr to VRNTTGT as par 1
               LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Ptr to src value
               ST    R15,4(,R1)  * Set ptr to VRNTSRC pas par 2
               L     R15,LWZMVCPA_A10 * Get LWZMVCP entry point
               BASR  R14,R15     * Go for it
*
               LA    R2,1
            ELSE
               XR    R2,R2
            ENDIF
         ELSE
            LA    R2,12
         ENDIF
*
AVL#10_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A10                  DC    A(AVL#94)  QueryAVLNODE
LWZMVCPA_A10                 DC    V(LWZMVCP)
*
WORKDSAA10                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA10                      DS    4A
*
PARMNAMEA10                  DS    A
PARMVALUEA10                 DS    A
*
AVLNODEPTRA10                DS    A
*
WORKDSAA10_SIZ               EQU   *-WORKDSAA10
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Delete
*
AVL#11   CEEENTRY AUTO=WORKDSAA11_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA11,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA11,4(R1) * Parm 2 is search name ptr
*
*        Call recursive Delete function
         LA    R1,WORKA11        * Point R1 to parameter area
         LA    R15,AVLNODE_root  * Get ptr to AVLNODE_root
         ST    R15,0(,R1)        * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA11 * Set search name ptr as parm 2
         L     R15,AVL#98A_A11   * Get AVL#98 entry point
         BASR  R14,R15           * Go for it
*
         IF (C,R15,EQ,=A(1)) THEN
            L     R15,nodeCount
            BCTR  R15,R0         * Decrease node count
            ST    R15,nodeCount
*
            XR    R2,R2
         ELSE
            LA    R2,12
         ENDIF
*
AVL#11_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#98A_A11                  DC    A(AVL#98)
*
WORKDSAA11                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA11                      DS    3A
*
PARMNAMEA11                  DS    A
*
FOUNDA11                     DS    F
*
WORKDSAA11_SIZ               EQU   *-WORKDSAA11
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Contains
*
AVL#12   CEEENTRY AUTO=WORKDSAA12_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA12,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA12,4(R1) * Parm 2 is search name ptr
*
*        Call recursive Query function
         MVC   AVLNODEPTRA12,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKA12        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA12 * Set search name ptr as parm 2
         LA    R15,AVLNODEPTRA12 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AVL#94A_A12   * Get AVL#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRA12,EQ,=A(0)) THEN
            LA    R2,1
         ELSE
            XR    R2,R2
         ENDIF
*
AVL#12_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A12                  DC    A(AVL#94)
*
WORKDSAA12                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA12                      DS    4A
*
PARMNAMEA12                  DS    A
*
AVLNODEPTRA12                DS    A
*
WORKDSAA12_SIZ               EQU   *-WORKDSAA12
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL Put
*
AVL#13   CEEENTRY AUTO=WORKDSAA13_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA13,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMNAMEA13,4(R1) * Parm 2 is new node name ptr
*
         MVC   PARMVALUEA13,8(R1) * Parm 3 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRA13,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKA13        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMNAMEA13 * Set search name ptr as parm 2
         LA    R15,AVLNODEPTRA13 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AVL#94A_A13   * Get AVL#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRA13,EQ,=A(0)) THEN
*
            LA    R1,WORKA13     * Point R1 to parameter area
            LA    R15,AVLNODE_root * Get ptr to ptr to AVLNODE
            ST    R15,0(,R1)     * Set ptr to ptr to AVLNODE as parm 1
            MVC   4(4,R1),PARMNAMEA13 * Set node name ptr as parm 2
            MVC   8(4,R1),PARMVALUEA13 * Set node value ptr as parm 3
            OI    8(R1),X'80'    * Indicate last parm
            L     R15,AVL#99A_A13 * Get AVL#99 entry point
            BASR  R14,R15        * Go for it
*
            LTR   R15,R15
            IF (Z) THEN
               ASI   nodeCount,1 * Increate node count
*
               XR    R2,R2
            ELSE
               LA    R2,12
            ENDIF
         ELSE
*           Copy the found node's value
            LA    R1,WORKA13     * Point R1 to parameter area
            MVC   0(4,R1),PARMVALUEA13 * Set ptr to VRNTSRC as par 1
            L     R15,AVLNODEPTRA13 * Get returned node
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Ptr to tgt value
            ST    R15,4(,R1)     * Set ptr to VRNTTGT pas par 2
            L     R15,LWZMVCPA_A13 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            XR    R2,R2
         ENDIF
*
AVL#13_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#94A_A13                  DC    A(AVL#94)
AVL#99A_A13                  DC    A(AVL#99)
LWZMVCPA_A13                 DC    V(LWZMVCP)
*
WORKDSAA13                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA13                      DS    4A
*
PARMNAMEA13                  DS    A
PARMVALUEA13                 DS    A
*
AVLNODEPTRA13                DS    A
*
WORKDSAA13_SIZ               EQU   *-WORKDSAA13
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL QueryAVLNODE
*
AVL#94   CEEENTRY AUTO=WORKDSAA94_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA94,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AVL_obj,R8        * Address object DSECT
*
         L     R7,0(,R1)         * Parm 1 ptr to root node
         L     R6,4(,R1)         * Parm 2 query name
         L     R5,8(,R1)         * Parm 3 ptr to ptr to return node
         L     R4,12(,R1)        * Parm 4 exact match (0 / 1)
*
*        If we've reached an empty node ptr
         LTR   R7,R7
         IF (Z) THEN
*           If exact match (R4 /= 0)
            LTR   R4,R4
            IF (NZ) THEN
               MVC   0(4,R5),=A(0) * Set return node ptr to NULL
            ENDIF
*
*        If we haven't reached an empty node yet
         ELSE
            USING AVLNODE,R7     * Addressability of node
*
*           Compare query name with current node's name
            XR    R0,R0          * Stop at zero term
            L     R2,AVLNODE_NAME * Point to node's name
            LTR   R3,R6          * Copy query name
            IF (Z) THEN
               LA    R3,EMPTYNAMEA94 * No name provided
            ENDIF
            CLST  R2,R3          * Compare names
            BC    1,*-4          * Compare incomplete, try again
            SELECT
            WHEN CC=8
               MVI   EQUALA94,X'00' * Strings are equal
            WHEN CC=4
               MVI   EQUALA94,X'01' * Query name greater
            WHEN CC=2
               MVI   EQUALA94,X'FF' * Query name lesser
            ENDSEL
*
*           If don't perform exact match (R4 == 0)
            LTR   R4,R4
            IF (Z) THEN
*              If names match exactly
               IF (CLI,EQUALA94,EQ,X'00') THEN
*                 Skip this name and continue with the next
                  MVI   EQUALA94,X'01'
*
*              If Query name's lesser
               ELSEIF (CLI,EQUALA94,EQ,X'FF') THEN
*                 Save this node as return node, it might be the
*                 next alphabetical value, if not it will be
*                 overwritten in the next recursive call
                  ST    R7,0(,R5) * Save in return node
               ENDIF
            ENDIF
*
*           If Query name and current node name are unequal
            IF (CLI,EQUALA94,NE,X'00') THEN
*
*              If Query name was lesser
               IF (CLI,EQUALA94,EQ,X'FF') THEN
                  L     R14,AVLNODE_KID   * Use left kid as next node
*
*              If Query name was greater
               ELSE
                  L     R14,AVLNODE_KID+4 * Use right kid as next node
               ENDIF
*
               LA    R1,WORKA94  * Address parameter area
               ST    R14,0(,R1)  * root node ptr
               ST    R6,4(,R1)   * query name
               ST    R5,8(,R1)   * ptr to ptr to return node
               ST    R4,12(,R1)  * exact match
               LA    R15,AVL#94  * Get entry point
               BASR  R14,R15     * Recursive call to same routine
*
*           If Query name and current node name are equal
            ELSE
               ST    R7,0(,R5)   * Save in return node
            ENDIF
         ENDIF
*
         IF (CLC,0(4,R5),NE,=A(0)) THEN
            LA    R2,1
         ELSE
            XR    R2,R2
         ENDIF
*
AVL#94_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
EMPTYNAMEA94                 DC    X'00'
*
WORKDSAA94                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA94                      DS    4A
EQUALA94                     DS    C
*
WORKDSAA94_SIZ               EQU   *-WORKDSAA94
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL DisposeAVLNODE
*
AVL#95   CEEENTRY AUTO=WORKDSAA95_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA95,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AVL_obj,R8        * Address object DSECT
*
         L     R7,0(,R1)         * Parm 1 ptr to ptr to node
         L     R6,0(,R7)         * Get ptr to node
         USING AVLNODE,R6
*
         LT    R5,AVLNODE_KID    * Test left kid
         IF (NZ) THEN            * If not NULL
            LA    R1,WORKA95     * Address parameter area
            LA    R15,AVLNODE_KID * Get ptr to ptr to left kid
            ST    R15,0(,R1)     * Set ptr to ptr to left kid as parm 1
            LA    R15,AVL#95     * Get entry point
            BASR  R14,R15        * Recursive call to same routine
         ENDIF
*
         LT    R5,AVLNODE_KID+4  * Test right kid
         IF (NZ) THEN            * If not NULL
            LA    R1,WORKA95     * Address parameter area
            LA    R15,AVLNODE_KID+4 * Get ptr to ptr to right kid
            ST    R15,0(,R1)     * Set ptr to ptr to right kid parm 1
            LA    R15,AVL#95     * Get entry point
            BASR  R14,R15        * Recursive call to same routine
         ENDIF
*
*        Now that all child nodes have been disposed, free the node's
*        name and value before freeing the node itself
         MVC   AVLNODE_NAME,=A(0)
*
*        CALL  CEEFRST,(AVLNODE_NAME,FCA95),MF=(E,WORKA95)
*
         LA    R4,AVLNODE_VALUE  * Point R4 to value VARIANT
         IF (CLC,vt-VARIANT(4,R4),GE,=A(64)) THEN
            MVC   value-VARIANT(4,R4),=A(0)
*
*           CALL  CEEFRST,(value-VARIANT(R4),FCA95),MF=(E,WORKA95)
         ELSEIF (CLC,vt-VARIANT(4,R4),EQ,=A(VT_UNKNOWN)) THEN
            L     R15,value-VARIANT(,R4)
            LA    R1,WORKA95
            ST    R15,0(,R1)
            L     R15,0(,R15)
            L     R15,8(,R15)
            BASR  R14,R15
         ENDIF
*
         MVC   0(4,R6),=A(0)
*
*        CALL  CEEFRST,(ADDRA95,FCA95),MF=(E,WORKA95)
*
AVL#95_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAA95                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA95                      DS    2A
*
WORKDSAA95_SIZ               EQU   *-WORKDSAA95
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL RightRotate
*
*         Y *               X *
*          / \               / \
*         /   \             /   \
*      X *     * T3     T1 *     * Y
*       / \                     / \
*      /   \                   /   \
*  T1 *     * T2           T2 *     * T3
*
AVL#96   CEEENTRY AUTO=WORKDSAA96_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA96,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AVL_obj,R8        * Address object DSECT
*
         L     R15,0(,R1)        * Parm 1 ptr to node to rotate
         L     R7,0(,R15)        * Point R7 to root node
NODE_Y   USING AVLNODE,R7        * Addressability of node Y
         L     R6,NODE_Y.AVLNODE_KID * Point R6 to left kid of Y
NODE_X   USING AVLNODE,R6        * Addressability of node X
         L     R5,NODE_X.AVLNODE_KID+4 * Point R5 to right kid of X
NODE_T2  USING AVLNODE,R5        * Addressability of node T2
*
*        Do the rotate
         ST    R7,NODE_X.AVLNODE_KID+4 * Make node Y the right kid of X
         ST    R5,NODE_Y.AVLNODE_KID * Make T2 the left kid of Y
         LTR   R5,R5
         IF (NZ) THEN            * If T2 node is not NULL
            ST    R7,NODE_T2.AVLNODE_PARENT * Make Y the parent of T2
         ENDIF
*        Make X the top node by giving it Y's parent
         MVC   NODE_X.AVLNODE_PARENT,NODE_Y.AVLNODE_PARENT
         ST    R6,NODE_Y.AVLNODE_PARENT * Make X the parent of Y
*
*        Recalculate Y node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_Y.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_Y.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_Y.AVLNODE_HEIGHT * Store Y node's height
*
*        Recalculate X node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_X.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_X.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_X.AVLNODE_HEIGHT * Store X node's height
*
*        Return X node (which is now in Y's place
         ST    R6,0(,R15)        * Put ptr to node X in parm 1
*
AVL#96_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAA96                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKDSAA96_SIZ               EQU   *-WORKDSAA96
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL LeftRotate
*
*      X *                     Y *
*       / \                     / \
*      /   \                   /   \
*  T1 *     * Y             X *     * T3
*          / \               / \
*         /   \             /   \
*     T2 *     * T3     T1 *     * T2
*
AVL#97   CEEENTRY AUTO=WORKDSAA97_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA97,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AVL_obj,R8        * Address object DSECT
*
         L     R15,0(,R1)        * Parm 1 ptr to node to rotate
         L     R7,0(,R15)        * Point R7 to root node
NODE_X   USING AVLNODE,R7        * Addressability of node X
         L     R6,NODE_X.AVLNODE_KID+4 * Point R6 to right kid of X
NODE_Y   USING AVLNODE,R6        * Addressability of node Y
         L     R5,NODE_Y.AVLNODE_KID * Point R5 to left kid of Y
NODE_T2  USING AVLNODE,R5        * Addressability of node T2
*
*        Do the rotate
         ST    R7,NODE_Y.AVLNODE_KID * Make node X the left kid of Y
         ST    R5,NODE_X.AVLNODE_KID+4 * Make T2 the right kid of X
         LTR   R5,R5
         IF (NZ) THEN            * If T2 node is not NULL
            ST    R7,NODE_T2.AVLNODE_PARENT * Make X the parent of T2
         ENDIF
*        Make Y the top node by giving it X's parent
         MVC   NODE_Y.AVLNODE_PARENT,NODE_X.AVLNODE_PARENT
         ST    R6,NODE_X.AVLNODE_PARENT * Make Y the parent of X
*
*        Recalculate X node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_X.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_X.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_X.AVLNODE_HEIGHT * Store Y node's height
*
*        Recalculate Y node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_Y.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_Y.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_Y.AVLNODE_HEIGHT * Store X node's height
*
*        Return Y node (which is now in X's place
         ST    R6,0(,R15)        * Put ptr to node Y in parm 1
*
AVL#97_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAA97                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKDSAA97_SIZ               EQU   *-WORKDSAA97
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL DeleteNode
*
AVL#98   CEEENTRY AUTO=WORKDSAA98_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA98,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMROOTPTRA98,0(R1) * Parm 1 is ptr to root node ptr
         MVC   PARMNODENAMEA98,4(R1) * Parm 2 is node name
*
         MVC   RETCODEA98,=A(0)  * Preset retcode to not found
*
         L     R5,PARMROOTPTRA98 * Ptr to ptr to root node
         L     R5,0(,R5)         * Ptr to node
         USING AVLNODE,R5        * Addressability of AVL node
*
*        Compare Delete name with current node's name
         XR    R0,R0             * Stop at zero term
         L     R2,AVLNODE_NAME   * Point to current node's name
         L     R3,PARMNODENAMEA98 * Delete name
         CLST  R2,R3             * Compare names
         BC    1,*-4             * Compare incomplete, try again
         SELECT
         WHEN CC=8
            MVI   EQUALA98,X'00' * Strings are equal
         WHEN CC=4
            MVI   EQUALA98,X'01' * Query name greater
         WHEN CC=2
            MVI   EQUALA98,X'FF' * Query name lesser
         ENDSEL
*
*        If Delete name and current node name are unequal
         IF (CLI,EQUALA98,NE,X'00') THEN
*
*           If Delete name was lesser
            IF (CLI,EQUALA98,EQ,X'FF') THEN
               LA    R14,AVLNODE_KID * Use left kid as next node
            ELSE
               LA    R14,AVLNODE_KID+4 * Use right kid as next node
            ENDIF
*
            LA    R1,WORKA98     * Address parameter area
            ST    R14,0(,R1)     * Parm 1 ptr to next root node ptr
            MVC   4(4,R1),PARMNODENAMEA98 * Parm 2 is node name
            LA    R15,AVL#98     * Get entry point
            BASR  R14,R15        * Recursive call to same routine
*
            ST    R15,RETCODEA98 * Tricle down return code recursively
*
*        If Delete name and current node name are equal
         ELSE
*
            MVC   RETCODEA98,=A(1)
*
*           If either or both kids are NULL
            IF (CLC,AVLNODE_KID(4),EQ,=A(0)),OR,                       X
               (CLC,AVLNODE_KID+4(4),EQ,=A(0)) THEN
               LT    R4,AVLNODE_KID * Check left kid NULL
               IF (Z) THEN
                  LT    R4,AVLNODE_KID+4 * Check right kid NULL
               ENDIF
               IF (Z) THEN
*                 End up here if both kids are NULL
                  LR    R4,R5    * Save node ptr in R4
                  XR    R5,R5    * Zero out current node reg
                  L     R14,PARMROOTPTRA98 * Get root node ptr
                  MVC   0(4,R14),=A(0) * Also zero out ptr 2 ptr 2 node
               ELSE
*                 End up here if just one kid is not NULL
                  MVC   AVLNODE_PARENT-AVLNODE(4,R4),AVLNODE_PARENT
                  L     R14,PARMROOTPTRA98 * Get root node ptr
                  ST    R4,0(,R14) * Kid becomes the parent
                  LR    R4,R5    * Save node ptr in R4
                  L     R5,0(,R14) * Put kid ptr in curr node reg
               ENDIF
*
               IF (C,R4,EQ,firstNode) THEN
                  MVC   firstNode,AVLNODE_PARENT
               ENDIF
*
               IF (C,R4,EQ,lastNode) THEN
                  MVC   lastNode,AVLNODE_PARENT
               ENDIF
*
*              Dispose of node, but first make sure only the node, not
*              recursively go through children
               MVC   AVLNODE_KID-AVLNODE(4,R4),=A(0)
               MVC   AVLNODE_KID-AVLNODE+4(4,R4),=A(0)
*
               LA    R1,WORKA98  * Address parameter area
               ST    R4,ADDRA98  * Save node ptr in local var
               LA    R15,ADDRA98 * Get ptr to node ptr
               ST    R15,0(,R1)  * Set ptr to node ptr as parm 1
               MVC   4(4,R1),PARMNODENAMEA98 * Parm 2 is node name
               L     R15,AVL#95A_A98 * Get dispose entry point
               BASR  R14,R15     * Go for it
*
*           If both kids are not NULL
            ELSE
*
*              Find the left most kid anywhere below this node's
*              right kid (so find the next ordered node)
               L     R4,AVLNODE_KID+4 * Get the right kid
               LT    R3,AVLNODE_KID-AVLNODE(,R4) * Get left kid
               DO WHILE=(NZ)
                  LR    R4,R3    * Switch to grandkid
                  LT    R3,AVLNODE_KID-AVLNODE(,R4) * Get left kid
               ENDDO
*
*              Replace this node's name and value by the one we found
*              to be the next in alphab. order
               L     R15,AVLNODE_NAME
               MVC   AVLNODE_NAME,AVLNODE_NAME-AVLNODE(R4)
               ST    R15,AVLNODE_NAME-AVLNODE(,R4)
               MVC   SAVEVARA98,AVLNODE_VALUE
               MVC   AVLNODE_VALUE,AVLNODE_VALUE-AVLNODE(R4)
               MVC   AVLNODE_VALUE-AVLNODE(VARIANT_SIZ,R4),SAVEVARA98
*
*              Now, instead of deleting this node, delete the one who's
*              name and value we copied, start searching from right kid
               LA    R1,WORKA98  * Address parameter area
               LA    R15,AVLNODE_KID+4 * Get ptr to right kid
               ST    R15,0(,R1)  * ptr to right kid node ptr is parm 1
               MVC   4(4,R1),AVLNODE_NAME-AVLNODE(R4)
               LA    R15,AVL#98  * Get dispose entry point
               BASR  R14,R15     * Go for it
            ENDIF
         ENDIF
*
*        If there's no node left to balance, get out
         LTR   R5,R5
         BZ    AVL#98_RET
*
*        Recalculate this node's height
         LA    R4,1              * R4 = 1
         LT    R2,AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,AVLNODE_HEIGHT * Store Y node's height
*
*        Get difference between left kid's height and right kid's
         SR    R2,R3
*
*        If difference larger than +1, it's either a left left or
*        a left right scenario
         IF (C,R2,GT,=F'1') THEN
*
*           Get the left kid's grandkid's height
            L     R4,AVLNODE_KID * Get left kid
            LT    R2,AVLNODE_KID-AVLNODE(,R4) * Get left grandkid
            IF (NZ) THEN
               L     R2,AVLNODE_HEIGHT-AVLNODE(,R2)
            ENDIF
            LT    R3,AVLNODE_KID+4-AVLNODE(,R4) * Get right grandkid
            IF (NZ) THEN
               L     R3,AVLNODE_HEIGHT-AVLNODE(,R3)
            ENDIF
*
*           Balance the left kid
            SR    R2,R3
*
*           If difference >= 0
            IF (NM) THEN
*              Left left ...
*
*              ... so do a right rotate
               LA    R1,WORKA98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRA98 * Root node ptr ptr parm 1
               L     R15,AVL#96A_A98 * Get right rotate entry point
               BASR  R14,R15     * Go for it
*
*           If difference < 0
            ELSE
*              Left right ...
*
*              ... so do a left rotate ...
               LA    R1,WORKA98  * Address parameter area
               LA    R2,AVLNODE_KID * Get left kid ptr ptr
               ST    R2,0(,R1)   * Set left kid ptr ptr as parm 1
               L     R15,AVL#97A_A98 * Get left rotate entry point
               BASR  R14,R15     * Go for it
*
*              ... followed by a right rotate
               LA    R1,WORKA98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRA98 * Root node ptr ptr parm 1
               L     R15,AVL#96A_A98 * Get right rotate entry point
               BASR  R14,R15     * Go for it
            ENDIF
*
*        If difference smaller than -1, it's either a right right or
*        a right left scenario
         ELSEIF (C,R2,LT,=F'-1') THEN
*
*           Get the right kid's grandkid's heights
            L     R4,AVLNODE_KID+4 * Get the right kid
            LT    R2,AVLNODE_KID-AVLNODE(,R4) * Get left grandkid
            IF (NZ) THEN
               L     R2,AVLNODE_HEIGHT-AVLNODE(,R2)
            ENDIF
            LT    R3,AVLNODE_KID+4-AVLNODE(,R4) * Get right grandkid
            IF (NZ) THEN
               L     R3,AVLNODE_HEIGHT-AVLNODE(,R3)
            ENDIF
*
*           Balance the right kid
            SR    R2,R3
*
*           If difference <= 0
            IF (NP) THEN
*              Right right ...
*
*              ... so do a left rotate
               LA    R1,WORKA98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRA98 * Root node ptr ptr parm 1
               L     R15,AVL#97A_A98 * Get left rotate entry point
               BASR  R14,R15     * Go for it
*
*           If difference > 0
            ELSE
*              Right left ...
*
*              ... so do a right rotate ...
               LA    R1,WORKA98  * Address parameter area
               LA    R2,AVLNODE_KID+4 * Get right kid ptr ptr
               ST    R2,0(,R1)   * Set right kid ptr ptr as parm 1
               L     R15,AVL#96A_A98 * Get right rotate entry point
               BASR  R14,R15     * Go for it
*
*              ... followed by a left rotate
               LA    R1,WORKA98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRA98 * Root node ptr ptr parm 1
               L     R15,AVL#97A_A98 * Get left rotate entry point
               BASR  R14,R15     * Go for it
            ENDIF
         ENDIF
*
AVL#98_RET EQU   *
         L     R2,RETCODEA98
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AVL#95A_A98                  DC    A(AVL#95)
AVL#96A_A98                  DC    A(AVL#96)
AVL#97A_A98                  DC    A(AVL#97)
*
WORKDSAA98                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA98                      DS    3A
RETCODEA98                   DS    F
*
PARMROOTPTRA98               DS    A
PARMNODENAMEA98              DS    A
*
ADDRA98                      DS    A
EQUALA98                     DS    C
SAVEVARA98                   DS    CL(VARIANT_SIZ)
*
WORKDSAA98_SIZ               EQU   *-WORKDSAA98
*
LWZMAVL  CSECT
*
         DROP
*
* IAVL InsertNode
*
AVL#99   CEEENTRY AUTO=WORKDSAA99_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAA99,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AVL_obj,R8        * Address object DSECT
*
         MVC   PARMROOTPTRA99,0(R1) * Parm 1 ptr to ptr to root node
         MVC   PARMNODENAMEA99,4(R1) * Parm 2 ptr to new node name
         IF (TM,8(R1),X'80',Z) THEN
            MVC   PARMNODEVALUEA99,8(R1) * Parm 3 VARIANT* node value
            MVC   PARMNODEPARENTA99,12(R1) * Parm 4 parent node ptr
            L     R15,16(,R1)    * Parm 5 calling AVL#99's EQUALA99
            NILH  R15,X'7FFF'    * Turn off high order bit
            ST    R15,PARMPREVEQUALA99 * Save in local var
            MVI   0(R15),X'00'   * Initialize to equal
         ELSE
            L     R15,8(,R1)     * Parm 3 VARIANT* node value
            NILH  R15,X'7FFF'    * Turn off high order bit
            ST    R15,PARMNODEVALUEA99 * Save in local var
            MVC   PARMNODEPARENTA99,=A(0) * No parent node
            MVC   PARMPREVEQUALA99,=A(0) * No previous EQUALA99
         ENDIF
*
         L     R14,PARMROOTPTRA99 * Get ptr to ptr to root node
         LT    R14,0(,R14)       * Check for NULL root node
*
*        If current root node is NULL
         IF (Z) THEN
*
*           Allocate memory for new node
            LA    R1,WORKA99
            MVC   0(4,R1),=A(AVLNODE_SIZ)
            MVC   4(4,R1),PARMROOTPTRA99
            L     R15,G_GTST
            BASR  R14,R15
*
            L     R7,PARMROOTPTRA99
            L     R7,0(,R7)
            USING AVLNODE,R7
*
*           Initialize new node with nulls
            MVI   0(R7),X'00'
            MVC   1(AVLNODE_SIZ-1,R7),0(R7)
*
*           Allocate memory for String037 node name
            XR    R0,R0          * Search for terminating zero
            L     R1,PARMNODENAMEA99 * Point R1 to zterm node name
            LA    R2,4095(,R1)   * Search for max 4K
            SRST  R2,R1          * Scan for zero terminator
            BRC   1,*-4          * Scan was incomplete, try again
            BRC   2,*-12         * Not found, try another 4K
*
            L     R1,PARMNODENAMEA99 * Point R3 to zterm node name
            SR    R2,R1          * Calculate length
            LA    R2,1(,R2)      * Add 1 for zero term
            ST    R2,G_GTSTSIZ   * And save it for GTST
*
            LA    R1,WORKA99
            ST    R2,0(,R1)
            LA    R15,AVLNODE_NAME
            ST    R15,4(,R1)
            L     R15,G_GTST
            BASR  R14,R15
*
*           Copy parm provided name to new node name
            L     R4,AVLNODE_NAME
            L     R2,PARMNODENAMEA99
            L     R3,G_GTSTSIZ
            LR    R5,R3
            MVCL  R4,R2
*
*           Copy parm provided value to new node value
            LA    R1,WORKA99
            MVC   0(4,R1),PARMNODEVALUEA99
            LA    R15,AVLNODE_VALUE
            ST    R15,4(,R1)
            L     R15,LWZMVCPA_A99
            BASR  R14,R15
*
*           Initialize rest of node properties
            MVC   AVLNODE_HEIGHT,=A(1)
            MVC   AVLNODE_PARENT,PARMNODEPARENTA99
*
            LT    R5,firstNode
            IF (Z) THEN
               LR    R5,R7
               ST    R5,firstNode
            ENDIF
*
            LT    R5,lastNode
            IF (NZ) THEN
               ST    R7,AVLNODE_NEXT-AVLNODE(,R5)
               ST    R5,AVLNODE_PREV
            ENDIF
*
            ST    R7,lastNode
*
            DROP  R7
*
*        If current root node is not NULL
         ELSE
            L     R5,PARMROOTPTRA99
            L     R5,0(,R5)
            USING AVLNODE,R5
*
*           Compare insert name and current node's name
            XR    R0,R0          * Stop at zero term
            L     R2,AVLNODE_NAME * Point to current node's name
            L     R3,PARMNODENAMEA99 * Delete name
            CLST  R2,R3          * Compare names
            BC    1,*-4          * Compare incomplete, try again
            SELECT
            WHEN CC=8
               ILOG_Write OBJECT=G_ILOG,WORK=WORKA99,LINE=MAK104E_A99, X
               LOGLEVEL=LOG_LEVEL_ERROR
*
               CALL  CEE3ABD,(=A(1008),=A(3)),MF=(E,WORKA99)
            WHEN CC=4
               MVI   EQUALA99,X'01' * Query name greater
            WHEN CC=2
               MVI   EQUALA99,X'FF' * Query name lesser
            ENDSEL
*
*           If room was provided for a 'previous' EQUALA99
            LT    R14,PARMPREVEQUALA99
            IF (NZ) THEN
               MVC   0(1,R14),EQUALA99 * Copy this EQUALA99 to caller's
            ENDIF
*
            LA    R1,WORKA99
            IF (CLI,EQUALA99,EQ,X'FF') THEN
               LA    R14,AVLNODE_KID   * Use left kid as next node
            ELSE
               LA    R14,AVLNODE_KID+4 * Use right kid as next node
            ENDIF
            ST    R14,0(,R1)     * Kid becomes next root node parm 1
            MVC   4(4,R1),PARMNODENAMEA99 * Same node name as parm 2
            MVC   8(4,R1),PARMNODEVALUEA99 * Same node value as parm 3
            ST    R5,12(,R1)     * This node becomes parent parm 4
            LA    R14,PREVEQUALA99
            ST    R14,16(,R1)    * EQUALA99 as parm 5
            OI    16(R1),X'80'   * And flag it as last parm
            LA    R15,AVL#99     * Get AVL#99 entry point
            BASR  R14,R15        * Recursive call to same routing
*
            LTR   R15,R15
            BNE   AVL#99_RET
*
*           (one of) the recursive call(s) created the new node
*           The code beyond this point is 'on the way out'
*
*           Recalculate this node's height
            LA    R4,1           * R4 = 1
            LT    R2,AVLNODE_KID
            IF (NZ) THEN         * If left kid is not NULL
               L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid hght
            ENDIF
            LT    R3,AVLNODE_KID+4
            IF (NZ) THEN         * If right kid is not NULL
               L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid hgt
            ENDIF
            CR    R2,R3          * See if left or right has most height
            IF (GE) THEN         * If left kid
               AR    R4,R2       * Add left kid's height
            ELSE                 * If right kid
               AR    R4,R3       * Add right kid's height
            ENDIF
            ST    R4,AVLNODE_HEIGHT * Store Y node's height
*
*           Get difference between left kid's height and right kid's
            SR    R2,R3
*
*           If difference larger than +1, it's either a left left or
*           a left right scenario
            IF (C,R2,GT,=F'1') THEN
*
*              Remember, 'on the way out', so previous EQUALA99 is
*              really the last kid's comparison result
*
*              If Insert name was lesser than kid's name
               IF (CLI,PREVEQUALA99,EQ,X'FF') THEN
*                 Left left ...
*
*                 ... so do a right rotate
                  LA    R1,WORKA99 * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRA99 * Root node pt pt parm 1
                  L     R15,AVL#96A_A99 * Get right rotate entry point
                  BASR  R14,R15  * Go for it
*
*              If Insert name was greater that kid's name
               ELSE
*                 Left right ...
*
*                 ... so do a left rotate ...
                  LA    R1,WORKA99 * Address parameter area
                  LA    R2,AVLNODE_KID * Get left kid ptr ptr
                  ST    R2,0(,R1) * Set left kid ptr ptr as parm 1
                  L     R15,AVL#97A_A99 * Get left rotate entry point
                  BASR  R14,R15  * Go for it
*
*                 ... followed by a right rotate
                  LA    R1,WORKA99 * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRA99 * Root node pt pt par 1
                  L     R15,AVL#96A_A99 * Get right rotate entry point
                  BASR  R14,R15     * Go for it
               ENDIF
*
*           If difference smaller than -1, it's either a right right or
*           a right left scenario
            ELSEIF (C,R2,LT,=F'-1') THEN
*
*              Remember, 'on the way out', so previous EQUALA99 is
*              really the last kid's comparison result
*
*              If Insert name was greater than kid's name
               IF (CLI,PREVEQUALA99,EQ,X'01') THEN
*                 Right right ...
*
*                 ... so do a left rotate
                  LA    R1,WORKA99 * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRA99 * Root node pt pt parm 1
                  L     R15,AVL#97A_A99 * Get left rotate entry point
                  BASR  R14,R15  * Go for it
*
*              If Insert name was lesser than kid's name
               ELSE
*                 Right left ...
*
*                 ... so do a right rotate ...
                  LA    R1,WORKA99 * Address parameter area
                  LA    R2,AVLNODE_KID+4 * Get right kid ptr ptr
                  ST    R2,0(,R1) * Set right kid ptr ptr as parm 1
                  L     R15,AVL#96A_A99 * Get right rotate entry point
                  BASR  R14,R15  * Go for it
*
*                 ... followed by a left rotate
                  LA    R1,WORKA99  * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRA99 * Root node pt pt parm 1
                  L     R15,AVL#97A_A99 * Get left rotate entry point
                  BASR  R14,R15     * Go for it
               ENDIF
            ENDIF
*
            DROP  R5
         ENDIF
*
AVL#99_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK104E_A99                  DC    C'MAK104E Duplicate entry not alloweX
               d',X'00'
*
                             DS    0F
AVL#96A_A99                  DC    A(AVL#96)
AVL#97A_A99                  DC    A(AVL#97)
LWZMVCPA_A99                 DC    V(LWZMVCP)
*
WORKDSAA99                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKA99                      DS    5A
*
PARMROOTPTRA99               DS    A
PARMNODENAMEA99              DS    A
PARMNODEVALUEA99             DS    A
PARMNODEPARENTA99            DS    A
PARMPREVEQUALA99             DS    A
*
EQUALA99                     DS    C
PREVEQUALA99                 DS    C
*
WORKDSAA99_SIZ               EQU   *-WORKDSAA99
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 QueryInterface
*
AV2#01   MQRYIFCE SUF=B01,IFACE=IAV2
*
* IAV2 AddRef
*
AV2#02   MADDREF
*
* IAV2 Release
*
AV2#03   CEEENTRY AUTO=WORKDSAB03_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSAB03,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    AV2#03_RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            DROP  R6
            USING AV2_obj,R6
*
            LA    R15,AVLNODE_root
            IF (CLC,0(4,R15),NE,=A(0)) THEN
               LA    R1,WORKB03
               ST    R15,0(,R1)
               L     R15,AV2#95A_B03
               BASR  R14,R15
            ENDIF
*
            DROP  R6
            USING COM_obj,R6
*
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTRB03   * Put ptr in variable
*           CALL  CEEFRST,(OBJPTRB03,FCB03),MF=(E,WORKB03)
*
            L     R15,G_OBJCOUNT
            BCTR  R15,R0
            ST    R15,G_OBJCOUNT
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R6,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKB03
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKB03,      X
               ZSTR=MAK502D_AV2
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKB03,      X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORKB03,LINE=0(,R2),      X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
         ENDIF
*
AV2#03_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
AV2#95A_B03                  DC    A(AV2#95)
*
                             DS    0F
MAK502D_AV2                  DC    C'MAK502D Deleted IAV2 object '
                             DC    X'00'
*
WORKDSAB03                   DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTRB03                    DS    A
WORKB03                      DS    3A
*
WORKDSAB03_SIZ               EQU   *-WORKDSAB03
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Count
*
AV2#04   MGETPFW OBJ=AV2,PROP=nodeCount
*
* IAV2 Exists
*
AV2#05   CEEENTRY AUTO=WORKDSAB05_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB05,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB05,4(R1) * Parm 2 is fword index
*
*        Call recursive Query function
         MVC   AVLNODEPTRB05,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKB05        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMINDEXB05 * Set fword as parm 2
         LA    R15,AVLNODEPTRB05 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AV2#94A_B05   * Get AV2#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query found a node
         IF (CLC,AVLNODEPTRB05,NE,=A(0)) THEN
            LA    R2,1
         ELSE
            XR    R2,R2
         ENDIF
*
AV2#05_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#94A_B05                  DC    A(AV2#94)
*
WORKDSAB05                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB05                      DS    4A
*
PARMINDEXB05                 DS    F
*
AVLNODEPTRB05                DS    A
*
WORKDSAB05_SIZ               EQU   *-WORKDSAB05
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Query
*
AV2#06   CEEENTRY AUTO=WORKDSAB06_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB06,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB06,4(R1) * Parm 2 is search index
*
         MVC   PARMVALUEB06,8(R1) * Parm 3 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRB06,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKB06        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMINDEXB06 * Set search index as parm 2
         LA    R15,AVLNODEPTRB06 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AV2#94A_B06   * Get AV2#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRB06,EQ,=A(0)) THEN
*           Set return value VARIANT to VT_NULL (which is not VT_EMPTY)
            L     R4,PARMVALUEB06 * Get ptr to VARIANT
            MVC   vt-VARIANT(4,R4),=A(VT_NULL)
            MVC   value-VARIANT(4,R4),=A(0)
            MVC   value-VARIANT+4(4,R4),=A(0)
*
            XR    R2,R2
*
*        If recursive Query found a node
         ELSE
*           Copy the found node's value
            LA    R1,WORKB06     * Point R1 to parameter area
            L     R15,AVLNODEPTRB06 * Get ptr to AVLNODE
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Get ptr to VRNTSRC
            ST    R15,0(,R1)     * Set ptr to VRNTSRC as parm 1
            MVC   4(4,R1),PARMVALUEB06 * Set ptr to VRNTTGT as parm 2
            L     R15,LWZMVCPA_B06 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            LA    R2,1
         ENDIF
*
AV2#06_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#94A_B06                  DC    A(AV2#94)
LWZMVCPA_B06                 DC    V(LWZMVCP)
*
WORKDSAB06                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB06                      DS    4A
*
PARMINDEXB06                 DS    F
PARMVALUEB06                 DS    A
*
AVLNODEPTRB06                DS    A
*
WORKDSAB06_SIZ               EQU   *-WORKDSAB06
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Next
*
AV2#07   CEEENTRY AUTO=WORKDSAB07_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB07,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB07,4(R1) * Parm 2 is search index
*
         L     R15,8(,R1)        * Parm 3 is return index ptr
         ST    R15,PARMINDEXPTRB07 * Save as local var
         MVC   0(4,R15),=A(0)    * Initialize return index to 0
*
         MVC   PARMVALUEB07,12(R1) * Parm 4 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRB07,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKB07        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMINDEXB07 * Set search index as parm 2
         LA    R15,AVLNODEPTRB07 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(0)    * Set no exact match as parm 4
         L     R15,AV2#94A_B07   * Get AV2#94 entry point
         BASR  R14,R15           * Go for it
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRB07,EQ,=A(0)) THEN
*           Set return value VARIANT to VT_NULL (which is not VT_EMPTY)
            L     R4,PARMVALUEB07 * Get ptr to VARIANT
            MVC   vt-VARIANT(4,R4),=A(VT_NULL)
            MVC   value-VARIANT(4,R4),=A(0)
            MVC   value-VARIANT+4(4,R4),=A(0)
*
            XR    R2,R2
*
*        If recursive Query found a node
         ELSE
*           Copy the found node's index
            L     R14,PARMINDEXPTRB07 * Get return index parameter
            L     R15,AVLNODEPTRB07 * Point R15 to found node
            MVC   0(4,R14),AVLNODE_NAME-AVLNODE(R15) * Copy index
*
*           Copy the found node's value
            LA    R1,WORKB07     * Point R1 to parameter area
            L     R15,AVLNODEPTRB07 * Get ptr to AVLNODE
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Get ptr to VRNTSRC
            ST    R15,0(,R1)     * Set ptr to VRNTSRC as parm 1
            MVC   4(4,R1),PARMVALUEB07 * Set ptr to VRNTTGT as parm 2
            L     R15,LWZMVCPA_B07 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            LA    R2,1
         ENDIF
*
AV2#07_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#94A_B07                  DC    A(AV2#94)
LWZMVCPA_B07                 DC    V(LWZMVCP)
*
WORKDSAB07                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB07                      DS    4A
*
PARMINDEXB07                 DS    F
PARMINDEXPTRB07              DS    A
PARMVALUEB07                 DS    A
*
AVLNODEPTRB07                DS    A
*
WORKDSAB07_SIZ               EQU   *-WORKDSAB07
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Next2
*
AV2#08   CEEENTRY AUTO=WORKDSAB08_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB08,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB08,4(R1) * Parm 2 is search index
*
         L     R15,8(,R1)        * Parm 3 is return index ptr
         ST    R15,PARMINDEXPTRB08 * Save as local var
         MVC   0(4,R15),=A(0)    * Initialize return index to 0
*
         MVC   PARMVALUEB08,12(R1) * Parm 4 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRB08,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKB08        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMINDEXB08 * Set search index as parm 2
         LA    R15,AVLNODEPTRB08 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set no exact match as parm 4
         L     R15,AV2#94A_B08   * Get AV2#94 entry point
         BASR  R14,R15           * Go for it
*
*        If a node was found, copy the next ptr's node
         IF (CLC,AVLNODEPTRB08,NE,=A(0)) THEN
            L     R15,AVLNODEPTRB08 * Address found node
            MVC   AVLNODEPTRB08,AVLNODE_NEXT-AVLNODE(R15) * copy next
         ENDIF
*
*        If recursive Query didn't find a node
         IF (CLC,AVLNODEPTRB08,EQ,=A(0)) THEN
*           Set return value VARIANT to VT_NULL (which is not VT_EMPTY)
            L     R4,PARMVALUEB08 * Get ptr to VARIANT
            MVC   vt-VARIANT(4,R4),=A(VT_NULL)
            MVC   value-VARIANT(4,R4),=A(0)
            MVC   value-VARIANT+4(4,R4),=A(0)
*
            XR    R2,R2
*
*        If recursive Query found a node
         ELSE
*           Copy the found node's index
            L     R14,PARMINDEXPTRB08 * Get return index parameter
            L     R15,AVLNODEPTRB08 * Point R15 to found node
            MVC   0(4,R14),AVLNODE_NAME-AVLNODE(R15) * Copy index
*
*           Copy the found node's value
            LA    R1,WORKB08     * Point R1 to parameter area
            L     R15,AVLNODEPTRB08 * Get ptr to AVLNODE
            LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Get ptr to VRNTSRC
            ST    R15,0(,R1)     * Set ptr to VRNTSRC as parm 1
            MVC   4(4,R1),PARMVALUEB08 * Set ptr to VRNTTGT as parm 2
            L     R15,LWZMVCPA_B08 * Get LWZMVCP entry point
            BASR  R14,R15        * Go for it
*
            LA    R2,1
         ENDIF
*
AV2#08_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#94A_B08                  DC    A(AV2#94)
LWZMVCPA_B08                 DC    V(LWZMVCP)
*
WORKDSAB08                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB08                      DS    4A
*
PARMINDEXB08                 DS    F
PARMINDEXPTRB08              DS    A
PARMVALUEB08                 DS    A
*
AVLNODEPTRB08                DS    A
*
WORKDSAB08_SIZ               EQU   *-WORKDSAB08
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Insert
*
AV2#09   CEEENTRY AUTO=WORKDSAB09_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB09,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB09,4(R1) * Parm 2 is new node index
*
         MVC   PARMVALUEB09,8(R1) * Parm 3 is ptr to VARIANT
*
         LA    R1,WORKB09        * Point R1 to parameter area
         LA    R15,AVLNODE_root  * Get ptr to ptr to AVLNODE
         ST    R15,0(,R1)        * Set ptr to ptr to AVLNODE as parm 1
         MVC   4(4,R1),PARMINDEXB09 * Set node index as parm 2
         MVC   8(4,R1),PARMVALUEB09 * Set node value ptr as parm 3
         OI    8(R1),X'80'       * Flag as last parm
         L     R15,AV2#99A_B09   * Get AV2#99 entry point
         BASR  R14,R15           * Go for it
*
         LTR   R15,R15
         IF (Z) THEN
            ASI   nodeCount,1    * Increate node count
*
            XR    R2,R2
         ELSE
            LA    R2,12
         ENDIF
*
AV2#09_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#99A_B09                  DC    A(AV2#99)
*
WORKDSAB09                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB09                      DS    3A
*
PARMINDEXB09                 DS    F
PARMVALUEB09                 DS    A
*
WORKDSAB09_SIZ               EQU   *-WORKDSAB09
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Update
*
AV2#10   CEEENTRY AUTO=WORKDSAB10_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB10,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB10,4(R1) * Parm 2 is new node index
*
         MVC   PARMVALUEB10,8(R1) * Parm 3 is ptr to VARIANT
*
*        Call recursive Query function
         MVC   AVLNODEPTRB10,=A(0) * Initialize AVLNODEPTR
*
         LA    R1,WORKB10        * Point R1 to parameter area
         MVC   0(4,R1),AVLNODE_root * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMINDEXB10 * Set search index as parm 2
         LA    R15,AVLNODEPTRB10 * Get ptr to AVLNODEPTR
         ST    R15,8(,R1)        * Set ptr to AVLNODEPTR as parm 3
         MVC   12(4,R1),=A(1)    * Set exact match as parm 4
         L     R15,AV2#94A_B10   * Get AV2#94 entry point
         BASR  R14,R15           * Go for it
*
         LTR   R15,R15
         IF (NZ) THEN
*
*           If recursive Query found a node
            LT    R15,AVLNODEPTRB10
            IF (NZ) THEN
*              Copy the found node's value
               LA    R1,WORKB10  * Point R1 to parameter area
               MVC   0(4,R1),PARMVALUEB10 * Set ptr to VRNTTGT as par 1
               LA    R15,AVLNODE_VALUE-AVLNODE(,R15) * Ptr to src value
               ST    R15,4(,R1)  * Set ptr to VRNTSRC as par 2
               L     R15,LWZMVCPA_B10 * Get LWZMVCP entry point
               BASR  R14,R15     * Go for it
*
               LA    R2,1
            ELSE
               XR    R2,R2
            ENDIF
         ELSE
            LA    R2,12
         ENDIF
*
AV2#10_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#94A_B10                  DC    A(AV2#94)
LWZMVCPA_B10                 DC    V(LWZMVCP)
*
WORKDSAB10                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB10                      DS    4A
*
PARMINDEXB10                 DS    F
PARMVALUEB10                 DS    A
*
AVLNODEPTRB10                DS    A
*
WORKDSAB10_SIZ               EQU   *-WORKDSAB10
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Delete
*
AV2#11   CEEENTRY AUTO=WORKDSAB11_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB11,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMINDEXB11,4(R1) * Parm 2 is search index
*
*        Call recursive Delete function
         LA    R1,WORKB11        * Point R1 to parameter area
         LA    R15,AVLNODE_root  * Get ptr to AVLNODE_root
         ST    R15,0(,R1)        * Set ptr to AVLNODE_root as parm 1
         MVC   4(4,R1),PARMINDEXB11 * Set search index as parm 2
         L     R15,AV2#98A_B11   * Get AV2#98 entry point
         BASR  R14,R15           * Go for it
*
         IF (C,R15,EQ,=A(1)) THEN
            L     R15,nodeCount
            BCTR  R15,R0         * Decrease node count
            ST    R15,nodeCount
*
            XR    R2,R2
         ELSE
            LA    R2,12
         ENDIF
*
AV2#11_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#98A_B11                  DC    A(AV2#98)
*
WORKDSAB11                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB11                      DS    3A
*
PARMINDEXB11                 DS    F
*
WORKDSAB11_SIZ               EQU   *-WORKDSAB11
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 Contains
*
AV2#12   CEEENTRY AUTO=WORKDSAB12_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB12,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R8,0(,R1)         * Parm 1 is object ptr
         USING AV2_obj,R8        * Address object DSECT
*
         L     R6,4(,R1)         * Parm 2 is VARIANT ptr
*
         L     R5,8(,R1)         * Parm 3 is ptr to fword index
         ST    R5,PARMINDEXOUTB12 * Save as local var
         MVC   0(,R5),=F'-1'     * Initialize return of -1
*
         L     R7,firstNode      * Start with first node
         USING AVLNODE,R7        * Addressability of AVLNODE
*
         DO WHILE=(C,R7,NE,=A(0),AND,                                  X
               CLC,G_RETCODE,EQ,=A(0))
            IF (CLC,AVLNODE_VALUE(VARIANT_SIZ),EQ,0(R6))
               L     R15,PARMINDEXOUTB12 * Point R15 to return index
               MVC   0(4,R15),AVLNODE_NAME * Set return index
*
               ASMLEAVE
            ENDIF
*
            L     R7,AVLNODE_NEXT
         ENDDO
*
         L     R4,0(,R5)
         IF (C,R4,GT,=F'-1') THEN
            LA    R2,1
         ELSE
            XR    R2,R2
         ENDIF
*
AV2#12_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
WORKDSAB12                   DSECT
*
                             ORG   *+CEEDSASZ
*
PARMVARIANTINB12             DS    A
PARMINDEXOUTB12              DS    A
*
WORKDSAB12_SIZ               EQU   *-WORKDSAB12
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 QueryAVLNODE
*
AV2#94   CEEENTRY AUTO=WORKDSAB94_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB94,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AV2_obj,R8        * Address object DSECT
*
         L     R7,0(,R1)         * Parm 1 ptr to root node
         L     R6,4(,R1)         * Parm 2 query index
         L     R5,8(,R1)         * Parm 3 ptr to ptr to return node
         L     R4,12(,R1)        * Parm 4 exact match (0 / 1)
*
*        If we've reached an empty node ptr
         LTR   R7,R7
         IF (Z) THEN
*           If exact match (R4 /= 0)
            LTR   R4,R4
            IF (NZ) THEN
               MVC   0(4,R5),=A(0) * Set return node ptr to NULL
            ENDIF
*
*        If we haven't reached an empty node yet
         ELSE
            USING AVLNODE,R7     * Addressability of node
*
*           Compare query index with current node's index
            L     R2,AVLNODE_NAME * Point to node's index
            CR    R2,R6          * Compare indeces
            SELECT
            WHEN CC=8
               MVI   EQUALB94,X'00' * Indeces are equal
            WHEN CC=4
               MVI   EQUALB94,X'01' * Query index greater
            WHEN CC=2
               MVI   EQUALB94,X'FF' * Query index lesser
            ENDSEL
*
*           If don't perform exact match (R4 == 0)
            LTR   R4,R4
            IF (Z) THEN
*              If indexes match
               IF (CLI,EQUALB94,EQ,X'00') THEN
*                 Skip this index and continue with the next
                  MVI   EQUALB94,X'01'
*
*              If Query index is lesser
               ELSEIF (CLI,EQUALB94,EQ,X'FF') THEN
*                 Save this node as return node, it might be the
*                 next alphabetical value, if not it will be
*                 overwritten in the next recursive call
                  ST    R7,0(,R5) * Save in return node
               ENDIF
            ENDIF
*
*           If Query index and current node index are unequal
            IF (CLI,EQUALB94,NE,X'00') THEN
*
*              If Query index was lesser
               IF (CLI,EQUALB94,EQ,X'FF') THEN
                  L     R14,AVLNODE_KID   * Use left kid as next node
*
*              If Query index was greater
               ELSE
                  L     R14,AVLNODE_KID+4 * Use right kid as next node
               ENDIF
*
               LA    R1,WORKB94  * Address parameter area
               ST    R14,0(,R1)  * root node ptr
               ST    R6,4(,R1)   * query index
               ST    R5,8(,R1)   * ptr to ptr to return node
               ST    R4,12(,R1)  * exact match
               LA    R15,AV2#94  * Get entry point
               BASR  R14,R15     * Recursive call to same routine
*
*           If Query index and current node index are equal
            ELSE
               ST    R7,0(,R5)   * Save in return node
            ENDIF
         ENDIF
*
         IF (CLC,0(4,R5),NE,=A(0)) THEN
            LA    R2,1
         ELSE
            XR    R2,R2
         ENDIF
*
AV2#94_RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
WORKDSAB94                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB94                      DS    4A
EQUALB94                     DS    C
*
WORKDSAB94_SIZ               EQU   *-WORKDSAB94
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 DisposeAVLNODE
*
AV2#95   CEEENTRY AUTO=WORKDSAB95_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB95,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AV2_obj,R8        * Address object DSECT
*
         L     R7,0(,R1)         * Parm 1 ptr to ptr to node
         L     R6,0(,R7)         * Get ptr to node
         USING AVLNODE,R6
*
         LT    R5,AVLNODE_KID    * Test left kid
         IF (NZ) THEN            * If not NULL
            LA    R1,WORKB95     * Address parameter area
            LA    R15,AVLNODE_KID * Get ptr to ptr to left kid
            ST    R15,0(,R1)     * Set ptr to ptr to left kid as parm 1
            LA    R15,AV2#95     * Get entry point
            BASR  R14,R15        * Recursive call to same routine
         ENDIF
*
         LT    R5,AVLNODE_KID+4  * Test right kid
         IF (NZ) THEN            * If not NULL
            LA    R1,WORKB95     * Address parameter area
            LA    R15,AVLNODE_KID+4 * Get ptr to ptr to right kid
            ST    R15,0(,R1)     * Set ptr to ptr to right kid parm 1
            LA    R15,AV2#95     * Get entry point
            BASR  R14,R15        * Recursive call to same routine
         ENDIF
*
*        Now that all child nodes have been disposed, free the node's
*        value before freeing the node itself
         LA    R4,AVLNODE_VALUE  * Point R4 to value VARIANT
         IF (CLC,vt-VARIANT(4,R4),GE,=A(64)) THEN
            MVC   value-VARIANT(4,R4),=A(0)
*
*           CALL  CEEFRST,(value-VARIANT(R4),FCB95),MF=(E,WORKB95)
         ELSEIF (CLC,vt-VARIANT(4,R4),EQ,=A(VT_UNKNOWN)) THEN
            L     R15,value-VARIANT(,R4)
            LA    R1,WORKB95
            ST    R15,0(,R1)
            L     R15,0(,R15)
            L     R15,8(,R15)
            BASR  R14,R15
         ENDIF
*
         MVC   0(4,R6),=A(0)
*
*        CALL  CEEFRST,(ADDRB95,FCB95),MF=(E,WORKB95)
*
AV2#95_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAB95                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB95                      DS    2A
*
WORKDSAB95_SIZ               EQU   *-WORKDSAB95
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 RightRotate
*
*         Y *               X *
*          / \               / \
*         /   \             /   \
*      X *     * T3     T1 *     * Y
*       / \                     / \
*      /   \                   /   \
*  T1 *     * T2           T2 *     * T3
*
AV2#96   CEEENTRY AUTO=WORKDSAB96_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB96,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AV2_obj,R8        * Address object DSECT
*
         L     R15,0(,R1)        * Parm 1 ptr to node to rotate
         L     R7,0(,R15)        * Point R7 to root node
NODE_Y   USING AVLNODE,R7        * Addressability of node Y
         L     R6,NODE_Y.AVLNODE_KID * Point R6 to left kid of Y
NODE_X   USING AVLNODE,R6        * Addressability of node X
         L     R5,NODE_X.AVLNODE_KID+4 * Point R5 to right kid of X
NODE_T2  USING AVLNODE,R5        * Addressability of node T2
*
*        Do the rotate
         ST    R7,NODE_X.AVLNODE_KID+4 * Make node Y the right kid of X
         ST    R5,NODE_Y.AVLNODE_KID * Make T2 the left kid of Y
         LTR   R5,R5
         IF (NZ) THEN            * If T2 node is not NULL
            ST    R7,NODE_T2.AVLNODE_PARENT * Make Y the parent of T2
         ENDIF
*        Make X the top node by giving it Y's parent
         MVC   NODE_X.AVLNODE_PARENT,NODE_Y.AVLNODE_PARENT
         ST    R6,NODE_Y.AVLNODE_PARENT * Make X the parent of Y
*
*        Recalculate Y node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_Y.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_Y.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_Y.AVLNODE_HEIGHT * Store Y node's height
*
*        Recalculate X node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_X.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_X.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_X.AVLNODE_HEIGHT * Store X node's height
*
*        Return X node (which is now in Y's place
         ST    R6,0(,R15)        * Put ptr to node X in parm 1
*
AV2#96_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAB96                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKDSAB96_SIZ               EQU   *-WORKDSAB96
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 LeftRotate
*
*      X *                     Y *
*       / \                     / \
*      /   \                   /   \
*  T1 *     * Y             X *     * T3
*          / \               / \
*         /   \             /   \
*     T2 *     * T3     T1 *     * T2
*
AV2#97   CEEENTRY AUTO=WORKDSAB97_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB97,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AV2_obj,R8        * Address object DSECT
*
         L     R15,0(,R1)        * Parm 1 ptr to node to rotate
         L     R7,0(,R15)        * Point R7 to root node
NODE_X   USING AVLNODE,R7        * Addressability of node X
         L     R6,NODE_X.AVLNODE_KID+4 * Point R6 to right kid of X
NODE_Y   USING AVLNODE,R6        * Addressability of node Y
         L     R5,NODE_Y.AVLNODE_KID * Point R5 to left kid of Y
NODE_T2  USING AVLNODE,R5        * Addressability of node T2
*
*        Do the rotate
         ST    R7,NODE_Y.AVLNODE_KID * Make node X the left kid of Y
         ST    R5,NODE_X.AVLNODE_KID+4 * Make T2 the right kid of X
         LTR   R5,R5
         IF (NZ) THEN            * If T2 node is not NULL
            ST    R7,NODE_T2.AVLNODE_PARENT * Make X the parent of T2
         ENDIF
*        Make Y the top node by giving it X's parent
         MVC   NODE_Y.AVLNODE_PARENT,NODE_X.AVLNODE_PARENT
         ST    R6,NODE_X.AVLNODE_PARENT * Make Y the parent of X
*
*        Recalculate X node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_X.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_X.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_X.AVLNODE_HEIGHT * Store Y node's height
*
*        Recalculate Y node's height
         LA    R4,1              * R4 = 1
         LT    R2,NODE_Y.AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,NODE_Y.AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,NODE_Y.AVLNODE_HEIGHT * Store X node's height
*
*        Return Y node (which is now in X's place
         ST    R6,0(,R15)        * Put ptr to node Y in parm 1
*
AV2#97_RET EQU   *
         CEETERM
*
         LTORG
*
WORKDSAB97                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKDSAB97_SIZ               EQU   *-WORKDSAB97
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 DeleteNode
*
AV2#98   CEEENTRY AUTO=WORKDSAB98_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB98,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMROOTPTRB98,0(R1) * Parm 1 is ptr to root node ptr
         MVC   PARMNODEINDEXB98,4(R1) * Parm 2 is node index
*
         MVC   RETCODEB98,=A(0)  * Preset retcode to not found
*
         L     R5,PARMROOTPTRB98 * Ptr to ptr to root node
         L     R5,0(,R5)         * Ptr to node
         USING AVLNODE,R5        * Addressability of AVL node
*
*        Compare Delete index with current node's index
         L     R2,AVLNODE_NAME   * Point to node's index
         C     R2,PARMNODEINDEXB98 * Compare indeces
         SELECT
         WHEN CC=8
            MVI   EQUALB98,X'00' * Indeces are equal
         WHEN CC=4
            MVI   EQUALB98,X'01' * Query index greater
         WHEN CC=2
            MVI   EQUALB98,X'FF' * Query index lesser
         ENDSEL
*
*        If Delete index and current node index are unequal
         IF (CLI,EQUALB98,NE,X'00') THEN
*
*           If Delete index was lesser
            IF (CLI,EQUALB98,EQ,X'FF') THEN
               LA    R14,AVLNODE_KID * Use left kid as next node
            ELSE
               LA    R14,AVLNODE_KID+4 * Use right kid as next node
            ENDIF
*
            LA    R1,WORKB98     * Address parameter area
            ST    R14,0(,R1)     * Parm 1 ptr to next root node ptr
            MVC   4(4,R1),PARMNODEINDEXB98 * Parm 2 is node index
            LA    R15,AV2#98     * Get entry point
            BASR  R14,R15        * Recursive call to same routine
*
            ST    R15,RETCODEB98 * Trickle down return code recursively
*
*        If Delete index and current node index are equal
         ELSE
*
            MVC   RETCODEB98,=A(1)
*
*           If either or both kids are NULL
            IF (CLC,AVLNODE_KID(4),EQ,=A(0)),OR,                       X
               (CLC,AVLNODE_KID+4(4),EQ,=A(0)) THEN
               LT    R4,AVLNODE_KID * Check left kid NULL
               IF (Z) THEN
                  LT    R4,AVLNODE_KID+4 * Check right kid NULL
               ENDIF
               IF (Z) THEN
*                 End up here if both kids are NULL
                  L     R14,PARMROOTPTRB98 * Get root node ptr
                  MVC   0(4,R14),=A(0) * Also zero out ptr 2 ptr 2 node
*
                  LT    R14,AVLNODE_PREV
                  IF (NZ) THEN
                     MVC   AVLNODE_NEXT-AVLNODE(4,R14),AVLNODE_NEXT
                  ENDIF
*
                  LT    R14,AVLNODE_NEXT
                  IF (NZ) THEN
                     MVC   AVLNODE_PREV-AVLNODE(4,R14),AVLNODE_PREV
                  ENDIF
*
                  IF (C,R5,EQ,firstNode) THEN
                     MVC   firstNode,AVLNODE_PARENT
                  ENDIF
*
                  IF (C,R5,EQ,lastNode) THEN
                     MVC   lastNode,AVLNODE_PARENT
                  ENDIF
*
                  LR    R4,R5    * Save node ptr in R4
                  XR    R5,R5    * Zero out current node reg
               ELSE
*                 End up here if just one kid is not NULL
                  MVC   AVLNODE_PARENT-AVLNODE(4,R4),AVLNODE_PARENT
                  L     R14,PARMROOTPTRB98 * Get root node ptr
                  ST    R4,0(,R14) * Kid becomes the parent
*
                  LT    R14,AVLNODE_PREV
                  IF (NZ) THEN
                     MVC   AVLNODE_NEXT-AVLNODE(4,R14),AVLNODE_NEXT
                  ENDIF
*
                  LT    R14,AVLNODE_NEXT
                  IF (NZ) THEN
                     MVC   AVLNODE_PREV-AVLNODE(4,R14),AVLNODE_PREV
                  ENDIF
*
                  IF (C,R5,EQ,firstNode) THEN
                     MVC   firstNode,AVLNODE_PARENT
                  ENDIF
*
                  IF (C,R5,EQ,lastNode) THEN
                     MVC   lastNode,AVLNODE_PARENT
                  ENDIF
*
                  LR    R4,R5    * Save node ptr in R4
                  L     R5,0(,R14) * Put kid ptr in curr node reg
               ENDIF
*
               IF (C,R4,EQ,firstNode) THEN
                  MVC   firstNode,AVLNODE_PARENT
               ENDIF
*
               IF (C,R4,EQ,lastNode) THEN
                  MVC   lastNode,AVLNODE_PARENT
               ENDIF
*
*              Dispose of node, but first make sure only the node, not
*              recursively go through children
               MVC   AVLNODE_KID-AVLNODE(4,R4),=A(0)
               MVC   AVLNODE_KID-AVLNODE+4(4,R4),=A(0)
*
               LA    R1,WORKB98  * Address parameter area
               ST    R4,ADDRB98  * Save node ptr in local var
               LA    R15,ADDRB98 * Get ptr to node ptr
               ST    R15,0(,R1)  * Set ptr to node ptr as parm 1
               MVC   4(4,R1),PARMNODEINDEXB98 * Parm 2 is node index
               L     R15,AV2#95A_B98 * Get dispose entry point
               BASR  R14,R15     * Go for it
*
*           If both kids are not NULL
            ELSE
*
*              Find the left most kid anywhere below this node's
*              right kid (so find the next ordered node)
               L     R4,AVLNODE_KID+4 * Get the right kid
               LT    R3,AVLNODE_KID-AVLNODE(,R4) * Get left kid
               DO WHILE=(NZ)
                  LR    R4,R3    * Switch to grandkid
                  LT    R3,AVLNODE_KID-AVLNODE(,R4) * Get left kid
               ENDDO
*
*              Replace this node's name and value by the one we found
*              to be the next in alphab. order
               L     R15,AVLNODE_NAME
               MVC   AVLNODE_NAME,AVLNODE_NAME-AVLNODE(R4)
               ST    R15,AVLNODE_NAME-AVLNODE(,R4)
               MVC   SAVEVARB98,AVLNODE_VALUE
               MVC   AVLNODE_VALUE,AVLNODE_VALUE-AVLNODE(R4)
               MVC   AVLNODE_VALUE-AVLNODE(VARIANT_SIZ,R4),SAVEVARB98
*
*              Now, instead of deleting this node, delete the one who's
*              name and value we copied, start searching from right kid
               LA    R1,WORKB98  * Address parameter area
               LA    R15,AVLNODE_KID+4 * Get ptr to right kid
               ST    R15,0(,R1)  * ptr to right kid node ptr is parm 1
               MVC   4(4,R1),AVLNODE_NAME-AVLNODE(R4)
               LA    R15,AV2#98  * Get dispose entry point
               BASR  R14,R15     * Go for it
            ENDIF
         ENDIF
*
*        If there's no node left to balance, get out
         LTR   R5,R5
         BZ    AV2#98_RET
*
*        Recalculate this node's height
         LA    R4,1              * R4 = 1
         LT    R2,AVLNODE_KID
         IF (NZ) THEN            * If left kid is not NULL
            L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid height
         ENDIF
         LT    R3,AVLNODE_KID+4
         IF (NZ) THEN            * If right kid is not NULL
            L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid height
         ENDIF
         CR    R2,R3             * See if left or right has most height
         IF (GE) THEN            * If left kid
            AR    R4,R2          * Add left kid's height
         ELSE                    * If right kid
            AR    R4,R3          * Add right kid's height
         ENDIF
         ST    R4,AVLNODE_HEIGHT * Store Y node's height
*
*        Get difference between left kid's height and right kid's
         SR    R2,R3
*
*        If difference larger than +1, it's either a left left or
*        a left right scenario
         IF (C,R2,GT,=F'1') THEN
*
*           Get the left kid's grandkid's height
            L     R4,AVLNODE_KID * Get left kid
            LT    R2,AVLNODE_KID-AVLNODE(,R4) * Get left grandkid
            IF (NZ) THEN
               L     R2,AVLNODE_HEIGHT-AVLNODE(,R2)
            ENDIF
            LT    R3,AVLNODE_KID+4-AVLNODE(,R4) * Get right grandkid
            IF (NZ) THEN
               L     R3,AVLNODE_HEIGHT-AVLNODE(,R3)
            ENDIF
*
*           Balance the left kid
            SR    R2,R3
*
*           If difference >= 0
            IF (NM) THEN
*              Left left ...
*
*              ... so do a right rotate
               LA    R1,WORKB98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRB98 * Root node ptr ptr parm 1
               L     R15,AV2#96A_B98 * Get right rotate entry point
               BASR  R14,R15     * Go for it
*
*           If difference < 0
            ELSE
*              Left right ...
*
*              ... so do a left rotate ...
               LA    R1,WORKB98  * Address parameter area
               LA    R2,AVLNODE_KID * Get left kid ptr ptr
               ST    R2,0(,R1)   * Set left kid ptr ptr as parm 1
               L     R15,AV2#97A_B98 * Get left rotate entry point
               BASR  R14,R15     * Go for it
*
*              ... followed by a right rotate
               LA    R1,WORKB98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRB98 * Root node ptr ptr parm 1
               L     R15,AV2#96A_B98 * Get right rotate entry point
               BASR  R14,R15     * Go for it
            ENDIF
*
*        If difference smaller than -1, it's either a right right or
*        a right left scenario
         ELSEIF (C,R2,LT,=F'-1') THEN
*
*           Get the right kid's grandkid's heights
            L     R4,AVLNODE_KID+4 * Get the right kid
            LT    R2,AVLNODE_KID-AVLNODE(,R4) * Get left grandkid
            IF (NZ) THEN
               L     R2,AVLNODE_HEIGHT-AVLNODE(,R2)
            ENDIF
            LT    R3,AVLNODE_KID+4-AVLNODE(,R4) * Get right grandkid
            IF (NZ) THEN
               L     R3,AVLNODE_HEIGHT-AVLNODE(,R3)
            ENDIF
*
*           Balance the right kid
            SR    R2,R3
*
*           If difference <= 0
            IF (NP) THEN
*              Right right ...
*
*              ... so do a left rotate
               LA    R1,WORKB98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRB98 * Root node ptr ptr parm 1
               L     R15,AV2#97A_B98 * Get left rotate entry point
               BASR  R14,R15     * Go for it
*
*           If difference > 0
            ELSE
*              Right left ...
*
*              ... so do a right rotate ...
               LA    R1,WORKB98  * Address parameter area
               LA    R2,AVLNODE_KID+4 * Get right kid ptr ptr
               ST    R2,0(,R1)   * Set right kid ptr ptr as parm 1
               L     R15,AV2#96A_B98 * Get right rotate entry point
               BASR  R14,R15     * Go for it
*
*              ... followed by a left rotate
               LA    R1,WORKB98  * Address parameter area
               MVC   0(4,R1),PARMROOTPTRB98 * Root node ptr ptr parm 1
               L     R15,AV2#97A_B98 * Get left rotate entry point
               BASR  R14,R15     * Go for it
            ENDIF
         ENDIF
*
AV2#98_RET EQU   *
         L     R2,RETCODEB98
         CEETERM RC=(R2)
*
         LTORG
*
                             DS    0F
AV2#95A_B98                  DC    A(AV2#95)
AV2#96A_B98                  DC    A(AV2#96)
AV2#97A_B98                  DC    A(AV2#97)
*
WORKDSAB98                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB98                      DS    3A
RETCODEB98                   DS    F
*
PARMROOTPTRB98               DS    A
PARMNODEINDEXB98             DS    F
*
ADDRB98                      DS    A
EQUALB98                     DS    C
SAVEVARB98                   DS    CL(VARIANT_SIZ)
*
WORKDSAB98_SIZ               EQU   *-WORKDSAB98
*
LWZMAVL  CSECT
*
         DROP
*
* IAV2 InsertNode
*
AV2#99   CEEENTRY AUTO=WORKDSAB99_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSAB99,R13    * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         USING AV2_obj,R8        * Address object DSECT
*
         MVC   PARMROOTPTRB99,0(R1) * Parm 1 ptr to ptr to root node
         MVC   PARMNODEINDEXB99,4(R1) * Parm 2 new node index
         IF (TM,8(R1),X'80',Z) THEN
            MVC   PARMNODEVALUEB99,8(R1) * Parm 3 VARIANT* node value
            MVC   PARMNODEPARENTB99,12(R1) * Parm 4 parent node ptr
            L     R15,16(,R1)    * Parm 5 calling AV2#99's EQUALB99
            NILH  R15,X'7FFF'    * Turn off high order bit
            ST    R15,PARMPREVEQUALB99 * Save in local var
            MVI   0(R15),X'00'   * Initialize to equal
         ELSE
            L     R15,8(,R1)     * Parm 3 VARIANT* node value
            NILH  R15,X'7FFF'    * Turn off high order bit
            ST    R15,PARMNODEVALUEB99 * Save in local var
            MVC   PARMNODEPARENTB99,=A(0) * No parent node
            MVC   PARMPREVEQUALB99,=A(0) * No previous EQUALB99
         ENDIF
*
         L     R14,PARMROOTPTRB99 * Get ptr to ptr to root node
         LT    R14,0(,R14)       * Check for NULL root node
*
*        If current root node is NULL
         IF (Z) THEN
*
*           Allocate memory for new node
            LA    R1,WORKB99
            MVC   0(4,R1),=A(AVLNODE_SIZ)
            MVC   4(4,R1),PARMROOTPTRB99
            L     R15,G_GTST
            BASR  R14,R15
*
            L     R7,PARMROOTPTRB99
            L     R7,0(,R7)
            USING AVLNODE,R7
*
*           Initialize new node with nulls
            MVI   0(R7),X'00'
            MVC   1(AVLNODE_SIZ-1,R7),0(R7)
*
*           Copy parm provided index to new node index
            MVC   AVLNODE_NAME,PARMNODEINDEXB99
*
*           Copy parm provided value to new node value
            LA    R1,WORKB99
            MVC   0(4,R1),PARMNODEVALUEB99
            LA    R15,AVLNODE_VALUE
            ST    R15,4(,R1)
            L     R15,LWZMVCPA_B99
            BASR  R14,R15
*
*           Initialize rest of node properties
            MVC   AVLNODE_HEIGHT,=A(1)
            MVC   AVLNODE_PARENT,PARMNODEPARENTB99
*
            LT    R5,firstNode
            IF (Z) THEN
               LR    R5,R7
               ST    R5,firstNode
            ENDIF
*
            LT    R5,lastNode
            IF (NZ) THEN
               ST    R7,AVLNODE_NEXT-AVLNODE(,R5)
               ST    R5,AVLNODE_PREV
            ENDIF
*
            ST    R7,lastNode
*
            DROP  R7
*
*        If current root node is not NULL
         ELSE
            L     R5,PARMROOTPTRB99
            L     R5,0(,R5)
            USING AVLNODE,R5
*
*           Compare insert index and current node's index
            L     R14,AVLNODE_NAME
            C     R14,PARMNODEINDEXB99
            SELECT
            WHEN CC=8
               ILOG_Write OBJECT=G_ILOG,WORK=WORKB99,LINE=MAK104E_B99, X
               LOGLEVEL=LOG_LEVEL_ERROR
*
               CALL  CEE3ABD,(=A(1008),=A(3)),MF=(E,WORKB99)
            WHEN CC=4
               MVI   EQUALB99,X'01' * Query name greater
            WHEN CC=2
               MVI   EQUALB99,X'FF' * Query name lesser
            ENDSEL
*
*           If room was provided for a 'previous' EQUALA99
            LT    R14,PARMPREVEQUALB99
            IF (NZ) THEN
               MVC   0(1,R14),EQUALB99 * Copy this EQUALA99 to caller's
            ENDIF
*
            LA    R1,WORKB99
            IF (CLI,EQUALB99,EQ,X'FF') THEN
               LA    R14,AVLNODE_KID   * Use left kid as next node
            ELSE
               LA    R14,AVLNODE_KID+4 * Use right kid as next node
            ENDIF
            ST    R14,0(,R1)     * Kid becomes next root node parm 1
            MVC   4(4,R1),PARMNODEINDEXB99 * Same node index as parm 2
            MVC   8(4,R1),PARMNODEVALUEB99 * Same node value as parm 3
            ST    R5,12(,R1)     * This node becomes parent parm 4
            LA    R14,PREVEQUALB99
            ST    R14,16(,R1)    * EQUALA99 as parm 5
            OI    16(R1),X'80'   * And flag it as last parm
            LA    R15,AV2#99     * Get AV2#99 entry point
            BASR  R14,R15        * Recursive call to same routing
*
*           (one of) the recursive call(s) created the new node
*           The code beyond this point is 'on the way out'
*
*           Recalculate this node's height
            LA    R4,1           * R4 = 1
            LT    R2,AVLNODE_KID
            IF (NZ) THEN         * If left kid is not NULL
               L     R2,AVLNODE_HEIGHT-AVLNODE(,R2) * Get left kid hght
            ENDIF
            LT    R3,AVLNODE_KID+4
            IF (NZ) THEN         * If right kid is not NULL
               L     R3,AVLNODE_HEIGHT-AVLNODE(,R3) * Get right kid hgt
            ENDIF
            CR    R2,R3          * See if left or right has most height
            IF (GE) THEN         * If left kid
               AR    R4,R2       * Add left kid's height
            ELSE                 * If right kid
               AR    R4,R3       * Add right kid's height
            ENDIF
            ST    R4,AVLNODE_HEIGHT * Store Y node's height
*
*           Get difference between left kid's height and right kid's
            SR    R2,R3
*
*           If difference larger than +1, it's either a left left or
*           a left right scenario
            IF (C,R2,GT,=F'1') THEN
*
*              Remember, 'on the way out', so previous EQUALA99 is
*              really the last kid's comparison result
*
*              If Insert name was lesser than kid's name
               IF (CLI,PREVEQUALB99,EQ,X'FF') THEN
*                 Left left ...
*
*                 ... so do a right rotate
                  LA    R1,WORKB99 * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRB99 * Root node pt pt parm 1
                  L     R15,AV2#96A_B99 * Get right rotate entry point
                  BASR  R14,R15  * Go for it
*
*              If Insert name was greater that kid's name
               ELSE
*                 Left right ...
*
*                 ... so do a left rotate ...
                  LA    R1,WORKB99 * Address parameter area
                  LA    R2,AVLNODE_KID * Get left kid ptr ptr
                  ST    R2,0(,R1) * Set left kid ptr ptr as parm 1
                  L     R15,AV2#97A_B99 * Get left rotate entry point
                  BASR  R14,R15  * Go for it
*
*                 ... followed by a right rotate
                  LA    R1,WORKB99 * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRB99 * Root node pt pt par 1
                  L     R15,AV2#96A_B99 * Get right rotate entry point
                  BASR  R14,R15     * Go for it
               ENDIF
*
*           If difference smaller than -1, it's either a right right or
*           a right left scenario
            ELSEIF (C,R2,LT,=F'-1') THEN
*
*              Remember, 'on the way out', so previous EQUALA99 is
*              really the last kid's comparison result
*
*              If Insert name was greater than kid's name
               IF (CLI,PREVEQUALB99,EQ,X'01') THEN
*                 Right right ...
*
*                 ... so do a left rotate
                  LA    R1,WORKB99 * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRB99 * Root node pt pt parm 1
                  L     R15,AV2#97A_B99 * Get left rotate entry point
                  BASR  R14,R15  * Go for it
*
*              If Insert name was lesser than kid's name
               ELSE
*                 Right left ...
*
*                 ... so do a right rotate ...
                  LA    R1,WORKB99 * Address parameter area
                  LA    R2,AVLNODE_KID+4 * Get right kid ptr ptr
                  ST    R2,0(,R1) * Set right kid ptr ptr as parm 1
                  L     R15,AV2#96A_B99 * Get right rotate entry point
                  BASR  R14,R15  * Go for it
*
*                 ... followed by a left rotate
                  LA    R1,WORKB99  * Address parameter area
                  MVC   0(4,R1),PARMROOTPTRB99 * Root node pt pt parm 1
                  L     R15,AV2#97A_B99 * Get left rotate entry point
                  BASR  R14,R15     * Go for it
               ENDIF
            ENDIF
*
            DROP  R5
         ENDIF
*
AV2#99_RET EQU   *
         CEETERM
*
         LTORG
*
                             DS    0F
MAK104E_B99                  DC    C'MAK104E Duplicate entry not alloweX
               d',X'00'
*
                             DS    0F
AV2#96A_B99                  DC    A(AV2#96)
AV2#97A_B99                  DC    A(AV2#97)
LWZMVCPA_B99                 DC    V(LWZMVCP)
*
WORKDSAB99                   DSECT
*
                             ORG   *+CEEDSASZ
*
WORKB99                      DS    5A
*
PARMROOTPTRB99               DS    A
PARMNODEINDEXB99             DS    A
PARMNODEVALUEB99             DS    A
PARMNODEPARENTB99            DS    A
PARMPREVEQUALB99             DS    A
*
EQUALB99                     DS    C
PREVEQUALB99                 DS    C
*
WORKDSAB99_SIZ               EQU   *-WORKDSAB99
*
LWZMAVL  CSECT
*
         DROP
*
         COPY  REGS              * Register equates
*
         END   LWZMAVL
