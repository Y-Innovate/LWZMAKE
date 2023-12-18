*
* FMG obj's Vtbl
*
IFMG_Vtbl                    DSECT
IFMG_QueryInterfacePtr       DS    A   * Regular
IFMG_AddRefPtr               DS    A   *   COM
IFMG_ReleasePtr              DS    A   *     Methods
IFMG_StatPtr                 DS    A   * Stat
IFMG_MemberlistPtr           DS    A   * Memberlist
IFMG_DDNameToDSNamePtr       DS    A   * DDNameToDSName
IFMG_DDNameToPathPtr         DS    A   * DDNameToPath
                             DS    0F
IFMG_Vtbl_SIZ                EQU   *-IFMG_Vtbl
*
         MACRO
         IFMG_QueryInterface &OBJECT=,&WORK=,&GUID=,&RETOBJ=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&GUID    * Get ptr to GUID
         ST    R14,4(,R1)   * Set ptr to GUID as parm 2
         LA    R14,&RETOBJ  * Get ptr to ptr to return object
         ST    R14,8(,R1)   * Set ptr to ptr to return object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_QueryInterfacePtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to QueryInterface entry point
         MEND
*
         MACRO
         IFMG_AddRef &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_AddRefPtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to AddRef entry point
         MEND
*
         MACRO
         IFMG_Release &OBJECT=,&WORK=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_ReleasePtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Release entry point
         MEND
*
         MACRO
         IFMG_Stat &OBJECT=,&WORK=,&FILE=,&IFFO=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&FILE    * Get ptr to zero term string
         ST    R14,4(,R1)   * Set ptr to zero term string as parm 2
         MVC   8(4,R1),&IFFO * Set IFFO object as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_StatPtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Stat entry point
         MEND
*
         MACRO
         IFMG_Memberlist &OBJECT=,&WORK=,&PDSNAME=,&FILTER=,           X
               &ISTBPTR_OUT=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&PDSNAME * Get ptr to zero term string
         ST    R14,4(,R1)   * Set ptr to zero term string as parm 2
         LA    R14,&FILTER  * Get ptr to zero term string
         ST    R14,8(,R1)   * Set ptr to zero term string as parm 3
         LA    R14,&ISTBPTR_OUT * Get ptr to ISTB to return
         ST    R14,12(,R1)  * Set ptr to ISTR to return as parm 4
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_MemberlistPtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to Memberlist entry point
         MEND
*
         MACRO
         IFMG_DDNameToDSName &OBJECT=,&WORK=,&DDNAME=,&DSNAME=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&DDNAME  * Get ptr to DD name
         ST    R14,4(,R1)   * Set ptr to DD name as parm 2
         LA    R14,&DSNAME  * Get ptr to DS name
         ST    R14,8(,R1)   * Set ptr to DS name as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_DDNameToDSNamePtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to DDNameToDSName entry point
         MEND
*
         MACRO
         IFMG_DDNameToPath &OBJECT=,&WORK=,&DDNAME=,&PATH=
         L     R15,&OBJECT  * Ptr to Vtbl
         LA    R1,&WORK     * Address execute storage
         ST    R15,0(,R1)   * Set 'this' (ptr to Vtbl) as parm 1
         LA    R14,&DDNAME  * Get ptr to DD name
         ST    R14,4(,R1)   * Set ptr to DD name as parm 2
         LA    R14,&PATH    * Get ptr to Path
         ST    R14,8(,R1)   * Set ptr to Path as parm 3
         L     R15,0(,R15)  * Point to start of Vtbl
         L     R15,IFMG_DDNameToPathPtr-IFMG_Vtbl(,R15)
         BASR  R14,R15      * Branch to DDNameToPath entry point
         MEND
