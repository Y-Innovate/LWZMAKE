*---------------------------------------------------------------------*
* Program    : LWZMVCP                                                *
* Description: Copy a VARIANT                                         *
*---------------------------------------------------------------------*
         TITLE 'LWZMVCP'
*
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's
*
         COPY  IFACES            * Object interfaces
*
         COPY  MINSTANT          * Macro to instantiate new object
*
* Main routine
*
LWZMVCP  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10
*
         USING WORKDSA,R13       * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global area DSECT
*
         L     R6,0(,R1)         * Parm 1 is source VARIANT
VRNTSRC  USING VARIANT,R6        * Addressability of source VARIANT
*
         L     R5,4(,R1)         * Parm 2 is target VARIANT
VRNTTGT  USING VARIANT,R5        * Addressability of target VARIANT
*
         IF (CLC,VRNTTGT.vt,LT,=A(64)) THEN
            IF (CLC,VRNTTGT.vt,EQ,=A(VT_UNKNOWN)) THEN
               L     R15,VRNTTGT.value * Ptr to Vtbl
               LA    R1,WORK     * Address execute storage
               ST    R15,0(,R1)  * Set 'this' (ptr to Vtbl) as parm 1
               L     R15,0(,R15) * Point to start of Vtbl
               L     R15,8(,R15) * Get Release entry point address
               BASR  R14,R15     * Branch to Release entry point
            ENDIF
         ELSE
            MVC   VRNTTGT.value(4),=A(0)
*
*           CALL  CEEFRST,(VRNTTGT.value,FC),MF=(E,WORK)
         ENDIF
*
*        Everything below vt 64 is simple type
         IF (CLC,VRNTSRC.vt,LT,=A(64)) THEN
            MVC   0(VARIANT_SIZ,R5),0(R6)
*
            IF (CLC,VRNTTGT.vt,EQ,=A(VT_UNKNOWN)) THEN
               MVC   TEMPOBJ,=A(0)
*
               L     R15,VRNTTGT.value * Ptr to Vtbl
               LA    R1,WORK     * Address execute storage
               ST    R15,0(,R1)  * Object ptr is parm 1
               LA    R14,G_ISTR_GUID * Point R14 to ISTR GUID
               ST    R14,4(,R1)  * ISTR GUID ptr is parm 2
               LA    R14,TEMPOBJ * Point R14 to return obj
               ST    R14,8(,R1)  * Ptr to return obj is parm 3
               L     R15,0(,R15) * Point R15 to Vtbl
               L     R15,0(,R15) * Point R15 to QueryInterface
               BASR  R14,R15     * QueryInterface
*
               LTR   R15,R15
               IF (Z) THEN
                  L     R15,VRNTTGT.value
                  LA    R1,WORK
                  ST    R15,0(,R1)
                  L     R15,0(,R15)
                  L     R15,8(,R15)
                  BASR  R14,R15
*
                  MINSTANT GUID=G_ISTR_GUID,WORK=WORK,                 X
               OBJPTR=VRNTTGT.value
*
                  L     R2,VRNTSRC.value
                  L     R2,STR_lpString-STR_obj(,R2)
                  ISTR_Set OBJECT=VRNTTGT.value,WORK=WORK,STR=0(,R2)
*
               ELSE
                  L     R15,VRNTTGT.value * Ptr to Vtbl
                  LA    R1,WORK  * Address execute storage
                  ST    R15,0(,R1) * Set 'this' (ptr to Vtbl) as parm 1
                  L     R15,0(,R15) * Point to start of Vtbl
                  L     R15,4(,R15) * Get AddRef entry point address
                  BASR  R14,R15  * Branch to AddRef entry point
               ENDIF
            ENDIF
*
*        If not a simple type, it's a string
         ELSE
            MVC   VRNTTGT.vt,VRNTSRC.vt * Copy vt
            L     R2,VRNTSRC.value * Get ptr in R2
            L     R3,0(,R2)      * Get length prefix
            L     R3,4(,R3)      * Add 4 for length
            ST    R3,G_GTSTSIZ   * Save in local var
*
*           Allocate memory for copy of string
            LA    R1,WORK
            ST    R3,0(,R1)
            LA    R15,VRNTTGT.value
            ST    R15,4(,R1)
            L     R15,G_GTST
            BASR  R14,R15
*
            L     R14,VRNTTGT.value * Target ptr in R0
            L     R2,VRNTSRC.value * Source ptr in R2
            LR    R15,R3         * Copy length from R3 to R1
            MVCL  R14,R2         * Copy prefix and string
         ENDIF
*
LWZMVCP_RET EQU   *
         CEETERM                 * Return to caller
*
         LTORG
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
WORK                         DS    4A  * Work area
FC                           DS    3A  * Feedback code
TEMPOBJ                      DS    A
*
WORKDSA_SIZ                  EQU   *-WORKDSA
*
         MGLOBAL                 * Global area DSECT
*
         COPY  DSCOMOBJ          * Generic COM obj DSECT
*
         COPY  DSSTR
*
         COPY  VARIANT
*
LWZMVCP  CSECT
*
         DROP
*
         COPY  REGS              * Register equates
*
         END   LWZMVCP
