         MACRO
&NAME    MQRYIFCE &SUF=,&IFACE=
.*
         LCLC  &CURCSECT
&CURCSECT SETC '&SYSECT'
.*
&NAME    CEEENTRY AUTO=WORKDSA&SUF._SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSA&SUF,R13   * Address DSA and extra stg for vars
*
         USING GLOBAL,R9         * Address global DSECT
*
         MVC   THIS&SUF,0(R1)    * Store 'this' pointer
         L     R5,4(,R1)         * Get pointer to GUID
*
*        If anything other than &IFACE interface was requested
         IF (CLC,0(16,R5),NE,G_&IFACE._GUID) THEN
            LA    R2,12
            B     &NAME._RET
         ENDIF
*
         L     R4,8(,R1)         * Param 3 points to return obj ptr
         MVC   0(4,R4),THIS&SUF  * Give back this pointer
*
*        Increase reference count for returned object pointer copy
         &IFACE._AddRef OBJECT=THIS&SUF,WORK=WORK&SUF
*
         XR    R2,R2
*
&NAME._RET EQU   *
         CEETERM RC=(R2)
*
         LTORG
*
WORKDSA&SUF                  DSECT
*
                             ORG   *+CEEDSASZ
*
THIS&SUF                     DS    A
WORK&SUF                     DS    A
*
WORKDSA&SUF._SIZ             EQU   *-WORKDSA&SUF
*
         DROP
&CURCSECT CSECT
.*
         MEND
