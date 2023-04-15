         MACRO
&NAME    MRELEASE &SUF=,&OBJ=
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
         L     R6,0(,R1)         * Param 1 points to this object
         USING COM_obj,R6
*
         LT    R5,count          * Load current ref count
         BZ    &NAME._RET        * Should never happen....
         S     R5,=A(1)          * Decrease ref count
         ST    R5,count          * Put new ref count back
*
*        If reference count dropped to 0, object can be freed
         IF (Z) THEN
            MVC   lpVtbl,=A(0)
*
*           L     R5,lpVtbl      * Get ptr to Vtbl
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher
*           ST    R5,OBJPTR&SUF  * Put ptr in variable
*           CALL  CEEFRST,(OBJPTR&SUF,FC&SUF),MF=(E,WORK&SUF)
*
            L     R15,G_OBJCOUNT
            BCTR  R15,R0
            ST    R15,G_OBJCOUNT
.*
         AIF ('&OBJ' EQ 'LOG').SKIP_LOG
*
            L     R15,G_ILOG
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN
               ST    R6,G_DEC8
               UNPK  G_ZONED8(9),G_DEC8(5)
               L     R15,G_HEXTAB
               TR    G_ZONED8(8),0(R15)
               MVI   G_ZONED8+8,X'00'
*
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK&SUF
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK&SUF,     X
               ZSTR=MAK502D_&OBJ
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK&SUF,     X
               ZSTR=G_ZONED8
*
               L     R2,G_ISTB_tmp
               L     R2,STB_lpBuf-STB_obj(,R2)
               ILOG_Write OBJECT=G_ILOG,WORK=WORK&SUF,LINE=0(,R2),     X
               LOGLEVEL=LOG_LEVEL_DEBUG2
            ENDIF
.SKIP_LOG ANOP
.*
         ENDIF
*
&NAME._RET EQU   *
         CEETERM
*
         LTORG
.*
         AIF ('&OBJ' EQ 'LOG').SKIP_LOG2
*
                             DS    0F
MAK502D_&OBJ                 DC    C'MAK502D Deleted I&OBJ object '
                             DC    X'00'
*
                             DS    0F
HEXTAB_&SUF                  EQU   *-C'0'
                             DC    C'0123456789ABCDEF'
.SKIP_LOG2 ANOP
*
WORKDSA&SUF                  DSECT
*
                             ORG   *+CEEDSASZ
*
OBJPTR&SUF                   DS    A
FC&SUF                       DS    3A
WORK&SUF                     DS    3A
*
WORKDSA&SUF._SIZ             EQU   *-WORKDSA&SUF
*
         DROP
&CURCSECT CSECT
.*
         MEND
