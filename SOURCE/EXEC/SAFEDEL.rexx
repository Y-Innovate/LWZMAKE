/* REXX */
/**********************************************************************/
/* Program    : SAFEDEL                                               */
/*                                                                    */
/* Description: This program accepts a space delimited set of data    */
/*              sets and/or PDS(E) members, checks if they exist and  */
/*              if so deletes them.                                   */
/*                                                                    */
/* Environment: TSO or ISPF                                           */
/*                                                                    */
/* Parameters : A space delimited set of data sets and/or PDS(E)      */
/*              members.                                              */
/*                                                                    */
/* Returns    : 0 when all deletes successful                         */
/*              8 when a delete failed                                */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "MY.DS1 MY.PDS MY.PDS2(MEM1)"                               */
/*                                                                    */
/* CALL 'SAFEDEL' _par                                                */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL safeDelete

IF g.DELETE.retcode /= 0 THEN
   g.error = 8

EXIT g.error

/**********************************************************************/
/* Initializations                                                    */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL
SAY '******************************************'
SAY '* SAFEDEL                                *'
SAY '******************************************'
SAY g.arg

g.error = 0
g.DELETE.retcode = 0

RETURN

safeDelete: PROCEDURE EXPOSE g. SIGL
/**********************************************************************/
/* Safely delete dataset(s)                                           */
/**********************************************************************/

_wrds = WORDS(g.arg)

DO i = 1 TO _wrds WHILE g.DELETE.retcode == 0
   SAY 'Check 'WORD(g.arg,i)

   _exists = SYSDSN("'"WORD(g.arg,i)"'")

   IF _exists == "OK" THEN DO
      SAY 'Delete 'WORD(g.arg,i)

      ADDRESS TSO "DELETE '"WORD(g.arg,i)"'"

      IF RC /= 0 THEN DO
         g.DELETE.retcode = RC
      END
   END
END

RETURN
