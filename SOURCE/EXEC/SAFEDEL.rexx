/* REXX */
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL safeDelete

IF g.DELETE.retcode ª= 0 THEN
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

      IF RC ª= 0 THEN DO
         g.DELETE.retcode = RC
      END
   END
END

RETURN
