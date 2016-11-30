/* REXX */
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL executeCmd

EXIT g.error

/**********************************************************************/
/* Initializations                                                    */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL
SAY '******************************************'
SAY '* UNIXCMD                                *'
SAY '******************************************'
SAY g.arg

g.error = 0

RETURN

executeCmd: PROCEDURE EXPOSE g. SIGL
/**********************************************************************/
/* Execute UNIX command                                               */
/**********************************************************************/

CALL BPXWUNIX g.arg,,_stdout.,_stderr.

SAY 'RESULT = 'RESULT
IF RESULT < 0 THEN DO
   g.error = 8
END

SAY 'STDOUT'
DO i = 1 TO _stdout.0
   SAY _stdout.i
END
SAY 'STDERR'
DO i = 1 TO _stderr.0
   SAY _stderr.i
END

RETURN
