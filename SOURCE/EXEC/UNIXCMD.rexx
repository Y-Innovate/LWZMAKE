/* REXX */
/**********************************************************************/
/* Program    : UNIXCMD                                               */
/*                                                                    */
/* Description: This program accepts a Unix System Services command   */
/*              and executes it.                                      */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : A single unix command.                                */
/*                                                                    */
/* Returns    : 0 when unix command returned >= 0                     */
/*              8 when unix command returned <  0                     */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "mkdir -m 755 /u/myusr/newdir"                              */
/*                                                                    */
/* CALL 'UNIXCMD' _par                                                */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
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

CALL BPXWUNIX g.arg,,_stdout.,_stderr.,,1

CMDRESULT = RESULT
SAY 'RESULT = 'CMDRESULT
IF CMDRESULT < 0 THEN DO
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
