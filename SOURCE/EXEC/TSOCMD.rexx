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
SAY '* TSOCMD                                 *'
SAY '******************************************'
SAY g.arg

g.error = 0

RETURN

executeCmd: PROCEDURE EXPOSE g. SIGL
/**********************************************************************/
/* Execute TSO command                                                */
/**********************************************************************/

ADDRESS TSO g.arg

g.error = RC

RETURN
