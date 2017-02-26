/* REXX */
/**********************************************************************/
/* Program    : TSOCMD                                                */
/*                                                                    */
/* Description: This program accepts a TSO command and executes it.   */
/*                                                                    */
/* Environment: TSO or ISPF                                           */
/*                                                                    */
/* Parameters : A TSO command.                                        */
/*                                                                    */
/* Returns    : 0 when TSO command successful                         */
/*              n any TSO command return code                         */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "ALLOC DATASET('MY.PDS.JCL') NEW RECFM(F,B) LRECL(80)" || , */
/*        " CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)"          */
/*                                                                    */
/* CALL 'TSOCMD' _par                                                 */
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
