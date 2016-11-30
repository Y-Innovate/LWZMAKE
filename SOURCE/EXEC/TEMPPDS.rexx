/* REXX */
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL allocatePDS

IF g.error == 0 THEN
   RETURN g.dsname
ELSE
   RETURN g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* TEMPPDS'
SAY COPIES('*',100)

g.error = 0

g.dsname = ""

RETURN

/**********************************************************************/
/* Allocate a temporary PDS                                           */
/**********************************************************************/
allocatePDS: PROCEDURE EXPOSE g. SIGL

_rc = BPXWDYN("ALLOC NEW RECFM(F,B) DSORG(PO) LRECL(80) TRACKS" || ,
              " SPACE(5,5) DIR(30) UNIT(SYSDA) RTDSN(_dsn) MSG(_msg.)")

IF _rc == 0 THEN DO
   g.dsname = _dsn
END
ELSE DO
   DO i = 1 TO _msg.0
      SAY _msg.i
   END
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of a PDS failed with '_rc
END

RETURN

/**********************************************************************/
/* Log a message                                                      */
/**********************************************************************/
log: PROCEDURE EXPOSE g. SIGL

PARSE ARG _msg

SAY g.rexxname SIGL _msg

RETURN
