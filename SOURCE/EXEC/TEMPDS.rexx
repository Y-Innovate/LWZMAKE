/* REXX */
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL allocateDS

IF g.error == 0 THEN DO
   SAY 'Allocated data set: 'g.dsname
   SAY 'Allocated with dd : 'g.ddname

   RETURN g.ddname
END
ELSE
   RETURN g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* TEMPDS'
SAY COPIES('*',100)

g.error = 0

g.ddname = ""
g.dsname = ""

RETURN

/**********************************************************************/
/* Allocate a temporary data set                                      */
/**********************************************************************/
allocateDS: PROCEDURE EXPOSE g. SIGL

_rc = BPXWDYN("ALLOC NEW RECFM(F,B) DSORG(PS) LRECL(80) TRACKS" || ,
              " SPACE(5,5) UNIT(SYSDA) RTDDN(_ddn) RTDSN(_dsn)" || ,
              " MSG(_msg.)")

IF _rc == 0 THEN DO
   g.ddname = _ddn
   g.dsname = _dsn
END
ELSE DO
   DO i = 1 TO _msg.0
      SAY _msg.i
   END
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of a temp data set failed with '_rc
END

RETURN

/**********************************************************************/
/* Log a message                                                      */
/**********************************************************************/
log: PROCEDURE EXPOSE g. SIGL

PARSE ARG _msg

SAY g.rexxname SIGL _msg

RETURN
