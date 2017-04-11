/* REXX */
PARSE ARG g.ddname
PARSE SOURCE . . g.rexxname .

CALL init

CALL checkEmpty

IF g.error == 0 THEN
   SAY 'Data set is empty'
ELSE
   SAY 'Data set is not empty'

RETURN g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* CHKEMPTY'
SAY COPIES('*',100)

g.error = 0

SAY 'Check data set with dd: 'g.ddname

RETURN

/**********************************************************************/
/* Check empty file                                                   */
/**********************************************************************/
checkEmpty: PROCEDURE EXPOSE g. SIGL

"EXECIO 1 DISKR "g.ddname" (STEM _rec."

IF RC == 0 THEN DO
   g.error = 4
END
ELSE DO
   IF RC /= 4 THEN DO
      g.error = 8
      CALL log "EXECIO failed "RC
   END
END

RETURN

/**********************************************************************/
/* Log a message                                                      */
/**********************************************************************/
log: PROCEDURE EXPOSE g. SIGL

PARSE ARG _msg

SAY g.rexxname SIGL _msg

RETURN
