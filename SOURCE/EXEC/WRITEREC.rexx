/* REXX */
PARSE ARG g.ddname","g.record
PARSE SOURCE . . g.rexxname .

CALL init

CALL writeRecord

RETURN g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* WRITEREC'
SAY COPIES('*',100)

g.error = 0

SAY 'Write to ddname: 'g.ddname
SAY 'Write record   : "'g.record'"'

RETURN

/**********************************************************************/
/* Write a record                                                     */
/**********************************************************************/
writeRecord: PROCEDURE EXPOSE g. SIGL

PUSH g.record
"EXECIO 1 DISKW "g.ddname

IF RC /= 0 THEN DO
   g.error = 8
   CALL log "EXECIO failed "RC
END

RETURN

/**********************************************************************/
/* Log a message                                                      */
/**********************************************************************/
log: PROCEDURE EXPOSE g. SIGL

PARSE ARG _msg

SAY g.rexxname SIGL _msg

RETURN
