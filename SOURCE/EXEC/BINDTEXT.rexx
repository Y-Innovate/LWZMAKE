/* REXX */
/**********************************************************************/
/* Program    : BINDTEXT                                              */
/*                                                                    */
/* Description:                                                       */
/*                                                                    */
/* Environment: TSO or ISPF                                           */
/*                                                                    */
/* Parameters : A PDS(E) name with a member.                          */
/*                                                                    */
/* Returns    : member contents when successful                       */
/*              8 for failure                                         */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "MY.PDS2 MEM1"                                              */
/*                                                                    */
/* CALL 'BINDTEXT' _par                                               */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.pds g.mem
PARSE SOURCE . . g.rexxname .

CALL init

IF g.error == 0 THEN DO
   CALL allocateMem
END

IF g.error == 0 THEN DO
   CALL readText
END

CALL freeMem

IF g.error /= 0 THEN DO
   EXIT g.error
END
ELSE DO
   EXIT g.returnText
END

/**********************************************************************/
/* Initializations                                                    */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* BINDTEXT'
SAY COPIES('*',100)
SAY g.pds g.mem

g.error = 0

g.allocated = 0

g.returnText = ""

RETURN

/**********************************************************************/
/* Allocate member                                                    */
/**********************************************************************/
allocateMem: PROCEDURE EXPOSE g. SIGL

_dsn = g.pds"("g.mem")"

_rc = BPXWDYN("ALLOC DSN('"_dsn"') SHR RTDDN(_ddn)")

IF _rc == 0 THEN DO
   g.allocated = 1
   g.ddname = _ddn
END
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of '_dsn' failed with '_rc
END

RETURN

/**********************************************************************/
/* Free member                                                        */
/**********************************************************************/
freeMem: PROCEDURE EXPOSE g. SIGL

IF g.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.ddname")")

   IF _rc == 0 THEN DO
      g.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.ddname' failed with '_rc
   END
END

RETURN

/**********************************************************************/
/* Read member text                                                   */
/**********************************************************************/
readText: PROCEDURE EXPOSE g. SIGL

"EXECIO * DISKR "g.ddname" (STEM _text. FINIS"

IF RC == 0 THEN DO
   IF _text.0 > 0 THEN DO
      DO I = 1 TO _text.0
         _thisLine = SUBSTR(_text.I,1,72)
         _wordCount = WORDS(_thisLine)

         IF _wordCount > 0 THEN DO
            _startAt = 1
            IF g.returnText == "" THEN DO
               IF WORD(_thisLine,1) == 'BIND' THEN ,
                  _startAt = 2
            END

            DO J = _startAt TO _wordCount
               IF WORD(_thisLine,J) /= "-" THEN ,
                  g.returnText = g.returnText" "WORD(_thisLine,J)
            END
         END
      END
   END
END
ELSE DO
   CALL log "Error in EXECIO "RC
   g.error = 8
END

RETURN

/**********************************************************************/
/* Log a message                                                      */
/**********************************************************************/
log: PROCEDURE EXPOSE g. SIGL

PARSE ARG _msg

SAY g.rexxname SIGL _msg

RETURN
