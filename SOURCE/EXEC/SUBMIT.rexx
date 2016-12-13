/* REXX */
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL submitJob

EXIT g.error

/**********************************************************************/
/* Initializations                                                    */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* SUBMIT'
SAY COPIES('*',100)
SAY g.arg

g.error = 0

RETURN

/**********************************************************************/
/* Submit the job                                                     */
/**********************************************************************/
submitJob: PROCEDURE EXPOSE g. SIGL

_jcl_allocated = 0
_wtr_allocated = 0

_rc = BPXWDYN("ALLOC DSN('"g.arg"') SHR RTDDN(_ddnjcl)")

IF _rc == 0 THEN DO
   _jcl_allocated = 1
END
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of 'g.arg' failed with '_rc
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC SYSOUT WRITER(INTRDR) RTDDN(_ddnwtr)")

   IF _rc == 0 THEN DO
      _wtr_allocated = 1
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of INTRDR failed with '_rc
   END
END

IF g.error == 0 THEN DO
   "EXECIO * DISKR "_ddnjcl" (STEM _jcl. FINIS"

   _rc = RC

   IF _rc == 0 THEN DO
      "EXECIO * DISKW "_ddnwtr" (STEM _Jcl. FINIS"

      _rc = RC

      IF _rc /= 0 THEN DO
         g.error = 8
         CALL log "EXECIO DISKW for internal reader failed "_rc
      END
   END
   ELSE DO
      g.error = 8
      CALL log "EXECIO DISKR for input JCL failed "_rc
   END
END

IF _jcl_allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("_ddnjcl")")

   IF _rc /= 0 THEN DO
      g.error = 8
      CALL log "FREE for input JCL failed "_rc
   END
END

IF _wtr_allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("_ddnwtr")")

   IF _rc /= 0 THEN DO
      g.error = 8
      CALL log "FREE for internal reader failed "_rc
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
