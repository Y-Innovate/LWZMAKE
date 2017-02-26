/* REXX */
/**********************************************************************/
/* Program    : LKED                                                  */
/*                                                                    */
/* Description: This program invokes IEWBLINK to link-edit one or     */
/*              more objects into a load module.                      */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-SYSLIN(-syslin-)--SYSLMOD(-syslmod-)------------>  */
/*                                                                    */
/*              >--+----------------------+------------------------>  */
/*                 |         .--------.   |                           */
/*                 |         V        |   |                           */
/*                 '-SYSLIB(---syslib-+-)-'                           */
/*                                                                    */
/*              >--+----------------------+------------------------>  */
/*                 |         .--------.   |                           */
/*                 |         V        |   |                           */
/*                 '-OBJECT(---object-+-)-'                           */
/*                                                                    */
/*              >--+------------------+---------------------------->  */
/*                 |       .------.   |                               */
/*                 |       V      |   |                               */
/*                 '-LOAD(---load-+-)-'                               */
/*                                                                    */
/*              >--+---------------------+--+----------------+--><    */
/*                 '-SYSPRINT(-listing-)-'  '-PARM(-params-)-'        */
/*                                                                    */
/*              syslin : Input binder control statements.             */
/*              syslmod: Output load module data set.                 */
/*              syslib : One or more input object or load module      */
/*                       PDS(E)'s used to resolve external references */
/*                       that were not explicitly specified.          */
/*              object : One or more input object PDS(E)'s that you   */
/*                       can explicitly refer to in the binder        */
/*                       control statements.                          */
/*              load   : One or more input load module PDS(E)'s that  */
/*                       you can explicitly refer to in the binder    */
/*                       control statements.                          */
/*              listing: Output binder listing data set.              */
/*              params : Parameters to IEWBLINK.                      */
/*                                                                    */
/* Returns    : 0 when IEWBLINK returned 4 or less                    */
/*              8 when REXX error occurs or when parameter string     */
/*                contains syntax error                               */
/*              n any IEWBLINK return code > 4                        */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "SYSLIN(MY.LKEDIN.PDS(MEMBER))"       || ,                  */
/*        " SYSLMOD(MY.LOADLIB.PDS(MEMBER))"    || ,                  */
/*        " SYSLIB(CEE.SCEELKED)"               || ,                  */
/*        " OBJECT(MY.OBJECT.PDS(MEMBER))"      || ,                  */
/*        " PARM(LIST,XREF,RENT,REUS)"                                */
/*                                                                    */
/* CALL 'LKED' _par                                                   */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL parseArguments

IF g.error == 0 THEN DO
   CALL allocDDs
END

IF g.error == 0 THEN DO
   CALL invokeIEWBLINK
END

CALL freeDDs

IF g.IEWBLINK.retcode > 4 | g.IEWBLINK.retcode < 0 THEN
   g.error = g.IEWBLINK.retcode

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* LKED'
SAY COPIES('*',100)

g.error = 0
g.IEWBLINK.retcode = 0

g.syslin = ""
g.syslmod = ""
g.syslib.0 = 0
g.sysprint = ""
g.object.0 = 0
g.load.0 = 0
g.parm = ""

g.SYSLIN.allocated = 0
g.SYSLMOD.allocated = 0
g.SYSLIB.allocated = 0
g.SYSLIB.ddname.0 = 0
g.SYSPRINT.allocated = 0
g.SYSTERM.allocated = 0
g.SYSDEFSD.allocated = 0
g.OBJECT.allocated = 0
g.OBJECT.ddname.0 = 0
g.LOAD.allocated = 0
g.LOAD.ddname.0 = 0

RETURN

/**********************************************************************/
/* Allocate DD's for invoking IEWBLINK                                */
/**********************************************************************/
allocDDs: PROCEDURE EXPOSE g. SIGL

_rc = BPXWDYN("ALLOC DSN('"g.syslin"') SHR RTDDN(_ddn)")

IF _rc == 0 THEN DO
   g.SYSLIN.allocated = 1
   g.SYSLIN.ddname = _ddn
END
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of 'g.syslin' failed with '_rc
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.syslmod"') SHR RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSLMOD.allocated = 1
      g.SYSLMOD.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.syslmod' failed with '_rc
   END
END

IF g.error == 0 THEN DO
   IF g.syslib.0 == 0 THEN DO
      _rc = BPXWDYN("ALLOC DUMMY FI(SYSLIB) SHR")

      IF _rc == 0 THEN DO
         g.SYSLIB.allocated = 1
         g.SYSLIB.ddname.0 = 1
         g.SYSLIB.ddname.1 = "SYSLIB"
      END
      ELSE DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Dynamic allocation of dummy SYSLIB failed with '_rc
      END
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC DSN('"g.syslib.1"') FI(SYSLIB) SHR REUSE")

      IF _rc == 0 THEN DO
         g.SYSLIB.allocated = 1
         g.SYSLIB.ddname.0 = 1
         g.SYSLIB.ddname.1 = "SYSLIB"
      END
      ELSE DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Dynamic allocation of 'g.syslib.1' failed with '_rc
      END

      DO i = 2 TO g.syslib.0 WHILE g.error == 0
         _rc = BPXWDYN("ALLOC DSN('"g.syslib.i"') SHR RTDDN(_ddn)")

         IF _rc == 0 THEN DO
            _nextDD = g.SYSLIB.ddname.0 + 1
            g.SYSLIB.ddname.0 = _nextDD
            g.SYSLIB.ddname._nextDD = _ddn

            _rc = BPXWDYN("CONCAT DDLIST(SYSLIB,"_ddn")")

            IF _rc /= 0 THEN DO
               g.error = 8
               IF _rc > 0 THEN _rc = D2X(_rc)
               CALL log 'Dynamic concatenation failed with '_rc
            END
         END
         ELSE DO
            g.error = 8
            IF _rc > 0 THEN _rc = D2X(_rc)
            CALL log 'Dynamic allocation of 'g.syslib.i' failed with '_rc
         END
      END
   END
END

IF g.error == 0 THEN DO
   IF g.sysprint /= "" THEN DO
      _rc = BPXWDYN("ALLOC DSN('"g.sysprint"') SHR RTDDN(_ddn)")
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC NEW RECFM(V,B,M) DSORG(PS) LRECL(133) CYL" || ,
                    " SPACE(1,1) RTDDN(_ddn)")
   END

   IF _rc == 0 THEN DO
      g.SYSPRINT.allocated = 1
      g.SYSPRINT.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSPRINT failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSTERM.allocated = 1
      g.SYSTERM.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSTERM failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSDEFSD.allocated = 1
      g.SYSDEFSD.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSDEFSD failed with '_rc
   END
END

IF g.error == 0 & g.object.0 /= 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.object.1"') FI(OBJECT) SHR REUSE")

   IF _rc == 0 THEN DO
      g.OBJECT.allocated = 1
      g.OBJECT.ddname.0 = 1
      g.OBJECT.ddname.1 = "OBJECT"
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.object.1' failed with '_rc
   END

   DO i = 2 TO g.object.0 WHILE g.error == 0
      _rc = BPXWDYN("ALLOC DSN('"g.object.i"') SHR RTDDN(_ddn)")

      IF _rc == 0 THEN DO
         _nextDD = g.OBJECT.ddname.0 + 1
         g.OBJECT.ddname.0 = _nextDD
         g.OBJECT.ddname._nextDD = _ddn

         _rc = BPXWDYN("CONCAT DDLIST(OBJECT,"_ddn")")

         IF _rc /= 0 THEN DO
            g.error = 8
            IF _rc > 0 THEN _rc = D2X(_rc)
            CALL log 'Dynamic concatenation failed with '_rc
         END
      END
      ELSE DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Dynamic allocation of 'g.object.i' failed with '_rc
      END
   END
END

IF g.error == 0 & g.load.0 /= 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.load.1"') FI(LOAD) SHR REUSE")

   IF _rc == 0 THEN DO
      g.LOAD.allocated = 1
      g.LOAD.ddname.0 = 1
      g.LOAD.ddname.1 = "LOAD"
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.load.1' failed with '_rc
   END

   DO i = 2 TO g.load.0 WHILE g.error == 0
      _rc = BPXWDYN("ALLOC DSN('"g.load.i"') SHR RTDDN(_ddn)")

      IF _rc == 0 THEN DO
         _nextDD = g.LOAD.ddname.0 + 1
         g.LOAD.ddname.0 = _nextDD
         g.LOAD.ddname._nextDD = _ddn

         _rc = BPXWDYN("CONCAT DDLIST(LOAD,"_ddn")")

         IF _rc /= 0 THEN DO
            g.error = 8
            IF _rc > 0 THEN _rc = D2X(_rc)
            CALL log 'Dynamic concatenation failed with '_rc
         END
      END
      ELSE DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Dynamic allocation of 'g.load.i' failed with '_rc
      END
   END
END

RETURN

/**********************************************************************/
/* Free DD's after invoking IEWBLINK                                  */
/**********************************************************************/
freeDDs: PROCEDURE EXPOSE g. SIGL

IF g.SYSLIN.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSLIN.ddname")")

   IF _rc == 0 THEN DO
      g.SYSLIN.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSLIN.ddname' failed with '_rc
   END
END

IF g.SYSLMOD.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSLMOD.ddname")")

   IF _rc == 0 THEN DO
      g.SYSLMOD.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSLMOD.ddname' failed with '_rc
   END
END

IF g.SYSLIB.allocated == 1 THEN DO
   DO i = 1 TO g.SYSLIB.ddname.0
      _rc = BPXWDYN("FREE FI("g.SYSLIB.ddname.i")")

      IF _rc /= 0 THEN DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Free of file 'g.SYSLIB.ddname.i' failed with '_rc
      END
   END
   g.SYSLIB.allocated = 0
END

IF g.SYSPRINT.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSPRINT.ddname")")

   IF _rc == 0 THEN DO
      g.SYSPRINT.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSPRINT.ddname' failed with '_rc
   END
END

IF g.SYSTERM.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSTERM.ddname")")

   IF _rc == 0 THEN DO
      g.SYSTERM.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSTERM.ddname' failed with '_rc
   END
END

IF g.SYSDEFSD.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSDEFSD.ddname")")

   IF _rc == 0 THEN DO
      g.SYSDEFSD.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSDEFSD.ddname' failed with '_rc
   END
END

IF g.OBJECT.allocated == 1 THEN DO
   DO i = 1 TO g.OBJECT.ddname.0
      _rc = BPXWDYN("FREE FI("g.OBJECT.ddname.i")")

      IF _rc /= 0 THEN DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Free of file 'g.OBJECT.ddname.i' failed with '_rc
      END
   END

   g.OBJECT.allocated = 0
END

IF g.LOAD.allocated == 1 THEN DO
   DO i = 1 TO g.LOAD.ddname.0
      _rc = BPXWDYN("FREE FI("g.LOAD.ddname.i")")

      IF _rc /= 0 THEN DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Free of file 'g.LOAD.ddname.i' failed with '_rc
      END
   END

   g.LOAD.allocated = 0
END

RETURN

/**********************************************************************/
/* Invoke IEWBLINK                                                    */
/**********************************************************************/
invokeIEWBLINK: PROCEDURE EXPOSE g. SIGL

_prog = 'IEWBLINK'
_parm = g.parm
_ddlist = LEFT(g.SYSLIN.ddname,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSLMOD.ddname,8) || ,
          LEFT(g.SYSLIB.ddname.1,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSPRINT.ddname,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSTERM.ddname,8) || ,
          LEFT(g.SYSDEFSD.ddname,8)

ADDRESS LINKMVS _prog '_parm _ddlist'

g.IEWBLINK.retcode = RC

"EXECIO * DISKR "g.SYSPRINT.ddname" (STEM _sysprint. FINIS"

_rc = RC

IF _rc == 0 THEN DO
   _rc = BPXWDYN("ALLOC SYSOUT(A) REUSE RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      SAY "SYSPRINT copied to DD "_ddn

      "EXECIO "_sysprint.0" DISKW "_ddn" (STEM _sysprint. FINIS"

      _rc = RC

      IF _rc /= 0 THEN DO
         g.error = 8
         CALL log "EXECIO DISKW for copy SYSPRINT failed "_rc
      END

      _rc = BPXWDYN("FREE FI("_ddn")")

      IF _rc /= 0 THEN DO
         g.error = 8
         CALL log "FREE for copy SYSPRINT failed "_rc
      END
   END
   ELSE DO
      g.error = 8
      CALL log "ALLOC for copy SYSPRINT failed "_rc
   END
END
ELSE DO
   g.error = 8
   CALL log "EXECIO DISKR for SYSPRINT failed "_rc
END

IF g.IEWBLINK.retcode > 4 THEN DO
   CALL 'SAFEDEL' g.SYSLMOD
END

RETURN

/**********************************************************************/
/* Parse arguments                                                    */
/**********************************************************************/
parseArguments: PROCEDURE EXPOSE g. SIGL

g.parser.EXPECTED_EOF = 1
g.parser.EXPECTED_NORMAL = 2
g.parser.EXPECTED_OPEN_BRACKET = 4
g.parser.EXPECTED_CLOSE_BRACKET = 8
g.parser.EXPECTED_DOT = 16
g.parser.EXPECTED_ANYTHING_ELSE = 64

g.parser.SCAN_STATE_NOT_IN_PARM = 1
g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM = g.parser.EXPECTED_EOF + ,
                                          g.parser.EXPECTED_NORMAL
g.parser.SCAN_STATE_IN_PARM1 = 2
g.parser.EXPECTED_FOR_STATE_IN_PARM1 = g.parser.EXPECTED_OPEN_BRACKET
g.parser.SCAN_STATE_IN_PARM2 = 3
g.parser.EXPECTED_FOR_STATE_IN_PARM2 = g.parser.EXPECTED_NORMAL + ,
                                       g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_PARM3 = 4
g.parser.EXPECTED_FOR_STATE_IN_PARM3 = g.parser.EXPECTED_NORMAL + ,
                                       g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_DSNAME1 = 5
g.parser.EXPECTED_FOR_STATE_IN_DSNAME1 = g.parser.EXPECTED_NORMAL + ,
                                         g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_DSNAME2 = 6
g.parser.EXPECTED_FOR_STATE_IN_DSNAME2 = g.parser.EXPECTED_OPEN_BRACKET + ,
                                         g.parser.EXPECTED_CLOSE_BRACKET + ,
                                         g.parser.EXPECTED_DOT
g.parser.SCAN_STATE_IN_DSNAME3 = 7
g.parser.EXPECTED_FOR_STATE_IN_DSNAME3 = g.parser.EXPECTED_NORMAL
g.parser.SCAN_STATE_IN_DSNAME4 = 8
g.parser.EXPECTED_FOR_STATE_IN_DSNAME4 = g.parser.EXPECTED_NORMAL
g.parser.SCAN_STATE_IN_DSNAME5 = 9
g.parser.EXPECTED_FOR_STATE_IN_DSNAME5 = g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_PARM_PARM1 = 10
g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM1 = g.parser.EXPECTED_NORMAL + ,
                                            g.parser.EXPECTED_CLOSE_BRACKET + ,
                                            g.parser.EXPECTED_DOT + ,
                                            g.parser.EXPECTED_ANYTHING_ELSE
g.parser.SCAN_STATE_IN_PARM_PARM2 = 11
g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM2 = g.parser.EXPECTED_NORMAL + ,
                                            g.parser.EXPECTED_CLOSE_BRACKET + ,
                                            g.parser.EXPECTED_DOT + ,
                                            g.parser.EXPECTED_ANYTHING_ELSE

g.parser.scanState = 1
g.parser.scanStateTable.1 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM
g.parser.scanStateTable.2 = g.parser.EXPECTED_FOR_STATE_IN_PARM1
g.parser.scanStateTable.3 = g.parser.EXPECTED_FOR_STATE_IN_PARM2
g.parser.scanStateTable.4 = g.parser.EXPECTED_FOR_STATE_IN_PARM3
g.parser.scanStateTable.5 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME1
g.parser.scanStateTable.6 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME2
g.parser.scanStateTable.7 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME3
g.parser.scanStateTable.8 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME4
g.parser.scanStateTable.9 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME5
g.parser.scanStateTable.10 = g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM1
g.parser.scanStateTable.11 = g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM2

_parmName = ""

CALL initLexer

DO WHILE g.error == 0
   CALL lexerGetToken

   IF g.error /= 0  | g.scanner.currChar == 'EOF' THEN LEAVE

   _parmName = g.lexer.currToken

   g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1
   CALL lexerGetToken
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   SELECT
   WHEN _parmName == 'SYSLIN' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.syslin = _dsname
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single dataset allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'SYSLMOD' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.syslmod = _dsname
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single dataset allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'SYSLIB' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         _nextSyslib = g.syslib.0 + 1
         g.syslib.0 = _nextSyslib
         g.syslib._nextSyslib = _dsname
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   WHEN _parmName == 'OBJECT' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         _nextObject = g.object.0 + 1
         g.object.0 = _nextObject
         g.object._nextObject = _dsname
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   WHEN _parmName == 'LOAD' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         _nextLoad = g.load.0 + 1
         g.load.0 = _nextLoad
         g.load._nextLoad = _dsname
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   WHEN _parmName == 'SYSPRINT' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.sysprint = _dsname
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single dataset allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'PARM' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM2
      DO WHILE g.lexer.currToken /= ')'
         g.parm = g.parm || g.lexer.currToken
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   OTHERWISE
      NOP
   END
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM
END

IF g.syslin == "" THEN DO
   CALL log 'SYSLIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.syslmod == "" THEN DO
   CALL log 'SYSLMOD(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'SYSLIN:   'g.syslin
   SAY 'SYSLMOD:  'g.syslmod
   DO i = 1 TO g.syslib.0
      SAY 'SYSLIB:   'g.syslib.i
   END
   DO i = 1 TO g.object.0
      SAY 'OBJECT:   'g.object.i
   END
   DO i = 1 TO g.load.0
      SAY 'LOAD:     'g.load.i
   END
   SAY 'SYSPRINT: 'g.sysprint
   SAY 'PARM:     'g.parm
END

RETURN

/**********************************************************************/
/* Parse data set name                                                */
/**********************************************************************/
parseDsname: PROCEDURE EXPOSE g. SIGL

_dsname = g.lexer.currToken
DO WHILE g.error == 0
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME2
   CALL lexerGetToken
   IF g.error /= 0 THEN LEAVE
   IF g.lexer.currToken /= '.' THEN LEAVE
   _dsname = _dsname || g.lexer.currToken
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME3
   CALL lexerGetToken
   IF g.error /= 0 THEN LEAVE
   _dsname = _dsname || g.lexer.currToken
   IF g.scanner.peekChar /= '.' & g.scanner.peekChar /= '(' THEN LEAVE
END

IF g.lexer.currToken == '(' THEN DO
   _dsname = _dsname || g.lexer.currToken
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME4
   CALL lexerGetToken
   IF g.error == 0 THEN DO
      _dsname = _dsname || g.lexer.currToken
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME5
      CALL lexerGetToken
   END
   IF g.error == 0 THEN DO
      _dsname = _dsname || g.lexer.currToken
   END
END

RETURN _dsname

/**********************************************************************/
/* Initialize lexer                                                   */
/**********************************************************************/
initLexer: PROCEDURE EXPOSE g. SIGL

g.upperArg = TRANSLATE(g.arg)
g.upperArgLen = LENGTH(g.upperArg)

g.scanner.colIndex = 0

g.lexer.IDENTIFIER_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
g.lexer.IDENTIFIER_STARTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
g.lexer.IDENTIFIER_CHARS = g.lexer.IDENTIFIER_STARTCHARS || "0123456789@#$"

RETURN

/**********************************************************************/
/* Lexer get token                                                    */
/**********************************************************************/
lexerGetToken: PROCEDURE EXPOSE g. SIGL

g.lexer.currToken = ''

CALL scannerGetChar

DO WHILE g.error == 0 & g.scanner.currChar == ' '
   CALL scannerGetChar
END

IF g.error == 0 THEN DO
   _state = g.parser.scanState

   IF g.scanner.currChar == 'EOF' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_EOF))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected end of parameter at pos 'g.scanner.colIndex
         g.error = 8
      END
      SIGNAL lexerGetToken_complete
   END

   IF g.scanner.currChar == '.' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_DOT))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected "." at pos 'g.scanner.colIndex
         g.error = 8
      END
      SIGNAL lexerGetToken_complete
   END

   IF g.scanner.currChar == '(' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_OPEN_BRACKET))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected "(" at pos 'g.scanner.colIndex
         g.error = 8
      END
      SIGNAL lexerGetToken_complete
   END

   IF g.scanner.currChar == ')' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_CLOSE_BRACKET))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected ")" at pos 'g.scanner.colIndex
         g.error = 8
      END
      SIGNAL lexerGetToken_complete
   END

   IF VERIFY(g.scanner.currChar, g.lexer.IDENTIFIER_STARTCHARS) == 0 THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_NORMAL))) == 0 THEN DO
         CALL log 'Unexpected identifier at pos 'g.scanner.colIndex
         g.error = 8
         SIGNAL lexerGetToken_complete
      END
      g.lexer.currToken = g.scanner.currChar
      DO WHILE g.error == 0 & g.scanner.currChar /= 'EOF' & ,
               VERIFY(g.scanner.peekChar, g.lexer.IDENTIFIER_CHARS) == 0
         CALL scannerGetChar

         g.lexer.currToken = g.lexer.currToken || g.scanner.currChar
      END
      SIGNAL lexerGetToken_complete
   END

   _expected = g.parser.scanStateTable._state
   IF C2D(BITAND(D2C(_expected), ,
                 D2C(g.parser.EXPECTED_ANYTHING_ELSE))) /= 0 THEN
     g.lexer.currToken = g.scanner.currChar
     SIGNAL lexerGetToken_complete
   END

   CALL log 'Unexpected character at 'g.scanner.colIndex
   g.error = 8

END

lexerGetToken_complete:

RETURN

/**********************************************************************/
/* Scanner of parameter characters                                    */
/**********************************************************************/
scannerGetChar: PROCEDURE EXPOSE g. SIGL

g.scanner.colIndex = g.scanner.colIndex + 1
g.scanner.peekChar = ''

IF g.scanner.colIndex > g.upperArgLen THEN DO
   g.scanner.currChar = 'EOF'
END
ELSE DO
   g.scanner.currChar = SUBSTR(g.upperArg, g.scanner.colIndex, 1)
   IF g.scanner.colIndex < g.upperArgLen THEN DO
      g.scanner.peekChar = SUBSTR(g.upperArg, g.scanner.colIndex + 1, 1)
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
