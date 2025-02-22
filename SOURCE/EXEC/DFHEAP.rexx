/* REXX */
/**********************************************************************/
/* Program    : DFHEAP                                                */
/*                                                                    */
/* Description: This program invokes DFHEAP1$ to translate CICS       */
/*              commands into regular assembler code.                 */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-SYSIN(-source-)--SYSPUNCH(-xlated-)------------->  */
/*                                                                    */
/*              >--+---------------------+--+----------------+--><    */
/*                 '-SYSPRINT(-listing-)-'  '-PARM(-params-)-'        */
/*                                                                    */
/*                                 .-YES-.                            */
/*              >--+---------------+-----+-+--><                      */
/*                 '-PRINTSUCCESS(-+-NO--+)'                          */
/*                                                                    */
/*              source : Input assembler source data set.             */
/*              xlated : Output translated assembler source data set. */
/*              listing: Output translation listing data set.         */
/*              params : Parameters to DFHEAP1$.                      */
/*              printsuccess: Copy listing to job log when successful */
/*                            translate YES/NO.                       */
/*                                                                    */
/* Returns    : 0 when DFHEAP1$ returned 4 or less                    */
/*              8 when REXX error occurs or when parameter string     */
/*                contains syntax error                               */
/*              n any DFHEAP1$ return code > 4                        */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "SYSIN(MY.ASM.PDS(MEMBER))"            || ,                 */
/*        " SYSPUNCH(MY.XLATED.ASM.PDS(MEMBER))" || ,                 */
/*        " SYSPRINT(MY.LST.PDS(MEMBER))"        || ,                 */
/*        " PARM(NOPROLOG,NOEPILOG)"                                  */
/*                                                                    */
/* CALL 'DFHEAP' _par                                                 */
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
   CALL invokeDFHEAP1$
END

CALL freeDDs

IF g.DFHEAP1$.retcode > 4 | g.DFHEAP1$.retcode < 0 THEN
   g.error = g.DFHEAP1$.retcode

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* DFHEAP'
SAY COPIES('*',100)

g.error = 0
g.DFHEAP1$.retcode = 0

g.sysin = ""
g.syspunch = ""
g.sysprint = ""
g.parm = ""
g.printsuccess = "Y"

g.SYSIN.allocated = 0
g.SYSPUNCH.allocated = 0
g.SYSPRINT.allocated = 0

RETURN

/**********************************************************************/
/* Allocate DD's for invoking DFHEAP1$                                */
/**********************************************************************/
allocDDs: PROCEDURE EXPOSE g. SIGL

_rc = BPXWDYN("ALLOC DSN('"g.sysin"') SHR RTDDN(_ddn)")

IF _rc == 0 THEN DO
   g.SYSIN.allocated = 1
   g.SYSIN.ddname = _ddn
END
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of 'g.sysin' failed with '_rc
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.syspunch"') SHR RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSPUNCH.allocated = 1
      g.SYSPUNCH.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.syspunch' failed with '_rc
   END
END

IF g.error == 0 THEN DO
   IF g.sysprint == "" THEN DO
      _rc = BPXWDYN("ALLOC NEW RECFM(V,B,M) DSORG(PS) LRECL(133)" || ,
                    " CYL SPACE(1,1) RTDDN(_ddn)")
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC DSN('"g.sysprint"') SHR RTDDN(_ddn)")
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

RETURN

/**********************************************************************/
/* Free DD's after invoking DFHEAP1$                                  */
/**********************************************************************/
freeDDs: PROCEDURE EXPOSE g. SIGL

IF g.SYSIN.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSIN.ddname")")

   IF _rc == 0 THEN DO
      g.SYSIN.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSIN.ddname' failed with '_rc
   END
END

IF g.SYSPUNCH.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSPUNCH.ddname")")

   IF _rc == 0 THEN DO
      g.SYSPUNCH.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSPUNCH.ddname' failed with '_rc
   END
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

RETURN

/**********************************************************************/
/* Invoke DFHEAP1$                                                    */
/**********************************************************************/
invokeDFHEAP1$: PROCEDURE EXPOSE g. SIGL

_prog = 'DFHEAP1$'
_parm = g.parm
_ddlist = COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSIN.ddname,8) || ,
          LEFT(g.SYSPRINT.ddname,8) || ,
          LEFT(g.SYSPUNCH.ddname,8)

ADDRESS LINKMVS _prog '_parm _ddlist'

g.DFHEAP1$.retcode = RC

IF g.printsuccess == 'Y' | g.DFHEAP1$.retcode /= 0 THEN DO
   "EXECIO * DISKR "g.SYSPRINT.ddname" (STEM _sysprint. FINIS"

   _rc = RC

   IF _rc == 0 THEN DO
      _rc = BPXWDYN("ALLOC SYSOUT(A) RTDDN(_ddn)")

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
g.parser.EXPECTED_COMMA = 32

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
                                            g.parser.EXPECTED_DOT
g.parser.SCAN_STATE_IN_PARM_PARM2 = 11
g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM2 = g.parser.EXPECTED_NORMAL + ,
                                            g.parser.EXPECTED_CLOSE_BRACKET + ,
                                            g.parser.EXPECTED_DOT + ,
                                            g.parser.EXPECTED_COMMA

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
   WHEN _parmName == 'SYSIN' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.sysin = _dsname
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
   WHEN _parmName == 'SYSPUNCH' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.syspunch = _dsname
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
   WHEN _parmName == 'PRINTSUCCESS' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      IF TRANSLATE(g.lexer.currToken) == 'NO' THEN DO
         g.printsuccess = 'N'
      END
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
      DO WHILE g.lexer.currToken /= ')'
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

IF g.sysin == "" THEN DO
   CALL log 'SYSIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.syspunch == "" THEN DO
   CALL log 'SYSPUNCH(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'SYSIN:        'g.sysin
   SAY 'SYSPUNCH:     'g.syspunch
   SAY 'SYSPRINT:     'g.sysprint
   SAY 'PARM:         'g.parm
   SAY 'PRINTSUCCESS: 'g.printsuccess
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

   IF g.scanner.currChar == ',' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_COMMA))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected "," at pos 'g.scanner.colIndex
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

   CALL log "Unexpected character '"g.scanner.currChar"' at "g.scanner.colIndex
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
