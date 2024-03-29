/* REXX */
/**********************************************************************/
/* Program    : IEBCOP                                                */
/*                                                                    */
/* Description: This program invokes IEBCOPY to copy one, multiple or */
/*              all members from one PDS(E) to another.               */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-PDSIN(-pdsin-)--PDSOUT(-pdsout-)---------------->  */
/*                                                                    */
/*              >--MEMBER(-member-)--+----------------+------------>  */
/*                                   '-SYSIN(-sysin-)-'               */
/*                                                                    */
/*                                                     .-YES-.        */
/*              >--+----------------+--+---------------+-----+-+--><  */
/*                 '-PRINT(-print-)-'  '-PRINTSUCCESS(-+-NO--+)'      */
/*                                                                    */
/*              pdsin : Input PDS(E) data set.                        */
/*              pdsout: Output PDS(E) data set.                       */
/*              member: IEBCOPY SELECT MEMBER selection list.         */
/*              sysin : Input SYSIN data set.                         */
/*              print : SYSOUT to write IEBCOPY to a sysout file      */
/*                        this is the default                         */
/*                      DD:<ddname> to write IEBCOPY to a ddname      */
/*                        allocated outside this REXX                 */
/*                      anything else is considered a data set name   */
/*                        to receive the IEBCOPY output               */
/*              printsuccess: Copy listing to job log when successful */
/*                            compile YES/NO.                         */
/*                                                                    */
/* Returns    : 0 when IEBCOPY returned 0                             */
/*              8 when REXX error occurs or when parameter string     */
/*                contains syntax error                               */
/*              n any IEBCOPY return code /= 0                        */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "PDSIN(MY.INPUT.PDS)"     || ,                              */
/*        " PDSOUT(MY.OUTPUT.PDS)"  || ,                              */
/*        " MEMBER(MEM1,MEM2,ABC*)"                                   */
/*                                                                    */
/* CALL 'IEBCOPY' _par                                                */
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
   CALL invokeIEBCOPY
END

CALL freeDDs

IF g.IEBCOPY.retcode /= 0 THEN
   g.error = g.IEBCOPY.retcode

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* IEBCOPY'
SAY COPIES('*',100)

g.error = 0
g.IEBCOPY.retcode = 0

g.sysin = ""
g.sysprint = ""
g.pdsin = ""
g.pdsout = ""
g.member = ""
g.print = "SYSOUT"
g.printsuccess = "Y"

g.SYSIN.allocated = 0
g.SYSPRINT.allocated = 0
g.PDSIN.allocated = 0
g.PDSOUT.allocated = 0

RETURN

/**********************************************************************/
/* Allocate DD's for invoking IEBCOPY                                 */
/**********************************************************************/
allocDDs: PROCEDURE EXPOSE g. SIGL

_rc = BPXWDYN("ALLOC DSN('"g.pdsin"') SHR RTDDN(_ddn)")

IF _rc == 0 THEN DO
   g.PDSIN.allocated = 1
   g.PDSIN.ddname = _ddn
END
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of 'g.pdsin' failed with '_rc
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.pdsout"') SHR RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.PDSOUT.allocated = 1
      g.PDSOUT.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.pdsout' failed with '_rc
   END
END

IF g.error == 0 THEN DO
   IF g.sysin /= "" THEN DO
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
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC NEW RECFM(F,B) DSORG(PS) LRECL(80) TRACKS" || ,
                    " SPACE(1,1) RTDDN(_ddn)")

      IF _rc == 0 THEN DO
         g.SYSIN.allocated = 1
         g.SYSIN.ddname = _ddn

         _members.0 = 0
         _unstr = g.member
         PARSE VAR _unstr _onemem","_unstr
         DO UNTIL _onemem == ""
            _nextmemnr = _members.0 + 1
            _members.0 = _nextmemnr
            _members._nextmemnr = _onemem
            PARSE VAR _unstr _onemem","_unstr
         END

         "NEWSTACK"
         _copygroup = "         COPYGROUP OUTDD="g.PDSOUT.ddname
         _copygroup = _copygroup || ",INDD=(("g.PDSIN.ddname",R))"
         QUEUE _copygroup
         IF _members.0 <= 1 THEN DO
            QUEUE "         SELECT MEMBER=("g.member")"
         END
         ELSE DO
            _questr = "         SELECT MEMBER=("_members.1","
            _questr = _questr || COPIES(" ",71-LENGTH(_questr)) || "X"
            QUEUE _questr
            DO i = 2 TO _members.0
               _questr = "                        "_members.i
               IF i < _members.0 THEN DO
                  _questr = _questr || "," || ,
                            COPIES(" ",70-LENGTH(_questr)) || "X"
               END
               ELSE DO
                  _questr = _questr")"
               END
               QUEUE _questr
            END
         END
         "EXECIO "QUEUED()" DISKW "g.SYSIN.ddname" (FINIS"
         _rc = RC
         IF _rc /= 0 THEN DO
            g.error = 8
            CALL log 'EXECIO to SYSIN failed with '_rc
         END
         "DELSTACK"
      END
      ELSE DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Dynamic allocation of SYSIN failed with '_rc
      END
   END
END

IF g.error == 0 THEN DO
   IF g.print == 'SYSOUT' THEN DO
      _rc = BPXWDYN("ALLOC NEW RECFM(V,B,M) DSORG(PS) LRECL(133)" || ,
                    " CYL SPACE(1,1) RTDDN(_ddn)")

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
   ELSE DO
      IF SUBSTR(g.print,1,3) == "DD:" THEN DO
         g.SYSPRINT.ddname = SUBSTR(g.print,4)
      END
   END
END

RETURN

/**********************************************************************/
/* Free DD's after invoking IEBCOPY                                   */
/**********************************************************************/
freeDDs: PROCEDURE EXPOSE g. SIGL

IF g.PDSIN.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.PDSIN.ddname")")

   IF _rc == 0 THEN DO
      g.PDSIN.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.PDSIN.ddname' failed with '_rc
   END
END

IF g.PDSOUT.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.PDSOUT.ddname")")

   IF _rc == 0 THEN DO
      g.PDSOUT.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.PDSOUT.ddname' failed with '_rc
   END
END

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
/* Invoke IEBCOPY                                                     */
/**********************************************************************/
invokeIEBCOPY: PROCEDURE EXPOSE g. SIGL

_prog = 'IEBCOPY'
_parm = ''
_ddlist = COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSIN.ddname,8) || ,
          LEFT(g.SYSPRINT.ddname,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.PDSIN.ddname,8) || ,
          LEFT(g.PDSOUT.ddname,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8)

ADDRESS LINKMVS _prog '_parm _ddlist'

g.IEBCOPY.retcode = RC

IF g.printsuccess == 'Y' | g.IEBCOPY.retcode /= 0 THEN DO
   "EXECIO * DISKR "g.SYSPRINT.ddname" (STEM _sysprint. FINIS"

   _rc = RC

   IF _rc == 0 THEN DO
      IF g.print == "SYSOUT" | ,
         g.IEBCOPY.retcode /= 0 THEN DO
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
g.parser.EXPECTED_ASTERISK = 32
g.parser.EXPECTED_COMMA = 64
g.parser.EXPECTED_COLON = 128

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
g.parser.SCAN_STATE_IN_MEMBER1 = 10
g.parser.EXPECTED_FOR_STATE_IN_MEMBER1 = g.parser.EXPECTED_NORMAL + ,
                                         g.parser.EXPECTED_OPEN_BRACKET + ,
                                         g.parser.EXPECTED_CLOSE_BRACKET + ,
                                         g.parser.EXPECTED_DOT + ,
                                         g.parser.EXPECTED_ASTERISK
g.parser.SCAN_STATE_IN_MEMBER2 = 11
g.parser.EXPECTED_FOR_STATE_IN_MEMBER2 = g.parser.EXPECTED_NORMAL + ,
                                         g.parser.EXPECTED_OPEN_BRACKET + ,
                                         g.parser.EXPECTED_CLOSE_BRACKET + ,
                                         g.parser.EXPECTED_DOT + ,
                                         g.parser.EXPECTED_ASTERISK + ,
                                         g.parser.EXPECTED_COMMA
g.parser.SCAN_STATE_IN_PRINT1 = 12
g.parser.EXPECTED_FOR_STATE_IN_PRINT1 = g.parser.EXPECTED_NORMAL + ,
                                        g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_PRINT2 = 13
g.parser.EXPECTED_FOR_STATE_IN_PRINT2 = g.parser.EXPECTED_COLON + ,
                                        g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_PRINT3 = 14
g.parser.EXPECTED_FOR_STATE_IN_PRINT3 = g.parser.EXPECTED_NORMAL + ,
                                        g.parser.EXPECTED_CLOSE_BRACKET

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
g.parser.scanStateTable.10 = g.parser.EXPECTED_FOR_STATE_IN_MEMBER1
g.parser.scanStateTable.11 = g.parser.EXPECTED_FOR_STATE_IN_MEMBER2
g.parser.scanStateTable.12 = g.parser.EXPECTED_FOR_STATE_IN_PRINT1
g.parser.scanStateTable.13 = g.parser.EXPECTED_FOR_STATE_IN_PRINT2
g.parser.scanStateTable.14 = g.parser.EXPECTED_FOR_STATE_IN_PRINT3

_parmName = ""

CALL initLexer

DO WHILE g.error == 0
   CALL lexerGetToken

   IF g.error /= 0   | g.scanner.currChar == 'EOF' THEN LEAVE

   _parmName = g.lexer.currToken

   g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1
   CALL lexerGetToken
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   SELECT
   WHEN _parmName == 'PDSIN' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.pdsin = _dsname
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
   WHEN _parmName == 'PDSOUT' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.pdsout = _dsname
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
   WHEN _parmName == 'MEMBER' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_MEMBER1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      _member = ""
      DO WHILE g.lexer.currToken /= ')'
         _member = _member || g.lexer.currToken
         g.member = _member
         g.parser.scanState = g.parser.SCAN_STATE_IN_MEMBER2
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
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
   WHEN _parmName == 'PRINT' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PRINT1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      _print = ""
      DO WHILE g.lexer.currToken /= ')'
         _print = _print || g.lexer.currToken
         g.print = _print
         IF _print == 'DD' THEN DO
            g.parser.scanState = g.parser.SCAN_STATE_IN_PRINT2
         END
         ELSE DO
            g.parser.scanState = g.parser.SCAN_STATE_IN_PRINT3
         END
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

IF g.pdsin == "" THEN DO
   CALL log 'PDSIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.pdsout == "" THEN DO
   CALL log 'PDSOUT(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'PDSIN:        'g.pdsin
   SAY 'PDSOUT:       'g.pdsout
   SAY 'MEMBER:       'g.member
   SAY 'SYSIN:        'g.sysin
   SAY 'PRINT:        'g.print
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

   IF g.scanner.currChar == '*' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_ASTERISK))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected "*" at pos 'g.scanner.colIndex
         g.error = 8
      END
      SIGNAL lexerGetToken_complete
   END

   IF g.scanner.currChar == ':' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_COLON))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected ":" at pos 'g.scanner.colIndex
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

         IF g.scanner.currChar /= 'EOF' THEN DO
            g.lexer.currToken = g.lexer.currToken || g.scanner.currChar
         END
      END
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
