/* REXX */
/**********************************************************************/
/* Program    : DFHWS2LS                                              */
/*                                                                    */
/* Description: This program invokes the DFHWS2LS utility to generate */
/*              copybooks from a wsdl and xsd's.                      */
/*                                                                    */
/* Environment: TSO or ISPF                                           */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-SYSTEM(-db2subsys-)--| anything-else |--><         */
/*                                                                    */
/*              db2subsys:     DB2 subsystem id.                      */
/*              anything-else: parameters to pass to the BIND stmt.   */
/*                                                                    */
/* Returns    : 0 when BIND returned 4 or less                        */
/*              8 when REXX error occurs or when parameter string     */
/*                contains syntax error                               */
/*              n any BIND return code > 4                            */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "SYSTEM(DBBG) PACKAGE(MYPACK) MEMBER(MYPROG)" || ,          */
/*        " OWNER(MYOWNER) LIBRARY(MY.DBRM.PDS)"        || ,          */
/*        " QUALIFIER(MYQUAL)"                                        */
/*                                                                    */
/* CALL 'BIND' _par                                                   */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL parseArguments

IF g.error == 0 THEN DO
   CALL makeInputDFHWS2LS

   CALL invokeDFHWS2LS
END

IF g.DFHWS2LS.retcode > 4 | g.DFHWS2LS.retcode < 0 THEN
   g.error = g.DFHWS2LS.retcode

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

CALL log COPIES('*',100)
CALL log '* DFHWS2LS'
CALL log COPIES('*',100)

g.error = 0
g.DFHWS2LS.retcode = 0

g.wsdlpath = ""
g.service  = ""
g.pgmname  = ""
g.reqmem   = ""
g.respmem  = ""
g.pdslib   = ""

RETURN

/**********************************************************************/
/* Make input for DFHWS2LS                                            */
/**********************************************************************/
makeInputDFHWS2LS: PROCEDURE EXPOSE g. SIGL

x=SYSCALLS('ON')

CALL log x

input.1 = 'PDSLIB='g.pdslib
input.2 = 'REQMEM='g.reqmem
input.3 = 'RESPMEM='g.respmem
input.4 = 'LANG=COBOL'
input.5 = 'PGMNAME='g.pgmname
input.6 = 'PGMINT=CHANNEL'
input.7 = 'MAPPING-LEVEL=4.3'
input.8 = 'MINIMUM-RUNTIME-LEVEL=CURRENT'
input.9 = 'LOGFILE='g.wsdlpath'/'g.service'.log'
input.10 = 'WSBIND='g.wsdlpath'/'g.service'.wsbind'
input.11 = 'WSDL='g.wsdlpath'/'g.service'.wsdl'
input.12 = 'CHAR-VARYING=NO'
input.13 = 'INLINE-MAX-OCCURS=1000'
input.14 = 'CCSID=1200'
input.0 = 14

fname = g.wsdlpath'/'g.service'.in'

ADDRESS SYSCALL "open" fname O_WRONLY+O_CREAT+O_TRUNC 664
fd = RETVAL

ADDRESS SYSCALL "chattr" fname ST_CCSID 1047

DO i = 1 TO input.0
   restofrec = input.i
   DO UNTIL restofrec == ''
      IF LENGTH(restofrec) > 72 THEN DO
         rec = SUBSTR(restofrec,1,71) || '*' || X2C(15)
         restofrec = SUBSTR(restofrec,72)
      END
      ELSE DO
         rec = restofrec || X2C(15)
         restofrec = ''
      END
      ADDRESS SYSCALL "write" fd "rec" LENGTH(rec)
   END
END

ADDRESS SYSCALL "close" fd

RETURN

/**********************************************************************/
/* Invoke DFHWS2LS                                                    */
/**********************************************************************/
invokeDFHWS2LS: PROCEDURE EXPOSE g. SIGL

_javadir  = 'java/J8.0_64'
_ussdir   = 'cicsts61'
_service  = ':'
_pathmain = '/usr/lpp/cicsts'

_cmd = '/usr/lpp/cicsts/cicsts61/lib/wsdl/DFHWS2LS'
_cmd = _cmd" '"_javadir"' '"_ussdir"' '"g.wsdlpath"/"g.service"'"
_cmd = _cmd" '"_service"' '"_pathmain"'"

CALL BPXWUNIX _cmd,,_stdout.,_stderr.,,1

CMDRESULT = RESULT

CALL log 'RESULT = 'CMDRESULT

IF CMDRESULT /= 0 THEN DO
   g.error = 8
END

CALL log 'STDOUT'
DO i = 1 TO _stdout.0
   CALL log _stdout.i
END

CALL log 'STDERR'
DO i = 1 TO _stderr.0
   CALL log _stderr.i
END

RETURN

/**********************************************************************/
/* Parse arguments                                                    */
/**********************************************************************/
parseArguments: PROCEDURE EXPOSE g. SIGL

g.parser.EXPECTED_EOF = 1
g.parser.EXPECTED_NORMAL = 2
g.parser.EXPECTED_NORMALWSVC = 4
g.parser.EXPECTED_OPEN_BRACKET = 8
g.parser.EXPECTED_CLOSE_BRACKET = 16
g.parser.EXPECTED_DOT = 32
g.parser.EXPECTED_COMMA = 64
g.parser.EXPECTED_QUOTE = 128
g.parser.EXPECTED_DBLQUOTE = 256
g.parser.EXPECTED_ASTERISK = 512

g.parser.SCAN_STATE_NOT_IN_PARM = 1
g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM = g.parser.EXPECTED_EOF + ,
                                          g.parser.EXPECTED_NORMAL
g.parser.SCAN_STATE_IN_PARM1 = 2
g.parser.EXPECTED_FOR_STATE_IN_PARM1 = g.parser.EXPECTED_OPEN_BRACKET
g.parser.SCAN_STATE_IN_PARM2 = 3
g.parser.EXPECTED_FOR_STATE_IN_PARM2 = g.parser.EXPECTED_NORMAL + ,
                                       g.parser.EXPECTED_CLOSE_BRACKET + ,
                                       g.parser.EXPECTED_QUOTE + ,
                                       g.parser.EXPECTED_DBLQUOTE
g.parser.SCAN_STATE_IN_PARM3 = 4
g.parser.EXPECTED_FOR_STATE_IN_PARM3 = g.parser.EXPECTED_NORMAL + ,
                                       g.parser.EXPECTED_CLOSE_BRACKET + ,
                                       g.parser.EXPECTED_DOT + ,
                                       g.parser.EXPECTED_COMMA + ,
                                       g.parser.EXPECTED_QUOTE + ,
                                       g.parser.EXPECTED_DBLQUOTE + ,
                                       g.parser.EXPECTED_ASTERISK
g.parser.SCAN_STATE_IN_SERVICE1 = 5
g.parser.EXPECTED_FOR_STATE_IN_SERVICE1 = g.parser.EXPECTED_NORMALWSVC
g.parser.SCAN_STATE_IN_SERVICE2 = 6
g.parser.EXPECTED_FOR_STATE_IN_SERVICE2 = g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_DSNAME1 = 7
g.parser.EXPECTED_FOR_STATE_IN_DSNAME1 = g.parser.EXPECTED_NORMAL + ,
                                         g.parser.EXPECTED_CLOSE_BRACKET
g.parser.SCAN_STATE_IN_DSNAME2 = 8
g.parser.EXPECTED_FOR_STATE_IN_DSNAME2 = g.parser.EXPECTED_OPEN_BRACKET + ,
                                         g.parser.EXPECTED_CLOSE_BRACKET + ,
                                         g.parser.EXPECTED_DOT
g.parser.SCAN_STATE_IN_DSNAME3 = 9
g.parser.EXPECTED_FOR_STATE_IN_DSNAME3 = g.parser.EXPECTED_NORMAL
g.parser.SCAN_STATE_IN_DSNAME4 = 10
g.parser.EXPECTED_FOR_STATE_IN_DSNAME4 = g.parser.EXPECTED_NORMAL
g.parser.SCAN_STATE_IN_DSNAME5 = 11
g.parser.EXPECTED_FOR_STATE_IN_DSNAME5 = g.parser.EXPECTED_CLOSE_BRACKET

g.parser.scanState = 1
g.parser.scanStateTable.1 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM
g.parser.scanStateTable.2 = g.parser.EXPECTED_FOR_STATE_IN_PARM1
g.parser.scanStateTable.3 = g.parser.EXPECTED_FOR_STATE_IN_PARM2
g.parser.scanStateTable.4 = g.parser.EXPECTED_FOR_STATE_IN_PARM3
g.parser.scanStateTable.5 = g.parser.EXPECTED_FOR_STATE_IN_SERVICE1
g.parser.scanStateTable.6 = g.parser.EXPECTED_FOR_STATE_IN_SERVICE2
g.parser.scanStateTable.7 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME1
g.parser.scanStateTable.8 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME2
g.parser.scanStateTable.9 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME3
g.parser.scanStateTable.10 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME4
g.parser.scanStateTable.11 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME5

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
   WHEN _parmName == 'SERVICE' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_SERVICE1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.service = g.lexer.currToken

         g.parser.scanState = g.parser.SCAN_STATE_IN_SERVICE2
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single keyword allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'PGMNAME' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.pgmname = g.lexer.currToken
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single keyword allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'REQMEM' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.reqmem = g.lexer.currToken
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single keyword allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'RESPMEM' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.respmem = g.lexer.currToken
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         IF g.lexer.currToken /= ')' THEN DO
            CALL log 'Only single keyword allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   WHEN _parmName == 'PDSLIB' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.pdslib = _dsname
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
   OTHERWISE
      NOP
   END
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM
END

IF g.error == 0 THEN DO
   _lastSlash = LASTPOS('/',g.service)
   g.wsdlpath = SUBSTR(g.service,1,_lastSlash-1)
   g.service = SUBSTR(g.service,_lastSlash+1)
   _lastPeriod = LASTPOS('.',g.service)
   IF _lastPeriod > 0 THEN
      g.service = SUBSTR(g.service,1,_lastPeriod-1)
   CALL log 'WSDLPATH: 'g.wsdlpath
   CALL log 'SERVICE : 'g.service
   CALL log 'PGMNAME : 'g.pgmname
   CALL log 'REQMEM  : 'g.reqmem
   CALL log 'RESPMEM : 'g.respmem
   CALL log 'PDSLIB  : 'g.pdslib
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

g.argLen = LENGTH(g.arg)

g.scanner.colIndex = 0

g.lexer.IDENTIFIER_STARTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
g.lexer.IDENTIFIER_CHARS      = g.lexer.IDENTIFIER_STARTCHARS || ,
                                "0123456789@#$"
g.lexer.IDENTIFIER_STARTCHARS = g.lexer.IDENTIFIER_STARTCHARS || ,
                                "0123456789"
g.lexer.WSVCIDENT_STARTCHARS  = g.lexer.IDENTIFIER_STARTCHARS || ,
                                "abcdefghijklmnopqrstuvwxyz/"
g.lexer.WSVCIDENT_CHARS       = g.lexer.IDENTIFIER_CHARS || '_-./' || ,
                                "abcdefghijklmnopqrstuvwxyz"

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

   IF g.scanner.currChar == "'" THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_QUOTE))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected "''" at pos 'g.scanner.colIndex
         g.error = 8
      END
      SIGNAL lexerGetToken_complete
   END

   IF g.scanner.currChar == '"' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_DBLQUOTE))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected ''"'' at pos 'g.scanner.colIndex
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

   IF VERIFY(g.scanner.currChar, g.lexer.IDENTIFIER_STARTCHARS) == 0 THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_NORMAL))) == 0 & ,
         C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_NORMALWSVC))) == 0 THEN DO
         CALL log 'Unexpected identifier at pos 'g.scanner.colIndex
         g.error = 8
         SIGNAL lexerGetToken_complete
      END
      g.lexer.currToken = g.scanner.currChar
      DO WHILE g.error == 0 & g.scanner.currChar /= 'EOF'
         IF VERIFY(g.scanner.peekChar, g.lexer.IDENTIFIER_CHARS) == 0 THEN DO
            IF C2D(BITAND(D2C(_expected), ,
                          D2C(g.parser.EXPECTED_NORMAL + ,
                              g.parser.EXPECTED_NORMALWSVC))) == 0 THEN
               LEAVE
         END
         ELSE IF VERIFY(g.scanner.peekChar, g.lexer.WSVCIDENT_CHARS) == 0 ,
         THEN DO
            IF C2D(BITAND(D2C(_expected), ,
                          D2C(g.parser.EXPECTED_NORMALWSVC))) == 0 THEN
               LEAVE
         END
         ELSE LEAVE

         CALL scannerGetChar

         g.lexer.currToken = g.lexer.currToken || g.scanner.currChar
      END
      SIGNAL lexerGetToken_complete
   END

   IF VERIFY(g.scanner.currChar, g.lexer.WSVCIDENT_STARTCHARS) == 0 THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), ,
                    D2C(g.parser.EXPECTED_NORMALWSVC))) == 0 THEN DO
         CALL log 'Unexpected identifier at pos 'g.scanner.colIndex
         g.error = 8
         SIGNAL lexerGetToken_complete
      END
      g.lexer.currToken = g.scanner.currChar
      DO WHILE g.error == 0 & g.scanner.currChar /= 'EOF'
         IF VERIFY(g.scanner.peekChar, g.lexer.WSVCIDENT_CHARS) == 0 ,
         THEN DO
            IF C2D(BITAND(D2C(_expected), ,
                          D2C(g.parser.EXPECTED_NORMALWSVC))) == 0 THEN
               LEAVE
         END
         ELSE LEAVE

         CALL scannerGetChar

         g.lexer.currToken = g.lexer.currToken || g.scanner.currChar
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

IF g.scanner.colIndex > g.argLen THEN DO
   g.scanner.currChar = 'EOF'
END
ELSE DO
   g.scanner.currChar = SUBSTR(g.arg, g.scanner.colIndex, 1)
   IF g.scanner.colIndex < g.argLen THEN DO
      g.scanner.peekChar = SUBSTR(g.arg, g.scanner.colIndex + 1, 1)
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
