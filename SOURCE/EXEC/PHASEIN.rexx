/* REXX */
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL parseArguments

IF g.error == 0 THEN DO
   CALL initAPI
END

IF g.error == 0 THEN DO
   CALL connect
END

IF g.error == 0 THEN DO
   CALL getProgram
END

IF g.error == 0 & g.count > 0 THEN DO
   CALL phasein
END

IF g.connected == 1 THEN DO
   CALL disconnect
END

IF g.initialized == 1 THEN DO
   CALL termAPI
END

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* PHASEIN'
SAY COPIES('*',100)

g.error = 0

g.initialized = 0
g.connected = 0
g.count = 0

g.CONTEXT = ''
g.SCOPE = ''
g.VERSION = '0530'
g.programs.0 = 0

RETURN

/**********************************************************************/
/* Initialize CPSM API                                                */
/**********************************************************************/
initAPI: PROCEDURE EXPOSE g. SIGL

SAY 'Initializing API...'

XX = EYUINIT()

IF XX = 0 THEN DO
   g.initialized = 1
END
ELSE DO
   CALL log 'ERROR INITIALIZING API'
   g.error = 8
END

RETURN

/**********************************************************************/
/* Terminate CPSM API                                                 */
/**********************************************************************/
termAPI: PROCEDURE EXPOSE g. SIGL

XX = EYUTERM()

IF XX /= 0 THEN DO
   CALL log 'ERROR TERMINATING API'
   g.error = 8
END

RETURN

/**********************************************************************/
/* Establish a connection                                             */
/**********************************************************************/
connect: PROCEDURE EXPOSE g. SIGL

Say 'Establishing connection...'

XX = EYUAPI('CONNECT' ,
            'CONTEXT('g.CONTEXT')' ,
            'SCOPE('g.SCOPE')' ,
            'VERSION('g.VERSION')' ,
            'THREAD(g.THREAD)' ,
            'RESPONSE(W_RESPONSE)' ,
            'REASON(W_REASON)')

IF XX == 0 & W_RESPONSE == 1024 THEN DO
   g.connected = 1
END
ELSE DO
   CALL log "ERROR CONNECTING W_RESPONSE("W_RESPONSE") W_REASON(" || ,
            W_REASON")"
   g.error = 8
END

RETURN

/**********************************************************************/
/* Disconnect                                                         */
/**********************************************************************/
disconnect: PROCEDURE EXPOSE g. SIGL

Say 'Sever connection...'

XX = EYUAPI('TERMINATE RESPONSE(W_RESPONSE) REASON(W_REASON)')

IF XX /= 0 | W_RESPONSE /= 1024 THEN DO
   CALL log "ERROR TERMINATING CONNECTION W_RESPONSE("W_RESPONSE || ,
            ") W_REASON("W_REASON")"
   g.error = 8
END

RETURN

/**********************************************************************/
/* Get program resource                                               */
/**********************************************************************/
getProgram: PROCEDURE EXPOSE g. SIGL

Say 'Get the PROGRAM resource table...'

W_CRITERIA = 'PROGRAM='g.programs.1
IF g.programs.0 > 1 THEN DO
   DO I = 2 TO g.programs.0
      W_CRITERIA = W_CRITERIA || ' OR PROGRAM='g.programs.I
   END
END
W_CRITERIA = W_CRITERIA || '.'

W_CRITERIALEN = 'LENGTH'(W_CRITERIA)

XX = EYUAPI('GET OBJECT(PROGRAM)' ,
                'CRITERIA(W_CRITERIA)' ,
                'LENGTH('W_CRITERIALEN')' ,
                'COUNT(W_COUNT)' ,
                'RESULT(g.RESULT)' ,
                'THREAD(g.THREAD)' ,
                'RESPONSE(W_RESPONSE)' ,
                'REASON(W_REASON)')

IF XX == 0 & W_RESPONSE == 1024 THEN DO
   g.count = W_COUNT
   SAY "Count: "g.COUNT
END
ELSE DO
   IF XX == 0 & (W_RESPONSE == 1027 | W_RESPONSE == 1034) THEN DO
      g.count = 0
   END
   ELSE DO
      CALL log "ERROR GET OBJECT W_RESPONSE("W_RESPONSE || ,
               ") W_REASON("W_REASON")"
      g.error = 8
   END
END

RETURN

/**********************************************************************/
/* Perform PHASEIN                                                    */
/**********************************************************************/
phasein: PROCEDURE EXPOSE g. SIGL

Say 'Perform PHASEIN...'

XX = EYUAPI('PERFORM SET ACTION(PHASEIN)',
                'RESULT(g.RESULT)' ,
                'THREAD(g.THREAD)' ,
                'RESPONSE(W_RESPONSE)' ,
                'REASON(W_REASON)')

IF XX /= 0 | W_RESPONSE /= 1024 THEN DO
   CALL log "ERROR PERFORMING ACTION(PHASEIN) W_RESPONSE(" || ,
            W_RESPONSE") W_REASON("W_REASON")"
   g.error = 8
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
g.parser.SCAN_STATE_IN_PARM_PARM1 = 5
g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM1 = g.parser.EXPECTED_NORMAL + ,
                                            g.parser.EXPECTED_CLOSE_BRACKET + ,
                                            g.parser.EXPECTED_DOT + ,
                                            g.parser.EXPECTED_ANYTHING_ELSE
g.parser.SCAN_STATE_IN_PARM_PARM2 = 6
g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM2 = g.parser.EXPECTED_NORMAL + ,
                                            g.parser.EXPECTED_CLOSE_BRACKET + ,
                                            g.parser.EXPECTED_DOT + ,
                                            g.parser.EXPECTED_ANYTHING_ELSE

g.parser.scanState = 1
g.parser.scanStateTable.1 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM
g.parser.scanStateTable.2 = g.parser.EXPECTED_FOR_STATE_IN_PARM1
g.parser.scanStateTable.3 = g.parser.EXPECTED_FOR_STATE_IN_PARM2
g.parser.scanStateTable.4 = g.parser.EXPECTED_FOR_STATE_IN_PARM3
g.parser.scanStateTable.5 = g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM1
g.parser.scanStateTable.6 = g.parser.EXPECTED_FOR_STATE_IN_PARM_PARM2

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
   WHEN _parmName == 'PROGRAMS' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM2
      DO WHILE g.lexer.currToken /= ')'
         _nextProgram = g.programs.0 + 1
         g.programs.0 = _nextProgram
         g.programs._nextProgram = g.lexer.currToken
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   WHEN _parmName == 'CONTEXT' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM2
      DO WHILE g.lexer.currToken /= ')'
         g.CONTEXT = g.CONTEXT || g.lexer.currToken
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   WHEN _parmName == 'SCOPE' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM2
      DO WHILE g.lexer.currToken /= ')'
         g.SCOPE = g.SCOPE || g.lexer.currToken
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

IF g.CONTEXT == "" THEN DO
   CALL log 'CONTEXT(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.SCOPE == "" THEN DO
   CALL log 'SCOPE(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'CONTEXT: 'g.CONTEXT
   SAY 'SCOPE  : 'g.SCOPE
   DO i = 1 TO g.programs.0
      SAY 'PROGRAM: 'g.programs.i
   END
END

RETURN

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
