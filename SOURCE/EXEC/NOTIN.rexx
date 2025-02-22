/* REXX */
/**********************************************************************/
/* Program    : NOTIN                                                 */
/*                                                                    */
/* Description: This program compares two lists of items and returns  */
/*              the items found in the first list, but not in the     */
/*              second.                                               */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-SET1(-items1-)--SET2(-items2-)--><                 */
/*                                                                    */
/*              items1: space delimited list of items.                */
/*              items2: space delimited list of items.                */
/*                                                                    */
/* Returns    : list of items only in SET1                            */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "SET1(A B C D E)"                  || ,                     */
/*        " SET2(A C E)"                     || ,                     */
/*                                                                    */
/* CALL 'NOTIN' _par                                                  */
/*                                                                    */
/* _rc = RESULT  <-- would be "B D"                                   */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL parseArguments

IF g.error == 0 THEN DO
   CALL filterSets
END

SAY 'RETURN:       'g.retvalue

RETURN g.retvalue

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* NOTIN'
SAY COPIES('*',100)

g.error = 0
g.retvalue = ""

g.set1 = ""
g.set2 = ""

RETURN

/**********************************************************************/
/* Filter sets                                                        */
/**********************************************************************/
filterSets: PROCEDURE EXPOSE g. SIGL

_set1 = g.set1
_set2 = g.set2
_s1.0 = 0
_s2.0 = 0

DO WHILE _set1 <> ''
   PARSE VAR _set1 _word _set1

   DO I = 1 TO _s1.0
      IF _s1.I > _word THEN LEAVE
   END

   IF I <= _s1.0 THEN DO
      DO J = _s1.0 TO I BY -1
         nextJ = J + 1
         _s1.nextJ = _s1.J
      END
   END

   _s1.I = _word

   _s1.0 = _s1.0 + 1
END

DO WHILE _set2 <> ''
   PARSE VAR _set2 _word _set2

   DO I = 1 TO _s2.0
      IF _s2.I > _word THEN LEAVE
   END

   IF I <= _s2.0 THEN DO
      DO J = _s2.0 TO I BY -1
         nextJ = J + 1
         _s2.nextJ = _s2.J
      END
   END

   _s2.I = _word

   _s2.0 = _s2.0 + 1
END

IF _s1.0 == 0
THEN I = -1
ELSE I = 1

IF _s2.0 == 0
THEN J = -1
ELSE J = 1

IF I < 0 & J < 0 THEN RETURN

IF I >= 0 & J >= 0 THEN DO
   IF _s1.I <= _s2.J
   THEN _lowest = _s1.I
   ELSE _lowest = _s2.J
END
ELSE DO
   IF J < 0
   THEN _lowest = _s1.I
   ELSE _lowest = _s2.J
END

DO UNTIL I < 0 & J < 0
   IF _s1.I == _lowest & _s2.J /= _lowest THEN DO
      IF g.retvalue == ""
      THEN g.retvalue = _s1.I
      ELSE g.retvalue = g.retvalue" "_s1.I
   END

   IF _s1.I == _lowest THEN DO
      IF I < _s1.0
      THEN I = I + 1
      ELSE I = -1
   END

   IF _s2.J == _lowest THEN DO
      IF J < _s2.0
      THEN J = J + 1
      ELSE J = -1
   END

   IF I < 0 & J < 0 THEN LEAVE

   IF I >= 0 & J >= 0 THEN DO
      IF _s1.I <= _s2.J
      THEN _lowest = _s1.I
      ELSE _lowest = _s2.J
   END
   ELSE DO
      IF J < 0
      THEN _lowest = _s1.I
      ELSE _lowest = _s2.J
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

g.parser.scanState = 1
g.parser.scanStateTable.1 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM
g.parser.scanStateTable.2 = g.parser.EXPECTED_FOR_STATE_IN_PARM1
g.parser.scanStateTable.3 = g.parser.EXPECTED_FOR_STATE_IN_PARM2
g.parser.scanStateTable.4 = g.parser.EXPECTED_FOR_STATE_IN_PARM3

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
   WHEN _parmName == 'SET1' THEN DO
      _openBracketCount = 0
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      IF g.lexer.currToken /= ')' THEN DO
         g.set1 = g.lexer.currToken
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
      DO WHILE g.lexer.currToken /= ')' | _openBracketCount > 0
         g.set1 = g.set1" "g.lexer.currToken

         IF g.lexer.currToken == '(' THEN DO
            _openBracketCount = _openBracketCount + 1
         END
         IF g.lexer.currToken == ')' THEN DO
            _openBracketCount = _openBracketCount - 1
         END

         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
   END
   WHEN _parmName == 'SET2' THEN DO
      _openBracketCount = 0
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      IF g.lexer.currToken /= ')' THEN DO
         g.set2 = g.lexer.currToken
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      END
      DO WHILE g.lexer.currToken /= ')' | _openBracketCount > 0
         g.set2 = g.set2" "g.lexer.currToken

         IF g.lexer.currToken == '(' THEN DO
            _openBracketCount = _openBracketCount + 1
         END
         IF g.lexer.currToken == ')' THEN DO
            _openBracketCount = _openBracketCount - 1
         END

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

IF g.error == 0 & g.syslin == "" THEN DO
   CALL log 'SYSLIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'SET1:         'g.set1
   SAY 'SET2:         'g.set2
END

RETURN

/**********************************************************************/
/* Initialize lexer                                                   */
/**********************************************************************/
initLexer: PROCEDURE EXPOSE g. SIGL

g.upperArg = TRANSLATE(g.arg)
g.upperArgLen = LENGTH(g.upperArg)

g.scanner.colIndex = 0

g.lexer.IDENTIFIER_STARTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
g.lexer.IDENTIFIER_CHARS = g.lexer.IDENTIFIER_STARTCHARS || "0123456789@#$"
g.lexer.IDENTIFIER_STARTCHARS = g.lexer.IDENTIFIER_CHARS

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
