/* REXX */
/**********************************************************************/
/* Program    : UPDCOPY                                               */
/*                                                                    */
/* Description: This program copies an input data set to an output    */
/*              data set and performs one or more search and replace  */
/*              actions during copying.                               */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-DSIN(-dsin-)--DSOUT(-dsout-)-------------------->  */
/*                                                                    */
/*              >--UPDWITH(-updwith-)--><                             */
/*                                                                    */
/*              dsin   : Input data set.                              */
/*              dsout  : Output data set.                             */
/*              updwith: Input search and replace data set.           */
/*                                                                    */
/* Format     : The search and replace data set can supply one or     */
/*              more lines in the following format:                   */
/*                                                                    */
/*                >search_text>replace_text>                          */
/*                                                                    */
/*              For example the following set of search and replace   */
/*              lines:                                                */
/*                                                                    */
/*                >{variableHLQ}>MYUSR>                               */
/*                >{dsType}>SRC>                                      */
/*                                                                    */
/*              would cause this input text:                          */
/*                                                                    */
/*       //COPYDEL EXEC PGM=IDCAMS                                    */
/*       //SYSIN     DD *                                             */
/*         PRINT INDATASET({variableHLQ}.INPUT.{dsType})              */
/*         DELETE ({variableHLQ}.INPUT.{dsType}) SCRATCH PURGE        */
/*       //SYSPRINT  DD SYSOUT=*                                      */
/*                                                                    */
/*              to be turned into this output text:                   */
/*                                                                    */
/*       //COPYDEL EXEC PGM=IDCAMS                                    */
/*       //SYSIN     DD *                                             */
/*         PRINT INDATASET(MYUSR.INPUT.SRC)                           */
/*         DELETE (MYUSR.INPUT.SRC) SCRATCH PURGE                     */
/*       //SYSPRINT  DD SYSOUT=*                                      */
/*                                                                    */
/* Returns    : 0 when UPDCOPY successful                             */
/*              8 when UPDCOPY unsuccessful                           */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "DSIN(MY.INPUT.PDS(MEM1))"         || ,                     */
/*        " DSOUT(MY.OUTPUT.PDS(MEM1))"      || ,                     */
/*        " UPDWITH(MY.UPDCOPY.INPUT(SET1))"                          */
/*                                                                    */
/* CALL 'UPDCOPY' _par                                                */
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
   CALL parseUpd
END

IF g.error == 0 THEN DO
   CALL updCopy
END

CALL freeDDs

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* UPDCOPY'
SAY COPIES('*',100)

g.error = 0

g.dsin = ""
g.dsout = ""
g.updwith = ""

g.DSIN.allocated = 0
g.DSOUT.allocated = 0
g.UPDWITH.allocated = 0

g.updValues.0 = 0

RETURN

/**********************************************************************/
/* Allocate DD's for copying                                          */
/**********************************************************************/
allocDDs: PROCEDURE EXPOSE g. SIGL

_rc = BPXWDYN("ALLOC DSN('"g.dsin"') SHR RTDDN(_ddn)")

IF _rc == 0 THEN DO
   g.DSIN.allocated = 1
   g.DSIN.ddname = _ddn
END
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of 'g.dsin' failed with '_rc
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.dsout"') SHR RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.DSOUT.allocated = 1
      g.DSOUT.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.dsout' failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DSN('"g.updwith"') SHR RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.UPDWITH.allocated = 1
      g.UPDWITH.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of 'g.updwith' failed with '_rc
   END
END

RETURN

/**********************************************************************/
/* Free DD's after invoking copying                                   */
/**********************************************************************/
freeDDs: PROCEDURE EXPOSE g. SIGL

IF g.DSIN.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.DSIN.ddname")")

   IF _rc == 0 THEN DO
      g.DSIN.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.DSIN.ddname' failed with '_rc
   END
END

IF g.DSOUT.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.DSOUT.ddname")")

   IF _rc == 0 THEN DO
      g.DSOUT.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.DSOUT.ddname' failed with '_rc
   END
END

IF g.UPDWITH.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.UPDWITH.ddname")")

   IF _rc == 0 THEN DO
      g.UPDWITH.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.UPDWITH.ddname' failed with '_rc
   END
END

RETURN

/**********************************************************************/
/* Parse update statements                                            */
/**********************************************************************/
parseUpd: PROCEDURE EXPOSE g. SIGL

"EXECIO * DISKR "g.UPDWITH.ddname" (STEM _updwith. FINIS"

IF RC == 0 THEN DO
   DO i = 1 TO _updwith.0
      _var = SUBSTR(_updwith.i,1,72)
      PARSE VAR _var '>'_replaceWhat'>'_replaceWith'>'
      _nextUpdValue = g.updValues.0 + 1
      g.updValues._nextUpdValue.replaceWhat = _replaceWhat
      g.updValues._nextUpdValue.replaceWith = _replaceWith
      g.updValues.0 = _nextUpdValue
   END
END
ELSE DO
   g.error = 8
   CALL log 'Error reading UPDWITH data set 'RC
END

RETURN

/**********************************************************************/
/* Update and copy                                                    */
/**********************************************************************/
updCopy: PROCEDURE EXPOSE g. SIGL

"NEWSTACK"

"EXECIO * DISKR "g.DSIN.ddname

DO WHILE g.error = 0 & QUEUED() > 0
   PARSE PULL _line

   _linelength = LENGTH(_line)

   _replacementDone = 1

   DO i = 1 TO 100 WHILE g.error = 0 & _replacementDone = 1
      _replacementDone = 0

      DO j = 1 TO g.updValues.0 WHILE g.error = 0
         _newline = ""

         _start = 1

         _found = INDEX(_line, g.updValues.j.replaceWhat, _start)

         DO WHILE g.error = 0 & _found > 0
            _replacementDone = 1

            IF _found > 1 THEN DO
               _newline = _newline || SUBSTR(_line,_start,_found-1)
            END
            _newline = _newline || g.updValues.j.replaceWith

            _start = _found + LENGTH(g.updValues.j.replaceWhat)

            _found = INDEX(_line, g.updValues.j.replaceWhat, _start)
         END

         IF g.error = 0 THEN DO
            _newline = _newline || SUBSTR(_line,_start)
            _line = _newline
         END
      END
   END

   IF g.error = 0 THEN DO
      PUSH SUBSTR(_line,1,_linelength)
      "EXECIO 1 DISKW "g.DSOUT.ddname
   END
END

"EXECIO 0 DISKR "g.DSIN.ddname" (FINIS"
"EXECIO 0 DISKW "g.DSOUT.ddname" (FINIS"

"DELSTACK"

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

   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE

   _parmName = g.lexer.currToken

   g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1
   CALL lexerGetToken
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   SELECT
   WHEN _parmName == 'DSIN' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.dsin = _dsname
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
   WHEN _parmName == 'DSOUT' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.dsout = _dsname
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
   WHEN _parmName == 'UPDWITH' THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      DO WHILE g.lexer.currToken /= ')'
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         _dsname = parseDsname()
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.updwith = _dsname
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

IF g.dsin == "" THEN DO
   CALL log 'DSIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.dsout == "" THEN DO
   CALL log 'DSOUT(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.updwith == "" THEN DO
   CALL log 'UPDWITH(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'DSIN:    'g.dsin
   SAY 'DSOUT:   'g.dsout
   SAY 'UPDWITH: 'g.updwith
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
