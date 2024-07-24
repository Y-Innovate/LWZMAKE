/* REXX */
/**********************************************************************/
/* Program    : ZPARMLKD                                              */
/*                                                                    */
/* Description: This program generates IEWL link edit input control   */
/*              statements to link edit DB2 ZPARM load modules.       */
/*                                                                    */
/* Environment: ISPF                                                  */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>--'loadmod'--'mvs_data_set_name(member_name)'-->    */
/*                                                                    */
/* Returns    : 0 when file copies successfully                       */
/*              8 when there was an error                             */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "'D1XCPARM' 'MY.LKED.PDS(MEMBER)'                           */
/*                                                                    */
/* CALL 'ZPARMLKD' _par                                               */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL parseArguments

IF g.error == 0 THEN DO
   CALL do_createmem
END

IF g.error == 0 & g.notouch == 0 THEN DO
   CALL do_touch
END

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* ZPARMLKD'
SAY COPIES('*',100)

g.error = 0

g.loadmod = ""
g.pdsmem = ""

RETURN

/**********************************************************************/
/* Create a member with link edit control statements                  */
/**********************************************************************/
do_createmem: PROCEDURE EXPOSE g. SIGL

lines.1 = " INCLUDE OBJECT("g.loadmod")"
lines.2 = " INCLUDE SYSLIB(DSNZPARM)"
lines.3 = " ORDER DSNAA"
lines.4 = " INCLUDE SYSLIB(DSNAA)"
lines.5 = " ENTRY   DSNZMSTR"
lines.6 = " MODE    AMODE(31),RMODE(ANY)"
lines.7 = " NAME    "g.loadmod"(R)"

_rc = BPXWDYN("ALLOC DSN('"g.pdsmem"') SHR RTDDN(_ddn)")

If _rc == 0 Then Do
   Do I = 1 To 7
      Push lines.I

      "EXECIO 1 DISKW "_ddn
   End

   "EXECIO 0 DISKW "_ddn" (FINIS"

   _rc = BPXWDYN("FREE FI("_ddn")")
End
ELSE DO
   g.error = 8
   IF _rc > 0 THEN _rc = D2X(_rc)
   CALL log 'Dynamic allocation of 'g.pdsmem' failed with '_rc
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
g.parser.EXPECTED_QUOTE = 16
g.parser.EXPECTED_DOT = 32
g.parser.EXPECTED_ANYTHING_ELSE = 64

g.parser.SCAN_STATE_NOT_IN_PARM = 1
g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM = g.parser.EXPECTED_EOF + ,
                                          g.parser.EXPECTED_QUOTE

g.parser.SCAN_STATE_NOT_IN_PARM2 = 2
g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM2 = g.parser.EXPECTED_EOF + ,
                                           g.parser.EXPECTED_NORMAL

g.parser.SCAN_STATE_IN_PARM1 = 3
g.parser.EXPECTED_FOR_STATE_IN_PARM1 = g.parser.EXPECTED_NORMAL + ,
                                       g.parser.EXPECTED_DOT + ,
                                       g.parser.EXPECTED_ANYTHING_ELSE
g.parser.SCAN_STATE_IN_PARM2 = 4
g.parser.EXPECTED_FOR_STATE_IN_PARM2 = g.parser.EXPECTED_NORMAL + ,
                                       g.parser.EXPECTED_DOT + ,
                                       g.parser.EXPECTED_ANYTHING_ELSE + ,
                                       g.parser.EXPECTED_QUOTE
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

g.parser.scanState = 1
g.parser.scanStateTable.1 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM
g.parser.scanStateTable.2 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM2
g.parser.scanStateTable.3 = g.parser.EXPECTED_FOR_STATE_IN_PARM1
g.parser.scanStateTable.4 = g.parser.EXPECTED_FOR_STATE_IN_PARM2
g.parser.scanStateTable.5 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME1
g.parser.scanStateTable.6 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME2
g.parser.scanStateTable.7 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME3
g.parser.scanStateTable.8 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME4
g.parser.scanStateTable.9 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME5

_parmName = ""

CALL initLexer

CALL lexerGetToken /* Should be quote */

IF g.error == 0 & g.scanner.currChar == 'EOF' THEN DO
   CALL log 'No parameters found'
   g.error = 8
END

IF g.error == 0 THEN DO
   g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1

   _loadmod = ""

   DO UNTIL g.error /= 0
      CALL lexerGetToken

      IF g.error /= 0 | g.scanner.currChar == "'" THEN LEAVE

      _loadmod = _loadmod || g.lexer.currToken

      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2
   END

   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM
END

IF g.error == 0 THEN DO
   CALL lexerGetToken

   IF g.error == 0 & g.scanner.currChar == 'EOF' THEN DO
      CALL log 'No PDS(E) data set and member parameter found'
      g.error = 8
   END

   IF g.error == 0 THEN DO
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1

      CALL lexerGetToken

      IF g.error == 0 THEN DO
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1

         _pdsmem = parseDsname()

         IF g.error == 0 THEN DO
            g.parser.scanState = g.parser.SCAN_STATE_IN_PARM2

            CALL lexerGetToken

            IF g.error == 0 & g.scanner.currChar /= "'" THEN DO
               CALL log 'PDS(E) data set and member wrong'
               g.error = 8
            END
         END
      END
   END

   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM2
END

IF g.error == 0 THEN DO
   g.loadmod = _loadmod
   g.pdsmem = _pdsmem

   SAY 'loadmod:  'g.loadmod
   SAY 'pdsmem:   'g.pdsmem
/* SAY 'complete parm: 'g.arg */
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

/*g.upperArg = TRANSLATE(g.arg)*/
g.argLen = LENGTH(g.arg)

g.scanner.colIndex = 0

g.lexer.IDENTIFIER_STARTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ@$#"
g.lexer.IDENTIFIER_CHARS = g.lexer.IDENTIFIER_STARTCHARS || "0123456789"

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

   IF g.scanner.currChar == "'" THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_QUOTE))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected "''" at pos 'g.scanner.colIndex
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
      DO WHILE g.error == 0 & g.scanner.peekChar /= 'EOF' & ,
               VERIFY(g.scanner.peekChar, g.lexer.IDENTIFIER_CHARS) == 0
         CALL scannerGetChar

         g.lexer.currToken = g.lexer.currToken || g.scanner.currChar
      END
      SIGNAL lexerGetToken_complete
   END

   _expected = g.parser.scanStateTable._state
   IF C2D(BITAND(D2C(_expected), ,
                 D2C(g.parser.EXPECTED_ANYTHING_ELSE))) /= 0 THEN DO
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

IF g.scanner.colIndex > g.argLen THEN DO
   g.scanner.currChar = 'EOF'
END
ELSE DO
   g.scanner.currChar = SUBSTR(g.arg, g.scanner.colIndex, 1)
   IF g.scanner.colIndex < g.argLen THEN DO
      g.scanner.peekChar = SUBSTR(g.arg, g.scanner.colIndex + 1, 1)
   END
   ELSE DO
      g.scanner.peekChar = 'EOF'
   END
END

RETURN

/**********************************************************************/
/* Log a message                                                      */
/**********************************************************************/
log: PROCEDURE EXPOSE g. SIGL

PARSE ARG _msg

SAY g.rexxname SIGL _msg

ADDRESS ISPEXEC "VGET (ZERRMSG ZERRSM ZERRLM)"
SAY ZERRMSG" - "ZERRSM
SAY ZERRLM

RETURN
