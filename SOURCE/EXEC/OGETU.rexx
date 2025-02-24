/* REXX */
/**********************************************************************/
/* Program    : OGETU                                                 */
/*                                                                    */
/* Description: This program copies a USS file into a PDS(E) member   */
/*              and sets the member statistics to the USS file's last */
/*              modified time.                                        */
/*                                                                    */
/* Environment: ISPF                                                  */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>--'pathname'--'mvs_data_set_name(member_name)-->    */
/*                                                                    */
/* Returns    : 0 when file copies successfully                       */
/*              8 when there was an error                             */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "'/some/path/file' 'MY.PDS(MEMBER)'                         */
/*                                                                    */
/* CALL 'OGETU' _par                                                  */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL parseArguments

IF g.error == 0 THEN DO
   CALL do_tmpfile
END

IF g.error == 0 THEN DO
   CALL do_ogetu
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
SAY '* OGETU'
SAY COPIES('*',100)

g.error = 0

g.pathname = ""
g.pdsmem = ""
g.binary = 0
g.load = 0
g.ibm1047 = 0
g.notouch = 0

RETURN

/**********************************************************************/
/* Execute tmpname command                                            */
/**********************************************************************/
do_tmpfile: PROCEDURE EXPOSE g. SIGL

_x = SYSCALLS('ON')

ADDRESS SYSCALL 'time'

_time = RETVAL

PARSE VAR g.pdsmem _someDataset'('_someMember')'

g.tmpfile = '/tmp/'_someMember || _time

RETURN

/**********************************************************************/
/* Copy USS file to PDS member                                        */
/**********************************************************************/
do_ogetu: PROCEDURE EXPOSE g. SIGL

_pdsmem = STRREPLACE(g.pdsmem, "$", "\$")

IF g.binary == 1 THEN DO
   _cmd = "cp -B '"g.pathname"' "'"//'"'"_pdsmem"'"'"'

   CALL BPXWUNIX _cmd,,_stdout.,_stderr.,,1

   IF RESULT /= 0 THEN DO
      g.error = 8

      CALL log 'STDOUT'
      DO i = 1 TO _stdout.0
         CALL log _stdout.i
      END
      CALL log 'STDERR'
      DO i = 1 TO _stderr.0
         CALL log _stderr.i
      END
   END
END
ELSE IF g.load == 1 THEN DO
   _cmd = "cp -X '"g.pathname"' "'"//'"'"_pdsmem"'"'"'

   CALL BPXWUNIX _cmd,,_stdout.,_stderr.,,1

   IF RESULT /= 0 THEN DO
      g.error = 8

      CALL log 'STDOUT'
      DO i = 1 TO _stdout.0
         CALL log _stdout.i
      END
      CALL log 'STDERR'
      DO i = 1 TO _stderr.0
         CALL log _stderr.i
      END
   END

   g.notouch = 1
END
ELSE DO
   IF g.ibm1047 == 1 THEN
      _cmd = "cp '"g.pathname"' '"g.tmpfile"'"
   ELSE
      _cmd = "iconv -f IBM-1047 -t IBM-037 -T '"g.pathname"' > '"g.tmpfile"'"

   CALL BPXWUNIX _cmd,,_stdout.,_stderr.,,1

   IF RESULT /= 0 THEN DO
      g.error = 8

      CALL log 'STDOUT'
      DO i = 1 TO _stdout.0
         CALL log _stdout.i
      END
      CALL log 'STDERR'
      DO i = 1 TO _stderr.0
         CALL log _stderr.i
      END
   END

   IF g.error == 0 THEN DO
      _x = SYSCALLS('ON')

      path = g.tmpfile

      ADDRESS SYSCALL 'readfile (path) file.'

      IF RC >= 0 & RETVAL >= 0 & file.0 > 0 THEN DO
         PARSE VAR g.pdsmem _someDataset'('_someMember')'

         IF _someMember /= "" THEN DO
            _lminit = "LMINIT DATAID(MYID)"
            _lminit = _lminit || " DATASET('"_someDataset"')"
            _lminit = _lminit || " ENQ(SHRW)"

            ADDRESS ISPEXEC _lminit

            IF RC /= 0 THEN DO
               CALL log 'LMINIT failed with 'RC

               CALL log ZERRMSG" - "ZERRSM
               CALL log ZERRLM

               g.error = 8
            END

            IF g.error == 0 THEN DO
               _lmopen = "LMOPEN DATAID(&MYID) OPTION(OUTPUT)"

               ADDRESS ISPEXEC _lmopen

               IF RC /= 0 THEN DO
                  CALL log 'LMOPEN failed with 'RC

                  CALL log ZERRMSG" - "ZERRSM
                  CALL log ZERRLM

                  g.error = 8
               END

               IF g.error == 0 THEN DO
                  DO I = 1 TO file.0
                     rcd = file.I
                     rcdlen = LENGTH(rcd)

                     IF rcdlen == 0 THEN rcdlen = 1

                     _lmput = "LMPUT DATAID(&MYID) MODE(INVAR) DATALOC(rcd)"
                     _lmput = _lmput" DATALEN(&rcdlen)"

                     ADDRESS ISPEXEC _lmput

                     IF RC /= 0 THEN DO
                        CALL log 'LMPUT failed with 'RC

                        CALL log ZERRMSG" - "ZERRSM
                        CALL log ZERRLM

                        g.error = 8

                        EXIT
                     END
                  END

                  IF g.error == 0 THEN DO
                     mem = _someMember

                     _lmmrep = "LMMREP DATAID(&MYID) MEMBER(&mem)"

                     ADDRESS ISPEXEC _lmmrep

                     IF RC /= 0 & RC /= 8 THEN DO
                        CALL log 'LMMREP failed with 'RC

                        CALL log ZERRMSG" - "ZERRSM
                        CALL log ZERRLM

                        g.error = 8
                     END
                  END

                  _lmclose = "LMCLOSE DATAID(&MYID)"

                  ADDRESS ISPEXEC _lmclose

                  IF RC /= 0 THEN DO
                     CALL log 'LMCLOSE failed with 'RC

                     CALL log ZERRMSG" - "ZERRSM
                     CALL log ZERRLM

                     g.error = 8
                  END
               END
            END

            IF g.error == 0 THEN DO
               ADDRESS ISPEXEC "LMFREE DATAID(&MYID)"

               IF RC /= 0 THEN DO
                  CALL log 'LMFREE failed with 'RC

                  CALL log ZERRMSG" - "ZERRSM
                  CALL log ZERRLM

                  g.error = 8
               END
            END
         END
      END
      ELSE DO
         CALL log 'readfile returned 'RC' 'RETVAL
         g.error = 8
      END

      _cmd = "rm '"g.tmpfile"'"

      CALL BPXWUNIX _cmd,,_stdout.,_stderr.,,1

      IF RESULT /= 0 THEN DO
         g.error = 8

         CALL log 'STDOUT'
         DO i = 1 TO _stdout.0
            CALL log _stdout.i
         END
         CALL log 'STDERR'
         DO i = 1 TO _stderr.0
            CALL log _stderr.i
         END
      END
   END
END

RETURN

/**********************************************************************/
/* Touch member                                                       */
/**********************************************************************/
do_touch: PROCEDURE EXPOSE g. SIGL

_x = SYSCALLS('ON')

path = g.pathname

ADDRESS SYSCALL 'stat (path) st.'

_timestamp = ST.ST_MTIME

NUMERIC DIGITS 21

cvt     = C2d( Storage( D2x( 16 ), 4 ) )
cvttz   = C2d( Storage( D2x( cvt + 304 ), 4 ) )
cvtext2 = C2d( Storage( D2x( cvt + 328 ), 4 ) )
cvtldto = C2d( Storage( D2x( cvtext2 + 56 ), 8 ), 8 )
absldto = Abs( cvtldto )
secs    = absldto / x2d("F4240000")

_timestamp = _timestamp + secs

ADDRESS SYSCALL 'gmtime '_timestamp' tm.'

_moddate = tm.TM_YEAR || ,
           '/' || ,
           RIGHT("0" || tm.TM_MON, 2) || ,
           '/' || ,
           RIGHT("0" || tm.TM_MDAY, 2)
_modtime = RIGHT("0" || tm.TM_HOUR, 2) || ,
           ':' || ,
           RIGHT("0" || tm.TM_MIN, 2) || ,
           ':' || ,
           RIGHT("0" || tm.TM_SEC, 2)

NUMERIC DIGITS

PARSE VAR g.pdsmem _someDataset'('_someMember')'

IF _someMember /= "" THEN DO
   _lminit = "LMINIT DATAID(MYID)"
   _lminit = _lminit || " DATASET('"_someDataset"')"
   _lminit = _lminit || " ENQ(SHRW)"

   ADDRESS ISPEXEC _lminit

   IF RC /= 0 THEN DO
      CALL log 'LMINIT failed with 'RC

      CALL log ZERRMSG" - "ZERRSM
      CALL log ZERRLM

      g.error = 8
   END

   IF g.error == 0 THEN DO
      _lmmstats = "LMMSTATS DATAID(&MYID)"
      _lmmstats = _lmmstats || " MEMBER("_someMember")"
      _lmmstats = _lmmstats || " MODDATE4("_moddate")"
      _lmmstats = _lmmstats || " MODTIME("_modtime")"
      _lmmstats = _lmmstats || " USER("USERID()")"

      ADDRESS ISPEXEC _lmmstats

      IF RC /= 0 THEN DO
         CALL log 'LMMSTATS failed with 'RC
         CALL log _lmmstats

         CALL log ZERRMSG" - "ZERRSM
         CALL log ZERRLM

         g.error = 8
      END
   END

   IF g.error == 0 THEN DO
      ADDRESS ISPEXEC "LMFREE DATAID(&MYID)"

      IF RC /= 0 THEN DO
         CALL log 'LMFREE failed with 'RC

         CALL log ZERRMSG" - "ZERRSM
         CALL log ZERRLM

         g.error = 8
      END
   END
END

RETURN

/**********************************************************************/
/* A function to do a string replace                                  */
/**********************************************************************/
STRREPLACE:

ORIGINAL = ARG(1)
OLDTXT = ARG(2)
NEWTXT = ARG(3)
/* YOU CAN CHANGE THE BELOW KEY (TMPTXT), WHICH IS USED AS A TEMPORARY
POINTER TO IDENTIFY THE TEXT TO BE REPLACED */
TMPTXT = '6A53CD2EW1F'
NEWSTR = ORIGINAL
DO WHILE POS(OLDTXT,NEWSTR) > 0
NEWSTR = SUBSTR(NEWSTR, 1 , POS(OLDTXT,NEWSTR)-1) ||,
TMPTXT || SUBSTR(NEWSTR, POS(OLDTXT,NEWSTR) + LENGTH(OLDTXT))
END
DO WHILE POS(TMPTXT,NEWSTR) > 0
NEWSTR = SUBSTR(NEWSTR, 1 , POS(TMPTXT,NEWSTR)-1) ||,
NEWTXT || SUBSTR(NEWSTR, POS(TMPTXT,NEWSTR) + LENGTH(TMPTXT))
END

RETURN NEWSTR

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

   _pathname = ""

   DO UNTIL g.error /= 0
      CALL lexerGetToken

      IF g.error /= 0 | g.scanner.currChar == "'" THEN LEAVE

      _pathname = _pathname || g.lexer.currToken

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

IF g.error == 0 & g.scanner.currChar /= 'EOF' THEN DO
   CALL lexerGetToken

   DO WHILE g.error == 0 & g.scanner.currChar /= 'EOF'
      _upperToken = g.lexer.currToken
      UPPER _upperToken

      IF _upperToken == 'BINARY' THEN
         g.binary = 1
      ELSE IF _upperToken == 'LOAD' THEN
         g.load = 1
      ELSE IF _upperToken == 'IBM1047' THEN
         g.ibm1047 = 1
      ELSE IF _upperToken == 'NOTOUCH' THEN
         g.notouch = 1

      CALL lexerGetToken
   END
END

IF g.error == 0 THEN DO
   g.pathname = _pathname
   g.pdsmem = _pdsmem

   SAY 'pathname: 'g.pathname
   SAY 'pdsmem:   'g.pdsmem
   SAY 'binary:   'g.binary
   SAY 'load:     'g.load
   SAY 'ibm1047:  'g.ibm1047
   SAY 'notouch:  'g.notouch
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

RETURN
