/* REXX */
/**********************************************************************/
/* Program    : COBOL5                                                */
/*                                                                    */
/* Description: This program invokes IGYCRCTL to compile a source.    */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-SYSIN(-source-)--SYSLIN(-object-)--------------->  */
/*                                                                    */
/*              >--+-----------------------+----------------------->  */
/*                 |         .---------.   |                          */
/*                 |         V         |   |                          */
/*                 '-SYSLIB(---copylib-+-)-'                          */
/*                                                                    */
/*              >--+--------------------+--+--------------------+-->  */
/*                 '-SYSOPTF(-optfile-)-'  '-DBRMLIB(-dbrmlib-)-'     */
/*                                                                    */
/*              >--+---------------------+-><                         */
/*                 '-SYSPRINT(-listing-)-'                            */
/*                                                                    */
/*              source : Input COBOL source data set.                 */
/*              object : Output object module data set.               */
/*              copylib: One or more input COBOL copybook PDS(E)'s,   */
/*                       separated by spaces.                         */
/*              optfile: Input COBOL compiler options data set.       */
/*              dbrmlib: Output DBRM data set.                        */
/*              listing: Output COBOL compile listing data set.       */
/*                                                                    */
/* Returns    : 0 when IGYCRCTL returned 4 or less                    */
/*              8 when REXX error occurs or when parameter string     */
/*                contains syntax error                               */
/*              n any IGYCRCTL return code > 4                        */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "SYSIN(MY.COBOL.PDS(MEMBER))"   || ,                        */
/*        " SYSLIN(MY.OBJ.PDS(MEMBER))"   || ,                        */
/*        " SYSLIB(CEE.SCEESAMP)"         || ,                        */
/*        " SYSPRINT(MY.LST.PDS(MEMBER))"                             */
/*                                                                    */
/* CALL 'COBOL' _par                                                  */
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
   CALL invokeIGYCRCTL
END

CALL freeDDs

IF g.IGYCRCTL.retcode > 4 | g.IGYCRCTL.retcode < 0 THEN
   g.error = g.IGYCRCTL.retcode

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* COBOL5'
SAY COPIES('*',100)

g.error = 0
g.IGYCRCTL.retcode = 0

g.syslin = ""
g.syslib.0 = 0
g.sysin = ""
g.sysoptf = ""
g.dbrmlib = ""
g.sysprint = ""

g.SYSLIN.allocated = 0
g.SYSLIB.allocated = 0
g.SYSLIB.ddname.0 = 0
g.SYSIN.allocated = 0
g.SYSPRINT.allocated = 0
g.SYSPUNCH.allocated = 0
g.SYSUT1.allocated = 0
g.SYSUT2.allocated = 0
g.SYSUT3.allocated = 0
g.SYSUT4.allocated = 0
g.SYSTERM.allocated = 0
g.SYSUT5.allocated = 0
g.SYSUT6.allocated = 0
g.SYSUT7.allocated = 0
g.SYSADATA.allocated = 0
g.SYSJAVA.allocated = 0
g.SYSMDECK.allocated = 0
g.DBRMLIB.allocated = 0
g.SYSOPTF.allocated = 0
g.SYSUT8.allocated = 0
g.SYSUT9.allocated = 0
g.SYSUT10.allocated = 0
g.SYSUT11.allocated = 0
g.SYSUT12.allocated = 0
g.SYSUT13.allocated = 0
g.SYSUT14.allocated = 0
g.SYSUT15.allocated = 0

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
   IF g.syslib.0 == 0 THEN DO
      _rc = BPXWDYN("ALLOC DUMMY SHR RTDDN(_ddn)")

      IF _rc == 0 THEN DO
         g.SYSLIB.allocated = 1
         g.SYSLIB.ddname.0 = 1
         g.SYSLIB.ddname.1 = _ddn
      END
      ELSE DO
         g.error = 8
         IF _rc > 0 THEN _rc = D2X(_rc)
         CALL log 'Dynamic allocation of dummy SYSLIB failed with '_rc
      END
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC DSN('"g.syslib.1"') SHR RTDDN(_ddn)")

      IF _rc == 0 THEN DO
         g.SYSLIB.allocated = 1
         g.SYSLIB.ddname.0 = 1
         g.SYSLIB.ddname.1 = _ddn
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

            _rc = BPXWDYN("CONCAT DDLIST("g.SYSLIB.ddname.1","_ddn")")

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
      g.SYSPUNCH.allocated = 1
      g.SYSPUNCH.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSPUNCH failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT1.allocated = 1
      g.SYSUT1.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT1 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT2.allocated = 1
      g.SYSUT2.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT2 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT3.allocated = 1
      g.SYSUT3.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT3 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT4.allocated = 1
      g.SYSUT4.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT4 failed with '_rc
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
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT5.allocated = 1
      g.SYSUT5.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT5 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT6.allocated = 1
      g.SYSUT6.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT6 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT7.allocated = 1
      g.SYSUT7.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT7 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSADATA.allocated = 1
      g.SYSADATA.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSADATA failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSJAVA.allocated = 1
      g.SYSJAVA.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSJAVA failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSMDECK.allocated = 1
      g.SYSMDECK.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSMDECK failed with '_rc
   END
END

IF g.error == 0 THEN DO
   IF g.dbrmlib /= "" THEN DO
      _rc = BPXWDYN("ALLOC DSN('"g.dbrmlib"') SHR RTDDN(_ddn)")
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")
   END

   IF _rc == 0 THEN DO
      g.DBRMLIB.allocated = 1
      g.DBRMLIB.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of DBRMLIB failed with '_rc
   END
END

IF g.error == 0 THEN DO
   IF g.sysoptf /= "" THEN DO
      _rc = BPXWDYN("ALLOC DSN('"g.sysoptf"') SHR RTDDN(_ddn)")
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")
   END

   IF _rc == 0 THEN DO
      g.SYSOPTF.allocated = 1
      g.SYSOPTF.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSOPTF failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT8.allocated = 1
      g.SYSUT8.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT8 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT9.allocated = 1
      g.SYSUT9.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT9 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT10.allocated = 1
      g.SYSUT10.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT10 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT11.allocated = 1
      g.SYSUT11.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT11 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT12.allocated = 1
      g.SYSUT12.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT12 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT13.allocated = 1
      g.SYSUT13.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT13 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT14.allocated = 1
      g.SYSUT14.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT14 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(460) SPACE(350,100) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT15.allocated = 1
      g.SYSUT15.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT15 failed with '_rc
   END
END

RETURN

/**********************************************************************/
/* Free DD's after invoking ASMA90                                    */
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

IF g.SYSUT1.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT1.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT1.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT1.ddname' failed with '_rc
   END
END

IF g.SYSUT2.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT2.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT2.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT2.ddname' failed with '_rc
   END
END

IF g.SYSUT3.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT3.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT3.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT3.ddname' failed with '_rc
   END
END

IF g.SYSUT4.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT4.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT4.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT4.ddname' failed with '_rc
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

IF g.SYSUT5.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT5.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT5.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT5.ddname' failed with '_rc
   END
END

IF g.SYSUT6.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT6.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT6.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT6.ddname' failed with '_rc
   END
END

IF g.SYSUT7.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT7.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT7.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT7.ddname' failed with '_rc
   END
END

IF g.SYSADATA.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSADATA.ddname")")

   IF _rc == 0 THEN DO
      g.SYSADATA.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSADATA.ddname' failed with '_rc
   END
END

IF g.SYSJAVA.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSJAVA.ddname")")

   IF _rc == 0 THEN DO
      g.SYSJAVA.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSJAVA.ddname' failed with '_rc
   END
END

IF g.SYSMDECK.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSMDECK.ddname")")

   IF _rc == 0 THEN DO
      g.SYSMDECK.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSMDECK.ddname' failed with '_rc
   END
END

IF g.DBRMLIB.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.DBRMLIB.ddname")")

   IF _rc == 0 THEN DO
      g.DBRMLIB.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.DBRMLIB.ddname' failed with '_rc
   END
END

IF g.SYSOPTF.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSOPTF.ddname")")

   IF _rc == 0 THEN DO
      g.SYSOPTF.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSOPTF.ddname' failed with '_rc
   END
END

IF g.SYSUT8.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT8.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT8.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT8.ddname' failed with '_rc
   END
END

IF g.SYSUT9.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT9.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT9.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT9.ddname' failed with '_rc
   END
END

IF g.SYSUT10.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT10.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT10.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT10.ddname' failed with '_rc
   END
END

IF g.SYSUT11.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT11.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT11.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT11.ddname' failed with '_rc
   END
END

IF g.SYSUT12.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT12.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT12.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT12.ddname' failed with '_rc
   END
END

IF g.SYSUT13.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT13.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT13.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT13.ddname' failed with '_rc
   END
END

IF g.SYSUT14.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT14.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT14.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT14.ddname' failed with '_rc
   END
END

IF g.SYSUT15.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT15.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT15.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT15.ddname' failed with '_rc
   END
END

RETURN

/**********************************************************************/
/* Invoke IGYCRCTL                                                    */
/**********************************************************************/
invokeIGYCRCTL: PROCEDURE EXPOSE g. SIGL

_prog = 'IGYCRCTL'
_parm = ''
IF g.sysoptf /= "" THEN DO
   _parm = 'OPTFILE'
END
_ddlist = LEFT(g.SYSLIN.ddname,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSLIB.ddname.1,8) || ,
          LEFT(g.SYSIN.ddname,8)   || ,
          LEFT(g.SYSPRINT.ddname,8) || ,
          LEFT(g.SYSPUNCH.ddname,8) || ,
          LEFT(g.SYSUT1.ddname,8) || ,
          LEFT(g.SYSUT2.ddname,8) || ,
          LEFT(g.SYSUT3.ddname,8) || ,
          LEFT(g.SYSUT4.ddname,8) || ,
          LEFT(g.SYSTERM.ddname,8) || ,
          LEFT(g.SYSUT5.ddname,8) || ,
          LEFT(g.SYSUT6.ddname,8) || ,
          LEFT(g.SYSUT7.ddname,8) || ,
          LEFT(g.SYSADATA.ddname,8) || ,
          LEFT(g.SYSJAVA.ddname,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSMDECK.ddname,8) || ,
          LEFT(g.DBRMLIB.ddname,8) || ,
          LEFT(g.SYSOPTF.ddname,8) || ,
          LEFT(g.SYSUT8.ddname,8) || ,
          LEFT(g.SYSUT9.ddname,8) || ,
          LEFT(g.SYSUT10.ddname,8) || ,
          LEFT(g.SYSUT11.ddname,8) || ,
          LEFT(g.SYSUT12.ddname,8) || ,
          LEFT(g.SYSUT13.ddname,8) || ,
          LEFT(g.SYSUT14.ddname,8) || ,
          LEFT(g.SYSUT15.ddname,8)

ADDRESS LINKMVS _prog '_parm _ddlist'

g.IGYCRCTL.retcode = RC

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

_parmName = ""

CALL initLexer

DO WHILE g.error == 0
   CALL lexerGetToken

   IF g.error /= 0  | g.scanner.currChar == 'EOF' THEN LEAVE

   _parmName = g.lexer.currToken

   g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1
   CALL lexerGetToken
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
   CALL lexerGetToken
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   DO WHILE g.lexer.currToken /= ')'
      _dsname = parseDsname()
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      SELECT
         WHEN _parmName == 'SYSLIN' THEN DO
            g.syslin = _dsname
         END
         WHEN _parmName == 'SYSIN' THEN DO
            g.sysin = _dsname
         END
         WHEN _parmName == 'SYSLIB' THEN DO
            _nextSyslib = g.syslib.0 + 1
            g.syslib.0 = _nextSyslib
            g.syslib._nextSyslib = _dsname
         END
         WHEN _parmName == 'SYSOPTF' THEN DO
            g.sysoptf = _dsname
         END
         WHEN _parmName == 'SYSPRINT' THEN DO
            g.sysprint = _dsname
         END
         WHEN _parmName == 'DBRMLIB' THEN DO
            g.dbrmlib = _dsname
         END
         OTHERWISE
            NOP
      END
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
      CALL lexerGetToken
      IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
      IF g.lexer.currToken /= ')' THEN DO
         IF _parmName == 'SYSLIN' | _parmName == 'SYSIN' | ,
            _parmName == 'SYSOPTF' | _parmName == 'SYSPRINT' THEN DO
            _parmName == 'DBRMLIB' THEN DO
            CALL log 'Only single dataset allowed at pos 'g.scanner.colIndex
            g.error = 8
            RETURN
         END
      END
   END
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM
END

IF g.syslin == "" THEN DO
   CALL log 'SYSLIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 & g.syslmod == "" THEN DO
   CALL log 'SYSIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'SYSLIN:   'g.syslin
   SAY 'SYSIN:    'g.sysin
   DO i = 1 to g.syslib.0
      SAY 'SYSLIB:   'g.syslib.i
   END
   SAY 'SYSOPTF:  'g.sysoptf
   SAY 'SYSPRINT: 'g.sysprint
   SAY 'DBRMLIB:  'g.dbrmlib
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
