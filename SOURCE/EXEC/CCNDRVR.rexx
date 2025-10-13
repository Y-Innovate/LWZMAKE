/* REXX */
/**********************************************************************/
/* Program    : CCNDRVR                                               */
/*                                                                    */
/* Description: This program invokes CCNDRVR to compile a C source.   */
/*                                                                    */
/* Environment: Any (plain LWZMAKE, TSO, ISPF)                        */
/*                                                                    */
/* Parameters : The program accepts a single parameter string with    */
/*              the following syntax:                                 */
/*                                                                    */
/*              >>-SYSIN(-source-)--SYSLIN(-object-)--------------->  */
/*                                                                    */
/*              >--+--------------------------+-------------------->  */
/*                 |         .------------.   |                       */
/*                 |         V            |   |                       */
/*                 '-SYSLIB(---includelib-+-)-'                       */
/*                                                                    */
/*              >--+--------------------+->                           */
/*                 '-SYSCPRT(-listing-)-'                             */
/*                                                                    */
/*              >--+--------------------------+-------------------->  */
/*                 |          .------------.   |                      */
/*                 |          V            |   |                      */
/*                 '-INCLUDE(---includelib-+-)-'                      */
/*                                                                    */
/*                                 .-YES-.                            */
/*              >--+---------------+-----+-+--><                      */
/*                 '-PRINTSUCCESS(-+-NO--+)'                          */
/*                                                                    */
/*              source : Input C source data set.                     */
/*              object : Output object module data set.               */
/*              includelib: one or more input C include PDS(E)'s,     */
/*                       separated by spaces.                         */
/*              listing: Output C compile listing data set.           */
/*              printsuccess: Copy listing to job log when successful */
/*                            compile.                                */
/*                                                                    */
/* Returns    : 0 when CCNDRVR returned 4 or less                     */
/*              8 when REXX error occurs or when parameter string     */
/*                contains syntax error                               */
/*              n any CCNDRVR return code > 4                         */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "SYSIN(MY.C.PDS(MEMBER))" || ,                              */
/*        " SYSLIN(MY.OBJ.PDS(MEMBER))" || ,                          */
/*        " SYSLIB(CEE.SCEEH.H CEE.SCEEH.SYS.H)" || ,                 */
/*        " SYSCPRT(MY.LST.PDS(MEMBER))"                              */
/*                                                                    */
/* CALL 'CCNDRVR' _par                                                */
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
   CALL invokeCCNDRVR
END

CALL freeDDs

SAY 'RETURN CODE:  'g.CCNDRVR.retcode

IF g.CCNDRVR.retcode > 4 | g.CCNDRVR.retcode < 0 THEN
   g.error = g.CCNDRVR.retcode

EXIT g.error

/**********************************************************************/
/* Initialize                                                         */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL

SAY COPIES('*',100)
SAY '* CCNDRVR'
SAY COPIES('*',100)

g.error = 0
g.CCNDRVR.retcode = 0

g.syslin = ""
g.syslib.0 = 0
g.sysin = ""
g.syscprt = ""
g.sysprint = ""
g.parm = ""
g.printsuccess = "Y"

g.SYSLIN.allocated = 0
g.SYSLIB.allocated = 0
g.SYSLIB.ddname.0 = 0
g.SYSIN.allocated = 0
g.SYSCPRT.allocated = 0
g.SYSPRINT.allocated = 0
g.SYSPUNCH.allocated = 0
g.SYSUT1.allocated = 0
g.SYSUT4.allocated = 0
g.SYSUT5.allocated = 0
g.SYSUT6.allocated = 0
g.SYSUT7.allocated = 0
g.SYSUT8.allocated = 0
g.SYSUT9.allocated = 0
g.SYSUT10.allocated = 0
g.SYSUT14.allocated = 0
g.SYSUT15.allocated = 0
g.SYSUT16.allocated = 0
g.SYSUT17.allocated = 0
g.INCLUDE.allocated = 0
g.INCLUDE.ddname.0 = 0

RETURN

/**********************************************************************/
/* Allocate DD's for invoking CCNDRVR                                 */
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
   IF g.syscprt /= "" THEN DO
      _rc = BPXWDYN("ALLOC DSN('"g.syscprt"') SHR RTDDN(_ddn)")
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC NEW RECFM(F,B,A) DSORG(PS) LRECL(121) CYL" || ,
                    " SPACE(1,1) RTDDN(_ddn)")
   END

   IF _rc == 0 THEN DO
      g.SYSCPRT.allocated = 1
      g.SYSCPRT.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSCPRT failed with '_rc
   END
END

IF g.error == 0 THEN DO
   IF g.sysprint /= "" THEN DO
      _rc = BPXWDYN("ALLOC DSN('"g.sysprint"') SHR RTDDN(_ddn)")
   END
   ELSE DO
      _rc = BPXWDYN("ALLOC SYSOUT(A) REUSE RTDDN(_ddn)")
      /*_rc = BPXWDYN("ALLOC NEW RECFM(V,B,M) DSORG(PS) LRECL(133) CYL" || ,
                      " SPACE(1,1) RTDDN(_ddn)")*/
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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

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

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT16.allocated = 1
      g.SYSUT16.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT16 failed with '_rc
   END
END

IF g.error == 0 THEN DO
   _rc = BPXWDYN("ALLOC NEW BLOCK(800) SPACE(500,500) RTDDN(_ddn)")

   IF _rc == 0 THEN DO
      g.SYSUT17.allocated = 1
      g.SYSUT17.ddname = _ddn
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Dynamic allocation of SYSUT17 failed with '_rc
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

IF g.SYSCPRT.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSCPRT.ddname")")

   IF _rc == 0 THEN DO
      g.SYSCPRT.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSCPRT.ddname' failed with '_rc
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

IF g.SYSUT16.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT16.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT16.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT16.ddname' failed with '_rc
   END
END

IF g.SYSUT17.allocated == 1 THEN DO
   _rc = BPXWDYN("FREE FI("g.SYSUT17.ddname")")

   IF _rc == 0 THEN DO
      g.SYSUT17.allocated = 0
   END
   ELSE DO
      g.error = 8
      IF _rc > 0 THEN _rc = D2X(_rc)
      CALL log 'Free of file 'g.SYSUT17.ddname' failed with '_rc
   END
END

RETURN

/**********************************************************************/
/* Invoke CCNDRVR                                                     */
/**********************************************************************/
invokeCCNDRVR: PROCEDURE EXPOSE g. SIGL

_prog = 'CCNDRVR'
_parm = g.parm
_ddlist = LEFT(g.SYSIN.ddname,8) || ,
          LEFT(g.SYSLIN.ddname,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSLIB.ddname.1,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSPRINT.ddname,8) || ,
          LEFT(g.SYSCPRT.ddname,8) || ,
          LEFT(g.SYSPUNCH.ddname,8) || ,
          LEFT(g.SYSUT1.ddname,8) || ,
          LEFT(g.SYSUT4.ddname,8) || ,
          LEFT(g.SYSUT5.ddname,8) || ,
          LEFT(g.SYSUT6.ddname,8) || ,
          LEFT(g.SYSUT7.ddname,8) || ,
          LEFT(g.SYSUT8.ddname,8) || ,
          LEFT(g.SYSUT9.ddname,8) || ,
          LEFT(g.SYSUT10.ddname,8) || ,
          LEFT(g.SYSUT14.ddname,8) || ,
          LEFT(g.SYSUT15.ddname,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          LEFT(g.SYSUT16.ddname,8) || ,
          LEFT(g.SYSUT17.ddname,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8) || ,
          COPIES('00'X,8)

ADDRESS LINKMVS _prog '_parm _ddlist'

g.CCNDRVR.retcode = RC

IF g.printsuccess == "Y" | ,
   g.CCNDRVR.retcode > 4 | g.CCNDRVR.retcode < 0 THEN DO
   "EXECIO * DISKR "g.SYSCPRT.ddname" (STEM _syscprt. FINIS"

   _rc = RC

   IF _rc == 0 THEN DO
      _rc = BPXWDYN("ALLOC SYSOUT(A) RTDDN(_ddn)")

      IF _rc == 0 THEN DO
         SAY "SYSCPRT copied to DD "_ddn

         "EXECIO "_syscprt.0" DISKW "_ddn" (STEM _syscprt. FINIS"

         _rc = RC

         IF _rc /= 0 THEN DO
            g.error = 8
            CALL log "EXECIO DISKW for copy SYSCPRT failed "_rc
         END

         _rc = BPXWDYN("FREE FI("_ddn")")

         IF _rc /= 0 THEN DO
            g.error = 8
            CALL log "FREE for copy SYSCPRT failed "_rc
         END
      END
      ELSE DO
         g.error = 8
         CALL log "ALLOC for copy SYSCPRT failed "_rc
      END
   END
   ELSE DO
      g.error = 8
      CALL log "EXECIO DISKR for SYSCPRT failed "_rc
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
g.parser.EXPECTED_COLON = 64

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
                                            g.parser.EXPECTED_OPEN_BRACKET + ,
                                            g.parser.EXPECTED_CLOSE_BRACKET + ,
                                            g.parser.EXPECTED_DOT + ,
                                            g.parser.EXPECTED_COMMA + ,
                                            g.parser.EXPECTED_COLON

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
      WHEN _parmName == 'SYSIN' | _parmName == 'SYSLIN' | ,
           _parmName == 'SYSLIB' | _parmName == 'SYSCPRT' | ,
           _parmName == 'SYSPRINT' THEN DO
         g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         DO WHILE g.lexer.currToken /= ')'
            _dsname = parseDsname()
            IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
            SELECT
               WHEN _parmName == 'SYSIN' THEN DO
                  g.sysin = _dsname
               END
               WHEN _parmName == 'SYSLIN' THEN DO
                  g.syslin = _dsname
               END
               WHEN _parmName == 'SYSLIB' THEN DO
                  _nextSyslib = g.syslib.0 + 1
                  g.syslib.0 = _nextSyslib
                  g.syslib._nextSyslib = _dsname
               END
               WHEN _parmName == 'SYSCPRT' THEN DO
                  g.syscprt = _dsname
               END
               WHEN _parmName == 'SYSPRINT' THEN DO
                  g.sysprint = _dsname
               END
               OTHERWISE
                  NOP
            END
            g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3
            CALL lexerGetToken
            IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
            IF g.lexer.currToken /= ')' THEN DO
               IF _parmName == 'SYSLIN' | _parmName == 'SYSIN' | ,
                  _parmName == 'SYSCPRT' | _parmName == 'SYSPRINT' THEN DO
                  CALL log 'Only single dataset allowed at pos '
                           g.scanner.colIndex
                  g.error = 8
                  RETURN
               END
            END
        END
      END
      WHEN _parmName == 'PARM' THEN DO
         _openBracketCount = 0
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM1
         CALL lexerGetToken
         IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
         g.parser.scanState = g.parser.SCAN_STATE_IN_PARM_PARM2
         DO WHILE g.lexer.currToken /= ')' | _openBracketCount > 0
            g.parm = g.parm || g.lexer.currToken
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
   END
   IF g.error /= 0 | g.scanner.currChar == 'EOF' THEN LEAVE
   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM
END

IF g.error == 0 & g.syslmod == "" THEN DO
   CALL log 'SYSIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.syslin == "" THEN DO
   CALL log 'SYSLIN(...) expected but not found or specified wrong'
   g.error = 8
END

IF g.error == 0 THEN DO
   SAY 'SYSIN:        'g.sysin
   SAY 'SYSLIN:       'g.syslin
   DO i = 1 to g.syslib.0
      SAY 'SYSLIB:       'g.syslib.i
   END
   SAY 'SYSCPRT:      'g.syscprt
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

g.lexer.IDENTIFIER_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
g.lexer.IDENTIFIER_CHARS = g.lexer.IDENTIFIER_CHARS || "0123456789@#$"
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

   IF g.scanner.currChar == ':' THEN DO
      _expected = g.parser.scanStateTable._state
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_COLON))) /= 0 THEN DO
         g.lexer.currToken = g.scanner.currChar
      END
      ELSE DO
         CALL log 'Unexpected ":" at pos 'g.scanner.colIndex
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
