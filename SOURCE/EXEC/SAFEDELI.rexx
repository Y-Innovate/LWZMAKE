/* REXX */
/**********************************************************************/
/* Program    : SAFEDELI                                              */
/*                                                                    */
/* Description: This program accepts a space delimited set of PDS(E)  */
/*              data sets with members, checks if they exist and if   */
/*              so deletes them using ISPF library services.          */
/*                                                                    */
/* Environment: ISPF                                                  */
/*                                                                    */
/* Parameters : A space delimited set of PDS(E) data sets with members*/
/*                                                                    */
/* Returns    : 0 when all deletes successful                         */
/*              8 when a delete failed                                */
/*                                                                    */
/* Sample code:                                                       */
/* _par = "MY.DS1 MY.PDS MY.PDS2(MEM1)"                               */
/*                                                                    */
/* CALL 'SAFEDEL' _par                                                */
/*                                                                    */
/* _rc = RESULT                                                       */
/**********************************************************************/
PARSE ARG g.arg
PARSE SOURCE . . g.rexxname .

CALL init

CALL safeDelete

IF g.DELETE.retcode /= 0 THEN
   g.error = 8

EXIT g.error

/**********************************************************************/
/* Initializations                                                    */
/**********************************************************************/
init: PROCEDURE EXPOSE g. SIGL
SAY '******************************************'
SAY '* SAFEDELI                               *'
SAY '******************************************'
SAY g.arg

g.error = 0
g.DELETE.retcode = 0

RETURN

safeDelete: PROCEDURE EXPOSE g. SIGL
/**********************************************************************/
/* Safely delete dataset(s)                                           */
/**********************************************************************/

_wrds = WORDS(g.arg)

DO i = 1 TO _wrds WHILE g.DELETE.retcode == 0
   SAY 'Check 'WORD(g.arg,i)

   _exists = SYSDSN("'"WORD(g.arg,i)"'")

   IF _exists == "OK" THEN DO
      _parseThis = WORD(g.arg,i)

      PARSE VAR _parseThis _someDataset'('_someMember')'

      SAY 'Delete '_someDataset'('_someMember')'

      IF _someMember /= "" THEN DO
         _lminit = "LMINIT DATAID(MYID)"
         _lminit = _lminit || " DATASET('"_someDataset"')"
         _lminit = _lminit || " ENQ(SHRW)"

         ADDRESS ISPEXEC _lminit

         IF RC /= 0 THEN DO
            CALL log 'LMINIT failed with 'RC
            g.error = 8
         END

         IF g.error == 0 THEN DO
            _lmopen = "LMOPEN DATAID(&MYID) OPTION(OUTPUT)"

            ADDRESS ISPEXEC _lmopen

            IF RC /= 0 THEN DO
               CALL log 'LMOPEN failed with 'RC
               g.error = 8
            END

            IF g.error == 0 THEN DO
               mem = _someMember

               _lmmdel = "LMMDEL DATAID(&MYID) MEMBER(&mem)"

               ADDRESS ISPEXEC _lmmdel

               IF RC /= 0 & RC /= 8 THEN DO
                  CALL log 'LMMDEL failed with 'RC
                  g.error = 8
               END

               _lmclose = "LMCLOSE DATAID(&MYID)"

               ADDRESS ISPEXEC _lmclose

               IF RC /= 0 THEN DO
                  CALL log 'LMCLOSE failed with 'RC
                  g.error = 8
               END
            END

            IF g.error == 0 THEN DO
               ADDRESS ISPEXEC "LMFREE DATAID(&MYID)"

               IF RC /= 0 THEN DO
                  CALL log 'LMFREE failed with 'RC
                  g.error = 8
               END
            END
         END
      END
   END
END

RETURN
