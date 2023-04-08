/* REXX */
TRACE R

_rc = BPXWDYN("ALLOC SHR DA('SYS1.NUCLEUS') BLKSIZE(256) LRECL(256)" ||,
              " RECFM(F) DSORG(PS) VOL(B2RES1) RTDDN(_ddnin)")

"EXECIO * DISKR "_ddnin" (STEM _nuc. FINIS"

_rc = BPXWDYN("FREE FI("_ddnin")")

_rc = BPXWDYN("ALLOC NEW CATALOG DA('YBTK.NUCLEUS.PDSDIR.Z22') RECFM(F,B)" ||,
              " LRECL(256) DSORG(PS) RTDDN(_ddnout)")

"EXECIO "_nuc.0" DISKW "_ddnout" (STEM _nuc. FINIS"

_rc = BPXWDYN("FREE FI("_ddnout")")

EXIT
