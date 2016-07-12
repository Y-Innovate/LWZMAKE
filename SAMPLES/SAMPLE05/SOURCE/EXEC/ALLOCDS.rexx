/* REXX */
PARSE ARG _dsname
_rc = BPXWDYN("ALLOC NEW DA('"_dsname"') RECFM(F,B) DSORG(PS) LRECL(80)" || ,
              " TRACKS SPACE(1,1) CATALOG RTDDN(_ddn)")
IF _rc == 0 THEN DO
   "EXECIO 0 DISKW "_ddn" (FINIS"
   _rc = BPXWDYN("FREE FI("_ddn")")
   IF _rc ¬= 0 THEN DO
      SAY "Free file failed with "_rc
   END
END
ELSE DO
   SAY "Alloc file failed with "_rc
END
EXIT _rc