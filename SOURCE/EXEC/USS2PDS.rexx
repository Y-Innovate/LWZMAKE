/* REXX */
PARSE ARG g.file g.pds
SAY COPIES('*',100)
SAY '* USS2PDS'
SAY COPIES('*',100)
SAY 'USS file:     'g.file
SAY 'PDS:          'g.pds
_lastSlash = LASTPOS('/',g.file)
_filename  = SUBSTR(g.file,_lastSlash+1)
IF POS('.',_filename) /= 0 THEN DO
   _filename = SUBSTR(_filename,1,POS('.',_filename)-1)
END
SAY 'Member name:  '_filename
retValue = g.pds"("_filename")"
SAY 'Return value: 'retValue
RETURN retValue
