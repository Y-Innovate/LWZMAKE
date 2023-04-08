/* REXX */
PARSE ARG g.file
SAY COPIES('*',100)
SAY '* USSFILE'
SAY COPIES('*',100)
SAY 'USS file:     'g.file
_lastSlash = LASTPOS('/',g.file)
retValue = SUBSTR(g.file,_lastSlash+1)
SAY 'Return value: 'retValue
RETURN retValue
