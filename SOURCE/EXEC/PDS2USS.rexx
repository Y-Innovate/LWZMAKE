/* REXX */
PARSE ARG g.pds g.folder g.ext
SAY COPIES('*',100)
SAY '* PDS2USS'
SAY COPIES('*',100)
SAY 'PDS member:     'g.pds
SAY 'USS folder:     'g.folder
SAY 'File extension: 'g.ext
PARSE VAR g.pds _dataSet"("g.member")"
SAY 'Data set:       '_dataSet
SAY 'Member:         'g.member
retValue = g.folder
IF RIGHT(g.folder,1) /= '/' THEN DO
   retValue = retValue || '/'
END
IF LEFT(g.ext,1) /= '.' THEN DO
   g.ext = '.' || g.ext
END
retValue = retValue || g.member || g.ext
SAY 'Return value:   'retValue
RETURN retValue
