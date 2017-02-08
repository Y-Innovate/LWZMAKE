/* REXX */
PARSE ARG someText prependText
SAY COPIES('*',100)
SAY '* PREPEND'
SAY COPIES('*',100)
SAY 'Starting text:   'someText
SAY 'Text to prepend: 'prependText
retValue = ""
prependCount = WORDS(prependText)
IF prepentCount > 0 THEN DO
   retValue = WORD(prependText,1) || someText
   DO I = 2 TO prependCount
      retValue = retValue" "WORD(prependText,I) || someText
   END
END
SAY 'Return value:    'retValue
SAY COPIES('-',100)
RETURN retValue
