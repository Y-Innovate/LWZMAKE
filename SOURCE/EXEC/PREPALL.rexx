/* REXX */
PARSE ARG prependText strings
SAY COPIES('*',100)
SAY '* PREPALL'
SAY COPIES('*',100)
SAY 'Text to prepend:    'prependText
SAY 'Prepend to strings: 'strings
retValue = ""
stringCount = WORDS(strings)
IF stringCount > 0 THEN DO
   retValue = prependText || WORD(strings,1)
   DO I = 2 TO stringCount
      retValue = retValue" "prependText || WORD(strings,I)
   END
END
SAY 'Return value:       'retValue
SAY COPIES('-',100)
RETURN retValue
