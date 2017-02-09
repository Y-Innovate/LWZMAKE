/* REXX */
PARSE ARG appendText strings
SAY COPIES('*',100)
SAY '* APNDALL'
SAY COPIES('*',100)
SAY 'Text to append:    'appendText
SAY 'Append to strings: 'strings
retValue = ""
stringCount = WORDS(strings)
IF stringCount > 0 THEN DO
   retValue = WORD(strings,1) || appendText
   DO I = 2 TO stringCount
      retValue = retValue" "WORD(strings,I) || appendText
   END
END
SAY 'Return value:      'retValue
SAY COPIES('-',100)
RETURN retValue
