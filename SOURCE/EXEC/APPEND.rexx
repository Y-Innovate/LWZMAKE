/* REXX */
PARSE ARG someText appendText
SAY COPIES('*',100)
SAY '* APPEND'
SAY COPIES('*',100)
SAY 'Starting text:  'someText
SAY 'Text to append: 'appendText
retValue = ""
appendCount = WORDS(appendText)
IF appendCount > 0 THEN DO
   retvalue = someText || WORD(appendText,1)
   DO I = 2 TO appendCount
      retvalue = retvalue" "someText || WORD(appendText,I)
   END
END
SAY 'Return value:   'retvalue
SAY COPIES('-',100)
RETURN retvalue
