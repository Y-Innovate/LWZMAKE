/* REXX */
PARSE ARG someText startIndex substrLen
SAY COPIES('*',100)
SAY '* SUBSTR'
SAY COPIES('*',100)
SAY 'Text:        'someText
SAY 'Start index: 'startIndex
SAY 'Length:      'substrLen
IF substrLen == "" THEN DO
   retValue = SUBSTR(someText,startIndex)
END
ELSE DO
   retValue = SUBSTR(someText,startIndex,substrLen)
END
SAY 'Return value:   'retvalue
SAY COPIES('-',100)
RETURN retvalue
