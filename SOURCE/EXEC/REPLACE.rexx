/* REXX */
PARSE ARG someText replaceText withText
SAY COPIES('*',100)
SAY '* REPLACE'
SAY COPIES('*',100)
SAY 'Starting text:        'someText
SAY 'Text to replace:      'replaceText
SAY 'Text to replace with: 'withText
retValue = someText
tmpText = '6A53CD2EW1F'
newText = someText
nextPos = POS(replaceText,newText)
DO WHILE nextPos > 0
   newText = SUBSTR(newText,1,nextPos-1) || tmpText || ,
             SUBSTR(newText,nextPos+LENGTH(replaceText))
   nextPos = POS(replaceText,newText)
END
nextPos = POS(tmpText,newText)
DO WHILE nextPos > 0
   newText = SUBSTR(newText,1,nextPos-1) || withText || ,
             SUBSTR(newText,nextPos+LENGTH(tmpText))
   nextPos = POS(tmpText,newText)
END
retValue = newText
SAY 'Return value:   'retvalue
SAY COPIES('-',100)
RETURN retvalue
