/* REXX */
PARSE ARG argSet1','argSet2
argSet1WordCount = WORDS(argSet1)
argSet2WordCount = WORDS(argSet2)
argSet1Word# = 1
argSet2Word# = 1
currentWord = ""
retString = ""
CALL setWord
DO WHILE currentWord /= ""
  IF argSet1Word# <= argSet1WordCount & ,
     argSet2Word# <= argSet2WordCount & ,
     currentWord = WORD(argSet1,argSet1Word#) & ,
     currentWord = WORD(argSet2,argSet2Word#) THEN DO
    IF retString = "" THEN DO
      retString = currentWord
    END
    ELSE DO
      retString = retString" "currentWord
    END
  END
  IF argSet1Word# <= argSet1WordCount & ,
     currentWord = WORD(argSet1,argSet1Word#) THEN DO
    argSet1Word# = argSet1Word# + 1
  END
  IF argSet2Word# <= argSet2WordCount & ,
     currentWord = WORD(argSet2,argSet2Word#) THEN DO
    argSet2Word# = argSet2Word# + 1
  END
  CALL setWord
END
RETURN retString

setWord:
IF argSet1Word# <= argSet1WordCount THEN DO
  IF argSet2Word# <= argSet2WordCount THEN DO
    IF WORD(argSet1,argSet1Word#) <= WORD(argSet2,argSet2Word#) THEN DO
      currentWord = WORD(argSet1,argSet1Word#)
    END
    ELSE DO
      currentWord = WORD(argSet2,argSet2Word#)
    END
  END
  ELSE DO
    currentWord = WORD(argSet1,argSet1Word#)
  END
END
ELSE DO
  IF argSet2Word# <= argSet2WordCount THEN DO
    currentWord = WORD(argSet2,argSet2Word#)
  END
  ELSE DO
    currentWord = ""
  END
END
RETURN
