/* REXX */
PARSE ARG argdsname hlqtostrip
SAY COPIES('*',100)
SAY '* STRIPHLQ'
SAY COPIES('*',100)
SAY 'Data set name: 'argdsname
SAY 'HLQ to strip : 'hlqtostrip
dotindex = 1
IF hlqtostrip = "" THEN DO
   IF INDEX(argdsname,'.') > 0 ,
   THEN dotindex = INDEX(argdsname,'.') + 1
END
ELSE DO
   IF SUBSTR(argdsname,1,LENGTH(hlqtostrip)) = hlqtostrip THEN DO
      dotindex = LENGTH(hlqtostrip) + 1
      IF SUBSTR(argdsname,dotindex,1) = "." ,
      THEN dotindex = dotindex + 1
   END
END
retvalue = SUBSTR(argdsname,dotindex)
SAY 'Return value : 'retvalue
RETURN retvalue
