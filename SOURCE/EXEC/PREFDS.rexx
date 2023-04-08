/* REXX */
PARSE ARG prefdsname llqs
SAY COPIES('*',100)
SAY '* PREFDS'
SAY COPIES('*',100)
SAY 'Data set name prefix: 'prefdsname
SAY 'Low level qualifiers: 'llqs
llqcount = WORDS(llqs)
retvalue = prefdsname"."WORD(llqs,1)
DO I = 2 TO llqcount
   retvalue = retvalue" "prefdsname"."WORD(llqs,I)
END
SAY 'Return value        : 'retvalue
RETURN retvalue
