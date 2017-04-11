/* REXX */
PARSE ARG argdsname
SAY COPIES('*',100)
SAY '* LOWQUAL'
SAY COPIES('*',100)
SAY 'Data set name: 'argdsname
tmpdsname = TRANSLATE(argdsname," ",".")
qualcount = WORDS(tmpdsname)
retvalue = WORD(tmpdsname,qualcount)
SAY 'Return value : 'retvalue
RETURN retvalue
