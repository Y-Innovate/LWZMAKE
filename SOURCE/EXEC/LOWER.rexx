/* REXX */
PARSE ARG myArg
SAY 'LOWERCASE'
_upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
_lower = 'abcdefghijklmnopqrstuvwxyz'
_ret = TRANSLATE(myArg, _lower, _upper)
SAY _ret
RETURN _ret
