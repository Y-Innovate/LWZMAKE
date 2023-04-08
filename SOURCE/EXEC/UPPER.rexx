/* REXX */
PARSE ARG myArg
SAY 'UPPERCASE'
_upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
_lower = 'abcdefghijklmnopqrstuvwxyz'
_ret = TRANSLATE(myArg, _upper, _lower)
SAY _ret
RETURN _ret
