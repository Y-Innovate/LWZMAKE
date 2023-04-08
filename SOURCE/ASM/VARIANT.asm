* DSECT for a VARIANT (typed value)
*
VARIANT                      DSECT
vt                           DS    F
value                        DS    2F
VARIANT_SIZ                  EQU   *-VARIANT
*
VT_EMPTY                     EQU   0
VT_NULL                      EQU   1
VT_UNKNOWN                   EQU   13
VT_UI1                       EQU   17
VT_UI2                       EQU   18
VT_UI4                       EQU   19
VT_VARCHAR037                EQU   58
VT_VARCHAR1200               EQU   59
VT_CHARARRAY037              EQU   60
VT_CHARARRAY1200             EQU   61
VT_STRING037                 EQU   64
VT_STRING1208                EQU   65
VT_STRING1200                EQU   66
