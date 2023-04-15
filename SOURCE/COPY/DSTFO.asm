* Properties for a TFO object
*
TFO_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
TFO_obj_attrs                EQU   *
*
TFO_ISTB_token               DS    A   * token text
TFO_tokenType                DS    F   * token type
TFO_nLine                    DS    F   * line number
TFO_pos                      DS    F   * position in line
TFO_nSpaces                  DS    F   * number of spaces
TFO_bEOF                     DS    C   * Flag for End-Of-File
TFO_bEOL                     DS    C   * Flag for End-Of-Line
*
TFO_obj_attrs_SIZ            EQU   *-TFO_obj_attrs
*
                             DS    0F
*
TFO_obj_SIZ                  EQU   *-TFO_obj
