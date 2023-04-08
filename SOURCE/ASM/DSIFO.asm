* Properties for a IFO object
*
IFO_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
IFO_obj_attrs                EQU   *
*
IFO_bEOF                     DS    C   * Flag for End-Of-File
IFO_bEOL                     DS    C   * Flag for End-Of-Line
                             DS    0F
IFO_cCurrChar                DS    C   * Current character
IFO_cPeekChar                DS    C   * Peek character 1
IFO_cPeekChar2               DS    C   * Peek character 2
                             DS    0F
IFO_pos                      DS    F   * Current position in line
IFO_nLine                    DS    F   * Line number
*
IFO_obj_attrs_SIZ            EQU   *-IFO_obj_attrs
*
                             DS    0F
*
IFO_obj_SIZ                  EQU   *-IFO_obj
