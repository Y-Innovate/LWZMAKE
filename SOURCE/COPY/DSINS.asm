* Properties for a INS object
*
INS_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
INS_obj_attrs                EQU   *
*
INS_bEOF                     DS    C    * Flag for End-Of-File
INS_bEOL                     DS    C    * Flag for End-Of-Line
                             DS    0F
INS_cCurrChar                DS    C    * Current character
INS_cPeekChar                DS    C    * Peek character 1
INS_cPeekChar2               DS    C    * Peek character 2
                             DS    0F
INS_ISTR_currLine            DS    A    * Current line
INS_nCurrLineLength          DS    F    * Length of current line
INS_pos                      DS    F    * Position in current line
INS_nLine                    DS    F    * Line number in file
*
INS_obj_attrs_SIZ            EQU   *-INS_obj_attrs
*
                             DS    0F
*
INS_obj_SIZ                  EQU   *-INS_obj
