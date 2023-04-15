* Properties for a IND object
*
IND_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
IND_obj_attrs                EQU   *
*
IND_cDDName                  DS    CL8  * DD name
IND_lpDCB                    DS    A    * Ptr to DCB of file
IND_bOpen                    DS    C    * Flag for DCB is open
IND_bEOF                     DS    C    * Flag for End-Of-File
IND_bEOL                     DS    C    * Flag for End-Of-Line
                             DS    0F
IND_cCurrChar                DS    C    * Current character
IND_cPeekChar                DS    C    * Peek character 1
IND_cPeekChar2               DS    C    * Peek character 2
                             DS    0F
IND_ISTR_currLine            DS    A    * Current line
IND_nCurrLineLength          DS    F    * Length of current line
IND_pos                      DS    F    * Position in current line
IND_nLine                    DS    F    * Line number in file
*
IND_obj_attrs_SIZ            EQU   *-IND_obj_attrs
*
                             DS    0F
*
IND_obj_SIZ                  EQU   *-IND_obj
