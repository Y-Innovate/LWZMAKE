* Properties for a INF object
*
INF_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
INF_obj_attrs                EQU   *
*
INF_cDDName                  DS    CL8  * DD name
INF_pPath                    DS    A    * Path pointer (len prefixed)
INF_FD                       DS    F    * File descriptor
INF_bOpen                    DS    C    * Flag for file is open
INF_bEOF                     DS    C    * Flag for End-Of-File
INF_bEOL                     DS    C    * Flag for End-Of-Line
                             DS    0F
INF_cCurrChar                DS    C    * Current character
INF_cPeekChar                DS    C    * Peek character 1
INF_cPeekChar2               DS    C    * Peek character 2
                             DS    0F
INF_ISTB_currLine            DS    A    * Current line
INF_nCurrLineLength          DS    F    * Length of current line
INF_pos                      DS    F    * Position in current line
INF_nLine                    DS    F    * Line number in file
*
INF_obj_attrs_SIZ            EQU   *-INF_obj_attrs
*
                             DS    0F
*
INF_obj_SIZ                  EQU   *-INF_obj
