* Properties for a TOK object
*
TOK_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
TOK_obj_attrs                EQU   *
*
IIN_in                       DS    A   * Input of any type
IIFO_ii                      DS    A   * InputInfo for IIN_in
*
TOK_obj_attrs_SIZ            EQU   *-TOK_obj_attrs
*
                             DS    0F
*
TOK_obj_SIZ                  EQU   *-TOK_obj
