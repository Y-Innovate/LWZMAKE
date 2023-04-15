* Properties for a ST3 object
*
ST3_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST3_obj_attrs                EQU   *
*
ST3_statementType            DS    C
                             DS    0F
ST3_ITFO_tiPhony             DS    A
*
ST3_obj_attrs_SIZ            EQU   *-ST3_obj_attrs
*
                             DS    0F
*
ST3_obj_SIZ                  EQU   *-ST3_obj
