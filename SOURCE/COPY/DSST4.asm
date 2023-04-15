* Properties for a ST4 object
*
ST4_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST4_obj_attrs                EQU   *
*
ST4_statementType            DS    C
                             DS    0F
ST4_ITFO_tiRecipePrefix      DS    A
*
ST4_obj_attrs_SIZ            EQU   *-ST4_obj_attrs
*
                             DS    0F
*
ST4_obj_SIZ                  EQU   *-ST4_obj
