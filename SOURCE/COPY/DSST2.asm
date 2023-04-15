* Properties for a ST2 object
*
ST2_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST2_obj_attrs                EQU   *
*
ST2_statementType            DS    C
ST2_recipeStatement          DS    C
                             DS    0F
ST2_ITFO_tiCall              DS    A
*
ST2_obj_attrs_SIZ            EQU   *-ST2_obj_attrs
*
                             DS    0F
*
ST2_obj_SIZ                  EQU   *-ST2_obj
