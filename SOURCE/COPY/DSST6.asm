* Properties for a ST6 object
*
ST6_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST6_obj_attrs                EQU   *
*
ST6_statementType            DS    C
ST6_recipeStatement          DS    C
                             DS    0F
ST6_ITFO_tiSh                DS    A
*
ST6_obj_attrs_SIZ            EQU   *-ST6_obj_attrs
*
                             DS    0F
*
ST6_obj_SIZ                  EQU   *-ST6_obj
