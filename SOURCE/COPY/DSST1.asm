* Properties for a ST1 object
*
ST1_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST1_obj_attrs                EQU   *
*
ST1_statementType            DS    C
ST1_recipeStatement          DS    C
                             DS    0F
ST1_ITFO_tiName              DS    A
ST1_ITFO_tiOperator          DS    A
ST1_ITFO_tiValue             DS    A
*
ST1_obj_attrs_SIZ            EQU   *-ST1_obj_attrs
*
                             DS    0F
*
ST1_obj_SIZ                  EQU   *-ST1_obj
