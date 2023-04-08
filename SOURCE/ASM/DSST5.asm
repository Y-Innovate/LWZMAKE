* Properties for a ST5 object
*
ST5_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST5_obj_attrs                EQU   *
*
ST5_statementType            DS    C
                             DS    0F
ST5_ITFO_tiTargets           DS    A
ST5_ITFO_tiPrereqs           DS    A
*
ST5_obj_attrs_SIZ            EQU   *-ST5_obj_attrs
*
                             DS    0F
*
ST5_obj_SIZ                  EQU   *-ST5_obj
