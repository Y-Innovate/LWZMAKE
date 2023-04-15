* Properties for a ST8 object
*
ST8_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST8_obj_attrs                EQU   *
*
ST8_statementType            DS    C
                             DS    0F
ST8_ITFO_tiBuildWhen         DS    A
*
ST8_obj_attrs_SIZ            EQU   *-ST8_obj_attrs
*
                             DS    0F
*
ST8_obj_SIZ                  EQU   *-ST8_obj
