* Properties for a ST7 object
*
ST7_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
ST7_obj_attrs                EQU   *
*
ST7_statementType            DS    C
                             DS    0F
ST7_ITFO_tiHome              DS    A
*
ST7_obj_attrs_SIZ            EQU   *-ST7_obj_attrs
*
                             DS    0F
*
ST7_obj_SIZ                  EQU   *-ST7_obj
