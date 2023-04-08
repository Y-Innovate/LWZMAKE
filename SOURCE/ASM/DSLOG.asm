* Properties for a LOG object
*
LOG_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
LOG_obj_attrs                EQU   *
*
lpDCB                        DS    A    * Ptr to DCB of file
bOpen                        DS    C    * Flag for DCB is open
cLogLevel                    DS    C    * Log level
                             DS    0F
cLine                        DS    CL160
*
LOG_obj_attrs_SIZ            EQU   *-LOG_obj_attrs
*
                             DS    0F
*
LOG_obj_SIZ                  EQU   *-LOG_obj
