* Properties for a STR object
*
STR_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
STR_obj_attrs                EQU   *
*
STR_lpString                 DS    A
STR_nStrLen                  DS    F
*
STR_obj_attrs_SIZ            EQU   *-STR_obj_attrs
*
                             DS    0F
*
STR_obj_SIZ                  EQU   *-STR_obj
