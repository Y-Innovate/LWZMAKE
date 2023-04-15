* Properties for a FFO object
*
FFO_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
FFO_obj_attrs                EQU   *
*
FFO_ftype                    DS    C
FFO_fname                    DS    A
FFO_fnameLen                 DS    F
FFO_member                   DS    A
FFO_memberLen                DS    F
FFO_exists                   DS    C
FFO_recfm                    DS    C
                             DS    0F
FFO_volser                   DS    CL6
FFO_dsorg                    DS    CL2
*
FFO_alterDate                DS    CL16
*
FFO_obj_attrs_SIZ            EQU   *-FFO_obj_attrs
*
                             DS    0F
*
FFO_obj_SIZ                  EQU   *-FFO_obj
