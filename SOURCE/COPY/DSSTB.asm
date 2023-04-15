* Properties for a STB object
*
STB_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
STB_obj_attrs                EQU   *
*
STB_lpBuf                    DS    A
STB_nStrLen                  DS    F
STB_nBufSize                 DS    F
*
STB_obj_attrs_SIZ            EQU   *-STB_obj_attrs
*
                             DS    0F
*
STB_obj_SIZ                  EQU   *-STB_obj
