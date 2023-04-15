* Properties for a PSS object
*
PSS_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
PSS_obj_attrs                EQU   *
*
PSS_IAV2_state               DS    A   * ArrayList of states
PSS_cRecipePrefix            DS    C   * Recipe prefix character
PSS_cPhase                   DS    C   * Phase number
*
PSS_obj_attrs_SIZ            EQU   *-PSS_obj_attrs
*
                             DS    0F
*
PSS_obj_SIZ                  EQU   *-PSS_obj
