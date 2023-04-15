* Properties for a PRS object
*
PRS_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
PRS_obj_attrs                EQU   *
*
IAV2_statements              DS    A   * ArrayList of statements
IAVL_variables               DS    A   * HashMap of variables
IAVL_targets                 DS    A   * HashMap of targets
IAVL_phonies                 DS    A   * HashMap of phonies
*
firstRuleStatement           DS    A   * First rule statement
firstBuild                   DS    C   * Flag first call to BuildTgts
*
PRS_obj_attrs_SIZ            EQU   *-PRS_obj_attrs
*
                             DS    0F
*
PRS_obj_SIZ                  EQU   *-PRS_obj
