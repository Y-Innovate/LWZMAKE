* Common properties at the start of each COM object
*
COM_obj                      DSECT
lpVtbl                       DS    A   * Ptr to Vtbl for this obj
count                        DS    F   * Reference count
COM_obj_SIZ                  EQU   *-COM_obj
