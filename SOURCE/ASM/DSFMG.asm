* Properties for a FMG object
*
FMG_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
FMG_obj_attrs                EQU   *
*
FMG_MODRSNRT                 DS    0A
FMG_MODID                    DS    CL2
FMG_RSNCODE                  DS    C
FMG_RTNCODE                  DS    C
*
FMG_DAREA_PTR                DS    A
*
FMG_CSIFIELD                 DS    CL(CSIFIELD_DSECT_SIZ)
*
                             DS    0F
FMG_CAMLST_PARM              DS    4A
FMG_CAMLST_VOLSER            DS    CL6
                             DS    0F
FMG_CAMLST_DSNAME            DS    CL44
FMG_CAMLST_WORKAREA          DS    CL140
                             DS    CL8   * Can't figure out why this is
*                                        * needed, but OBTAIN goes
*                                        * beyond 140 workarea
*
FMG_DYNALLOC_WORKAREA_PTR    DS    A
*
FMG_DIRREC                   DS    CL256
*
                             DS    0F
FMG_IEWBIND_DIALOG           DS    CL8
FMG_IEWBIND_WORKMOD          DS    CL8
FMG_IEWBIND_DCBPTR           DS    CL4
FMG_IEWBIND_DEPTR            DS    CL4
FMG_IEWBIND_CURSOR           DS    CL4
FMG_IEWBIND_COUNT            DS    CL4
FMG_IEWBIND_CLASS            DS    HL2,CL16
*
                             DS    0F
FMG_BLDLIST                  DS    F
FMG_BLDL_DE                  DS    CL8
                             DS    CL50
*
FMG_IEWBFDAT_SB_PAR4A        DS    4A
FMG_IEWBFDAT_SB_SB           DS    CL4
FMG_IEWBFDAT_SB_MTOKEN       DS    CL4
FMG_IEWBFDAT_SB_PGMNAME      DS    CL8
*
FMG_IEWBFDAT_GD_PAR8A        DS    8A
FMG_IEWBFDAT_GD_GD           DS    CL4
FMG_IEWBFDAT_GD_MTOKEN       DS    CL4
FMG_IEWBFDAT_GD_B_IDRB       DS    CL8
FMG_IEWBFDAT_GD_CURSOR       DS    F
FMG_IEWBFDAT_GD_COUNT        DS    F
*
FMG_IEWBFDAT_EN_PAR2A        DS    2A
FMG_IEWBFDAT_EN_EN           DS    CL4
FMG_IEWBFDAT_EN_MTOKEN       DS    CL4
*
FMG_obj_attrs_SIZ            EQU   *-FMG_obj_attrs
*
                             DS    0F
*
FMG_obj_SIZ                  EQU   *-FMG_obj
