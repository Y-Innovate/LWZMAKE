* Properties for a REX object
*
REX_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
REX_obj_attrs                EQU   *
*
REX_inTSO                    DS    C
REX_inISPF                   DS    C
                             DS    2C
*
                             DS    0F
REX_EXECBLK                  DS    CL(EXECBLK_V2_LEN)
*
                             DS    0F
REX_EVALBLK                  DS    CL(272)
                             DS    CL4
*
REX_IRXRLT_FUNCTION          DS    CL8
*
REX_ARGS                     DS    CL16
REX_REASON                   DS    CL4
*
REX_EXECBLK_Ptr              DS    A
REX_ARGS_Ptr                 DS    A
REX_FLAGS                    DS    A
REX_INSTBLK_Ptr              DS    A
REX_CPPL_Ptr                 DS    A
REX_EVALBLK_Ptr              DS    A
REX_WORKAREA_Ptr             DS    A
REX_USRFIELD_Ptr             DS    A
REX_REASON_Ptr               DS    A
REX_ENVBLOCK_Ptr             DS    A
*
REX_obj_attrs_SIZ            EQU   *-REX_obj_attrs
*
                             DS    0F
*
REX_obj_SIZ                  EQU   *-REX_obj
