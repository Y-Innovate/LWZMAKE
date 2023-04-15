* Properties for a USS object
*
USS_obj                      DSECT
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
USS_obj_attrs                EQU   *
*
USS_PAR15A                   DS    15A
*
USS_PATH_LEN                 DS    F
USS_PATH_PTR                 DS    A
*
USS_RETVAL                   DS    F
USS_RETCODE                  DS    F
USS_REASON                   DS    F
*
                             DS    0F
USS_BPX1STA_STATAREA         DS    CL(ST#LEN)
*
                             DS    0F
USS_BPX1STA_STAT_LEN         DS    F
USS_BPX1STA_STAT_PTR         DS    A
*
USS_BPX1PIP_FDREAD1          DS    F
USS_BPX1PIP_FDWRITE1         DS    F
*
USS_BPX1SPN_ARGCNT           DS    F
USS_BPX1SPN_ARGLLST          DS    4A
USS_BPX1SPN_ARGSLST          DS    4A
USS_BPX1SPN_ENVCNT           DS    F
USS_BPX1SPN_ENVLENS          DS    2A
USS_BPX1SPN_ENVPARMS         DS    2A
USS_BPX1SPN_FDCNT            DS    F
USS_BPX1SPN_FDLST            DS    3A
*
USS_ALET                     DS    F
USS_COUNT                    DS    F
USS_OPTIONS                  DS    F
USS_MODE                     DS    F
USS_FD                       DS    F
*
USS_BUFFERA                  DS    A
USS_BUFFER                   DS    CL4
*
USS_DDNAME                   DS    CL8
USS_DSNAME                   DS    CL44
*
USS_WAST                     DS    A
USS_WASTA                    DS    A
*
USS_INHE                     DS    CL(INHE#LENGTH)
*
USS_MAK119E                  DS    CL23
*
USS_obj_attrs_SIZ            EQU   *-USS_obj_attrs
*
                             DS    0F
*
USS_obj_SIZ                  EQU   *-USS_obj
