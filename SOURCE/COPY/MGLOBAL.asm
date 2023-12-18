                             MACRO
                             MGLOBAL
*
GLOBAL                       DSECT
*
G_RETCODE                    DS    F    * Returncode
*
                             DS    0F
G_LINKL                      DS    CL(8) * LINK_SIZ
                             DS    0F
G_LINKD                      DS    CL(8) * LINK_SIZ
*
                             DS    0F
G_LOADL                      DS    CL(16) * LOAD_SIZ
                             DS    0F
G_LOADD                      DS    CL(16) * LOAD_SIZ
*
                             DS    0F
G_OPENL                      DS    CL(8) * OPEN_SIZ
                             DS    0F
G_OPEND                      DS    CL(8) * OPEN_SIZ
*
                             DS    0F
G_CLOSEL                     DS    CL(8) * CLOSE_SIZ
                             DS    0F
G_CLOSED                     DS    CL(8) * CLOSE_SIZ
*
                             DS    0F
G_SWAREQL                    DS    CL(8) * SWAREQ_SIZ
                             DS    0F
G_SWAREQD                    DS    CL(8) * SWAREQ_SIZ
*
G_GETDSABL                   DS    0F
                             DS    CL(16) * GETDSAB_SIZ
G_GETDSABD                   DS    0F
                             DS    CL(16) * GETDSAB_SIZ
*
G_GTST                       DS    A
G_GTSTSIZ                    DS    F
G_HEAP                       DS    A    * My own heap start block
*
G_OBJCOUNT                   DS    F    * Total global object count
*
G_IAVL_GUID                  DS    CL16
G_IAV2_GUID                  DS    CL16
G_IFFO_GUID                  DS    CL16
G_IFMG_GUID                  DS    CL16
G_IIFO_GUID                  DS    CL16
G_IIND_GUID                  DS    CL16
G_IINF_GUID                  DS    CL16
G_IINS_GUID                  DS    CL16
G_ILOG_GUID                  DS    CL16
G_IPRS_GUID                  DS    CL16
G_IPSS_GUID                  DS    CL16
G_IREX_GUID                  DS    CL16
G_ISTB_GUID                  DS    CL16
G_ISTR_GUID                  DS    CL16
G_IST1_GUID                  DS    CL16
G_IST2_GUID                  DS    CL16
G_IST3_GUID                  DS    CL16
G_IST4_GUID                  DS    CL16
G_IST5_GUID                  DS    CL16
G_IST6_GUID                  DS    CL16
G_IST7_GUID                  DS    CL16
G_IST8_GUID                  DS    CL16
G_ITFO_GUID                  DS    CL16
G_ITOK_GUID                  DS    CL16
G_IUSS_GUID                  DS    CL16
*
G_ILOG                       DS    A    * Logger object
G_IREX                       DS    A    * REXX object
G_IUSS                       DS    A    * USS object
G_IFMG                       DS    A    * File manager object
G_ISTB_buildTargets          DS    A    * Parameter string of targets
G_ISTB_tmp                   DS    A    * Quick use string builder
*
G_HOMELEN                    DS    F
G_HOME                       DS    A
*
G_BUILDWHEN                  DS    CL4
*
G_IRXINITA                   DS    A
G_IRXEXECA                   DS    A
G_IRXRLTA                    DS    A
G_IGGCSI00A                  DS    A
G_BPX1STAA                   DS    A
G_BPX1PIPA                   DS    A
G_BPX1SPNA                   DS    A
G_BPX1OPNA                   DS    A
G_BPX1CLOA                   DS    A
G_BPX1REDA                   DS    A
G_BPX1WATA                   DS    A
*
G_TRT_ANY_BUT_SPACE          DS    A
G_TRT_ONLY_SPACE             DS    A
G_TRT_ONLY_ZEROS             DS    A
G_HEXTAB                     DS    A
*
                             DS    0AD
G_DEC8                       DS    PL8
G_ZONED8                     DS    CL8
G_ZONED8_EXTRA               DS    CL4
*
G_TIMEDATE                   DS    PL16 * for TIME macro
                             DS    0F
G_TIMEDATEZ                  DS    CL32 * for formatting time
*
                             DS    0F
G_CONVTOD_INAREA             DS    4F
G_CONVTOD_OUTAREA            DS    2F
G_CONVTODL                   DS    CL(32) * CONVTOD_SIZ
G_CONVTODD                   DS    CL(32) * CONVTOD_SIZ
*
                             DS    0F
G_STCKCONV_OUTAREA           DS    CL16
G_STCKCONVL                  DS    CL(28) * STCKCONV_SIZ
G_STCKCONVD                  DS    CL(28) * STCKCONV_SIZ
*
                             DS    0F
G_DATEWORK_DEC_1             DS    CL8
G_DATEWORK_DEC_2             DS    CL8
G_DATEWORK_ZON               DS    CL16
*
G_SWEPAPTR                   DS    A
G_EPA                        DS    CL28
*
G_WTOBLOCK                   DS    0F
G_WTOLEN                     DS    H
G_WTOFIL                     DS    H
G_WTOTEXT                    DS    CL80
G_WTOBLOCK_SIZ               EQU   *-G_WTOBLOCK
*
GLOBAL_SIZ                   EQU   *-GLOBAL
*
                             MEND
